type error = {
  span : Diagnostic.span;
  message : string
}
type definition = {
  descriptor : Sst_validation.callable_descriptor;
  pending : Termination.pending_summary;
  stable_id : string;
  body : Sst.expression;
  visibility : [ `Opaque | `Revealed ];
  types : Sst.type_definition list;
  parametric_adts : Parametric_adt.t list;
}
type activation = Spec_unfolding.activation = {
  function_id : Sst.function_id;
  depth : int;
  span : Diagnostic.span;
}
type prepared = {
  validated : Sst_validation.validated_program;
  termination : Termination.plan;
  definitions : definition list;
  obligations : Vir.obligation list;
}
let recursive_lowerings = ref 0
type value =
  | Integer of Vir.integer_term
  | Boolean of Vir.boolean_term
  | Aggregate of Vir.aggregate_term
  | Parametric of Vir.parametric_term
  | Function of {
      function_term : Vir.spec_function_term;
      function_arrow : Sst.typ;
    }
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error
let fail span format =
  Printf.ksprintf (fun message -> Error { span; message }) format
let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let stable_id id =
  let safe =
    String.map
      (function
        | ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as character -> character
        | _ -> '_')
      id.Sst.function_name
  in
  Printf.sprintf "spec.%d.%s" id.function_index safe
let vir_aggregate_type_of_sst descriptors = function
  | Sst.Aggregate type_id ->
      Some
        { Vir.aggregate_type_index = type_id.type_index;
          aggregate_type_name = type_id.type_name;
          aggregate_type_arguments = [] }
  | Sst.Application (constructor, arguments) ->
      if Parametric_type.is_spec_function (Sst.Application (constructor, arguments))
      then None
      else
        Option.map
          (fun descriptor ->
            let type_id = Parametric_adt.type_id descriptor in
            { Vir.aggregate_type_index = type_id.type_index;
              aggregate_type_name =
                type_id.type_name ^ "<"
                ^ String.concat ","
                    (List.map Parametric_type.to_string arguments)
                ^ ">";
              aggregate_type_arguments = arguments })
          (Parametric_adt.find descriptors constructor)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> None
let symbol_of_binding parametric_adts (binding : Sst.binding) =
  let sort =
    match binding.typ with
    | Sst.Int -> Vir.Integer
    | Sst.Bool -> Vir.Boolean
    | Sst.Aggregate type_id ->
        Vir.Aggregate
          {
            Vir.aggregate_type_index = type_id.type_index;
            aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
          }
    | Sst.Parameter binder -> Vir.Parametric binder
    | Sst.Application _ when Parametric_type.is_spec_function binding.typ ->
        Vir.Parametric (Spec_function_logic_private.binder binding.typ)
    | Sst.Application _ ->
        Vir.Aggregate
          (Option.get
             (vir_aggregate_type_of_sst parametric_adts binding.typ))
    | Sst.Unit | Sst.Tuple _ -> assert false
  in
  {
    Vir.symbol_id = binding.id;
    source_name = binding.name;
    sort;
    role = Vir.Input;
    span = binding.span;
  }
let quantifier_value parametric_adts (binding : Sst.binding) =
  let symbol =
    { (symbol_of_binding parametric_adts binding) with Vir.role = Vir.Local }
  in
  let value =
    match binding.typ with
    | Sst.Int -> Integer (Vir.Integer_symbol symbol)
    | Sst.Bool -> Boolean (Vir.Boolean_symbol symbol)
    | Sst.Application _ when Parametric_type.is_spec_function binding.typ ->
        Function
          {
            function_term =
              Result.get_ok
                (Spec_function_logic_private.of_symbol ~arrow:binding.typ
                   symbol);
            function_arrow = binding.typ;
          }
    | Sst.Aggregate _ | Sst.Application _ ->
        Aggregate
          {
            Vir.aggregate_type =
              Option.get
                (vir_aggregate_type_of_sst parametric_adts binding.typ);
            aggregate_desc = Vir.Aggregate_symbol symbol;
          }
    | Sst.Parameter binder ->
        Parametric
          {
            Vir.parametric_sort = binder;
            parametric_desc = Vir.Parametric_symbol symbol;
          }
    | Sst.Unit | Sst.Tuple _ -> assert false
  in
  (symbol, value)
let bounded_quantifier_body kind binder body =
  match binder.Vir.sort with
  | Vir.Integer ->
      let range =
        match Vir.integer_range (Vir.Integer_symbol binder) with
        | [] -> Vir.Boolean_constant true
        | first :: rest ->
            List.fold_left
              (fun combined term -> Vir.Boolean_and (combined, term))
              first rest
      in
      (match kind with
      | Logic_quantifier_private.Forall ->
          Vir.Boolean_or (Vir.Boolean_not range, body)
      | Logic_quantifier_private.Exists -> Vir.Boolean_and (range, body))
  | Vir.Boolean | Vir.Aggregate _ | Vir.Parametric _ -> body
let initial_environment parametric_adts parameters =
  List.map
    (fun parameter ->
      let parameter = Sst.require_value_parameter parameter in
      match parameter.pattern.pattern_desc with
      | Sst.Bind binding -> (
          let symbol = symbol_of_binding parametric_adts binding in
          ( binding.id,
            match binding.typ with
            | Sst.Int -> Integer (Vir.Integer_symbol symbol)
            | Sst.Bool -> Boolean (Vir.Boolean_symbol symbol)
            | Sst.Aggregate type_id ->
                Aggregate
                  {
                    Vir.aggregate_type =
                      {
                        aggregate_type_index = type_id.type_index;
                        aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
                      };
                    aggregate_desc = Vir.Aggregate_symbol symbol;
                  }
            | Sst.Parameter binder ->
                Parametric
                  { Vir.parametric_sort = binder;
                    parametric_desc = Vir.Parametric_symbol symbol }
            | Sst.Application _ when Parametric_type.is_spec_function binding.typ
              ->
                Function
                  {
                    function_term =
                      Result.get_ok
                        (Spec_function_logic_private.of_symbol
                           ~arrow:binding.typ symbol);
                    function_arrow = binding.typ;
                  }
            | Sst.Application _ ->
                Aggregate
                  { Vir.aggregate_type =
                      Option.get
                        (vir_aggregate_type_of_sst parametric_adts binding.typ);
                    aggregate_desc = Vir.Aggregate_symbol symbol }
            | Sst.Unit | Sst.Tuple _ -> assert false
          ) )
      | _ -> assert false)
    parameters
let lookup span environment binding =
  match List.assoc_opt binding.Sst.id environment with
  | Some value -> Ok value
  | None -> fail span "unbound recursive-spec binding %s#%d" binding.name binding.id
let integer span = function
  | Integer term -> Ok term
  | Boolean _ | Aggregate _ | Parametric _ | Function _ ->
      fail span "expected an integer recursive-spec term"
let boolean span = function
  | Boolean term -> Ok term
  | Integer _ | Aggregate _ | Parametric _ | Function _ ->
      fail span "expected a Boolean recursive-spec term"
let aggregate span = function
  | Aggregate term -> Ok term
  | Integer _ | Boolean _ | Parametric _ | Function _ ->
      fail span "expected an aggregate recursive-spec term"
let vir_aggregate_type (type_id : Sst.type_id) =
  {
    Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }
let selector_range parametric_adts typ =
  match typ with
  | Sst.Int -> Vir.Integer
  | Sst.Bool -> Vir.Boolean
  | Sst.Aggregate type_id -> Vir.Aggregate (vir_aggregate_type type_id)
  | Sst.Parameter binder -> Vir.Parametric binder
  | Sst.Application _ when Parametric_type.is_spec_function typ ->
      Vir.Parametric (Spec_function_logic_private.binder typ)
  | Sst.Application _ ->
      Vir.Aggregate
        (Option.get (vir_aggregate_type_of_sst parametric_adts typ))
  | Sst.Unit | Sst.Tuple _ -> assert false

let argument_selector parametric_adts domain
    (constructor : Sst.constructor_id) index typ =
  let selector_range =
    selector_range parametric_adts typ
  in
  {
    Vir.selector_domain = domain;
    selector_range;
    selector_namespace =
      Printf.sprintf "t%d_%s_c%d_%s" constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name;
    selector_index = index;
    selector_name = Printf.sprintf "$arg%d" index;
    selector_path = [];
  }

let field_owner_namespace = function
  | Sst.Record_owner type_id ->
      Printf.sprintf "t%d_%s_record" type_id.type_index type_id.type_name
  | Sst.Constructor_owner constructor ->
      Printf.sprintf "t%d_%s_c%d_%s_inline"
        constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name

let field_selector parametric_adts domain (field : Sst.field_id) typ =
  let selector_range =
    selector_range parametric_adts typ
  in
  {
    Vir.selector_domain = domain;
    selector_range;
    selector_namespace = field_owner_namespace field.field_owner;
    selector_index = field.field_index;
    selector_name = field.field_name;
    selector_path = [];
  }

let selected_argument parametric_adts aggregate constructor index typ =
  let selector =
    argument_selector parametric_adts aggregate.Vir.aggregate_type constructor
      index typ
  in
  match typ with
  | Sst.Int -> Integer (Vir.Integer_selector (selector, aggregate))
  | Sst.Bool -> Boolean (Vir.Boolean_selector (selector, aggregate))
  | Sst.Aggregate type_id ->
      Aggregate
        {
          Vir.aggregate_type = vir_aggregate_type type_id;
          aggregate_desc = Vir.Aggregate_selector (selector, aggregate);
        }
  | Sst.Parameter binder ->
      Parametric
        { Vir.parametric_sort = binder;
          parametric_desc = Vir.Parametric_selector (selector, aggregate) }
  | Sst.Application _ ->
      Aggregate
        { Vir.aggregate_type =
            Option.get (vir_aggregate_type_of_sst parametric_adts typ);
          aggregate_desc = Vir.Aggregate_selector (selector, aggregate) }
  | Sst.Unit | Sst.Tuple _ -> assert false

let selected_field parametric_adts aggregate field typ =
  let selector =
    field_selector parametric_adts aggregate.Vir.aggregate_type field typ
  in
  match typ with
  | Sst.Int -> Integer (Vir.Integer_selector (selector, aggregate))
  | Sst.Bool -> Boolean (Vir.Boolean_selector (selector, aggregate))
  | Sst.Aggregate type_id ->
      Aggregate
        {
          Vir.aggregate_type = vir_aggregate_type type_id;
          aggregate_desc = Vir.Aggregate_selector (selector, aggregate);
        }
  | Sst.Parameter binder ->
      Parametric
        { Vir.parametric_sort = binder;
          parametric_desc = Vir.Parametric_selector (selector, aggregate) }
  | Sst.Application _ ->
      Aggregate
        { Vir.aggregate_type =
            Option.get (vir_aggregate_type_of_sst parametric_adts typ);
          aggregate_desc = Vir.Aggregate_selector (selector, aggregate) }
  | Sst.Unit | Sst.Tuple _ -> assert false

let recursive_argument = function
  | Integer term -> Vir.Recursive_integer_argument term
  | Boolean term -> Vir.Recursive_boolean_argument term
  | Aggregate term -> Vir.Recursive_aggregate_argument term
  | Parametric term -> Vir.Recursive_parametric_argument term
  | Function function_ ->
      Vir.Recursive_parametric_argument function_.function_term

let application_term = function
  | Integer term -> Vir.Integer_application term
  | Boolean term -> Vir.Boolean_application term
  | Aggregate term -> Vir.Aggregate_application term
  | Parametric term -> Vir.Parametric_application term
  | Function function_ -> Vir.Parametric_application function_.function_term

let value_of_application parametric_adts span typ application =
  Spec_function_logic_private.value_of_logic_application
    {
      integer = (fun application ->
        Integer (Vir.Integer_symbolic_application application));
      boolean = (fun application ->
        Boolean (Vir.Boolean_symbolic_application application));
      parametric = (fun binder application ->
        Parametric
          {
            Vir.parametric_sort = binder;
            parametric_desc = Vir.Parametric_symbolic_application application;
          });
      function_ = (fun arrow application ->
        Function
          {
            function_term =
              {
                Vir.parametric_sort = Spec_function_logic_private.binder arrow;
                parametric_desc =
                  Vir.Parametric_symbolic_application application;
              };
            function_arrow = arrow;
          });
      aggregate = (fun aggregate_type application ->
        Aggregate
          {
            Vir.aggregate_type;
            aggregate_desc = Vir.Aggregate_symbolic_application application;
          });
      aggregate_type = vir_aggregate_type_of_sst parametric_adts;
      invalid = (fun message -> { span; message });
    }
    typ application

let rec irrefutable_pattern (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Wildcard | Sst.Bind _ -> true
  | Sst.Record_pattern fields ->
      List.for_all (fun (_, nested) -> irrefutable_pattern nested) fields
  | Sst.Unit_pattern | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Tuple_pattern _ | Sst.Constructor_pattern _
  | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
      false

let rec bind_pattern parametric_adts environment (pattern : Sst.pattern) value =
  match (pattern.pattern_desc, value) with
  | Sst.Wildcard, _ ->
      Ok (environment, Vir.Boolean_constant true)
  | Sst.Bind binding, value ->
      Ok ((binding.id, value) :: environment, Vir.Boolean_constant true)
  | Sst.Constructor_pattern (constructor, arguments), Aggregate aggregate ->
      let tag =
        Vir.Integer_compare
          ( Vir.Equal,
            Vir.Aggregate_tag
              (aggregate.Vir.aggregate_type, aggregate),
            Vir.Integer_constant (Z.of_int constructor.constructor_index) )
      in
      let rec loop index environment conditions = function
        | [] ->
            Ok
              ( environment,
                List.fold_left
                  (fun condition nested ->
                    Vir.Boolean_and (condition, nested))
                  tag (List.rev conditions) )
        | (argument : Sst.pattern) :: rest ->
            let selected =
              selected_argument parametric_adts aggregate constructor index
                argument.typ
            in
            let* environment, condition =
              bind_pattern parametric_adts environment argument selected
            in
            loop (index + 1) environment (condition :: conditions) rest
      in
      loop 0 environment [] arguments
  | Sst.Record_pattern fields, Aggregate aggregate ->
      List.fold_left
        (fun result (field, (nested : Sst.pattern)) ->
          let* environment, condition = result in
          let selected =
            selected_field parametric_adts aggregate field nested.Sst.typ
          in
          let* environment, nested_condition =
            bind_pattern parametric_adts environment nested selected
          in
          Ok
            ( environment,
              Vir.Boolean_and (condition, nested_condition) ))
        (Ok (environment, Vir.Boolean_constant true))
        fields
  | Sst.Int_pattern expected, Integer actual ->
      Ok
        ( environment,
          Vir.Integer_compare
            (Vir.Equal, actual, Vir.Integer_constant expected) )
  | Sst.Bool_pattern expected, Boolean actual ->
      Ok
        ( environment,
          Vir.Boolean_equal (actual, Vir.Boolean_constant expected) )
  | _ ->
      fail pattern.span
        "recursive-spec termination supports constructor and scalar patterns"

let conjunction conditions =
  match conditions with
  | [] -> Vir.Boolean_constant true
  | condition :: rest ->
      List.fold_left
        (fun combined condition -> Vir.Boolean_and (combined, condition))
        condition rest

let bind_match_pattern parametric_adts translate environment scrutinee pattern =
  match scrutinee.Sst.expression_desc with
  | Sst.Tuple_value _ -> (
      match Recursive_spec_tuple_match_private.plan scrutinee pattern with
      | Error error ->
          fail pattern.Sst.span "%s"
            (Recursive_spec_tuple_match_private.error_to_string error)
      | Ok leaves ->
          let* case_environment, conditions =
            List.fold_left
              (fun result
                   (leaf : Recursive_spec_tuple_match_private.leaf) ->
                let* case_environment, conditions = result in
                let* value = translate environment leaf.expression in
                let* case_environment, condition =
                  bind_pattern parametric_adts case_environment leaf.pattern
                    value
                in
                Ok (case_environment, condition :: conditions))
              (Ok (environment, [])) leaves
          in
          Ok (case_environment, conjunction (List.rev conditions)))
  | _ ->
      let* scrutinee = translate environment scrutinee in
      bind_pattern parametric_adts environment pattern scrutinee

let translate_boolean_match parametric_adts translate environment
    (expression : Sst.expression) scrutinee cases =
  let* cases =
    List.fold_left
      (fun result (case : Sst.case) ->
        let* translated = result in
        let* case_environment, condition =
          bind_match_pattern parametric_adts translate environment scrutinee
            case.case_pattern
        in
        let* condition =
          match case.case_guard with
          | None -> Ok condition
          | Some guard ->
              let guard_span = guard.span in
              let* guard = translate case_environment guard in
              let* guard = boolean guard_span guard in
              Ok (Vir.Boolean_and (condition, guard))
        in
        let* body = translate case_environment case.case_body in
        let* body = boolean case.case_body.span body in
        Ok ((condition, body) :: translated))
      (Ok []) cases
    |> Result.map List.rev
  in
  match List.rev cases with
  | [] -> fail expression.Sst.span "recursive-spec helper match has no cases"
  | (_, fallback) :: remaining ->
      Ok
        (Boolean
           (List.fold_left
              (fun alternative (condition, consequent) ->
                Vir.Boolean_or
                  ( Vir.Boolean_and (condition, consequent),
                    Vir.Boolean_and
                      (Vir.Boolean_not condition, alternative) ))
              fallback remaining))

let rec translate parametric_adts environment (expression : Sst.expression) =
  let translate = translate parametric_adts in
  let recurse = translate environment in
  match expression.expression_desc with
  | Sst.Int_constant value -> Ok (Integer (Vir.Integer_constant value))
  | Sst.Bool_constant value -> Ok (Boolean (Vir.Boolean_constant value))
  | Sst.Variable { binding; _ } -> lookup expression.span environment binding
  | Sst.Checked_arithmetic (operation, operands) ->
      let* operands =
        List.fold_left
          (fun result operand ->
            let* operands = result in
            let* operand = recurse operand in
            let* operand = integer expression.span operand in
            Ok (operand :: operands))
          (Ok []) operands
      in
      let operands = List.rev operands in
      let result =
        match (operation, operands) with
        | Sst.Add, [ left; right ] -> Vir.Integer_add (left, right)
        | Sst.Subtract, [ left; right ] -> Vir.Integer_subtract (left, right)
        | Sst.Negate, [ value ] -> Vir.Integer_negate value
        | Sst.Multiply_constant coefficient, [ value ] ->
            Vir.Integer_multiply_constant (coefficient, value)
        | Sst.Successor, [ value ] ->
            Vir.Integer_add (value, Vir.Integer_constant Z.one)
        | Sst.Predecessor, [ value ] ->
            Vir.Integer_subtract (value, Vir.Integer_constant Z.one)
        | Sst.Absolute_value, [ value ] -> Vir.Integer_absolute_value value
        | _ -> assert false
      in
      Ok (Integer result)
  | Sst.Compare (comparison, left, right) -> (
      let* left = recurse left in
      let* right = recurse right in
      match (left, right) with
      | Integer left, Integer right ->
          let comparison =
            match comparison with
            | Sst.Equal -> Vir.Equal
            | Sst.Not_equal -> Vir.Not_equal
            | Sst.Less_than -> Vir.Less_than
            | Sst.Less_or_equal -> Vir.Less_or_equal
            | Sst.Greater_than -> Vir.Greater_than
            | Sst.Greater_or_equal -> Vir.Greater_or_equal
          in
          Ok (Boolean (Vir.Integer_compare (comparison, left, right)))
      | Boolean left, Boolean right -> (
          match comparison with
          | Sst.Equal -> Ok (Boolean (Vir.Boolean_equal (left, right)))
          | Sst.Not_equal ->
              Ok (Boolean (Vir.Boolean_not_equal (left, right)))
          | _ -> fail expression.span "ordered comparison on booleans")
      | Aggregate left, Aggregate right -> (
          match comparison with
          | Sst.Equal -> Ok (Boolean (Vir.Aggregate_equal (left, right)))
          | Sst.Not_equal ->
              Ok
                (Boolean
                   (Vir.Boolean_not (Vir.Aggregate_equal (left, right))))
          | _ -> fail expression.span "ordered comparison on aggregates")
      | Function left, Function right
        when Parametric_type.equal left.function_arrow right.function_arrow ->
          (match comparison with
          | Sst.Equal ->
              Ok
                (Boolean
                   (Vir.Parametric_equal
                      (left.function_term, right.function_term)))
          | Sst.Not_equal ->
              Ok
                (Boolean
                   (Vir.Boolean_not
                      (Vir.Parametric_equal
                         (left.function_term, right.function_term))))
          | _ ->
              fail expression.span
                "ordered comparison on specification functions")
      | _ -> fail expression.span "recursive-spec comparison sort mismatch")
  | Sst.Boolean_not operand ->
      let* operand = recurse operand in
      let* operand = boolean expression.span operand in
      Ok (Boolean (Vir.Boolean_not operand))
  | Sst.Boolean_binary (operation, left, right) ->
      let* left = recurse left in
      let* right = recurse right in
      let* left = boolean expression.span left in
      let* right = boolean expression.span right in
      Ok
        (Boolean
           (match operation with
           | Sst.And -> Vir.Boolean_and (left, right)
           | Sst.Or -> Vir.Boolean_or (left, right)))
  | Sst.Direct_call _
    when Spec_function_sst_private.application expression <> None ->
      let application =
        Option.get (Spec_function_sst_private.application expression)
      in
      let result_type = application.application_result in
      let* function_ = recurse application.application_function in
      let* argument = recurse application.application_argument in
      let* function_term =
        match function_ with
        | Function function_
          when
            Parametric_type.equal function_.function_arrow
              application.application_arrow ->
            Ok function_.function_term
        | Function _ ->
            fail expression.span
              "specification-function application has the wrong arrow"
        | Integer _ | Boolean _ | Aggregate _ | Parametric _ ->
            fail expression.span
              "specification-function application head is not a function"
      in
      let* application =
        Spec_function_logic_private.application
          ~arrow:application.application_arrow ~function_:function_term
          ~argument:(recursive_argument argument)
          ~result_type:application.application_result ~span:expression.span
        |> Result.map_error (fun message ->
               { span = expression.span; message })
      in
      value_of_application parametric_adts expression.span
        result_type application
  | Sst.Direct_call
      {
        call_form = (Sst.Specification_call | Sst.Proof_call);
        callee;
        type_arguments;
        arguments;
        _;
      } ->
      let* arguments =
        List.fold_left
          (fun result argument ->
            let* arguments = result in
            let _, value = Sst.require_value_argument argument in
            let* value = recurse value in
            Ok (recursive_argument value :: arguments))
          (Ok []) arguments
        |> Result.map List.rev
      in
      Ok
        (Boolean
           (Vir.Boolean_specification_application
              { callee; type_arguments; arguments; span = expression.span }))
  | Sst.Symbolic_application application ->
      let* arguments =
        List.fold_left
          (fun result source ->
            let* arguments = result in
            let* value = recurse source in
            Ok (recursive_argument value :: arguments))
          (Ok []) (Symbolic_application_private.arguments application)
        |> Result.map List.rev
      in
      let* application =
        Symbolic_application_private.replace_arguments arguments application
        |> Result.map_error (fun message ->
               { span = expression.span; message })
      in
      value_of_application parametric_adts expression.span
        (Symbolic_application_private.result_type application)
        application
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      let kind =
        match expression.expression_desc with
        | Sst.Forall _ -> Logic_quantifier_private.Forall
        | Sst.Exists _ -> Logic_quantifier_private.Exists
        | _ -> assert false
      in
      let binder, value =
        quantifier_value parametric_adts quantifier.quantifier_binder
      in
      let scoped =
        (quantifier.quantifier_binder.id, value) :: environment
      in
      let* body = translate scoped quantifier.quantifier_body in
      let* body = boolean quantifier.quantifier_body.span body in
      let* trigger =
        match quantifier.quantifier_trigger with
        | None -> Ok None
        | Some trigger ->
            let* trigger = translate scoped trigger in
            Ok (Some (application_term trigger))
      in
      let* quantifier =
        Vir.make_boolean_quantifier
          ~sort_of_type:(function
            | Parametric_type.Int -> Ok Vir.Integer
            | Bool -> Ok Vir.Boolean
            | Parameter binder -> Ok (Vir.Parametric binder)
            | Application _ as typ
              when Parametric_type.is_spec_function typ ->
                Ok
                  (Vir.Parametric
                     (Spec_function_logic_private.binder typ))
            | Application _ as typ -> (
                match vir_aggregate_type_of_sst parametric_adts typ with
                | Some aggregate -> Ok (Vir.Aggregate aggregate)
                | None -> Error "quantifier application sort is unavailable")
            | Unit | Tuple _ | Aggregate _ ->
                Error "unsupported quantifier binder sort")
          ~schema:
            (Logic_quantifier_private.singleton
               quantifier.quantifier_metadata)
          ~binders:[ binder ]
          ~body:(bounded_quantifier_body kind binder body) ~trigger
        |> Result.map_error (fun message ->
               { span = expression.span; message })
      in
      Ok
        (Boolean
           (match kind with
           | Logic_quantifier_private.Forall -> Vir.Forall_term quantifier
           | Logic_quantifier_private.Exists -> Vir.Exists_term quantifier))
  | Sst.Let (bindings, body) ->
      let* environment =
        List.fold_left
          (fun result (pattern, value) ->
            let* environment = result in
            let* value = translate environment value in
            let* environment, _condition =
              bind_pattern parametric_adts environment pattern value
            in
            if irrefutable_pattern pattern then Ok environment
            else
                fail pattern.span
                  "recursive-spec termination requires irrefutable lets")
          (Ok environment) bindings
      in
      translate environment body
  | Sst.If (condition, consequent, Some alternative)
    -> (
      let condition_span = condition.span in
      let* condition = translate environment condition in
      let* condition = boolean condition_span condition in
      let* consequent = translate environment consequent in
      let* alternative = translate environment alternative in
      match (consequent, alternative) with
      | Boolean consequent, Boolean alternative ->
          Ok
            (Boolean
               (Vir.Boolean_or
                  ( Vir.Boolean_and (condition, consequent),
                    Vir.Boolean_and (Vir.Boolean_not condition, alternative) )))
      | Aggregate consequent, Aggregate alternative
        when consequent.aggregate_type = alternative.aggregate_type ->
          Ok
            (Aggregate
               {
                 Vir.aggregate_type = consequent.aggregate_type;
                 aggregate_desc =
                   Vir.Aggregate_conditional
                     (condition, consequent, alternative);
               })
      | Function consequent, Function alternative
        when
          Parametric_type.equal consequent.function_arrow
            alternative.function_arrow ->
          Ok
            (Function
               {
                 function_term =
                   {
                     Vir.parametric_sort =
                       consequent.function_term.parametric_sort;
                     parametric_desc =
                       Vir.Parametric_conditional
                         ( condition,
                           consequent.function_term,
                           alternative.function_term );
                   };
                 function_arrow = consequent.function_arrow;
               })
      | Integer _, Integer _ ->
          fail expression.span
            "integer recursive-spec conditionals are not termination terms"
      | _ -> fail expression.span "recursive-spec conditional sort mismatch")
  | Sst.Constructor_value { constructor; arguments } ->
      let* arguments =
        List.fold_left
          (fun result argument ->
            let* translated = result in
            let* value = translate environment argument in
            Ok (recursive_argument value :: translated))
          (Ok []) arguments
        |> Result.map List.rev
      in
      Ok
        (Aggregate
           {
             Vir.aggregate_type =
               Option.get
                 (vir_aggregate_type_of_sst parametric_adts expression.typ);
             aggregate_desc =
               Vir.Aggregate_constructor { constructor; arguments };
           })
  | Sst.Record_value { record_type; fields } ->
      let* fields =
        List.fold_left
          (fun result (field, expression) ->
            let* translated = result in
            let* value = translate environment expression in
            Ok ((field, recursive_argument value) :: translated))
          (Ok []) fields
        |> Result.map List.rev
      in
      Ok
        (Aggregate
           {
             Vir.aggregate_type =
               Option.get
                 (vir_aggregate_type_of_sst parametric_adts expression.typ);
             aggregate_desc = Vir.Aggregate_record { record_type; fields };
           })
  | Sst.Field_read { record; field } ->
      let* record = translate environment record in
      let* record = aggregate expression.span record in
      Ok
        (match field.field_owner with
        | Sst.Constructor_owner constructor ->
            selected_argument parametric_adts record constructor
              field.field_index expression.typ
        | Sst.Record_owner _ ->
            selected_field parametric_adts record field expression.typ)
  | Sst.Match (scrutinee, cases) when expression.typ = Sst.Bool ->
      translate_boolean_match parametric_adts translate environment expression
        scrutinee cases
  | _ ->
      fail expression.span
        "expression is outside scalar recursive-spec termination terms"

let path_condition parametric_adts expression environment =
  let* value = translate parametric_adts environment expression in
  boolean expression.span value

let bind_actuals parametric_adts parameters arguments environment =
  let parameters = List.map Sst.require_value_parameter parameters in
  List.fold_left2
    (fun result parameter argument ->
      let _, argument = Sst.require_value_argument argument in
      let* bound = result in
      let* value = translate parametric_adts environment argument in
      match parameter.Sst.pattern.pattern_desc with
      | Sst.Bind binding -> Ok ((binding.id, value) :: bound)
      | _ -> assert false)
    (Ok []) parameters arguments

let obligations_for_definition rank_domains definition =
  let parametric_adts = definition.parametric_adts in
  let translate = translate parametric_adts in
  let authenticated_domain = Termination.pending_domain definition.pending in
  let descriptor = definition.descriptor in
  let source = Sst_validation.callable_definition descriptor in
  let function_ref =
    {
      Vir.function_index = source.function_id.function_index;
      function_name = source.function_id.function_name;
    }
  in
  let parameters = source.parameters in
  let environment = initial_environment parametric_adts parameters in
  let decrease, structural_domain =
    match Sst_validation.callable_decrease descriptor with
    | Sst_validation.Direct_integer_decrease decrease ->
        ( Sst_validation.decrease_clause decrease
          |> Sst_validation.contract_clause_expression,
          None )
    | Sst_validation.Direct_structural_decrease (decrease, certificate) ->
        let rank_id = Sst_validation.rank_domain_id certificate in
        let domain =
          List.find_opt
            (fun domain -> String.equal (Vir.rank_domain_id domain) rank_id)
            rank_domains
        in
        ( Sst_validation.decrease_clause decrease
          |> Sst_validation.contract_clause_expression,
          domain )
    | Sst_validation.Direct_frozen_spine_decrease (decrease, _) ->
        ( Sst_validation.decrease_clause decrease
          |> Sst_validation.contract_clause_expression,
          None )
    | Sst_validation.Direct_parametric_decrease (decrease, _) ->
        ( Sst_validation.decrease_clause decrease
          |> Sst_validation.contract_clause_expression,
          None )
    | _ -> assert false
  in
  let decrease_span =
    match Sst_validation.callable_decrease descriptor with
    | Sst_validation.Direct_integer_decrease decrease
    | Sst_validation.Direct_structural_decrease (decrease, _)
    | Sst_validation.Direct_parametric_decrease (decrease, _)
    | Sst_validation.Direct_frozen_spine_decrease (decrease, _) ->
        Sst_validation.decrease_clause decrease
        |> Sst_validation.contract_clause_span
    | _ -> assert false
  in
  let* entry_value = translate environment decrease in
  let* entry =
    match (authenticated_domain, structural_domain) with
    | Termination.Integer_height, None ->
        integer decrease_span entry_value
    | Termination.Structural_rank certificate, Some domain
      when
        String.equal
          (Termination.structural_rank_id certificate)
          (Vir.rank_domain_id domain) ->
        let* aggregate = aggregate decrease_span entry_value in
        Ok (Vir.Integer_rank_project (domain, aggregate))
    | Termination.Frozen_spine_direct_edge _, None ->
        Ok (Vir.Integer_constant Z.one)
    | Termination.Parametric_direct_edge _, None ->
        Ok (Vir.Integer_constant Z.one)
    | ( ( Termination.Integer_height
      | Termination.Structural_rank _
      | Termination.Parametric_direct_edge _
      | Termination.Frozen_spine_direct_edge _ ),
      _ ) ->
        fail decrease_span
          "sealed recursive rank domain is absent or mismatched"
  in
  let ranked_measure span value =
    match (authenticated_domain, structural_domain) with
    | Termination.Integer_height, None -> integer span value
    | Termination.Structural_rank certificate, Some domain
      when
        String.equal
          (Termination.structural_rank_id certificate)
          (Vir.rank_domain_id domain) ->
        let* aggregate = aggregate span value in
        Ok (Vir.Integer_rank_project (domain, aggregate))
    | Termination.Frozen_spine_direct_edge frozen, None -> (
        match value with
        | Aggregate
            {
              Vir.aggregate_desc =
                Vir.Aggregate_selector (selector, _);
              _
            }
          when
            selector.selector_index
              = frozen.Sst.frozen_next_child_field.field_index
            && selector.selector_domain.aggregate_type_index
               = frozen.frozen_link.type_index
            && String.equal selector.selector_domain.aggregate_type_name
                 frozen.frozen_link.type_name ->
            Ok (Vir.Integer_constant Z.zero)
        | Aggregate _ -> Ok (Vir.Integer_constant Z.one)
        | Integer _ | Boolean _ | Parametric _ | Function _ ->
            fail span "frozen-spine decrease must remain an aggregate")
    | Termination.Parametric_direct_edge _, None -> (
        match value with
        | Aggregate { Vir.aggregate_desc = Vir.Aggregate_selector _; _ } ->
            Ok (Vir.Integer_constant Z.zero)
        | Aggregate _ -> Ok (Vir.Integer_constant Z.one)
        | Integer _ | Boolean _ | Parametric _ | Function _ ->
            fail span "parametric decrease must remain an aggregate")
    | ( ( Termination.Integer_height
      | Termination.Structural_rank _
      | Termination.Parametric_direct_edge _
      | Termination.Frozen_spine_direct_edge _ ),
      _ ) ->
        fail span "recursive call rank domain differs from its entry domain"
  in
  let inputs =
    List.filter_map
      (fun (_, value) ->
        match value with
        | Integer (Vir.Integer_symbol symbol)
        | Boolean (Vir.Boolean_symbol symbol)
        | Aggregate
            {
              Vir.aggregate_desc = Vir.Aggregate_symbol symbol;
              _
            } ->
            Some symbol
        | _ -> None)
      environment
  in
  let make kind span path goal =
    {
      Vir.obligation_index = 0;
      function_ref;
      kind;
      span;
      assumptions = [];
      required_preceding_safety = [];
      path_condition = List.rev path;
      goal;
      projection_symbols = inputs;
    }
  in
  let entry_obligation =
    make
      (Vir.Entry_measure_nonnegative { declaration_span = decrease_span })
      decrease_span []
      (Vir.Integer_compare
         (Vir.Less_or_equal, Vir.Integer_constant Z.zero, entry))
  in
  let rec walk environment path obligations expression =
    match expression.Sst.expression_desc with
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let _binder, value =
          quantifier_value parametric_adts quantifier.quantifier_binder
        in
        let scoped =
          (quantifier.quantifier_binder.id, value) :: environment
        in
        (* The authenticated trigger is a source subterm of the body. Walking
           the body therefore inspects recursive trigger calls exactly once. *)
        walk scoped path obligations quantifier.quantifier_body
    | Sst.If (condition, consequent, Some alternative) ->
        let* condition =
          path_condition parametric_adts condition environment
        in
        let* obligations =
          walk environment (condition :: path) obligations consequent
        in
        walk environment (Vir.Boolean_not condition :: path) obligations
          alternative
    | Sst.Let
        ( [ ( { pattern_desc = Sst.Bind _; _ },
                ({ expression_desc = Sst.Match _; _ } as matched) ); ],
          body )
      when match authenticated_domain with
        | Termination.Frozen_spine_direct_edge _ -> true
        | Termination.Parametric_direct_edge _ -> true
        | Termination.Integer_height | Termination.Structural_rank _ -> false
      ->
        let* obligations = walk environment path obligations matched in
        walk environment path obligations body
    | Sst.Let (bindings, body) ->
        let* environment =
          List.fold_left
            (fun result (pattern, value) ->
              let* environment = result in
              let* value = translate environment value in
              let* environment, _condition =
                bind_pattern parametric_adts environment pattern value
              in
              if irrefutable_pattern pattern then Ok environment
              else
                  fail pattern.span
                    "recursive-spec termination requires irrefutable lets")
            (Ok environment) bindings
        in
        walk environment path obligations body
    | Sst.Match (scrutinee, cases) ->
        List.fold_left
          (fun result (case : Sst.case) ->
            let* obligations = result in
            let* case_environment, condition =
              bind_match_pattern parametric_adts translate environment
                scrutinee case.case_pattern
            in
            let* path =
              match case.case_guard with
              | None -> Ok (condition :: path)
              | Some guard ->
                  let* guard =
                    path_condition parametric_adts guard case_environment
                  in
                  Ok (guard :: condition :: path)
            in
            walk case_environment path obligations case.case_body)
          (Ok obligations) cases
    | Sst.Direct_call
        { callee; arguments; recursive = true; call_form = Sst.Specification_call;
          _; }
      when same_function_id callee source.function_id ->
        let* actual_environment =
          bind_actuals parametric_adts parameters arguments environment
        in
        let* current_value = translate actual_environment decrease in
        let* current = ranked_measure expression.span current_value in
        let* () =
          match (authenticated_domain, entry_value, current_value, current) with
          | ( Termination.Structural_rank _,
              Aggregate entry_aggregate,
              Aggregate
                {
                  Vir.aggregate_desc =
                    Vir.Aggregate_selector (value_selector, parent);
                  _;
                },
              Vir.Integer_rank_project
                ( domain,
                  {
                    Vir.aggregate_desc =
                      Vir.Aggregate_selector (rank_selector, _);
                    _;
                  } ) )
            when
              parent = entry_aggregate
              && value_selector = rank_selector
              && Vir.rank_selector_is_positive_child domain rank_selector ->
              Ok ()
          | ( Termination.Structural_rank _,
              Aggregate entry_aggregate,
              Aggregate
                {
                  Vir.aggregate_desc =
                    Vir.Aggregate_selector (value_selector, parent);
                  _;
                },
              Vir.Integer_rank_project
                ( domain,
                  {
                    Vir.aggregate_desc =
                      Vir.Aggregate_selector (rank_selector, _);
                    _;
                  } ) ) ->
              fail expression.span
                "structural recursion child authentication mismatch \
                 (parent=%b selector=%b positive-child=%b)"
                (parent = entry_aggregate)
                (value_selector = rank_selector)
                (Vir.rank_selector_is_positive_child domain rank_selector)
          | Termination.Structural_rank _, _, _, _ ->
              fail expression.span
                "structural recursion must select an authenticated immediate \
                 child"
          | ( Termination.Frozen_spine_direct_edge _, _, _,
            Vir.Integer_constant value )
            when Z.equal value Z.zero ->
              Ok ()
          | Termination.Frozen_spine_direct_edge _, _, _, _ ->
              fail expression.span
                "frozen-spine recursion must select its exact direct child"
          | ( Termination.Parametric_direct_edge _,
              _,
              Aggregate { Vir.aggregate_desc = Vir.Aggregate_selector _; _ },
              Vir.Integer_constant value )
            when Z.equal value Z.zero ->
              Ok ()
          | Termination.Parametric_direct_edge _, _, _, _ ->
              fail expression.span
                "parametric recursion must select an authenticated direct field"
          | Termination.Integer_height, _, _, _ -> Ok ()
        in
        let nonnegative =
          make
            (Vir.Recursive_call_measure_nonnegative
               {
                 callee = function_ref;
                 declaration_span = decrease_span;
                 call_span = expression.span;
               })
            expression.span path
            (Vir.Integer_compare
               (Vir.Less_or_equal, Vir.Integer_constant Z.zero, current))
        in
        let strict =
          make
            (Vir.Recursive_call_strict_descent
               {
                 callee = function_ref;
                 declaration_span = decrease_span;
                 call_span = expression.span;
               })
            expression.span path
            (Vir.Integer_compare (Vir.Less_than, current, entry))
        in
        Ok (obligations @ [ nonnegative; strict ])
    | _ ->
        List.fold_left
          (fun result child ->
            let* obligations = result in
            walk environment path obligations child)
          (Ok obligations)
          (match expression.expression_desc with
          | Sst.Checked_arithmetic (_, children) -> children
          | Sst.Compare (_, left, right)
          | Sst.Boolean_binary (_, left, right)
          | Sst.Sequence (left, right) ->
              [ left; right ]
          | Sst.Boolean_not child -> [ child ]
          | Sst.Forall quantifier | Sst.Exists quantifier ->
              [ quantifier.quantifier_body ]
          | Sst.Direct_call { arguments; _ } ->
              List.map
                (fun argument -> snd (Sst.require_value_argument argument))
                arguments
          | Sst.Callback_call { arguments; _ }
          | Sst.Callback_requires { arguments; _ } -> List.map snd arguments
          | Sst.Callback_ensures { application = { arguments; _ }; result } ->
              List.map snd arguments @ [ result ]
          | Sst.Match (scrutinee, cases) ->
              scrutinee
              :: List.concat_map
                   (fun (case : Sst.case) ->
                     Option.to_list case.case_guard @ [ case.case_body ])
                   cases
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Variable _
          | Sst.Reveal _ | Sst.Reveal_with_fuel _ ->
              []
          | _ ->
              (* Admission rejects every remaining shape for this first slice. *)
              [])
  in
  let* obligations = walk environment [] [ entry_obligation ] definition.body in
  let recursive_path =
    List.find_map
      (fun obligation ->
        match obligation.Vir.kind with
        | Vir.Recursive_call_measure_nonnegative _ ->
            Some obligation.path_condition
        | _ -> None)
      obligations
  in
  let obligations =
    match (obligations, recursive_path) with
    | entry :: rest, Some path ->
        { entry with Vir.path_condition = path } :: rest
    | obligations, None -> obligations
    | [], Some _ -> assert false
  in
  Ok
    (List.mapi
       (fun obligation_index obligation ->
         { obligation with Vir.obligation_index })
       obligations)

let default_activations definitions proof_index =
  List.filter_map
    (fun definition ->
      let id = Sst_validation.callable_id definition.descriptor in
      if id.function_index < proof_index && definition.visibility = `Revealed
      then Some { function_id = id; depth = 1; span = definition.body.span }
      else None)
    definitions

let prepare program =
  let* validated =
    match Sst_validation.validate program with
    | Ok validated -> Ok validated
    | Error error ->
        fail error.Sst_validation.span "%s"
          (Sst_validation.error_to_string error)
  in
  let* termination =
    match Termination.prepare (Termination.analyze validated) with
    | Ok plan -> Ok plan
    | Error error -> fail error.Termination.span "%s" (Termination.error_to_string error)
  in
  let definitions =
    List.filter_map
      (fun descriptor ->
        let source = Sst_validation.callable_definition descriptor in
        match source.body with
        | Sst.Recursive_spec_definition { body = _; visibility; _ } ->
            Option.bind
              (Termination.find_pending_summary termination source.function_id)
              (fun pending ->
                match
                  Typedtree_adapter_private.Public
                  .expanded_recursive_specification
                    ~program:(Sst_validation.program validated)
                    ~definition:source
                with
                | Error _ -> None
                | Ok expanded ->
                    incr recursive_lowerings;
                    Some
                      {
                        descriptor;
                        pending;
                        stable_id = stable_id source.function_id;
                        body = expanded;
                        visibility;
                        types = (Sst_validation.program validated).types;
                        parametric_adts =
                          (Sst_validation.program validated).parametric_adts;
                      })
        | _ -> None)
      (Sst_validation.callable_descriptors validated)
  in
  let* () =
    if
      List.length definitions
      =
      List.fold_left
        (fun count descriptor ->
          match (Sst_validation.callable_definition descriptor).Sst.body with
          | Sst.Recursive_spec_definition _ -> count + 1
          | _ -> count)
        0 (Sst_validation.callable_descriptors validated)
    then Ok ()
    else fail (Diagnostic.file_span "<recursive-spec>") "missing sealed termination plan"
  in
  let rank_domains = Vir.rank_domains_of_validated validated in
  let* obligations =
    List.fold_left
      (fun result definition ->
        let* obligations = result in
        let* next = obligations_for_definition rank_domains definition in
        Ok (obligations @ next))
      (Ok []) definitions
  in
  let parametric_adts = (Sst_validation.program validated).parametric_adts in
  let* obligations =
    List.fold_left
      (fun result (obligation : Vir.obligation) ->
        let* obligations = result in
        let applications =
          Vir.obligation_aggregate_types obligation
          |> List.filter_map (fun aggregate ->
                 match aggregate.Vir.aggregate_type_arguments with
                 | [] -> None
                 | arguments ->
                     Some (aggregate.aggregate_type_index, arguments))
        in
        match
          Logical_adt_schema_private.instantiate
            ~descriptors:parametric_adts ~applications
        with
        | Ok [] -> Ok (obligation :: obligations)
        | Ok schemas ->
            Ok
              ({ obligation with
                 Vir.required_preceding_safety =
                   Vir.Logical_adt_schema schemas
                   :: obligation.required_preceding_safety }
              :: obligations)
        | Error error ->
            fail obligation.span "%s"
              (Logical_adt_schema_private.error_to_string error))
      (Ok []) obligations
    |> Result.map List.rev
  in
  Ok { validated; termination; definitions; obligations }

let validated_program prepared = prepared.validated
let termination_plan prepared = prepared.termination
let termination_obligations prepared = prepared.obligations
let definitions prepared = prepared.definitions
let proof_entry_activations prepared function_id =
  default_activations prepared.definitions function_id.Sst.function_index
let definition_id definition = Sst_validation.callable_id definition.descriptor
let definition_stable_id definition = definition.stable_id
let definition_parameters definition =
  (Sst_validation.callable_definition definition.descriptor).Sst.parameters
let definition_result_type definition =
  (Sst_validation.callable_definition definition.descriptor).Sst.result_type
let definition_body definition = definition.body
let definition_visibility definition = definition.visibility
let definition_span definition =
  (Sst_validation.callable_definition definition.descriptor).Sst.span
let definition_types definition = definition.types
let definition_parametric_adts definition = definition.parametric_adts

module For_testing = struct
  let recursive_lowering_count () = !recursive_lowerings
  let reset_recursive_lowering_count () = recursive_lowerings := 0
end

let error_to_string error =
  Printf.sprintf "%s at %s:%d:%d-%d:%d" error.message
    (Filename.basename error.span.Diagnostic.file)
    error.span.start_pos.line error.span.start_pos.column error.span.end_pos.line
    error.span.end_pos.column
