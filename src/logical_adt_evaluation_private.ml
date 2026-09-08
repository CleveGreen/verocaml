type value = Spec_function_logic_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Bit_vector_value of Vir.bit_vector_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of function_value

and function_value = Spec_function_logic_private.function_value = {
  function_term : Vir.spec_function_term;
  function_arrow : Sst.typ;
  function_closure : function_closure;
}

and function_closure = Spec_function_logic_private.function_closure =
  | Abstract_function
  | Lambda_function of {
      lambda : Spec_function_sst_private.lambda;
      lambda_environment : (int * value) list;
    }
  | Named_function of {
      definition : Sst.function_definition;
      named_arguments : (string option * value) list;
      named_argument_types : Sst.typ list;
      named_type_arguments : Sst.typ list;
    }

let vir_aggregate_type (type_id : Sst.type_id) : Vir.aggregate_type =
  {
    aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
    aggregate_type_arguments = [];
  }

let vir_aggregate_type_of_sst descriptors = function
  | Sst.Aggregate type_id -> Some (vir_aggregate_type type_id)
  | Sst.Application (constructor, arguments) -> (
      match Parametric_adt.find descriptors constructor with
      | None -> None
      | Some descriptor ->
          let type_id = Parametric_adt.type_id descriptor in
          Some
            { Vir.aggregate_type_index = type_id.type_index;
              aggregate_type_name =
                type_id.type_name ^ "<"
                ^ String.concat "," (List.map Parametric_type.to_string arguments)
                ^ ">";
              aggregate_type_arguments = arguments })
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _
  | Sst.Tuple _
  | Sst.Parameter _ ->
      None

let selector_domain aggregate selector =
  { selector with Vir.selector_domain = aggregate.Vir.aggregate_type }

let owner_type = function
  | Sst.Record_owner type_id -> type_id
  | Sst.Constructor_owner constructor -> constructor.constructor_type

let owner_namespace = function
  | Sst.Record_owner type_id ->
      Printf.sprintf "t%d_%s_record" type_id.type_index type_id.type_name
  | Sst.Constructor_owner constructor ->
      Printf.sprintf "t%d_%s_c%d_%s_inline"
        constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name

let field_selector (field : Sst.field_id) path selector_range : Vir.selector =
  {
    selector_domain = vir_aggregate_type (owner_type field.field_owner);
    selector_range;
    selector_namespace = owner_namespace field.field_owner;
    selector_index = field.field_index;
    selector_name = field.field_name;
    selector_path = path;
  }

let argument_selector (constructor : Sst.constructor_id) index path
    selector_range : Vir.selector =
  {
    selector_domain = vir_aggregate_type constructor.constructor_type;
    selector_range;
    selector_namespace =
      Printf.sprintf "t%d_%s_c%d_%s" constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name;
    selector_index = index;
    selector_name = Printf.sprintf "$arg%d" index;
    selector_path = path;
  }

let rec requires_parametric_selection = function
  | Sst.Parameter _ | Sst.Application _ -> true
  | Sst.Tuple components ->
      List.exists
        (fun (_, component) -> requires_parametric_selection component)
        components
  | Sst.Unit | Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Bit_vector _
  | Sst.Aggregate _ ->
      false

let rec selected_value_without_state aggregate make_selector path = function
  | Sst.Unit -> Unit_value
  | Sst.Int | Sst.Mathematical_int ->
      Integer_value
        (Vir.Integer_selector (make_selector path Vir.Integer, aggregate))
  | Sst.Bool ->
      Boolean_value
        (Vir.Boolean_selector (make_selector path Vir.Boolean, aggregate))
  | Sst.Bit_vector width ->
      Bit_vector_value
        (Result.get_ok
           (Vir.bv_selector
              (make_selector path (Vir.Bit_vector width)) aggregate))
  | Sst.Aggregate type_id ->
      let aggregate_type = vir_aggregate_type type_id in
      Aggregate_value
        {
          Vir.aggregate_type;
          aggregate_desc =
            Vir.Aggregate_selector
              (make_selector path (Vir.Aggregate aggregate_type), aggregate);
        }
  | Sst.Parameter _ | Sst.Application _ ->
      invalid_arg "parametric values cannot be selected from legacy aggregates"
  | Sst.Tuple components ->
      Tuple_value
        (List.mapi
           (fun index (_, typ) ->
             selected_value_without_state aggregate make_selector
               (path @ [ index ]) typ)
           components)

let rec selected_parametric_value_without_state ~aggregate_type aggregate
    make_selector path = function
  | Sst.Unit -> Unit_value
  | Sst.Int | Sst.Mathematical_int ->
      Integer_value
        (Vir.Integer_selector (make_selector path Vir.Integer, aggregate))
  | Sst.Bool ->
      Boolean_value
        (Vir.Boolean_selector (make_selector path Vir.Boolean, aggregate))
  | Sst.Bit_vector width ->
      Bit_vector_value
        (Result.get_ok
           (Vir.bv_selector
              (make_selector path (Vir.Bit_vector width)) aggregate))
  | Sst.Parameter binder ->
      let parameter = Sst.Parameter binder in
      let actual =
        if
          List.exists
            (Parametric_type.equal parameter)
            aggregate.Vir.aggregate_type.aggregate_type_arguments
        then parameter
        else
          List.nth_opt aggregate.aggregate_type.aggregate_type_arguments
            binder.ordinal
          |> Option.value ~default:parameter
      in
      (match actual with
       | Sst.Parameter actual_binder ->
           Parametric_value
             {
               Vir.parametric_sort = actual_binder;
               parametric_desc =
                 Vir.Parametric_selector
                   ( make_selector path (Vir.Parametric actual_binder),
                     aggregate );
             }
       | actual ->
           selected_parametric_value_without_state ~aggregate_type aggregate
             make_selector path actual)
  | (Sst.Aggregate _ | Sst.Application _) as typ -> (
      match aggregate_type typ with
      | None -> invalid_arg "selected ADT field has no authenticated descriptor"
      | Some selected_type ->
          Aggregate_value
            { Vir.aggregate_type = selected_type;
              aggregate_desc =
                Vir.Aggregate_selector
                  (make_selector path (Vir.Aggregate selected_type), aggregate) })
  | Sst.Tuple components ->
      Tuple_value
        (List.mapi
           (fun index (_, typ) ->
             selected_parametric_value_without_state ~aggregate_type aggregate
               make_selector (path @ [ index ]) typ)
           components)

let rec ranges_of_value typ value =
  match (typ, value) with
  | Sst.Int, Integer_value term -> Vir.integer_range term
  | Sst.Mathematical_int, Integer_value _ -> []
  | Sst.Tuple components, Tuple_value values
    when List.length components = List.length values ->
      List.map2
        (fun (_, typ) value -> ranges_of_value typ value)
        components values
      |> List.concat
  | ( Sst.Unit | Sst.Bool | Sst.Bit_vector _ | Sst.Aggregate _ | Sst.Parameter _
    | Sst.Application _ ),
    ( Unit_value | Boolean_value _ | Bit_vector_value _ | Aggregate_value _ | Parametric_value _
    | Function_value _ ) ->
      []
  | _, _ -> []

let value_of_recursive_argument = function
  | Vir.Recursive_integer_argument term -> Integer_value term
  | Vir.Recursive_boolean_argument term -> Boolean_value term
  | Vir.Recursive_bv_argument term -> Bit_vector_value term
  | Vir.Recursive_aggregate_argument term -> Aggregate_value term
  | Vir.Recursive_parametric_argument term -> Parametric_value term

let conjunction equations =
  List.fold_left
    (fun combined equation -> Vir.Boolean_and (combined, equation))
    (Vir.Boolean_constant true) equations

let exact_constructor_under assumptions aggregate =
  match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_constructor _ -> Some aggregate
  | _ ->
      List.find_map
        (function
          | Vir.Aggregate_equal
              (left, ({ Vir.aggregate_desc = Vir.Aggregate_constructor _; _ } as
                       constructed))
            when left = aggregate ->
              Some constructed
          | Vir.Aggregate_equal
              (({ Vir.aggregate_desc = Vir.Aggregate_constructor _; _ } as
                constructed),
               right)
            when right = aggregate ->
              Some constructed
          | _ -> None)
        assumptions

let rec equality left right =
  match (left, right) with
  | Integer_value left, Integer_value right ->
      Some (Vir.Integer_compare (Vir.Equal, left, right))
  | Boolean_value left, Boolean_value right ->
      Some (Vir.Boolean_equal (left, right))
  | Bit_vector_value left, Bit_vector_value right ->
      Vir.bv_equal left right |> Result.to_option
  | Unit_value, Unit_value -> Some (Vir.Boolean_constant true)
  | Aggregate_value left, Aggregate_value right
    when left.aggregate_type = right.aggregate_type ->
      aggregate_equality left right
  | Parametric_value left, Parametric_value right ->
      Parametric_logic_private.equal left right |> Result.to_option
  | Function_value left, Function_value right
    when Parametric_type.equal left.function_arrow right.function_arrow ->
      Some (Vir.Parametric_equal (left.function_term, right.function_term))
  | Tuple_value left, Tuple_value right
    when List.length left = List.length right ->
      let equations = List.map2 equality left right in
      if List.for_all Option.is_some equations then
        Some
          (List.fold_left
             (fun combined equation ->
               Vir.Boolean_and (combined, Option.get equation))
             (Vir.Boolean_constant true) equations)
      else None
  | _ -> None

and aggregate_equality left right =
  if left.Vir.aggregate_type.aggregate_type_arguments = [] then
    Some (Vir.Aggregate_equal (left, right))
  else aggregate_application_equality left right

and aggregate_application_equality left right =
  let equations_of_values left right =
    if List.length left <> List.length right then None
    else
      let equations = List.map2 equality left right in
      if List.for_all Option.is_some equations then
        Some (conjunction (List.map Option.get equations))
      else None
  in
  let against_constructor aggregate constructor arguments =
    let tag =
      Vir.Integer_compare
        ( Vir.Equal,
          Vir.Aggregate_tag (aggregate.Vir.aggregate_type, aggregate),
          Vir.Integer_constant (Z.of_int constructor.Sst.constructor_index) )
    in
    let selected =
      List.mapi
        (fun index argument ->
          let value = value_of_recursive_argument argument in
          let selector range =
            selector_domain aggregate
              (argument_selector constructor index [] range)
          in
          match value with
          | Integer_value _ ->
              Integer_value
                (Vir.Integer_selector (selector Vir.Integer, aggregate))
          | Boolean_value _ ->
              Boolean_value
                (Vir.Boolean_selector (selector Vir.Boolean, aggregate))
          | Bit_vector_value term ->
              Bit_vector_value
                (Result.get_ok
                   (Vir.bv_selector
                      (selector (Vir.Bit_vector term.bit_vector_width))
                      aggregate))
          | Aggregate_value selected ->
              Aggregate_value
                { Vir.aggregate_type = selected.aggregate_type;
                  aggregate_desc =
                    Vir.Aggregate_selector
                      (selector (Vir.Aggregate selected.aggregate_type),
                       aggregate) }
          | Parametric_value term ->
              Parametric_value
                { Vir.parametric_sort = term.parametric_sort;
                  parametric_desc =
                    Vir.Parametric_selector
                      (selector (Vir.Parametric term.parametric_sort),
                       aggregate) }
          | Unit_value | Tuple_value _ | Function_value _ -> assert false)
        arguments
    in
    Option.map
      (fun payload -> Vir.Boolean_and (tag, payload))
      (equations_of_values selected
         (List.map value_of_recursive_argument arguments))
  in
  let against_record aggregate fields =
    let equations =
      List.map
        (fun (field, argument) ->
          let value = value_of_recursive_argument argument in
          let selector range =
            selector_domain aggregate (field_selector field [] range)
          in
          let selected =
            match value with
            | Integer_value _ ->
                Integer_value
                  (Vir.Integer_selector (selector Vir.Integer, aggregate))
            | Boolean_value _ ->
                Boolean_value
                  (Vir.Boolean_selector (selector Vir.Boolean, aggregate))
            | Bit_vector_value term ->
                Bit_vector_value
                  (Result.get_ok
                     (Vir.bv_selector
                        (selector (Vir.Bit_vector term.bit_vector_width))
                        aggregate))
            | Aggregate_value value ->
                Aggregate_value
                  { Vir.aggregate_type = value.aggregate_type;
                    aggregate_desc =
                      Vir.Aggregate_selector
                        (selector (Vir.Aggregate value.aggregate_type),
                         aggregate) }
            | Parametric_value term ->
                Parametric_value
                  { Vir.parametric_sort = term.parametric_sort;
                    parametric_desc =
                      Vir.Parametric_selector
                        (selector (Vir.Parametric term.parametric_sort),
                         aggregate) }
            | Unit_value | Tuple_value _ | Function_value _ -> assert false
          in
          equality selected value)
        fields
    in
    if List.for_all Option.is_some equations then
      Some (conjunction (List.map Option.get equations))
    else None
  in
  match (left.Vir.aggregate_desc, right.Vir.aggregate_desc) with
  | ( Vir.Aggregate_constructor { constructor = left_constructor;
                                  arguments = left_arguments },
      Vir.Aggregate_constructor { constructor = right_constructor;
                                  arguments = right_arguments } ) ->
      if left_constructor = right_constructor then
        equations_of_values
          (List.map value_of_recursive_argument left_arguments)
          (List.map value_of_recursive_argument right_arguments)
      else Some (Vir.Boolean_constant false)
  | Vir.Aggregate_record { fields = left_fields; _ },
    Vir.Aggregate_record { fields = right_fields; _ }
    when List.map fst left_fields = List.map fst right_fields ->
      equations_of_values
        (List.map (fun (_, value) -> value_of_recursive_argument value) left_fields)
        (List.map (fun (_, value) -> value_of_recursive_argument value) right_fields)
  | Vir.Aggregate_constructor { constructor; arguments }, _ ->
      against_constructor right constructor arguments
  | _, Vir.Aggregate_constructor { constructor; arguments } ->
      against_constructor left constructor arguments
  | Vir.Aggregate_record { fields; _ }, _ -> against_record right fields
  | _, Vir.Aggregate_record { fields; _ } -> against_record left fields
  | _ -> Some (Vir.Aggregate_equal (left, right))

let scalar_variant_equality descriptors typ (left : Vir.aggregate_term)
    (right : Vir.aggregate_term) =
  if left.Vir.aggregate_type <> right.aggregate_type then None
  else
    match (Parametric_adt.exec_scalar_layout descriptors typ, typ) with
    | Some layout, Sst.Application (type_constructor, _) -> (
        match Parametric_adt.find descriptors type_constructor with
        | None -> None
        | Some descriptor ->
        let type_id = Parametric_adt.type_id descriptor in
        let tag aggregate index =
          Vir.Integer_compare
            ( Vir.Equal,
              Vir.Aggregate_tag (aggregate.Vir.aggregate_type, aggregate),
              Vir.Integer_constant (Z.of_int index) )
        in
        let tag_equality =
          Vir.Integer_compare
            ( Vir.Equal,
              Vir.Aggregate_tag (left.aggregate_type, left),
              Vir.Aggregate_tag (right.aggregate_type, right) )
        in
        let constructor_law (constructor, fields) =
          let constructor_id =
            { Sst.constructor_type = type_id;
              constructor_index = constructor.Parametric_adt.constructor_index;
              constructor_name = constructor.constructor_name }
          in
          let payload_equality (field, kind) =
            let range =
              match kind with
              | Parametric_adt.Scalar_bool -> Vir.Boolean
              | Scalar_int -> Vir.Integer
              | Scalar_bv width -> Vir.Bit_vector width
            in
            let selector aggregate =
              { (argument_selector constructor_id field.Parametric_adt.field_index
                   [] range) with
                Vir.selector_domain = aggregate.Vir.aggregate_type }
            in
            match kind with
            | Parametric_adt.Scalar_bool ->
                Vir.Boolean_equal
                  ( Vir.Boolean_selector (selector left, left),
                    Vir.Boolean_selector (selector right, right) )
            | Scalar_int ->
                Vir.Integer_compare
                  ( Vir.Equal,
                    Vir.Integer_selector (selector left, left),
                    Vir.Integer_selector (selector right, right) )
            | Scalar_bv width ->
                Result.get_ok
                  (Vir.bv_equal
                     (Result.get_ok
                        (Vir.bv_selector
                           { (selector left) with
                             selector_range = Vir.Bit_vector width }
                           left))
                     (Result.get_ok
                        (Vir.bv_selector
                           { (selector right) with
                             selector_range = Vir.Bit_vector width }
                           right)))
          in
          Vir.Boolean_or
            ( Vir.Boolean_not (tag left constructor.constructor_index),
              conjunction (List.map payload_equality fields) )
        in
        Some (conjunction (tag_equality :: List.map constructor_law layout)))
    | None, _
    | Some _,
      (Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _
      | Sst.Tuple _
      | Sst.Aggregate _
      | Sst.Parameter _) ->
        None

let written_constructor (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Constructor_value { constructor; _ } -> Some constructor
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Lift_runtime_int _ | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
  | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
  | Sst.Bv_binary _ | Sst.Bv_compare _
  | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _ | Sst.Field_read _
  | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ | Sst.Let_mutable _
  | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _
  | Sst.If _ | Sst.Match _ | Sst.Direct_call _ | Sst.Callback_call _
  | Sst.Callback_requires _ | Sst.Callback_ensures _ | Sst.Checked_arithmetic _
  | Sst.Compare _ | Sst.Boolean_binary _ | Sst.Boolean_not _ | Sst.Old _
  | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Reveal _
  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _ | Sst.Forall _
  | Sst.Exists _ | Sst.Symbolic_application _
  | Sst.Logical_constant_reference _ ->
      None

let rec boolean_base polarity = function
  | Vir.Boolean_not term -> boolean_base (not polarity) term
  | term -> (term, polarity)

let boolean_relation assumption =
  let rec relation equal = function
    | Vir.Forall_term _ | Vir.Exists_term _ -> None
    | Vir.Boolean_not term -> relation (not equal) term
    | Vir.Boolean_equal (left, right) -> Some (left, right, equal)
    | Vir.Boolean_not_equal (left, right) -> Some (left, right, not equal)
    | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _ -> None
    | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Boolean_and _
    | Vir.Boolean_or _ | Vir.Integer_compare _ | Vir.Boolean_selector _
    | Vir.Parametric_equal _ | Vir.Aggregate_equal _
    | Vir.Boolean_invariant_application _ | Vir.Logical_adt_schema _
    | Vir.Boolean_recursive_spec_application _
    | Vir.Boolean_symbolic_application _
    | Vir.Boolean_specification_application _ | Vir.Callback_requires _
    | Vir.Callback_ensures _ ->
        None
  in
  Option.map
    (fun (left, right, equal) ->
      let left, left_positive = boolean_base true left in
      let right, right_positive = boolean_base true right in
      let same =
        if equal then left_positive = right_positive
        else left_positive <> right_positive
      in
      (left, right, same))
    (relation true assumption)

let boolean_fact_evidence assumptions relations candidate =
  let rec component fuel seen = function
    | [] -> seen
    | _ when fuel = 0 -> seen
    | ((term, same) as current) :: pending ->
        if List.mem current seen then component fuel seen pending
        else
          let adjacent =
            List.fold_left
              (fun adjacent (left, right, relation_same) ->
                if term = left then
                  (right, if relation_same then same else not same) :: adjacent
                else if term = right then
                  (left, if relation_same then same else not same) :: adjacent
                else adjacent)
              [] relations
          in
          component (fuel - 1) (current :: seen)
            (List.rev_append adjacent pending)
  in
  let component = component 32 [] [ (candidate, true) ] in
  let evidence =
    List.fold_left
      (fun evidence (term, same) ->
        match term with
        | Vir.Boolean_constant value ->
            (if same then value else not value) :: evidence
        | Vir.Forall_term _ | Vir.Exists_term _ | Vir.Boolean_symbol _
        | Vir.Boolean_not _ | Vir.Boolean_and _ | Vir.Boolean_or _
        | Vir.Integer_compare _ | Vir.Boolean_equal _ | Vir.Boolean_not_equal _
        | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _
        | Vir.Boolean_selector _ | Vir.Parametric_equal _ | Vir.Aggregate_equal _
        | Vir.Boolean_invariant_application _ | Vir.Logical_adt_schema _
        | Vir.Boolean_recursive_spec_application _
        | Vir.Boolean_symbolic_application _
        | Vir.Boolean_specification_application _ | Vir.Callback_requires _
        | Vir.Callback_ensures _ ->
            evidence)
      [] component
  in
  List.fold_left
    (fun evidence assumption ->
      match boolean_relation assumption with
      | Some _ -> evidence
      | None ->
          let fact, positive = boolean_base true assumption in
          List.fold_left
            (fun evidence (term, same) ->
              if term = fact then
                (if same then positive else not positive) :: evidence
              else evidence)
            evidence component)
    evidence assumptions
  |> List.sort_uniq compare

let consistent_boolean_fact assumptions relations candidate =
  match boolean_fact_evidence assumptions relations candidate with
  | [ value ] -> Some value
  | [] | _ :: _ :: _ -> None

let boolean_facts_are_consistent assumptions relations =
  let candidates =
    List.fold_left
      (fun candidates assumption ->
        match boolean_relation assumption with
        | Some (left, right, _) -> left :: right :: candidates
        | None -> fst (boolean_base true assumption) :: candidates)
      [] assumptions
    |> List.sort_uniq compare
  in
  List.for_all
    (fun candidate ->
      match boolean_fact_evidence assumptions relations candidate with
      | [] | [ _ ] -> true
      | _ :: _ :: _ -> false)
    candidates

let reconstruction_evidence assumptions ~constructors ~written_expression
    ~observe =
  match written_constructor written_expression with
  | Some successor when List.mem successor constructors ->
      let relations = List.filter_map boolean_relation assumptions in
      let tag_fact constructor =
        consistent_boolean_fact assumptions relations (observe constructor)
      in
      let predecessor_facts =
        constructors
        |> List.filter_map (fun constructor ->
               if constructor = successor then None
               else Some (constructor, tag_fact constructor))
      in
      boolean_facts_are_consistent assumptions relations
      && tag_fact successor = Some false
      && List.length
           (List.filter (fun (_, fact) -> fact = Some true) predecessor_facts)
         = 1
  | Some _ | None -> false

let fields_of_owner type_definitions owner =
  let type_id =
    match owner with
    | Sst.Record_owner type_id -> type_id
    | Sst.Constructor_owner constructor -> constructor.constructor_type
  in
  match
    List.find_opt
      (fun (definition : Sst.type_definition) -> definition.type_id = type_id)
      type_definitions
  with
  | Some { type_kind = Sst.Record_definition fields; _ } -> (
      match owner with
      | Sst.Record_owner _ -> fields
      | Sst.Constructor_owner _ -> [])
  | Some { type_kind = Sst.Variant_definition constructors; _ } -> (
      match owner with
      | Sst.Record_owner _ -> []
      | Sst.Constructor_owner constructor ->
          Option.value ~default:[]
            (List.find_map
               (fun (definition : Sst.constructor_definition) ->
                 if definition.constructor_id = constructor then
                   Some definition.constructor_fields
                 else None)
               constructors))
  | None -> []

let constructors_of_field type_definitions owner field =
  match
    fields_of_owner type_definitions owner
    |> List.find_opt (fun (definition : Sst.field_definition) ->
           definition.field_id = field)
  with
  | Some { field_type = Sst.Aggregate type_id; _ } -> (
      match
        List.find_opt
          (fun (definition : Sst.type_definition) ->
            definition.type_id = type_id)
          type_definitions
      with
      | Some { type_kind = Sst.Variant_definition constructors; _ } ->
          List.map
            (fun (definition : Sst.constructor_definition) ->
              definition.constructor_id)
            constructors
      | Some { type_kind = Sst.Record_definition _; _ } | None -> [])
  | Some
      {
        field_type =
          (Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
          | Sst.Bit_vector _ | Sst.Tuple _
          | Sst.Parameter _ | Sst.Application _);
        _;
      }
  | None ->
      []

let find_integer_equation assumptions candidate =
  match
    List.find_map
      (function
        | Vir.Integer_compare (Vir.Equal, left, right) when left = candidate ->
            Some right
        | _ -> None)
      assumptions
  with
  | Some _ as found -> found
  | None ->
      List.find_map
        (function
          | Vir.Integer_compare
              ( Vir.Equal,
                (Vir.Integer_selector (selector, _) as observed),
                right )
            when right = candidate
                 && String.equal selector.selector_namespace
                      "verocaml_owned_root_scalar_v1" ->
              Some observed
          | _ -> None)
        assumptions

let find_boolean_equation assumptions candidate =
  match
    List.find_map
      (function
        | Vir.Boolean_equal (left, right) when left = candidate -> Some right
        | _ -> None)
      assumptions
  with
  | Some _ as found -> found
  | None ->
      List.find_map
        (function
          | Vir.Boolean_equal
              ((Vir.Boolean_selector (selector, _) as observed), right)
            when right = candidate
                 && String.equal selector.selector_namespace
                      "verocaml_owned_root_scalar_v1" ->
              Some observed
          | _ -> None)
        assumptions

let rec normalize_owned_integer assumptions fuel term =
  if fuel = 0 then term
  else
    match find_integer_equation assumptions term with
    | Some replacement when replacement <> term ->
        normalize_owned_integer assumptions (fuel - 1) replacement
    | Some _ | None -> term

let rec normalize_owned_boolean assumptions fuel term =
  if fuel = 0 then term
  else
    match find_boolean_equation assumptions term with
    | Some replacement when replacement <> term ->
        normalize_owned_boolean assumptions (fuel - 1) replacement
    | Some _ | None -> term

let find_aggregate_equation assumptions candidate =
  List.find_map
    (function
      | Vir.Aggregate_equal (left, right) when left = candidate -> Some right
      | _ -> None)
    assumptions

let rec normalize_owned_aggregate assumptions fuel term =
  if fuel = 0 then term
  else
    match find_aggregate_equation assumptions term with
    | Some replacement when replacement <> term ->
        normalize_owned_aggregate assumptions (fuel - 1) replacement
    | Some _ | None -> term

let owned_aggregate_at_path type_definitions assumptions root steps =
  let rec loop aggregate = function
    | [] -> Some (normalize_owned_aggregate assumptions 32 aggregate)
    | Sst.Owned_tree_field field :: rest ->
        let aggregate = normalize_owned_aggregate assumptions 32 aggregate in
        let nested_type =
          match
            fields_of_owner type_definitions field.field_owner
            |> List.find_opt (fun (definition : Sst.field_definition) ->
                   definition.field_id = field)
          with
          | Some { field_type = Sst.Aggregate type_id; _ } -> Some type_id
          | Some
              {
                field_type =
                  (Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
                  | Sst.Bit_vector _
                  | Sst.Tuple _ | Sst.Parameter _ | Sst.Application _);
                _;
              }
          | None ->
              None
        in
        Option.bind nested_type (fun nested_type ->
            let nested_type = vir_aggregate_type nested_type in
            let expected =
              {
                Vir.aggregate_type = nested_type;
                aggregate_desc =
                  Vir.Aggregate_selector
                    ( field_selector field [] (Vir.Aggregate nested_type),
                      aggregate );
              }
            in
            Option.bind (find_aggregate_equation assumptions expected) (fun next ->
                loop next rest))
    | Sst.Owned_tree_constructor constructor :: rest ->
        let aggregate = normalize_owned_aggregate assumptions 32 aggregate in
        let aggregate_type = vir_aggregate_type constructor.constructor_type in
        let expected =
          {
            Vir.aggregate_type;
            aggregate_desc =
              Vir.Aggregate_selector
                ( argument_selector constructor 0 []
                    (Vir.Aggregate aggregate_type),
                  aggregate );
          }
        in
        Option.bind (find_aggregate_equation assumptions expected) (fun next ->
            loop next rest)
  in
  loop root steps

let owned_scalar_aggregate_root_is_flat (aggregate : Vir.aggregate_term) =
  match aggregate.aggregate_desc with
  | Vir.Aggregate_symbol _ -> true
  | Vir.Aggregate_selector _ | Vir.Aggregate_constructor _
  | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
  | Vir.Aggregate_symbolic_application _
  | Vir.Aggregate_imported_model_application _
  | Vir.Aggregate_recursive_spec_application _ ->
      false

let rec owned_scalar_integer_is_flat = function
  | Vir.Integer_constant _ | Vir.Integer_symbol _ -> true
  | Vir.Integer_add (left, right) | Vir.Integer_subtract (left, right)
  | Vir.Integer_multiply (left, right) ->
      owned_scalar_integer_is_flat left && owned_scalar_integer_is_flat right
  | Vir.Integer_negate operand | Vir.Integer_absolute_value operand
  | Vir.Integer_multiply_constant (_, operand) ->
      owned_scalar_integer_is_flat operand
  | Vir.Integer_selector (selector, root) ->
      String.equal selector.selector_namespace "verocaml_owned_root_scalar_v1"
      && owned_scalar_aggregate_root_is_flat root
  | Vir.Integer_rank_project _ | Vir.Aggregate_tag _ | Vir.Integer_conditional _
  | Vir.Integer_symbolic_application _
  | Vir.Integer_recursive_spec_application _
  | Vir.Integer_bv_to_int_unsigned _ | Vir.Integer_bv_to_int_signed _ ->
      false

and owned_scalar_boolean_is_flat = function
  | Vir.Forall_term _ | Vir.Exists_term _ -> false
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Logical_adt_schema _ ->
      true
  | Vir.Boolean_not operand -> owned_scalar_boolean_is_flat operand
  | Vir.Boolean_and (left, right)
  | Vir.Boolean_or (left, right)
  | Vir.Boolean_equal (left, right)
  | Vir.Boolean_not_equal (left, right) ->
      owned_scalar_boolean_is_flat left && owned_scalar_boolean_is_flat right
  | Vir.Integer_compare (_, left, right) ->
      owned_scalar_integer_is_flat left && owned_scalar_integer_is_flat right
  | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _ -> false
  | Vir.Boolean_selector (selector, root) ->
      String.equal selector.selector_namespace "verocaml_owned_root_scalar_v1"
      && owned_scalar_aggregate_root_is_flat root
  | Vir.Parametric_equal (left, right) ->
      List.for_all owned_scalar_boolean_is_flat
        (Parametric_logic_private.conditions left
        @ Parametric_logic_private.conditions right)
  | Vir.Aggregate_equal _ | Vir.Boolean_invariant_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _ | Vir.Callback_requires _
  | Vir.Callback_ensures _ ->
      false
