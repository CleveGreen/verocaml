include Logical_adt_evaluation_private
type model_capability = Logical_spec_capability_private.model_capability
type contract_clause_kind = Logical_spec_capability_private.contract_clause_kind = Requires | Ensures
type root_identity = Logical_spec_capability_private.root_identity
type call_target = Logical_spec_capability_private.call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of model_capability
  | Opaque_recursive
  | Unsupported
type permit = Logical_spec_authentication_private.permit
type strict_permit = Logical_spec_capability_private.permit
let authenticate = Logical_spec_authentication_private.authenticate
let classify_definition = Logical_spec_authentication_private.classify_definition
let authenticate_invariant_contract = Logical_spec_admission_private.admit
let root_identity_to_string = Logical_spec_capability_private.root_identity_to_string
let authenticate_expression = Logical_spec_authentication_private.authenticate_expression
type ('context, 'state, 'error) callbacks = {
  classify : Sst.function_id -> call_target;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  option_instance : Sst.typ -> Parametric_adt.option_instance option;
  environment : 'state -> (int * value) list;
  with_environment : 'state -> (int * value) list -> 'state;
  assume : 'state -> Vir.boolean_term list -> 'state;
  observe_field_read : 'context -> 'state -> Sst.field_id -> Vir.aggregate_term -> value -> 'state;
  enter_definition : 'context -> Sst.function_definition -> 'context;
  evaluate_recursive : 'context -> Sst.expression -> 'state -> (value * 'state, 'error) result;
  error : Diagnostic.span -> string -> 'error;
}
let ( let* ) result continuation = match result with
  | Ok value -> continuation value | Error _ as error -> error
let vir_comparison = function
  | Sst.Equal -> Vir.Equal
  | Sst.Not_equal -> Vir.Not_equal
  | Sst.Less_than -> Vir.Less_than
  | Sst.Less_or_equal -> Vir.Less_or_equal
  | Sst.Greater_than -> Vir.Greater_than
  | Sst.Greater_or_equal -> Vir.Greater_or_equal
let recursive_argument error span =
  Spec_function_logic_private.recursive_argument ~error ~span
let application_term error span =
  Spec_function_logic_private.application_term ~error ~span
let boolean_conditional condition consequent alternative =
  Vir.Boolean_or (Vir.Boolean_and (condition, consequent),
    Vir.Boolean_and (Vir.Boolean_not condition, alternative))
let conjunction = function
  | [] -> Vir.Boolean_constant true
  | first :: rest -> List.fold_left
      (fun combined term -> Vir.Boolean_and (combined, term)) first rest
let quantified_body kind binder body =
  match binder.Vir.sort with
  | Vir.Integer ->
      let range = conjunction (Vir.integer_range (Vir.Integer_symbol binder)) in
      (match kind with
      | Logic_quantifier_private.Forall ->
          Vir.Boolean_or (Vir.Boolean_not range, body)
      | Logic_quantifier_private.Exists -> Vir.Boolean_and (range, body))
  | Vir.Boolean | Vir.Aggregate _ | Vir.Parametric _ -> body
let quantified_value callbacks =
  Spec_function_logic_private.quantified_value
    ~aggregate_type:callbacks.aggregate_type ~error:callbacks.error
let rec conditional error span condition consequent alternative =
  match (condition, consequent, alternative) with
  | Vir.Boolean_constant true, consequent, _ -> Ok consequent
  | Vir.Boolean_constant false, _, alternative -> Ok alternative
  | _, Unit_value, Unit_value -> Ok Unit_value
  | _, Integer_value consequent, Integer_value alternative ->
      Ok
        (Integer_value
           (Vir_integer_conditional_private.create_formula ~condition ~consequent ~alternative))
  | _, Parametric_value consequent, Parametric_value alternative ->
      Parametric_logic_private.conditional condition consequent alternative
      |> Result.map (fun value -> Parametric_value value)
      |> Result.map_error (error span)
  | _, Function_value consequent, Function_value alternative ->
      Spec_function_logic_private.conditional condition consequent alternative
      |> Result.map (fun value -> Function_value value)
      |> Result.map_error (error span)
  | _, Boolean_value consequent, Boolean_value alternative ->
      Ok (Boolean_value (boolean_conditional condition consequent alternative))
  | _, Aggregate_value consequent, Aggregate_value alternative
    when consequent.aggregate_type = alternative.aggregate_type ->
      Ok
        (Aggregate_value
           {
             Vir.aggregate_type = consequent.aggregate_type;
             aggregate_desc =
               Vir.Aggregate_conditional (condition, consequent, alternative);
           })
  | _, Tuple_value consequent, Tuple_value alternative
    when List.length consequent = List.length alternative ->
      let rec components combined consequent alternative =
        match (consequent, alternative) with
        | [], [] -> Ok (Tuple_value (List.rev combined))
        | then_ :: consequent, else_ :: alternative ->
            let* value = conditional error span condition then_ else_ in
            components (value :: combined) consequent alternative
        | _ -> assert false
      in
      components [] consequent alternative
  | _ -> Error (error span "logical conditional branch type mismatch")
let resolve_optional ~error ~default ~presence ~payload =
  let rec merge condition consequent alternative =
    match (consequent, alternative) with
    | Unit_value, Unit_value -> Ok Unit_value
    | Integer_value consequent, Integer_value alternative ->
        Ok
          (Integer_value
             (Vir_integer_conditional_private.create_formula ~condition ~consequent ~alternative))
    | Boolean_value consequent, Boolean_value alternative ->
        Ok
          (Boolean_value (boolean_conditional condition consequent alternative))
    | Parametric_value consequent, Parametric_value alternative ->
        Parametric_logic_private.conditional condition consequent alternative
        |> Result.map (fun value -> Parametric_value value)
        |> Result.map_error error
    | Tuple_value consequent, Tuple_value alternative
      when List.length consequent = List.length alternative ->
        let rec loop values consequent alternative =
          match (consequent, alternative) with
          | [], [] -> Ok (Tuple_value (List.rev values))
          | consequent :: consequents, alternative :: alternatives ->
              let* value = merge condition consequent alternative in
              loop (value :: values) consequents alternatives
          | _ -> assert false
        in
        loop [] consequent alternative
    | _ -> Error (error "optional default type mismatch")
  in
  match presence with
  | Vir.Boolean_constant false -> Ok default
  | Vir.Boolean_constant true -> Ok payload
  | condition -> merge condition payload default
let bind_pattern aggregate_type error environment (pattern : Sst.pattern) value
    =
  let rec bind environment pattern value =
    match (pattern.Sst.pattern_desc, value) with
    | Sst.Wildcard, _ | Sst.Unit_pattern, Unit_value -> Ok environment
    | Sst.Bind binding, value -> Ok ((binding.id, value) :: environment)
    | Sst.Tuple_pattern patterns, Tuple_value values
      when List.length patterns = List.length values ->
        List.fold_left2
          (fun result (_, (pattern : Sst.pattern)) value ->
            let* environment = result in
            bind environment pattern value)
          (Ok environment) patterns values
    | Sst.Record_pattern fields, Aggregate_value aggregate ->
        List.fold_left
          (fun result (field, (pattern : Sst.pattern)) ->
            let* environment = result in
            let selected =
              selected_parametric_value_without_state ~aggregate_type aggregate
                (fun path sort ->
                  selector_domain aggregate (field_selector field path sort))
                [] pattern.Sst.typ
            in
            bind environment pattern selected)
          (Ok environment) fields
    | _ -> Error (error pattern.span "logical binding pattern type mismatch")
  in
  bind environment pattern value
let pattern_condition aggregate_type error (pattern : Sst.pattern) value =
  let rec translate environment (pattern : Sst.pattern) value =
    match (pattern.Sst.pattern_desc, value) with
    | Sst.Wildcard, _ | Sst.Unit_pattern, Unit_value ->
        Ok (Vir.Boolean_constant true, environment)
    | Sst.Bind binding, value ->
        Ok (Vir.Boolean_constant true, (binding.id, value) :: environment)
    | Sst.Int_pattern expected, Integer_value actual ->
        Ok
          ( Vir.Integer_compare
              (Vir.Equal, actual, Vir.Integer_constant expected),
            environment )
    | Sst.Bool_pattern expected, Boolean_value actual ->
        Ok
          ( Vir.Boolean_equal (actual, Vir.Boolean_constant expected),
            environment )
    | Sst.Tuple_pattern patterns, Tuple_value values
      when List.length patterns = List.length values ->
        translate_many environment (List.map snd patterns) values
    | Sst.Record_pattern fields, Aggregate_value aggregate ->
        let patterns, values, reconstructed_fields =
          fields
          |> List.fold_left
               (fun (patterns, values, reconstructed)
                    (field, (nested : Sst.pattern)) ->
                 let selected =
                   selected_parametric_value_without_state ~aggregate_type
                     aggregate
                     (fun path sort ->
                       selector_domain aggregate
                         (field_selector field path sort))
                     [] nested.Sst.typ
                 in
                 ( nested :: patterns,
                   selected :: values,
                   (field, selected) :: reconstructed ))
               ([], [], [])
        in
        let patterns = List.rev patterns and values = List.rev values in
        let* nested, environment = translate_many environment patterns values in
        let* reconstructed_fields =
          List.fold_left
            (fun result (field, value) ->
              let* fields = result in
              let* value = recursive_argument error pattern.span value in
              Ok ((field, value) :: fields))
            (Ok []) reconstructed_fields
        in
        let record_type =
          match fields with
          | (field, _) :: _ -> owner_type field.Sst.field_owner
          | [] -> assert false
        in
        let reconstructed =
          {
            Vir.aggregate_type = aggregate.aggregate_type;
            aggregate_desc =
              Vir.Aggregate_record
                { record_type; fields = reconstructed_fields };
          }
        in
        Ok
          ( Vir.Boolean_and
              (Vir.Aggregate_equal (aggregate, reconstructed), nested),
            environment )
    | ( Sst.Constructor_pattern (constructor, arguments),
        Aggregate_value aggregate ) ->
        let tag =
          Vir.Integer_compare
            ( Vir.Equal,
              Vir.Aggregate_tag (aggregate.aggregate_type, aggregate),
              Vir.Integer_constant (Z.of_int constructor.constructor_index) )
        in
        let values =
          List.mapi
            (fun index (nested : Sst.pattern) ->
              selected_parametric_value_without_state ~aggregate_type aggregate
                (fun path sort ->
                  selector_domain aggregate
                    (argument_selector constructor index path sort))
                [] nested.Sst.typ)
            arguments
        in
        let* nested, environment =
          translate_many environment arguments values
        in
        let* reconstructed_arguments =
          List.fold_left
            (fun result value ->
              let* arguments = result in
              let* value = recursive_argument error pattern.span value in
              Ok (value :: arguments))
            (Ok []) values
        in
        let reconstructed =
          {
            Vir.aggregate_type = aggregate.aggregate_type;
            aggregate_desc =
              Vir.Aggregate_constructor
                { constructor; arguments = List.rev reconstructed_arguments };
          }
        in
        Ok
          ( Vir.Boolean_and
              ( tag,
                Vir.Boolean_and
                  (Vir.Aggregate_equal (aggregate, reconstructed), nested) ),
            environment )
    | _ -> Error (error pattern.span "logical match pattern type mismatch")
  and translate_many environment (patterns : Sst.pattern list) values =
    match (patterns, values) with
    | [], [] -> Ok (Vir.Boolean_constant true, environment)
    | pattern :: patterns, value :: values ->
        let* condition, environment = translate environment pattern value in
        let* rest, environment = translate_many environment patterns values in
        Ok (Vir.Boolean_and (condition, rest), environment)
    | _ -> assert false
  in
  translate [] pattern value
type ('context, 'state, 'error) runtime = {
  authorization : Logical_spec_authentication_private.authorization;
  callbacks : ('context, 'state, 'error) callbacks;
  current_model : model_capability option;
}
let with_strict_permit runtime ordinary strict =
  match runtime.authorization with
  | Logical_spec_authentication_private.Ordinary _ -> ordinary
  | Strict (permit, validated) -> strict permit validated
let authorized_definition runtime callee result_type =
  Logical_spec_authentication_private.authorize_definition runtime.authorization
    ~classify:runtime.callbacks.classify callee ~result_type
let uncovered_direct_call runtime callbacks context (expression : Sst.expression) state callee =
  if Logical_spec_authentication_private.is_strict runtime.authorization then
    Error
      (callbacks.error expression.Sst.span
         "strict logical permit does not cover the call target")
  else
    match callbacks.classify callee with
    | Opaque_recursive -> callbacks.evaluate_recursive context expression state
    | Local_nonrecursive _ | Captured_model _ | Unsupported ->
        Error
          (callbacks.error expression.span
             "logical Spec permit does not cover the call target")
let authorized_reference runtime (expression : Sst.expression) callee =
  let* authorized =
    authorized_definition runtime callee expression.typ
    |> Result.map_error (runtime.callbacks.error expression.span)
  in
  match authorized with
  | Some authorized -> Ok authorized
  | None ->
      Error
        (runtime.callbacks.error expression.span
           "specification-function reference lacks same-unit authority")
let value_of_function_application callbacks error span =
  Spec_function_logic_private.value_of_application
    ~aggregate_type:callbacks.aggregate_type ~error ~span
let expect_integer runtime span = function
  | Integer_value value -> Ok value
  | _ -> Error (runtime.callbacks.error span "expected integer logical value")
let expect_boolean runtime span = function
  | Boolean_value value -> Ok value
  | _ -> Error (runtime.callbacks.error span "expected Boolean logical value")
let validate_aggregate runtime span descriptor typ actual =
  with_strict_permit runtime (Ok ()) (fun permit validated ->
      Logical_spec_capability_private.validate_aggregate_value permit ~validated
        ~descriptor typ actual
      |> Result.map_error (runtime.callbacks.error span))
let option_descriptors runtime expression missing =
  match
    ( runtime.callbacks.option_instance expression.Sst.typ,
      runtime.callbacks.aggregate_type expression.typ )
  with
  | Some instance, Some aggregate_type ->
      let* () =
        with_strict_permit runtime (Ok ()) (fun permit validated ->
            Logical_spec_capability_private.validate_option_value permit
              ~validated expression.typ instance aggregate_type
            |> Result.map_error
                 (runtime.callbacks.error expression.Sst.span))
      in
      Ok (instance, aggregate_type)
  | (None | Some _), (None | Some _) ->
      Error (runtime.callbacks.error expression.span missing)
let optional_constructor runtime expression state constructor arguments =
  let* instance, aggregate_type =
    option_descriptors runtime expression
      "optional value lacks the pinned compiler descriptor"
  in
  let descriptor = instance.Parametric_adt.option_descriptor in
  let constructor : Parametric_adt.constructor = constructor instance in
  let constructor =
    {
      Sst.constructor_type = Parametric_adt.type_id descriptor;
      constructor_index = constructor.constructor_index;
      constructor_name = constructor.constructor_name;
    }
  in
  Ok
    ( Aggregate_value
        {
          Vir.aggregate_type;
          aggregate_desc = Vir.Aggregate_constructor { constructor; arguments };
        },
      state )
let authorize_field_read runtime (expression : Sst.expression) field =
  with_strict_permit runtime (Ok true) (fun permit validated ->
      Logical_spec_capability_private.authorize_field_read permit ~validated
        ~current_model:runtime.current_model field ~result_type:expression.Sst.typ
      |> Result.map_error (runtime.callbacks.error expression.span))
let observe_field_read callbacks observe context state field aggregate value =
  if observe then callbacks.observe_field_read context state field aggregate value
  else state
let field_read_eval runtime context (expression : Sst.expression) state field aggregate =
  let* observe = authorize_field_read runtime expression field in
  let conditional_assumptions = ref [] in
  let required_equality left right = match equality left right with
    | Some equality -> Ok equality
    | None -> Error (runtime.callbacks.error expression.span "conditional field type mismatch")
  in
  let rec select aggregate = match aggregate.Vir.aggregate_desc with
    | Vir.Aggregate_record { fields; _ } -> (
        match List.find_opt (fun (candidate, _) -> candidate = field) fields with
        | Some (_, argument) -> Ok (value_of_recursive_argument argument)
        | None -> select_symbolically aggregate)
    | Vir.Aggregate_conditional (condition, consequent, alternative) ->
        let* selected = select_symbolically aggregate in
        let* consequent = select consequent in
        let* alternative = select alternative in
        let* consequent_equality = required_equality selected consequent in
        let* alternative_equality = required_equality selected alternative in
        conditional_assumptions := boolean_conditional condition consequent_equality
          alternative_equality :: !conditional_assumptions;
        Ok selected
    | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
    | Vir.Aggregate_constructor _ | Vir.Aggregate_imported_model_application _
    | Vir.Aggregate_recursive_spec_application _
    | Vir.Aggregate_symbolic_application _ -> select_symbolically aggregate
  and select_symbolically aggregate =
    if aggregate.Vir.aggregate_type.aggregate_type_arguments = [] then
      Ok (selected_value_without_state aggregate (field_selector field) [] expression.typ)
    else Ok (selected_parametric_value_without_state
      ~aggregate_type:runtime.callbacks.aggregate_type aggregate
      (fun path sort -> selector_domain aggregate (field_selector field path sort))
      [] expression.typ)
  in
  let* value = select aggregate in
  let state = runtime.callbacks.assume state !conditional_assumptions in
  Ok (value, observe_field_read runtime.callbacks observe context state field aggregate value)
let rec evaluate_list runtime context state expressions =
  match expressions with
  | [] -> Ok ([], state)
  | expression :: rest ->
      let* value, state = expression_eval runtime context expression state in
      let* values, state = evaluate_list runtime context state rest in
      Ok (value :: values, state)
and expression_eval runtime context (expression : Sst.expression) state =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let recurse = expression_eval runtime context in
  match expression.expression_desc with
  | Sst.Int_constant value ->
      Ok (Integer_value (Vir.Integer_constant value), state)
  | Sst.Bool_constant value ->
      Ok (Boolean_value (Vir.Boolean_constant value), state)
  | Sst.Unit_constant -> Ok (Unit_value, state)
  | Sst.Variable { binding; _ } -> (
      match List.assoc_opt binding.id (callbacks.environment state) with
      | Some value -> Ok (value, state)
      | None ->
          Error (error expression.span ("unbound variable " ^ binding.name)))
  | Sst.Tuple_value components ->
      let* values, state =
        evaluate_list runtime context state (List.map snd components)
      in
      Ok (Tuple_value values, state)
  | Sst.Record_value { record_type; fields } ->
      let* values, state =
        evaluate_list runtime context state (List.map snd fields)
      in
      let rec arguments converted fields values =
        match (fields, values) with
        | [], [] -> Ok (List.rev converted)
        | (field, _) :: fields, value :: values ->
            let* value = recursive_argument error expression.span value in
            arguments ((field, value) :: converted) fields values
        | _ -> assert false
      in
      let* fields = arguments [] fields values in
      let* aggregate_type =
        match callbacks.aggregate_type expression.typ with
        | Some aggregate_type -> Ok aggregate_type
        | None ->
            Error
              (error expression.span
                 "record has no authenticated ADT descriptor")
      in
      let* () =
        validate_aggregate runtime expression.span record_type expression.typ
          aggregate_type
      in
      Ok
        ( Aggregate_value
            {
              Vir.aggregate_type;
              aggregate_desc = Vir.Aggregate_record { record_type; fields };
            },
          state )
  | Sst.Constructor_value { constructor; arguments } ->
      let* values, state = evaluate_list runtime context state arguments in
      let rec convert converted = function
        | [] -> Ok (List.rev converted)
        | value :: rest ->
            let* value = recursive_argument error expression.span value in
            convert (value :: converted) rest
      in
      let* arguments = convert [] values in
      let* aggregate_type =
        match callbacks.aggregate_type expression.typ with
        | Some aggregate_type -> Ok aggregate_type
        | None ->
            Error
              (error expression.span
                 "constructor has no authenticated ADT descriptor")
      in
      let* () =
        validate_aggregate runtime expression.span constructor.constructor_type
          expression.typ aggregate_type
      in
      Ok
        ( Aggregate_value
            {
              Vir.aggregate_type;
              aggregate_desc =
                Vir.Aggregate_constructor { constructor; arguments };
            },
          state )
  | Sst.Field_read { record; field } -> (
      let* record, state = recurse record state in
      match record with
      | Aggregate_value aggregate -> field_read_eval runtime context expression state field aggregate
      | _ -> Error (error expression.span "field read source is not aggregate"))
  | Sst.Let (bindings, body) ->
      let outer_environment = callbacks.environment state in
      let* values, state =
        evaluate_list runtime context state (List.map snd bindings)
      in
      let* environment =
        List.fold_left2
          (fun result (pattern, _) value ->
            let* environment = result in
            bind_pattern callbacks.aggregate_type error environment pattern
              value)
          (Ok outer_environment) bindings values
      in
      let* value, state =
        expression_eval runtime context body
          (callbacks.with_environment state environment)
      in
      Ok (value, callbacks.with_environment state outer_environment)
  | Sst.Sequence (first, second) -> (
      let* first, state = recurse first state in
      match first with
      | Unit_value -> recurse second state
      | _ -> Error (error expression.span "logical sequence prefix is not unit")
      )
  | Sst.If (condition, consequent, Some alternative) ->
      let* condition, state = recurse condition state in
      let* condition = expect_boolean runtime expression.span condition in
      let* consequent, state = recurse consequent state in
      let* alternative, state = recurse alternative state in
      let* value =
        conditional error expression.span condition consequent alternative
      in
      Ok (value, state)
  | Sst.Match (scrutinee, cases) -> (
      let* scrutinee, state = recurse scrutinee state in
      let outer_environment = callbacks.environment state in
      let rec evaluate_cases translated state = function
        | [] -> Ok (List.rev translated, state)
        | (case : Sst.case) :: rest ->
            let* condition, bindings =
              pattern_condition callbacks.aggregate_type error case.case_pattern
                scrutinee
            in
            let case_state =
              callbacks.with_environment state (bindings @ outer_environment)
            in
            let* condition, case_state =
              match case.case_guard with
              | None -> Ok (condition, case_state)
              | Some guard ->
                  let* guard, case_state =
                    expression_eval runtime context guard case_state
                  in
                  let* guard = expect_boolean runtime case.case_span guard in
                  Ok (Vir.Boolean_and (condition, guard), case_state)
            in
            let* body, state =
              expression_eval runtime context case.case_body case_state
            in
            let state = callbacks.with_environment state outer_environment in
            evaluate_cases ((condition, body) :: translated) state rest
      in
      let* cases, state = evaluate_cases [] state cases in
      let coverage =
        cases |> List.map fst |> function
        | [] -> Vir.Boolean_constant false
        | condition :: rest ->
            List.fold_left
              (fun coverage condition -> Vir.Boolean_or (coverage, condition))
              condition rest
      in
      let state = callbacks.assume state [ coverage ] in
      match List.rev cases with
      | [] -> Error (error expression.span "logical match has no cases")
      | (_, fallback) :: remaining ->
          let* value =
            List.fold_left
              (fun result (condition, body) ->
                let* alternative = result in
                conditional error expression.span condition body alternative)
              (Ok fallback) remaining
          in
          Ok (value, state))
  | _ -> scalar_eval runtime context expression state
and checked_arithmetic_eval runtime context (expression : Sst.expression) state
    operation arguments =
  let callbacks = runtime.callbacks in
  let recurse = expression_eval runtime context in
  let binary_integer constructor left right =
    let* left, state = recurse left state in
    let* left = expect_integer runtime expression.span left in
    let* right, state = recurse right state in
    let* right = expect_integer runtime expression.span right in
    Ok (Integer_value (constructor left right), state)
  in
  match (operation, arguments) with
  | Sst.Add, [ left; right ] ->
      binary_integer (fun left right -> Vir.Integer_add (left, right)) left right
  | Sst.Subtract, [ left; right ] ->
      binary_integer
        (fun left right -> Vir.Integer_subtract (left, right))
        left right
  | Sst.Negate, [ operand ] ->
      let* operand, state = recurse operand state in
      let* operand = expect_integer runtime expression.span operand in
      Ok (Integer_value (Vir.Integer_negate operand), state)
  | Sst.Multiply_constant coefficient, [ operand ] ->
      let* operand, state = recurse operand state in
      let* operand = expect_integer runtime expression.span operand in
      Ok
        (Integer_value (Vir.Integer_multiply_constant (coefficient, operand)),
         state)
  | Sst.Successor, [ operand ] ->
      let* operand, state = recurse operand state in
      let* operand = expect_integer runtime expression.span operand in
      Ok
        (Integer_value
           (Vir.Integer_add (operand, Vir.Integer_constant Z.one)),
         state)
  | Sst.Predecessor, [ operand ] ->
      let* operand, state = recurse operand state in
      let* operand = expect_integer runtime expression.span operand in
      Ok
        (Integer_value
           (Vir.Integer_subtract (operand, Vir.Integer_constant Z.one)),
         state)
  | Sst.Absolute_value, [ operand ] ->
      let* operand, state = recurse operand state in
      let* operand = expect_integer runtime expression.span operand in
      Ok (Integer_value (Vir.Integer_absolute_value operand), state)
  | _ ->
      Error
        (callbacks.error expression.span "malformed logical arithmetic")
and quantifier_eval runtime context (expression : Sst.expression) state
    quantifier =
  let callbacks = runtime.callbacks in
  let recurse = expression_eval runtime context in
  let kind = Logic_quantifier_private.kind quantifier.Sst.quantifier_metadata in
  let outer_environment = callbacks.environment state in
  let* binder, value =
    quantified_value callbacks quantifier.quantifier_binder
  in
  let scoped =
    callbacks.with_environment state
      ((quantifier.quantifier_binder.id, value) :: outer_environment)
  in
  let* body, body_state = recurse quantifier.quantifier_body scoped in
  let* body = expect_boolean runtime expression.span body in
  let* trigger, inner =
    match quantifier.quantifier_trigger with
    | None -> Ok (None, body_state)
    | Some source ->
        let trigger_state =
          callbacks.with_environment body_state (callbacks.environment scoped)
        in
        let* trigger, trigger_state = recurse source trigger_state in
        let* trigger = application_term callbacks.error source.span trigger in
        Ok (Some trigger, trigger_state)
  in
  let* quantifier_term =
    Vir.make_boolean_quantifier
      ~sort_of_type:(fun typ ->
        match typ with
        | Parametric_type.Int -> Ok Vir.Integer
        | Bool -> Ok Vir.Boolean
        | Parameter binder -> Ok (Vir.Parametric binder)
        | Application _ as typ when Parametric_type.is_spec_function typ ->
            Ok (Vir.Parametric (Spec_function_logic_private.binder typ))
        | Application _ -> (
            match callbacks.aggregate_type typ with
            | Some aggregate -> Ok (Vir.Aggregate aggregate)
            | None -> Error "quantifier application sort is unavailable")
        | Unit | Tuple _ | Aggregate _ ->
            Error "unsupported quantifier binder sort")
      ~schema:
        (Logic_quantifier_private.singleton quantifier.quantifier_metadata)
      ~binders:[ binder ] ~body:(quantified_body kind binder body) ~trigger
    |> Result.map_error (callbacks.error expression.span)
  in
  Ok
    ( Boolean_value
        (match kind with
        | Logic_quantifier_private.Forall -> Vir.Forall_term quantifier_term
        | Logic_quantifier_private.Exists -> Vir.Exists_term quantifier_term),
      callbacks.with_environment inner outer_environment )
and scalar_eval runtime context (expression : Sst.expression) state =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let recurse = expression_eval runtime context in
  match expression.expression_desc with
  | Sst.Checked_arithmetic (operation, arguments) ->
      checked_arithmetic_eval runtime context expression state operation
        arguments
  | Sst.Compare (comparison, left, right) ->
      let* left, state = recurse left state in
      let* right, state = recurse right state in
      let* comparison =
        match comparison with
        | Sst.Equal | Sst.Not_equal -> (
            match equality left right with
            | Some equality ->
                Ok
                  (if comparison = Sst.Equal then equality
                   else Vir.Boolean_not equality)
            | None -> Error (error expression.span "comparison type mismatch"))
        | Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
        | Sst.Greater_or_equal ->
            let* left = expect_integer runtime expression.span left in
            let* right = expect_integer runtime expression.span right in
            Ok (Vir.Integer_compare (vir_comparison comparison, left, right))
      in
      Ok (Boolean_value comparison, state)
  | Sst.Boolean_not operand ->
      let* operand, state = recurse operand state in
      let* operand = expect_boolean runtime expression.span operand in
      Ok (Boolean_value (Vir.Boolean_not operand), state)
  | Sst.Boolean_binary (operation, left, right) ->
      let* left, state = recurse left state in
      let* left = expect_boolean runtime expression.span left in
      let* right, state = recurse right state in
      let* right = expect_boolean runtime expression.span right in
      Ok
        ( Boolean_value
            (match operation with
            | Sst.And -> Vir.Boolean_and (left, right)
            | Sst.Or -> Vir.Boolean_or (left, right)),
          state )
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      quantifier_eval runtime context expression state quantifier
  | Sst.Optional_absent ->
      optional_constructor runtime expression state
        (fun instance -> instance.Parametric_adt.option_absent)
        []
  | Sst.Optional_present payload ->
      let* payload, state = recurse payload state in
      let* argument = recursive_argument error expression.span payload in
      optional_constructor runtime expression state
        (fun instance -> instance.Parametric_adt.option_present)
        [ argument ]
  | Sst.Optional_forward carrier ->
      let* carrier, state = recurse carrier state in
      let* _, expected =
        option_descriptors runtime expression
          "optional forwarding lacks the pinned compiler descriptor"
      in
      (match carrier with
      | Aggregate_value aggregate when aggregate.aggregate_type = expected ->
          Ok (carrier, state)
      | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
      | Aggregate_value _ | Parametric_value _ | Function_value _ ->
          Error (error expression.span "optional forwarding type mismatch"))
  | Sst.Direct_call _
    when Spec_function_sst_private.lambda expression <> None ->
      specification_lambda_eval runtime context expression state
        (Option.get (Spec_function_sst_private.lambda expression))
  | Sst.Direct_call _
    when Spec_function_sst_private.application expression <> None ->
      specification_function_application_eval runtime context expression state
        (Option.get (Spec_function_sst_private.application expression))
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        type_arguments;
        arguments;
        _;
      }
    when Parametric_type.is_spec_function expression.typ ->
      specification_function_reference_eval runtime context expression state
        callee type_arguments arguments
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        type_arguments;
        arguments;
        _;
      } ->
      direct_call_eval runtime context expression state callee type_arguments
        arguments
  | Sst.Symbolic_application application ->
      symbolic_application_eval runtime context expression state application
  | Sst.If (_, _, None)
  | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ | Sst.Let_mutable _
  | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Direct_call _ | Sst.Reveal _
  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _ | Sst.Local_assert _
  | Sst.Proof_region _ | Sst.Old _ ->
      Error (error expression.span "expression escaped logical Spec permit")
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
  | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Let _ | Sst.Sequence _
  | Sst.If (_, _, Some _)
  | Sst.Match _ ->
      Error (error expression.span "logical Spec evaluator dispatch failed")
and direct_call_eval runtime context expression state callee type_arguments
    arguments =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let* authorized =
    authorized_definition runtime callee expression.Sst.typ
    |> Result.map_error (error expression.span)
  in
  match authorized with
      | Some authorized ->
          let definition = authorized.definition in
          let* actuals, state =
            evaluate_list runtime context state
              (List.map
                 (fun argument -> snd (Sst.require_value_argument argument))
                 arguments)
          in
          let parameters =
            let substitute =
              Parametric_type.substitute
                (List.combine definition.type_binders type_arguments)
            in
            List.map Sst.require_value_parameter definition.parameters
            |> List.map (fun (parameter : Sst.value_parameter) ->
                   {
                     parameter with
                     Sst.pattern =
                       Sst.map_pattern_types substitute parameter.pattern;
                   })
          in
          if List.length actuals <> List.length parameters then
            Error (error expression.span "logical Spec argument count mismatch")
          else
            let caller_environment = callbacks.environment state in
            let* environment =
              List.fold_left2
                (fun result parameter actual ->
                  let* environment = result in
                  bind_pattern callbacks.aggregate_type error environment
                    parameter.Sst.pattern actual)
                (Ok []) parameters actuals
            in
            (match definition.body with
            | Sst.Spec_definition body ->
                let nested_context =
                  callbacks.enter_definition context definition
                in
                let substitute =
                  Parametric_type.substitute
                    (List.combine definition.type_binders type_arguments)
                in
                let* value, state =
                  expression_eval
                    { runtime with current_model = authorized.model }
                    nested_context
                    (Sst.map_expression_types substitute body.expression)
                    (callbacks.with_environment state environment)
                in
                Ok
                  ( value,
                    callbacks.with_environment state caller_environment )
            | Sst.Checked_exec _ | Sst.Recursive_spec_definition _
            | Sst.Proof_body _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _
            | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
                assert false)
      | None -> uncovered_direct_call runtime callbacks context expression state callee
and symbolic_application_eval runtime context expression state application =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let* values, state = evaluate_list runtime context state
      (Symbolic_application_private.arguments application) in
  let* arguments =
    List.fold_left
      (fun result value ->
        let* arguments = result in
        let* argument = recursive_argument error expression.Sst.span value in
        Ok (argument :: arguments))
      (Ok []) values
    |> Result.map List.rev
  in
  let* application =
    Symbolic_application_private.replace_arguments arguments application
    |> Result.map_error (error expression.span)
  in
  if Parametric_type.is_spec_function expression.typ then
    Ok
      ( Function_value
          {
            function_term =
              {
                Vir.parametric_sort =
                  Spec_function_logic_private.binder expression.typ;
                parametric_desc =
                  Vir.Parametric_symbolic_application application;
              };
            function_arrow = expression.typ;
            function_closure = Abstract_function;
          },
        state )
  else
    let* application =
      Vir.symbolic_application ~aggregate_type:callbacks.aggregate_type
        application
      |> Result.map_error (error expression.span)
    in
    let value =
      match application with
      | Vir.Integer_application term -> Integer_value term
      | Boolean_application term -> Boolean_value term
      | Aggregate_application term -> Aggregate_value term
      | Parametric_application term -> Parametric_value term
    in
    Ok (value, state)
and specification_lambda_eval runtime _context expression state lambda =
  let callbacks = runtime.callbacks in
  let* value =
    Spec_function_logic_private.lambda_value
      ~environment:(callbacks.environment state) ~error:callbacks.error
      ~span:expression.span lambda
  in
  let function_ =
    match value with Function_value function_ -> function_ | _ -> assert false
  in
  let services : (_, _, _) Spec_function_logic_private.axiom_services =
    {
      evaluate = expression_eval runtime;
      aggregate_type = callbacks.aggregate_type;
      environment = callbacks.environment;
      with_environment = callbacks.with_environment;
      assume = callbacks.assume;
      enter_definition = callbacks.enter_definition;
      bind_pattern =
        bind_pattern callbacks.aggregate_type callbacks.error;
      equality;
      error = callbacks.error;
    }
  in
  let* state =
    Spec_function_logic_private.materialize_lambda_axiom services
      ~context:_context ~span:expression.span function_ state
  in
  Ok (value, state)
and specification_function_reference_eval runtime _context expression state
    callee type_arguments arguments =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let* authorized = authorized_reference runtime expression callee in
      let definition = authorized.definition in
      let source_arguments =
        List.map
          (fun argument -> snd (Sst.require_value_argument argument))
          arguments
      in
      let* values, state =
        evaluate_list runtime _context state source_arguments
      in
      let rec convert converted = function
        | [] -> Ok (List.rev converted)
        | value :: rest ->
            let* argument = recursive_argument error expression.span value in
            convert (argument :: converted) rest
      in
      let* vir_arguments = convert [] values in
      let argument_types =
        List.map (fun (argument : Sst.expression) -> argument.typ)
          source_arguments
      in
      let* function_term =
        Spec_function_logic_private.named ~arrow:expression.typ
          ~function_id:callee ~type_arguments ~arguments:vir_arguments
          ~argument_types ~span:expression.span
        |> Result.map_error (error expression.span)
      in
      let named_arguments =
        List.map2
          (fun argument value ->
            let label, _ = Sst.require_value_argument argument in
            (label, value))
          arguments values
      in
      let function_ =
        {
          function_term;
          function_arrow = expression.typ;
          function_closure =
            Named_function
              {
                definition;
                named_arguments;
                named_argument_types = argument_types;
                named_type_arguments = type_arguments;
              };
        }
      in
      let* state =
        materialize_named_axioms runtime _context expression.span function_
          state
      in
      Ok (Function_value function_, state)
and specification_function_application_eval runtime context expression state
    application =
  let callbacks = runtime.callbacks in
  let error = callbacks.error in
  let* function_value, state =
    expression_eval runtime context application.application_function state
  in
  let* argument, state =
    expression_eval runtime context application.application_argument state
  in
  match function_value with
  | Function_value function_ -> (
      match function_.function_closure with
      | Lambda_function { lambda; lambda_environment } ->
          let caller_environment = callbacks.environment state in
          let environment =
            (lambda.lambda_parameter.id, argument) :: lambda_environment
          in
          let* value, state =
            expression_eval runtime context lambda.lambda_body
              (callbacks.with_environment state environment)
          in
          Ok (value, callbacks.with_environment state caller_environment)
      | Named_function
          {
            definition;
            named_arguments;
            named_argument_types;
            named_type_arguments;
          } ->
          let arguments =
            named_arguments @ [ (application.application_label, argument) ]
          in
          let argument_types =
            named_argument_types @ [ application.application_argument.typ ]
          in
          let parameters =
            let substitute =
              Parametric_type.substitute
                (List.combine definition.type_binders named_type_arguments)
            in
            List.map Sst.require_value_parameter definition.parameters
            |> List.map (fun (parameter : Sst.value_parameter) ->
                   {
                     parameter with
                     Sst.pattern =
                       Sst.map_pattern_types substitute parameter.pattern;
                   })
          in
          if List.length arguments < List.length parameters then
            let* vir_arguments =
              List.fold_left
                (fun result (_, value) ->
                  let* arguments = result in
                  let* argument =
                    recursive_argument error expression.span value
                  in
                  Ok (argument :: arguments))
                (Ok []) arguments
              |> Result.map List.rev
            in
            let* function_term =
              Spec_function_logic_private.named ~arrow:expression.typ
                ~function_id:definition.function_id
                ~type_arguments:named_type_arguments ~arguments:vir_arguments
                ~argument_types ~span:expression.span
              |> Result.map_error (error expression.span)
            in
            Ok
              ( Function_value
                  {
                    function_term;
                    function_arrow = expression.typ;
                    function_closure =
                      Named_function
                        {
                          definition;
                          named_arguments = arguments;
                          named_argument_types = argument_types;
                          named_type_arguments;
                        };
                  },
                state )
          else if List.length arguments = List.length parameters then
            let caller_environment = callbacks.environment state in
            let* environment =
              List.fold_left2
                (fun result parameter (_, actual) ->
                  let* environment = result in
                  bind_pattern callbacks.aggregate_type error environment
                    parameter.Sst.pattern actual)
                (Ok []) parameters arguments
            in
            (match definition.body with
            | Sst.Spec_definition body ->
                let nested_context =
                  callbacks.enter_definition context definition
                in
                let substitute =
                  Parametric_type.substitute
                    (List.combine definition.type_binders named_type_arguments)
                in
                let* value, state =
                  expression_eval runtime nested_context
                    (Sst.map_expression_types substitute body.expression)
                    (callbacks.with_environment state environment)
                in
                Ok
                  ( value,
                    callbacks.with_environment state caller_environment )
            | Sst.Symbolic_declaration declaration ->
                let* vir_arguments =
                  List.fold_left
                    (fun result (_, value) ->
                      let* arguments = result in
                      let* argument =
                        recursive_argument error expression.span value
                      in
                      Ok (argument :: arguments))
                    (Ok []) arguments
                  |> Result.map List.rev
                in
                let* symbolic =
                  Symbolic_application_private.create declaration
                    ~type_arguments:named_type_arguments
                    ~arguments:vir_arguments ~argument_types
                    ~result_type:expression.typ ~span:expression.span
                  |> Result.map_error (error expression.span)
                in
                let* value =
                  value_of_function_application callbacks error expression.span
                    expression.typ symbolic
                in
                Ok
                  ( value,
                    callbacks.with_environment state caller_environment )
            | Sst.Checked_exec _ | Sst.Recursive_spec_definition _
            | Sst.Proof_body _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _
            | Sst.Trusted_external_body _ ->
                assert false)
          else
            Error
              (error expression.span
                 "specification-function application exceeds its arity")
      | Abstract_function ->
          let* argument =
            recursive_argument error expression.span argument
          in
          let* symbolic =
            Spec_function_logic_private.application
              ~arrow:application.application_arrow
              ~function_:function_.function_term ~argument
              ~result_type:application.application_result
              ~span:expression.span
            |> Result.map_error (error expression.span)
          in
          let* value =
            value_of_function_application callbacks error expression.span
              application.application_result symbolic
          in
          Ok (value, state))
  | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
  | Aggregate_value _ | Parametric_value _ ->
      Error
        (error expression.span
           "specification-function application has a non-function head")
and materialize_named_axioms runtime context span function_ state =
  let callbacks = runtime.callbacks in
  let services : (_, _, _) Spec_function_logic_private.axiom_services =
    {
      evaluate = expression_eval runtime;
      aggregate_type = callbacks.aggregate_type;
      environment = callbacks.environment;
      with_environment = callbacks.with_environment;
      assume = callbacks.assume;
      enter_definition = callbacks.enter_definition;
      bind_pattern = bind_pattern callbacks.aggregate_type callbacks.error;
      equality;
      error = callbacks.error;
    }
  in
  Spec_function_logic_private.materialize_named_axioms services ~context ~span
    function_ state
let runtime authorization callbacks = { authorization; callbacks; current_model = None }
let evaluate permit callbacks initial_context root initial_state =
  expression_eval (runtime (Logical_spec_authentication_private.Ordinary permit) callbacks) initial_context root initial_state
let evaluate_invariant_contract permit ~validated ~root_identity callbacks context (root : Sst.expression) state =
  let* () =
    Logical_spec_capability_private.validate_root permit ~validated
      ~root_identity root
    |> Result.map_error (callbacks.error root.span)
  in
  expression_eval
    (runtime
       (Logical_spec_authentication_private.Strict (permit, validated))
       callbacks)
    context root state
