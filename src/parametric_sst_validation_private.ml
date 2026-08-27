type error = {
  function_id : Sst.function_id;
  span : Diagnostic.span;
  message : string;
}

let ( let* ) result continue =
  match result with Ok value -> continue value | Error _ as error -> error

let fail function_id span message = Error { function_id; span; message }

let rec iter_result visit = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = visit value in
      iter_result visit rest

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let validate_binders program (definition : Sst.function_definition) =
  let expected_owner =
    Parametric_type.owner ~index:definition.function_id.function_index
      ~name:definition.function_id.function_name
  in
  let rec loop ordinal = function
    | [] -> Ok ()
    | (binder : Parametric_type.binder) :: rest ->
        if Parametric_type.compare_owner binder.owner expected_owner <> 0 then
          fail definition.function_id definition.span
            "type binder owner differs from its function"
        else if binder.ordinal <> ordinal then
          fail definition.function_id definition.span
            "type binders have forged, duplicate, or reordered ordinals"
        else loop (ordinal + 1) rest
  in
  match loop 0 definition.type_binders with
  | Ok () -> Ok ()
  | Error _ as ordinary_error -> (
      match Sst_callback_private.local_scope_type_binders program definition with
      | Ok (Some binders)
        when
          List.length binders = List.length definition.type_binders
          && List.for_all2
               (fun left right ->
                 Parametric_type.compare_binder left right = 0)
               binders definition.type_binders ->
          Ok ()
      | Ok (Some _) ->
          fail definition.function_id definition.span
            "local callback type binders differ from its lexical owner"
      | Ok None -> ordinary_error
      | Error (span, message) -> fail definition.function_id span message)

let validate_type descriptors function_id span binders typ =
  let rec loop = function
    | Parametric_type.Unit | Bool | Int | Aggregate _ -> Ok ()
    | Tuple components ->
        iter_result (fun (_, component) -> loop component) components
    | Parameter binder ->
        if
          List.exists
            (fun candidate ->
              Parametric_type.compare_binder candidate binder = 0)
            binders
        then Ok ()
        else fail function_id span "type contains an unbound parameter binder"
    | Application (constructor, arguments) ->
        let* () =
          match Parametric_adt.find descriptors constructor with
          | Some descriptor
            when List.length arguments = List.length (Parametric_adt.binders descriptor) ->
              Ok ()
          | Some _ -> fail function_id span "parametric ADT application arity differs from its descriptor"
          | None -> (
              match Parametric_type.validate_application constructor arguments with
              | Ok () -> Ok ()
              | Error message -> fail function_id span message)
        in
        iter_result loop arguments
  in
  loop typ

let validate_pattern descriptors function_id binders (pattern : Sst.pattern) =
  let rec loop (pattern : Sst.pattern) =
    let* () = validate_type descriptors function_id pattern.span binders pattern.typ in
    match pattern.pattern_desc with
    | Sst.Wildcard | Int_pattern _ | Bool_pattern _ | Unit_pattern -> Ok ()
    | Bind binding -> validate_type descriptors function_id binding.span binders binding.typ
    | Owned_tree_cursor_pattern cursor ->
        validate_type descriptors function_id cursor.cursor_binding.span binders
          cursor.cursor_binding.typ
    | Tuple_pattern components ->
        iter_result (fun (_, nested) -> loop nested) components
    | Record_pattern components ->
        iter_result (fun (_, nested) -> loop nested) components
    | Constructor_pattern (_, arguments) -> iter_result loop arguments
    | Or_pattern (left, right) ->
        let* () = loop left in
        loop right
  in
  loop pattern

let validate_call descriptors functions (definition : Sst.function_definition)
    (expression : Sst.expression) callee type_arguments arguments =
  let function_id = definition.Sst.function_id in
  let binders = definition.type_binders in
  let* () =
    iter_result
      (validate_type descriptors function_id expression.Sst.span binders)
      type_arguments
  in
  match
    List.find_opt
      (fun candidate -> same_function_id candidate.Sst.function_id callee)
      functions
  with
  | None ->
      (* Preserve the established unknown-call diagnostic in the ordinary
         identity validator. No parametric vector can be authenticated without
         a target signature. *)
      Ok ()
  | Some callee_definition
    when callee_definition.type_binders = [] && type_arguments = [] ->
      Ok ()
  | Some callee_definition -> (
      let rec value_formals = function
        | [] -> Ok []
        | Sst.Value_parameter parameter :: rest ->
            let* rest = value_formals rest in
            Ok (parameter :: rest)
        | Sst.Callback_parameter _ :: rest -> value_formals rest
      in
      let rec value_actuals = function
        | [] -> Ok []
        | Sst.Value_argument { label; value } :: rest ->
            let* rest = value_actuals rest in
            Ok ((label, value) :: rest)
        | Sst.Callback_argument _ :: rest -> value_actuals rest
      in
      let* formals = value_formals callee_definition.parameters in
      let* actuals = value_actuals arguments in
      let formal_types = List.map (fun parameter -> parameter.Sst.pattern.typ) formals in
      let formal_labels =
        List.map (fun (parameter : Sst.value_parameter) -> parameter.Sst.label) formals
      in
      let actual_types = List.map (fun (_, (actual : Sst.expression)) -> actual.typ) actuals in
      let actual_labels = List.map fst actuals in
      let* inferred =
        match
          Parametric_lowering_private.infer_labeled_type_arguments
            ~binders:callee_definition.type_binders ~formal_types ~formal_labels
            ~actual_types ~actual_labels
            ~formal_result:callee_definition.result_type
            ~actual_result:expression.typ
        with
        | Ok inferred -> Ok inferred
        | Error message -> fail function_id expression.span message
      in
      let* () =
        if
          List.length inferred = List.length type_arguments
          && List.for_all2 Parametric_type.equal inferred type_arguments
        then Ok ()
        else
          fail function_id expression.span
            "direct-call type vector differs from canonical inference"
      in
      match
        Parametric_lowering_private.validate_sst_direct_call
          ~definition:callee_definition ~type_arguments
          ~actual_result:expression.typ ~call_span:expression.span ~arguments:actuals
      with
      | Ok () -> Ok ()
      | Error (span, message) -> fail function_id span message)

let validate_expression descriptors functions definition expression =
  let function_id = definition.Sst.function_id in
  let binders = definition.type_binders in
  let rec loop (expression : Sst.expression) =
    let* () =
      validate_type descriptors function_id expression.span binders expression.typ
    in
    let visit = iter_result loop in
    match expression.expression_desc with
    | Sst.Int_constant _ | Bool_constant _ | Unit_constant | Variable _
    | Mutable_read _ | Owned_tree_rebase _ | Optional_absent | Reveal _
    | Reveal_with_fuel _ ->
        Ok ()
    | Tuple_value values -> visit (List.map snd values)
    | Record_value { fields; _ } -> visit (List.map snd fields)
    | Constructor_value { arguments; _ } | Checked_arithmetic (_, arguments) ->
        visit arguments
    | Field_read { record; _ } -> loop record
    | Field_write { value; _ }
    | Shared_scalar_field_write { value; _ }
    | Owned_tree_nested_write { value; _ }
    | Mutable_write { value; _ }
    | Optional_present value
    | Optional_forward value
    | Use_type_invariant { value; _ }
    | Old value ->
        loop value
    | Let_mutable (binding, initial, body) ->
        let* () = validate_type descriptors function_id binding.span binders binding.typ in
        let* () = loop initial in
        loop body
    | Let (bindings, body) ->
        let* () =
          iter_result
            (fun (pattern, value) ->
              let* () = validate_pattern descriptors function_id binders pattern in
              loop value)
            bindings
        in
        loop body
    | Sequence (left, right)
    | Compare (_, left, right)
    | Boolean_binary (_, left, right) ->
        let* () = loop left in
        loop right
    | If (condition, consequent, alternative) ->
        let* () = loop condition in
        let* () = loop consequent in
        iter_result loop (Option.to_list alternative)
    | Match (scrutinee, cases) ->
        let* () = loop scrutinee in
        iter_result
          (fun (case : Sst.case) ->
            let* () = validate_pattern descriptors function_id binders case.case_pattern in
            let* () = iter_result loop (Option.to_list case.case_guard) in
            loop case.case_body)
          cases
    | Boolean_not operand
    | Proof_region operand
    | Local_assert { predicate = operand; _ } ->
        loop operand
    | Forall quantifier | Exists quantifier ->
        let* () =
          validate_type descriptors function_id quantifier.quantifier_binder.span
            binders quantifier.quantifier_binder.typ
        in
        let* () = loop quantifier.quantifier_body in
        visit (Option.to_list quantifier.quantifier_trigger)
    | Direct_call
        { callee; type_arguments; arguments; call_form = _; recursive = _ } ->
        let values =
          List.filter_map
            (function
              | Sst.Value_argument { value; _ } -> Some value
              | Sst.Callback_argument _ -> None)
            arguments
        in
        if Spec_function_sst_private.application expression <> None then
          visit values
        else if Spec_function_sst_private.is_reference expression then
          let* () =
            iter_result
              (validate_type descriptors function_id expression.span binders)
              type_arguments
          in
          visit values
        else
          let* () =
            validate_call descriptors functions definition expression callee
              type_arguments arguments
          in
          visit values
    | Callback_call application | Callback_requires application ->
        visit (List.map snd application.arguments)
    | Callback_ensures { application; result } ->
        visit (List.map snd application.arguments @ [ result ])
    | Symbolic_application application ->
        visit (Symbolic_application_private.arguments application)
  in
  loop expression

let validate_staged descriptors functions definition staged =
  validate_expression descriptors functions definition staged.Sst.expression

let validate_contracts descriptors functions definition =
  let contracts = definition.Sst.contracts in
  let predicate (clause : Sst.predicate_clause) =
    validate_staged descriptors functions definition clause.Sst.predicate
  in
  let ensures (clause : Sst.ensures_clause) =
    let* () =
      iter_result
        (validate_pattern descriptors definition.function_id definition.type_binders)
        (Option.to_list clause.Sst.binder)
    in
    validate_staged descriptors functions definition clause.predicate
  in
  let* () = iter_result predicate contracts.requires in
  let* () = iter_result ensures contracts.ensures in
  let* () = iter_result predicate contracts.decreases in
  iter_result predicate contracts.assertions

let validate_definition descriptors functions definition =
  let function_id = definition.Sst.function_id in
  let binders = definition.type_binders in
  let* () =
    iter_result
      (function
        | Sst.Callback_parameter _ -> Ok ()
        | Sst.Value_parameter parameter ->
            let* () = validate_pattern descriptors function_id binders parameter.pattern in
            match parameter.optional_default with
            | None -> Ok ()
            | Some default ->
                let* () =
                  validate_pattern descriptors function_id binders default.optional_pattern
                in
                validate_expression descriptors functions definition default.optional_expression)
      definition.parameters
  in
  let* () =
    validate_type descriptors function_id definition.span binders definition.result_type
  in
  let* () = validate_contracts descriptors functions definition in
  match definition.body with
  | Sst.Checked_exec { body; _ } | Proof_body { body; _ } ->
      validate_staged descriptors functions definition body
  | Spec_definition body | Recursive_spec_definition { body; _ } ->
      validate_staged descriptors functions definition body
  | External_specification _ | Trusted_external_spec_target _
  | Trusted_external_body _ | Symbolic_declaration _ ->
      Ok ()

let validate_program (program : Sst.program) =
  let* () =
    match Parametric_adt.validate_registry program.parametric_adts with
    | Ok () -> Ok ()
    | Error error ->
        let function_id =
          match program.functions with
          | definition :: _ -> definition.Sst.function_id
          | [] -> { Sst.function_index = -1; function_name = "program" }
        in
        let span =
          match program.functions with
          | definition :: _ -> definition.Sst.span
          | [] -> Diagnostic.span_of_location ~fallback_file:"<program>" Location.none
        in
        fail function_id span (error.descriptor ^ ": " ^ error.message)
  in
  let* () = iter_result (validate_binders program) program.functions in
  iter_result
    (validate_definition program.parametric_adts program.functions)
    program.functions
