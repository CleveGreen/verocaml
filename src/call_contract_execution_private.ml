type value = Logical_spec_evaluation_private.value
type environment = (int * value) list

type clause = Callback_contract_private.execution_clause = {
  ordinal : int;
  span : Diagnostic.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type call_summary = {
  definition : Sst.function_definition;
  requires : clause list;
  ensures : clause list;
  assertions : clause list;
  executable_body : Sst.expression;
}

type ('obligation, 'path) evaluation = {
  obligations : 'obligation list;
  paths : 'path list;
}

type
  ( 'context,
    'state,
    'summary,
    'evaluated,
    'obligation,
    'error )
  runtime = {
  evaluate :
    'context ->
    Sst.expression ->
    'state ->
    (('obligation, 'evaluated) evaluation, 'error) result;
  value : 'evaluated -> value;
  state : 'evaluated -> 'state;
  evaluated : value -> 'state -> 'evaluated;
  function_ref : 'context -> Vir.function_ref;
  logical : 'context -> bool;
  callback_environment :
    'context -> (Sst.callback_binding * Sst.callback_binding) list;
  find_summary : 'context -> Sst.function_id -> 'summary option;
  definition : 'summary -> Sst.function_definition;
  requires_clauses : 'summary -> clause list;
  ensures_clauses : 'summary -> clause list;
  contract_context :
    'context ->
    Sst.function_definition ->
    environment ->
    environment option ->
    'context;
  environment : 'state -> environment;
  with_environment : 'state -> environment -> 'state;
  with_assumptions : 'state -> Vir.boolean_term list -> 'state;
  bind_pattern :
    string ->
    environment ->
    Sst.pattern ->
    value ->
    ((environment * Vir.boolean_term list), 'error) result;
  fresh_value :
    'state ->
    source_name:string ->
    role:Vir.symbol_role ->
    span:Sst.span ->
    project:bool ->
    Sst.typ ->
    ((value * 'state), 'error) result;
  expect_boolean :
    string -> Sst.span -> value -> (Vir.boolean_term, 'error) result;
  emit_goal :
    Vir.function_ref ->
    Vir.vc_kind ->
    Sst.span ->
    Vir.boolean_term ->
    'state ->
    'obligation * 'state;
  malformed : string -> Sst.span -> string -> 'error;
  record : Vir.reached_callback_call -> unit;
}

let application callback arguments call_span =
  { Vir.callback; arguments; call_span }

let requires callback arguments call_span =
  Vir.Callback_requires (application callback arguments call_span)

let ensures callback arguments result call_span =
  Vir.Callback_ensures
    { application = application callback arguments call_span; result }

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let append left right = left @ right

let evaluate_contexts evaluate contexts =
  let rec loop obligations paths = function
    | [] -> Ok { obligations; paths = List.rev paths }
    | context :: rest ->
        let* evaluated = evaluate context in
        loop
          (append obligations evaluated.obligations)
          (List.rev_append evaluated.paths paths)
          rest
  in
  loop [] [] contexts

let relation_argument = function
  | Logical_spec_evaluation_private.Integer_value term ->
      Ok (Vir.Recursive_integer_argument term)
  | Boolean_value term -> Ok (Vir.Recursive_boolean_argument term)
  | Aggregate_value term -> Ok (Vir.Recursive_aggregate_argument term)
  | Parametric_value term -> Ok (Vir.Recursive_parametric_argument term)
  | Function_value _ ->
      Error "specification-function values cannot cross the callback ABI"
  | Unit_value | Tuple_value _ ->
      Error "unit and tuple callback relation arguments are unsupported"

let rec callback_result = function
  | Logical_spec_evaluation_private.Unit_value -> Ok Vir.Unit_result
  | Integer_value (Vir.Integer_symbol symbol) ->
      Ok (Vir.Integer_result symbol)
  | Boolean_value (Vir.Boolean_symbol symbol) ->
      Ok (Vir.Boolean_result symbol)
  | Tuple_value values ->
      let rec collect results = function
        | [] -> Ok (Vir.Tuple_result (List.rev results))
        | value :: rest ->
            let* result = callback_result value in
            collect (result :: results) rest
      in
      collect [] values
  | Aggregate_value
      { Vir.aggregate_desc = Vir.Aggregate_symbol symbol; _ } ->
      Ok (Vir.Aggregate_result symbol)
  | Parametric_value
      { Vir.parametric_desc = Vir.Parametric_symbol symbol; _ } ->
      Ok (Vir.Parametric_result symbol)
  | Function_value _ ->
      Error "specification-function values cannot cross the callback ABI"
  | Integer_value _ | Boolean_value _ | Aggregate_value _
  | Parametric_value _ ->
      Error "callback result is not one fresh first-order value"

let call_precondition definition clause call_span =
  Vir.Call_precondition
    {
      callee =
        {
          Vir.function_index = definition.Sst.function_id.function_index;
          function_name = definition.function_id.function_name;
        };
      precondition_ordinal = clause.ordinal;
      declaration_span = clause.span;
      call_span;
    }

let instantiate_summary (summary : call_summary) type_arguments =
  let binders = summary.definition.Sst.type_binders in
  match
    Parametric_lowering_private.instantiate ~binders ~arguments:type_arguments
      summary.definition.result_type
  with
  | Error _ as error -> error
  | Ok result_type ->
      let substitute =
        Parametric_type.substitute (List.combine binders type_arguments)
      in
      let map_parameter = function
        | Sst.Value_parameter parameter ->
            Sst.Value_parameter
              {
                parameter with
                pattern =
                  Sst.map_pattern_types substitute parameter.pattern;
                optional_default =
                  Option.map
                    (fun default ->
                      {
                        Sst.optional_pattern =
                          Sst.map_pattern_types substitute
                            default.Sst.optional_pattern;
                        optional_expression =
                          Sst.map_expression_types substitute
                            default.optional_expression;
                      })
                    parameter.optional_default;
              }
        | Sst.Callback_parameter _ as parameter -> parameter
      in
      let map_clause clause =
        {
          clause with
          binder = Option.map (Sst.map_pattern_types substitute) clause.binder;
          payload = Sst.map_expression_types substitute clause.payload;
        }
      in
      Ok
        {
          definition =
            {
              summary.definition with
              parameters = List.map map_parameter summary.definition.parameters;
              result_type;
            };
          requires = List.map map_clause summary.requires;
          ensures = List.map map_clause summary.ensures;
          assertions = List.map map_clause summary.assertions;
          executable_body =
            Sst.map_expression_types substitute summary.executable_body;
        }

let malformed runtime context span message =
  Error
    (runtime.malformed
       (runtime.function_ref context).Vir.function_name span message)

let relation_argument_for runtime context span value =
  relation_argument value
  |> Result.map_error (fun message ->
         runtime.malformed
           (runtime.function_ref context).Vir.function_name span message)

let callback_result_for runtime context span value =
  callback_result value
  |> Result.map_error (fun message ->
         runtime.malformed
           (runtime.function_ref context).Vir.function_name span message)

let summary runtime context (expression : Sst.expression) callback =
  let id =
    {
      Sst.function_index = callback.Sst.callback_id;
      function_name = callback.callback_name;
    }
  in
  match runtime.find_summary context id with
  | Some summary -> Ok summary
  | None ->
      malformed runtime context expression.Sst.span
        "authenticated callback definition is absent"

let evaluate_arguments runtime context arguments state =
  let rec loop obligations paths = function
    | [] -> Ok { obligations; paths }
    | (_, argument) :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun (state, values) ->
              let* argument = runtime.evaluate context argument state in
              Ok
                {
                  obligations = argument.obligations;
                  paths =
                    List.map
                      (fun evaluated ->
                        (runtime.state evaluated, runtime.value evaluated :: values))
                      argument.paths;
                })
            paths
        in
        loop
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  loop [] [ (state, []) ] arguments

let bind_contract_environment runtime context (expression : Sst.expression) callback summary values
    state =
  let function_name = (runtime.function_ref context).Vir.function_name in
  let parameters =
    List.filter_map
      (function
        | Sst.Value_parameter parameter -> Some parameter
        | Sst.Callback_parameter _ -> None)
      (runtime.definition summary).Sst.parameters
  in
  if List.length parameters <> List.length values then
    malformed runtime context expression.Sst.span
      "callback contract argument count differs from its shape"
  else
    let* environment, ranges =
      List.fold_left2
        (fun environment parameter actual ->
          let* environment, ranges = environment in
          let* environment, nested =
            runtime.bind_pattern function_name environment
              parameter.Sst.pattern actual
          in
          Ok (environment, ranges @ nested))
        (Ok ([], [])) parameters values
    in
    let* environment =
      List.fold_left
        (fun environment
             (capture : Callback_certificate_private.capture) ->
          let* environment = environment in
          match List.assoc_opt capture.binding_id (runtime.environment state) with
          | Some value -> Ok ((capture.binding_id, value) :: environment)
          | None ->
              malformed runtime context expression.span
                "authenticated callback capture is absent from the current call edge")
        (Ok environment)
        (Callback_certificate_private.captures
           callback.Sst.callback_certificate)
    in
    Ok (environment, ranges)

let evaluate_contract runtime context (expression : Sst.expression) callback summary values result
    clauses state =
  let function_name = (runtime.function_ref context).Vir.function_name in
  let* environment, ranges =
    bind_contract_environment runtime context expression callback summary values
      state
  in
  let caller_environment = runtime.environment state in
  let contract_context =
    runtime.contract_context context (runtime.definition summary) environment
      (Option.map (fun _ -> environment) result)
  in
  let rec clauses_loop obligations paths = function
    | [] ->
        Ok
          {
            obligations;
            paths =
              List.map
                (fun (state, predicate) ->
                  runtime.evaluated
                    (Logical_spec_evaluation_private.Boolean_value predicate)
                    (runtime.with_environment state caller_environment))
                paths;
          }
    | clause :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun (state, combined) ->
              let* clause_environment, binder_ranges =
                match clause.binder, result with
                | None, (None | Some _) -> Ok (environment, [])
                | Some binder, Some result ->
                    runtime.bind_pattern function_name environment binder result
                | Some _, None ->
                    malformed runtime context clause.span
                      "callback requires clause has a result binder"
              in
              let state =
                runtime.with_assumptions
                  (runtime.with_environment state clause_environment)
                  binder_ranges
              in
              let* predicate =
                runtime.evaluate contract_context clause.payload state
              in
              let* paths =
                evaluate_contexts
                  (fun evaluated ->
                    let* predicate =
                      runtime.expect_boolean function_name clause.span
                        (runtime.value evaluated)
                    in
                    Ok
                      {
                        obligations = [];
                        paths =
                          [
                            ( runtime.state evaluated,
                              Vir.Boolean_and (combined, predicate) );
                          ];
                      })
                  predicate.paths
              in
              Ok { obligations = predicate.obligations; paths = paths.paths })
            paths
        in
        clauses_loop
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  clauses_loop []
    [
      ( runtime.with_assumptions state ranges,
        Vir.Boolean_constant true );
    ]
    clauses

let relation_arguments runtime context (expression : Sst.expression) values =
  let rec convert converted = function
    | [] -> Ok (List.rev converted)
    | value :: rest ->
        let* argument =
          relation_argument_for runtime context expression.Sst.span value
        in
        convert (argument :: converted) rest
  in
  convert [] values

let opaque_projection runtime context (expression : Sst.expression) callback relation_arguments
    result =
  match expression.Sst.expression_desc, result with
  | Sst.Callback_requires _, None ->
      Ok
        (Logical_spec_evaluation_private.Boolean_value
           (requires callback relation_arguments expression.span))
  | Sst.Callback_ensures _, Some result ->
      let* result =
        relation_argument_for runtime context expression.span result
      in
      Ok
        (Logical_spec_evaluation_private.Boolean_value
           (ensures callback relation_arguments result expression.span))
  | (Sst.Callback_call _ | Sst.Callback_requires _), Some _
  | Sst.Callback_ensures _, None ->
      malformed runtime context expression.span
        "callback projection result arity mismatch"
  | _ -> assert false

let record_call runtime context (expression : Sst.expression) callback relation_arguments result
    ensures state obligation =
  let* recorded_result =
    callback_result_for runtime context expression.Sst.span result
  in
  runtime.record
    {
      Vir.application =
        { callback; arguments = relation_arguments; call_span = expression.span };
      result = recorded_result;
      ensures;
    };
  Ok
    {
      obligations = [ obligation ];
      paths =
        [
          runtime.evaluated result
            (runtime.with_assumptions state [ ensures ]);
        ];
    }

let execute_opaque runtime context (expression : Sst.expression) callback relation_arguments state =
  let function_ref = runtime.function_ref context in
  let requires = requires callback relation_arguments expression.Sst.span in
  let obligation, state =
    runtime.emit_goal function_ref
      (Vir.Callback_precondition
         { callback; call_span = expression.span })
      expression.span requires state
  in
  let* result, state =
    runtime.fresh_value state
      ~source_name:(callback.callback_name ^ ".callback-result")
      ~role:Vir.Result ~span:expression.span ~project:true expression.typ
  in
  let* relation_result =
    relation_argument_for runtime context expression.span result
  in
  record_call runtime context expression callback relation_arguments result
    (ensures callback relation_arguments relation_result expression.span)
    state obligation

let execute_concrete runtime context (expression : Sst.expression) callback summary values
    relation_arguments state =
  let function_ref = runtime.function_ref context in
  let function_name = function_ref.Vir.function_name in
  let* required =
    evaluate_contract runtime context expression callback summary values None
      (runtime.requires_clauses summary) state
  in
  let execute required =
    let* goal =
      runtime.expect_boolean function_name expression.span
        (runtime.value required)
    in
    let obligation, state =
      runtime.emit_goal function_ref
        (Vir.Callback_precondition
           { callback; call_span = expression.span })
        expression.span goal (runtime.state required)
    in
    let* result, state =
      runtime.fresh_value state
        ~source_name:(callback.callback_name ^ ".callback-result")
        ~role:Vir.Result ~span:expression.span ~project:true expression.typ
    in
    let* ensured =
      evaluate_contract runtime context expression callback summary values
        (Some result) (runtime.ensures_clauses summary) state
    in
    let* materialized =
      evaluate_contexts
        (fun ensured ->
          let* ensures =
            runtime.expect_boolean function_name expression.span
              (runtime.value ensured)
          in
          record_call runtime context expression callback relation_arguments
            result ensures (runtime.state ensured) obligation)
        ensured.paths
    in
    Ok
      {
        obligations = append ensured.obligations materialized.obligations;
        paths = materialized.paths;
      }
  in
  let* executed = evaluate_contexts execute required.paths in
  Ok
    {
      obligations = append required.obligations executed.obligations;
      paths = executed.paths;
    }

let instantiate runtime context (expression : Sst.expression) callback source_result
    (state, reversed_values) =
  let values = List.rev reversed_values in
  let arguments, result =
    match source_result, List.rev values with
    | Some _, result :: arguments -> (List.rev arguments, Some result)
    | None, _ -> (values, None)
    | Some _, [] -> ([], None)
  in
  let* relation_arguments =
    relation_arguments runtime context expression arguments
  in
  let concrete =
    match
      Callback_certificate_private.kind callback.Sst.callback_certificate
    with
    | Callback_certificate_private.Formal -> false
    | Top_level | Local -> true
  in
  match expression.expression_desc with
  | Sst.Callback_call _ ->
      if runtime.logical context then
        malformed runtime context expression.span
          "callback call reached logical evaluation"
      else if concrete then
        let* summary = summary runtime context expression callback in
        execute_concrete runtime context expression callback summary arguments
          relation_arguments state
      else execute_opaque runtime context expression callback relation_arguments state
  | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
      if not (runtime.logical context) then
        malformed runtime context expression.span
          "callback projection reached runtime evaluation"
      else if concrete then
        let* summary = summary runtime context expression callback in
        let clauses =
          match expression.expression_desc with
          | Sst.Callback_requires _ -> runtime.requires_clauses summary
          | Sst.Callback_ensures _ -> runtime.ensures_clauses summary
          | _ -> assert false
        in
        evaluate_contract runtime context expression callback summary arguments
          result clauses state
      else
        let* value =
          opaque_projection runtime context expression callback
            relation_arguments result
        in
        Ok
          {
            obligations = [];
            paths = [ runtime.evaluated value state ];
          }
  | _ -> assert false

let evaluate_callback runtime context (expression : Sst.expression) state =
  let application, source_result =
    match expression.Sst.expression_desc with
    | Sst.Callback_call application | Sst.Callback_requires application ->
        (application, None)
    | Sst.Callback_ensures { application; result } ->
        (application, Some result)
    | _ -> assert false
  in
  let callback =
    match
      List.find_opt
        (fun (formal, _) ->
          Sst_callback_private.same_binding formal application.callback)
        (runtime.callback_environment context)
    with
    | Some (_, actual) -> actual
    | None -> application.callback
  in
  let application = { application with Sst.callback } in
  let source_arguments =
    application.arguments
    @ Option.fold ~none:[] ~some:(fun result -> [ (None, result) ])
        source_result
  in
  let* evaluated = evaluate_arguments runtime context source_arguments state in
  let* instantiated =
    evaluate_contexts
      (instantiate runtime context expression application.callback source_result)
      evaluated.paths
  in
  Ok
    {
      obligations = append evaluated.obligations instantiated.obligations;
      paths = instantiated.paths;
    }
