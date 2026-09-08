type t = {
  original_full_key : string;
  profile_full_key : string;
  target_full_keys : string list;
  trusted_dependencies : string list;
  full_key : string;
}

let ( let* ) = Result.bind
let mathematical = function Sst.Mathematical_int | Bool | Unit -> true | _ -> false

let mathematical_definition (definition : Sst.function_definition) =
  let rec expressions = function
    | [] -> true
    | expression :: rest ->
        mathematical expression.Sst.typ
        && (match expression.expression_desc with
          | Sst.Forall q | Exists q -> mathematical q.quantifier_binder.typ
          | Lift_runtime_int _ -> false
          | _ -> true)
        && expressions (Sst_callback_private.expression_children expression @ rest) in
  mathematical definition.result_type
  && List.for_all (function Sst.Value_parameter p -> mathematical p.pattern.typ | _ -> false) definition.parameters
  && expressions (Numeric_proof_dependencies_private.definition_expressions definition)

let complete ~capability:(capability [@delator.skip]) ~consumer:(consumer [@delator.skip])
    ~report:(report [@delator.skip]) (original [@delator.skip]) =
  let result =
    let* () = if Numeric_original_obligation_private.matches_consumer original consumer
      && List.exists ((==) original) (Verification_driver_private.original_obligations report)
      then Ok () else Error "Abstract coverage requires the exact original obligation captured by this consumer report." in
    let* completion = match Verification_driver_private.verified_completion report with
      | Some completion -> Ok completion
      | None -> Error "Abstract coverage requires a successfully verified source proof." in
    let validated = Verification_driver_private.validated report in
    let* () = if Verification_driver_private.completion_matches completion ~implementation:consumer ~validated
      then Ok () else Error "Abstract coverage belongs to a different consumer compilation." in
    let* descriptor = match Sst_validation.find_callable validated
        {Sst.function_index = original.obligation.function_ref.function_index;
         function_name = original.obligation.function_ref.function_name} with
      | Some descriptor -> Ok descriptor
      | None -> Error "The original proof function is not available in this verified program." in
    let* evidence = Numeric_proof_dependencies_private.complete_definition
      ~completion ~implementation:consumer ~validated ~context:Ghost_context
      (Sst_validation.callable_definition descriptor)
      |> Result.map_error (fun _ -> "This proof context does not support target-independent mathematical coverage.") in
    let* () = if List.for_all (fun dependency -> mathematical_definition dependency.Numeric_proof_dependencies_private.definition)
      evidence.dependencies then Ok ()
      else Error "Abstract identity coverage cannot use runtime integers or target-dependent operations. Select concrete coverage." in
    let* profile = Build_target_profile_private.authenticate_profile capability in
    let* targets = Build_target_profile_private.authenticate_instances capability in
    let targets = Numeric_receipt_private.canonical_members
      ~full_key:(fun (target : Build_target_profile_private.instance) -> target.full_key) targets in
    let target_full_keys = List.map (fun target -> target.Build_target_profile_private.full_key) targets in
    let dependency_key dependency = Numeric_receipt_private.encode ~schema:"mathematical-proof-dependency"
      [dependency.Numeric_proof_dependencies_private.compiler_uid;
       string_of_bool dependency.trusted;
       Sst.to_string {(Sst_validation.program validated) with functions = [dependency.definition]}] in
    let dependency_keys = List.map dependency_key evidence.dependencies |> List.sort String.compare in
    let trusted_dependencies = List.map dependency_key evidence.trusted_dependencies |> List.sort String.compare in
    let original_full_key = original.full_key and profile_full_key = profile.full_key in
    let full_key = Numeric_receipt_private.encode ~schema:"verocaml.mathematical-identity-coverage.v1"
      [original_full_key; profile_full_key; Numeric_receipt_private.list target_full_keys;
       Numeric_receipt_private.list dependency_keys; Numeric_receipt_private.list trusted_dependencies] in
    Ok {original_full_key; profile_full_key; target_full_keys; trusted_dependencies; full_key} in
  [%log.debug "checked mathematical all-target identity coverage"
    ~function_name:(Delator.Field.string original.obligation.function_ref.function_name)
    ~decision:(Delator.Field.string (if Result.is_ok result then "covered" else "unavailable"))
    ~reason:(Delator.Field.string (match result with Ok _ -> "identity-specialization" | Error reason -> reason))
    ~trusted_dependencies:(Delator.Field.int (match result with Ok coverage -> List.length coverage.trusted_dependencies | Error _ -> 0))
    ~new_solver_query:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]
