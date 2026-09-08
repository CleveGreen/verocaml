type recursive_verification = {
  authority : Recursive_spec_encoding.verified option;
  termination_obligations : int;
  terminal_result : Solver_backend.obligation_result option;
}

type preflight = recursive_verification

type threaded = {
  threaded_policy : Solver_policy_private.t;
  threaded_recursive : recursive_verification;
}

type prepared_vc = {
  prepared_obligation : Vir.obligation;
  projection_manifest : Z3_bridge.vir_projection_metadata list;
  deliver_projection_model : bool;
  worker_vc : Function_vc_worker_private.vc_request;
  retry_query : Recursive_spec_encoding.prepared_query option;
}

type prepared_function = {
  prepared_source_ordinal : int;
  prepared_vcs : prepared_vc list;
  preparation_error : string option;
  request : Function_vc_worker_private.request;
  policy : Solver_policy_private.t;
}

let force_recursive_local_inconclusive_for_testing = ref false
let recursive_retry_rlimit_for_testing = ref None
let local_counter_commit_observer_for_testing = ref None
let preparation_error_source_ordinal_for_testing = ref None

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let preflight_with ~solver_policy ~program prepare =
  Recursive_spec_preservation.clear_pending program;
  let has_recursive_specification =
    List.exists
      (fun (definition : Sst.function_definition) ->
        match definition.body with
        | Sst.Recursive_spec_definition _ -> true
        | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
        | Sst.External_specification _
        | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
            false)
      program.Sst.functions
  in
  if not has_recursive_specification then
    Ok { authority = None; termination_obligations = 0; terminal_result = None }
  else
  match prepare () with
  | Error error ->
      let detail = Recursive_spec_encoding.error_to_string error in
      [%log.debug "recursive specification preflight preparation failed"
        ~stage:(Delator.Field.string "recursive-specification-preflight")
        ~operation:(Delator.Field.string "prepare")
        ~detail:(Delator.Field.string detail)
        ~decision:(Delator.Field.string "rejected")];
      Error
        (Verification_pipeline.Internal_setup_error detail)
  | Ok prepared when not (Recursive_spec_encoding.has_definitions prepared) ->
      Ok { authority = None; termination_obligations = 0; terminal_result = None }
  | Ok prepared -> (
      match
        Recursive_spec_encoding.verify_for_preflight
          ~rlimit:(Solver_policy_private.rlimit solver_policy)
          ~timeout_ms:(Solver_policy_private.timeout_ms solver_policy)
          prepared
      with
      | Error error ->
          let detail = Recursive_spec_encoding.error_to_string error in
          [%log.debug "recursive specification preflight verification failed"
            ~stage:(Delator.Field.string "recursive-specification-preflight")
            ~operation:(Delator.Field.string "verify")
            ~detail:(Delator.Field.string detail)
            ~decision:(Delator.Field.string "rejected")];
          Error
            (Verification_pipeline.Internal_setup_error detail)
      | Ok (Recursive_spec_encoding.Verification_inconclusive result) ->
          Ok
            {
              authority = None;
              termination_obligations = 1;
              terminal_result = Some result;
            }
      | Ok (Recursive_spec_encoding.Verification_verified verified) ->
          let rec check = function
            | [] ->
                Recursive_spec_preservation.retain_pending program
                  (Recursive_spec_encoding.preservation_capabilities verified);
                Ok
                  {
                    authority = Some verified;
                    termination_obligations =
                      Recursive_spec_encoding.termination_obligation_count
                        prepared;
                    terminal_result = None;
                  }
            | function_id :: rest -> (
                match Recursive_spec_encoding.base_query verified function_id with
                | Ok _ -> check rest
                | Error error ->
                    let detail = Recursive_spec_encoding.error_to_string error in
                    [%log.debug
                      "recursive specification preflight base query failed"
                      ~stage:
                        (Delator.Field.string
                           "recursive-specification-preflight")
                      ~operation:(Delator.Field.string "base-query")
                      ~function_name:
                        (Delator.Field.string function_id.function_name)
                      ~detail:(Delator.Field.string detail)
                      ~decision:(Delator.Field.string "rejected")];
                    Error
                      (Verification_pipeline.Internal_setup_error detail))
          in
          check (Recursive_spec_encoding.definition_ids verified))
[@@delator.instrument] [@@delator.level debug]

let preflight ~solver_policy program =
  [%log.debug "starting recursive preflight from an unvalidated SST program"
    ~stage:(Delator.Field.string "recursive-specification-preflight")
    ~validation_authority:(Delator.Field.string "local-validation")
    ~decision:(Delator.Field.string "started")];
  preflight_with ~solver_policy ~program (fun () ->
      Recursive_spec_encoding.prepare program)
[@@delator.instrument] [@@delator.level debug]

let preflight_validated ~solver_policy validated =
  let program = Sst_validation.program validated in
  [%log.debug "starting recursive preflight from retained validation authority"
    ~stage:(Delator.Field.string "recursive-specification-preflight")
    ~validation_authority:(Delator.Field.string "retained")
    ~decision:(Delator.Field.string "started")];
  preflight_with ~solver_policy ~program (fun () ->
      Recursive_spec_encoding.prepare_validated validated)
[@@delator.instrument] [@@delator.level debug]

let termination_obligations preflight = preflight.termination_obligations
let terminal_result preflight = preflight.terminal_result

let preservation_capabilities preflight =
  match preflight.authority with
  | None -> []
  | Some verified ->
      Recursive_spec_encoding.preservation_capabilities verified

let proof_entry_activations preflight function_id =
  match preflight.authority with
  | None -> []
  | Some verified ->
      Recursive_spec_encoding.proof_entry_activations verified function_id

let function_id_of_execution (execution : Vir.function_execution) =
  {
    Sst.function_index = execution.Vir.function_ref.function_index;
    function_name = execution.function_ref.function_name;
  }

let nonproof_activations recursive_verification execution =
  match recursive_verification.authority with
  | None -> []
  | Some verified ->
      Recursive_spec_encoding.proof_entry_activations verified
        (function_id_of_execution execution)

let route_obligation mode routes obligation =
  match mode with
  | Sst.Proof -> (
      match routes with
      | [] -> Error "proof activation route is missing"
      | route :: routes ->
          let* routed =
            Verification_session.proof_activation_route route obligation
          in
          Ok (Some routed, routes))
  | Sst.Exec -> (
      match routes with
      | route :: routes
        when
          Verification_session.proof_activation_route_matches route obligation ->
          let* routed =
            Verification_session.proof_activation_route route obligation
          in
          Ok (Some routed, routes)
      | [] | _ :: _ -> Ok (None, routes))
  | Sst.Spec -> Ok (None, routes)

let activations_for_obligation nonproof_entry_activations = function
  | Some routed -> Verification_session.routed_proof_activations routed
  | None -> nonproof_entry_activations

let prepare_recursive_job ~solver_policy recursive_verification ~activations
    routed ~canonical_index obligation =
  match recursive_verification.authority with
  | None -> Error "recursive proof obligation has no verified totality authority"
  | Some verified ->
      let controls = Recursive_spec_encoding.snapshot_solver_controls () in
      let ground_constructors =
        match routed with
        | None -> []
        | Some routed ->
            Verification_session.routed_ground_constructors routed
      in
      let prepared =
        Recursive_spec_encoding.prepare_proof ~controls verified ~activations
          ~ground_constructors ~routed:(Option.is_some routed) obligation
      in
      (match prepared with
      | Error error -> Error (Recursive_spec_encoding.error_to_string error)
      | Ok prepared ->
          Vc_solver_job_private.prepare_recursive ~canonical_index
            ~solver_policy
            ~force_initial_inconclusive:
              !force_recursive_local_inconclusive_for_testing
            ~retry_rlimit:!recursive_retry_rlimit_for_testing prepared obligation
          |> Result.map_error Vc_solver_job_private.error_to_string)

let prepare_job ~solver_policy recursive_verification ~activations routed
    ~canonical_index obligation =
  if Vir.obligation_has_recursive_specification obligation then
    prepare_recursive_job ~solver_policy recursive_verification ~activations
      routed ~canonical_index obligation
  else if
    Vir.obligation_has_structural_rank obligation
  then
    Vc_solver_job_private.prepare_direct
      ~route:Vc_solver_job_private.Structural_rank ~canonical_index
      ~solver_policy obligation
    |> Result.map_error Vc_solver_job_private.error_to_string
  else if Vir.obligation_has_logical_aggregate_construction obligation then
    Vc_solver_job_private.prepare_direct
      ~route:Vc_solver_job_private.Logical_aggregate ~canonical_index
      ~solver_policy obligation
    |> Result.map_error Vc_solver_job_private.error_to_string
  else
    Vc_solver_job_private.prepare_ordinary ~canonical_index ~solver_policy
      obligation
    |> Result.map_error Vc_solver_job_private.error_to_string

let direct_requirements =
  [
    Logic_ir.Named_sorts;
    Logic_ir.Uninterpreted_functions;
    Logic_ir.Linear_integer_arithmetic;
    Logic_ir.Quantifiers;
    Logic_ir.Explicit_patterns;
    Logic_ir.Quantifier_ids;
  ]

let worker_ground_plan prepared =
  match Recursive_spec_encoding.prepared_ground_classification prepared with
  | None -> None
  | Some (outcome, counters) ->
      Some
        {
          Function_vc_worker_private.ground_complete_violation =
            (outcome = Recursive_spec_encoding.Ground_complete_violation);
          ground_attempts = counters.attempts;
          ground_complete_violations = counters.complete_violations;
          ground_abstentions = counters.abstentions;
          ground_antecedents_checked = counters.antecedents_checked;
        }

let worker_retry_control prepared =
  match Recursive_spec_encoding.prepared_retry_control prepared with
  | `Real -> Function_vc_worker_private.Retry_real
  | `Unknown -> Retry_unknown
  | `Counterexample -> Retry_counterexample

let prepare_worker_vc threaded ~activations routed ~canonical_index
    obligation =
  let policy = threaded.threaded_policy in
  let timeout_ms = Solver_policy_private.timeout_ms policy in
  let rlimit = Solver_policy_private.rlimit policy in
  let make ~deliver_projection_model route projection_manifest retry_query =
    [%log.trace "prepared coordinator model delivery policy"
      ~stage:(Delator.Field.string "coordinator-model-delivery")
      ~canonical_index:(Delator.Field.int canonical_index)
      ~projection_count:(Delator.Field.int (List.length projection_manifest))
      ~deliver_projection_model:
        (Delator.Field.bool deliver_projection_model)
      ~decision:
        (Delator.Field.string
           (if deliver_projection_model then "preserved" else "suppressed"))];
    Ok
      {
        prepared_obligation = obligation;
        projection_manifest;
        deliver_projection_model;
        worker_vc =
          {
            Function_vc_worker_private.canonical_index;
            timeout_ms;
            rlimit;
            route;
          };
        retry_query;
      }
  in
  if Vir.obligation_has_recursive_specification obligation then
    match threaded.threaded_recursive.authority with
    | None ->
        Error
          "recursive proof obligation has no verified totality authority"
    | Some verified ->
        let controls = Recursive_spec_encoding.snapshot_solver_controls () in
        let ground_constructors =
          match routed with
          | None -> []
          | Some routed ->
              Verification_session.routed_ground_constructors routed
        in
        let prepared =
          Recursive_spec_encoding.prepare_proof ~controls verified
            ~activations ~ground_constructors
            ~routed:(Option.is_some routed) obligation
        in
        Result.bind
          (Result.map_error Recursive_spec_encoding.error_to_string prepared)
          (fun prepared ->
               let initial_query =
                 Recursive_spec_encoding.prepared_initial_query prepared
                 |> Recursive_spec_encoding.detach_prepared_query
               in
               let retry_query =
                 Recursive_spec_encoding.prepared_retry_query prepared
               in
               let retry_detached =
                 Option.map Recursive_spec_encoding.detach_prepared_query
                   retry_query
               in
               let retry_rlimit =
                 Option.value !recursive_retry_rlimit_for_testing
                   ~default:rlimit
               in
               let force_initial_inconclusive =
                 !force_recursive_local_inconclusive_for_testing
                 &&
                 match obligation.Vir.kind with
                 | Vir.Local_assertion _ -> true
                 | Vir.Arithmetic_safety _ | Vir.Assertion _
                 | Vir.Postcondition _ | Vir.Call_precondition _
                 | Vir.Callback_precondition _ | Vir.Invariant_validity _
                 | Vir.Entry_measure_nonnegative _
                 | Vir.Recursive_call_measure_nonnegative _
                 | Vir.Recursive_call_strict_descent _ ->
                     false
               in
               make ~deliver_projection_model:false
                 (Function_vc_worker_private.Recursive
                    {
                      initial_query;
                      retry_query = retry_detached;
                      retry_eligible =
                        Recursive_spec_encoding.prepared_retry_eligible
                          prepared;
                      ground_plan = worker_ground_plan prepared;
                      force_initial_inconclusive;
                      retry_control = worker_retry_control prepared;
                      retry_rlimit;
                    })
                 [] retry_query)
  else
    let requires, direct, route =
      if Vir.obligation_has_structural_rank obligation then
        ( direct_requirements,
          true,
          fun query deliver_original_model ->
            Function_vc_worker_private.Structural_rank
              { direct_query = query; deliver_original_model } )
      else if Vir.obligation_has_logical_aggregate_construction obligation
      then
        ( direct_requirements,
          true,
          fun query deliver_original_model ->
            Function_vc_worker_private.Logical_aggregate
              { direct_query = query; deliver_original_model } )
      else
        ( [],
          false,
          fun query _ -> Function_vc_worker_private.Ordinary query )
    in
    Result.bind
      (Z3_bridge.detach_vir ~requires obligation
      |> Result.map_error Z3_bridge.error_to_string)
      (fun (query, projection_manifest) ->
        let has_bv_projection =
          List.exists
            (fun metadata ->
              Option.is_some (Z3_bridge.projection_width metadata))
            projection_manifest
        in
        let deliver_projection_model =
          if direct then has_bv_projection else true
        in
        make ~deliver_projection_model
          (route query deliver_projection_model)
          projection_manifest None)

let prepare_function threaded ~source_ordinal
    (request : Verification_pipeline.solve_request) =
  let execution = request.execution in
  let nonproof_entry_activations =
    nonproof_activations threaded.threaded_recursive execution
  in
  let rec loop canonical_index prepared routes error = function
    | [] ->
        let error =
          match (error, routes) with
          | None, _ :: _ -> Some "proof activation route was unused"
          | (None | Some _), [] | Some _, _ :: _ -> error
        in
        let prepared_vcs = List.rev prepared in
        let injected =
          !preparation_error_source_ordinal_for_testing = Some source_ordinal
        in
        let prepared_vcs = if injected then [] else prepared_vcs in
        let error =
          if injected then
            Some
              (Printf.sprintf "injected preparation error ordinal=%d"
                 source_ordinal)
          else error
        in
        Ok
          {
            prepared_source_ordinal = source_ordinal;
            prepared_vcs;
            preparation_error = error;
            request =
              {
                Function_vc_worker_private.source_ordinal;
                vcs = List.map (fun vc -> vc.worker_vc) prepared_vcs;
              };
            policy = threaded.threaded_policy;
          }
    | _ :: _ when Option.is_some error ->
        loop canonical_index prepared routes error []
    | obligation :: rest -> (
        match route_obligation execution.Vir.mode routes obligation with
        | Error message ->
            loop canonical_index prepared routes (Some message) []
        | Ok (routed, routes) ->
            let activations =
              activations_for_obligation nonproof_entry_activations routed
            in
            match
              prepare_worker_vc threaded ~activations routed
                ~canonical_index obligation
            with
            | Error message ->
                loop canonical_index prepared routes (Some message) []
            | Ok vc ->
                loop (canonical_index + 1) (vc :: prepared) routes None
                  rest)
  in
  loop 0 [] request.proof_activation_routes None execution.Vir.obligations
[@@delator.instrument] [@@delator.level trace]

let worker_request prepared = prepared.request

let commit_job job solved =
  if
    Vc_solver_job_private.result_index solved
    <> Vc_solver_job_private.job_index job
    || Vc_solver_job_private.result_obligation solved
       <> Vc_solver_job_private.job_obligation job
  then Error "prepared solver job returned a mismatched canonical index"
  else (
    List.iteri
      (fun attempt_index telemetry ->
        Option.iter
          (fun observer ->
            observer
              ~canonical_index:(Vc_solver_job_private.job_index job)
              ~attempt_index telemetry)
          !local_counter_commit_observer_for_testing;
        Z3_bridge.commit_local_counters telemetry)
      (Vc_solver_job_private.attempt_telemetry solved);
    Option.iter Solver_backend_counter_private.commit
      (Vc_solver_job_private.ordinary_contribution solved);
    let recursive =
      Vc_solver_job_private.recursive_observations solved
    in
    Recursive_spec_encoding.commit_prepared_observations
      ~proof_queries:recursive.proof_queries ~retry:recursive.retry
      ~ground:recursive.ground
      ~last_retry_query:recursive.last_retry_query;
    Vc_solver_job_private.outcome solved
    |> Result.map_error Vc_solver_job_private.error_to_string)

let solve_execution ~solver_policy recursive_verification _config
    (request : Verification_pipeline.solve_request) =
  let execution = request.execution in
  let nonproof_entry_activations =
    nonproof_activations recursive_verification execution
  in
  let rec loop canonical_index results routes = function
    | [] ->
        if routes = [] then Ok (List.rev results)
        else Error "proof activation route was unused"
    | obligation :: rest ->
        let* routed, routes =
          route_obligation execution.Vir.mode routes obligation
        in
        let activations =
          activations_for_obligation nonproof_entry_activations routed
        in
        let* job =
          prepare_job ~solver_policy recursive_verification ~activations routed
            ~canonical_index obligation
        in
        let solved = Vc_solver_job_private.solve_prepared job in
        let* outcome = commit_job job solved in
        let completed = { Solver_backend.obligation; outcome } :: results in
        match outcome with
        | Solver_backend.Verified ->
            loop (canonical_index + 1) completed routes rest
        | Counterexample _ | Inconclusive _ -> Ok (List.rev completed)
  in
  loop 0 [] request.proof_activation_routes execution.Vir.obligations
[@@delator.instrument] [@@delator.level debug]

let configure ~solver_policy preflight =
  match
    Solver_backend.config_with_rlimit
      ~rlimit:(Solver_policy_private.rlimit solver_policy)
      ~timeout_ms:(Solver_policy_private.timeout_ms solver_policy)
  with
  | Error error ->
      Error (Verification_pipeline.Solver_configuration_error error)
  | Ok config ->
      let source_ordinal = ref 0 in
      Ok
        (fun (request : Verification_pipeline.solve_request) ->
          let current_source_ordinal = !source_ordinal in
          incr source_ordinal;
          if
            !preparation_error_source_ordinal_for_testing
            = Some current_source_ordinal
          then (
            [%log.debug "injecting solver preparation dependency failure"
              ~source_ordinal:(Delator.Field.int current_source_ordinal)];
            Error "verification preparation could not authenticate its inputs")
          else solve_execution ~solver_policy preflight config request)
[@@delator.instrument] [@@delator.level debug]

let add_telemetry (left : Z3_bridge.counters)
    (right : Z3_bridge.counters) =
  {
    Z3_bridge.capability_resolutions =
      left.capability_resolutions + right.capability_resolutions;
    translations = left.translations + right.translations;
    contexts_created = left.contexts_created + right.contexts_created;
    solvers_created = left.solvers_created + right.solvers_created;
    solver_resets = left.solver_resets + right.solver_resets;
    contexts_cleaned = left.contexts_cleaned + right.contexts_cleaned;
    contexts_live = left.contexts_live + right.contexts_live;
    maximum_contexts_live =
      max left.maximum_contexts_live right.maximum_contexts_live;
    selected_logics = left.selected_logics @ right.selected_logics;
  }

let empty_telemetry =
  {
    Z3_bridge.capability_resolutions = 0;
    translations = 0;
    contexts_created = 0;
    solvers_created = 0;
    solver_resets = 0;
    contexts_cleaned = 0;
    contexts_live = 0;
    maximum_contexts_live = 0;
    selected_logics = [];
  }

let backend_reason = function
  | Z3_bridge.Resource_exhausted -> Solver_backend.Resource_exhausted
  | Timed_out -> Timed_out
  | Backend_unknown message -> Backend_unknown message

let detached_integer rendered =
  try Z.of_string rendered
  with _ ->
    let length = String.length rendered in
    if
      length >= 5
      && String.starts_with ~prefix:"(- " rendered
      && rendered.[length - 1] = ')'
    then
      Z.neg (Z.of_string (String.sub rendered 3 (length - 4)))
    else raise (Invalid_argument "invalid detached integer model value")

let aggregate_identity rendered =
  match List.rev (String.split_on_char '!' rendered) with
  | index :: "val" :: _ -> (
      try Z.of_string index
      with _ -> Z.of_int (Hashtbl.hash rendered))
  | _ -> (
      try detached_integer rendered
      with _ -> Z.of_int (Hashtbl.hash rendered))

let detached_value_identity = function
  | Z3_bridge.Detached_integer (identity, _)
  | Detached_boolean (identity, _)
  | Detached_aggregate (identity, _)
  | Detached_bit_vector (identity, _, _) ->
      identity

let reject_detached_model_value metadata reason message =
  let symbol = Z3_bridge.projection_symbol metadata in
  let width =
    match Z3_bridge.projection_width metadata with
    | Some width -> Bv_width.to_int width
    | None -> 0
  in
  [%log.error "detached model projection failed coordinator validation"
    ~stage:(Delator.Field.string "coordinator-model-reassembly")
    ~reason:(Delator.Field.string reason)
    ~projection_order:
      (Delator.Field.int (Z3_bridge.projection_order metadata))
    ~symbol_id:(Delator.Field.int symbol.Vir.symbol_id)
    ~expected_width:(Delator.Field.int width)
    ~decision:(Delator.Field.string "rejected")];
  Error message
[@@delator.instrument] [@@delator.level error]

let reject_detached_model_header ~expected_count ~observed_count reason message =
  [%log.error "detached model manifest failed coordinator validation"
    ~stage:(Delator.Field.string "coordinator-model-reassembly")
    ~reason:(Delator.Field.string reason)
    ~expected_count:(Delator.Field.int expected_count)
    ~observed_count:(Delator.Field.int observed_count)
    ~decision:(Delator.Field.string "rejected")];
  Error message
[@@delator.instrument] [@@delator.level error]

let validate_detached_model_headers projection_manifest values =
  let expected_count = List.length projection_manifest
  and observed_count = List.length values in
  if expected_count <> observed_count then
    reject_detached_model_header ~expected_count ~observed_count "count"
      "worker model projection count does not match coordinator"
  else
    let seen_expected = Hashtbl.create expected_count
    and seen_observed = Hashtbl.create observed_count in
    let rec validate_manifest order = function
      | [] -> Ok ()
      | metadata :: rest ->
          let identity = Z3_bridge.projection_identity metadata in
          if Z3_bridge.projection_order metadata <> order then
            reject_detached_model_header ~expected_count ~observed_count
              "manifest-order"
              "coordinator model projection manifest order is invalid"
          else if Hashtbl.mem seen_expected identity then
            reject_detached_model_header ~expected_count ~observed_count
              "manifest-duplicate-identity"
              "coordinator model projection manifest identity is duplicated"
          else (
            Hashtbl.add seen_expected identity ();
            validate_manifest (order + 1) rest)
    in
    let rec validate_observed = function
      | [] -> Ok ()
      | None :: rest -> validate_observed rest
      | Some value :: rest ->
          let identity = detached_value_identity value in
          if Hashtbl.mem seen_observed identity then
            reject_detached_model_header ~expected_count ~observed_count
              "response-duplicate-identity"
              "worker model projection identity is duplicated"
          else (
            Hashtbl.add seen_observed identity ();
            validate_observed rest)
    in
    let rec validate_order manifest observed =
      match (manifest, observed) with
      | [], [] -> Ok ()
      | _ :: manifest, None :: observed -> validate_order manifest observed
      | metadata :: manifest, Some value :: observed ->
          let expected_identity = Z3_bridge.projection_identity metadata
          and observed_identity = detached_value_identity value in
          if String.equal expected_identity observed_identity then
            validate_order manifest observed
          else
            reject_detached_model_value metadata "response-order-or-identity"
              "worker model projection identity/order does not match coordinator"
      | [], _ :: _ | _ :: _, [] ->
          reject_detached_model_header ~expected_count ~observed_count "count"
            "worker model projection count does not match coordinator"
    in
    let* () = validate_manifest 0 projection_manifest in
    let* () = validate_observed values in
    validate_order projection_manifest values

let detached_model_value metadata = function
  | None -> (
      match Z3_bridge.projection_width metadata with
      | Some _ ->
          reject_detached_model_value metadata "mandatory-value-absent"
            "worker omitted a mandatory BV model projection"
      | None -> Ok None)
  | Some value ->
      match (Z3_bridge.projection_width metadata, value) with
      | None, Z3_bridge.Detached_integer (_, rendered) -> (
          match (Z3_bridge.projection_symbol metadata).Vir.sort with
          | Vir.Integer ->
              (try Ok (Some (Solver_backend.Integer (detached_integer rendered)))
               with _ ->
                 reject_detached_model_value metadata "integer-payload"
                   "worker returned a malformed integer model value")
          | Vir.Boolean | Bit_vector _ | Aggregate _ | Parametric _ ->
              reject_detached_model_value metadata "sort"
                "worker model projection sort does not match coordinator")
      | None, Detached_boolean (_, observed) -> (
          match (Z3_bridge.projection_symbol metadata).Vir.sort with
          | Vir.Boolean -> Ok (Some (Solver_backend.Boolean observed))
          | Vir.Integer | Bit_vector _ | Aggregate _ | Parametric _ ->
              reject_detached_model_value metadata "sort"
                "worker model projection sort does not match coordinator")
      | None, Detached_aggregate (_, rendered) -> (
          match (Z3_bridge.projection_symbol metadata).Vir.sort with
          | Vir.Aggregate _ ->
              Ok
                (Some
                   (Solver_backend.Aggregate_identity
                      (aggregate_identity rendered)))
          | Vir.Integer | Boolean | Bit_vector _ | Parametric _ ->
              reject_detached_model_value metadata "sort"
                "worker model projection sort does not match coordinator")
      | Some expected_width,
        Detached_bit_vector (_, observed_reference, residue) -> (
          match Z3_bridge.projection_width_reference metadata with
          | None ->
              reject_detached_model_value metadata "missing-width-reference"
                "coordinator BV projection lost its width reference"
          | Some expected_reference
            when not (String.equal expected_reference observed_reference) ->
              reject_detached_model_value metadata "width-reference"
                "worker BV model width reference does not match coordinator"
          | Some _ ->
              let capability =
                Bv_backend_capability_receipt_private.capability ()
              in
              let* observed_width =
                match
                  Bv_width.decode_reference capability observed_reference
                with
                | Ok width -> Ok width
                | Error _ ->
                    reject_detached_model_value metadata
                      "width-reference-decode"
                      "worker BV model width reference is invalid"
              in
              if not (Bv_width.equal expected_width observed_width) then
                reject_detached_model_value metadata "width"
                  "worker BV model width does not match coordinator"
              else
                let* decoded =
                  match Bv_value.of_string ~width:expected_width residue with
                  | Ok value -> Ok value
                  | Error _ ->
                      reject_detached_model_value metadata "residue"
                        "worker BV model residue is invalid"
                in
                [%log.trace "reassembled mandatory detached BV projection"
                  ~stage:
                    (Delator.Field.string "coordinator-bv-model-reassembly")
                  ~projection_order:
                    (Delator.Field.int (Z3_bridge.projection_order metadata))
                  ~width:(Delator.Field.int (Bv_width.to_int expected_width))
                  ~residue_decimal_bytes:
                    (Delator.Field.int (String.length residue))
                  ~decision:(Delator.Field.string "accepted")];
                Ok (Some (Solver_backend.Bit_vector decoded)))
      | ( Some _,
          (Detached_integer _ | Detached_boolean _ | Detached_aggregate _) )
      | None, Detached_bit_vector _ ->
          reject_detached_model_value metadata "sort"
            "worker model projection sort does not match coordinator"

let detached_outcome policy projection_manifest = function
  | Z3_bridge.Detached_verified -> Ok Solver_backend.Verified
  | Detached_inconclusive reason ->
      Ok
        (Solver_backend.Inconclusive
           {
             configured_timeout_ms =
               Solver_policy_private.timeout_ms policy;
             configured_rlimit = Solver_policy_private.rlimit policy;
             reason = backend_reason reason;
           })
  | Detached_counterexample values ->
      let* () = validate_detached_model_headers projection_manifest values in
      let rec reassemble bindings manifest values =
        match (manifest, values) with
        | [], [] -> Ok (List.rev bindings)
        | metadata :: manifest, value :: values ->
            let* value = detached_model_value metadata value in
            reassemble
              ({ Solver_backend.symbol =
                   Z3_bridge.projection_symbol metadata;
                 value }
              :: bindings)
              manifest values
        | [], _ :: _ | _ :: _, [] ->
            Error "worker model projection count does not match coordinator"
      in
      let* bindings = reassemble [] projection_manifest values in
      Ok (Solver_backend.Counterexample bindings)

let commit_worker_vc policy prepared
    (result : Function_vc_worker_private.vc_result) =
  if result.result_index <> prepared.worker_vc.canonical_index then
    Error "worker VC result returned a mismatched canonical index"
  else (
    List.iteri
      (fun attempt_index telemetry ->
        Option.iter
          (fun observer ->
            observer ~canonical_index:result.result_index ~attempt_index
              telemetry)
          !local_counter_commit_observer_for_testing;
        Z3_bridge.commit_local_counters telemetry)
      result.attempt_telemetry;
    if result.ordinary_contribution then
      result.attempt_telemetry
      |> List.fold_left add_telemetry empty_telemetry
      |> Solver_backend_counter_private.contribution
      |> Solver_backend_counter_private.commit;
    let retry = result.retry_observation in
    let ground = result.ground_observation in
    Recursive_spec_encoding.commit_prepared_observations
      ~proof_queries:result.proof_queries
      ~retry:
        {
          Recursive_spec_encoding.attempts = retry.retry_attempts;
          queries = retry.retry_queries;
          facts = retry.retry_facts;
          verified = retry.retry_verified;
          counterexamples = retry.retry_counterexamples;
          inconclusives = retry.retry_inconclusives;
          abstentions = retry.retry_abstentions;
        }
      ~ground:
        {
          Recursive_spec_encoding.attempts =
            ground.observed_ground_attempts;
          complete_violations =
            ground.observed_ground_complete_violations;
          abstentions = ground.observed_ground_abstentions;
          antecedents_checked = ground.observed_ground_antecedents_checked;
        }
      ~last_retry_query:
        (if retry.retry_used then prepared.retry_query else None);
    match result.result_outcome with
    | Error message -> Error message
    | Ok outcome ->
        detached_outcome policy
          (if prepared.deliver_projection_model then
             prepared.projection_manifest
           else [])
          outcome)

let commit_function prepared
    (worker : Function_vc_worker_private.result) =
  if worker.result_source_ordinal <> prepared.prepared_source_ordinal then
    Error "worker function result returned a mismatched source ordinal"
  else
    let worker_error index message =
      Error
        (Printf.sprintf "worker function ordinal=%d vc=%d raised: %s"
           prepared.prepared_source_ordinal index message)
    in
    let rec loop completed prepared_vcs worker_results =
      match (prepared_vcs, worker_results) with
      | [], [] -> (
          match worker.worker_exception with
          | Some (index, message) -> worker_error index message
          | None -> (
              match prepared.preparation_error with
              | Some message -> Error message
              | None -> Ok (List.rev completed)))
      | prepared_vc :: _, [] -> (
          match worker.worker_exception with
          | Some (index, message)
            when index = prepared_vc.worker_vc.canonical_index ->
              worker_error index message
          | Some _ ->
              Error "worker exception returned a mismatched canonical index"
          | None -> Error "worker omitted a prepared canonical VC")
      | [], _ :: _ -> Error "worker returned an unprepared canonical VC"
      | prepared_vc :: prepared_rest, result :: result_rest -> (
          match commit_worker_vc prepared.policy prepared_vc result with
          | Error _ as error -> error
          | Ok outcome ->
              let obligation_result =
                {
                  Solver_backend.obligation =
                    prepared_vc.prepared_obligation;
                  outcome;
                }
              in
              match outcome with
              | Solver_backend.Verified ->
                  loop (obligation_result :: completed) prepared_rest
                    result_rest
              | Counterexample _ | Inconclusive _ ->
                  if result_rest <> [] then
                    Error
                      "worker continued after the first nonverified VC"
                  else Ok (List.rev (obligation_result :: completed)))
    in
    loop [] prepared.prepared_vcs worker.vc_results

let configure_threaded ~solver_policy preflight =
  match
    Solver_backend.config_with_rlimit
      ~rlimit:(Solver_policy_private.rlimit solver_policy)
      ~timeout_ms:(Solver_policy_private.timeout_ms solver_policy)
  with
  | Error error ->
      Error (Verification_pipeline.Solver_configuration_error error)
  | Ok _ ->
      Ok
        {
          threaded_policy = solver_policy;
          threaded_recursive = preflight;
        }
[@@delator.instrument] [@@delator.level debug]

module For_testing = struct
  let force_recursive_local_inconclusive force =
    force_recursive_local_inconclusive_for_testing := force

  let recursive_retry_rlimit rlimit =
    recursive_retry_rlimit_for_testing := rlimit

  let inject_preparation_error ordinal =
    preparation_error_source_ordinal_for_testing := ordinal

  let observe_local_counter_commits observer callback =
    match !local_counter_commit_observer_for_testing with
    | Some _ ->
        invalid_arg "local counter commit observation is already active"
    | None ->
        local_counter_commit_observer_for_testing := Some observer;
        Fun.protect
          ~finally:(fun () ->
            local_counter_commit_observer_for_testing := None)
          callback
end
