type completion = {
  issuer : unit ref;
  token : unit ref;
  implementation : Cmt_input.implementation;
  validated : Sst_validation.validated_program;
  program_snapshot : string;
  provider_completion : Verified_provider_completion_private.t;
  proof_evidence : Verification_proof_evidence_private.t option;
  session_destroyed : bool;
}

type report = {
  status : Verification_pipeline.status;
  original_obligations : Numeric_original_obligation_private.t list;
  semantic_sst : string Lazy.t;
  vir : Vir.program;
  functions : int;
  obligations : int;
  results : Solver_backend.obligation_result list;
  counters : Verification_session.counters;
  validated : Sst_validation.validated_program;
  prior_law_evidence : Numeric_prior_law_closure_private.coordinator option;
  completion : completion option;
}

type error =
  | Frontend_error of Diagnostic.t
  | Validation_error of Sst_validation.error
  | Invariant_error of Type_invariant.error
  | Provider_surface_error of string
  | Pipeline_error of Verification_pipeline.error
  | Internal_error of string

let driver_error_of_pipeline_error = function
  | Verification_pipeline.Engine_error
      {
        Symbolic_executor_private.unsupported = Missing_decreases;
        function_name;
        span;
      } ->
      let detail =
        Printf.sprintf
          "Recursive function %S needs one decreases clause. Add [%%verocaml.decreases ...] at the beginning of its body."
          function_name
      in
      [%log.debug "routed resolved recursive rank failure to frontend diagnostic"
        ~stage:(Delator.Field.string "termination-correlation")
        ~route:(Delator.Field.string "resolved-call-scc")
        ~failure_class:(Delator.Field.string "missing-measure")
        ~diagnostic_code:
          (Delator.Field.string "VERO_INVALID_RECURSIVE_RANK")
        ~decision:(Delator.Field.string "frontend-rejection")];
      Frontend_error
        (Diagnostic.make (Diagnostic.Invalid_recursive_rank detail) span)
  | error -> Pipeline_error error

let render_semantic_sst program validated =
  String.concat "\n"
    [
      Sst.to_string program;
      "instance-modes";
      Sst_validation.instance_mode_dump validated;
      "finite-formals";
      Sst_validation.finite_formal_dump validated;
      "";
    ]

let completion_issuer = ref ()
let driver_entries = ref 0

let uses_external_target_specification (vir : Vir.program) =
  List.exists
    (fun (execution : Vir.function_execution) ->
      List.exists
        (function
          | Vir.Trusted_external_target_specification_use _ -> true
          | Trusted_external_specification_use _
          | Trusted_external_body_use _ ->
              false)
        execution.trusted_summary_uses)
    vir.functions

let prepare_numeric_bv_source ~imports ~implementation ~validated = function
  | None -> Ok None
  | Some request ->
      Numeric_bv_source_admission_private.create ~implementation ~validated
        ~registration:imports request

let run_with_policy ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?(capture_numeric_obligations = false)
    ?numeric_native ?validate_provider_surface
    ?external_specifications ?imported implementation =
  incr driver_entries;
  let lowered =
    match imported with
    | None ->
        Result.map
          (fun program -> (`Public program, program))
          (Typedtree_lowering.lower ~allow_imported_opens implementation)
    | Some imported ->
        Result.map
          (fun lowered ->
            (`Imported lowered, Typedtree_lowering_private.program lowered))
          (Typedtree_lowering_private.lower
             ~allow_public_parametric_signatures ?external_specifications ~imported
             implementation)
  in
  match lowered with
  | Error diagnostic -> Error (Frontend_error diagnostic)
  | Ok (authority, program) ->
      let imports =
        match authority with
        | `Public _ -> None
        | `Imported lowered ->
            Some (Typedtree_lowering_private.registration lowered)
      in
      let execute () =
      let validation =
        match authority with
        | `Public _ -> Sst_validation.validate program
        | `Imported lowered ->
            (match
               Typedtree_lowering_private.external_registration lowered
             with
            | None ->
                Sst_validation_private.Public.validate_with_imports
                  (Typedtree_lowering_private.registration lowered)
                  program
            | Some external_specifications ->
                Sst_validation_private.Public.validate_with_imports_and_external
                  (Typedtree_lowering_private.registration lowered)
                  external_specifications program)
      in
      match validation with
      | Error error -> Error (Validation_error error)
      | Ok validated -> (
          match
            Option.fold ~none:(Ok ())
              ~some:(fun validate -> validate validated)
              validate_provider_surface
          with
          | Error message -> Error (Provider_surface_error message)
          | Ok () -> (
          match Type_invariant.authenticate validated with
          | Error error -> Error (Invariant_error error)
          | Ok invariants -> (
              match
                prepare_numeric_bv_source ~imports ~implementation ~validated
                  numeric_native
              with
              | Error message -> Error (Provider_surface_error message)
              | Ok numeric_bv_source ->
              let prior_law_evidence =
                Numeric_prior_law_closure_private.For_pipeline.create
                  ~implementation ~program ~validated
              in
              let semantic_sst =
                lazy (render_semantic_sst program validated)
              in
              let preflight = ref None in
              let run_preflight () =
                match
                  Verification_solver_private.preflight_validated ~solver_policy
                    validated
                with
                | Error error -> Error error
                | Ok prepared ->
                    preflight := Some prepared;
                    (match
                       Verification_solver_private.terminal_result prepared
                     with
                    | Some _ ->
                        Error
                          (Verification_pipeline.Internal_setup_error
                             "recursive verification stopped inconclusively")
                    | None ->
                        Ok
                          (Verification_solver_private.termination_obligations
                             prepared))
              in
              let original_obligations = ref [] in
              let configure_solver () =
                match !preflight with
                | None ->
                    Error
                      (Verification_pipeline.Internal_setup_error
                         "recursive verification preflight was not retained")
                | Some preflight ->
                    Verification_solver_private.configure ~solver_policy
                      preflight
                    |> Result.map (fun solve (request : Verification_pipeline.solve_request) ->
                      if capture_numeric_obligations then begin
                        let captured = Numeric_original_obligation_private.For_pipeline.capture
                          ~implementation ~imported ~program ~definition:request.definition request.execution in
                        original_obligations := List.rev_append captured !original_obligations
                      end;
                      solve request)
              in
              let proof_entry_activations () =
                match !preflight with
                | None -> fun (_ : Sst.function_id) -> []
                | Some preflight ->
                    Verification_solver_private.proof_entry_activations
                      preflight
              in
              let results = ref [] in
              let pipeline_result =
                Verification_pipeline.run_validated ~imports ~numeric_bv_source
                  ~implementation
                  ~program ~validated ~invariants ~preflight:run_preflight
                  ~proof_entry_activations
                  ~configure_solver
                  ~on_function_commit:
                    (fun definition execution results ->
                      Numeric_prior_law_closure_private.For_pipeline.observe
                        prior_law_evidence ~definition ~execution results;
                      Option.iter
                        (fun source ->
                          Numeric_bv_source_admission_private.observe_prior_law
                            source prior_law_evidence ~definition)
                        numeric_bv_source)
                  ~on_result:(fun result -> results := result :: !results)
              in
              Numeric_prior_law_closure_private.For_pipeline.close
                prior_law_evidence;
              match pipeline_result with
              | Error message -> Error (Internal_error message)
              | Ok pipeline -> (
                  match pipeline.outcome with
                  | Error error -> (
                      match !preflight with
                      | Some preflight -> (
                          match
                            Verification_solver_private.terminal_result
                              preflight
                          with
                          | Some result ->
                              Ok
                                {
                                  status = Verification_pipeline.Inconclusive;
                                  original_obligations = List.rev !original_obligations;
                                  semantic_sst;
                                  vir =
                                    {
                                      Vir.policy = program.policy;
                                      rank_domains =
                                        Vir.rank_domains_of_validated validated;
                                      trusted_external_body_declarations = [];
                                      functions = [];
                                    };
                                  functions = 0;
                                  obligations = 1;
                                  results = [ result ];
                                  counters = pipeline.counters;
                                  validated;
                                  prior_law_evidence = Some prior_law_evidence;
                                  completion = None;
                                }
                          | None -> Error (driver_error_of_pipeline_error error))
                      | None -> Error (driver_error_of_pipeline_error error))
                  | Ok outcome ->
                      let completion =
                        match outcome.status with
                        | Verification_pipeline.Verified
                          when not (uses_external_target_specification outcome.vir) ->
                            Some
                              {
                                issuer = completion_issuer;
                                token = ref ();
                                implementation;
                                validated;
                                program_snapshot = Sst.to_string program;
                                proof_evidence = pipeline.proof_evidence;
                                provider_completion =
                                  Verified_provider_completion_private.For_driver.issue
                                    ~implementation ~program;
                                session_destroyed = pipeline.session_destroyed;
                              }
                        | Verified | Counterexample | Inconclusive
                        | Incomplete_source ->
                            None
                      in
                      Ok
                        {
                          status = outcome.status;
                          original_obligations = List.rev !original_obligations;
                          semantic_sst;
                          vir = outcome.vir;
                          functions = outcome.functions;
                          obligations = outcome.obligations;
                          results = List.rev !results;
                          counters = pipeline.counters;
                          validated;
                          prior_law_evidence = Some prior_law_evidence;
                          completion;
                        }))))
      in
      (match authority with
      | `Public _ -> execute ()
      | `Imported lowered ->
          Fun.protect
            ~finally:(fun () ->
              Imported_callable.invalidate_registration
                (Typedtree_lowering_private.registration lowered);
              Option.iter
                External_target_specification_private.invalidate
                (Typedtree_lowering_private.external_registration lowered))
            execute)
[@@delator.instrument] [@@delator.level debug]


let run_with_policy_threaded ~threads ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?(capture_numeric_obligations = false)
    ?numeric_native ?external_specifications ?imported
    implementation =
  incr driver_entries;
  let lowered =
    match imported with
    | None ->
        Result.map
          (fun program -> (`Public program, program))
          (Typedtree_lowering.lower ~allow_imported_opens implementation)
    | Some imported ->
        Result.map
          (fun lowered ->
            (`Imported lowered, Typedtree_lowering_private.program lowered))
          (Typedtree_lowering_private.lower
             ~allow_public_parametric_signatures ?external_specifications ~imported
             implementation)
  in
  match lowered with
  | Error diagnostic -> Error (Frontend_error diagnostic)
  | Ok (authority, program) ->
      let imports =
        match authority with
        | `Public _ -> None
        | `Imported lowered ->
            Some (Typedtree_lowering_private.registration lowered)
      in
      let execute () =
      let validation =
        match authority with
        | `Public _ -> Sst_validation.validate program
        | `Imported lowered ->
            (match
               Typedtree_lowering_private.external_registration lowered
             with
            | None ->
                Sst_validation_private.Public.validate_with_imports
                  (Typedtree_lowering_private.registration lowered)
                  program
            | Some external_specifications ->
                Sst_validation_private.Public.validate_with_imports_and_external
                  (Typedtree_lowering_private.registration lowered)
                  external_specifications program)
      in
      match validation with
      | Error error -> Error (Validation_error error)
      | Ok validated -> (
          match Type_invariant.authenticate validated with
          | Error error -> Error (Invariant_error error)
          | Ok invariants -> (
              match
                prepare_numeric_bv_source ~imports ~implementation ~validated
                  numeric_native
              with
              | Error message -> Error (Provider_surface_error message)
              | Ok numeric_bv_source ->
              let prior_law_evidence =
                Numeric_prior_law_closure_private.For_pipeline.create
                  ~implementation ~program ~validated
              in
              let semantic_sst =
                lazy (render_semantic_sst program validated)
              in
              let preflight = ref None in
              let run_preflight () =
                match
                  Verification_solver_private.preflight_validated ~solver_policy
                    validated
                with
                | Error error -> Error error
                | Ok prepared ->
                    preflight := Some prepared;
                    (match
                       Verification_solver_private.terminal_result prepared
                     with
                    | Some _ ->
                        Error
                          (Verification_pipeline.Internal_setup_error
                             "recursive verification stopped inconclusively")
                    | None ->
                        Ok
                          (Verification_solver_private.termination_obligations
                             prepared))
              in
              let original_obligations = ref [] in
              let configure_solver () =
                match !preflight with
                | None ->
                    Error
                      (Verification_pipeline.Internal_setup_error
                         "recursive verification preflight was not retained")
                | Some preflight ->
                    Verification_solver_private.configure_threaded ~solver_policy
                      preflight
                    |> Result.map (fun threaded ->
                           Verification_pipeline.Threaded_solve
                             {
                               prepare =
                                 (fun ~source_ordinal (request : Verification_pipeline.solve_request) ->
                                   if capture_numeric_obligations then begin
                                     let captured = Numeric_original_obligation_private.For_pipeline.capture
                                       ~implementation ~imported ~program ~definition:request.definition request.execution in
                                     original_obligations := List.rev_append captured !original_obligations
                                   end;
                                   Verification_solver_private.prepare_function threaded ~source_ordinal request);
                               worker_request =
                                 Verification_solver_private.worker_request;
                               commit =
                                 Verification_solver_private.commit_function;
                             })
              in
              let proof_entry_activations () =
                match !preflight with
                | None -> fun (_ : Sst.function_id) -> []
                | Some preflight ->
                    Verification_solver_private.proof_entry_activations
                      preflight
              in
              let results = ref [] in
              let pipeline_result =
                Verification_pipeline.run_validated_with_threads ~threads ~imports
                  ~numeric_bv_source
                  ~implementation
                  ~program ~validated ~invariants ~preflight:run_preflight
                  ~proof_entry_activations
                  ~configure_solver
                  ~on_function_commit:
                    (fun definition execution results ->
                      Numeric_prior_law_closure_private.For_pipeline.observe
                        prior_law_evidence ~definition ~execution results;
                      Option.iter
                        (fun source ->
                          Numeric_bv_source_admission_private.observe_prior_law
                            source prior_law_evidence ~definition)
                        numeric_bv_source)
                  ~on_result:(fun result -> results := result :: !results)
              in
              Numeric_prior_law_closure_private.For_pipeline.close
                prior_law_evidence;
              match pipeline_result with
              | Error message -> Error (Internal_error message)
              | Ok pipeline -> (
                  match pipeline.outcome with
                  | Error error -> (
                      match !preflight with
                      | Some preflight -> (
                          match
                            Verification_solver_private.terminal_result
                              preflight
                          with
                          | Some result ->
                              Ok
                                {
                                  status = Verification_pipeline.Inconclusive;
                                  original_obligations = List.rev !original_obligations;
                                  semantic_sst;
                                  vir =
                                    {
                                      Vir.policy = program.policy;
                                      rank_domains =
                                        Vir.rank_domains_of_validated validated;
                                      trusted_external_body_declarations = [];
                                      functions = [];
                                    };
                                  functions = 0;
                                  obligations = 1;
                                  results = [ result ];
                                  counters = pipeline.counters;
                                  validated;
                                  prior_law_evidence = Some prior_law_evidence;
                                  completion = None;
                                }
                          | None -> Error (driver_error_of_pipeline_error error))
                      | None -> Error (driver_error_of_pipeline_error error))
                  | Ok outcome ->
                      let completion =
                        match outcome.status with
                        | Verification_pipeline.Verified
                          when not (uses_external_target_specification outcome.vir) ->
                            Some
                              {
                                issuer = completion_issuer;
                                token = ref ();
                                implementation;
                                validated;
                                program_snapshot = Sst.to_string program;
                                proof_evidence = pipeline.proof_evidence;
                                provider_completion =
                                  Verified_provider_completion_private.For_driver.issue
                                    ~implementation ~program;
                                session_destroyed = pipeline.session_destroyed;
                              }
                        | Verified | Counterexample | Inconclusive
                        | Incomplete_source ->
                            None
                      in
                      Ok
                        {
                          status = outcome.status;
                          original_obligations = List.rev !original_obligations;
                          semantic_sst;
                          vir = outcome.vir;
                          functions = outcome.functions;
                          obligations = outcome.obligations;
                          results = List.rev !results;
                          counters = pipeline.counters;
                          validated;
                          prior_law_evidence = Some prior_law_evidence;
                          completion;
                        })))
      in
      (match authority with
      | `Public _ -> execute ()
      | `Imported lowered ->
          Fun.protect
            ~finally:(fun () ->
              Imported_callable.invalidate_registration
                (Typedtree_lowering_private.registration lowered);
              Option.iter
                External_target_specification_private.invalidate
                (Typedtree_lowering_private.external_registration lowered))
            execute)
[@@delator.instrument] [@@delator.level debug]


let run_with_policy_and_threads ~threads ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?(capture_numeric_obligations = false)
    ?numeric_native ?external_specifications ?imported
    implementation =
  if threads = 1 then
    run_with_policy ~solver_policy ~allow_imported_opens
      ~allow_public_parametric_signatures ~capture_numeric_obligations
      ?numeric_native ?external_specifications ?imported implementation
  else
    match Physical_core_count_private.validate_threads threads with
    | Error message -> Error (Internal_error message)
    | Ok () ->
        run_with_policy_threaded ~threads ~solver_policy ~allow_imported_opens
          ~allow_public_parametric_signatures ~capture_numeric_obligations ?external_specifications ?imported
          ?numeric_native
          implementation
[@@delator.instrument] [@@delator.level debug]

let run ?rlimit ~timeout_ms ~allow_imported_opens ?external_specifications ?imported
    implementation =
  let policy =
    match rlimit with
    | None -> Solver_policy_private.create_default ~timeout_ms
    | Some rlimit -> Solver_policy_private.create ~timeout_ms ~rlimit
  in
  match policy with
  | Error policy_error ->
      Error
        (Pipeline_error
           (Verification_pipeline.Setup_error
              (Verification_pipeline.Solver_configuration_error
                 (Solver_backend.Invalid_configuration
                    (Solver_policy_private.error_to_string policy_error)))))
  | Ok solver_policy ->
      run_with_policy ~solver_policy ~allow_imported_opens ?external_specifications ?imported
        implementation
[@@delator.instrument] [@@delator.level debug]

let run_with_threads ~threads ?rlimit ~timeout_ms ~allow_imported_opens
    ?external_specifications ?imported implementation =
  if threads = 1 then
    run ?rlimit ~timeout_ms ~allow_imported_opens ?external_specifications ?imported
      implementation
  else
    let policy =
      match rlimit with
      | None -> Solver_policy_private.create_default ~timeout_ms
      | Some rlimit -> Solver_policy_private.create ~timeout_ms ~rlimit
    in
    match policy with
    | Error policy_error ->
        Error
          (Pipeline_error
             (Verification_pipeline.Setup_error
                (Verification_pipeline.Solver_configuration_error
                   (Solver_backend.Invalid_configuration
                      (Solver_policy_private.error_to_string policy_error)))))
    | Ok solver_policy ->
        run_with_policy_and_threads ~threads ~solver_policy
          ~allow_imported_opens ?external_specifications ?imported implementation
[@@delator.instrument] [@@delator.level debug]

let status report = report.status
let semantic_sst report = Lazy.force report.semantic_sst
let semantic_sst_lazy report = report.semantic_sst
let vir report = report.vir
let functions report = report.functions
let obligations report = report.obligations
let results report = report.results
let counters report = report.counters
let validated report = report.validated
let original_obligations report = report.original_obligations
let prior_law_evidence report = report.prior_law_evidence

let verified_completion report =
  match report.completion with
  | Some completion
    when completion.issuer == completion_issuer
         && completion.token != completion_issuer
         && Sst_validation.program completion.validated
            == Sst_validation.program report.validated
         && completion.session_destroyed
         && String.equal completion.program_snapshot
              (Sst.to_string (Sst_validation.program completion.validated))
         && Verified_provider_completion_private.authenticates
              completion.provider_completion
              ~implementation:completion.implementation
              ~program:(Sst_validation.program completion.validated) ->
      Some completion
  | Some _ | None -> None

let completion_matches completion ~implementation ~validated =
  completion.issuer == completion_issuer
  && completion.token != completion_issuer
  && completion.implementation == implementation
  && completion.validated == validated
  && completion.session_destroyed
  && String.equal completion.program_snapshot
       (Sst.to_string (Sst_validation.program validated))
  && Verified_provider_completion_private.authenticates
       completion.provider_completion ~implementation
       ~program:(Sst_validation.program validated)

let provider_completion completion =
  if
    completion.issuer == completion_issuer
    && completion.token != completion_issuer
    && completion.session_destroyed
    && Verified_provider_completion_private.authenticates
         completion.provider_completion
         ~implementation:completion.implementation
         ~program:(Sst_validation.program completion.validated)
  then Some completion.provider_completion
  else None

let proof_evidence completion =
  if not (completion_matches completion ~implementation:completion.implementation
            ~validated:completion.validated)
  then None
  else
    Option.bind completion.proof_evidence (fun evidence ->
        if Verification_proof_evidence_private.matches_program evidence
          (Sst_validation.program completion.validated)
        then Some evidence else None)

module For_testing = struct
  let reset_driver_entries () = driver_entries := 0
  let driver_entries () = !driver_entries
end
