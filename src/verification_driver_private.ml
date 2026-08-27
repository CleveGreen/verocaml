type completion = {
  issuer : unit ref;
  token : unit ref;
  implementation : Cmt_input.implementation;
  validated : Sst_validation.validated_program;
  program_snapshot : string;
  provider_completion : Verified_provider_completion_private.t;
  session_destroyed : bool;
}

type report = {
  status : Verification_pipeline.status;
  semantic_sst : string Lazy.t;
  vir : Vir.program;
  functions : int;
  obligations : int;
  results : Solver_backend.obligation_result list;
  counters : Verification_session.counters;
  validated : Sst_validation.validated_program;
  completion : completion option;
}

type error =
  | Frontend_error of Diagnostic.t
  | Validation_error of Sst_validation.error
  | Invariant_error of Type_invariant.error
  | Pipeline_error of Verification_pipeline.error
  | Internal_error of string

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

let run_with_policy ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?external_specifications ?imported
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
          | Ok invariants ->
              let semantic_sst =
                lazy (render_semantic_sst program validated)
              in
              let preflight = ref None in
              let run_preflight () =
                match
                  Verification_solver_private.preflight ~solver_policy program
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
              let configure_solver () =
                match !preflight with
                | None ->
                    Error
                      (Verification_pipeline.Internal_setup_error
                         "recursive verification preflight was not retained")
                | Some preflight ->
                    Verification_solver_private.configure ~solver_policy
                      preflight
              in
              let proof_entry_activations () =
                match !preflight with
                | None -> fun (_ : Sst.function_id) -> []
                | Some preflight ->
                    Verification_solver_private.proof_entry_activations
                      preflight
              in
              let results = ref [] in
              match
                Verification_pipeline.run_validated ~imports ~implementation
                  ~program ~validated ~invariants ~preflight:run_preflight
                  ~proof_entry_activations
                  ~configure_solver
                  ~on_result:(fun result -> results := result :: !results)
              with
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
                                  completion = None;
                                }
                          | None -> Error (Pipeline_error error))
                      | None -> Error (Pipeline_error error))
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
                          semantic_sst;
                          vir = outcome.vir;
                          functions = outcome.functions;
                          obligations = outcome.obligations;
                          results = List.rev !results;
                          counters = pipeline.counters;
                          validated;
                          completion;
                        }))
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


let run_with_policy_threaded ~threads ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?external_specifications ?imported
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
          | Ok invariants ->
              let semantic_sst =
                lazy (render_semantic_sst program validated)
              in
              let preflight = ref None in
              let run_preflight () =
                match
                  Verification_solver_private.preflight ~solver_policy program
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
                                 Verification_solver_private.prepare_function
                                   threaded;
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
              match
                Verification_pipeline.run_validated_with_threads ~threads ~imports
                  ~implementation
                  ~program ~validated ~invariants ~preflight:run_preflight
                  ~proof_entry_activations
                  ~configure_solver
                  ~on_result:(fun result -> results := result :: !results)
              with
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
                                  completion = None;
                                }
                          | None -> Error (Pipeline_error error))
                      | None -> Error (Pipeline_error error))
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
                          semantic_sst;
                          vir = outcome.vir;
                          functions = outcome.functions;
                          obligations = outcome.obligations;
                          results = List.rev !results;
                          counters = pipeline.counters;
                          validated;
                          completion;
                        }))
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


let run_with_policy_and_threads ~threads ~solver_policy ~allow_imported_opens
    ?(allow_public_parametric_signatures = false) ?external_specifications ?imported
    implementation =
  if threads = 1 then
    run_with_policy ~solver_policy ~allow_imported_opens
      ~allow_public_parametric_signatures ?external_specifications ?imported implementation
  else
    match Physical_core_count_private.validate_threads threads with
    | Error message -> Error (Internal_error message)
    | Ok () ->
        run_with_policy_threaded ~threads ~solver_policy ~allow_imported_opens
          ~allow_public_parametric_signatures ?external_specifications ?imported
          implementation

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

let status report = report.status
let semantic_sst report = Lazy.force report.semantic_sst
let semantic_sst_lazy report = report.semantic_sst
let vir report = report.vir
let functions report = report.functions
let obligations report = report.obligations
let results report = report.results
let counters report = report.counters
let validated report = report.validated

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

module For_testing = struct
  let reset_driver_entries () = driver_entries := 0
  let driver_entries () = !driver_entries
end
