let () = ignore Private_receipt_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 1)
    format

type prepared_input = {
  implementation : Cmt_input.implementation;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  invariants : Type_invariant.environment;
}

let prepare filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.code
  in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.code
  in
  let validated =
    match Sst_validation.validate program with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  { implementation; program; validated; invariants }

let create_session input =
  match
    Verification_session.create ~imports:None
      ~implementation:input.implementation
      ~validated:input.validated ~invariants:input.invariants
  with
  | Ok session -> session
  | Error message -> fail "%s" message

let schedule filename =
  let input = prepare filename in
  let session = create_session input in
  Fun.protect
    ~finally:(fun () -> Verification_session.destroy session)
    (fun () ->
      match
        Symbolic_executor_private.prepare_program ~imports:None ~session
          ~validated:input.validated ~invariants:input.invariants input.program
      with
      | Error error ->
          let kind =
            match error.Symbolic_executor_private.unsupported with
            | Symbolic_executor_private.Malformed_sst
                "recursive verified-result receipt edge is unsupported" ->
                "recursive-receipt-edge"
            | Symbolic_executor_private.Malformed_sst
                "imported, external, or trusted result cannot be a receipt dependency"
              ->
                "untrusted-receipt-source"
            | _ -> "unsupported"
          in
          Printf.printf "schedule=rejected function=%s kind=%s\n"
            error.function_name kind
      | Ok prepared ->
          Symbolic_executor_private.scheduled_functions prepared
          |> List.iter (fun scheduled ->
              let definition =
                Symbolic_executor_private.scheduled_definition scheduled
              in
              Printf.printf "%s source=%b dependent=%b\n"
                definition.Sst.function_id.function_name
                (Symbolic_executor_private.scheduled_is_receipt_source scheduled)
                (Symbolic_executor_private.scheduled_is_receipt_dependent
                   scheduled)))

let find_definition input name =
  input.program.Sst.functions
  |> List.find (fun definition ->
      String.equal definition.Sst.function_id.function_name name)

let identity filename =
  let input = prepare filename in
  let session = create_session input in
  Fun.protect
    ~finally:(fun () -> Verification_session.destroy session)
    (fun () ->
      let boxed = find_definition input "Box.make" in
      let local = find_definition input "make" in
      let get accessor definition =
        match accessor session definition with
        | Ok value -> value
        | Error message -> fail "%s" message
      in
      let boxed_leaf =
        get Verification_session.canonical_callable_leaf_name boxed
      in
      let local_leaf =
        get Verification_session.canonical_callable_leaf_name local
      in
      let boxed_path =
        get Verification_session.canonical_callable_resolved_path boxed
      in
      let local_path =
        get Verification_session.canonical_callable_resolved_path local
      in
      let boxed_uid =
        get Verification_session.canonical_callable_binding_uid boxed
      in
      let local_uid =
        get Verification_session.canonical_callable_binding_uid local
      in
      let boxed_key = get Verification_session.canonical_callable_key boxed in
      let local_key = get Verification_session.canonical_callable_key local in
      Printf.printf
        "identity same-display=%b distinct-paths=%b distinct-uids=%b \
         distinct-keys=%b\n"
        (String.equal boxed_leaf local_leaf)
        (not (String.equal boxed_path local_path))
        (not (String.equal boxed_uid local_uid))
        (not (String.equal boxed_key local_key)))

let solver_config () =
  match Solver_backend.config ~timeout_ms:5000 with
  | Ok config -> config
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let solve_obligation config obligation =
  let outcome =
    match Solver_backend.solve_obligation config obligation with
    | Ok outcome -> outcome
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  { Solver_backend.obligation; outcome }

let solve_all config execution =
  List.map (solve_obligation config) execution.Vir.obligations

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let run_production input ~incomplete_source =
  let configure_solver () =
    let config = solver_config () in
    Ok
      (fun (request : Verification_pipeline.solve_request) ->
        if incomplete_source && request.receipt_source then
          match request.execution.Vir.obligations with
          | first :: _ :: _ -> Ok [ solve_obligation config first ]
          | [] | [ _ ] ->
              Error "receipt source did not expose an incomplete prefix"
        else Ok (solve_all config request.execution))
  in
  match
    Verification_pipeline.run_validated ~imports:None
      ~implementation:input.implementation
      ~program:input.program ~validated:input.validated
      ~invariants:input.invariants
      ~preflight:(fun () -> Ok 0)
      ~proof_entry_activations:(fun () _ -> [])
      ~configure_solver
      ~on_result:(fun _ -> ())
  with
  | Error message -> fail "%s" message
  | Ok report -> (
      match report.outcome with
      | Ok completion -> (completion, report)
      | Error (Verification_pipeline.Engine_error error) ->
          fail "%s" (Symbolic_executor_private.error_to_string error)
      | Error (Setup_error (Internal_setup_error message))
      | Error (Solve_error message) ->
          fail "%s" message
      | Error (Setup_error (Solver_configuration_error error)) ->
          fail "%s" (Solver_backend.error_to_string error))

let production_sessions filename =
  let input = prepare filename in
  let first, first_report = run_production input ~incomplete_source:false in
  let second, second_report = run_production input ~incomplete_source:true in
  let first_counters = first_report.Verification_pipeline.counters in
  let second_counters = second_report.Verification_pipeline.counters in
  Printf.printf
    "production-sessions first=%s first-issued=%d first-consumed=%d \
     first-destroyed=%b second=%s second-issued=%d second-consumed=%d \
     second-dependent-lowerings=%d second-dependent-backends=%d \
     second-dependent-solvers=%d second-destroyed=%b\n"
    (status_name first.status) first_counters.receipts_issued
    first_counters.receipts_consumed first_report.session_destroyed
    (status_name second.status)
    second_counters.receipts_issued second_counters.receipts_consumed
    second_counters.dependent_lowerings
    second_counters.dependent_backend_contexts
    second_counters.dependent_solver_attempts second_report.session_destroyed

let () =
  match Array.to_list Sys.argv with
  | [ _; "matrix" ] ->
      Verification_session.For_testing.adversarial_matrix ()
      @ Symbolic_executor_private.For_testing.scheduler_matrix ()
      |> List.iter print_endline
  | [ _; "schedule"; filename ] -> schedule filename
  | [ _; "identity"; filename ] -> identity filename
  | [ _; "sessions"; filename ] -> production_sessions filename
  | _ ->
      failwith
        "usage: private_receipt_tool (matrix|schedule FILE|identity \
         FILE|sessions FILE)"
