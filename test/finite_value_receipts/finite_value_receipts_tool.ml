let () = ignore Finite_value_receipts_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let invariants validated =
  match Type_invariant.authenticate validated with
  | Ok invariants -> invariants
  | Error error -> fail "%s" (Type_invariant.error_to_string error)

let reset () =
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let render_counters counters =
  Printf.sprintf
    "witness=%d parent=%d child=%d result-witness=%d finalization=%d consumption=%d recursive=%d dependent-lowering=%d backend=%d solver=%d"
    counters.Verification_session.finite_witness_issuances
    counters.finite_parent_issuances counters.finite_child_derivations
    counters.finite_result_witness_records counters.finite_result_finalizations
    counters.finite_consumptions
    (Symbolic_executor_private.For_testing.authority_observation ())
      .recursive_spec_lowerings
    counters.dependent_lowerings counters.dependent_backend_contexts
    counters.dependent_solver_attempts

let run_with outcome filename =
  reset ();
  let implementation = load filename in
  let program = lower implementation in
  let validated = validate program in
  let invariants = invariants validated in
  let solve_calls = ref 0 in
  let configure_solver () =
    Ok
      (fun (request : Verification_pipeline.solve_request) ->
        incr solve_calls;
        Ok
          (List.map
             (fun obligation ->
               {
                 Solver_backend.obligation;
                 outcome;
               })
             request.execution.Vir.obligations))
  in
  let report =
    match
      Verification_pipeline.run_validated ~imports:None ~implementation
        ~program ~validated
        ~invariants ~preflight:(fun () -> Ok 0) ~configure_solver
        ~proof_entry_activations:(fun () _ -> [])
        ~on_result:(fun _ -> ())
    with
    | Ok report -> report
    | Error message -> fail "%s" message
  in
  (report, !solve_calls)

let run filename = run_with Solver_backend.Verified filename

let positive filename =
  let report, solve_calls = run filename in
  match report.outcome with
  | Error (Verification_pipeline.Engine_error error) ->
      fail "%s" (Symbolic_executor_private.error_to_string error)
  | Error (Setup_error _) -> fail "positive setup error"
  | Error (Solve_error message) -> fail "%s" message
  | Ok completion ->
      let counters = report.counters in
      if
        completion.status <> Verification_pipeline.Verified
        || counters.finite_witness_issuances = 0
        || counters.finite_parent_issuances = 0
        || counters.finite_child_derivations = 0
        || counters.finite_consumptions = 0
      then fail "positive did not exercise every structural finite operation";
      Printf.printf "status=%s functions=%d obligations=%d solve-calls=%d %s\n"
        (status completion.status) completion.functions completion.obligations
        solve_calls (render_counters counters)

let transfer filename =
  let report, solve_calls = run filename in
  match report.outcome with
  | Error (Verification_pipeline.Engine_error error) ->
      fail "%s" (Symbolic_executor_private.error_to_string error)
  | Error (Setup_error _) -> fail "transfer setup error"
  | Error (Solve_error message) -> fail "%s" message
  | Ok completion ->
      let counters = report.counters in
      if
        completion.status <> Verification_pipeline.Verified
        || counters.receipts_issued = 0 || counters.receipts_consumed = 0
        || counters.finite_result_witness_records = 0
        || counters.finite_result_finalizations = 0
        || counters.finite_consumptions = 0
      then fail "transfer did not require both private result authorities";
      Printf.printf
        "status=%s completed-issued=%d completed-consumed=%d solve-calls=%d %s\n"
        (status completion.status) counters.receipts_issued
        counters.receipts_consumed solve_calls (render_counters counters)

let negative filename =
  let report, solve_calls = run filename in
  match report.outcome with
  | Ok _ -> fail "negative unexpectedly lowered"
  | Error _ ->
      let counters = report.counters in
      let authority =
        Symbolic_executor_private.For_testing.authority_observation ()
      in
      if
        counters.finite_witness_issuances <> 0
        || counters.finite_parent_issuances <> 0
        || counters.finite_child_derivations <> 0
        || counters.finite_result_witness_records <> 0
        || counters.finite_result_finalizations <> 0
        || counters.finite_consumptions <> 0
        || authority.recursive_spec_lowerings <> 0 || solve_calls <> 0
        || Solver_backend.For_testing.solver_creation_count () <> 0
      then fail "negative crossed the finite/pre-solver authority boundary";
      Printf.printf "rejected solve-calls=0 %s session-destroyed=%b\n"
        (render_counters counters) report.session_destroyed

let failed_transfer filename =
  let report, solve_calls =
    run_with (Solver_backend.Counterexample []) filename
  in
  match report.outcome with
  | Error error ->
      fail "failed-transfer pipeline error: %s"
        (match error with
        | Verification_pipeline.Engine_error error ->
            Symbolic_executor_private.error_to_string error
        | Setup_error _ -> "setup"
        | Solve_error message -> message)
  | Ok completion ->
      let counters = report.counters in
      if
        completion.status <> Verification_pipeline.Counterexample
        || counters.finite_result_witness_records = 0
        || counters.receipts_issued <> 0
        || counters.finite_result_finalizations <> 0
        || counters.finite_consumptions <> 0
        || counters.dependent_lowerings <> 0
        || counters.dependent_backend_contexts <> 0
        || counters.dependent_solver_attempts <> 0
      then fail "failed callee finalized or transferred finite authority";
      Printf.printf
        "status=counterexample completed-issued=0 finalization=0 consumption=0 \
         dependent=0/0/0 solve-calls=%d result-witness=%d\n"
        solve_calls counters.finite_result_witness_records

let frontend_negative filename =
  reset ();
  let rejected =
    match Cmt_input.load filename with
    | Error _ -> true
    | Ok implementation -> (
        match Typedtree_lowering.lower implementation with
        | Error _ -> true
        | Ok program -> (
            match Sst_validation.validate program with
            | Error _ -> true
            | Ok _ -> false))
  in
  let authority =
    Symbolic_executor_private.For_testing.authority_observation ()
  in
  let z3 = Z3_bridge.counters () in
  if
    not rejected || authority.recursive_spec_lowerings <> 0
    || Solver_backend.For_testing.solver_creation_count () <> 0
    || z3.contexts_created <> 0 || z3.solvers_created <> 0
  then fail "frontend negative crossed a private authority boundary";
  Printf.printf
    "rejected finite=0/0/0/0/0/0 recursive=0 backend=0 solver=0 \
     session=not-created\n"

let () =
  match Array.to_list Sys.argv with
  | [ _; "matrix" ] ->
      List.iter print_endline
        (Finite_value_registry.For_testing.adversarial_matrix ())
  | [ _; "positive"; filename ] -> positive filename
  | [ _; "transfer"; filename ] -> transfer filename
  | [ _; "failed-transfer"; filename ] -> failed_transfer filename
  | [ _; "frontend-negative"; filename ] -> frontend_negative filename
  | [ _; "negative"; filename ] -> negative filename
  | _ ->
      fail
        "usage: finite_value_receipts_tool \
         (matrix|positive|transfer|failed-transfer|frontend-negative FILE)"
