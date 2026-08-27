let () = ignore Solver_resource_policy_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let require label condition =
  if not condition then fail "%s" label

let span =
  Diagnostic.
    {
      file = "solver_resource_policy.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let obligation goal =
  Vir.
    {
      obligation_index = 0;
      function_ref = { function_index = 0; function_name = "resource_probe" };
      kind = Assertion { assertion_ordinal = 0 };
      span;
      assumptions = [];
      required_preceding_safety = [];
      path_condition = [];
      goal;
      projection_symbols = [];
    }

let direct_config = Z3_bridge.{ timeout_ms = 60_000; model = true }

let require_balanced label expected =
  let counters = Z3_bridge.counters () in
  require (label ^ ": contexts")
    (counters.contexts_created = expected
    && counters.contexts_cleaned = expected
    && counters.contexts_live = 0);
  require (label ^ ": solvers")
    (counters.solvers_created = expected
    && counters.solver_resets = expected);
  require (label ^ ": isolation")
    (counters.maximum_contexts_live <= 1)

let expect_direct_reason label expected result =
  match result with
  | Ok (Z3_bridge.Inconclusive reason) when reason = expected -> ()
  | Ok _ -> fail "%s: direct query returned a conclusive outcome" label
  | Error error -> fail "%s: %s" label (Z3_bridge.error_to_string error)

let unit () =
  let default =
    match Solver_policy_private.create_default ~timeout_ms:60_000 with
    | Ok policy -> policy
    | Error error -> fail "%s" (Solver_policy_private.error_to_string error)
  in
  require "default timeout changed"
    (Solver_policy_private.timeout_ms default = 60_000);
  require "default rlimit is not finite and positive"
    (Solver_policy_private.rlimit default
    = Solver_policy_private.default_rlimit
    && Solver_policy_private.default_rlimit > 0);
  List.iter
    (fun (timeout_ms, rlimit) ->
      match Solver_policy_private.create ~timeout_ms ~rlimit with
      | Error _ -> ()
      | Ok _ -> fail "invalid policy %d/%d was accepted" timeout_ms rlimit)
    [ (0, 1); (-1, 1); (1, 0); (1, -1) ];
  let old_config =
    match Solver_backend.config ~timeout_ms:60_000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let explicit_config =
    match
      Solver_backend.config_with_rlimit ~timeout_ms:60_000 ~rlimit:17
    with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  require "old backend entry did not use the sole default"
    (Solver_backend.rlimit old_config
    = Solver_policy_private.default_rlimit);
  require "explicit backend entry changed its budget"
    (Solver_backend.timeout_ms explicit_config = 60_000
    && Solver_backend.rlimit explicit_config = 17);
  Z3_bridge.reset_counters ();
  (match
     Z3_bridge.solve_vir ~rlimit:0 direct_config
       (obligation (Vir.Boolean_constant true))
   with
  | Error (Z3_bridge.Invalid_configuration _) -> ()
  | Error error ->
      fail "zero rlimit had the wrong error: %s"
        (Z3_bridge.error_to_string error)
  | Ok _ -> fail "zero rlimit reached a solver");
  require_balanced "invalid" 0;
  Z3_bridge.reset_counters ();
  expect_direct_reason "backend unknown"
    (Z3_bridge.Backend_unknown "controlled unknown")
    (Z3_bridge.solve_vir ~controlled:Force_unknown ~rlimit:17 direct_config
       (obligation (Vir.Boolean_constant true)));
  require_balanced "backend unknown" 1;
  Z3_bridge.reset_counters ();
  expect_direct_reason "timeout" Z3_bridge.Timed_out
    (Z3_bridge.solve_vir ~controlled:Force_timeout ~rlimit:17 direct_config
       (obligation (Vir.Boolean_constant true)));
  require_balanced "timeout" 1;
  Z3_bridge.reset_counters ();
  let low () =
    Z3_bridge.solve_vir ~rlimit:1 direct_config
      (obligation (Vir.Boolean_constant true))
  in
  expect_direct_reason "rlimit first" Z3_bridge.Resource_exhausted (low ());
  expect_direct_reason "rlimit second" Z3_bridge.Resource_exhausted (low ());
  require_balanced "independent low queries" 2;
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let low_backend =
    match
      Solver_backend.config_with_rlimit ~timeout_ms:60_000 ~rlimit:1
    with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  (match
     Solver_backend.solve_obligation low_backend
       (obligation (Vir.Boolean_constant true))
   with
  | Ok
      (Solver_backend.Inconclusive
        {
          configured_timeout_ms = 60_000;
          configured_rlimit = 1;
          reason = Resource_exhausted;
        }) ->
      ()
  | Ok _ -> fail "ordinary low-budget query was not resource-exhausted"
  | Error error -> fail "%s" (Solver_backend.error_to_string error));
  require "ordinary backend did not use direct Z3"
    (Solver_backend.For_testing.solver_creation_count () = 1);
  require_balanced "ordinary backend" 1;
  print_endline
    "policy default=finite validation=positive-only reasons=resource/timeout/backend-unknown isolation=fresh cleanup=balanced ordinary=direct-z3"

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let outcome = function
  | Solver_backend.Verified -> "verified"
  | Counterexample _ -> "counterexample"
  | Inconclusive { reason = Resource_exhausted; _ } ->
      "resource-exhausted"
  | Inconclusive { reason = Timed_out; _ } -> "timeout"
  | Inconclusive { reason = Backend_unknown _; _ } -> "backend-unknown"

let kind = function
  | Vir.Entry_measure_nonnegative _ -> "entry-measure-nonnegative"
  | Recursive_call_measure_nonnegative _ ->
      "recursive-call-measure-nonnegative"
  | Recursive_call_strict_descent _ -> "recursive-call-strict-descent"
  | Arithmetic_safety _ | Assertion _ | Postcondition _
  | Call_precondition _ | Callback_precondition _ | Local_assertion _
  | Invariant_validity _ ->
      "execution"

let write_file filename contents =
  let channel = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let retained timeout_ms rlimit filename dump_paths =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_ground_counterexample_counters ();
  Recursive_spec_encoding.For_testing.reset_nullary_branch_retry_counters ();
  let report =
    match
      match rlimit with
      | None ->
          Verification_driver_private.run ~timeout_ms
            ~allow_imported_opens:false implementation
      | Some rlimit ->
          Verification_driver_private.run ~rlimit ~timeout_ms
            ~allow_imported_opens:false implementation
    with
    | Ok report -> report
    | Error _ -> fail "retained verification failed before a report"
  in
  let results = Verification_driver_private.results report in
  let first =
    match results with
    | result :: _ -> result
    | [] -> fail "retained verification produced no solver result"
  in
  let direct = Z3_bridge.counters () in
  let session = Verification_driver_private.counters report in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  Printf.printf
    "status=%s functions=%d obligations=%d first=%s#%d/%s/%s results=%d\n"
    (status (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)
    first.obligation.function_ref.function_name
    first.obligation.function_ref.function_index
    (kind first.obligation.kind) (outcome first.outcome)
    (List.length results);
  Printf.printf
    "native contexts=%d solvers=%d resets=%d cleaned=%d live=%d max-live=%d backend=%d\n"
    direct.contexts_created direct.solvers_created direct.solver_resets
    direct.contexts_cleaned direct.contexts_live direct.maximum_contexts_live
    (Solver_backend.For_testing.solver_creation_count ());
  Printf.printf
    "frontier callee=%d/%d/%d dependent=%d/%d/%d receipts=%d/%d retry=%d/%d ground=%d\n"
    session.callee_solver_attempts session.callee_verified_results
    session.callee_failed_results session.dependent_lowerings
    session.dependent_backend_contexts session.dependent_solver_attempts
    session.receipts_issued session.receipts_consumed retry.attempts
    retry.queries ground.attempts;
  (match dump_paths with
  | None -> ()
  | Some (sst_path, vir_path) ->
      write_file sst_path (Verification_driver_private.semantic_sst report);
      write_file vir_path
        (Vir.to_string (Verification_driver_private.vir report)));
  match Verification_driver_private.status report with
  | Verification_pipeline.Verified -> ()
  | Counterexample -> exit 1
  | Inconclusive -> exit 3
  | Incomplete_source -> exit 2

let () =
  let retained_rlimit = function
    | "default" -> None
    | rlimit -> Some (int_of_string rlimit)
  in
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> unit ()
  | [ _; "retained"; timeout_ms; rlimit; filename ] ->
      retained (int_of_string timeout_ms) (retained_rlimit rlimit) filename
        None
  | [ _; "retained"; timeout_ms; rlimit; filename; sst_path; vir_path ] ->
      retained (int_of_string timeout_ms) (retained_rlimit rlimit) filename
        (Some (sst_path, vir_path))
  | _ ->
      fail
        "usage: solver_resource_policy_tool (unit|retained TIMEOUT RLIMIT CMT \
         [SST VIR])"
