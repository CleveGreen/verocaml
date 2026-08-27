let () = ignore Recursive_invariant_transition_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let artifact name = Filename.concat "artifacts" (name ^ ".cmt")

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop offset =
    offset + needle_length <= text_length
    &&
    (String.sub text offset needle_length = needle || loop (offset + 1))
  in
  needle_length = 0 || loop 0

let transition_steps report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.filter_map (fun obligation ->
                match obligation.Vir.kind with
                | Vir.Invariant_validity
                    {
                      boundary =
                        Vir.Transition_preservation
                          {
                            transition_kind;
                            pre_version;
                            successor_version;
                            _;
                          };
                      _;
                    } ->
                    Some
                      ( (match transition_kind with
                        | Vir.Nested_transition -> "nested"
                        | Vir.Direct_root_transition -> "root"
                        | Vir.Rebase_transition -> "rebase"),
                        pre_version,
                        successor_version )
                | Vir.Arithmetic_safety _ | Vir.Assertion _
                | Vir.Local_assertion _ | Vir.Postcondition _
                | Vir.Call_precondition _
                | Vir.Invariant_validity _ | Vir.Callback_precondition _
                | Vir.Entry_measure_nonnegative _
                | Vir.Recursive_call_measure_nonnegative _
                | Vir.Recursive_call_strict_descent _ ->
                    None))

let render_steps steps =
  steps
  |> List.map (fun (kind, before, after) ->
         Printf.sprintf "%s:v%d->v%d" kind before after)
  |> String.concat ","

let terminal_observations report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.fold_left
       (fun count execution ->
         count
         + List.fold_left
             (fun count obligation ->
               match obligation.Vir.kind with
               | Vir.Invariant_validity
                   {
                     boundary =
                       Vir.Terminal_observation { snapshot = true; _ };
                     _;
                   } ->
                   count + 1
               | Vir.Arithmetic_safety _ | Vir.Assertion _
               | Vir.Local_assertion _ | Vir.Postcondition _
               | Vir.Call_precondition _
               | Vir.Invariant_validity _ | Vir.Callback_precondition _
               | Vir.Entry_measure_nonnegative _
               | Vir.Recursive_call_measure_nonnegative _
               | Vir.Recursive_call_strict_descent _ ->
                   count)
             0 execution.Vir.obligations)
       0

let expected_steps = function
  | "constructor_drop" | "root_rebase" ->
      [ ("rebase", 0, 1); ("root", 1, 2) ]
  | "constructor_zero_head" | "equal_branch" | "terminal_snapshot" ->
      [ ("nested", 0, 1) ]
  | "nested_cut" -> [ ("nested", 0, 1); ("root", 1, 2) ]
  | name -> fail "no expected transition sequence for %s" name

let positive name =
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load (artifact name))
  with
  | Error _ -> fail "positive %s did not return a production-driver report" name
  | Ok report ->
      let counters = Verification_driver_private.counters report in
      let steps = transition_steps report in
      let expected = expected_steps name in
      let nested =
        List.fold_left
          (fun count (kind, _, _) ->
            if kind = "nested" then count + 1 else count)
          0 expected
      in
      let roots = List.length expected - nested in
      let expected_terminal = if name = "equal_branch" then 2 else 1 in
      if
        Verification_driver_private.status report
        <> Verification_pipeline.Verified
        || counters.Verification_session.transition_predecessor_transfers <> 1
        || counters.transition_predecessor_consumptions <> 1
        || counters.transition_result_receipts <> 1
        || counters.transition_teardown_removals <> 1
        || counters.transition_preservation_obligations
           <> List.length expected
        || counters.transition_nested_reconstructions <> nested
        || counters.transition_root_reconstructions <> roots
        || steps <> expected
        || terminal_observations report <> expected_terminal
      then fail "positive %s missed exact transition lifecycle evidence" name;
      let vir = Vir.to_string (Verification_driver_private.vir report) in
      let forbidden =
        [
          "invariant-open";
          "points-to";
          "permission";
          "frame-rule";
          "separation";
        ]
      in
      if
        List.exists
          (contains vir)
          forbidden
      then fail "positive %s emitted forbidden structural vocabulary" name;
      Printf.printf
        "positive=%s status=%s lifecycle=%d/%d/%d/%d preservation=%d reconstruction=%d/%d ordered=%s terminal=%d\n"
        name
        (status_name (Verification_driver_private.status report))
        counters.transition_predecessor_transfers
        counters.transition_predecessor_consumptions
        counters.transition_result_receipts
        counters.transition_teardown_removals
        counters.transition_preservation_obligations
        counters.transition_nested_reconstructions
        counters.transition_root_reconstructions (render_steps steps)
        (terminal_observations report)

type totals = {
  transfers : int;
  consumptions : int;
  results : int;
  teardown : int;
  preservation : int;
  nested : int;
  roots : int;
  lowerings : int;
  backends : int;
  solvers : int;
}

let zero =
  {
    transfers = 0;
    consumptions = 0;
    results = 0;
    teardown = 0;
    preservation = 0;
    nested = 0;
    roots = 0;
    lowerings = 0;
    backends = 0;
    solvers = 0;
  }

let add_observation totals
    (row :
      Verification_session.For_testing
      .transition_predecessor_counter_observation) =
  {
    transfers = totals.transfers + row.transfers;
    consumptions = totals.consumptions + row.consumptions;
    results = totals.results + row.result_receipts;
    teardown = totals.teardown + row.teardown_removals;
    preservation = totals.preservation + row.preservation_obligations;
    nested = totals.nested + row.nested_reconstructions;
    roots = totals.roots + row.root_reconstructions;
    lowerings = totals.lowerings + row.dependent_lowerings;
    backends = totals.backends + row.dependent_backend_contexts;
    solvers = totals.solvers + row.dependent_solver_attempts;
  }

let observe callback =
  let result, rows =
    Verification_session.For_testing.observe_transition_predecessor_counters
      callback
  in
  (result, List.fold_left add_observation zero rows)

let run_driver name =
  Verification_driver_private.run ~timeout_ms:5_000
    ~allow_imported_opens:false (load (artifact name))

let zero_work_negative name =
  let result, totals = observe (fun () -> run_driver name) in
  let rejected = match result with Error _ -> true | Ok _ -> false in
  if
    not rejected || totals.transfers <> 0 || totals.consumptions <> 0
    || totals.results <> 0 || totals.preservation <> 0
    || totals.lowerings <> 0 || totals.backends <> 0 || totals.solvers <> 0
  then fail "negative %s crossed its pre-lowering boundary" name;
  Printf.printf
    "negative=%s rejected=true lifecycle=%d/%d/%d teardown=%d dependent=%d/%d/%d preservation=%d\n"
    name totals.transfers totals.consumptions totals.results totals.teardown
    totals.lowerings totals.backends totals.solvers totals.preservation

let use_invariant_negative () =
  let result, totals =
    observe (fun () -> run_driver "use_invariant_from_formal")
  in
  if
    (match result with Error _ -> false | Ok _ -> true)
    || totals.transfers <> 1 || totals.consumptions <> 0
    || totals.results <> 0 || totals.teardown <> 1
    || totals.lowerings <> 1 || totals.backends <> 0 || totals.solvers <> 0
    || totals.preservation <> 0
  then fail "use_invariant_from_formal counter boundary changed";
  Printf.printf
    "negative=use_invariant_from_formal rejected=true lifecycle=%d/%d/%d teardown=%d dependent=%d/%d/%d preservation=%d\n"
    totals.transfers totals.consumptions totals.results totals.teardown
    totals.lowerings totals.backends totals.solvers totals.preservation

let failing_preservation name expected_emitted =
  Solver_backend.For_testing.reset_solver_creation_count ();
  let result = run_driver name in
  match result with
  | Error _ -> fail "%s rejected before its concrete preservation VC" name
  | Ok report ->
      let counters = Verification_driver_private.counters report in
      let results = Verification_driver_private.results report in
      let failing =
        List.filter
          (fun result ->
            match
              (result.Solver_backend.obligation.Vir.kind, result.outcome)
            with
            | ( Vir.Invariant_validity
                  { boundary = Vir.Transition_preservation _; _ },
                Solver_backend.Counterexample _ ) ->
                true
            | _ -> false)
          results
      in
      if
        Verification_driver_private.status report
        <> Verification_pipeline.Counterexample
        || counters.transition_predecessor_transfers <> 1
        || counters.transition_predecessor_consumptions <> 1
        || counters.transition_result_receipts <> 0
        || counters.transition_teardown_removals <> 1
        || counters.transition_preservation_obligations <> 1
        || counters.dependent_lowerings <> 1
        || counters.dependent_backend_contexts <> 1
        || counters.dependent_solver_attempts <> 1
        || List.length failing <> 1
        || List.length results <> 3
        || List.length (transition_steps report) <> expected_emitted
      then fail "%s did not stop at its first ordered failing VC" name;
      Printf.printf
        "negative=%s status=counterexample lifecycle=1/1/0/1 dependent=1/1/1 preservation=1 failing=1 emitted=%d validated-results=%d backend-solvers=%d\n"
        name expected_emitted (List.length results)
        (Solver_backend.For_testing.solver_creation_count ())

let attack_names =
  [
    "program";
    "cmt";
    "family";
    "session";
    "caller";
    "caller-key";
    "caller-path";
    "caller-binding";
    "caller-body";
    "callee";
    "callee-key";
    "callee-path";
    "callee-binding";
    "callee-body";
    "call";
    "path";
    "instance";
    "source";
    "source-call";
    "actual";
    "actual-symbol";
    "actual-path";
    "formal";
    "root";
    "version";
    "stale";
    "mode";
    "type";
    "invariant";
    "model";
    "predicate";
    "obligation";
    "obligation-fingerprint";
    "issuer";
    "affinity";
    "copied";
    "branch-only";
    "divergent-join";
  ]

let attack name =
  Verification_session.For_testing
  .set_transition_predecessor_attack_for_testing (Some name);
  let result, totals =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_transition_predecessor_attack_for_testing None)
      (fun () -> observe (fun () -> run_driver "constructor_zero_head"))
  in
  if
    (match result with Error _ -> false | Ok _ -> true)
    || totals.transfers <> 0 || totals.consumptions <> 0
    || totals.results <> 0 || totals.lowerings <> 0 || totals.backends <> 0
    || totals.solvers <> 0 || totals.preservation <> 0
    || totals.teardown <> 1
  then fail "attack %s crossed reauthentication" name;
  Printf.printf
    "attack=%s rejected lifecycle=0/0/0 teardown=1 dependent=0/0/0 preservation=0\n"
    name

let session_replay () =
  Verification_session.For_testing.reset_transition_predecessor_replay_for_testing ();
  Verification_session.For_testing
  .set_transition_predecessor_attack_for_testing (Some "capture");
  let first, first_totals =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_transition_predecessor_attack_for_testing None)
      (fun () -> observe (fun () -> run_driver "constructor_zero_head"))
  in
  Verification_session.For_testing
  .set_transition_predecessor_attack_for_testing (Some "replay");
  let second, second_totals =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_transition_predecessor_attack_for_testing None)
      (fun () -> observe (fun () -> run_driver "constructor_zero_head"))
  in
  Verification_session.For_testing.reset_transition_predecessor_replay_for_testing ();
  if
    (match first with Ok report ->
       Verification_driver_private.status report
       = Verification_pipeline.Verified
     | Error _ -> false)
    |> not
    || first_totals.transfers <> 1 || first_totals.consumptions <> 1
    || first_totals.results <> 1 || first_totals.teardown <> 1
    || (match second with Error _ -> false | Ok _ -> true)
    || second_totals.transfers <> 0 || second_totals.consumptions <> 0
    || second_totals.results <> 0 || second_totals.lowerings <> 0
    || second_totals.backends <> 0 || second_totals.solvers <> 0
    || second_totals.teardown <> 1
  then fail "same-process session replay boundary changed";
  print_endline
    "session_replay first=1/1/1/1 second=0/0/0/1 dependent-second=0/0/0"

let consumed_reuse () =
  Verification_session.For_testing
  .set_transition_predecessor_double_consume_for_testing true;
  let result, totals =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_transition_predecessor_double_consume_for_testing false)
      (fun () -> observe (fun () -> run_driver "constructor_zero_head"))
  in
  if
    (match result with Error _ -> false | Ok _ -> true)
    || totals.transfers <> 1 || totals.consumptions <> 1
    || totals.results <> 0 || totals.teardown <> 1
    || totals.lowerings <> 1 || totals.backends <> 0 || totals.solvers <> 0
    || totals.preservation <> 0
  then fail "consumed transition predecessor reuse boundary changed";
  print_endline
    "consumed_reuse first=1/1/0/1 reuse=rejected dependent=1/0/0 preservation=0"

let two_valid_call_instances () =
  match run_driver "two_valid_call_instances_adversary" with
  | Error _ -> fail "two exact call instances did not verify"
  | Ok report ->
      let counters = Verification_driver_private.counters report in
      if
        Verification_driver_private.status report
        <> Verification_pipeline.Verified
        || counters.transition_predecessor_transfers <> 2
        || counters.transition_predecessor_consumptions <> 2
        || counters.transition_result_receipts <> 2
        || counters.transition_teardown_removals <> 2
        || counters.transition_preservation_obligations <> 1
        || counters.transition_nested_reconstructions <> 1
        || counters.transition_root_reconstructions <> 0
        || terminal_observations report <> 2
      then fail "two exact call instances lost per-call affine identity";
      print_endline
        "call_affinity exact-instances=2 lifecycle=2/2/2/2 preservation=1 reconstruction=1/0 terminal=2"

let matrix () =
  List.iter positive
    [
      "constructor_drop";
      "constructor_zero_head";
      "root_rebase";
      "nested_cut";
      "equal_branch";
      "terminal_snapshot";
    ];
  List.iter zero_work_negative
    [
      "arbitrary_entry";
      "copied_or_rebound";
      "ghost_forgetting";
      "mixed_call_instance_adversary";
      "trusted_transition";
    ];
  use_invariant_negative ();
  failing_preservation "invalid_one_write" 1;
  failing_preservation "temporarily_invalid_multi_write" 2;
  List.iter attack attack_names;
  two_valid_call_instances ();
  consumed_reuse ();
  session_replay ();
  print_endline
    "observer-injection=unavailable capability-source=previewed-production-call-only"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--matrix" ] -> matrix ()
  | _ -> fail "usage: recursive_invariant_transition_pipeline_tool.exe --matrix"
