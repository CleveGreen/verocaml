let () = ignore Proof_activation_routing_prerequisites.ready
(* Ordinary matches stay outside ground-witness routing. *)

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 2)
    format

let contains text fragment =
  let fragment_length = String.length fragment in
  let rec loop index =
    index + fragment_length <= String.length text
    &&
    (String.equal (String.sub text index fragment_length) fragment
    || loop (index + 1))
  in
  fragment_length = 0 || loop 0

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let run filename =
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load filename)
  with
  | Ok report -> report
  | Error (Verification_driver_private.Pipeline_error
      (Verification_pipeline.Solve_error message)) ->
      fail "solve-error: %s" message
  | Error _ -> fail "verification failed before producing a report"

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let observe filename callable =
  let report, events =
    Verification_session.For_testing.observe_proof_activations (fun () ->
        run filename)
  in
  Printf.printf "status=%s functions=%d obligations=%d\n"
    (status_name (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report);
  events
  |> List.filter (fun event ->
         contains event ("callable=" ^ callable ^ "#"))
  |> List.iter print_endline

let negative filename =
  let report = run filename in
  let status = Verification_driver_private.status report in
  let failed =
    Verification_driver_private.results report
    |> List.find_opt (fun result ->
           result.Solver_backend.outcome <> Solver_backend.Verified)
  in
  match failed with
  | None -> fail "negative fixture unexpectedly verified"
  | Some result ->
      Printf.printf "status=%s function=%s index=%d\n"
        (status_name status)
        result.obligation.Vir.function_ref.function_name
        result.obligation.obligation_index

let adversaries filename =
  let program = lower (load filename) in
  let prepared =
    match Recursive_spec_encoding.prepare program with
    | Ok prepared -> prepared
    | Error error ->
        fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let verified =
    match Recursive_spec_encoding.verify ~timeout_ms:5_000 prepared with
    | Ok verified -> verified
    | Error error ->
        fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let proof_definition =
    List.find
      (fun definition -> definition.Sst.mode = Sst.Proof)
      program.Sst.functions
  in
  let obligation =
    {
      Vir.obligation_index = 0;
      function_ref =
        {
          function_index = proof_definition.function_id.function_index;
          function_name = proof_definition.function_id.function_name;
        };
      kind = Vir.Assertion { assertion_ordinal = 0 };
      span = proof_definition.span;
      assumptions = [];
      required_preceding_safety = [];
      path_condition = [];
      goal = Vir.Boolean_constant true;
      projection_symbols = [];
    }
  in
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let dispatch () =
    let query =
      match
        Recursive_spec_encoding.For_testing.proof_obligation_query verified
          ~activations:[] obligation
      with
      | Ok query -> query
      | Error error ->
          fail "%s" (Recursive_spec_encoding.error_to_string error)
    in
    (match
       Z3_bridge.solve_query
         { Z3_bridge.timeout_ms = 5_000; model = false }
         query
     with
    | Ok _ -> ()
    | Error error -> fail "%s" (Z3_bridge.error_to_string error));
    let config =
      match Solver_backend.config ~timeout_ms:5_000 with
      | Ok config -> config
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
    in
    match Solver_backend.solve_obligation config obligation with
    | Ok _ -> ()
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let lines =
    Verification_session.For_testing.proof_activation_adversarial_matrix
      ~on_valid:dispatch
  in
  List.iter print_endline lines;
  let direct = Z3_bridge.counters () in
  Printf.printf "control-delta recursive-query=%d smt=%d direct-context=%d direct-solver=%d\n"
    (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
    (Solver_backend.For_testing.solver_creation_count ())
    direct.contexts_created direct.solvers_created

let ordinary_matches filename =
  Recursive_spec_encoding.For_testing.reset_ground_counterexample_counters ();
  Recursive_spec_encoding.For_testing.reset_nullary_branch_retry_counters ();
  Verification_session.For_testing
  .set_ground_constructor_match_attack_for_testing (Some "forged");
  let report =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_ground_constructor_match_attack_for_testing None)
      (fun () -> run filename)
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    Verification_driver_private.status report <> Verification_pipeline.Verified
    || ground.attempts <> 0 || retry.attempts <> 0
  then
    fail "ordinary match entered routing (ground=%d retry=%d)"
      ground.attempts retry.attempts;
  Printf.printf
    "ordinary-matches status=verified functions=%d obligations=%d \
     ground-attempts=0 retry-attempts=0 trusted-body-parity=1\n"
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)

let () =
  match Array.to_list Sys.argv with
  | [ _; "observe"; filename; callable ] -> observe filename callable
  | [ _; "negative"; filename ] -> negative filename
  | [ _; "adversaries"; filename ] -> adversaries filename
  | [ _; "ordinary-matches"; filename ] -> ordinary_matches filename
  | _ ->
      fail
        "usage: proof_activation_routing_tool.exe (observe FILE CALLABLE | \
         negative FILE | adversaries FILE | ordinary-matches FILE)"
