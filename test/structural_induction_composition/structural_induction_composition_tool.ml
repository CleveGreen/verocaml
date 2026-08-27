let () = ignore Structural_induction_composition_prerequisites.ready

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

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let invariants validated =
  match Type_invariant.authenticate validated with
  | Ok invariants -> invariants
  | Error error -> fail "%s" (Type_invariant.error_to_string error)

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let is_postcondition_failure result =
  match
    ( result.Solver_backend.obligation.Vir.kind,
      result.Solver_backend.outcome )
  with
  | Vir.Postcondition _,
    (Solver_backend.Counterexample _ | Solver_backend.Inconclusive _) ->
      true
  | _, _ -> false

let mutant kind filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let suppress_calls, suppress_summaries =
    match kind with
    | "call-removed" -> (true, false)
    | "summary-suppressed" -> (false, true)
    | _ -> fail "unknown mutant %s" kind
  in
  Symbolic_executor_private.For_testing
  .suppress_recursive_proof_calls_for_testing suppress_calls;
  Symbolic_executor_private.For_testing
  .suppress_recursive_proof_summaries_for_testing suppress_summaries;
  let result =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_recursive_proof_calls_for_testing false;
        Symbolic_executor_private.For_testing
        .suppress_recursive_proof_summaries_for_testing false)
      (fun () ->
        Verification_driver_private.run ~timeout_ms:5_000
          ~allow_imported_opens:false (load filename))
  in
  match result with
  | Error _ -> fail "%s mutant did not reach ordinary verification" kind
  | Ok report ->
      let status = Verification_driver_private.status report in
      let results = Verification_driver_private.results report in
      let counters = Verification_driver_private.counters report in
      let direct = Z3_bridge.counters () in
      if
        (status <> Verification_pipeline.Counterexample
        && status <> Verification_pipeline.Inconclusive)
        || not (List.exists is_postcondition_failure results)
        || direct.contexts_created = 0
      then fail "%s mutant did not fail its enclosing postcondition" kind;
      (match kind with
      | "call-removed"
        when
          counters.Verification_session.proof_call_visits <> 0
          || counters.proof_call_summaries <> 0 ->
          fail "call-removed mutant issued proof-call authority"
      | "summary-suppressed"
        when
          counters.proof_call_visits = 0
          || counters.proof_call_summaries <> 0 ->
          fail "summary-suppressed mutant counter mismatch"
      | _ -> ());
      Printf.printf
        "mutant=%s status=%s postcondition-failure=true visits=%d summaries=%d solver-contexts=%d results=%d\n"
        kind (status_name status) counters.proof_call_visits
        counters.proof_call_summaries direct.contexts_created
        (List.length results)

let logic_snapshot filename =
  let implementation = load filename in
  let program = lower implementation in
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
  let report =
    match
      Verification_driver_private.run ~timeout_ms:5_000
        ~allow_imported_opens:false implementation
    with
    | Ok report -> report
    | Error _ -> fail "logic snapshot production verification failed"
  in
  let execution =
    Verification_driver_private.vir report
    |> fun program ->
    List.find
      (fun execution -> execution.Vir.mode = Sst.Proof)
      program.Vir.functions
  in
  let obligation =
    List.find
      (fun obligation ->
        match obligation.Vir.kind with
        | Vir.Postcondition _ -> true
        | Vir.Arithmetic_safety _ | Vir.Assertion _
        | Vir.Local_assertion _
        | Vir.Call_precondition _ | Vir.Callback_precondition _
        | Vir.Invariant_validity _
        | Vir.Entry_measure_nonnegative _
        | Vir.Recursive_call_measure_nonnegative _
        | Vir.Recursive_call_strict_descent _ ->
            false)
      execution.obligations
  in
  let function_id =
    {
      Sst.function_index = execution.function_ref.function_index;
      function_name = execution.function_ref.function_name;
    }
  in
  let activations =
    Recursive_spec_encoding.proof_entry_activations verified function_id
  in
  let query =
    match
      Recursive_spec_encoding.For_testing.proof_obligation_query verified
        ~activations obligation
    with
    | Ok query -> query
    | Error error ->
        fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let qids =
    Logic_ir.View.axioms query |> List.map Logic_ir.View.axiom_qid
  in
  let count_suffix suffix =
    List.fold_left
      (fun count qid ->
        if String.ends_with ~suffix qid then count + 1 else count)
      0 qids
  in
  let rank_sorts =
    Logic_ir.View.declarations query
    |> List.filter (function
         | Logic_ir.View.Sort_declaration sort ->
             String.starts_with ~prefix:"RankType_"
               (Logic_ir.View.named_sort_name sort)
         | Logic_ir.View.Function_declaration _ -> false)
    |> List.length
  in
  Printf.printf
    "logic rank-sorts=%d axioms=%d fuel-body=%d public-link=%d rank-nonnegative=%d rank-child=%d activations=%d assertions=%d\n"
    rank_sorts (List.length qids) (count_suffix ".fuel-body")
    (count_suffix ".public-link")
    (List.fold_left
       (fun total qid ->
         if
           String.starts_with ~prefix:"verocaml.rank." qid
           && String.ends_with ~suffix:".nonnegative" qid
         then total + 1
         else total)
       0 qids)
    (List.fold_left
       (fun total qid ->
         if
           String.starts_with ~prefix:"verocaml.rank." qid
           && String.ends_with ~suffix:".child" qid
         then total + 1
         else total)
       0 qids)
    (List.length activations)
    (List.length (Logic_ir.View.assertions query))

let conditional_revisit filename =
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
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
               { Solver_backend.obligation; outcome = Solver_backend.Verified })
             request.execution.Vir.obligations))
  in
  let report =
    match
      Verification_pipeline.run_validated ~imports:None ~implementation
        ~program ~validated ~invariants ~preflight:(fun () -> Ok 0)
        ~proof_entry_activations:(fun () _ -> [])
        ~configure_solver ~on_result:(fun _ -> ())
    with
    | Ok report -> report
    | Error message -> fail "%s" message
  in
  let rejected =
    match report.outcome with
    | Error (Verification_pipeline.Engine_error error) ->
        Symbolic_executor_private.error_to_string error
        |> String.ends_with
             ~suffix:
               "malformed SST: recursive proof child was already visited on \
                this path at tree_conditional_revisit.ml:10:6-10:36"
    | Error (Setup_error _) | Error (Solve_error _) | Ok _ -> false
  in
  let counters = report.counters in
  let authority =
    Symbolic_executor_private.For_testing.authority_observation ()
  in
  let z3 = Z3_bridge.counters () in
  if
    not rejected || counters.Verification_session.proof_call_visits <> 1
    || counters.proof_call_summaries <> 1
    || authority.recursive_spec_lowerings <> 0
    || authority.recursive_proof_rank_lowerings <> 1
    || counters.dependent_lowerings <> 0
    || counters.dependent_backend_contexts <> 0
    || counters.dependent_solver_attempts <> 0 || !solve_calls <> 0
    || Solver_backend.For_testing.solver_creation_count () <> 0
    || z3.contexts_created <> 0 || z3.solvers_created <> 0
    || not report.session_destroyed
  then fail "conditional revisit crossed its pre-solver rejection boundary";
  Printf.printf
    "conditional-revisit=rejected visits=1 summaries=1 rank-lowerings=1 \
     recursive-spec-lowerings=0 dependent=0/0/0 backend=0 solver=0 \
     session-destroyed=true\n"

let () =
  match Array.to_list Sys.argv with
  | [ _; "mutant"; kind; filename ] -> mutant kind filename
  | [ _; "conditional-revisit"; filename ] -> conditional_revisit filename
  | [ _; "authority-matrix" ] ->
      List.iter print_endline
        (Finite_value_registry.For_testing.proof_visit_matrix ())
  | [ _; "logic-snapshot"; filename ] -> logic_snapshot filename
  | _ ->
      fail
        "usage: structural_induction_composition_tool \
         (mutant KIND|conditional-revisit|logic-snapshot) FILE"
