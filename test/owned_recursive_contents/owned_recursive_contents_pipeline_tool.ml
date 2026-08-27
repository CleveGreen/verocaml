let () = ignore Owned_recursive_contents_prerequisites.ready
(* This focused executable links the scoped private lifecycle test hooks. *)

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

let artifact name = Filename.concat "artifacts" (name ^ ".cmt")

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let zero_owned_contents =
  ({
     Verification_session.lineages_opened = 0;
     lineage_graph_authentications = 0;
     mapped_candidates_authenticated = 0;
     lineages_invalidated = 0;
     lineages_closed = 0;
     candidates_issued = 0;
     candidates_consumed = 0;
     candidates_rejected = 0;
     candidates_finished = 0;
     permits_issued = 0;
     permits_consumed = 0;
     permits_rejected = 0;
     recursive_routes = 0;
     ground_equations = 0;
     model_results = 0;
     manifests_issued = 0;
     completions = 0;
     receipts_finalized = 0;
     successor_receipts = 0;
     predecessor_retirements = 0;
     permit_retirements = 0;
     lineage_teardowns = 0;
     candidate_teardowns = 0;
     permit_teardowns = 0;
     receipt_teardowns = 0;
   }
    : Verification_session.owned_contents_counters)

let add_owned_contents
    (left : Verification_session.owned_contents_counters)
    (right : Verification_session.owned_contents_counters) =
  ({
     Verification_session.lineages_opened =
       left.Verification_session.lineages_opened + right.lineages_opened;
     lineage_graph_authentications =
       left.lineage_graph_authentications
       + right.lineage_graph_authentications;
     mapped_candidates_authenticated =
       left.mapped_candidates_authenticated
       + right.mapped_candidates_authenticated;
     lineages_invalidated =
       left.lineages_invalidated + right.lineages_invalidated;
     lineages_closed = left.lineages_closed + right.lineages_closed;
     candidates_issued =
       left.Verification_session.candidates_issued
       + right.Verification_session.candidates_issued;
     candidates_consumed =
       left.candidates_consumed + right.candidates_consumed;
     candidates_rejected =
       left.candidates_rejected + right.candidates_rejected;
     candidates_finished =
       left.candidates_finished + right.candidates_finished;
     permits_issued = left.permits_issued + right.permits_issued;
     permits_consumed = left.permits_consumed + right.permits_consumed;
     permits_rejected = left.permits_rejected + right.permits_rejected;
     recursive_routes = left.recursive_routes + right.recursive_routes;
     ground_equations = left.ground_equations + right.ground_equations;
     model_results = left.model_results + right.model_results;
     manifests_issued = left.manifests_issued + right.manifests_issued;
     completions = left.completions + right.completions;
     receipts_finalized =
       left.receipts_finalized + right.receipts_finalized;
     successor_receipts =
       left.successor_receipts + right.successor_receipts;
     predecessor_retirements =
       left.predecessor_retirements + right.predecessor_retirements;
     permit_retirements =
       left.permit_retirements + right.permit_retirements;
     lineage_teardowns =
       left.lineage_teardowns + right.lineage_teardowns;
     candidate_teardowns =
       left.candidate_teardowns + right.candidate_teardowns;
     permit_teardowns =
       left.permit_teardowns + right.permit_teardowns;
     receipt_teardowns =
       left.receipt_teardowns + right.receipt_teardowns;
   }
    : Verification_session.owned_contents_counters)

let total_owned_contents observations =
  List.fold_left add_owned_contents zero_owned_contents observations

let all_zero counters =
  counters.Verification_session.lineages_opened = 0
  && counters.lineage_graph_authentications = 0
  && counters.mapped_candidates_authenticated = 0
  && counters.lineages_invalidated = 0
  && counters.lineages_closed = 0
  && counters.candidates_issued = 0
  && counters.candidates_consumed = 0
  && counters.candidates_rejected = 0
  && counters.candidates_finished = 0
  && counters.permits_issued = 0
  && counters.permits_consumed = 0
  && counters.permits_rejected = 0
  && counters.recursive_routes = 0
  && counters.ground_equations = 0
  && counters.model_results = 0
  && counters.manifests_issued = 0
  && counters.completions = 0
  && counters.receipts_finalized = 0
  && counters.successor_receipts = 0
  && counters.predecessor_retirements = 0
  && counters.permit_retirements = 0
  && counters.lineage_teardowns = 0
  && counters.candidate_teardowns = 0
  && counters.permit_teardowns = 0
  && counters.receipt_teardowns = 0

let print_owned prefix counters =
  Printf.printf
    "%s lineages=%d/%d/%d/%d graphs=%d mapped=%d candidates=%d/%d/%d/%d permits=%d/%d/%d routes=%d equations=%d results=%d manifests=%d completions=%d receipts=%d successors=%d retirements=%d/%d teardown=%d/%d/%d\n"
    prefix counters.Verification_session.lineages_opened
    counters.lineages_closed counters.lineages_invalidated
    counters.lineage_teardowns counters.lineage_graph_authentications
    counters.mapped_candidates_authenticated
    counters.candidates_issued
    counters.candidates_consumed counters.candidates_rejected
    counters.candidates_finished counters.permits_issued
    counters.permits_consumed counters.permits_rejected
    counters.recursive_routes counters.ground_equations
    counters.model_results counters.manifests_issued counters.completions
    counters.receipts_finalized counters.successor_receipts
    counters.predecessor_retirements counters.permit_retirements
    counters.candidate_teardowns counters.permit_teardowns
    counters.receipt_teardowns

let reset_work_counters () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ()

let assert_no_backend row =
  let backend = Solver_backend.For_testing.solver_creation_count ()
  and z3 = Z3_bridge.counters ()
  and logic =
    Recursive_spec_encoding.For_testing.proof_query_construction_count ()
  in
  if
    backend <> 0 || z3.contexts_created <> 0 || z3.solvers_created <> 0
    || logic <> 0
  then
    fail "%s crossed zero-work backend=%d z3=%d/%d logic=%d" row backend
      z3.contexts_created z3.solvers_created logic

let positive name ~successors =
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        Verification_driver_private.run ~timeout_ms:5_000
          ~allow_imported_opens:false (load (artifact name)))
  in
  let report =
    match result with
    | Ok report -> report
    | Error _ -> fail "positive %s did not reach a driver report" name
  in
  let status = Verification_driver_private.status report in
  let counters = total_owned_contents observations in
  if
    status <> Verification_pipeline.Verified
    || List.length observations <> 1
    || counters.lineages_opened = 0
    || counters.lineages_closed <> counters.lineages_opened
    || counters.lineages_invalidated <> 0
    || counters.lineage_teardowns <> counters.lineages_opened
    || counters.lineage_graph_authentications
       <> counters.candidates_issued
    || counters.mapped_candidates_authenticated
       < counters.candidates_issued
    || counters.candidates_issued = 0
    || counters.candidates_consumed <> counters.candidates_issued
    || counters.candidates_finished <> counters.candidates_issued
    || counters.candidates_rejected <> 0
    || counters.permits_consumed <> counters.permits_issued
    || counters.permits_rejected <> 0
    || counters.recursive_routes <> counters.ground_equations
    || counters.model_results <> counters.candidates_issued
    || counters.manifests_issued <> counters.completions
    || counters.receipts_finalized = 0
    || counters.successor_receipts < successors
    || counters.predecessor_retirements < successors
    || counters.permit_retirements <> counters.permits_issued
    || counters.candidate_teardowns <> counters.candidates_issued
    || counters.permit_teardowns <> counters.permits_issued
    || counters.receipt_teardowns <> counters.receipts_finalized
  then (
    print_owned ("positive-failure=" ^ name) counters;
    fail "positive %s has unbalanced owned-contents lifecycle" name);
  print_owned
    (Printf.sprintf "positive=%s status=%s"
       name (status_name status))
    counters

let packaged_zero_work name =
  reset_work_counters ();
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        Verification_driver_private.run ~timeout_ms:1_000
          ~allow_imported_opens:false (load (artifact name)))
  in
  (match result with
  | Error _ -> ()
  | Ok _ -> fail "negative %s unexpectedly reached a report" name);
  let counters = total_owned_contents observations in
  if not (all_zero counters) then (
    print_owned ("negative-failure=" ^ name) counters;
    fail "negative %s crossed its owned zero-work boundary" name);
  assert_no_backend ("negative=" ^ name);
  Printf.printf
    "negative=%s rejected sessions=%d owned=0 backend=0 z3=0/0 logic=0\n"
    name (List.length observations)

let run_custom_pipeline implementation solve =
  let program = lower implementation in
  let validated = validate program in
  let invariants = invariants validated in
  Verification_pipeline.run_validated ~imports:None ~implementation ~program
    ~validated ~invariants ~preflight:(fun () -> Ok 0)
    ~proof_entry_activations:(fun () _ -> [])
    ~configure_solver:(fun () -> Ok solve) ~on_result:(fun _ -> ())

let pipeline_attack ?(graph = false) attack =
  let implementation = load (artifact "closed_baseline") in
  let configure_calls = ref 0 and solve_calls = ref 0 in
  reset_work_counters ();
  Verification_session.For_testing.set_owned_contents_attack_for_testing
    (Some attack);
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        Fun.protect
          ~finally:(fun () ->
            Verification_session.For_testing
            .set_owned_contents_attack_for_testing None)
          (fun () ->
            let program = lower implementation in
            let validated = validate program in
            let invariants = invariants validated in
            Verification_pipeline.run_validated ~imports:None
              ~implementation ~program ~validated ~invariants
              ~preflight:(fun () -> Ok 0)
              ~proof_entry_activations:(fun () _ -> [])
              ~configure_solver:(fun () ->
                incr configure_calls;
                Ok
                  (fun request ->
                    incr solve_calls;
                    Ok
                      (List.map
                         (fun obligation ->
                           {
                             Solver_backend.obligation;
                             outcome = Solver_backend.Verified;
                           })
                         request.Verification_pipeline.execution
                           .Vir.obligations)))
              ~on_result:(fun _ -> ())))
  in
  let report =
    match result with
    | Ok report -> report
    | Error message -> fail "attack %s session failed: %s" attack message
  in
  let rejected =
    match report.Verification_pipeline.outcome with
    | Error (Verification_pipeline.Engine_error _) -> true
    | Error
        (Verification_pipeline.Setup_error _
        | Verification_pipeline.Solve_error _)
    | Ok _ ->
        false
  in
  let counters = total_owned_contents observations in
  let generic = report.counters in
  let expected_candidates = if graph then 0 else 1 in
  let expected_lineages = if graph then 0 else 1 in
  if
    not rejected || List.length observations <> 1
    || counters.lineages_opened <> expected_lineages
    || counters.lineages_closed <> 0
    || counters.lineages_invalidated <> expected_lineages
    || counters.lineage_teardowns <> expected_lineages
    || counters.lineage_graph_authentications <> expected_candidates
    || counters.mapped_candidates_authenticated
       < expected_candidates
    || counters.candidates_issued <> expected_candidates
    || counters.candidates_consumed <> 0
    || counters.candidates_rejected <> 1
    || counters.candidates_finished <> 0
    || counters.permits_issued <> 0
    || counters.recursive_routes <> 0
    || counters.ground_equations <> 0
    || counters.model_results <> 0
    || counters.manifests_issued <> 0
    || counters.completions <> 0
    || counters.receipts_finalized <> 0
    || counters.permit_retirements <> 0
    || counters.candidate_teardowns <> expected_candidates
    || !solve_calls <> 0
    || generic.Verification_session.dependent_lowerings <> 0
    || generic.dependent_backend_contexts <> 0
    || generic.dependent_solver_attempts <> 0
    || not report.session_destroyed
  then (
    print_owned ("attack-failure=" ^ attack) counters;
    fail
      "attack %s crossed boundary rejected=%b configure=%d solve=%d destroyed=%b lineages=%d/%d/%d/%d graphs=%d mapped=%d candidates=%d/%d/%d/%d permits=%d/%d/%d"
      attack rejected !configure_calls !solve_calls report.session_destroyed
      counters.lineages_opened counters.lineages_closed
      counters.lineages_invalidated counters.lineage_teardowns
      counters.lineage_graph_authentications
      counters.mapped_candidates_authenticated counters.candidates_issued
      counters.candidates_consumed counters.candidates_rejected
      counters.candidates_finished counters.permits_issued
      counters.permits_consumed counters.permits_rejected);
  assert_no_backend ("attack=" ^ attack);
  Printf.printf
    "attack=%s rejected graph=%b lineages=%d/0/%d/%d candidates=%d/0/1/0 downstream=0 solver=0 session-destroyed=true\n"
    attack graph expected_lineages expected_lineages expected_lineages
    expected_candidates

let missing_predecessor () =
  reset_work_counters ();
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        Verification_driver_private.run ~timeout_ms:1_000
          ~allow_imported_opens:false
          (load (artifact "missing_predecessor")))
  in
  (match result with
  | Error (Verification_driver_private.Pipeline_error _) -> ()
  | Error _ -> fail "missing predecessor rejected at the wrong boundary"
  | Ok _ -> fail "missing predecessor unexpectedly verified");
  let counters = total_owned_contents observations in
  if
    counters.candidates_issued <> 0
    || counters.candidates_rejected <> 1
    || counters.candidates_consumed <> 0
    || counters.permits_issued <> 0
    || counters.recursive_routes <> 0
    || counters.ground_equations <> 0
    || counters.model_results <> 0
    || counters.manifests_issued <> 0
    || counters.receipts_finalized <> 0
  then (
    print_owned "missing-predecessor-failure" counters;
    fail "missing predecessor crossed its candidate-finalization boundary");
  assert_no_backend "missing-predecessor";
  print_endline
    "predecessor=missing rejected candidates=0/0/1/0 downstream=0 backend=0"

let forged_source () =
  reset_work_counters ();
  let implementation = load (artifact "closed_baseline") in
  let program = lower implementation in
  Sst_validation_private.Owned_recursive_contents_private.For_testing.set_attack
    (Some "forged-source");
  let rejected =
    Fun.protect
      ~finally:(fun () ->
        Sst_validation_private.Owned_recursive_contents_private.For_testing.set_attack None)
      (fun () ->
        match Sst_validation.validate program with
        | Error _ -> true
        | Ok _ -> false)
  in
  if not rejected then fail "forged retained source unexpectedly validated";
  assert_no_backend "forged-source";
  print_endline
    "grammar=forged-source rejected owned=0 backend=0 z3=0/0 logic=0"

let two_sessions () =
  let implementation = load (artifact "closed_baseline") in
  let results, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        [
          Verification_driver_private.run ~timeout_ms:5_000
            ~allow_imported_opens:false implementation;
          Verification_driver_private.run ~timeout_ms:5_000
            ~allow_imported_opens:false implementation;
        ])
  in
  if
    List.length observations <> 2
    || not
         (List.for_all
            (function
              | Ok report ->
                  Verification_driver_private.status report
                  = Verification_pipeline.Verified
              | Error _ -> false)
            results)
    || not
         (List.for_all
            (fun counters ->
              counters.Verification_session.receipts_finalized = 1
              && counters.receipt_teardowns = 1
              && counters.candidates_issued
                 = counters.candidate_teardowns)
            observations)
  then fail "two same-process sessions leaked or shared owned authority";
  print_endline
    "two-session=isolated sessions=2 receipts=1+1 teardown=1+1"

let finalized_seed_control () =
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_seed_control
      (fun () ->
        Verification_driver_private.run ~timeout_ms:5_000
          ~allow_imported_opens:false
          (load (artifact "lifecycle_positive")))
  in
  let verified =
    match result with
    | Ok report ->
        Verification_driver_private.status report
        = Verification_pipeline.Verified
    | Error _ -> false
  in
  match observations with
  | [
   "seed-control=production seed-path=1/1/3/1 failure-path=1/1/4/1 failure-authority=0/0/0/0 failure-dependent=0/0/0 seed-survived=live@0 success-path=1/1/4/1 success-authority=1/1/1/1 seed-retirement=exactly-once second-retirement=rejected replay=rejected"
    as row;
    ]
    when verified ->
      print_endline row
  | _ ->
      fail
        "finalized seed survival/one-time retirement control failed verified=%b rows=%d"
        verified (List.length observations)

type completion_failure =
  | Construction
  | Reconstruction
  | Recursive_model
  | Safety
  | Contract
  | Invariant_establishment
  | Invariant_preservation

let completion_name = function
  | Construction -> "construction"
  | Reconstruction -> "reconstruction"
  | Recursive_model -> "recursive-model"
  | Safety -> "safety"
  | Contract -> "contract"
  | Invariant_establishment -> "invariant-establishment"
  | Invariant_preservation -> "invariant-preservation"

let completion_fixture = function
  | Construction -> "completion_construction"
  | Reconstruction -> "completion_reconstruction"
  | Recursive_model -> "completion_model"
  | Safety -> "completion_safety"
  | Contract -> "completion_contract"
  | Invariant_establishment -> "invariant_establishment_failure"
  | Invariant_preservation -> "invariant_preservation_failure"

let completion_matches failure definition (obligation : Vir.obligation) =
  let name = definition.Sst.function_id.function_name in
  match (failure, name, obligation.kind) with
  | Construction, "Stack.construct", Vir.Postcondition _ -> true
  | Reconstruction, "Stack.reconstruct", Vir.Postcondition _ -> true
  | Recursive_model, "Stack.observe", Vir.Local_assertion _ -> true
  | Safety, "Stack.observe_then_add", Vir.Arithmetic_safety _ -> true
  | Contract, "Stack.contracted", Vir.Postcondition _ -> true
  | ( Invariant_establishment,
      "Stack.establish",
      Vir.Invariant_validity { boundary = Vir.Constructor_establishment; _ }
    ) ->
      true
  | ( Invariant_preservation,
      "Stack.preserve",
      Vir.Invariant_validity
        { boundary = Vir.Transition_preservation _; _ } ) ->
      true
  | _ -> false

let completion_failure failure =
  reset_work_counters ();
  let implementation = load (artifact (completion_fixture failure)) in
  let failed = ref 0 and solve_calls = ref 0 in
  let result, observations =
    Verification_session.For_testing.observe_owned_contents_counters
      (fun () ->
        run_custom_pipeline implementation
          (fun request ->
            incr solve_calls;
            Ok
              (List.map
                 (fun obligation ->
                   let selected =
                     !failed = 0
                     && completion_matches failure request.definition
                          obligation
                   in
                   if selected then incr failed;
                   {
                     Solver_backend.obligation;
                     outcome =
                       (if selected then Solver_backend.Counterexample []
                        else Solver_backend.Verified);
                   })
                 request.execution.Vir.obligations)))
  in
  let report =
    match result with
    | Ok report -> report
    | Error message ->
        fail "completion %s pipeline failed: %s"
          (completion_name failure) message
  in
  let counterexample =
    match report.Verification_pipeline.outcome with
    | Ok completion ->
        completion.Verification_pipeline.status
        = Verification_pipeline.Counterexample
    | Error _ -> false
  in
  let counters = total_owned_contents observations in
  let generic = report.counters in
  if
    !failed <> 1 || !solve_calls = 0 || not counterexample
    || counters.lineages_opened = 0
    || counters.lineages_closed <> 0
    || counters.lineages_invalidated <> counters.lineages_opened
    || counters.lineage_teardowns <> counters.lineages_opened
    || counters.lineage_graph_authentications
       <> counters.candidates_issued
    || counters.mapped_candidates_authenticated
       < counters.candidates_issued
    || counters.candidates_issued = 0
    || counters.candidates_consumed <> counters.candidates_issued
    || counters.candidates_finished <> counters.candidates_issued
    || counters.manifests_issued = 0
    || counters.completions <> 0
    || counters.receipts_finalized <> 0
    || counters.successor_receipts <> 0
    || counters.predecessor_retirements <> 0
    || counters.permit_retirements <> counters.permits_issued
    || counters.receipt_teardowns <> 0
    || generic.Verification_session.dependent_lowerings <> 0
    || generic.dependent_backend_contexts <> 0
    || generic.dependent_solver_attempts <> 0
  then (
    print_owned
      ("completion-failure=" ^ completion_name failure)
      counters;
    fail "completion %s finalized authority or missed its independent failure"
      (completion_name failure));
  Printf.printf
    "completion=%s failed=1 lineages=%d/0/%d/%d graphs=%d candidates=%d/%d/%d manifests=%d completions=0 receipts=0 successor=0 retirement=0/%d\n"
    (completion_name failure) counters.lineages_opened
    counters.lineages_invalidated counters.lineage_teardowns
    counters.lineage_graph_authentications counters.candidates_issued
    counters.candidates_consumed counters.candidates_finished
    counters.manifests_issued counters.permit_retirements

let attacks =
  [
    "copied";
    "replay";
    "wrong-session";
    "wrong-program";
    "wrong-cmt";
    "wrong-family";
    "wrong-model";
    "wrong-helper";
    "wrong-body";
    "wrong-signature";
    "wrong-root";
    "stale-root";
    "wrong-version";
    "wrong-path";
    "wrong-grammar";
    "wrong-edge";
    "wrong-result";
    "wrong-mapped-child";
    "missing-construction";
    "failed-transition";
    "ambiguous-successor";
    "stale-lineage-replay";
    "cross-function";
  ]

let graph_attacks =
  [ "cyclic"; "shared"; "detached"; "duplicate-root"; "cross-branch-union" ]

let completion_failures =
  [
    Construction;
    Reconstruction;
    Recursive_model;
    Safety;
    Contract;
    Invariant_establishment;
    Invariant_preservation;
  ]

let matrix () =
  positive "lifecycle_positive" ~successors:3;
  positive "invariant_positive" ~successors:2;
  List.iter packaged_zero_work
    [
      "grammar_omitted";
      "grammar_duplicated";
      "grammar_substituted";
      "grammar_nonexhaustive";
      "grammar_indirect";
      "grammar_mutable_result";
      "external_recursive_model";
      "trusted_recursive_model";
      "general_transition_state_scalar";
    ];
  forged_source ();
  missing_predecessor ();
  List.iter pipeline_attack attacks;
  List.iter (pipeline_attack ~graph:true) graph_attacks;
  two_sessions ();
  finalized_seed_control ();
  List.iter completion_failure completion_failures

let () =
  match Array.to_list Sys.argv with
  | [ _; "--matrix" ] -> matrix ()
  | _ ->
      fail
        "usage: owned_recursive_contents_pipeline_tool.exe --matrix"
