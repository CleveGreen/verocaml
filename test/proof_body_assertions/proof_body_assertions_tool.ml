let () = ignore Proof_body_assertions_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 2)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let vc_name = function
  | Vir.Local_assertion { local_assertion_ordinal } ->
      Printf.sprintf "local-assertion[%d]" local_assertion_ordinal
  | Vir.Assertion { assertion_ordinal } ->
      Printf.sprintf "assertion[%d]" assertion_ordinal
  | Vir.Postcondition { postcondition_ordinal; _ } ->
      Printf.sprintf "postcondition[%d]" postcondition_ordinal
  | Vir.Arithmetic_safety _ -> "arithmetic-safety"
  | Vir.Call_precondition { precondition_ordinal; _ } ->
      Printf.sprintf "call-precondition[%d]" precondition_ordinal
  | Vir.Callback_precondition _ -> "callback-precondition"
  | Vir.Invariant_validity _ -> "invariant-validity"
  | Vir.Entry_measure_nonnegative _ -> "entry-measure-nonnegative"
  | Vir.Recursive_call_measure_nonnegative _ ->
      "recursive-call-measure-nonnegative"
  | Vir.Recursive_call_strict_descent _ -> "recursive-call-strict-descent"

let reset_observations () =
  Typedtree_adapter_private.Public.Proof_region_capture_for_testing.reset ();
  Instance_mode.For_testing.reset_builtin_local_assertion_mode_observations ();
  Verification_session.For_testing.reset_local_assertion_instance_observation ();
  Verification_session.For_testing
  .set_local_assertion_instance_attack_for_testing None;
  Verification_session.For_testing
  .set_proof_activation_batch_attack_for_testing None;
  Symbolic_executor_private.For_testing
  .proof_activation_snapshot_attack_for_testing None;
  Symbolic_executor_private.For_testing
  .recursive_spec_application_identity_forgery_for_testing false;
  Symbolic_executor_private.For_testing
  .suppress_local_assertion_successor_sites_for_testing [];
  Symbolic_executor_private.For_testing
  .reset_local_assertion_export_observation ();
  Verification_session.For_testing
  .set_ground_constructor_match_attack_for_testing None;
  Verification_solver_private.For_testing
  .force_recursive_local_inconclusive false;
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Symbolic_executor_private.For_testing
  .reset_proof_call_spent_visit_observation ();
  Recursive_spec_encoding.For_testing
  .reset_ground_counterexample_counters ();
  Recursive_spec_encoding.For_testing
  .reset_nullary_branch_retry_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

type boundary_snapshot = {
  static_issuances : int;
  reached_sst_nodes : int;
  local_instances_issued : int;
  local_instances_consumed : int;
  local_exports : int;
  activation_routes : int;
  recursive_lowerings : int;
  recursive_queries : int;
  backend_solvers : int;
  z3_contexts : int;
  z3_solvers : int;
}

let zero_boundary_snapshot =
  {
    static_issuances = 0;
    reached_sst_nodes = 0;
    local_instances_issued = 0;
    local_instances_consumed = 0;
    local_exports = 0;
    activation_routes = 0;
    recursive_lowerings = 0;
    recursive_queries = 0;
    backend_solvers = 0;
    z3_contexts = 0;
    z3_solvers = 0;
  }

let boundary_snapshot () =
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let local_instances_issued, local_instances_consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let direct = Z3_bridge.counters () in
  {
    static_issuances = static.static_issuances;
    reached_sst_nodes = static.reached_sst_nodes;
    local_instances_issued;
    local_instances_consumed;
    local_exports =
      Symbolic_executor_private.For_testing.local_assertion_export_count ();
    activation_routes =
      Verification_session.For_testing
      .proof_activation_route_consumption_count ();
    recursive_lowerings =
      Recursive_spec_encoding.For_testing.recursive_lowering_count ();
    recursive_queries =
      Recursive_spec_encoding.For_testing.proof_query_construction_count ();
    backend_solvers =
      Solver_backend.For_testing.solver_creation_count ();
    z3_contexts = direct.contexts_created;
    z3_solvers = direct.solvers_created;
  }

let boundary_delta before after =
  {
    static_issuances = after.static_issuances - before.static_issuances;
    reached_sst_nodes = after.reached_sst_nodes - before.reached_sst_nodes;
    local_instances_issued =
      after.local_instances_issued - before.local_instances_issued;
    local_instances_consumed =
      after.local_instances_consumed - before.local_instances_consumed;
    local_exports = after.local_exports - before.local_exports;
    activation_routes = after.activation_routes - before.activation_routes;
    recursive_lowerings =
      after.recursive_lowerings - before.recursive_lowerings;
    recursive_queries =
      after.recursive_queries - before.recursive_queries;
    backend_solvers = after.backend_solvers - before.backend_solvers;
    z3_contexts = after.z3_contexts - before.z3_contexts;
    z3_solvers = after.z3_solvers - before.z3_solvers;
  }

let boundary_is_zero snapshot =
  snapshot.static_issuances = 0
  && snapshot.reached_sst_nodes = 0
  && snapshot.local_instances_issued = 0
  && snapshot.local_instances_consumed = 0
  && snapshot.local_exports = 0
  && snapshot.activation_routes = 0
  && snapshot.recursive_lowerings = 0
  && snapshot.recursive_queries = 0
  && snapshot.backend_solvers = 0
  && snapshot.z3_contexts = 0
  && snapshot.z3_solvers = 0

let print_boundary_evidence label baseline delta =
  Printf.printf
    "%s baseline=static:%d/sst:%d/local:%d/%d/export:%d/route:%d/recursive:%d/%d/backend:%d/z3:%d/%d \
     delta=static:%d/sst:%d/local:%d/%d/export:%d/route:%d/recursive:%d/%d/backend:%d/z3:%d/%d\n"
    label baseline.static_issuances baseline.reached_sst_nodes
    baseline.local_instances_issued baseline.local_instances_consumed
    baseline.local_exports baseline.activation_routes
    baseline.recursive_lowerings baseline.recursive_queries
    baseline.backend_solvers baseline.z3_contexts baseline.z3_solvers
    delta.static_issuances delta.reached_sst_nodes
    delta.local_instances_issued delta.local_instances_consumed
    delta.local_exports delta.activation_routes delta.recursive_lowerings
    delta.recursive_queries delta.backend_solvers delta.z3_contexts
    delta.z3_solvers

let run_result filename =
  Verification_driver_private.run ~timeout_ms:5_000
    ~allow_imported_opens:false (load filename)

let error_class = function
  | Verification_driver_private.Frontend_error diagnostic ->
      Printf.sprintf "frontend:%s" diagnostic.Diagnostic.code
  | Validation_error _ -> "validation"
  | Invariant_error _ -> "invariant"
  | Pipeline_error (Verification_pipeline.Engine_error error) ->
      "engine:" ^ Symbolic_executor_private.error_to_string error
  | Pipeline_error (Verification_pipeline.Solve_error message) ->
      "solve:" ^ message
  | Pipeline_error (Verification_pipeline.Setup_error _) -> "setup"
  | Internal_error message -> "internal:" ^ message

let run filename =
  match run_result filename with
  | Ok report -> report
  | Error error -> fail "verification-error=%s" (error_class error)

let print_local_obligations report =
  Verification_driver_private.results report
  |> List.filter_map (fun result ->
         match result.Solver_backend.obligation.kind with
         | Vir.Local_assertion _ -> Some result
         | _ -> None)
  |> List.iter (fun result ->
         let obligation = result.Solver_backend.obligation in
         let goal_in_assumptions =
           List.exists (( = ) obligation.goal) obligation.assumptions
         in
         Printf.printf
           "function=%s vc=%s index=%d assumptions=%d path=%d \
            goal-in-assumptions=%b outcome=%s\n"
           obligation.function_ref.function_name
           (vc_name obligation.kind)
           obligation.obligation_index
           (List.length obligation.assumptions)
           (List.length obligation.path_condition)
           goal_in_assumptions
           (match result.outcome with
           | Solver_backend.Verified -> "verified"
           | Counterexample _ -> "counterexample"
           | Inconclusive _ -> "unknown"))

let print_counters report =
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let counters = Verification_driver_private.counters report in
  Printf.printf
    "counters static=%d sst=%d reached-issued=%d reached-consumed=%d \
     proof-visits=%d proof-summaries=%d recursive-query=%d solver=%d\n"
    static.static_issuances static.reached_sst_nodes issued consumed
    counters.proof_call_visits counters.proof_call_summaries
    (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
    (Solver_backend.For_testing.solver_creation_count ())

let solve filename =
  reset_observations ();
  let report, activations =
    Verification_session.For_testing.observe_proof_activations (fun () ->
        run filename)
  in
  Printf.printf "status=%s functions=%d obligations=%d\n"
    (status_name (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report);
  print_local_obligations report;
  activations
  |> List.filter (fun line ->
         let marker = "kind=local-assertion:" in
         let marker_length = String.length marker in
         let rec contains index =
           index + marker_length <= String.length line
           &&
           (String.equal (String.sub line index marker_length) marker
           || contains (index + 1))
         in
         contains 0)
  |> List.iter (fun line -> Printf.printf "activation %s\n" line);
  print_counters report

let contains_text text fragment =
  let fragment_length = String.length fragment in
  let rec loop index =
    index + fragment_length <= String.length text
    &&
    (String.equal
       (String.sub text index fragment_length)
       fragment
    || loop (index + 1))
  in
  loop 0

let outcome_name = function
  | Solver_backend.Verified -> "verified"
  | Counterexample _ -> "counterexample"
  | Inconclusive _ -> "unknown"

let create_stack_results report =
  Verification_driver_private.results report
  |> List.filter (fun result ->
         String.equal
           result.Solver_backend.obligation.function_ref.function_name
           "create_stack_with")

let create_stack_local results =
  List.find (fun result ->
      match result.Solver_backend.obligation.kind with
      | Vir.Local_assertion { local_assertion_ordinal = 0 } -> true
      | _ -> false)
    results

let create_stack_call results =
  List.find (fun result ->
      match result.Solver_backend.obligation.kind with
      | Vir.Call_precondition
          {
            callee = { function_name = "push_front"; _ };
            precondition_ordinal = 1;
            _;
          } ->
          true
      | _ -> false)
    results

let region_observe filename =
  reset_observations ();
  let report, events =
    Verification_session.For_testing.observe_proof_activations (fun () ->
        run filename)
  in
  let results = create_stack_results report in
  let local = create_stack_local results in
  let later = create_stack_call results in
  let fact_exported =
    List.exists
      (( = ) local.obligation.goal)
      later.obligation.assumptions
  in
  let issued fragment =
    List.filter (fun event ->
        String.starts_with ~prefix:"issued " event
        && contains_text event "callable=create_stack_with#"
        && contains_text event fragment)
      events
    |> List.length
  in
  Printf.printf
    "create-stack local=%s later=%s fact-exported=%b local-routes=%d \
     later-routes=%d\n"
    (outcome_name local.outcome) (outcome_name later.outcome) fact_exported
    (issued "kind=local-assertion:0")
    (issued "kind=pre:");
  let local_event =
    List.find (fun event ->
        String.starts_with ~prefix:"issued " event
        && contains_text event "callable=create_stack_with#"
        && contains_text event "kind=local-assertion:0")
      events
  in
  Printf.printf "create-stack route %s\n" local_event;
  let live = boundary_snapshot () in
  if
    live.local_instances_issued = 0
    || live.local_instances_consumed = 0
    || live.local_exports = 0
    || live.activation_routes = 0
    || live.recursive_lowerings = 0
    || live.recursive_queries = 0
    || live.backend_solvers = 0
    || live.z3_contexts = 0
    || live.z3_solvers = 0
  then fail "create-stack boundary counters are not a live nonzero control";
  print_boundary_evidence "create-stack-live" zero_boundary_snapshot live;
  print_counters report

let region_suppressed filename =
  reset_observations ();
  Symbolic_executor_private.For_testing
  .suppress_local_assertion_successor_for_testing true;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_local_assertion_successor_for_testing false)
      (fun () -> run filename)
  in
  let later =
    create_stack_results report |> create_stack_call
  in
  Printf.printf "suppressed create-stack later=%s\n"
    (outcome_name later.outcome)

let region_mutants filename =
  reset_observations ();
  let report = run filename in
  let results = Verification_driver_private.results report in
  let failures function_name kind =
    results
    |> List.filter (fun result ->
           String.equal result.Solver_backend.obligation.function_ref.function_name
             function_name
           && kind result.obligation.kind
           && result.outcome <> Solver_backend.Verified)
  in
  let local = function Vir.Local_assertion _ -> true | _ -> false in
  let push_precondition = function
    | Vir.Call_precondition
        {
          callee = { function_name = "push_front"; _ };
          precondition_ordinal = 1;
          _;
        } ->
        true
    | _ -> false
  in
  List.iter
    (fun (name, kind) ->
      let failed = failures name kind in
      Printf.printf "mutant=%s failed=%b outcomes=%s\n" name
        (failed <> [])
        (failed
        |> List.map (fun (result : Solver_backend.obligation_result) ->
               outcome_name result.outcome)
        |> String.concat ","))
    [
      ("assertion_removed", push_precondition);
      ("reveal_only", push_precondition);
      ("reveal_after", local);
      ("wrong_predicate", local);
      ("sibling_reveal", local);
      ("later_block", local);
      ("one_branch_export", push_precondition);
    ]

let region_activation_attack attack filename =
  reset_observations ();
  let baseline = ref None in
  if String.equal attack "missing-snapshot" then
    Symbolic_executor_private.For_testing
    .proof_activation_snapshot_attack_for_testing (Some attack)
  else
    Verification_session.For_testing
    .set_proof_activation_batch_attack_for_testing (Some attack);
  let result, events =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .proof_activation_snapshot_attack_for_testing None;
        Verification_session.For_testing
        .set_proof_activation_batch_attack_for_testing None)
      (fun () ->
        Symbolic_executor_private.For_testing
        .observe_proof_activation_prefinalizer_for_testing
          (fun () -> baseline := Some (boundary_snapshot ()))
          (fun () ->
            Verification_session.For_testing.observe_proof_activations
              (fun () -> run_result filename)))
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let direct = Z3_bridge.counters () in
  let baseline =
    match !baseline with
    | Some baseline -> baseline
    | None -> fail "region activation attack did not reach its prefinalizer"
  in
  let delta = boundary_delta baseline (boundary_snapshot ()) in
  if not (boundary_is_zero delta) then
    fail "region activation attack %s crossed its observed boundary" attack;
  match result with
  | Ok _ -> fail "region activation attack %s unexpectedly verified" attack
  | Error error ->
      Printf.printf
        "region-attack=%s rejected=%s reached-issued=%d reached-consumed=%d \
         activation-events=%d recursive-query=%d solver=%d z3-context=%d \
         z3-solver=%d\n"
        attack (error_class error) issued consumed (List.length events)
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created;
      print_boundary_evidence ("region-attack-" ^ attack) baseline delta

let failed_local report =
  Verification_driver_private.results report
  |> List.find_opt (fun result ->
         match result.Solver_backend.obligation.kind with
         | Vir.Local_assertion _ ->
             result.outcome <> Solver_backend.Verified
         | _ -> false)

let negative filename =
  reset_observations ();
  let report = run filename in
  match failed_local report with
  | None -> fail "negative fixture did not fail a local assertion"
  | Some result ->
      let obligation = result.obligation in
      Printf.printf "status=%s function=%s vc=%s index=%d outcome=%s\n"
        (status_name (Verification_driver_private.status report))
        obligation.function_ref.function_name
        (vc_name obligation.kind)
        obligation.obligation_index
        (match result.outcome with
        | Solver_backend.Verified -> "verified"
        | Counterexample _ -> "counterexample"
        | Inconclusive _ -> "unknown");
      print_counters report

let ground_counter_line label =
  let counters =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  Printf.printf
    "%s ground-attempts=%d ground-complete=%d ground-abstentions=%d \
     antecedents-checked=%d\n"
    label counters.attempts counters.complete_violations counters.abstentions
    counters.antecedents_checked

let nullary_counter_line label =
  let counters =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  Printf.printf
    "%s nullary-attempts=%d nullary-queries=%d nullary-facts=%d \
     nullary-verified=%d nullary-counterexamples=%d nullary-inconclusives=%d \
     nullary-abstentions=%d\n"
    label counters.attempts counters.queries counters.facts counters.verified
    counters.counterexamples counters.inconclusives counters.abstentions

let with_forced_recursive_local_inconclusive action =
  Verification_solver_private.For_testing
  .force_recursive_local_inconclusive true;
  Fun.protect
    ~finally:(fun () ->
      Verification_solver_private.For_testing
      .force_recursive_local_inconclusive false)
    action

let local_result_named report function_name =
  Verification_driver_private.results report
  |> List.find_opt (fun result ->
         String.equal
           result.Solver_backend.obligation.function_ref.function_name
           function_name
         &&
         match result.obligation.kind with
         | Vir.Local_assertion _ -> true
         | _ -> false)

let reconstruction filename =
  reset_observations ();
  let report = run filename in
  let results = Verification_driver_private.results report in
  let verified =
    List.filter
      (fun result -> result.Solver_backend.outcome = Solver_backend.Verified)
      results
  in
  let count function_name kind =
    List.fold_left
      (fun count result ->
        let obligation = result.Solver_backend.obligation in
        if
          String.equal obligation.function_ref.function_name function_name
          && kind obligation.kind
          && result.outcome = Solver_backend.Verified
        then count + 1
        else count)
      0 results
  in
  let post = function Vir.Postcondition _ -> true | _ -> false in
  let local = function Vir.Local_assertion _ -> true | _ -> false in
  let branch = count "branch_reconstruction" post in
  let direct = count "direct_branch_assert" local in
  let assumed = count "assumed_branch_assert" local in
  if
    Verification_driver_private.status report <> Verification_pipeline.Verified
    || List.length results <> 5 || List.length verified <> 5
    || branch <> 2 || direct <> 1 || assumed <> 1
  then fail "immutable reconstruction five-VC matrix changed";
  Printf.printf
    "reconstruction status=verified functions=%d obligations=5 verified=5 \
     branch-postconditions=%d direct-assert=%d assume-then-assert=%d\n"
    (Verification_driver_private.functions report)
    branch direct assumed

let reconstruction_matrix filename =
  reset_observations ();
  let report = run filename in
  let outcome function_name =
    match local_result_named report function_name with
    | Some result -> outcome_name result.Solver_backend.outcome
    | None -> fail "%s produced no reconstruction assertion" function_name
  in
  let positives =
    [
      "allocation_without_ensures";
      "alias_positive";
      "guard_positive";
      "nested_positive";
      "generic_alias_positive";
      "complete_record_positive";
      "subset_record_positive";
    ]
  in
  let negatives =
    [
      "wrong_constructor";
      "wrong_payload";
      "wrong_nested_payload";
      "wrong_field";
    ]
  in
  List.iter
    (fun name ->
      if not (String.equal (outcome name) "verified") then
        fail "%s reconstruction positive did not verify" name)
    positives;
  List.iter
    (fun name ->
      if String.equal (outcome name) "verified" then
        fail "%s reconstruction negative unexpectedly verified" name)
    negatives;
  Printf.printf
    "reconstruction-matrix positives=7/7 \
     negatives=wrong-constructor:counterexample,wrong-payload:counterexample,\
     wrong-nested:counterexample,wrong-field:counterexample\n"

let reconstruction_cohabitation filename =
  reset_observations ();
  let report = run filename in
  let program =
    Verification_driver_private.validated report |> Sst_validation.program
  in
  let owned_type =
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        match definition.representation with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction evidence) ->
            Option.is_some evidence.owned_tree_prerequisite
        | Sst.Revealed
        | Sst.Abstract_with_evidence
            (Sst.Incomplete_abstraction_evidence _
            | Sst.Proposed_same_cmt_abstraction _) ->
            false)
      program.types
  in
  let owned_type =
    match owned_type with
    | Some definition -> definition
    | None -> fail "cohabitation fixture has no authenticated owned abstraction"
  in
  let owned_immutable =
    Finite_value_registry.Finite_domain.deeply_immutable_type program.types
      (Sst.Aggregate owned_type.type_id)
  in
  if owned_immutable then
    fail "owned cohabitation value entered immutable reconstruction";
  let has_recursive_spec =
    List.exists
      (fun (definition : Sst.function_definition) ->
        match definition.body with
        | Sst.Recursive_spec_definition _ -> true
        | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
        | Sst.External_specification _ | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            false)
      program.functions
  in
  if not has_recursive_spec then
    fail "cohabitation fixture lost its recursive Spec";
  let require_outcome function_name expected =
    match local_result_named report function_name with
    | None -> fail "%s produced no local assertion" function_name
    | Some result ->
        let actual = outcome_name result.Solver_backend.outcome in
        if not (String.equal actual expected) then
          fail "%s local assertion was %s, expected %s" function_name actual
            expected
  in
  require_outcome "unrelated_immutable_reconstruction" "verified";
  require_outcome "unrelated_construction_exactly_once" "verified";
  require_outcome "wrong_value" "counterexample";
  require_outcome "wrong_payload" "counterexample";
  let exact_facts function_name =
    match local_result_named report function_name with
    | None -> fail "%s produced no local assertion" function_name
    | Some result ->
        List.filter
          Immutable_aggregate_fact_relevance_private
          .is_exact_aggregate_construction_equality
          result.Solver_backend.obligation.assumptions
        |> List.length
  in
  let reconstruction_facts =
    exact_facts "unrelated_immutable_reconstruction"
  in
  let construction_facts =
    exact_facts "unrelated_construction_exactly_once"
  in
  if reconstruction_facts <> 2 then
    fail "cohabiting immutable match did not retain reconstruction and construction";
  if construction_facts <> 1 then
    fail "cohabiting immutable construction fact was not canonical";
  Printf.printf
    "reconstruction-cohabitation owned-immutable-eligible=false \
     recursive-spec=true unrelated-match=verified construction-facts=%d \
     reconstruction-facts=%d wrong-value=counterexample \
     wrong-payload=counterexample\n"
    construction_facts reconstruction_facts

let require_local_outcome report function_name expected =
  match local_result_named report function_name with
  | None -> fail "%s produced no local assertion" function_name
  | Some result ->
      let actual = outcome_name result.Solver_backend.outcome in
      if not (String.equal actual expected) then
        fail "%s local assertion was %s, expected %s" function_name actual
          expected

let nullary_specialization_evidence () =
  let query =
    match
      Recursive_spec_encoding.For_testing.last_nullary_branch_retry_query ()
    with
    | Some query -> query
    | None -> fail "nullary specialization did not retain its retry query"
  in
  let assertions = Logic_ir.View.assertions query in
  let fact =
    match List.rev assertions with
    | _negated_goal :: fact :: _ -> fact
    | [] | [ _ ] -> fail "nullary retry lacks its added assertion"
  in
  let application, constructor, recognizer, constructor_index =
    match Logic_ir.View.term_node fact with
    | Logic_ir.View.And [ result; tag ] -> (
        match
          (Logic_ir.View.term_node result, Logic_ir.View.term_node tag)
        with
        | ( Logic_ir.View.Equal (application, constructor),
            Logic_ir.View.Equal (observed_tag, exact_tag) ) -> (
            match
              ( Logic_ir.View.term_node observed_tag,
                Logic_ir.View.term_node exact_tag )
            with
            | ( Logic_ir.View.Ite (recognized, then_, else_),
                Logic_ir.View.Integer constructor_index ) -> (
                match
                  ( Logic_ir.View.term_node recognized,
                    Logic_ir.View.term_node then_,
                    Logic_ir.View.term_node else_ )
                with
                | ( Logic_ir.View.Apply
                      (recognizer, [ recognized_application ]),
                    Logic_ir.View.Integer then_index,
                    Logic_ir.View.Integer _ )
                  when recognized_application = application
                       && Z.equal then_index constructor_index ->
                    (application, constructor, recognizer, constructor_index)
                | _ ->
                    fail "nullary retry native recognizer changed shape")
            | _ -> fail "nullary retry tag consequence changed shape")
        | _ -> fail "nullary retry result consequence changed shape")
    | _ -> fail "nullary retry did not add one result/tag conjunction"
  in
  let callee, scalar, controlling_actual =
    match Logic_ir.View.term_node application with
    | Logic_ir.View.Apply
        (callee, [ scalar; controlling_actual ]) ->
        (callee, scalar, controlling_actual)
    | _ -> fail "nullary retry fact does not name the exact binary application"
  in
  let constructor =
    match Logic_ir.View.term_node constructor with
    | Logic_ir.View.Apply (constructor, []) -> constructor
    | _ -> fail "nullary retry result is not an exact nullary constructor"
  in
  let scalar_symbol =
    match Logic_ir.View.term_node scalar with
    | Logic_ir.View.Apply (symbol, [])
      when Logic_ir.View.function_range symbol = Logic_ir.Int ->
        symbol
    | Logic_ir.View.Integer _ ->
        fail "nullary retry grounded its symbolic integer actual"
    | _ -> fail "nullary retry scalar actual changed shape"
  in
  let callee_name = Logic_ir.View.function_name callee in
  let constructor_name = Logic_ir.View.function_name constructor in
  let recognizer_name = Logic_ir.View.function_name recognizer in
  if
    not (contains_text callee_name "spec_index_alt_imp")
    || not (contains_text constructor_name "_Empty")
    || not (contains_text recognizer_name "verocaml_is_")
    || not (contains_text recognizer_name "Empty")
    || not (Z.equal constructor_index Z.zero)
    || Logic_ir.term_sort controlling_actual
       = Logic_ir.Int
  then fail "nullary retry fact lost its exact callee/constructor/tag identity";
  let recursive_qids =
    Logic_ir.View.axioms query
    |> List.map Logic_ir.View.axiom_qid
  in
  if
    List.exists (fun qid -> contains_text qid "verocaml.datatype")
      recursive_qids
  then fail "nullary retry retained quantified datatype axioms";
  List.iter
    (fun marker ->
      if
        not
          (List.exists
             (fun qid -> contains_text qid marker)
             recursive_qids)
      then fail "nullary retry omitted axiom marker %s" marker)
    [ "fuel-invariance"; "fuel-body"; "public-link" ];
  if List.length (Logic_ir.View.datatypes query) <> 1 then
    fail "nullary retry lacks one native datatype declaration";
  let rendered =
    match
      Z3_bridge.render_query { timeout_ms = 5_000; model = false } query
    with
    | Ok (_, rendered) -> rendered
    | Error error ->
        fail "nullary retry render failed: %s"
          (Z3_bridge.error_to_string error)
  in
  List.iter
    (fun name ->
      if not (contains_text rendered name) then
        fail "rendered retry omitted %s" name)
    [
      callee_name;
      constructor_name;
      Logic_ir.View.function_name scalar_symbol;
    ];
  Printf.printf
    "retry-fact rendered=true quantifier-free=true exact-application=true \
     constructor=Empty recognizer=Empty native-datatype=true symbolic-int=true \
     scalar-literal=false axioms=A1/A2/A3\n"

let nullary_positive filename =
  reset_observations ();
  let report =
    with_forced_recursive_local_inconclusive (fun () -> run filename)
  in
  require_local_outcome report "lemma_index_empty" "verified";
  let counters =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  if
    Verification_driver_private.status report
       <> Verification_pipeline.Verified
    || counters.attempts <> 1
    || counters.queries <> 1
    || counters.facts <> 1
    || counters.verified <> 1
    || counters.counterexamples <> 0
    || counters.inconclusives <> 0
    || counters.abstentions <> 0
    || ground.attempts <> 0
  then fail "nullary positive counters or outcome changed";
  Printf.printf
    "nullary-positive status=verified primary=forced-inconclusive \
     local=verified\n";
  nullary_counter_line "nullary-positive";
  ground_counter_line "nullary-positive";
  nullary_specialization_evidence ()

let nullary_retry_inconclusive filename =
  reset_observations ();
  Recursive_spec_encoding.For_testing.force_nullary_branch_retry_unknown true;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Recursive_spec_encoding.For_testing
        .force_nullary_branch_retry_unknown false)
      (fun () ->
        with_forced_recursive_local_inconclusive (fun () -> run filename))
  in
  require_local_outcome report "lemma_index_empty" "unknown";
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  if
    retry.attempts <> 1
    || retry.queries <> 1
    || retry.facts <> 1
    || retry.verified <> 0
    || retry.counterexamples <> 0
    || retry.inconclusives <> 1
    || retry.abstentions <> 0
    || ground.attempts <> 1
    || ground.complete_violations <> 0
    || ground.abstentions <> 1
  then fail "nullary retry-inconclusive policy changed";
  Printf.printf
    "nullary-retry-inconclusive primary=unknown retry=unknown final=unknown\n";
  nullary_counter_line "nullary-retry-inconclusive";
  ground_counter_line "nullary-retry-inconclusive"

let nullary_retry_counterexample filename =
  reset_observations ();
  Recursive_spec_encoding.For_testing
  .force_nullary_branch_retry_counterexample true;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Recursive_spec_encoding.For_testing
        .force_nullary_branch_retry_counterexample false)
      (fun () ->
        with_forced_recursive_local_inconclusive (fun () -> run filename))
  in
  require_local_outcome report "lemma_index_empty" "unknown";
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  if
    retry.attempts <> 1
    || retry.queries <> 1
    || retry.facts <> 1
    || retry.verified <> 0
    || retry.counterexamples <> 1
    || retry.inconclusives <> 0
    || retry.abstentions <> 0
    || ground.attempts <> 1
    || ground.complete_violations <> 0
    || ground.abstentions <> 1
  then fail "nullary retry-counterexample policy changed";
  Printf.printf
    "nullary-retry-counterexample primary=unknown retry=counterexample \
     final=unknown\n";
  nullary_counter_line "nullary-retry-counterexample";
  ground_counter_line "nullary-retry-counterexample"

let nullary_negative filename =
  reset_observations ();
  let report =
    with_forced_recursive_local_inconclusive (fun () -> run filename)
  in
  require_local_outcome report "wrong_result" "unknown";
  require_local_outcome report "reachable_false" "counterexample";
  require_local_outcome report "symbolic_expected" "unknown";
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    retry.attempts <> 2
    || retry.queries <> 2
    || retry.facts <> 2
    || retry.verified <> 0
    || retry.counterexamples <> 0
    || retry.inconclusives <> 2
    || retry.abstentions <> 0
  then fail "nullary negative outcome matrix changed";
  Printf.printf
    "nullary-negative wrong-result=unknown reachable-false=counterexample \
     symbolic-expected=unknown verified=0\n";
  nullary_counter_line "nullary-negative";
  ground_counter_line "nullary-negative"

let nullary_no_reveal filename =
  reset_observations ();
  let report =
    with_forced_recursive_local_inconclusive (fun () -> run filename)
  in
  List.iter
    (fun name ->
      match local_result_named report name with
      | Some { Solver_backend.outcome = Solver_backend.Verified; _ } ->
          if
            not
              (String.equal name "sibling_reveal"
              || String.equal name "revealed_default")
          then
            fail "%s unexpectedly verified" name
      | Some _ -> ()
      | None -> fail "%s produced no local assertion" name)
    [
      "removed_reveal";
      "late_reveal";
      "sibling_reveal";
      "wrong_callee_reveal";
      "synthetic_only";
      "zero_fuel";
      "revealed_default";
    ];
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    retry.attempts <> 7
    || retry.queries <> 1
    || retry.facts <> 1
    || retry.verified <> 1
    || retry.counterexamples <> 0
    || retry.inconclusives <> 0
    || retry.abstentions <> 6
  then
    fail
      "default/absent/late/sibling/wrong/zero reveal retry matrix changed \
       (attempts=%d queries=%d facts=%d verified=%d counterexamples=%d \
       inconclusives=%d abstentions=%d)"
      retry.attempts retry.queries retry.facts retry.verified
      retry.counterexamples retry.inconclusives retry.abstentions;
  Printf.printf
    "nullary-no-reveal removed=0 late=0 sibling=0 wrong-callee=0 \
     synthetic-only=0 zero-fuel=0 revealed-default=1 retries=1\n";
  nullary_counter_line "nullary-no-reveal"

let nullary_synthetic_only filename =
  reset_observations ();
  Recursive_spec_encoding.For_testing
  .suppress_original_nullary_branch_activation true;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Recursive_spec_encoding.For_testing
        .suppress_original_nullary_branch_activation false)
      (fun () ->
        with_forced_recursive_local_inconclusive (fun () -> run filename))
  in
  require_local_outcome report "lemma_index_empty" "unknown";
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    retry.attempts <> 1
    || retry.queries <> 0
    || retry.facts <> 0
    || retry.verified <> 0
    || retry.abstentions <> 1
  then fail "synthetic reached activation authorized a nullary retry";
  Printf.printf
    "nullary-synthetic-only final=unknown attempts=1 retries=0 \
     abstentions=1\n"

let nullary_unsupported filename =
  reset_observations ();
  let imported_type =
    {
      Vir.aggregate_type_index = 777;
      aggregate_type_name = "imported-nullary-control";
      aggregate_type_arguments = [];
    }
  in
  let imported_identity =
    Imported_callable.For_testing.test_aggregate_application_identity
      ~logical_digest:"imported-nullary-control"
      ~call_snapshot:"imported-nullary-call"
      ~registration_snapshot:"imported-nullary-registration"
      ~invocation_ordinal:0 ~application_snapshot:"imported-nullary-application"
  in
  let imported_argument =
    Vir.Recursive_aggregate_argument
      {
        aggregate_type = imported_type;
        aggregate_desc =
          Vir.Aggregate_imported_model_application
            {
              callee =
                { function_index = 777; function_name = "imported_model" };
              callable_path = "Imported.model";
              callable_uid = "imported-nullary-uid";
              provider_unit = "Imported";
              provider_interface = "imported-nullary-interface";
              provider_source = "imported-nullary-source";
              provider_family = "imported-nullary-family";
              provider_import = "imported-nullary-import";
              summary_digest = "imported-nullary-summary";
              closure_digest = "imported-nullary-closure";
              call_snapshot = "imported-nullary-call";
              registration_snapshot = "imported-nullary-registration";
              invocation_ordinal = 0;
              application_identity = imported_identity;
              arguments = [];
              result_type = imported_type;
              span = Diagnostic.file_span "<imported-nullary-control>";
            };
      }
  in
  if
    Recursive_spec_encoding.For_testing
    .nullary_branch_argument_supported imported_argument
  then fail "imported model actual entered nullary specialization";
  let report =
    with_forced_recursive_local_inconclusive (fun () -> run filename)
  in
  List.iter
    (fun name ->
      match local_result_named report name with
      | Some { Solver_backend.outcome = Solver_backend.Verified; _ } ->
          fail "%s unsupported shape unexpectedly verified" name
      | Some _ -> ()
      | None -> fail "%s unsupported shape produced no local assertion" name)
    [
      "non_nullary_input";
      "payload_result";
      "record_result";
      "selector_actual";
      "nested_application";
      "guarded_branch";
      "multiple_applications";
      "scalar_recursive";
      "recursive_scalar_actual";
      "recursive_selected_branch";
    ];
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    retry.attempts <> 10
    || retry.queries <> 0
    || retry.facts <> 0
    || retry.verified <> 0
    || retry.counterexamples <> 0
    || retry.inconclusives <> 0
    || retry.abstentions <> 10
  then fail "unsupported nullary specialization matrix changed";
  Printf.printf
    "nullary-unsupported non-nullary=abstain payload=abstain record=abstain \
     selector=abstain imported-model=abstain nested=abstain guarded=abstain \
     multiple=abstain scalar-result=abstain scalar-recursive=abstain \
     recursive-branch=abstain\n";
  nullary_counter_line "nullary-unsupported"

let nullary_application_attack filename =
  reset_observations ();
  Symbolic_executor_private.For_testing
  .recursive_spec_application_identity_forgery_for_testing true;
  let result =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .recursive_spec_application_identity_forgery_for_testing false)
      (fun () -> run_result filename)
  in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let direct = Z3_bridge.counters () in
  (match result with
  | Ok _ -> fail "forged recursive application unexpectedly reached a report"
  | Error _ -> ());
  if
    retry.attempts <> 0
    || retry.queries <> 0
    || retry.facts <> 0
    || Solver_backend.For_testing.solver_creation_count () <> 0
  then fail "forged recursive application crossed the retry/backend boundary";
  Printf.printf
    "nullary-application-attack forged=rejected attempts=0 retries=0 \
     facts=0 backend=0 z3=%d/%d\n"
    direct.contexts_created direct.solvers_created

let nullary_route_attack attack filename =
  reset_observations ();
  Verification_session.For_testing
  .set_ground_constructor_match_attack_for_testing (Some attack);
  let result =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_ground_constructor_match_attack_for_testing None)
      (fun () -> run_result filename)
  in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let direct = Z3_bridge.counters () in
  (match result with
  | Ok _ -> fail "nullary route attack %s unexpectedly reached a report" attack
  | Error _ -> ());
  if
    retry.attempts <> 0
    || retry.queries <> 0
    || retry.facts <> 0
    || Solver_backend.For_testing.solver_creation_count () <> 0
  then fail "nullary route attack %s crossed retry/backend work" attack;
  Printf.printf
    "nullary-route-attack=%s rejected attempts=0 retries=0 facts=0 \
     backend=0 z3=%d/%d\n"
    attack direct.contexts_created direct.solvers_created

let nullary_local_attack attack filename =
  reset_observations ();
  Verification_session.For_testing
  .set_local_assertion_instance_attack_for_testing (Some attack);
  let result =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_local_assertion_instance_attack_for_testing None)
      (fun () -> run_result filename)
  in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let direct = Z3_bridge.counters () in
  (match result with
  | Ok _ -> fail "nullary local attack %s unexpectedly reached a report" attack
  | Error _ -> ());
  if
    retry.attempts <> 0
    || retry.queries <> 0
    || retry.facts <> 0
    || Solver_backend.For_testing.solver_creation_count () <> 0
  then fail "nullary local attack %s crossed retry/backend work" attack;
  Printf.printf
    "nullary-local-attack=%s rejected attempts=0 retries=0 facts=0 \
     backend=0 z3=%d/%d\n"
    attack direct.contexts_created direct.solvers_created

let nullary_primary_outcomes verified_filename counterexample_filename =
  reset_observations ();
  let verified_report = run verified_filename in
  let verified_retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    Verification_driver_private.status verified_report
       <> Verification_pipeline.Verified
    || verified_retry.attempts <> 0
  then fail "primary Verified outcome created a nullary retry";
  reset_observations ();
  let counterexample_report = run counterexample_filename in
  let counterexample_retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  if
    Verification_driver_private.status counterexample_report
       <> Verification_pipeline.Counterexample
    || counterexample_retry.attempts <> 0
  then fail "primary Counterexample outcome created a nullary retry";
  Printf.printf
    "nullary-primary verified-retries=0 counterexample-retries=0\n"

let ground_negative filename =
  reset_observations ();
  let report = run filename in
  match failed_local report with
  | Some
      {
        Solver_backend.obligation;
        outcome = Solver_backend.Inconclusive _;
      } ->
      Printf.printf
        "ordinary-nonretry function=%s vc=%s index=%d outcome=unknown\n"
        obligation.function_ref.function_name (vc_name obligation.kind)
        obligation.obligation_index;
      ground_counter_line "ordinary-nonretry"
  | Some { outcome = Solver_backend.Counterexample _; _ } ->
      fail "ordinary nonretry negative unexpectedly used a complete ground route"
  | Some { outcome = Solver_backend.Verified; _ } | None ->
      fail "ground negative did not fail its local assertion"

let ground_positive filename =
  reset_observations ();
  let report = run filename in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  if Verification_driver_private.status report <> Verification_pipeline.Verified
  then fail "ground positive did not verify"
  else if ground.complete_violations <> 0 || ground.attempts <> 0
  then fail "ground fallback attempted to issue verification"
  else (
    Printf.printf "ground-positive status=verified functions=%d obligations=%d\n"
      (Verification_driver_private.functions report)
      (Verification_driver_private.obligations report);
    ground_counter_line "ground-positive")

let region_proof_call filename =
  reset_observations ();
  let report = run filename in
  if
    Verification_driver_private.status report
    <> Verification_pipeline.Verified
  then fail "Exec-region proof-call fixture did not verify";
  let local =
    Verification_driver_private.results report
    |> List.find_opt (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             "region_proof_call"
           &&
           match result.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal = 1 } -> true
           | _ -> false)
  in
  let counters = Verification_driver_private.counters report in
  let spent =
    Symbolic_executor_private.For_testing.proof_call_spent_visit_count ()
  in
  let ledger_summaries =
    Symbolic_executor_private.For_testing.proof_call_ledger_summary_count
      ~caller:"prove_twice_len" ~callee:"prove_twice_len"
  in
  let region_exports =
    Symbolic_executor_private.For_testing
    .exec_region_proof_summary_export_count ~caller:"region_proof_call"
      ~callee:"prove_twice_len"
  in
  (match local with
  | Some { outcome = Solver_backend.Verified; _ } -> ()
  | Some _ | None -> fail "Exec-region proof-call summary did not exit");
  if
    counters.proof_call_visits = 0
    || counters.proof_call_visits <> spent
    || counters.proof_call_summaries <> spent
    || ledger_summaries <> spent
    || region_exports = 0
  then
    fail "Exec-region recursive Proof call lacked its visit/summary ledger";
  Printf.printf
    "region-proof-call status=verified local=verified callee-ledger=%d/%d/%d \
     region-summary-exports=%d\n"
    counters.proof_call_visits spent ledger_summaries region_exports

let region_proof_summary_suppressed filename =
  reset_observations ();
  Symbolic_executor_private.For_testing
  .suppress_proof_summary_for_testing
    (Some ("region_proof_call", "prove_twice_len"));
  let report =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_proof_summary_for_testing None)
      (fun () -> run filename)
  in
  let failed =
    Verification_driver_private.results report
    |> List.find_opt (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             "region_proof_call"
           &&
           match
             ( result.obligation.kind,
               result.Solver_backend.outcome )
           with
           | Vir.Local_assertion { local_assertion_ordinal = 1 },
             (Solver_backend.Counterexample _ | Solver_backend.Inconclusive _) ->
               true
           | _ -> false)
  in
  (match failed with
  | Some _ -> ()
  | None -> fail "suppressed Exec-region proof summary did not fail its consumer");
  let counters = Verification_driver_private.counters report in
  let region_exports =
    Symbolic_executor_private.For_testing
    .exec_region_proof_summary_export_count ~caller:"region_proof_call"
      ~callee:"prove_twice_len"
  in
  if region_exports <> 0 then
    fail "suppressed Exec-region proof summary was still exported";
  Printf.printf
    "region-proof-summary-suppressed local=failed visits=%d summaries=%d \
     region-summary-exports=%d\n"
    counters.proof_call_visits counters.proof_call_summaries region_exports

let ground_abstention ~force_inconclusive label filename =
  reset_observations ();
  Verification_solver_private.For_testing
  .force_recursive_local_inconclusive force_inconclusive;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Verification_solver_private.For_testing
        .force_recursive_local_inconclusive false)
      (fun () -> run filename)
  in
  (match failed_local report with
  | Some { outcome = Solver_backend.Counterexample _; _ } ->
      fail "%s produced a false ground counterexample" label
  | Some { obligation; outcome = Solver_backend.Inconclusive _ } ->
      Printf.printf "%s function=%s vc=%s index=%d outcome=unknown\n" label
        obligation.function_ref.function_name
        (vc_name obligation.kind) obligation.obligation_index
  | Some { outcome = Solver_backend.Verified; _ } | None ->
      Printf.printf "%s outcome=verified\n" label);
  ground_counter_line label

let ground_attack attack filename =
  reset_observations ();
  Verification_session.For_testing
  .set_ground_constructor_match_attack_for_testing (Some attack);
  let result =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_ground_constructor_match_attack_for_testing None)
      (fun () -> run_result filename)
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  let direct = Z3_bridge.counters () in
  match result with
  | Ok _ ->
      fail "ground authority attack %s unexpectedly reached a report" attack
  | Error error ->
      Printf.printf
        "ground-attack=%s rejected=%s ground-attempts=%d ground-complete=%d \
         ground-abstentions=%d recursive-query=%d solver=%d z3-context=%d \
         z3-solver=%d\n"
        attack (error_class error) ground.attempts
        ground.complete_violations ground.abstentions
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created

let summary_suppressed filename =
  reset_observations ();
  Symbolic_executor_private.For_testing
  .suppress_recursive_proof_summaries_for_testing true;
  let result =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_recursive_proof_summaries_for_testing false)
      (fun () -> run filename)
  in
  match failed_local result with
  | None -> fail "summary suppression did not fail a local assertion"
  | Some failed ->
      Printf.printf "suppressed-summary function=%s vc=%s index=%d outcome=%s\n"
        failed.obligation.function_ref.function_name
        (vc_name failed.obligation.kind)
        failed.obligation.obligation_index
        (match failed.outcome with
        | Solver_backend.Verified -> "verified"
        | Counterexample _ -> "counterexample"
        | Inconclusive _ -> "unknown");
      print_counters result

let attack attack filename =
  reset_observations ();
  Verification_session.For_testing
  .set_local_assertion_instance_attack_for_testing (Some attack);
  let result, activation_events =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_local_assertion_instance_attack_for_testing None)
      (fun () ->
        Verification_session.For_testing.observe_proof_activations (fun () ->
            run_result filename))
  in
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let direct = Z3_bridge.counters () in
  match result with
  | Ok _ -> fail "attack %s unexpectedly reached a verification report" attack
  | Error error ->
      Printf.printf
        "attack=%s rejected=%s static=%d sst=%d reached-issued=%d \
         reached-consumed=%d activation-events=%d recursive-query=%d solver=%d \
         z3-context=%d z3-solver=%d\n"
        attack (error_class error) static.static_issuances
        static.reached_sst_nodes issued consumed
        (List.length activation_events)
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created

let validation_mutation label mutate filename =
  reset_observations ();
  let baseline = ref None in
  let result =
    Sst_validation_private.Public.For_testing
    .with_program_mutation_at_validation_boundary ~mutate
      ~observe:(fun () -> baseline := Some (boundary_snapshot ()))
      (fun () -> run_result filename)
  in
  let baseline =
    match !baseline with
    | Some baseline -> baseline
    | None -> fail "%s did not reach the production validation boundary" label
  in
  let delta = boundary_delta baseline (boundary_snapshot ()) in
  if not (boundary_is_zero delta) then
    fail "%s crossed the production validation boundary" label;
  (match result with
  | Error (Verification_driver_private.Validation_error _) -> ()
  | Error error ->
      fail "%s rejected outside validation: %s" label (error_class error)
  | Ok _ -> fail "%s unexpectedly passed production validation" label);
  Printf.printf "%s=rejected boundary=validation\n" label;
  print_boundary_evidence label baseline delta

let static_copy filename =
  validation_mutation "context-copy"
    (fun (program : Sst.program) ->
      {
        program with
        Sst.functions = List.map Fun.id program.functions;
      })
    filename

let raw_sst filename =
  let mutate (program : Sst.program) =
    let copied_definition =
      match program.Sst.functions with
      | [ definition ] -> (
          match definition.body with
          | Sst.Proof_body { body; provenance } -> (
              match body.expression.expression_desc with
              | Sst.Let (bindings, assertion) -> (
                  match assertion.expression_desc with
                  | Sst.Local_assert local ->
                      let raw_assertion =
                        {
                          assertion with
                          Sst.expression_desc =
                            Sst.Local_assert
                              {
                                local with
                                predicate =
                                  { local.predicate with typ = Sst.Bool };
                              };
                        }
                      in
                      let raw_body =
                        {
                          body with
                          expression =
                            {
                              body.expression with
                              expression_desc =
                                Sst.Let (bindings, raw_assertion);
                            };
                        }
                      in
                      {
                        definition with
                        body = Sst.Proof_body { body = raw_body; provenance };
                      }
                  | _ -> fail "fixture body lacks a local assertion")
              | _ -> fail "fixture body lacks the expected pure let")
          | _ -> fail "fixture is not an authenticated Proof")
      | _ -> fail "raw-SST fixture must contain one callable"
    in
    { program with Sst.functions = [ copied_definition ] }
  in
  validation_mutation "raw-sst" mutate filename

let results_for report function_name =
  Verification_driver_private.results report
  |> List.filter (fun result ->
         String.equal
           result.Solver_backend.obligation.function_ref.function_name
           function_name)

let local_results ordinal results =
  List.filter
    (fun result ->
      match result.Solver_backend.obligation.kind with
      | Vir.Local_assertion { local_assertion_ordinal } ->
          local_assertion_ordinal = ordinal
      | _ -> false)
    results

let postcondition_results results =
  List.filter
    (fun result ->
      match result.Solver_backend.obligation.kind with
      | Vir.Postcondition _ -> true
      | _ -> false)
    results

let exact_fact_present fact result =
  List.exists (( = ) fact) result.Solver_backend.obligation.assumptions

let verified result =
  match result.Solver_backend.outcome with
  | Solver_backend.Verified -> true
  | Counterexample _ | Inconclusive _ -> false

let branch_postcondition_check label function_name report =
  let results = results_for report function_name in
  let locals = local_results 0 results in
  let postconditions = postcondition_results results in
  match locals with
  | [ local ] ->
      let fact = local.Solver_backend.obligation.goal in
      let carrying = List.filter (exact_fact_present fact) postconditions in
      let siblings =
        List.filter (fun result -> not (exact_fact_present fact result)) postconditions
      in
      if
        (not (verified local))
        || exact_fact_present fact local
        || List.length postconditions <> 2
        || not (List.for_all verified postconditions)
        || List.length carrying <> 1
        || List.length siblings <> 1
        || (List.hd carrying).Solver_backend.obligation.path_condition
           <> local.Solver_backend.obligation.path_condition
      then fail "%s branch-local postcondition shape changed" label;
      Printf.printf
        "%s local-own-goal=false postconditions=2 fact-present=1 fact-absent=1 exact-successor=true\n"
        label
  | _ -> fail "%s did not produce one local assertion" label

let proof_branch_facts filename =
  reset_observations ();
  let report = run filename in
  branch_postcondition_check "proof-if" "proof_if_branch_fact" report;
  branch_postcondition_check "proof-match" "proof_match_branch_fact" report;
  let both = results_for report "proof_both_branch_facts" in
  let predecessors = local_results 0 both @ local_results 1 both in
  let continuations = local_results 2 both in
  let predecessor_fact =
    match predecessors with
    | [ left; right ]
      when left.Solver_backend.obligation.goal
           = right.Solver_backend.obligation.goal ->
        left.Solver_backend.obligation.goal
    | _ -> fail "both-branch Proof predecessors changed"
  in
  if
    List.length continuations <> 2
    || not (List.for_all verified predecessors)
    || List.exists (exact_fact_present predecessor_fact) predecessors
    || not (List.for_all verified continuations)
    || not (List.for_all (exact_fact_present predecessor_fact) continuations)
  then fail "both-branch Proof facts did not remain on both concrete paths";
  Printf.printf
    "proof-both predecessor-vcs=2 continuation-vcs=2 predecessor-own-goals=false fact-present=2\n";
  let inner = results_for report "exec_region_inner_branch" in
  let inner_local =
    match local_results 0 inner with
    | [ local ] -> local
    | _ -> fail "inner Exec proof region local assertion changed"
  in
  let call_preconditions =
    List.filter
      (fun result ->
        match result.Solver_backend.obligation.kind with
        | Vir.Call_precondition
            {
              callee = { function_name = "require_true"; _ };
              precondition_ordinal = 0;
              _;
            } ->
            true
        | _ -> false)
      inner
  in
  let call_outcomes =
    List.map (fun result -> outcome_name result.Solver_backend.outcome) call_preconditions
    |> List.sort String.compare
  in
  if
    (not (verified inner_local))
    || exact_fact_present inner_local.obligation.goal inner_local
    || List.length call_preconditions <> 2
    || List.exists
         (exact_fact_present inner_local.Solver_backend.obligation.goal)
         call_preconditions
    || call_outcomes <> [ "counterexample"; "verified" ]
    || Verification_driver_private.status report
       <> Verification_pipeline.Counterexample
  then fail "inner logical Exec branch bypassed intersection";
  Printf.printf
    "exec-inner current-mode=exec logical=true call-vcs=2 fact-present=0 outcomes=%s\n"
    (String.concat "," call_outcomes);
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let exports =
    Symbolic_executor_private.For_testing.local_assertion_export_count ()
  in
  let routes =
    Verification_session.For_testing.proof_activation_route_consumption_count ()
  in
  if
    Verification_driver_private.functions report <> 5
    || Verification_driver_private.obligations report <> 13
    || static.static_issuances <> 6
    || static.reached_sst_nodes <> 6
    || issued <> 7
    || consumed <> 7
    || exports <> 7
    || routes <> 11
  then
    fail
      "branch fact counts functions=%d obligations=%d static=%d sst=%d local=%d/%d exports=%d routes=%d"
      (Verification_driver_private.functions report)
      (Verification_driver_private.obligations report) static.static_issuances
      static.reached_sst_nodes issued consumed exports routes;
  Printf.printf
    "authority functions=5 obligations=13 static=6 sst=6 local=7/7 exports=7 routes=11\n"

let predecessor_check suppression filename =
  reset_observations ();
  let suppressed_ordinals, suppression_name =
    match suppression with
    | `None -> ([], "none")
    | `Left -> ([ 0 ], "left")
    | `Right -> ([ 1 ], "right")
    | `All -> ([ 0; 1 ], "all")
  in
  Symbolic_executor_private.For_testing
  .suppress_local_assertion_successor_sites_for_testing
    (List.map
       (fun ordinal -> ("both_predecessors", ordinal))
       suppressed_ordinals);
  let report, activation_events =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_local_assertion_successor_sites_for_testing [])
      (fun () ->
        Verification_session.For_testing.observe_proof_activations (fun () ->
            run filename))
  in
  let results =
    Verification_driver_private.results report
    |> List.filter (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             "both_predecessors")
  in
  let predecessors =
    results
    |> List.filter (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion
               { local_assertion_ordinal = (0 | 1) } ->
               true
           | _ -> false)
  in
  if
    List.length predecessors <> 2
    || not
         (List.for_all
            (fun result ->
              match result.Solver_backend.outcome with
              | Solver_backend.Verified -> true
              | Counterexample _ | Inconclusive _ -> false)
            predecessors)
  then fail "both-predecessor assertions did not both verify";
  let predecessor_goals =
    List.map
      (fun result -> result.Solver_backend.obligation.goal)
      predecessors
  in
  let continuations =
    results
    |> List.filter (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal = 2 } -> true
           | _ -> false)
  in
  let continuation_activation_events =
    List.filter
      (fun event ->
        contains_text event "issued "
        && contains_text event "callable=both_predecessors#"
        && contains_text event "kind=local-assertion:2")
      activation_events
  in
  let expected_checked_continuations =
    match suppression with
    | `None -> 2
    | `Left | `Right | `All -> 1
  in
  if
    List.length continuations <> expected_checked_continuations
    || List.length continuation_activation_events <> 2
    || not
         (List.for_all
            (fun event -> contains_text event "activations=[]")
            continuation_activation_events)
  then fail "both-predecessor continuation shape or activation erasure changed";
  Printf.printf
    "suppressed=%s continuation-vcs=2 checked-states=%d \
     continuation-activation-leaks=0\n"
    suppression_name (List.length continuations);
  List.iteri
    (fun index result ->
      let predecessor_fact_present =
        List.exists
          (fun fact ->
            List.exists (( = ) fact) result.Solver_backend.obligation.assumptions)
          predecessor_goals
      in
      let outcome =
        match result.outcome with
        | Solver_backend.Verified -> "verified"
        | Counterexample _ -> "counterexample"
        | Inconclusive _ -> "unknown"
      in
      let expected =
        match suppression with
        | `None -> predecessor_fact_present && String.equal outcome "verified"
        | `Left | `Right | `All ->
            (not predecessor_fact_present)
            && String.equal outcome "counterexample"
      in
      if
        not expected
        || List.exists
             (( = ) result.obligation.goal)
             result.obligation.assumptions
      then fail "both-predecessor continuation was vacuous";
      Printf.printf
        "continuation=%d predecessor-fact-present=%b \
         own-goal-in-assumptions=false outcome=%s\n"
        index predecessor_fact_present outcome)
    continuations

let one_predecessor_check filename =
  reset_observations ();
  let report = run filename in
  let results =
    Verification_driver_private.results report
    |> List.filter (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             "one_predecessor")
  in
  let predecessor_goal =
    results
    |> List.find_map (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal = 0 } ->
               Some result.obligation.goal
           | _ -> None)
    |> Option.get
  in
  let continuations =
    results
    |> List.filter (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal = 1 } -> true
           | _ -> false)
  in
  let observed =
    List.map
      (fun result ->
        ( exact_fact_present predecessor_goal result,
          outcome_name result.Solver_backend.outcome ))
      continuations
  in
  if
    observed <> [ (true, "verified"); (false, "counterexample") ]
    || List.exists
         (fun result -> exact_fact_present result.Solver_backend.obligation.goal result)
         continuations
    || Verification_driver_private.status report
       <> Verification_pipeline.Counterexample
  then fail "one-predecessor branch-local negative shape changed";
  Printf.printf "one-predecessor continuation-states=%d\n"
    (List.length continuations);
  List.iteri
    (fun index (fact_present, outcome) ->
      Printf.printf "continuation=%d predecessor-fact-present=%b outcome=%s\n"
        index fact_present outcome)
    observed

let reject filename =
  reset_observations ();
  match run_result filename with
  | Ok _ -> fail "malformed fixture unexpectedly verified"
  | Error error ->
      let static =
        Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
      in
      let issued, consumed =
        Verification_session.For_testing.local_assertion_instance_observation ()
      in
      let direct = Z3_bridge.counters () in
      Printf.printf
        "rejected=%s static=%d sst=%d reached-issued=%d reached-consumed=%d \
         recursive-query=%d solver=%d z3-context=%d z3-solver=%d\n"
        (error_class error) static.static_issuances static.reached_sst_nodes
        issued consumed
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created

let instance_mode_name = function
  | Sst.Exec_instance -> "Exec"
  | Sst.Tracked_instance -> "Tracked"
  | Sst.Ghost_instance -> "Ghost"

let builtin_matrix filename =
  reset_observations ();
  let report = run filename in
  let local_results =
    Verification_driver_private.results report
    |> List.filter_map (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal } ->
               Some
                 ( result.obligation.function_ref.function_name,
                   local_assertion_ordinal,
                   result )
           | _ -> None)
  in
  let callable_names =
    local_results |> List.map (fun (name, _, _) -> name)
    |> List.sort_uniq String.compare
  in
  Printf.printf "builtin-matrix status=%s functions=%d local-vcs=%d\n"
    (status_name (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (List.length local_results);
  List.iter
    (fun name ->
      let ordinals =
        local_results
        |> List.filter_map (fun (candidate, ordinal, _) ->
               if String.equal candidate name then Some ordinal else None)
        |> List.sort_uniq Int.compare
        |> List.map string_of_int |> String.concat ","
      in
      Printf.printf "ordinals function=%s sequence=%s\n" name ordinals)
    callable_names;
  Instance_mode.For_testing.builtin_local_assertion_mode_observations ()
  |> List.sort (fun (left, left_ordinal, _, _) (right, right_ordinal, _, _) ->
         let by_function =
           Int.compare left.Sst.function_index right.Sst.function_index
         in
         if by_function <> 0 then by_function
         else Int.compare left_ordinal right_ordinal)
  |> List.iter (fun (function_id, ordinal, statement_mode, predicate_mode) ->
         Printf.printf "mode function=%s ordinal=%d statement=%s predicate=%s\n"
           function_id.Sst.function_name ordinal
           (instance_mode_name statement_mode)
           (instance_mode_name predicate_mode));
  let local_goal =
    local_results
    |> List.find_map (fun (name, ordinal, result) ->
           if String.equal name "exec_fact_export" && ordinal = 0 then
             Some result.Solver_backend.obligation.goal
           else None)
    |> Option.get
  in
  let later =
    Verification_driver_private.results report
    |> List.find (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             "exec_fact_export"
           &&
           match result.obligation.kind with
           | Vir.Call_precondition
               {
                 callee = { function_name = "require_equal"; _ };
                 _;
               } ->
               true
           | _ -> false)
  in
  Printf.printf "normal-successor fact-exported=%b later=%s\n"
    (List.exists (( = ) local_goal) later.obligation.assumptions)
    (outcome_name later.outcome);
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  Printf.printf "builtin-authority static=%d sst=%d local=%d/%d exports=%d\n"
    static.static_issuances static.reached_sst_nodes issued consumed
    (Symbolic_executor_private.For_testing.local_assertion_export_count ());
  let scopes_issued, scopes_closed, scopes_active =
    Verification_session.For_testing
    .direct_exec_local_assertion_scope_observation ()
  in
  Printf.printf "direct-scopes issued=%d closed=%d active=%d\n"
    scopes_issued scopes_closed scopes_active

let builtin_false filename =
  reset_observations ();
  let report = run filename in
  let failures =
    Verification_driver_private.results report
    |> List.filter_map (fun result ->
           match result.Solver_backend.obligation.kind with
           | Vir.Local_assertion { local_assertion_ordinal } ->
               Some
                 ( result.obligation.function_ref.function_name,
                   local_assertion_ordinal,
                   outcome_name result.outcome )
           | _ -> None)
  in
  Printf.printf "builtin-false status=%s local-vcs=%d outcomes=%s\n"
    (status_name (Verification_driver_private.status report))
    (List.length failures)
    (failures
    |> List.map (fun (name, ordinal, outcome) ->
           Printf.sprintf "%s[%d]:%s" name ordinal outcome)
    |> String.concat ",")

let builtin_scope_attack attack filename =
  reset_observations ();
  Verification_session.For_testing
  .set_local_assertion_instance_attack_for_testing (Some attack);
  let result =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_local_assertion_instance_attack_for_testing None)
      (fun () -> run_result filename)
  in
  let static =
    Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
  in
  let issued, consumed =
    Verification_session.For_testing.local_assertion_instance_observation ()
  in
  let scopes_issued, scopes_closed, scopes_active =
    Verification_session.For_testing
    .direct_exec_local_assertion_scope_observation ()
  in
  let direct = Z3_bridge.counters () in
  (match result with
  | Ok _ -> fail "builtin scope attack %s unexpectedly verified" attack
  | Error error ->
      Printf.printf
        "builtin-scope-attack=%s rejected=%s static=%d sst=%d local=%d/%d \
         scopes=%d/%d/active:%d recursive-query=%d solver=%d z3=%d/%d\n"
        attack (error_class error) static.static_issuances
        static.reached_sst_nodes issued consumed scopes_issued scopes_closed
        scopes_active
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created)

let incomplete_ppx_reject filename =
  reset_observations ();
  let diagnostic =
    match Cmt_input.load filename with
    | Error diagnostic -> diagnostic
    | Ok _ -> fail "PPX-negative artifact is a complete retained CMT"
  in
  (match diagnostic.Diagnostic.classification with
  | Diagnostic.Unsupported_input Diagnostic.Partial_implementation
  | Diagnostic.Malformed_input ->
      ()
  | _ ->
      fail "PPX-negative artifact was not classified as incomplete: %s"
        diagnostic.code);
  let after = boundary_snapshot () in
  if not (boundary_is_zero (boundary_delta zero_boundary_snapshot after)) then
    fail "incomplete PPX artifact crossed the loader boundary";
  Printf.printf
    "incomplete-cmt=rejected loader=%s static=%d sst=%d local=%d/%d \
     export=%d route=%d recursive=%d/%d backend=%d z3=%d/%d\n"
    diagnostic.code after.static_issuances after.reached_sst_nodes
    after.local_instances_issued after.local_instances_consumed
    after.local_exports after.activation_routes after.recursive_lowerings
    after.recursive_queries after.backend_solvers after.z3_contexts
    after.z3_solvers

let import_reject consumer dependency =
  reset_observations ();
  match
    Interface_specification.authenticate ~timeout_ms:5_000
      ~dependency_files:[ dependency ] ~consumer_file:consumer
  with
  | Ok _ -> fail "malformed imported local assertion unexpectedly authenticated"
  | Error _ ->
      let static =
        Typedtree_adapter_private.Public.Local_assertion_for_testing.counters ()
      in
      let issued, consumed =
        Verification_session.For_testing.local_assertion_instance_observation ()
      in
      let direct = Z3_bridge.counters () in
      Printf.printf
        "import=rejected static=%d sst=%d reached-issued=%d \
         reached-consumed=%d recursive-query=%d solver=%d z3-context=%d \
         z3-solver=%d\n"
        static.static_issuances static.reached_sst_nodes issued consumed
        (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
        (Solver_backend.For_testing.solver_creation_count ())
        direct.contexts_created direct.solvers_created

let nonrecursive_spec_path_product filename =
  let report = run filename in
  let vir = Verification_driver_private.vir report in
  let contains text needle =
    let text_length = String.length text in
    let needle_length = String.length needle in
    let rec search index =
      index + needle_length <= text_length
      &&
      (String.sub text index needle_length = needle
      || search (index + 1))
    in
    needle_length = 0 || search 0
  in
  let target name =
    match
      List.find_opt
        (fun execution ->
          String.equal execution.Vir.function_ref.function_name name)
        vir.Vir.functions
    with
    | Some execution -> execution
    | None -> fail "missing path-product function %s" name
  in
  let print name =
    let execution = target name in
    let headers =
      execution.obligations
      |> List.map (fun (obligation : Vir.obligation) ->
             vc_name obligation.Vir.kind)
      |> String.concat ","
    in
    let internal_path_entries =
      execution.obligations
      |> List.concat_map (fun (obligation : Vir.obligation) ->
             obligation.Vir.path_condition)
      |> List.filter (fun condition ->
             let rendered = Vir.boolean_term_to_string condition in
             contains rendered "spec_opt_eq"
             || contains rendered "spec_index ")
      |> List.length
    in
    Printf.printf
      "path-product function=%s obligations=%d headers=%s internal-spec-paths=%d\n"
      name (List.length execution.obligations) headers internal_path_entries
  in
  print "lemma_opt_eq_none_trans";
  print "lemma_index_node_step_none";
  let print_fallback name =
    let execution = target name in
    let path_entries =
      execution.obligations
      |> List.concat_map (fun (obligation : Vir.obligation) ->
             obligation.Vir.path_condition)
      |> List.length
    in
    let recursive_goals =
      execution.obligations
      |> List.filter (fun (obligation : Vir.obligation) ->
             contains
               (Vir.boolean_term_to_string obligation.Vir.goal)
               "spec_index_source")
      |> List.length
    in
    Printf.printf
      "branch-fallback function=%s obligations=%d exits=%d path-entries=%d \
       recursive-goals=%d\n"
      name (List.length execution.obligations) (List.length execution.exits)
      path_entries recursive_goals
  in
  print_fallback "helper_hidden_if_falls_back";
  print_fallback "mixed_cache_match_falls_back";
  let counters = Verification_driver_private.counters report in
  Printf.printf
    "branch-fallback authority recursive-spec=%d/%d finite-result=%d/%d/%d\n"
    counters.Verification_session.recursive_spec_result_issuances
    counters.recursive_spec_result_consumptions
    counters.finite_result_manifests counters.finite_result_completions
    counters.finite_result_finalizations

let () =
  match Array.to_list Sys.argv with
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "reconstruction"; filename ] -> reconstruction filename
  | [ _; "reconstruction-matrix"; filename ] ->
      reconstruction_matrix filename
  | [ _; "reconstruction-cohabitation"; filename ] ->
      reconstruction_cohabitation filename
  | [ _; "region-observe"; filename ] -> region_observe filename
  | [ _; "region-suppressed"; filename ] -> region_suppressed filename
  | [ _; "region-mutants"; filename ] -> region_mutants filename
  | [ _; "region-attack"; attack_name; filename ] ->
      region_activation_attack attack_name filename
  | [ _; "negative"; filename ] -> negative filename
  | [ _; "ground-negative"; filename ] -> ground_negative filename
  | [ _; "ground-positive"; filename ] -> ground_positive filename
  | [ _; "nullary-positive"; filename ] -> nullary_positive filename
  | [ _; "nullary-retry-inconclusive"; filename ] ->
      nullary_retry_inconclusive filename
  | [ _; "nullary-retry-counterexample"; filename ] ->
      nullary_retry_counterexample filename
  | [ _; "nullary-negative"; filename ] -> nullary_negative filename
  | [ _; "nullary-no-reveal"; filename ] -> nullary_no_reveal filename
  | [ _; "nullary-synthetic-only"; filename ] ->
      nullary_synthetic_only filename
  | [ _; "nullary-unsupported"; filename ] ->
      nullary_unsupported filename
  | [ _; "nullary-application-attack"; filename ] ->
      nullary_application_attack filename
  | [ _; "nullary-route-attack"; attack_name; filename ] ->
      nullary_route_attack attack_name filename
  | [ _; "nullary-local-attack"; attack_name; filename ] ->
      nullary_local_attack attack_name filename
  | [
   _;
   "nullary-primary-outcomes";
   verified_filename;
   counterexample_filename;
  ] ->
      nullary_primary_outcomes verified_filename counterexample_filename
  | [ _; "region-proof-call"; filename ] -> region_proof_call filename
  | [ _; "region-proof-summary-suppressed"; filename ] ->
      region_proof_summary_suppressed filename
  | [ _; "ground-false-premise"; filename ] ->
      ground_abstention ~force_inconclusive:true "false-premise" filename
  | [ _; "ground-nonnullary"; filename ] ->
      ground_abstention ~force_inconclusive:true "non-nullary" filename
  | [ _; "ground-attack"; attack_name; filename ] ->
      ground_attack attack_name filename
  | [ _; "summary-suppressed"; filename ] -> summary_suppressed filename
  | [ _; "attack"; attack_name; filename ] -> attack attack_name filename
  | [ _; "static-copy"; filename ] -> static_copy filename
  | [ _; "raw-sst"; filename ] -> raw_sst filename
  | [ _; "proof-branch-facts"; filename ] -> proof_branch_facts filename
  | [ _; "predecessors"; filename ] -> predecessor_check `None filename
  | [ _; "predecessors-suppress-left"; filename ] ->
      predecessor_check `Left filename
  | [ _; "predecessors-suppress-right"; filename ] ->
      predecessor_check `Right filename
  | [ _; "predecessors-suppressed"; filename ] ->
      predecessor_check `All filename
  | [ _; "one-predecessor"; filename ] -> one_predecessor_check filename
  | [ _; "reject"; filename ] -> reject filename
  | [ _; "builtin-matrix"; filename ] -> builtin_matrix filename
  | [ _; "builtin-false"; filename ] -> builtin_false filename
  | [ _; "builtin-scope-attack"; attack_name; filename ] ->
      builtin_scope_attack attack_name filename
  | [ _; "incomplete-ppx-reject"; filename ] ->
      incomplete_ppx_reject filename
  | [ _; "import-reject"; consumer; dependency ] ->
      import_reject consumer dependency
  | [ _; "nonrecursive-spec-path-product"; filename ] ->
      nonrecursive_spec_path_product filename
  | _ ->
      fail
         "usage: proof_body_assertions_tool.exe (solve FILE | \
         reconstruction FILE | reconstruction-matrix FILE | \
         reconstruction-cohabitation FILE | negative FILE | \
         region-observe FILE | region-suppressed FILE | region-mutants FILE | \
         region-attack NAME FILE | \
         ground-negative FILE | ground-positive FILE | \
         nullary-positive FILE | nullary-retry-inconclusive FILE | \
         nullary-retry-counterexample FILE | \
         nullary-negative FILE | nullary-no-reveal FILE | \
         nullary-synthetic-only FILE | nullary-unsupported FILE | \
         nullary-application-attack FILE | \
         nullary-route-attack NAME FILE | \
         nullary-local-attack NAME FILE | \
         nullary-primary-outcomes VERIFIED COUNTEREXAMPLE | \
         region-proof-call FILE | region-proof-summary-suppressed FILE | \
         ground-false-premise FILE | \
         ground-nonnullary FILE | ground-attack NAME FILE | \
         summary-suppressed FILE | attack NAME FILE | static-copy FILE | \
         raw-sst FILE | proof-branch-facts FILE | predecessors FILE | \
         predecessors-suppress-left FILE | \
         predecessors-suppress-right FILE | predecessors-suppressed FILE | \
         one-predecessor FILE | reject FILE | builtin-matrix FILE | \
         builtin-false FILE | builtin-scope-attack NAME FILE | \
         incomplete-ppx-reject FILE | \
         import-reject CONSUMER DEPENDENCY | \
         nonrecursive-spec-path-product FILE | \
         DEPENDENCY)"
