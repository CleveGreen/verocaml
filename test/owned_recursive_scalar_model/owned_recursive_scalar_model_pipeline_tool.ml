let () = ignore Owned_recursive_scalar_model_prerequisites.ready
(* Keep this matrix linked to the scoped private executor/session ABI. *)

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

let count_substring text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop offset count =
    if offset + needle_length > text_length then count
    else if String.sub text offset needle_length = needle then
      loop (offset + needle_length) (count + 1)
    else loop (offset + 1) count
  in
  if needle_length = 0 then 0 else loop 0 0

let artifact name = Filename.concat "artifacts" (name ^ ".cmt")

let positive name =
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load (artifact name))
  with
  | Error _ -> fail "positive %s did not pass the production driver" name
  | Ok report ->
      let status = Verification_driver_private.status report in
      let counters = Verification_driver_private.counters report in
      let vir = Verification_driver_private.vir report |> Vir.to_string in
      let selectors =
        count_substring vir "verocaml_owned_root_scalar_v1"
      in
      let value_path_selectors =
        count_substring vir
          "verocaml_owned_root_scalar_v1.f0:top/c1:Node/f0:value/int"
      in
      let nested_tag_selectors =
        count_substring vir
          "verocaml_owned_root_scalar_v1.f0:top/c1:Node/f1:next/tag"
      in
      let value_model_mentions = count_substring vir "value_model" in
      if
        status <> Verification_pipeline.Verified
        || counters.Verification_session.owned_root_scalar_plans_issued = 0
        || counters.owned_root_scalar_plans_consumed
           <> counters.owned_root_scalar_plans_issued
        || counters.owned_root_scalar_plans_rejected <> 0
        || counters.owned_root_scalar_observations = 0
        || counters.owned_root_scalar_equations = 0 || selectors = 0
        || String.contains vir '\000'
      then fail "positive %s missed its scalar-observation evidence" name;
      if
        (name = "successor_freshness" || name = "nonvacuous_cut")
        &&
        counters.owned_root_scalar_fresh_successors = 0
      then fail "positive %s was vacuous or lacked reconstruction" name;
      if
        name = "nonvacuous_cut"
        && counters.owned_root_scalar_reconstructions <> 1
      then
        fail
          "positive nonvacuous_cut did not record exactly one Node-to-Empty reconstruction";
      if
        (name = "successor_freshness"
        || name = "direct_root_reconstruction_control"
        || name = "nested_noop_reconstruction_control")
        && counters.owned_root_scalar_reconstructions <> 0
      then fail "non-transitioning write was misclassified as a reconstruction";
      if
        (name = "constant_head_no_value_path"
        || name = "cross_template_demand_isolation")
        && value_path_selectors <> 0
      then fail "constant-head demand admitted an unrequested value path";
      if
        name = "cross_template_demand_isolation"
        && value_model_mentions <> 0
      then fail "uncalled same-domain value model reached VIR";
      if
        name = "uncontracted_mutation_no_demand"
        && (counters.owned_root_scalar_bridge_paths <> 0
           || counters.owned_root_scalar_bridge_equations <> 0)
      then fail "undemanded mutation emitted an owned observation bridge";
      if
        name = "successor_freshness"
        && value_path_selectors = 0
      then fail "requested value-path live control emitted no observation";
      if name = "nonvacuous_cut" && nested_tag_selectors = 0 then
        fail "requested nested-tag live control emitted no observation";
      Printf.printf
        "positive=%s status=%s plans=%d/%d/%d observations=%d equations=%d bridge=%d/%d fresh=%d reconstructions=%d root-selectors=%d value-path-selectors=%d nested-tag-selectors=%d value-model-mentions=%d functions=%d obligations=%d\n"
        name (status_name status) counters.owned_root_scalar_plans_issued
        counters.owned_root_scalar_plans_consumed
        counters.owned_root_scalar_plans_rejected
        counters.owned_root_scalar_observations
        counters.owned_root_scalar_equations
        counters.owned_root_scalar_bridge_paths
        counters.owned_root_scalar_bridge_equations
        counters.owned_root_scalar_fresh_successors
        counters.owned_root_scalar_reconstructions selectors
        value_path_selectors
        nested_tag_selectors
        value_model_mentions
        (Verification_driver_private.functions report)
        (Verification_driver_private.obligations report)

let driver_error_name = function
  | Verification_driver_private.Frontend_error _ -> "frontend"
  | Validation_error _ -> "validation"
  | Invariant_error _ -> "invariant"
  | Pipeline_error _ -> "pipeline"
  | Internal_error _ -> "internal"

let packaged_negative name =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let result, session_observations =
    Verification_session.For_testing.observe_owned_root_scalar_counters
      (fun () ->
        Verification_driver_private.run ~timeout_ms:1_000
          ~allow_imported_opens:false (load (artifact name)))
  in
  match result with
  | Ok _ -> fail "negative %s unexpectedly reached a driver report" name
  | Error error ->
      let observed =
        List.fold_left
          (fun totals
               (row :
                 Verification_session.For_testing
                 .owned_root_scalar_counter_observation) ->
            ( (let issued, consumed, rejected, observations, equations,
                   bridge_paths, bridge_equations, lowerings, contexts,
                   attempts =
                 totals
               in
               issued + row.plans_issued,
               consumed + row.plans_consumed,
               rejected + row.plans_rejected,
               observations + row.observations,
               equations + row.equations,
               bridge_paths + row.bridge_paths,
               bridge_equations + row.bridge_equations,
               lowerings + row.dependent_lowerings,
               contexts + row.dependent_backend_contexts,
               attempts + row.dependent_solver_attempts) ))
          (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
          session_observations
      in
      let plans_issued, plans_consumed, plans_rejected, observations,
          equations, bridge_paths, bridge_equations, dependent_lowerings,
          dependent_contexts, dependent_attempts =
        observed
      in
      let z3 = Z3_bridge.counters () in
      let backend = Solver_backend.For_testing.solver_creation_count () in
      let logic =
        Recursive_spec_encoding.For_testing.proof_query_construction_count ()
      in
      if
        plans_issued <> 0 || plans_consumed <> 0 || plans_rejected <> 0
        || observations <> 0 || equations <> 0 || dependent_lowerings <> 0
        || bridge_paths <> 0 || bridge_equations <> 0
        || dependent_contexts <> 0 || dependent_attempts <> 0
        || backend <> 0 || z3.contexts_created <> 0
        || z3.solvers_created <> 0 || logic <> 0
      then fail "negative %s crossed its zero-work boundary" name;
      Printf.printf
        "negative=%s rejected=%s sessions=%d plans=%d/%d/%d observations=%d equations=%d bridge=%d/%d logic-ir=%d dependent=%d/%d/%d backend=%d z3=%d/%d solver-calls=0\n"
        name (driver_error_name error) (List.length session_observations)
        plans_issued plans_consumed plans_rejected observations equations
        bridge_paths bridge_equations logic dependent_lowerings
        dependent_contexts dependent_attempts backend z3.contexts_created
        z3.solvers_created

let boundary_negative ~row ~name ~classify callback =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let result, session_observations =
    Verification_session.For_testing.observe_owned_root_scalar_counters callback
  in
  let rejected =
    match classify result with
    | Some rejected -> rejected
    | None -> fail "boundary %s unexpectedly admitted its input" name
  in
  let plans_issued, plans_consumed, plans_rejected, observations, equations,
      bridge_paths, bridge_equations, dependent_lowerings, dependent_contexts,
      dependent_attempts =
    List.fold_left
      (fun
        ( issued,
          consumed,
          rejected,
          observations,
          equations,
          bridge_paths,
          bridge_equations,
          lowerings,
          contexts,
          attempts )
        (seen :
          Verification_session.For_testing
          .owned_root_scalar_counter_observation) ->
        ( issued + seen.plans_issued,
          consumed + seen.plans_consumed,
          rejected + seen.plans_rejected,
          observations + seen.observations,
          equations + seen.equations,
          bridge_paths + seen.bridge_paths,
          bridge_equations + seen.bridge_equations,
          lowerings + seen.dependent_lowerings,
          contexts + seen.dependent_backend_contexts,
          attempts + seen.dependent_solver_attempts ))
      (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      session_observations
  in
  let z3 = Z3_bridge.counters () in
  let backend = Solver_backend.For_testing.solver_creation_count () in
  let logic =
    Recursive_spec_encoding.For_testing.proof_query_construction_count ()
  in
  if
    plans_issued <> 0 || plans_consumed <> 0 || plans_rejected <> 0
    || observations <> 0 || equations <> 0 || bridge_paths <> 0
    || bridge_equations <> 0 || dependent_lowerings <> 0
    || dependent_contexts <> 0 || dependent_attempts <> 0 || backend <> 0
    || z3.contexts_created <> 0 || z3.solvers_created <> 0 || logic <> 0
  then fail "boundary %s crossed its zero-work boundary" name;
  Printf.printf
    "%s=%s rejected=%s sessions=%d plans=0/0/0 observations=0 equations=0 bridge=0/0 logic-ir=0 dependent=0/0/0 backend=0 z3=0/0 solver-calls=0\n"
    row name rejected (List.length session_observations)

let loader_negative name =
  boundary_negative ~row:"partial-cmt" ~name
    ~classify:(function
      | Error diagnostic -> Some ("loader-" ^ diagnostic.Diagnostic.code)
      | Ok _ -> None)
    (fun () -> Cmt_input.load (artifact name))

let import_negative name consumer dependency =
  boundary_negative ~row:"import" ~name
    ~classify:(function
      | Error _ -> Some "interface-preflight"
      | Ok _ -> None)
    (fun () ->
      Interface_specification.authenticate ~timeout_ms:1_000
        ~dependency_files:[ dependency ] ~consumer_file:consumer)

let pipeline_attack attack =
  let implementation = load (artifact "scalar_model_baseline") in
  let program = lower implementation in
  let validated = validate program in
  let invariants = invariants validated in
  let configure_calls = ref 0 in
  let solve_calls = ref 0 in
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  Verification_session.For_testing
  .set_owned_root_scalar_plan_attack_for_testing (Some attack);
  let result =
    Fun.protect
      ~finally:(fun () ->
        Verification_session.For_testing
        .set_owned_root_scalar_plan_attack_for_testing None)
      (fun () ->
        Verification_pipeline.run_validated ~imports:None ~implementation
          ~program ~validated ~invariants ~preflight:(fun () -> Ok 0)
          ~proof_entry_activations:(fun () _ -> [])
          ~configure_solver:(fun () ->
            incr configure_calls;
            Ok
              (fun (request : Verification_pipeline.solve_request) ->
                incr solve_calls;
                Ok
                  (List.map
                     (fun obligation ->
                       {
                         Solver_backend.obligation;
                         outcome = Solver_backend.Verified;
                       })
                     request.execution.Vir.obligations)))
          ~on_result:(fun _ -> ()))
  in
  let report =
    match result with
    | Error message -> fail "attack %s session failed: %s" attack message
    | Ok report -> report
  in
  let rejected =
    match report.Verification_pipeline.outcome with
    | Error (Verification_pipeline.Engine_error _) -> true
    | Error (Setup_error _ | Solve_error _) | Ok _ -> false
  in
  let counters = report.counters in
  let z3 = Z3_bridge.counters () in
  let backend = Solver_backend.For_testing.solver_creation_count () in
  let logic =
    Recursive_spec_encoding.For_testing.proof_query_construction_count ()
  in
  if
    not rejected
    || counters.Verification_session.owned_root_scalar_plans_issued <> 1
    || counters.owned_root_scalar_plans_consumed <> 0
    || counters.owned_root_scalar_plans_rejected <> 1
    || counters.owned_root_scalar_observations <> 0
    || counters.owned_root_scalar_equations <> 0
    || counters.owned_root_scalar_bridge_paths <> 0
    || counters.owned_root_scalar_bridge_equations <> 0
    || counters.dependent_lowerings <> 0
    || counters.dependent_backend_contexts <> 0
    || counters.dependent_solver_attempts <> 0
    || !solve_calls <> 0 || backend <> 0 || z3.contexts_created <> 0
    || z3.solvers_created <> 0 || logic <> 0 || not report.session_destroyed
  then
    fail
      "attack %s crossed boundary rejected=%b plans=%d/%d/%d obs=%d eq=%d bridge=%d/%d dependent=%d/%d/%d configure=%d solve=%d backend=%d z3=%d/%d logic=%d destroyed=%b"
      attack rejected counters.owned_root_scalar_plans_issued
      counters.owned_root_scalar_plans_consumed
      counters.owned_root_scalar_plans_rejected
      counters.owned_root_scalar_observations
      counters.owned_root_scalar_equations
      counters.owned_root_scalar_bridge_paths
      counters.owned_root_scalar_bridge_equations
      counters.dependent_lowerings
      counters.dependent_backend_contexts
      counters.dependent_solver_attempts !configure_calls !solve_calls backend
      z3.contexts_created z3.solvers_created logic report.session_destroyed;
  Printf.printf
    "attack=%s rejected plans=1/0/1 observations=0 equations=0 bridge=0/0 logic-ir=0 dependent=0/0/0 backend=0 z3=0/0 solver-calls=0 session-destroyed=true\n"
    attack

let attacks =
  [
    "wrong-root";
    "stale-root";
    "copied";
    "rebound-root";
    "branch-root";
    "wrong-version";
    "wrong-path";
    "wrong-field";
    "wrong-constructor";
    "wrong-type";
    "wrong-model";
    "wrong-body";
    "wrong-signature";
    "wrong-program";
    "wrong-cmt";
    "wrong-family";
    "wrong-session";
    "substitution";
    "import";
  ]

let matrix () =
  List.iter positive
    [
      "scalar_model_baseline";
      "successor_freshness";
      "nonvacuous_cut";
      "opaque_local_client";
      "constant_head_no_value_path";
      "cross_template_demand_isolation";
      "uncontracted_mutation_no_demand";
      "direct_root_reconstruction_control";
      "nested_noop_reconstruction_control";
    ];
  List.iter packaged_negative
    [
      "recursive_traversal";
      "recursive_result";
      "mutable_result";
      "rank_finite_invariant_laundering";
      "imported_or_substituted_representation";
      "external_model";
      "trusted_model";
    ];
  List.iter loader_negative
    [
      "client_representation_access";
      "node_escape_or_equality";
      "model_effect";
      "ghost_mode_recovery";
    ];
  import_negative "retained-cmt" "artifacts/import/consumer.cmt"
    "artifacts/import/provider.cmt";
  import_negative "ordinary-cmt" "artifacts/ordinary/consumer.cmt"
    "artifacts/ordinary/provider.cmt";
  List.iter pipeline_attack attacks;
  print_endline
    "observer-injection=unavailable api=mutate-already-issued-plan-only"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--matrix" ] -> matrix ()
  | _ -> fail "usage: owned_recursive_scalar_model_pipeline_tool.exe --matrix"
