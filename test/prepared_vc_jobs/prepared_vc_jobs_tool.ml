let () = ignore Prepared_vc_jobs_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 2)
    format

let require label condition = if not condition then fail "%s" label

let span =
  Diagnostic.
    {
      file = "prepared_vc_jobs.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let symbol sort =
  Vir.
    {
      symbol_id = 0;
      source_name = "projected";
      sort;
      role = Input;
      span;
    }

let obligation
    ?(function_ref = Vir.{ function_index = 0; function_name = "prepared" })
    ?(index = 0) ?(projection_symbols = []) goal =
  Vir.
    {
      obligation_index = index;
      function_ref;
      kind = Assertion { assertion_ordinal = index };
      span;
      assumptions = [];
      required_preceding_safety = [];
      path_condition = [];
      goal;
      projection_symbols;
    }

let policy rlimit =
  match Solver_policy_private.create ~timeout_ms:60_000 ~rlimit with
  | Ok policy -> policy
  | Error error -> fail "%s" (Solver_policy_private.error_to_string error)

let same_counters left right =
  Marshal.to_string left [] = Marshal.to_string right []

let require_local label before telemetry =
  let after = Z3_bridge.counters () in
  require (label ^ " changed global counters") (same_counters before after);
  require (label ^ " local lifetime")
    (telemetry.Z3_bridge.capability_resolutions = 1
    && telemetry.translations = 1
    && telemetry.contexts_created = 1
    && telemetry.solvers_created = 1
    && telemetry.solver_resets = 1
    && telemetry.contexts_cleaned = 1
    && telemetry.contexts_live = 0
    && telemetry.maximum_contexts_live = 1
    && telemetry.selected_logics = [ "AUFLIA" ])

let query ~requires assertion =
  match
    Logic_ir.query (Logic_ir.create ()) ~axioms:[] ~assertions:[ assertion ]
      ~requires ~span
  with
  | Ok query -> query
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

let local_bridge () =
  Z3_bridge.reset_counters ();
  let before = Z3_bridge.counters () in
  let config = Z3_bridge.{ timeout_ms = 60_000; model = true } in
  let vir_cases =
    [
      ( "verified",
        Z3_bridge.Real,
        obligation (Vir.Boolean_constant true),
        (function Ok Z3_bridge.Verified -> true | _ -> false) );
      ( "counterexample",
        Z3_bridge.Real,
        obligation (Vir.Boolean_constant false),
        (function Ok (Z3_bridge.Counterexample _) -> true | _ -> false) );
      ( "unknown",
        Z3_bridge.Force_unknown,
        obligation (Vir.Boolean_constant true),
        (function Ok (Z3_bridge.Inconclusive _) -> true | _ -> false) );
      ( "exception",
        Z3_bridge.Force_backend_failure,
        obligation (Vir.Boolean_constant true),
        (function Error (Z3_bridge.Backend_failure _) -> true | _ -> false) );
    ]
  in
  List.iter
    (fun (label, controlled, obligation, accepted) ->
      let attempt =
        Z3_bridge.solve_vir_local ~controlled ~rlimit:100_000 config obligation
      in
      require (label ^ " outcome") (accepted attempt.result);
      require_local ("vir-" ^ label) before attempt.telemetry)
    vir_cases;
  let malformed_symbol = symbol Vir.Boolean in
  let malformed =
    obligation
      (Vir.Integer_compare
         ( Vir.Equal,
           Vir.Integer_symbol malformed_symbol,
           Vir.Integer_constant Z.zero ))
  in
  let malformed_attempt =
    Z3_bridge.solve_vir_local ~controlled:Z3_bridge.Real ~rlimit:100_000 config
      malformed
  in
  require "malformed VIR result"
    (match malformed_attempt.result with
    | Error (Z3_bridge.Malformed_vir _) -> true
    | _ -> false);
  require "malformed VIR global counters"
    (same_counters before (Z3_bridge.counters ()));
  require "malformed VIR local telemetry"
    (malformed_attempt.telemetry.capability_resolutions = 1
    && malformed_attempt.telemetry.translations = 1
    && malformed_attempt.telemetry.contexts_created = 0);
  let unsupported =
    Z3_bridge.solve_vir_local ~controlled:Z3_bridge.Real ~rlimit:100_000
      ~requires:[ Logic_ir.Nonlinear_integer_arithmetic ] config
      (obligation (Vir.Boolean_constant true))
  in
  require "capability result"
    (match unsupported.result with
    | Error (Z3_bridge.Unsupported_features _) -> true
    | _ -> false);
  require "capability telemetry"
    (unsupported.telemetry.capability_resolutions = 1
    && unsupported.telemetry.translations = 0
    && unsupported.telemetry.contexts_created = 0);
  let false_query = query ~requires:[] (Logic_ir.bool ~span false) in
  let true_query = query ~requires:[] (Logic_ir.bool ~span true) in
  let query_cases =
    [
      ("verified", Z3_bridge.Real, false_query);
      ("counterexample", Z3_bridge.Real, true_query);
      ("unknown", Z3_bridge.Force_unknown, true_query);
      ("exception", Z3_bridge.Force_backend_failure, true_query);
    ]
  in
  List.iter
    (fun (label, controlled, query) ->
      let attempt =
        Z3_bridge.solve_query_local ~controlled ~rlimit:100_000 config query
      in
      require_local ("query-" ^ label) before attempt.telemetry)
    query_cases;
  let unsupported_query =
    query ~requires:[ Logic_ir.Nonlinear_integer_arithmetic ]
      (Logic_ir.bool ~span true)
  in
  let unsupported =
    Z3_bridge.solve_query_local ~controlled:Z3_bridge.Real ~rlimit:100_000
      config unsupported_query
  in
  require "query capability result"
    (match unsupported.result with
    | Error (Z3_bridge.Unsupported_features _) -> true
    | _ -> false);
  require "query capability global"
    (same_counters before (Z3_bridge.counters ()));
  Printf.printf
    "local-bridge vir=verified/counterexample/unknown/error query=verified/counterexample/unknown/error global=unchanged cleanup=balanced\n"

let prepared_job () =
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let policy = policy 100_000 in
  let before_global = Z3_bridge.counters () in
  let prepare prepare index obligation =
    match prepare ~canonical_index:index ~solver_policy:policy obligation with
    | Ok job -> job
    | Error error -> fail "%s" (Vc_solver_job_private.error_to_string error)
  in
  let ordinary =
    prepare
      (fun ~canonical_index ~solver_policy obligation ->
        Vc_solver_job_private.prepare_ordinary ~canonical_index ~solver_policy
          obligation)
      3
      (obligation ~index:3
         ~projection_symbols:[ symbol Vir.Integer ]
         (Vir.Boolean_constant false))
  in
  let solved = Vc_solver_job_private.solve_prepared ordinary in
  require "ordinary index"
    (Vc_solver_job_private.job_index ordinary = 3
    && Vc_solver_job_private.result_index solved = 3);
  require "ordinary model projection"
    (match Vc_solver_job_private.outcome solved with
    | Ok (Solver_backend.Counterexample [ binding ]) ->
        binding.symbol = symbol Vir.Integer
    | _ -> false);
  let contribution =
    match Vc_solver_job_private.ordinary_contribution solved with
    | Some contribution -> contribution
    | None -> fail "ordinary contribution missing"
  in
  Solver_backend_counter_private.commit contribution;
  require "ordinary contribution count"
    (Solver_backend.For_testing.solver_creation_count () = 1);
  require_local "ordinary-job" before_global
    (Vc_solver_job_private.telemetry solved);
  let direct route index =
    match
      Vc_solver_job_private.prepare_direct ~route ~canonical_index:index
        ~solver_policy:policy (obligation ~index (Vir.Boolean_constant true))
    with
    | Error error -> fail "%s" (Vc_solver_job_private.error_to_string error)
    | Ok job -> Vc_solver_job_private.solve_prepared job
  in
  let structural = direct Vc_solver_job_private.Structural_rank 4 in
  let aggregate = direct Vc_solver_job_private.Logical_aggregate 5 in
  require "direct contribution leaked"
    (Vc_solver_job_private.ordinary_contribution structural = None
    && Vc_solver_job_private.ordinary_contribution aggregate = None
    && Solver_backend.For_testing.solver_creation_count () = 1);
  require_local "structural-job" before_global
    (Vc_solver_job_private.telemetry structural);
  require_local "aggregate-job" before_global
    (Vc_solver_job_private.telemetry aggregate);
  let first =
    direct Vc_solver_job_private.Structural_rank 6
    |> Vc_solver_job_private.telemetry
  in
  let second =
    direct Vc_solver_job_private.Structural_rank 7
    |> Vc_solver_job_private.telemetry
  in
  require "equal-policy jobs shared telemetry"
    (first.contexts_created = 1 && second.contexts_created = 1);
  let bad = symbol Vir.Boolean in
  let malformed =
    obligation
      (Vir.Integer_compare
         (Vir.Equal, Vir.Integer_symbol bad, Vir.Integer_constant Z.zero))
  in
  require "malformed job created"
    (match
       Vc_solver_job_private.prepare_ordinary ~canonical_index:8
         ~solver_policy:policy malformed
     with
    | Error _ -> true
    | Ok _ -> false);
  Printf.printf
    "prepared ordinary=projected/contribution-once direct=structural/logical/no-contribution indices=authenticated equal-policy=isolated malformed=zero-job\n"

let resource () =
  let policy = policy 1 in
  let job =
    match
      Vc_solver_job_private.prepare_ordinary ~canonical_index:0
        ~solver_policy:policy (obligation (Vir.Boolean_constant true))
    with
    | Ok job -> job
    | Error error -> fail "%s" (Vc_solver_job_private.error_to_string error)
  in
  let solved = Vc_solver_job_private.solve_prepared job in
  let telemetry = Vc_solver_job_private.telemetry solved in
  require "resource outcome"
    (match Vc_solver_job_private.outcome solved with
    | Ok
        (Solver_backend.Inconclusive
          { reason = Solver_backend.Resource_exhausted; _ }) ->
        true
    | _ -> false);
  require "resource cleanup"
    (telemetry.contexts_created = 1 && telemetry.solvers_created = 1
    && telemetry.solver_resets = 1 && telemetry.contexts_cleaned = 1
    && telemetry.contexts_live = 0);
  Printf.printf
    "resource outcome=resource-exhausted contexts/solvers/resets/cleaned=1/1/1/1 live=0\n"

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

type production_input = {
  implementation : Cmt_input.implementation;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  invariants : Type_invariant.environment;
}

let production_input filename =
  let implementation = load filename in
  let program = lower implementation in
  let validated = validate program in
  let invariants = invariants validated in
  { implementation; program; validated; invariants }

let run_production ?before_configure input =
  let solver_policy = policy 100_000 in
  let preflight = ref None in
  let run_preflight () =
    match
      Verification_solver_private.preflight ~solver_policy input.program
    with
    | Error error -> Error error
    | Ok prepared ->
        preflight := Some prepared;
        Ok (Verification_solver_private.termination_obligations prepared)
  in
  let configure_solver () =
    Option.iter (fun callback -> callback ()) before_configure;
    match !preflight with
    | None ->
        Error
          (Verification_pipeline.Internal_setup_error
             "prepared test lost production preflight")
    | Some preflight ->
        Verification_solver_private.configure ~solver_policy preflight
  in
  let proof_entry_activations () =
    match !preflight with
    | None -> fun (_ : Sst.function_id) -> []
    | Some preflight ->
        Verification_solver_private.proof_entry_activations preflight
  in
  match
    Verification_pipeline.run_validated ~imports:None
      ~implementation:input.implementation ~program:input.program
      ~validated:input.validated ~invariants:input.invariants
      ~preflight:run_preflight ~proof_entry_activations ~configure_solver
      ~on_result:(fun _ -> ())
  with
  | Ok report -> report
  | Error message -> fail "%s" message

let coordinator_order filename =
  let input = production_input filename in
  let solver_policy = policy 100_000 in
  let preflight =
    match
      Verification_solver_private.preflight ~solver_policy input.program
    with
    | Ok preflight -> preflight
    | Error _ -> fail "coordinator fixture failed preflight"
  in
  let solve =
    match
      Verification_solver_private.configure ~solver_policy preflight
    with
    | Ok solve -> solve
    | Error _ -> fail "coordinator fixture failed configuration"
  in
  let definition =
    match input.program.Sst.functions with
    | definition :: _ -> definition
    | [] -> fail "coordinator fixture has no definition"
  in
  let function_ref =
    Vir.
      {
        function_index = definition.Sst.function_id.function_index;
        function_name = definition.function_id.function_name;
      }
  in
  let body_provenance =
    match definition.Sst.body with
    | Sst.Checked_exec { provenance; _ }
    | Sst.Proof_body { provenance; _ }
    | Sst.Recursive_spec_definition { provenance; _ } ->
        provenance
    | Sst.Spec_definition _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        fail "coordinator fixture has no local body provenance"
  in
  let first =
    obligation ~function_ref ~index:40
      (Vir.Boolean_constant true)
  in
  let second =
    obligation ~function_ref ~index:41
      (Vir.Boolean_constant false)
  in
  let later =
    obligation ~function_ref ~index:42
      (Vir.Boolean_constant true)
  in
  let execution =
    Vir.
      {
        function_ref;
        mode = Sst.Exec;
        body_provenance;
        policy = definition.policy;
        trusted_summary_uses = [];
        reached_callback_calls = [];
        owned_tree_transitions = [];
        shared_scalar_heap_reads = [];
        shared_scalar_heap_writes = [];
        obligations = [ first; second; later ];
        exits = [];
      }
  in
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let commits = ref [] in
  let results =
    Verification_solver_private.For_testing.observe_local_counter_commits
      (fun ~canonical_index ~attempt_index telemetry ->
        commits :=
          ( canonical_index,
            attempt_index,
            telemetry.Z3_bridge.contexts_created )
          :: !commits)
      (fun () ->
        match
          solve
            {
              Verification_pipeline.definition;
              receipt_source = false;
              receipt_dependent = false;
              proof_activation_routes = [];
              execution;
            }
        with
        | Ok results -> results
        | Error message -> fail "%s" message)
  in
  require "production result order/cutoff"
    (match results with
    | [
     { Solver_backend.obligation = first_result; outcome = Verified };
     { obligation = second_result; outcome = Counterexample _ };
    ] ->
        first_result = first && second_result = second
    | _ -> false);
  let telemetry = Z3_bridge.counters () in
  require "production telemetry commit count/order"
    (telemetry.capability_resolutions = 2 && telemetry.translations = 2
    && telemetry.contexts_created = 2 && telemetry.solvers_created = 2
    && telemetry.solver_resets = 2 && telemetry.contexts_cleaned = 2
    && telemetry.contexts_live = 0 && telemetry.maximum_contexts_live = 1
    && telemetry.selected_logics = [ "AUFLIA"; "AUFLIA" ]);
  require "production ordinary contribution cutoff"
    (Solver_backend.For_testing.solver_creation_count () = 2);
  require "production commit invocation order"
    (List.rev !commits = [ (0, 0, 1); (1, 0, 1) ]);
  Printf.printf
    "production-coordinator results=40:verified,41:counterexample \
     cutoff=42-uncommitted telemetry-commits=2 order=canonical backend=2\n"

let production_failed_producer filename =
  let input = production_input filename in
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let report =
    run_production ~before_configure:Z3_bridge.reset_counters input
  in
  let counters = report.Verification_pipeline.counters in
  require "failed producer status"
    (match report.outcome with
    | Ok { Verification_pipeline.status = Counterexample; _ } -> true
    | Error _ | Ok _ -> false);
  require "failed producer session destruction" report.session_destroyed;
  require "failed producer issued a receipt"
    (counters.receipts_issued = 0 && counters.receipts_consumed = 0);
  require "failed producer reached dependent"
    (counters.dependent_lowerings = 0
    && counters.dependent_backend_contexts = 0
    && counters.dependent_solver_attempts = 0);
  require "failed producer result was not recorded"
    (counters.callee_failed_results > 0);
  Printf.printf
    "production-failed-producer status=counterexample receipts=0/0 \
     dependent=0/0/0 session-destroyed=true\n"

let production_recursive filename =
  let input = production_input filename in
  let commits = ref [] in
  Verification_solver_private.For_testing.force_recursive_local_inconclusive
    true;
  Recursive_spec_encoding.For_testing.force_nullary_branch_retry_unknown true;
  let report =
    Fun.protect
      ~finally:(fun () ->
        Verification_solver_private.For_testing
        .force_recursive_local_inconclusive false;
        Recursive_spec_encoding.For_testing
        .force_nullary_branch_retry_unknown false)
      (fun () ->
        Verification_solver_private.For_testing.observe_local_counter_commits
          (fun ~canonical_index ~attempt_index telemetry ->
            commits :=
              ( canonical_index,
                attempt_index,
                telemetry.Z3_bridge.contexts_created )
              :: !commits)
          (fun () ->
            run_production ~before_configure:Z3_bridge.reset_counters input))
  in
  require "recursive production status"
    (match report.Verification_pipeline.outcome with
    | Ok { status = Inconclusive; _ } -> true
    | Error _ | Ok _ -> false);
  require "recursive production session destruction" report.session_destroyed;
  require "recursive production commit invocation order"
    (List.rev !commits = [ (0, 0, 1); (0, 1, 1) ]);
  let telemetry = Z3_bridge.counters () in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  require "recursive production telemetry"
    (telemetry.contexts_created = 2 && telemetry.solvers_created = 2
    && telemetry.solver_resets = 2 && telemetry.contexts_cleaned = 2
    && telemetry.contexts_live = 0 && telemetry.maximum_contexts_live = 1);
  require "ground route committed native telemetry"
    (retry.queries = 1 && ground.attempts = 1);
  Printf.printf
    "production-recursive status=inconclusive commits=0.0,0.1 \
     native=2 ground-attempts=1/zero-commit session-destroyed=true\n"

let recursive_local filename =
  let implementation = load filename in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let prepared =
    match Recursive_spec_encoding.prepare program with
    | Ok prepared -> prepared
    | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let verified =
    match Recursive_spec_encoding.verify ~timeout_ms:60_000 prepared with
    | Ok verified -> verified
    | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let report =
    match
      Verification_driver_private.run ~timeout_ms:60_000
        ~allow_imported_opens:false implementation
    with
    | Ok report -> report
    | Error _ -> fail "recursive fixture failed before report"
  in
  let obligation =
    match Verification_driver_private.results report with
    | [ result ] -> result.Solver_backend.obligation
    | results ->
        fail "recursive fixture returned %d results" (List.length results)
  in
  let proof =
    List.find
      (fun definition ->
        match definition.Sst.body with
        | Sst.Proof_body _ -> true
        | Sst.Checked_exec _ | Sst.Spec_definition _
        | Sst.Recursive_spec_definition _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            false)
      program.Sst.functions
  in
  let activations =
    Recursive_spec_encoding.proof_entry_activations verified proof.function_id
  in
  let demand =
    match Recursive_spec_retry_demand_private.of_goal obligation.goal with
    | Ok (Some demand) -> demand
    | Ok None | Error _ -> fail "recursive fixture lost exact retry demand"
  in
  let input_symbol =
    Recursive_spec_retry_demand_private.arguments demand
    |> List.find_map (function
         | Vir.Recursive_aggregate_argument
             { aggregate_desc = Vir.Aggregate_symbol symbol; _ } ->
             Some symbol
         | Vir.Recursive_integer_argument _
         | Vir.Recursive_boolean_argument _
         | Vir.Recursive_aggregate_argument _
         | Vir.Recursive_parametric_argument _ ->
             None)
    |> Option.get
  in
  let empty =
    program.Sst.types
    |> List.find_map (fun definition ->
         match definition.Sst.type_kind with
         | Sst.Record_definition _ -> None
         | Sst.Variant_definition constructors ->
             List.find_opt
               (fun constructor ->
                 String.equal
                   constructor.Sst.constructor_id.constructor_name
                   "Empty")
               constructors
             |> Option.map (fun constructor -> constructor.Sst.constructor_id))
    |> Option.get
  in
  Recursive_spec_encoding.For_testing.force_nullary_branch_retry_unknown true;
  let controls = Recursive_spec_encoding.snapshot_solver_controls () in
  Recursive_spec_encoding.For_testing.force_nullary_branch_retry_unknown false;
  let payload =
    match
      Recursive_spec_encoding.prepare_proof ~controls verified ~activations
        ~ground_constructors:[ (input_symbol, empty) ] ~routed:true obligation
    with
    | Ok payload -> payload
    | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  let job =
    match
      Vc_solver_job_private.prepare_recursive ~canonical_index:0
        ~solver_policy:(policy 3_000_000) ~force_initial_inconclusive:true
        ~retry_rlimit:None payload obligation
    with
    | Ok job -> job
    | Error error -> fail "%s" (Vc_solver_job_private.error_to_string error)
  in
  Z3_bridge.reset_counters ();
  let before = Z3_bridge.counters () in
  let solved = Vc_solver_job_private.solve_prepared job in
  let routes = Vc_solver_job_private.route_counts solved in
  let observations =
    Vc_solver_job_private.recursive_observations solved
  in
  let telemetry = Vc_solver_job_private.telemetry solved in
  require "recursive controls were not copied"
    (match Vc_solver_job_private.outcome solved with
    | Ok (Solver_backend.Inconclusive _) -> true
    | _ -> false);
  require "recursive route matrix"
    (routes.recursive_initial = 1 && routes.recursive_retry = 1
    && routes.recursive_ground = 1);
  require "recursive observations"
    (observations.proof_queries = 2 && observations.retry.attempts = 1
    && observations.retry.queries = 1 && observations.retry.inconclusives = 1
    && observations.ground.attempts = 1);
  require "recursive local telemetry"
    (telemetry.contexts_created = 2 && telemetry.solvers_created = 2
    && telemetry.solver_resets = 2 && telemetry.contexts_cleaned = 2
    && telemetry.contexts_live = 0);
  require "recursive global counters changed"
    (same_counters before (Z3_bridge.counters ()));
  require "recursive contribution leaked"
    (Vc_solver_job_private.ordinary_contribution solved = None);
  Printf.printf
    "recursive routes=initial/retry/ground controls=copied telemetry=2/2/2/2 global=unchanged contribution=zero\n"

let () =
  match Array.to_list Sys.argv with
  | [ _; "local-bridge" ] -> local_bridge ()
  | [ _; "prepared-job" ] -> prepared_job ()
  | [ _; "resource" ] -> resource ()
  | [ _; "production-coordinator"; filename ] ->
      coordinator_order filename
  | [ _; "production-failed-producer"; filename ] ->
      production_failed_producer filename
  | [ _; "production-recursive"; filename ] ->
      production_recursive filename
  | [ _; "recursive-local"; filename ] -> recursive_local filename
  | _ ->
      fail
        "usage: prepared_vc_jobs_tool \
         (local-bridge|prepared-job|resource|production-coordinator CMT|\
         production-failed-producer CMT|production-recursive CMT|\
         recursive-local CMT)"
