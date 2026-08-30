type error =
  | Malformed_vir of string
  | Bridge_error of Z3_bridge.error

type direct_route = Structural_rank | Logical_aggregate

type recursive_route = {
  prepared : Recursive_spec_encoding.prepared_proof;
  force_initial_inconclusive : bool;
  retry_rlimit : int;
}

type route =
  | Ordinary of Z3_bridge.controlled
  | Direct of direct_route * Z3_bridge.controlled
  | Recursive of recursive_route

type prepared_job = {
  canonical_index : int;
  solver_policy : Solver_policy_private.t;
  obligation : Vir.obligation;
  route : route;
}

type route_counts = {
  ordinary : int;
  structural_rank : int;
  logical_aggregate : int;
  recursive_initial : int;
  recursive_retry : int;
  recursive_ground : int;
}

type recursive_observations = {
  proof_queries : int;
  retry : Recursive_spec_encoding.nullary_branch_retry_counters;
  ground : Recursive_spec_encoding.ground_counterexample_counters;
  last_retry_query : Recursive_spec_encoding.prepared_query option;
}

type solved = {
  result_index : int;
  result_obligation : Vir.obligation;
  outcome : (Solver_backend.outcome, error) result;
  telemetry : Z3_bridge.counters;
  attempt_telemetry : Z3_bridge.counters list;
  route_counts : route_counts;
  recursive_observations : recursive_observations;
  ordinary_contribution :
    Solver_backend_counter_private.contribution option;
}

let direct_requirements =
  [
    Logic_ir.Named_sorts;
    Logic_ir.Uninterpreted_functions;
    Logic_ir.Linear_integer_arithmetic;
    Logic_ir.Quantifiers;
    Logic_ir.Explicit_patterns;
    Logic_ir.Quantifier_ids;
  ]

let empty_telemetry =
  {
    Z3_bridge.capability_resolutions = 0;
    translations = 0;
    contexts_created = 0;
    solvers_created = 0;
    solver_resets = 0;
    contexts_cleaned = 0;
    contexts_live = 0;
    maximum_contexts_live = 0;
    selected_logics = [];
  }

let add_telemetry (left : Z3_bridge.counters)
    (right : Z3_bridge.counters) =
  {
    Z3_bridge.capability_resolutions =
      left.Z3_bridge.capability_resolutions + right.capability_resolutions;
    translations = left.translations + right.translations;
    contexts_created = left.contexts_created + right.contexts_created;
    solvers_created = left.solvers_created + right.solvers_created;
    solver_resets = left.solver_resets + right.solver_resets;
    contexts_cleaned = left.contexts_cleaned + right.contexts_cleaned;
    contexts_live = left.contexts_live + right.contexts_live;
    maximum_contexts_live =
      max left.maximum_contexts_live right.maximum_contexts_live;
    selected_logics = left.selected_logics @ right.selected_logics;
  }

let zero_routes =
  {
    ordinary = 0;
    structural_rank = 0;
    logical_aggregate = 0;
    recursive_initial = 0;
    recursive_retry = 0;
    recursive_ground = 0;
  }

let zero_retry =
  {
    Recursive_spec_encoding.attempts = 0;
    queries = 0;
    facts = 0;
    verified = 0;
    counterexamples = 0;
    inconclusives = 0;
    abstentions = 0;
  }

let zero_ground =
  {
    Recursive_spec_encoding.attempts = 0;
    complete_violations = 0;
    abstentions = 0;
    antecedents_checked = 0;
  }

let zero_recursive =
  {
    proof_queries = 0;
    retry = zero_retry;
    ground = zero_ground;
    last_retry_query = None;
  }

let error_to_string = function
  | Malformed_vir message -> "malformed prepared VIR: " ^ message
  | Bridge_error error -> Z3_bridge.error_to_string error

let validate_vir ~requires obligation =
  Vir_logic_ir_translation_private.translate ~requires obligation
  |> Result.map (fun _ -> ())
  |> Result.map_error (fun message -> Malformed_vir message)

let prepare_ordinary ?(controlled = Z3_bridge.Real) ~canonical_index
    ~solver_policy obligation =
  Result.map
    (fun () ->
      {
        canonical_index;
        solver_policy;
        obligation;
        route = Ordinary controlled;
      })
    (validate_vir ~requires:[] obligation)

let prepare_direct ?(controlled = Z3_bridge.Real) ~route ~canonical_index
    ~solver_policy obligation =
  Result.map
    (fun () ->
      {
        canonical_index;
        solver_policy;
        obligation;
        route = Direct (route, controlled);
      })
    (validate_vir ~requires:direct_requirements obligation)

let prepare_recursive ~canonical_index ~solver_policy
    ~force_initial_inconclusive ~retry_rlimit prepared obligation =
  let retry_rlimit =
    Option.value retry_rlimit
      ~default:(Solver_policy_private.rlimit solver_policy)
  in
  Ok
    {
      canonical_index;
      solver_policy;
      obligation;
      route =
        Recursive { prepared; force_initial_inconclusive; retry_rlimit };
    }

let backend_reason = function
  | Z3_bridge.Resource_exhausted -> Solver_backend.Resource_exhausted
  | Z3_bridge.Timed_out -> Solver_backend.Timed_out
  | Z3_bridge.Backend_unknown reason ->
      Solver_backend.Backend_unknown reason

let inconclusive policy reason =
  Solver_backend.Inconclusive
    {
      configured_timeout_ms = Solver_policy_private.timeout_ms policy;
      configured_rlimit = Solver_policy_private.rlimit policy;
      reason;
    }

let model_value = function
  | Z3_bridge.Integer value -> Solver_backend.Integer value
  | Z3_bridge.Boolean value -> Solver_backend.Boolean value
  | Z3_bridge.Aggregate_identity value ->
      Solver_backend.Aggregate_identity value

let ordinary_outcome policy = function
  | Z3_bridge.Verified -> Solver_backend.Verified
  | Z3_bridge.Counterexample bindings ->
      Solver_backend.Counterexample
        (List.map
           (fun (binding : Z3_bridge.model_binding) ->
             {
               Solver_backend.symbol = binding.symbol;
               value = Option.map model_value binding.value;
             })
           bindings)
  | Z3_bridge.Inconclusive reason ->
      inconclusive policy (backend_reason reason)

let private_outcome policy = function
  | Z3_bridge.Verified -> Solver_backend.Verified
  | Z3_bridge.Counterexample _ -> Solver_backend.Counterexample []
  | Z3_bridge.Inconclusive reason ->
      inconclusive policy (backend_reason reason)

let direct_config policy model =
  {
    Z3_bridge.timeout_ms = Solver_policy_private.timeout_ms policy;
    model;
  }

let solve_vir job ~controlled ~requires ~ordinary =
  let attempt =
    Z3_bridge.solve_vir_local ~controlled
      ~rlimit:(Solver_policy_private.rlimit job.solver_policy)
      ~requires (direct_config job.solver_policy true) job.obligation
  in
  let outcome =
    Result.map
      (if ordinary then ordinary_outcome job.solver_policy
       else private_outcome job.solver_policy)
      attempt.result
    |> Result.map_error (fun error -> Bridge_error error)
  in
  (outcome, attempt.telemetry)

let recursive_initial_outcome job force outcome =
  let ordinary = private_outcome job.solver_policy outcome in
  if
    force
    &&
    match job.obligation.Vir.kind with
    | Vir.Local_assertion _ -> true
    | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Postcondition _
    | Vir.Call_precondition _ | Vir.Callback_precondition _
    | Vir.Invariant_validity _
    | Vir.Entry_measure_nonnegative _
    | Vir.Recursive_call_measure_nonnegative _
    | Vir.Recursive_call_strict_descent _ ->
        false
  then
    inconclusive job.solver_policy
      (Solver_backend.Backend_unknown "controlled recursive local unknown")
  else ordinary

let solve_query job ~controlled ~rlimit query =
  Recursive_spec_encoding.solve_prepared_query_local ~controlled ~rlimit
    (direct_config job.solver_policy false)
    query

let retry_observation outcome =
  match outcome with
  | Z3_bridge.Verified ->
      {
        zero_retry with
        attempts = 1;
        queries = 1;
        facts = 1;
        verified = 1;
      }
  | Z3_bridge.Counterexample _ ->
      {
        zero_retry with
        attempts = 1;
        queries = 1;
        facts = 1;
        counterexamples = 1;
      }
  | Z3_bridge.Inconclusive _ ->
      {
        zero_retry with
        attempts = 1;
        queries = 1;
        facts = 1;
        inconclusives = 1;
      }

let classify_ground prepared original =
  match Recursive_spec_encoding.prepared_ground_payload prepared with
  | None -> (original, zero_ground, 0)
  | Some payload ->
      let outcome, counters =
        Recursive_spec_encoding.classify_ground payload
      in
      let result =
        match outcome with
        | Recursive_spec_encoding.Ground_complete_violation ->
            Solver_backend.Counterexample []
        | Recursive_spec_encoding.Ground_abstain -> original
      in
      (result, counters, 1)

let solve_recursive_retry job route original attempt_telemetry =
  let prepared = route.prepared in
  match Recursive_spec_encoding.prepared_retry_query prepared with
  | None ->
      let result, ground, ground_routes = classify_ground prepared original in
      let retry, retry_routes =
        if Recursive_spec_encoding.prepared_retry_eligible prepared then
          ({ zero_retry with attempts = 1; abstentions = 1 }, 1)
        else (zero_retry, 0)
      in
      ( Ok result,
        List.fold_left add_telemetry empty_telemetry attempt_telemetry,
        attempt_telemetry,
        retry,
        ground,
        None,
        retry_routes,
        ground_routes )
  | Some query -> (
      let control =
        Recursive_spec_encoding.prepared_retry_control prepared
      in
      let attempt, attempt_telemetry =
        match control with
        | `Counterexample ->
            ( {
                Z3_bridge.result = Ok (Z3_bridge.Counterexample []);
                telemetry = empty_telemetry;
              },
              attempt_telemetry )
        | `Unknown ->
            let attempt =
              solve_query job ~controlled:Z3_bridge.Force_unknown
                ~rlimit:route.retry_rlimit query
            in
            (attempt, attempt_telemetry @ [ attempt.telemetry ])
        | `Real ->
            let attempt =
              solve_query job ~controlled:Z3_bridge.Real
                ~rlimit:route.retry_rlimit query
            in
            (attempt, attempt_telemetry @ [ attempt.telemetry ])
      in
      let telemetry =
        List.fold_left add_telemetry empty_telemetry attempt_telemetry
      in
      match attempt.result with
      | Error error ->
          ( Error (Bridge_error error),
            telemetry,
            attempt_telemetry,
            {
              zero_retry with
              attempts = 1;
              queries = 1;
              facts = 1;
            },
            zero_ground,
            Some query,
            1,
            0 )
      | Ok Z3_bridge.Verified ->
          ( Ok Solver_backend.Verified,
            telemetry,
            attempt_telemetry,
            retry_observation Z3_bridge.Verified,
            zero_ground,
            Some query,
            1,
            0 )
      | Ok (Z3_bridge.Inconclusive Z3_bridge.Resource_exhausted as outcome) ->
          ( Ok (private_outcome job.solver_policy outcome),
            telemetry,
            attempt_telemetry,
            retry_observation outcome,
            zero_ground,
            Some query,
            1,
            0 )
      | Ok
          (Z3_bridge.Counterexample _
          | Z3_bridge.Inconclusive
              (Z3_bridge.Timed_out | Z3_bridge.Backend_unknown _) as outcome) ->
          let result, ground, ground_routes =
            classify_ground prepared original
          in
          ( Ok result,
            telemetry,
            attempt_telemetry,
            retry_observation outcome,
            ground,
            Some query,
            1,
            ground_routes ))

let solve_recursive job route =
  let initial =
    solve_query job ~controlled:Z3_bridge.Real
      ~rlimit:(Solver_policy_private.rlimit job.solver_policy)
      (Recursive_spec_encoding.prepared_initial_query route.prepared)
  in
  let attempt_telemetry = [ initial.telemetry ] in
  match initial.result with
  | Error error ->
      ( Error (Bridge_error error),
        initial.telemetry,
        attempt_telemetry,
        zero_retry,
        zero_ground,
        None,
        0,
        0 )
  | Ok initial_outcome ->
      let ordinary =
        recursive_initial_outcome job route.force_initial_inconclusive
          initial_outcome
      in
      (match ordinary with
      | Solver_backend.Inconclusive
          { reason = Solver_backend.Resource_exhausted; _ } ->
          ( Ok ordinary,
            initial.telemetry,
            attempt_telemetry,
            zero_retry,
            zero_ground,
            None,
            0,
            0 )
      | Solver_backend.Inconclusive _ ->
          let outcome, telemetry, attempt_telemetry, retry, ground,
              last_retry_query,
              retry_routes, ground_routes =
            solve_recursive_retry job route ordinary attempt_telemetry
          in
          ( outcome,
            telemetry,
            attempt_telemetry,
            retry,
            ground,
            last_retry_query,
            retry_routes,
            ground_routes )
      | Solver_backend.Verified | Solver_backend.Counterexample _ ->
          ( Ok ordinary,
            initial.telemetry,
            attempt_telemetry,
            zero_retry,
            zero_ground,
            None,
            0,
            0 ))

let solve_prepared job =
  let solved =
  match job.route with
  | Ordinary controlled ->
      let outcome, telemetry =
        solve_vir job ~controlled ~requires:[] ~ordinary:true
      in
      {
        result_index = job.canonical_index;
        result_obligation = job.obligation;
        outcome;
        telemetry;
        attempt_telemetry = [ telemetry ];
        route_counts = { zero_routes with ordinary = 1 };
        recursive_observations = zero_recursive;
        ordinary_contribution =
          Some (Solver_backend_counter_private.contribution telemetry);
      }
  | Direct (route, controlled) ->
      let outcome, telemetry =
        solve_vir job ~controlled ~requires:direct_requirements ~ordinary:false
      in
      let route_counts =
        match route with
        | Structural_rank -> { zero_routes with structural_rank = 1 }
        | Logical_aggregate -> { zero_routes with logical_aggregate = 1 }
      in
      {
        result_index = job.canonical_index;
        result_obligation = job.obligation;
        outcome;
        telemetry;
        attempt_telemetry = [ telemetry ];
        route_counts;
        recursive_observations = zero_recursive;
        ordinary_contribution = None;
      }
  | Recursive route ->
      let outcome, telemetry, attempt_telemetry, retry, ground,
          last_retry_query, retry_routes, ground_routes =
        solve_recursive job route
      in
      {
        result_index = job.canonical_index;
        result_obligation = job.obligation;
        outcome;
        telemetry;
        attempt_telemetry;
        route_counts =
          {
            zero_routes with
            recursive_initial = 1;
            recursive_retry = retry_routes;
            recursive_ground = ground_routes;
          };
        recursive_observations =
          {
            proof_queries = 1 + retry.queries;
            retry;
            ground;
            last_retry_query;
          };
        ordinary_contribution = None;
      }
  in
  [%log.trace "solver obligation completed"
    ~canonical_index:(Delator.Field.int job.canonical_index)
    ~route_name:
      (Delator.Field.string
         (match job.route with
         | Ordinary _ -> "ordinary"
         | Direct (Structural_rank, _) -> "structural-rank"
         | Direct (Logical_aggregate, _) -> "logical-aggregate"
         | Recursive _ -> "recursive"))
    ~outcome_name:
      (Delator.Field.string
         (match solved.outcome with
         | Error _ -> "error"
         | Ok Solver_backend.Verified -> "verified"
         | Ok (Solver_backend.Counterexample _) -> "counterexample"
         | Ok (Solver_backend.Inconclusive _) -> "inconclusive"))
    ~attempts:(Delator.Field.int (List.length solved.attempt_telemetry))
    ~contexts:(Delator.Field.int solved.telemetry.contexts_created)
    ~solvers:(Delator.Field.int solved.telemetry.solvers_created)];
  solved
[@@delator.instrument] [@@delator.level trace]

let job_index job = job.canonical_index
let job_obligation job = job.obligation
let result_index solved = solved.result_index
let result_obligation solved = solved.result_obligation
let outcome solved = solved.outcome
let telemetry solved = solved.telemetry
let attempt_telemetry solved = solved.attempt_telemetry
let route_counts solved = solved.route_counts
let recursive_observations solved = solved.recursive_observations
let ordinary_contribution solved = solved.ordinary_contribution
