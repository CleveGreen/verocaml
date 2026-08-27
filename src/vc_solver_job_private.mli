type prepared_job
type solved
type error

type direct_route = Structural_rank | Logical_aggregate

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

val prepare_ordinary :
  ?controlled:Z3_bridge.controlled ->
  canonical_index:int ->
  solver_policy:Solver_policy_private.t ->
  Vir.obligation ->
  (prepared_job, error) result

val prepare_direct :
  ?controlled:Z3_bridge.controlled ->
  route:direct_route ->
  canonical_index:int ->
  solver_policy:Solver_policy_private.t ->
  Vir.obligation ->
  (prepared_job, error) result

val prepare_recursive :
  canonical_index:int ->
  solver_policy:Solver_policy_private.t ->
  force_initial_inconclusive:bool ->
  retry_rlimit:int option ->
  Recursive_spec_encoding.prepared_proof ->
  Vir.obligation ->
  (prepared_job, error) result

val solve_prepared : prepared_job -> solved
val job_index : prepared_job -> int
val job_obligation : prepared_job -> Vir.obligation
val result_index : solved -> int
val result_obligation : solved -> Vir.obligation
val outcome : solved -> (Solver_backend.outcome, error) result
val telemetry : solved -> Z3_bridge.counters
val attempt_telemetry : solved -> Z3_bridge.counters list
val route_counts : solved -> route_counts
val recursive_observations : solved -> recursive_observations
val ordinary_contribution :
  solved -> Solver_backend_counter_private.contribution option
val error_to_string : error -> string
