type recursive_retry_control : value mod contended portable =
  | Retry_real
  | Retry_unknown
  | Retry_counterexample

type ground_plan : value mod contended portable = {
  ground_complete_violation : bool;
  ground_attempts : int;
  ground_complete_violations : int;
  ground_abstentions : int;
  ground_antecedents_checked : int;
}

type recursive_route : value mod contended portable = {
  initial_query : Z3_bridge.detached_query;
  retry_query : Z3_bridge.detached_query option;
  retry_eligible : bool;
  ground_plan : ground_plan option;
  force_initial_inconclusive : bool;
  retry_control : recursive_retry_control;
  retry_rlimit : int;
}

type direct_route : value mod contended portable = {
  direct_query : Z3_bridge.detached_query;
  deliver_original_model : bool;
}

type route : value mod contended portable =
  | Ordinary of Z3_bridge.detached_query
  | Structural_rank of direct_route
  | Logical_aggregate of direct_route
  | Recursive of recursive_route

type vc_request : value mod contended portable = {
  canonical_index : int;
  timeout_ms : int;
  rlimit : int;
  route : route;
}

type request : value mod contended portable = {
  source_ordinal : int;
  vcs : vc_request list;
}

type retry_observation : value mod contended portable = {
  retry_attempts : int;
  retry_queries : int;
  retry_facts : int;
  retry_verified : int;
  retry_counterexamples : int;
  retry_inconclusives : int;
  retry_abstentions : int;
  retry_used : bool;
}

type ground_observation : value mod contended portable = {
  observed_ground_attempts : int;
  observed_ground_complete_violations : int;
  observed_ground_abstentions : int;
  observed_ground_antecedents_checked : int;
}

type vc_result : value mod contended portable = {
  result_index : int;
  result_outcome : (Z3_bridge.detached_outcome, string) result;
  attempt_telemetry : Z3_bridge.counters list;
  ordinary_contribution : bool;
  proof_queries : int;
  retry_observation : retry_observation;
  ground_observation : ground_observation;
}

type result : value mod contended portable = {
  result_source_ordinal : int;
  result_domain : int;
  vc_results : vc_result list;
  worker_exception : (int * string) option;
}

type slot : value mod contended portable =
  | Pending of request
  | Complete of result

val run : request -> result @@ portable

module For_testing : sig
  val reset : unit -> unit
  val set_spin_iterations : int -> unit
  val peak_active_functions : unit -> int
  val inject_worker_error : int option -> unit
  val cleanup_failures : unit -> int
end
