type error
type prepared
type verified
type solver_controls
type prepared_proof
type prepared_query
type ground_payload
type verification_outcome =
  | Verification_verified of verified
  | Verification_inconclusive of Solver_backend.obligation_result
type ground_counterexample_outcome : value mod contended portable =
  | Ground_complete_violation
  | Ground_abstain

type ground_counterexample_counters : value mod contended portable = {
  attempts : int;
  complete_violations : int;
  abstentions : int;
  antecedents_checked : int;
}

type nullary_branch_retry_outcome =
  | Nullary_branch_verified
  | Nullary_branch_not_verified
  | Nullary_branch_inconclusive of Z3_bridge.inconclusive_reason
  | Nullary_branch_abstain

type nullary_branch_retry_counters : value mod contended portable = {
  attempts : int;
  queries : int;
  facts : int;
  verified : int;
  counterexamples : int;
  inconclusives : int;
  abstentions : int;
}

val prepare : Sst.program -> (prepared, error) result
val has_definitions : prepared -> bool
val termination_obligation_count : prepared -> int
val verify :
  ?rlimit:int ->
  timeout_ms:int ->
  prepared ->
  (verified, error) result

val verify_for_preflight :
  ?rlimit:int ->
  timeout_ms:int ->
  prepared ->
  (verification_outcome, error) result

val definition_ids : verified -> Sst.function_id list
val preservation_capabilities :
  verified -> Recursive_spec_preservation.capability list
val base_query : verified -> Sst.function_id -> (Logic_ir.query, error) result

val activated_query :
  verified ->
  Sst.function_id ->
  depths:int list ->
  (Logic_ir.query, error) result

val depth_equality_query :
  verified ->
  Sst.function_id ->
  arguments:[ `Int of Z.t | `Bool of bool ] list ->
  left_depth:int ->
  right_depth:int ->
  (Logic_ir.query, error) result

val public_link_query :
  verified ->
  Sst.function_id ->
  arguments:[ `Int of Z.t | `Bool of bool ] list ->
  depth:int ->
  (Logic_ir.query, error) result

val solve_proof_obligation :
  ?rlimit:int ->
  timeout_ms:int ->
  verified ->
  activations:Spec_unfolding.activation list ->
  Vir.obligation ->
  (Z3_bridge.outcome, error) result

val retry_nullary_branch :
  ?rlimit:int ->
  timeout_ms:int ->
  verified ->
  activations:Spec_unfolding.activation list ->
  ground_constructors:(Vir.symbol * Sst.constructor_id) list ->
  Vir.obligation ->
  (nullary_branch_retry_outcome, error) result

val ground_counterexample :
  verified ->
  activations:Spec_unfolding.activation list ->
  ground_constructors:(Vir.symbol * Sst.constructor_id) list ->
  Vir.obligation ->
  ground_counterexample_outcome

val snapshot_solver_controls : unit -> solver_controls

val prepare_proof :
  controls:solver_controls ->
  verified ->
  activations:Spec_unfolding.activation list ->
  ground_constructors:(Vir.symbol * Sst.constructor_id) list ->
  routed:bool ->
  Vir.obligation ->
  (prepared_proof, error) result

val prepared_initial_query : prepared_proof -> prepared_query
val prepared_retry_query : prepared_proof -> prepared_query option
val detach_prepared_query : prepared_query -> Z3_bridge.detached_query
val prepared_retry_eligible : prepared_proof -> bool
val prepared_ground_payload : prepared_proof -> ground_payload option
val prepared_ground_classification :
  prepared_proof ->
  (ground_counterexample_outcome * ground_counterexample_counters) option
val prepared_retry_control :
  prepared_proof -> [ `Real | `Unknown | `Counterexample ]

val solve_prepared_query_local :
  controlled:Z3_bridge.controlled ->
  rlimit:int ->
  Z3_bridge.config ->
  prepared_query ->
  Z3_bridge.outcome Z3_bridge.local_attempt

val classify_ground :
  ground_payload ->
  ground_counterexample_outcome * ground_counterexample_counters

val commit_prepared_observations :
  proof_queries:int ->
  retry:nullary_branch_retry_counters ->
  ground:ground_counterexample_counters ->
  last_retry_query:prepared_query option ->
  unit

val proof_entry_activations :
  verified -> Sst.function_id -> Spec_unfolding.activation list

val error_to_string : error -> string

module For_testing : sig
  val termination_obligations : prepared -> Vir.obligation list
  val verify_with_requirements :
    ?rlimit:int ->
    timeout_ms:int ->
    Logic_ir.feature list ->
    prepared ->
    (verified, error) result

  val proof_obligation_query :
    verified ->
    activations:Spec_unfolding.activation list ->
    Vir.obligation ->
    (Logic_ir.query, error) result

  val proof_query_construction_count : unit -> int
  val reset_proof_query_construction_count : unit -> unit
  val ground_counterexample_counters : unit -> ground_counterexample_counters
  val reset_ground_counterexample_counters : unit -> unit
  val nullary_branch_retry_counters : unit -> nullary_branch_retry_counters
  val reset_nullary_branch_retry_counters : unit -> unit
  val last_nullary_branch_retry_query : unit -> Logic_ir.query option
  val force_nullary_branch_retry_unknown : bool -> unit
  val force_nullary_branch_retry_counterexample : bool -> unit
  val nullary_branch_argument_supported : Vir.recursive_spec_argument -> bool
  val suppress_original_nullary_branch_activation : bool -> unit
  val suppress_reached_aggregate_authority : bool -> unit
  val a2_builder_construction_count : unit -> int
  val reset_a2_builder_construction_count : unit -> unit
  val helper_expansion_count : unit -> int
  val reset_helper_expansion_count : unit -> unit
  val reordered_helper_closure_copy_rejected :
    program:Sst.program -> definition:Sst.function_definition -> bool
  val raw_helper_copy_rejected :
    program:Sst.program -> definition:Sst.function_definition -> bool
  val partial_helper_call_rejected :
    program:Sst.program -> definition:Sst.function_definition -> bool
  val recursive_lowering_count : unit -> int
  val reset_recursive_lowering_count : unit -> unit
end
