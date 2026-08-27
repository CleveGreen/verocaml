(** Private, non-installed authority for one verification request.
    Session creation authenticates every callback certificate against the
    exact CMT compilation identity and the physical request session. *)
type t
type callee_snapshot
type obligation_manifest
type verified_completion
type consumed_fact
type finite_result_snapshot
type finite_result_manifest
type finite_result_completion
type proof_activation_authority
type proof_activation_snapshot
type proof_activation_manifest
type proof_activation_batch
type proof_activation_route
type routed_proof_obligation
type ground_constructor_match
type direct_exec_local_assertion_scope
type local_assertion_instance
type owned_root_scalar_observation_plan
type owned_contents_topology
type owned_contents_origin
type owned_contents_candidate
type owned_contents_permit
type owned_contents_manifest
type transition_predecessor_capability
type frozen_observation_permit
type frozen_constructor_manifest

type owned_contents_topology_node = {
  contents_node_path : Sst.field_id list;
  contents_node_value : Vir.aggregate_term;
  contents_node_constructor : Sst.constructor_id;
}

type owned_contents_topology_edge = {
  contents_edge_parent_path : Sst.field_id list;
  contents_edge_field : Sst.field_id;
  contents_edge_parent : Vir.aggregate_term;
  contents_edge_child : Vir.aggregate_term;
}

type owned_contents_counters = {
  lineages_opened : int;
  lineage_graph_authentications : int;
  mapped_candidates_authenticated : int;
  lineages_invalidated : int;
  lineages_closed : int;
  candidates_issued : int;
  candidates_consumed : int;
  candidates_rejected : int;
  candidates_finished : int;
  permits_issued : int;
  permits_consumed : int;
  permits_rejected : int;
  recursive_routes : int;
  ground_equations : int;
  model_results : int;
  manifests_issued : int;
  completions : int;
  receipts_finalized : int;
  successor_receipts : int;
  predecessor_retirements : int;
  permit_retirements : int;
  lineage_teardowns : int;
  candidate_teardowns : int;
  permit_teardowns : int;
  receipt_teardowns : int;
}

type counters = {
  callee_solver_attempts : int;
  callee_verified_results : int;
  callee_failed_results : int;
  dependent_lowerings : int;
  dependent_backend_contexts : int;
  dependent_solver_attempts : int;
  receipts_issued : int;
  receipts_consumed : int;
  finite_witness_issuances : int;
  finite_parent_issuances : int;
  finite_child_derivations : int;
  finite_result_promotions : int;
  finite_result_witness_records : int;
  finite_result_path_records : int;
  finite_result_manifests : int;
  finite_result_completions : int;
  finite_result_finalizations : int;
  finite_result_consumption_attempts : int;
  finite_result_consumptions : int;
  finite_consumptions : int;
  finite_formal_assumption_issuances : int;
  finite_formal_transfer_batches : int;
  finite_formal_transfers : int;
  finite_formal_transfer_consumptions : int;
  proof_call_visits : int;
  proof_call_summaries : int;
  recursive_spec_result_issuances : int;
  recursive_spec_result_consumptions : int;
  local_assertion_instances_issued : int;
  local_assertion_instances_consumed : int;
  owned_root_scalar_plans_issued : int;
  owned_root_scalar_plans_consumed : int;
  owned_root_scalar_plans_rejected : int;
  owned_root_scalar_observations : int;
  owned_root_scalar_equations : int;
  owned_root_scalar_bridge_paths : int;
  owned_root_scalar_bridge_equations : int;
  owned_root_scalar_fresh_successors : int;
  owned_root_scalar_reconstructions : int;
  transition_predecessor_transfers : int;
  transition_predecessor_consumptions : int;
  transition_result_receipts : int;
  transition_teardown_removals : int;
  transition_preservation_obligations : int;
  transition_nested_reconstructions : int;
  transition_root_reconstructions : int;
  shared_heap_issuances : int;
  shared_heap_writes : int;
  shared_heap_read_logs : int;
  shared_heap_epoch_advances : int;
  shared_heap_teardowns : int;
  invariant_cell_entry_eligibilities : int;
  invariant_cell_constructor_eligibilities : int;
  invariant_cell_closed_initializations : int;
  invariant_cell_opens : int;
  invariant_cell_updates : int;
  invariant_cell_closes : int;
  invariant_cell_effect_instantiations : int;
  invariant_cell_terminal_reads : int;
  invariant_cell_teardowns : int;
  frozen_constructor_template_issuances : int;
  frozen_constructor_template_teardowns : int;
  frozen_conditional_scope_issuances : int;
  frozen_conditional_scope_teardowns : int;
  frozen_result_instance_issuances : int;
  frozen_result_instance_teardowns : int;
  frozen_call_discharge_issuances : int;
  frozen_call_discharge_consumptions : int;
  frozen_call_discharge_teardowns : int;
  frozen_descent_witness_issuances : int;
  frozen_descent_witness_consumptions : int;
  frozen_descent_witness_teardowns : int;
  frozen_observation_consumptions : int;
  frozen_observation_teardowns : int;
}

val create :
  imports:Imported_callable.registration option ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  (t, string) result

val destroy : t -> unit
val is_active : t -> bool
val shared_heap_session_token : t -> unit ref
val register_shared_heap_teardown : t -> (unit -> unit) -> unit
val finalize_frozen_constructor :
  t ->
  Sst.function_definition ->
  Vir.function_execution ->
  frozen_constructor_manifest option ->
  Solver_backend.obligation_result list ->
  (unit, string) result
val authorize_frozen_constructor_obligations :
  t ->
  Sst.function_definition ->
  Vir.function_execution ->
  (frozen_constructor_manifest option, string) result
val register_frozen_formal_scope :
  t ->
  definition:Sst.function_definition ->
  formal:Sst.binding ->
  ordinal:int ->
  root:Vir.aggregate_term ->
  path_condition:Vir.boolean_term list ->
  (unit, string) result
val finalize_frozen_formal_scopes :
  t -> Sst.function_definition -> (unit, string) result
val issue_frozen_constructor_result_instance :
  t ->
  definition:Sst.function_definition ->
  callee:Sst.function_definition ->
  call_path:Diagnostic.span list ->
  path_condition:Vir.boolean_term list ->
  root:Vir.aggregate_term ->
  epoch:int ->
  allow_branch_reuse:bool ->
  (unit, string) result
val authorize_frozen_formal_call :
  t ->
  definition:Sst.function_definition ->
  callee:Sst.function_definition ->
  call_form:Sst.call_form ->
  call_path:Diagnostic.span list ->
  actuals:(Sst.expression * Vir.aggregate_term option) list ->
  path_condition:Vir.boolean_term list ->
  epoch:int ->
  (unit, string) result
val seal_frozen_observation :
  t ->
  definition:Sst.function_definition ->
  frozen:Sst.frozen_spine_prerequisite ->
  root:Vir.aggregate_term ->
  call_path:Diagnostic.span list ->
  path_condition:Vir.boolean_term list ->
  epoch:int ->
  (frozen_observation_permit, string) result
val consume_frozen_observation :
  t ->
  frozen_observation_permit ->
  definition:Sst.function_definition ->
  frozen:Sst.frozen_spine_prerequisite ->
  root:Vir.aggregate_term ->
  call_path:Diagnostic.span list ->
  path_condition:Vir.boolean_term list ->
  epoch:int ->
  (unit, string) result
val program_snapshot_digest : t -> string
val signature_snapshot_digest : t -> string
val canonical_callable_key :
  t -> Sst.function_definition -> (string, string) result

val canonical_callable_resolved_path :
  t -> Sst.function_definition -> (string, string) result

val canonical_callable_binding_uid :
  t -> Sst.function_definition -> (string, string) result

val canonical_callable_leaf_name :
  t -> Sst.function_definition -> (string, string) result

val callable_body_snapshot : t -> Sst.function_definition -> string

val issue_owned_root_scalar_observation_plan :
  t ->
  validated:Sst_validation.validated_program ->
  model:Sst_validation.model_descriptor ->
  caller:Sst.function_definition ->
  call:Sst.expression ->
  actual:Sst.expression ->
  root_binding:Sst.binding ->
  root:Vir.aggregate_term ->
  root_version:int ->
  path_condition:Vir.boolean_term list ->
  (owned_root_scalar_observation_plan, string) result

val consume_owned_root_scalar_observation_plan :
  t ->
  owned_root_scalar_observation_plan ->
  validated:Sst_validation.validated_program ->
  model:Sst_validation.model_descriptor ->
  caller:Sst.function_definition ->
  call:Sst.expression ->
  actual:Sst.expression ->
  root_binding:Sst.binding ->
  root:Vir.aggregate_term ->
  root_version:int ->
  path_condition:Vir.boolean_term list ->
  (unit, string) result

val owned_root_scalar_plan_root :
  owned_root_scalar_observation_plan -> Vir.aggregate_term

val owned_root_scalar_plan_template :
  owned_root_scalar_observation_plan ->
  Sst_validation_private.Public.owned_root_scalar_model_template

val note_owned_root_scalar_observation : t -> unit
val note_owned_root_scalar_equation : t -> unit
val note_owned_root_scalar_bridge_path : t -> unit
val note_owned_root_scalar_bridge_equation : t -> unit
val note_owned_root_scalar_reconstruction : t -> unit

val make_owned_contents_topology :
  root:Vir.aggregate_term ->
  nodes:owned_contents_topology_node list ->
  edges:owned_contents_topology_edge list ->
  (owned_contents_topology, string) result

val issue_closed_owned_contents_origin :
  t ->
  validated:Sst_validation.validated_program ->
  caller:Sst.function_definition ->
  expression:Sst.expression ->
  root:Vir.aggregate_term ->
  path_condition:Vir.boolean_term list ->
  (owned_contents_origin, string) result

val issue_successor_owned_contents_origin :
  t ->
  validated:Sst_validation.validated_program ->
  caller:Sst.function_definition ->
  expression:Sst.expression ->
  predecessor:owned_contents_origin ->
  predecessor_root:Vir.aggregate_term ->
  successor_root:Vir.aggregate_term ->
  path_condition:Vir.boolean_term list ->
  (owned_contents_origin, string) result

val owned_contents_origin_root :
  owned_contents_origin -> Vir.aggregate_term

val owned_contents_origin_version :
  owned_contents_origin -> int

val issue_owned_contents_candidate :
  t ->
  validated:Sst_validation.validated_program ->
  grammar:Sst_validation_private.Owned_recursive_contents_private.t ->
  caller:Sst.function_definition ->
  call:Sst.expression ->
  actual:Sst.expression ->
  root_binding:Sst.binding ->
  root:Vir.aggregate_term ->
  root_version:int ->
  path_condition:Vir.boolean_term list ->
  topology:owned_contents_topology ->
  origin:owned_contents_origin ->
  (owned_contents_candidate, string) result

val consume_owned_contents_candidate :
  t -> owned_contents_candidate -> (unit, string) result

val owned_contents_candidate_grammar :
  owned_contents_candidate -> Sst_validation_private.Owned_recursive_contents_private.t

val owned_contents_candidate_topology_root :
  owned_contents_candidate -> Vir.aggregate_term

val issue_owned_contents_permit :
  t ->
  owned_contents_candidate ->
  parent_path:Sst.field_id list ->
  field:Sst.field_id ->
  parent:Vir.aggregate_term ->
  child:Vir.aggregate_term ->
  call:Sst.expression ->
  call_span:Diagnostic.span ->
  path_condition:Vir.boolean_term list ->
  (owned_contents_permit, string) result

val consume_owned_contents_permit :
  t ->
  owned_contents_permit ->
  candidate:owned_contents_candidate ->
  parent_path:Sst.field_id list ->
  field:Sst.field_id ->
  parent:Vir.aggregate_term ->
  child:Vir.aggregate_term ->
  call:Sst.expression ->
  call_span:Diagnostic.span ->
  path_condition:Vir.boolean_term list ->
  (unit, string) result

val note_owned_contents_recursive_route :
  t -> owned_contents_candidate -> (unit, string) result

val note_owned_contents_ground_equation :
  t -> owned_contents_candidate -> (unit, string) result

val note_owned_contents_model_result :
  t -> owned_contents_candidate -> (unit, string) result

val finish_owned_contents_candidate :
  t -> owned_contents_candidate -> (unit, string) result

val authorize_owned_contents_obligations :
  t ->
  Sst.function_definition ->
  Vir.function_execution ->
  (owned_contents_manifest option, string) result

val complete_owned_contents :
  t ->
  owned_contents_manifest option ->
  Vir.function_execution ->
  Solver_backend.obligation_result list ->
  (bool, string) result

val preview_transition_predecessor :
  t ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  caller:Sst.function_definition ->
  callee:Sst.function_definition ->
  source:Sst.function_definition ->
  call_span:Diagnostic.span ->
  source_call_span:Diagnostic.span ->
  call_path:string ->
  actual:Sst.binding ->
  actual_version:int ->
  actual_path:string ->
  formal:Sst.binding ->
  root:Sst.binding ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  owned_version:int ->
  obligation_snapshot:string list ->
  branch_intersection:bool ->
  (transition_predecessor_capability, string) result

val activate_transition_predecessors :
  t -> transition_predecessor_capability list -> (unit, string) result

val consume_transition_predecessors :
  t ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  callee:Sst.function_definition ->
  formal:Sst.binding ->
  root:Sst.binding ->
  root_value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  owned_version:int ->
  obligation_snapshot:string list ->
  (unit, string) result

val note_transition_preservation : t -> Vir.invariant_transition_kind -> unit

val proof_activation_authority :
  t ->
  validated:Sst_validation.validated_program ->
  Sst.function_definition ->
  (proof_activation_authority, string) result

val exec_proof_region_activation_authority :
  t ->
  validated:Sst_validation.validated_program ->
  Sst.function_definition ->
  region:Sst.expression ->
  (proof_activation_authority, string) result

val snapshot_proof_activation :
  ?ground_matches:ground_constructor_match list ->
  proof_activation_authority ->
  activations:Spec_unfolding.activation list ->
  Vir.obligation ->
  proof_activation_snapshot

val finalize_proof_activation :
  t ->
  proof_activation_snapshot ->
  Vir.obligation ->
  (proof_activation_manifest, string) result

val proof_activation_batch :
  t ->
  (proof_activation_authority * Vir.obligation) list ->
  proof_activation_manifest list ->
  proof_activation_batch

val consume_proof_activation_batch :
  t ->
  proof_activation_batch ->
  Vir.function_execution ->
  (proof_activation_route list, string) result

val proof_activation_route :
  proof_activation_route ->
  Vir.obligation ->
  (routed_proof_obligation, string) result

val proof_activation_route_matches :
  proof_activation_route -> Vir.obligation -> bool

val routed_proof_activations :
  routed_proof_obligation -> Spec_unfolding.activation list

val routed_ground_constructors :
  routed_proof_obligation -> (Vir.symbol * Sst.constructor_id) list

val issue_ground_constructor_match :
  t ->
  authority:proof_activation_authority ->
  validated:Sst_validation.validated_program ->
  definition:Sst.function_definition ->
  case:Sst.case ->
  value:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  rank:Finite_value_registry.rank_snapshot ->
  receipt:Finite_value_registry.receipt ->
  path_condition:Vir.boolean_term list ->
  (ground_constructor_match, string) result

val issue_local_assertion_instance :
  t ->
  authority:proof_activation_authority ->
  validated:Sst_validation.validated_program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  ordinal:int ->
  assumptions:Vir.boolean_term list ->
  required_preceding_safety:Vir.boolean_term list ->
  path_condition:Vir.boolean_term list ->
  goal:Vir.boolean_term ->
  (local_assertion_instance, string) result

val authenticate_direct_exec_local_assertion_source :
  validated:Sst_validation.validated_program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  bool

val issue_and_consume_local_assertion :
  t ->
  direct_exec:bool ->
  authority:proof_activation_authority option ->
  validated:Sst_validation.validated_program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  ordinal:int ->
  assumptions:Vir.boolean_term list ->
  required_preceding_safety:Vir.boolean_term list ->
  path_condition:Vir.boolean_term list ->
  goal:Vir.boolean_term ->
  (direct_exec_local_assertion_scope option, string) result

val consume_local_assertion_instance :
  t ->
  local_assertion_instance ->
  expression:Sst.expression ->
  ordinal:int ->
  assumptions:Vir.boolean_term list ->
  required_preceding_safety:Vir.boolean_term list ->
  path_condition:Vir.boolean_term list ->
  goal:Vir.boolean_term ->
  (unit, string) result

val close_direct_exec_local_assertion_scope :
  t -> direct_exec_local_assertion_scope -> (unit, string) result

val callee_snapshot :
  t ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  Sst.function_definition ->
  (callee_snapshot, string) result

val authorize_obligations :
  t ->
  callee_snapshot ->
  Vir.function_execution ->
  (obligation_manifest, string) result

val complete :
  t ->
  obligation_manifest ->
  Solver_backend.obligation_result list ->
  (verified_completion, string) result

val issue :
  t ->
  verified_completion ->
  (unit, string) result

val consume :
  t ->
  callee_snapshot ->
  caller:Sst.function_id ->
  call_span:Diagnostic.span ->
  path_condition:Vir.boolean_term list ->
  result:Vir.aggregate_term ->
  (consumed_fact, string) result

val consumed_closed_fact : consumed_fact -> Vir.boolean_term
val consumed_matches_closed_fact : consumed_fact -> Vir.boolean_term -> bool
val same_consumed_fact : consumed_fact -> consumed_fact -> bool

val finite_registry : t -> (Finite_value_registry.t, string) result

val install_recursive_spec_preservation :
  t -> Recursive_spec_preservation.capability list -> (unit, string) result

val issue_recursive_spec_result :
  t ->
  caller_callable:string ->
  caller:Sst.function_id ->
  callee:Sst.function_id ->
  call_span:Diagnostic.span ->
  path_condition:Vir.boolean_term list ->
  application_identity:Recursive_spec_application_identity.t ->
  argument_receipts:
    (int * Vir.aggregate_term * Finite_value_registry.receipt) list ->
  result:Vir.aggregate_term ->
  mode:Sst.instance_mode ->
  typ:Sst.typ ->
  rank:Finite_value_registry.rank_snapshot ->
  (Finite_value_registry.receipt, string) result

val finite_result_snapshot :
  t ->
  validated:Sst_validation.validated_program ->
  Sst.function_definition ->
  rank:Finite_value_registry.rank_snapshot ->
  (finite_result_snapshot, string) result

val record_finite_result_exit :
  t ->
  finite_result_snapshot ->
  result:Vir.aggregate_term ->
  path_condition:Vir.boolean_term list ->
  (string, string) result

val promote_finite_result :
  t ->
  finite_result_snapshot ->
  source:Finite_value_registry.receipt option ->
  result:Vir.aggregate_term ->
  path_digest:string ->
  (Finite_value_registry.receipt, string) result

val authorize_finite_result_obligations :
  t ->
  finite_result_snapshot ->
  Vir.function_execution ->
  (finite_result_manifest option, string) result

val complete_finite_result :
  t ->
  finite_result_manifest ->
  Solver_backend.obligation_result list ->
  (finite_result_completion, string) result

val issue_finite_result :
  t -> finite_result_completion -> (unit, string) result

val consume_finite_result :
  t ->
  finite_result_snapshot ->
  caller:Sst.function_id ->
  caller_callable:string ->
  call_span:Diagnostic.span ->
  path_condition:Vir.boolean_term list ->
  result:Vir.aggregate_term ->
  (Finite_value_registry.receipt option, string) result

val note_callee_solver_attempt : t -> unit
val note_callee_result : t -> Solver_backend.outcome -> unit
val note_dependent_lowering : t -> unit
val note_dependent_backend_context : t -> unit
val note_dependent_solver_attempt : t -> unit
val note_shared_heap_issuance : t -> unit
val note_shared_heap_write : t -> unit
val note_shared_heap_read_log : t -> unit
val note_shared_heap_epoch_advance : t -> unit
val note_shared_heap_teardown : t -> unit
val note_invariant_cell_entry_eligibility : t -> unit
val note_invariant_cell_constructor_eligibility : t -> unit
val note_invariant_cell_closed_initialization : t -> unit
val note_invariant_cell_open : t -> unit
val note_invariant_cell_update : t -> unit
val note_invariant_cell_close : t -> unit
val note_invariant_cell_effect_instantiation : t -> unit
val note_invariant_cell_terminal_read : t -> unit
val note_invariant_cell_teardown : t -> unit
val counters : t -> counters
val render_counters : t -> string
val trace : t -> string -> unit

module For_testing : sig
  val set_owned_root_scalar_plan_attack_for_testing :
    string option -> unit
  val set_owned_contents_attack_for_testing : string option -> unit
  val observe_owned_contents_counters :
    (unit -> 'a) -> 'a * owned_contents_counters list
  val observe_owned_contents_seed_control :
    (unit -> 'a) -> 'a * string list

  type finite_result_lifecycle_counter_observation = {
    manifests : int;
    completions : int;
    consumption_attempts : int;
  }

  type owned_root_scalar_counter_observation = {
    plans_issued : int;
    plans_consumed : int;
    plans_rejected : int;
    observations : int;
    equations : int;
    bridge_paths : int;
    bridge_equations : int;
    dependent_lowerings : int;
    dependent_backend_contexts : int;
    dependent_solver_attempts : int;
  }

  type transition_predecessor_counter_observation = {
    transfers : int;
    consumptions : int;
    result_receipts : int;
    teardown_removals : int;
    preservation_obligations : int;
    nested_reconstructions : int;
    root_reconstructions : int;
    dependent_lowerings : int;
    dependent_backend_contexts : int;
    dependent_solver_attempts : int;
  }

  val observe_owned_root_scalar_counters :
    (unit -> 'a) -> 'a * owned_root_scalar_counter_observation list

  val observe_transition_predecessor_counters :
    (unit -> 'a) -> 'a * transition_predecessor_counter_observation list

  val set_transition_predecessor_attack_for_testing :
    string option -> unit

  val set_transition_predecessor_double_consume_for_testing :
    bool -> unit

  val reset_transition_predecessor_replay_for_testing : unit -> unit

  val observe_finite_result_lifecycle_counters :
    (unit -> 'a) ->
    'a * finite_result_lifecycle_counter_observation list

  val reset_local_assertion_instance_observation : unit -> unit
  val local_assertion_instance_observation : unit -> int * int

  val direct_exec_local_assertion_scope_observation :
    unit -> int * int * int

  val proof_activation_route_consumption_count : unit -> int

  val set_local_assertion_instance_attack_for_testing :
    string option -> unit

  val set_proof_activation_batch_attack_for_testing :
    string option -> unit

  val set_ground_constructor_match_attack_for_testing :
    string option -> unit

  val suppress_recursive_spec_preservation : bool -> unit
  val suppress_recursive_spec_result_issuance : bool -> unit
  val suppress_recursive_spec_construction_route : bool -> unit
  val observe_finite_result_calls : (unit -> 'a) -> 'a * string list
  val finite_result_call_instance_matrix : unit -> string list
  val observe_proof_activations : (unit -> 'a) -> 'a * string list
  val proof_activation_adversarial_matrix :
    on_valid:(unit -> unit) -> string list

  type obligation_mutation =
    | Complete
    | Missing
    | Incomplete
    | Failed
    | Timeout
    | Reordered
    | Duplicated
    | Extra
    | Fingerprint_mismatch
    | Wrong_return_boundary

  val adversarial_matrix : unit -> string list
  val set_finite_result_snapshot_attack_for_testing : string option -> unit
  val set_finite_result_call_instance_attack_for_testing :
    string option -> unit
  val set_finite_result_double_consume_for_testing : bool -> unit
  val retained_abi_matrix : unit -> string list
  val retained_abi_attack :
    Imported_callable.For_testing.abi_attack -> string
end
