module Direct_candidate = Finite_value_registry.Direct_candidate
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
type mutable_counters = {
  mutable callee_solver_attempts : int;
  mutable callee_verified_results : int;
  mutable callee_failed_results : int;
  mutable dependent_lowerings : int;
  mutable dependent_backend_contexts : int;
  mutable dependent_solver_attempts : int;
  mutable receipts_issued : int;
  mutable receipts_consumed : int;
  mutable finite_result_manifests : int;
  mutable finite_result_completions : int;
  mutable finite_result_consumption_attempts : int;
  mutable local_assertion_instances_issued : int;
  mutable local_assertion_instances_consumed : int;
  mutable owned_root_scalar_plans_issued : int;
  mutable owned_root_scalar_plans_consumed : int;
  mutable owned_root_scalar_plans_rejected : int;
  mutable owned_root_scalar_observations : int;
  mutable owned_root_scalar_equations : int;
  mutable owned_root_scalar_bridge_paths : int;
  mutable owned_root_scalar_bridge_equations : int;
  mutable owned_root_scalar_fresh_successors : int;
  mutable owned_root_scalar_reconstructions : int;
  mutable transition_predecessor_transfers : int;
  mutable transition_predecessor_consumptions : int;
  mutable transition_result_receipts : int;
  mutable transition_teardown_removals : int;
  mutable transition_preservation_obligations : int;
  mutable transition_nested_reconstructions : int;
  mutable transition_root_reconstructions : int;
  mutable shared_heap_issuances : int;
  mutable shared_heap_writes : int;
  mutable shared_heap_read_logs : int;
  mutable shared_heap_epoch_advances : int;
  mutable shared_heap_teardowns : int;
  mutable invariant_cell_entry_eligibilities : int;
  mutable invariant_cell_constructor_eligibilities : int;
  mutable invariant_cell_closed_initializations : int;
  mutable invariant_cell_opens : int;
  mutable invariant_cell_updates : int;
  mutable invariant_cell_closes : int;
  mutable invariant_cell_effect_instantiations : int;
  mutable invariant_cell_terminal_reads : int;
  mutable invariant_cell_teardowns : int;
  mutable frozen_constructor_template_issuances : int;
  mutable frozen_constructor_template_teardowns : int;
  mutable frozen_conditional_scope_issuances : int;
  mutable frozen_conditional_scope_teardowns : int;
  mutable frozen_result_instance_issuances : int;
  mutable frozen_result_instance_teardowns : int;
  mutable frozen_call_discharge_issuances : int;
  mutable frozen_call_discharge_consumptions : int;
  mutable frozen_call_discharge_teardowns : int;
  mutable frozen_descent_witness_issuances : int;
  mutable frozen_descent_witness_consumptions : int;
  mutable frozen_descent_witness_teardowns : int;
  mutable frozen_observation_consumptions : int;
  mutable frozen_observation_teardowns : int;
}
type program_identity = {
  unit_identity : string;
  cmt_identity : string;
  family_identity : string;
  interface_digest : string;
  source_digest : string;
  program_snapshot : string;
  signature_snapshot : string;
}
type invariant_snapshot = {
  invariant_id : string;
  abstract_type : Sst.type_id;
  certificate_id : string;
  model : Sst.function_id;
  model_type : Sst.typ;
  predicate : Sst.function_id;
  predicate_digest : string;
}
type callee_snapshot = {
  program : program_identity;
  resolved_path : string;
  binding_uid : string;
  callable_key : string;
  callee : Sst.function_id;
  body_snapshot : string;
  body_provenance : Sst.body_provenance;
  mode : Sst.verification_mode;
  result_mode : Sst.instance_mode;
  result_type : Sst.typ;
  invariant : invariant_snapshot;
}
type receipt = {
  issuer : unit ref;
  session : unit ref;
  token : unit ref;
  callee : callee_snapshot;
  obligation_fingerprints : string list;
  obligation_set_fingerprint : string;
  return_boundary_fingerprints : string list;
}
type obligation_manifest = {
  issuer : unit ref;
  session : unit ref;
  token : unit ref;
  callee : callee_snapshot;
  obligation_fingerprints : string list;
  obligation_set_fingerprint : string;
  return_boundary_fingerprints : string list;
}
type verified_completion = {
  issuer : unit ref;
  session : unit ref;
  token : unit ref;
  manifest : obligation_manifest;
}

type transition_predecessor_capability = {
  mutable predecessor_issuer : unit ref;
  mutable predecessor_affinity : unit ref;
  mutable predecessor_session : unit ref;
  predecessor_token : unit ref;
  mutable predecessor_program : program_identity;
  mutable predecessor_caller : Sst.function_id;
  mutable predecessor_caller_key : string;
  mutable predecessor_caller_path : string;
  mutable predecessor_caller_binding_uid : string;
  mutable predecessor_caller_body : string;
  mutable predecessor_callee : Sst.function_id;
  mutable predecessor_callee_key : string;
  mutable predecessor_callee_path : string;
  mutable predecessor_callee_binding_uid : string;
  mutable predecessor_callee_body : string;
  mutable predecessor_call_span : Diagnostic.span;
  mutable predecessor_call_path : string;
  mutable predecessor_call_instance : string;
  mutable predecessor_source : callee_snapshot;
  mutable predecessor_source_call_span : Diagnostic.span;
  mutable predecessor_actual : Sst.binding;
  mutable predecessor_actual_symbol : string;
  mutable predecessor_actual_version : int;
  mutable predecessor_actual_path : string;
  mutable predecessor_formal : Sst.binding;
  mutable predecessor_root : Sst.binding;
  mutable predecessor_mode : Sst.instance_mode;
  mutable predecessor_type : Sst.typ;
  mutable predecessor_invariant : invariant_snapshot;
  mutable predecessor_owned_version : int;
  mutable predecessor_obligation_snapshot : string list;
  mutable predecessor_obligation_fingerprint : string;
  mutable predecessor_branch_intersection : bool;
}

type finite_result_snapshot = {
  finite_snapshot_issuer : unit ref;
  finite_snapshot_session : unit ref;
  finite_snapshot_token : unit ref;
  finite_program : program_identity;
  finite_resolved_path : string;
  finite_binding_uid : string;
  finite_callable_key : string;
  finite_callee : Sst.function_id;
  finite_body_snapshot : string;
  finite_body_provenance : Sst.body_provenance;
  finite_mode : Sst.verification_mode;
  finite_result_mode : Sst.instance_mode;
  finite_result_type : Sst.typ;
  finite_rank : Finite_value_registry.rank_snapshot;
  finite_candidate : Direct_candidate.candidate;
  mutable finite_result_exits : (string * string) list;
  mutable finite_result_facts : (string * string) list;
}

type finite_result_manifest = {
  finite_manifest_issuer : unit ref;
  finite_manifest_session : unit ref;
  finite_manifest_token : unit ref;
  finite_manifest_snapshot : finite_result_snapshot;
  finite_manifest_candidate : Direct_candidate.obligation_manifest;
  finite_obligation_fingerprints : string list;
  finite_obligation_set_fingerprint : string;
}

type finite_result_completion = {
  finite_completion_issuer : unit ref;
  finite_completion_session : unit ref;
  finite_completion_manifest : finite_result_manifest;
  finite_candidate_completion : Direct_candidate.completion;
}

type finite_result_call_instance = {
  finite_call_issuer : unit ref;
  finite_call_session : unit ref;
  finite_call_token : unit ref;
  finite_call_caller : Sst.function_id;
  finite_call_caller_callable : string;
  finite_call_callee_snapshot : string;
  finite_call_span : Diagnostic.span;
  finite_call_path_digest : string;
  finite_call_result_symbol : Vir.symbol;
}

type proof_activation_scope =
  | Full_proof_execution
  | Exec_proof_region of {
      proof_scope_validated : Sst_validation.validated_program;
      proof_scope_definition : Sst.function_definition;
      proof_scope_region : Sst.expression;
      proof_scope_region_snapshot : string;
    }

type proof_activation_authority = {
  proof_authority_issuer : unit ref;
  proof_authority_session : unit ref;
  proof_authority_program : program_identity;
  proof_authority_resolved_path : string;
  proof_authority_binding_uid : string;
  proof_authority_callable_key : string;
  proof_authority_callable : Sst.function_id;
  proof_authority_body_snapshot : string;
  proof_authority_body_provenance : Sst.body_provenance;
  proof_authority_scope : proof_activation_scope;
}

type ground_constructor_match = {
  ground_match_issuer : unit ref;
  ground_match_session : unit ref;
  ground_match_token : unit ref;
  ground_match_authority : proof_activation_authority;
  ground_match_validated : Sst_validation.validated_program;
  ground_match_definition : Sst.function_definition;
  ground_match_case : Sst.case;
  ground_match_value : Vir.aggregate_term;
  ground_match_symbol : Vir.symbol;
  ground_match_constructor : Sst.constructor_id;
  ground_match_constructor_digest : string;
  ground_match_mode : Sst.instance_mode;
  ground_match_rank : Finite_value_registry.rank_snapshot;
  ground_match_receipt : Finite_value_registry.receipt;
  ground_match_path_condition : Vir.boolean_term list;
  ground_match_path_digest : string;
}

type proof_activation_snapshot = {
  proof_snapshot_issuer : unit ref;
  proof_snapshot_session : unit ref;
  proof_snapshot_token : unit ref;
  proof_snapshot_authority : proof_activation_authority;
  proof_snapshot_activations : Spec_unfolding.activation list;
  proof_snapshot_ground_matches : ground_constructor_match list;
  proof_snapshot_ground_obligation_digest : string option;
  proof_snapshot_ground_activation_digest : string option;
  proof_snapshot_provisional_index : int;
  proof_snapshot_obligation_digest : string;
  proof_snapshot_assumptions_digest : string;
  proof_snapshot_path_digest : string;
}

type proof_activation_manifest = {
  proof_manifest_issuer : unit ref;
  proof_manifest_session : unit ref;
  proof_manifest_token : unit ref;
  proof_manifest_snapshot : proof_activation_snapshot;
  proof_manifest_obligation_index : int;
  proof_manifest_kind : string;
  proof_manifest_path_digest : string;
  proof_manifest_obligation_fingerprint : string;
}

type proof_activation_batch = {
  proof_batch_issuer : unit ref;
  proof_batch_session : unit ref;
  proof_batch_token : unit ref;
  proof_batch_expected_members :
    (proof_activation_authority * string) list;
  proof_batch_members :
    (proof_activation_authority * string) list;
  proof_batch_manifests : proof_activation_manifest list;
}

type proof_activation_route = {
  proof_route_obligation_fingerprint : string;
  proof_route_activations : Spec_unfolding.activation list;
  proof_route_ground_matches : ground_constructor_match list;
}

type routed_proof_obligation = {
  routed_proof_activations : Spec_unfolding.activation list;
  routed_ground_constructors : (Vir.symbol * Sst.constructor_id) list;
}

type direct_exec_local_assertion_scope = {
  direct_scope_issuer : unit ref;
  direct_scope_session : unit ref;
  direct_scope_token : unit ref;
  direct_scope_validated : Sst_validation.validated_program;
  direct_scope_program : Sst.program;
  direct_scope_definition : Sst.function_definition;
  direct_scope_expression : Sst.expression;
  direct_scope_predicate : Sst.expression;
  direct_scope_program_snapshot : string;
  direct_scope_definition_snapshot : string;
  direct_scope_ordinal : int;
  direct_scope_path_digest : string;
  direct_scope_goal_digest : string;
  mutable direct_scope_active : bool;
}

type local_assertion_instance_source = Proof_local_assertion of proof_activation_authority
  | Direct_exec_local_assertion of direct_exec_local_assertion_scope

type local_assertion_instance = {
  local_instance_issuer : unit ref;
  local_instance_session : unit ref;
  local_instance_token : unit ref;
  local_instance_validated : Sst_validation.validated_program;
  local_instance_program : Sst.program;
  local_instance_definition : Sst.function_definition;
  local_instance_expression : Sst.expression;
  local_instance_source : local_assertion_instance_source;
  local_instance_ordinal : int;
  local_instance_assumptions_digest : string;
  local_instance_path_digest : string;
  local_instance_goal_digest : string;
}

type owned_root_scalar_observation_plan = {
  owned_plan_issuer : unit ref;
  owned_plan_session : unit ref;
  owned_plan_token : unit ref;
  owned_plan_validated : Sst_validation.validated_program;
  owned_plan_program_snapshot : string;
  owned_plan_signature_snapshot : string;
  owned_plan_unit_identity : string;
  owned_plan_cmt_identity : string;
  owned_plan_family_identity : string;
  owned_plan_model :
    Sst_validation_private.Public.owned_root_scalar_model_template;
  owned_plan_model_definition : Sst.function_definition;
  owned_plan_model_callable : string;
  owned_plan_model_body : string;
  owned_plan_model_signature : string;
  owned_plan_model_paths : string;
  owned_plan_result_fields : string;
  owned_plan_root_type : Sst.type_id;
  owned_plan_formal : Sst.binding;
  owned_plan_caller : Sst.function_definition;
  owned_plan_call : Sst.expression;
  owned_plan_actual : Sst.expression;
  owned_plan_root_binding : Sst.binding;
  owned_plan_root : Vir.aggregate_term;
  owned_plan_root_identity : string;
  owned_plan_root_version : int;
  owned_plan_path_digest : string;
}

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

type owned_contents_topology = {
  contents_topology_root : Vir.aggregate_term;
  contents_topology_nodes : owned_contents_topology_node list;
  contents_topology_edges : owned_contents_topology_edge list;
}

type owned_contents_origin = {
  contents_origin_issuer : unit ref;
  contents_origin_session : unit ref;
  contents_origin_token : unit ref;
  contents_origin_validated : Sst_validation.validated_program;
  contents_origin_program_snapshot : string;
  contents_origin_signature_snapshot : string;
  contents_origin_unit_identity : string;
  contents_origin_cmt_identity : string;
  contents_origin_family_identity : string;
  contents_origin_caller : Sst.function_definition;
  contents_origin_caller_snapshot : string;
  contents_origin_expression : Sst.expression;
  contents_origin_expression_snapshot : string;
  contents_origin_path : string list;
  contents_origin_path_digest : string;
  contents_origin_predecessor : owned_contents_origin option;
  contents_origin_predecessor_root : Vir.aggregate_term option;
  contents_origin_root : Vir.aggregate_term;
  contents_origin_version : int;
  contents_origin_digest : string;
}

type owned_contents_lineage = {
  contents_lineage_issuer : unit ref;
  contents_lineage_session : unit ref;
  contents_lineage_token : unit ref;
  contents_lineage_validated : Sst_validation.validated_program;
  contents_lineage_program_snapshot : string;
  contents_lineage_signature_snapshot : string;
  contents_lineage_unit_identity : string;
  contents_lineage_cmt_identity : string;
  contents_lineage_family_identity : string;
  contents_lineage_grammar :
    Sst_validation_private.Owned_recursive_contents_private.t;
  contents_lineage_grammar_digest : string;
  contents_lineage_model_snapshot : string;
  contents_lineage_helper_snapshot : string;
  contents_lineage_caller : Sst.function_definition;
  contents_lineage_caller_snapshot : string;
  contents_lineage_seed_receipt : unit ref option;
  mutable contents_lineage_candidates : owned_contents_candidate list;
  mutable contents_lineage_manifest : unit ref option;
  mutable contents_lineage_closed : bool;
  mutable contents_lineage_invalidated : bool;
}

and owned_contents_candidate = {
  contents_candidate_issuer : unit ref;
  contents_candidate_session : unit ref;
  contents_candidate_token : unit ref;
  contents_candidate_validated : Sst_validation.validated_program;
  contents_candidate_program_snapshot : string;
  contents_candidate_signature_snapshot : string;
  contents_candidate_unit_identity : string;
  contents_candidate_cmt_identity : string;
  contents_candidate_family_identity : string;
  contents_candidate_grammar : Sst_validation_private.Owned_recursive_contents_private.t;
  contents_candidate_grammar_digest : string;
  contents_candidate_model_snapshot : string;
  contents_candidate_helper_snapshot : string;
  contents_candidate_caller : Sst.function_definition;
  contents_candidate_call : Sst.expression;
  contents_candidate_actual : Sst.expression;
  contents_candidate_root_binding : Sst.binding;
  contents_candidate_root : Vir.aggregate_term;
  contents_candidate_root_identity : string;
  contents_candidate_root_version : int;
  contents_candidate_path_digest : string;
  contents_candidate_path : string list;
  contents_candidate_topology : owned_contents_topology;
  contents_candidate_topology_digest : string;
  contents_candidate_mapped_nodes : owned_contents_mapped_candidate list;
  contents_candidate_mapped_edges : owned_contents_mapped_edge list;
  contents_candidate_origin : owned_contents_origin;
  contents_candidate_origin_class :
    Sst_validation_private.Owned_recursive_contents_private.origin_class;
  contents_candidate_origin_digest : string;
  contents_candidate_lineage : owned_contents_lineage;
  contents_candidate_predecessor : owned_contents_candidate option;
  mutable contents_candidate_started : bool;
  mutable contents_candidate_finished : bool;
  mutable contents_candidate_finalized : bool;
  mutable contents_candidate_retired : bool;
}

and owned_contents_mapped_candidate = {
  contents_mapped_issuer : unit ref;
  contents_mapped_session : unit ref;
  contents_mapped_token : unit ref;
  contents_mapped_path : Sst.field_id list;
  contents_mapped_value : Vir.aggregate_term;
  contents_mapped_constructor : Sst.constructor_id;
}

and owned_contents_mapped_edge = {
  contents_mapped_edge_parent : owned_contents_mapped_candidate;
  contents_mapped_edge_field : Sst.field_id;
  contents_mapped_edge_child : owned_contents_mapped_candidate;
}

type owned_contents_receipt = {
  contents_receipt_issuer : unit ref;
  contents_receipt_session : unit ref;
  contents_receipt_token : unit ref;
  contents_receipt_candidate : owned_contents_candidate;
  contents_receipt_origin : owned_contents_origin;
  contents_receipt_grammar :
    Sst_validation_private.Owned_recursive_contents_private.t;
  contents_receipt_caller : Sst.function_definition;
  contents_receipt_root : Vir.aggregate_term;
  contents_receipt_root_version : int;
  contents_receipt_path : string list;
  contents_receipt_topology_digest : string;
  mutable contents_receipt_retired : bool;
}

type owned_contents_permit = {
  contents_permit_issuer : unit ref;
  contents_permit_session : unit ref;
  contents_permit_token : unit ref;
  contents_permit_lineage : owned_contents_lineage;
  contents_permit_candidate : owned_contents_candidate;
  contents_permit_parent_candidate : owned_contents_mapped_candidate;
  contents_permit_child_candidate : owned_contents_mapped_candidate;
  contents_permit_parent_path : Sst.field_id list;
  contents_permit_field : Sst.field_id;
  contents_permit_parent : Vir.aggregate_term;
  contents_permit_child : Vir.aggregate_term;
  contents_permit_call : Sst.expression;
  contents_permit_call_snapshot : string;
  contents_permit_call_span : Diagnostic.span;
  contents_permit_path_digest : string;
  mutable contents_permit_consumed : bool;
  mutable contents_permit_retired : bool;
}

type owned_contents_manifest = {
  contents_manifest_issuer : unit ref;
  contents_manifest_session : unit ref;
  contents_manifest_token : unit ref;
  contents_manifest_definition : Sst.function_definition;
  contents_manifest_lineages : owned_contents_lineage list;
  contents_manifest_candidates : owned_contents_candidate list;
  contents_manifest_obligations : string list;
  contents_manifest_set_digest : string;
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

type mutable_owned_contents_counters = {
  mutable lineages_opened : int;
  mutable lineage_graph_authentications : int;
  mutable mapped_candidates_authenticated : int;
  mutable lineages_invalidated : int;
  mutable lineages_closed : int;
  mutable candidates_issued : int;
  mutable candidates_consumed : int;
  mutable candidates_rejected : int;
  mutable candidates_finished : int;
  mutable permits_issued : int;
  mutable permits_consumed : int;
  mutable permits_rejected : int;
  mutable recursive_routes : int;
  mutable ground_equations : int;
  mutable model_results : int;
  mutable manifests_issued : int;
  mutable completions : int;
  mutable receipts_finalized : int;
  mutable successor_receipts : int;
  mutable predecessor_retirements : int;
  mutable permit_retirements : int;
  mutable lineage_teardowns : int;
  mutable candidate_teardowns : int;
  mutable permit_teardowns : int;
  mutable receipt_teardowns : int;
}

type frozen_constructor_template = {
  frozen_template_issuer : unit ref;
  frozen_template_session : unit ref;
  frozen_template_token : unit ref;
  frozen_template_program : program_identity;
  frozen_template_descriptor : Sst.frozen_spine_prerequisite;
  frozen_template_definition : Sst.function_definition;
  frozen_template_callable : string;
  frozen_template_resolved_path : string;
  frozen_template_binding_uid : string;
  frozen_template_body : string;
  frozen_template_definition_result : Vir.aggregate_term;
  frozen_template_definition_child : Vir.aggregate_term;
  frozen_template_definition_terminal_edge : Vir.aggregate_term;
  frozen_template_result_path : string;
  frozen_template_obligation_set : string;
  frozen_template_postconditions : string list;
  frozen_template_invariants : string list;
  frozen_template_structural_epoch : int;
}

type frozen_constructor_manifest = {
  frozen_constructor_manifest_issuer : unit ref;
  frozen_constructor_manifest_session : unit ref;
  frozen_constructor_manifest_token : unit ref;
  frozen_constructor_manifest_program : program_identity;
  frozen_constructor_manifest_descriptor : Sst.frozen_spine_prerequisite;
  frozen_constructor_manifest_definition : Sst.function_definition;
  frozen_constructor_manifest_body : string;
  frozen_constructor_manifest_obligations : string list;
  frozen_constructor_manifest_postconditions : string list;
  frozen_constructor_manifest_invariants : string list;
}

type frozen_conditional_scope = {
  frozen_conditional_issuer : unit ref;
  frozen_conditional_session : unit ref;
  frozen_conditional_token : unit ref;
  frozen_conditional_template : frozen_constructor_template;
  frozen_conditional_definition : Sst.function_definition;
  frozen_conditional_callable : string;
  frozen_conditional_resolved_path : string;
  frozen_conditional_binding_uid : string;
  frozen_conditional_body : string;
  frozen_conditional_formal : Sst.binding;
  frozen_conditional_ordinal : int;
  frozen_conditional_root : Vir.aggregate_term;
  frozen_conditional_child : Vir.aggregate_term;
  frozen_conditional_terminal_edge : Vir.aggregate_term;
  frozen_conditional_path : string;
  frozen_conditional_entry_path_condition : Vir.boolean_term list;
  frozen_conditional_structural_epoch : int;
  mutable frozen_conditional_verified : bool;
  mutable frozen_conditional_observation_permits :
    frozen_observation_permit list;
}

and frozen_constructor_result_instance = {
  frozen_instance_issuer : unit ref;
  frozen_instance_session : unit ref;
  frozen_instance_token : unit ref;
  frozen_instance_template : frozen_constructor_template;
  frozen_instance_program : program_identity;
  frozen_instance_root_definition : Sst.function_definition;
  frozen_instance_root_callable : string;
  frozen_instance_root_body : string;
  frozen_instance_caller : Sst.function_definition;
  frozen_instance_caller_callable : string;
  frozen_instance_caller_body : string;
  frozen_instance_callee : Sst.function_definition;
  frozen_instance_callee_callable : string;
  frozen_instance_callee_body : string;
  frozen_instance_call_edge : Sst_validation.call_edge_descriptor;
  frozen_instance_call_span : Diagnostic.span;
  frozen_instance_call_path : Diagnostic.span list;
  frozen_instance_path_condition : Vir.boolean_term list;
  frozen_instance_result_root : Vir.aggregate_term;
  frozen_instance_child : Vir.aggregate_term;
  frozen_instance_terminal_edge : Vir.aggregate_term;
  frozen_instance_structural_epoch : int;
  frozen_instance_digest : string;
}

and frozen_call_discharge = {
  frozen_discharge_issuer : unit ref;
  frozen_discharge_session : unit ref;
  frozen_discharge_token : unit ref;
  frozen_discharge_instance : frozen_constructor_result_instance;
  frozen_discharge_conditional : frozen_conditional_scope option;
  frozen_discharge_program : program_identity;
  frozen_discharge_root_definition : Sst.function_definition;
  frozen_discharge_root_callable : string;
  frozen_discharge_root_body : string;
  frozen_discharge_caller : Sst.function_definition;
  frozen_discharge_caller_callable : string;
  frozen_discharge_caller_body : string;
  frozen_discharge_callee : Sst.function_definition;
  frozen_discharge_callee_callable : string;
  frozen_discharge_callee_body : string;
  frozen_discharge_call_edge : Sst_validation.call_edge_descriptor;
  frozen_discharge_call_span : Diagnostic.span;
  frozen_discharge_call_path : Diagnostic.span list;
  frozen_discharge_formal : Sst.binding;
  frozen_discharge_ordinal : int;
  frozen_discharge_actual : Sst.expression;
  frozen_discharge_root : Vir.aggregate_term;
  frozen_discharge_path_condition : Vir.boolean_term list;
  frozen_discharge_epoch : int;
  frozen_discharge_structural_epoch : int;
  frozen_discharge_digest : string;
  mutable frozen_discharge_consumed : bool;
  mutable frozen_discharge_observation_permits : frozen_observation_permit list;
  mutable frozen_discharge_observed_epochs : int list;
}

and frozen_observation_source =
  | Frozen_conditional_proof of frozen_conditional_scope
  | Frozen_discharged_call of frozen_call_discharge

and frozen_observation_permit = {
  frozen_observation_issuer : unit ref;
  frozen_observation_session : unit ref;
  frozen_observation_token : unit ref;
  frozen_observation_source : frozen_observation_source;
  frozen_observation_definition : Sst.function_definition;
  frozen_observation_root : Vir.aggregate_term;
  frozen_observation_call_path : Diagnostic.span list;
  frozen_observation_path_condition : Vir.boolean_term list;
  frozen_observation_epoch : int;
  frozen_observation_digest : string;
  mutable frozen_observation_consumed : bool;
}

type t = {
  issuer : unit ref;
  session : unit ref;
  imported_registration : Imported_callable.registration option;
  identity : program_identity;
  mutable active : bool;
  mutable validated : Sst_validation.validated_program option;
  mutable invariants : Type_invariant.environment option;
  mutable callable_identities : Verification_identity.t option;
  mutable authorized_callees : callee_snapshot list;
  mutable obligation_manifests : obligation_manifest list;
  mutable verified_completions : verified_completion list;
  mutable receipts : receipt list;
  mutable consumed_result_symbols : Vir.symbol list;
  mutable consumed_result_routes :
    (Vir.symbol * Sst.function_id * Diagnostic.span * string) list;
  mutable finite_result_snapshots : finite_result_snapshot list;
  mutable finite_result_manifests : finite_result_manifest list;
  mutable finite_result_completions : finite_result_completion list;
  mutable finite_consumed_call_instances : finite_result_call_instance list;
  mutable proof_activation_authorities : proof_activation_authority list;
  mutable proof_activation_manifests : proof_activation_manifest list;
  mutable consumed_proof_activation_batches : unit ref list;
  mutable consumed_proof_activation_manifests : unit ref list;
  mutable finalized_proof_activation_snapshots : unit ref list;
  mutable issued_ground_constructor_matches : ground_constructor_match list;
  mutable active_direct_exec_local_assertion_scope : direct_exec_local_assertion_scope option;
  mutable issued_local_assertion_instances : local_assertion_instance list;
  mutable consumed_local_assertion_instances : unit ref list;
  mutable issued_owned_root_scalar_plans :
    owned_root_scalar_observation_plan list;
  mutable consumed_owned_root_scalar_plans : unit ref list;
  mutable owned_contents_lineages : owned_contents_lineage list;
  mutable owned_contents_candidates : owned_contents_candidate list;
  mutable owned_contents_permits : owned_contents_permit list;
  mutable owned_contents_manifests : owned_contents_manifest list;
  mutable owned_contents_receipts : owned_contents_receipt list;
  mutable issued_owned_contents_origins : owned_contents_origin list;
  mutable retired_owned_contents_origins : owned_contents_origin list;
  owned_contents_counters : mutable_owned_contents_counters;
  mutable previewed_transition_predecessors :
    transition_predecessor_capability list;
  mutable previewed_transition_predecessor_seals : (unit ref * string) list;
  mutable active_transition_predecessors :
    transition_predecessor_capability list;
  mutable consumed_transition_predecessors :
    transition_predecessor_capability list;
  mutable receipted_transition_predecessors : unit ref list;
  mutable recursive_spec_preservation :
    Recursive_spec_preservation.capability list;
  mutable frozen_constructor_manifests : frozen_constructor_manifest list;
  mutable frozen_constructor_templates : frozen_constructor_template list;
  mutable frozen_conditional_scopes : frozen_conditional_scope list;
  mutable frozen_constructor_result_instances :
    frozen_constructor_result_instance list;
  mutable frozen_call_discharges : frozen_call_discharge list;
  mutable shared_heap_teardowns : (unit -> unit) list;
  finite_candidate_lifecycle : Direct_candidate.t;
  finite_registry : Finite_value_registry.t;
  counters : mutable_counters;
}

let finite_result_lifecycle_counter_observer = ref None
let owned_root_scalar_counter_observer :
    (mutable_counters -> unit) option ref =
  ref None
let transition_predecessor_counter_observer :
    (mutable_counters -> unit) option ref =
  ref None

type call_instance = {
  token : unit ref;
  session : unit ref;
  caller : Sst.function_id;
  call_span : Diagnostic.span;
  path_digest : string;
  receipt : receipt;
  result_symbol : Vir.symbol;
}

type consumed_fact = {
  call_instance : call_instance;
  closed_fact : Vir.boolean_term;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let private_issuer = ref ()
let transition_predecessor_affinity = ref ()
let transition_predecessor_attack_for_testing = ref None
let transition_predecessor_double_consume_for_testing = ref false
let transition_predecessor_replay_for_testing :
    transition_predecessor_capability option ref =
  ref None
let local_assertion_instance_issuer = ref ()
let direct_exec_local_assertion_scope_issuer = ref ()
let owned_root_scalar_plan_issuer = ref ()
let owned_root_scalar_plan_attack_for_testing = ref None
let owned_contents_issuer = ref ()
let owned_contents_origin_issuer = ref ()
let owned_contents_attack_for_testing = ref None
let owned_contents_counter_observer :
    (owned_contents_counters -> unit) option ref =
  ref None
let owned_contents_seed_observer : (t -> unit) option ref = ref None
let owned_contents_seed_completion_observer :
    (owned_contents_manifest ->
    Vir.function_execution ->
    Solver_backend.obligation_result list ->
    unit)
    option
    ref =
  ref None
let local_assertion_instance_attack_for_testing = ref None
let proof_activation_batch_attack_for_testing = ref None
let observed_local_assertion_instances_issued = ref 0
let observed_local_assertion_instances_consumed = ref 0
let observed_direct_exec_local_assertion_scopes_issued = ref 0
let observed_direct_exec_local_assertion_scopes_closed = ref 0
let observed_proof_activation_routes_consumed = ref 0
let ground_constructor_match_issuer = ref ()
let ground_constructor_match_attack_for_testing = ref None
let finite_result_snapshot_attack_for_testing = ref None
let finite_result_call_instance_attack_for_testing = ref None
let finite_result_double_consume_for_testing = ref false
let finite_result_call_observation_events = ref None
let proof_activation_observation_events = ref None
let suppress_recursive_spec_preservation_for_testing = ref false
let suppress_recursive_spec_result_issuance_for_testing = ref false
let suppress_recursive_spec_construction_route_for_testing = ref false
let digest value = Digest.string value |> Digest.to_hex

let span_string (span : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" span.file span.start_pos.line
    span.start_pos.column span.end_pos.line span.end_pos.column

let mode_name = function
  | Sst.Exec -> "Exec"
  | Sst.Proof -> "Proof"
  | Sst.Spec -> "Spec"

let instance_mode_name = function
  | Sst.Exec_instance -> "Exec"
  | Sst.Tracked_instance -> "Tracked"
  | Sst.Ghost_instance -> "Ghost"

let function_id_string (id : Sst.function_id) =
  Printf.sprintf "%s#%d" id.function_name id.function_index

let function_ref_string (reference : Vir.function_ref) =
  Printf.sprintf "%s#%d" reference.function_name reference.function_index

let type_id_string (id : Sst.type_id) =
  Printf.sprintf "%s#%d" id.type_name id.type_index

let program_fingerprint identity =
  String.concat "\000"
    [
      identity.unit_identity;
      identity.cmt_identity;
      identity.family_identity;
      identity.interface_digest;
      identity.source_digest;
      identity.program_snapshot;
      identity.signature_snapshot;
    ]
  |> digest

let finite_program_identity identity : Finite_value_registry.program_identity =
  {
    unit_identity = identity.unit_identity;
    cmt_identity = identity.cmt_identity;
    family_identity = identity.family_identity;
    program_snapshot = identity.program_snapshot;
  }

let provenance_string = function
  | Sst.Authenticated_typedtree { source_file; declaration_span } ->
      "typedtree:" ^ source_file ^ ":" ^ span_string declaration_span
  | Sst.Raw_semantic_body span -> "raw:" ^ span_string span

let invariant_fingerprint invariant =
  String.concat "\000"
    [
      invariant.invariant_id;
      type_id_string invariant.abstract_type;
      invariant.certificate_id;
      function_id_string invariant.model;
      Sst.string_of_type invariant.model_type;
      function_id_string invariant.predicate;
      invariant.predicate_digest;
    ]
  |> digest

let callee_snapshot_fingerprint snapshot =
  String.concat "\000"
    [
      program_fingerprint snapshot.program;
      snapshot.resolved_path;
      snapshot.binding_uid;
      snapshot.callable_key;
      function_id_string snapshot.callee;
      snapshot.body_snapshot;
      provenance_string snapshot.body_provenance;
      mode_name snapshot.mode;
      instance_mode_name snapshot.result_mode;
      Sst.string_of_type snapshot.result_type;
      invariant_fingerprint snapshot.invariant;
    ]
  |> digest

let rank_fingerprint (rank : Finite_value_registry.rank_snapshot) =
  String.concat "\000"
    [
      rank.domain_id;
      rank.domain_version;
      rank.domain_digest;
      String.concat "|" rank.component_snapshot;
      String.concat "|" rank.profile_actual_snapshot;
    ]
  |> digest

let finite_result_snapshot_core_fingerprint snapshot =
  String.concat "\000"
    [
      program_fingerprint snapshot.finite_program;
      snapshot.finite_resolved_path;
      snapshot.finite_binding_uid;
      snapshot.finite_callable_key;
      function_id_string snapshot.finite_callee;
      snapshot.finite_body_snapshot;
      provenance_string snapshot.finite_body_provenance;
      mode_name snapshot.finite_mode;
      instance_mode_name snapshot.finite_result_mode;
      Sst.string_of_type snapshot.finite_result_type;
      rank_fingerprint snapshot.finite_rank;
    ]
  |> digest

let finite_result_snapshot_fingerprint snapshot =
  String.concat "\000"
    [
      finite_result_snapshot_core_fingerprint snapshot;
      Direct_candidate.callable snapshot.finite_candidate;
    ]
  |> digest

let program_identity ~implementation ~validated =
  let program = Sst_validation.program validated in
  let interface_digest =
    Option.value ~default:"missing-interface-digest"
      implementation.Cmt_input.interface_digest
  in
  let source_digest =
    Option.value ~default:"missing-source-digest"
      implementation.Cmt_input.source_digest
  in
  let family_identity =
    match implementation.Cmt_input.interface_family_markers with
    | [] -> "ordinary-or-implicit"
    | markers -> String.concat "," markers
  in
  let program_snapshot = Sst.to_string program |> digest in
  let signature_snapshot =
    let modes =
      implementation.Cmt_input.interface_mode_signatures
    |> List.map (fun (path, signature) ->
           path ^ "=" ^ Option.value ~default:"<missing>" signature)
      |> String.concat "\000"
    in
    let finite =
      implementation.Cmt_input.interface_finite_signatures
      |> List.map (fun (path, signature) ->
             path ^ "=" ^ Option.value ~default:"<missing>" signature)
      |> String.concat "\000"
    in
    String.concat "\000"
      [
        implementation.unit_name;
        interface_digest;
        source_digest;
        family_identity;
        program_snapshot;
        modes;
        finite;
      ]
    |> digest
  in
  {
    unit_identity = implementation.unit_name ^ ":" ^ interface_digest;
    cmt_identity =
      String.concat ":"
        [ implementation.filename; source_digest; program_snapshot ];
    family_identity;
    interface_digest;
    source_digest;
    program_snapshot;
    signature_snapshot;
  }

let empty_counters () =
  {
    callee_solver_attempts = 0;
    callee_verified_results = 0;
    callee_failed_results = 0;
    dependent_lowerings = 0;
    dependent_backend_contexts = 0;
    dependent_solver_attempts = 0;
    receipts_issued = 0;
    receipts_consumed = 0;
    finite_result_manifests = 0;
    finite_result_completions = 0;
    finite_result_consumption_attempts = 0;
    local_assertion_instances_issued = 0;
    local_assertion_instances_consumed = 0;
    owned_root_scalar_plans_issued = 0;
    owned_root_scalar_plans_consumed = 0;
    owned_root_scalar_plans_rejected = 0;
    owned_root_scalar_observations = 0;
    owned_root_scalar_equations = 0;
    owned_root_scalar_bridge_paths = 0;
    owned_root_scalar_bridge_equations = 0;
    owned_root_scalar_fresh_successors = 0;
    owned_root_scalar_reconstructions = 0;
    transition_predecessor_transfers = 0;
    transition_predecessor_consumptions = 0;
    transition_result_receipts = 0;
    transition_teardown_removals = 0;
    transition_preservation_obligations = 0;
    transition_nested_reconstructions = 0;
    transition_root_reconstructions = 0;
    shared_heap_issuances = 0;
    shared_heap_writes = 0;
    shared_heap_read_logs = 0;
    shared_heap_epoch_advances = 0;
    shared_heap_teardowns = 0;
    invariant_cell_entry_eligibilities = 0;
    invariant_cell_constructor_eligibilities = 0;
    invariant_cell_closed_initializations = 0;
    invariant_cell_opens = 0;
    invariant_cell_updates = 0;
    invariant_cell_closes = 0;
    invariant_cell_effect_instantiations = 0;
    invariant_cell_terminal_reads = 0;
    invariant_cell_teardowns = 0;
    frozen_constructor_template_issuances = 0;
    frozen_constructor_template_teardowns = 0;
    frozen_conditional_scope_issuances = 0;
    frozen_conditional_scope_teardowns = 0;
    frozen_result_instance_issuances = 0;
    frozen_result_instance_teardowns = 0;
    frozen_call_discharge_issuances = 0;
    frozen_call_discharge_consumptions = 0;
    frozen_call_discharge_teardowns = 0;
    frozen_descent_witness_issuances = 0;
    frozen_descent_witness_consumptions = 0;
    frozen_descent_witness_teardowns = 0;
    frozen_observation_consumptions = 0;
    frozen_observation_teardowns = 0;
  }

let empty_owned_contents_counters () =
  {
    lineages_opened = 0;
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

let create ~imports ~implementation ~validated ~invariants =
  let* callable_identities =
    Verification_identity.create ~imports ~implementation ~validated
  in
  let identity = program_identity ~implementation ~validated in
  let session = ref () in
  let* () =
    Sst_callback_private.authenticate_implementation ~implementation ~session
      (Sst_validation.program validated)
  in
  let* () =
    match imports with
    | None -> Ok ()
    | Some registration ->
        Imported_callable.begin_consumer_session registration ~session
  in
  Ok
    {
      issuer = private_issuer;
      session;
      imported_registration = imports;
      identity;
      active = true;
      validated = Some validated;
      invariants = Some invariants;
      callable_identities = Some callable_identities;
      authorized_callees = [];
      obligation_manifests = [];
      verified_completions = [];
      receipts = [];
      consumed_result_symbols = [];
      consumed_result_routes = [];
      finite_result_snapshots = [];
      finite_result_manifests = [];
      finite_result_completions = [];
      finite_consumed_call_instances = [];
      proof_activation_authorities = [];
      proof_activation_manifests = [];
      consumed_proof_activation_batches = [];
      consumed_proof_activation_manifests = [];
      finalized_proof_activation_snapshots = [];
      issued_ground_constructor_matches = [];
      active_direct_exec_local_assertion_scope = None;
      issued_local_assertion_instances = [];
      consumed_local_assertion_instances = [];
      issued_owned_root_scalar_plans = [];
      consumed_owned_root_scalar_plans = [];
      owned_contents_lineages = [];
      owned_contents_candidates = [];
      owned_contents_permits = [];
      owned_contents_manifests = [];
      owned_contents_receipts = [];
      issued_owned_contents_origins = [];
      retired_owned_contents_origins = [];
      owned_contents_counters = empty_owned_contents_counters ();
      previewed_transition_predecessors = [];
      previewed_transition_predecessor_seals = [];
      active_transition_predecessors = [];
      consumed_transition_predecessors = [];
      receipted_transition_predecessors = [];
      recursive_spec_preservation = [];
      frozen_constructor_manifests = [];
      frozen_constructor_templates = [];
      frozen_conditional_scopes = [];
      frozen_constructor_result_instances = [];
      frozen_call_discharges = [];
      shared_heap_teardowns = [];
      finite_candidate_lifecycle = Direct_candidate.create ~session;
      finite_registry =
        Finite_value_registry.create (finite_program_identity identity)
          ~session
          ~types:(Sst_validation.program validated).Sst.types;
      counters = empty_counters ();
    }

let destroy session =
  List.iter (fun teardown -> teardown ()) session.shared_heap_teardowns;
  session.shared_heap_teardowns <- [];
  Option.iter
    (fun validated ->
      let program = Sst_validation.program validated in
      Broadcast_vc_private.clear_program program;
      Broadcast_declaration_private.destroy program;
      Symbolic_declaration_private.destroy program)
    session.validated;
  Option.iter (fun observe -> observe session)
    !owned_contents_seed_observer;
  List.iter
    (fun lineage ->
      if
        not lineage.contents_lineage_closed
        && not lineage.contents_lineage_invalidated
      then (
        lineage.contents_lineage_invalidated <- true;
        session.owned_contents_counters.lineages_invalidated <-
          session.owned_contents_counters.lineages_invalidated + 1;
        List.iter
          (fun candidate ->
            if not candidate.contents_candidate_finalized then
              candidate.contents_candidate_retired <- true)
          lineage.contents_lineage_candidates;
        List.iter
          (fun permit ->
            if
              permit.contents_permit_lineage == lineage
              && not permit.contents_permit_retired
            then (
              permit.contents_permit_retired <- true;
              session.owned_contents_counters.permit_retirements <-
                session.owned_contents_counters.permit_retirements + 1))
          session.owned_contents_permits))
    session.owned_contents_lineages;
  session.counters.frozen_constructor_template_teardowns <-
    session.counters.frozen_constructor_template_teardowns
    + List.length session.frozen_constructor_templates;
  session.counters.frozen_conditional_scope_teardowns <-
    session.counters.frozen_conditional_scope_teardowns
    + List.length session.frozen_conditional_scopes;
  session.counters.frozen_result_instance_teardowns <-
    session.counters.frozen_result_instance_teardowns
    + List.length session.frozen_constructor_result_instances;
  session.counters.frozen_call_discharge_teardowns <-
    session.counters.frozen_call_discharge_teardowns
    + List.length session.frozen_call_discharges;
  session.counters.frozen_descent_witness_teardowns <-
    session.counters.frozen_descent_witness_teardowns
    + List.fold_left
        (fun count discharge ->
          count + List.length discharge.frozen_discharge_observed_epochs)
        0 session.frozen_call_discharges;
  session.counters.frozen_observation_teardowns <-
    session.counters.frozen_observation_teardowns
    + List.fold_left
        (fun count discharge ->
          count + List.length discharge.frozen_discharge_observed_epochs)
        0 session.frozen_call_discharges;
  Option.iter
    (fun observe -> observe session.counters)
    !finite_result_lifecycle_counter_observer;
  Option.iter
    (fun observe -> observe session.counters)
    !owned_root_scalar_counter_observer;
  session.owned_contents_counters.candidate_teardowns <-
    session.owned_contents_counters.candidate_teardowns
    + List.length session.owned_contents_candidates;
  session.owned_contents_counters.lineage_teardowns <-
    session.owned_contents_counters.lineage_teardowns
    + List.length session.owned_contents_lineages;
  session.owned_contents_counters.permit_teardowns <-
    session.owned_contents_counters.permit_teardowns
    + List.length session.owned_contents_permits;
  session.owned_contents_counters.receipt_teardowns <-
    session.owned_contents_counters.receipt_teardowns
    + List.length session.owned_contents_receipts;
  Option.iter
    (fun observe ->
      let counters = session.owned_contents_counters in
      observe
        ({
          lineages_opened = counters.lineages_opened;
          lineage_graph_authentications =
            counters.lineage_graph_authentications;
          mapped_candidates_authenticated =
            counters.mapped_candidates_authenticated;
          lineages_invalidated = counters.lineages_invalidated;
          lineages_closed = counters.lineages_closed;
          candidates_issued = counters.candidates_issued;
          candidates_consumed = counters.candidates_consumed;
          candidates_rejected = counters.candidates_rejected;
          candidates_finished = counters.candidates_finished;
          permits_issued = counters.permits_issued;
          permits_consumed = counters.permits_consumed;
          permits_rejected = counters.permits_rejected;
          recursive_routes = counters.recursive_routes;
          ground_equations = counters.ground_equations;
          model_results = counters.model_results;
          manifests_issued = counters.manifests_issued;
          completions = counters.completions;
          receipts_finalized = counters.receipts_finalized;
          successor_receipts = counters.successor_receipts;
          predecessor_retirements = counters.predecessor_retirements;
          permit_retirements = counters.permit_retirements;
          lineage_teardowns = counters.lineage_teardowns;
          candidate_teardowns = counters.candidate_teardowns;
          permit_teardowns = counters.permit_teardowns;
          receipt_teardowns = counters.receipt_teardowns;
        }
          : owned_contents_counters))
    !owned_contents_counter_observer;
  let transition_tokens =
    session.previewed_transition_predecessors
    @ session.active_transition_predecessors
    @ session.consumed_transition_predecessors
    |> List.map (fun capability -> capability.predecessor_token)
    |> List.fold_left
         (fun tokens token ->
           if List.memq token tokens then tokens else token :: tokens)
         []
  in
  session.counters.transition_teardown_removals <-
    session.counters.transition_teardown_removals
    + List.length transition_tokens;
  Option.iter
    (fun observe -> observe session.counters)
    !transition_predecessor_counter_observer;
  session.validated <- None;
  session.invariants <- None;
  session.callable_identities <- None;
  session.authorized_callees <- [];
  session.obligation_manifests <- [];
  session.verified_completions <- [];
  session.receipts <- [];
  session.consumed_result_symbols <- [];
  session.consumed_result_routes <- [];
  session.finite_result_snapshots <- [];
  session.finite_result_manifests <- [];
  session.finite_result_completions <- [];
  session.finite_consumed_call_instances <- [];
  session.proof_activation_authorities <- [];
  session.proof_activation_manifests <- [];
  session.consumed_proof_activation_batches <- [];
  session.consumed_proof_activation_manifests <- [];
  session.finalized_proof_activation_snapshots <- [];
  session.issued_ground_constructor_matches <- [];
  Option.iter
    (fun scope ->
      scope.direct_scope_active <- false;
      incr observed_direct_exec_local_assertion_scopes_closed)
    session.active_direct_exec_local_assertion_scope;
  session.active_direct_exec_local_assertion_scope <- None;
  session.issued_local_assertion_instances <- [];
  session.consumed_local_assertion_instances <- [];
  session.issued_owned_root_scalar_plans <- [];
  session.consumed_owned_root_scalar_plans <- [];
  session.owned_contents_lineages <- [];
  session.owned_contents_candidates <- [];
  session.owned_contents_permits <- [];
  session.owned_contents_manifests <- [];
  session.owned_contents_receipts <- [];
  session.issued_owned_contents_origins <- [];
  session.retired_owned_contents_origins <- [];
  session.previewed_transition_predecessors <- [];
  session.previewed_transition_predecessor_seals <- [];
  session.active_transition_predecessors <- [];
  session.consumed_transition_predecessors <- [];
  session.receipted_transition_predecessors <- [];
  session.recursive_spec_preservation <- [];
  session.frozen_constructor_manifests <- [];
  session.frozen_constructor_templates <- [];
  session.frozen_conditional_scopes <- [];
  session.frozen_constructor_result_instances <- [];
  session.frozen_call_discharges <- [];
  Direct_candidate.destroy session.finite_candidate_lifecycle;
  Finite_value_registry.destroy session.finite_registry;
  Option.iter
    (fun registration ->
      Imported_callable.end_consumer_session registration
        ~session:session.session)
    session.imported_registration;
  session.active <- false

let is_active session = session.active
let shared_heap_session_token (session : t) = session.session

let register_shared_heap_teardown (session : t) teardown =
  if session.active then
    session.shared_heap_teardowns <-
      teardown :: session.shared_heap_teardowns
  else invalid_arg "cannot register shared heap teardown on destroyed session"

let program_snapshot_digest session = session.identity.program_snapshot
let signature_snapshot_digest session = session.identity.signature_snapshot

let canonical_callable_key session (definition : Sst.function_definition) =
  let* identities =
    match session.callable_identities with
    | Some identities when session.active -> Ok identities
    | Some _ | None -> Error "verification session is destroyed"
  in
  let* callable = Verification_identity.find identities definition in
  Ok
    (String.concat "\000"
       [
         session.identity.unit_identity;
         Verification_identity.canonical_path_component callable;
         session.identity.signature_snapshot;
         string_of_int definition.function_id.function_index;
       ])

let canonical_callable_resolved_path session definition =
  let* identities =
    match session.callable_identities with
    | Some identities when session.active -> Ok identities
    | Some _ | None -> Error "verification session is destroyed"
  in
  let* callable = Verification_identity.find identities definition in
  Ok (Verification_identity.resolved_path callable)

let canonical_callable_binding_uid session definition =
  let* identities =
    match session.callable_identities with
    | Some identities when session.active -> Ok identities
    | Some _ | None -> Error "verification session is destroyed"
  in
  let* callable = Verification_identity.find identities definition in
  Ok (Verification_identity.binding_uid callable)

let canonical_callable_leaf_name session definition =
  let* identities =
    match session.callable_identities with
    | Some identities when session.active -> Ok identities
    | Some _ | None -> Error "verification session is destroyed"
  in
  let* callable = Verification_identity.find identities definition in
  Ok (Verification_identity.leaf_name callable)

let invariant_snapshot handle =
  {
    invariant_id = Type_invariant.invariant_id handle;
    abstract_type = Type_invariant.abstract_type handle;
    certificate_id = Type_invariant.certificate_id handle;
    model = Type_invariant.model_callable handle;
    model_type = Type_invariant.model_snapshot_type handle;
    predicate = Type_invariant.predicate_callable handle;
    predicate_digest = Type_invariant.predicate_digest handle;
  }

let body_snapshot program definition =
  Sst.to_string { program with Sst.functions = [ definition ] } |> digest

let callable_body_snapshot session definition =
  if session.active then
    Marshal.to_string definition [ Marshal.No_sharing ] |> digest
  else ""

let frozen_descriptor_for_constructor session
    (definition : Sst.function_definition) =
  match session.validated with
  | Some validated when session.active ->
      (Sst_validation.program validated).Sst.types
      |> List.find_map (fun (type_definition : Sst.type_definition) ->
             match type_definition.representation with
             | Sst.Abstract_with_evidence
                 (Sst.Authenticated_same_cmt_abstraction evidence) -> (
                 match Sst.frozen_spine_prerequisite evidence with
                 | Some frozen
                   when
                     frozen.frozen_constructor = definition.function_id
                     && frozen.frozen_root = type_definition.type_id ->
                     Some frozen
                 | Some _ | None -> None)
             | Sst.Revealed
             | Sst.Abstract_with_evidence
                 (Sst.Incomplete_abstraction_evidence _
                 | Sst.Proposed_same_cmt_abstraction _) ->
                 None)
  | Some _ | None -> None

let vir_type (type_id : Sst.type_id) =
  {
    Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }

let frozen_field_selector (field : Sst.field_id) range =
  let owner =
    match field.field_owner with
    | Sst.Record_owner owner -> owner
    | Sst.Constructor_owner constructor -> constructor.constructor_type
  in
  {
    Vir.selector_domain = vir_type owner;
    selector_range = range;
    selector_namespace =
      (match field.field_owner with
      | Sst.Record_owner owner ->
          Printf.sprintf "t%d_%s_record" owner.type_index owner.type_name
      | Sst.Constructor_owner constructor ->
          Printf.sprintf "t%d_%s_c%d_%s_inline"
            constructor.constructor_type.type_index
            constructor.constructor_type.type_name
            constructor.constructor_index constructor.constructor_name);
    selector_index = field.field_index;
    selector_name = field.field_name;
    selector_path = [];
  }

let frozen_argument_selector (constructor : Sst.constructor_id) index range =
  {
    Vir.selector_domain = vir_type constructor.constructor_type;
    selector_range = range;
    selector_namespace =
      Printf.sprintf "t%d_%s_c%d_%s"
        constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name;
    selector_index = index;
    selector_name = Printf.sprintf "$arg%d" index;
    selector_path = [];
  }

let frozen_child_and_terminal frozen root =
  let link_type = vir_type frozen.Sst.frozen_link in
  let root_type = vir_type frozen.frozen_root in
  let edge =
    {
      Vir.aggregate_type = link_type;
      aggregate_desc =
        Vir.Aggregate_selector
          ( frozen_field_selector frozen.frozen_edge_field
              (Vir.Aggregate link_type),
            root );
    }
  in
  let child =
    {
      Vir.aggregate_type = root_type;
      aggregate_desc =
        Vir.Aggregate_selector
          ( frozen_argument_selector frozen.frozen_next_constructor 0
              (Vir.Aggregate root_type),
            edge );
    }
  in
  let terminal_edge =
    {
      Vir.aggregate_type = link_type;
      aggregate_desc =
        Vir.Aggregate_selector
          ( frozen_field_selector frozen.frozen_edge_field
              (Vir.Aggregate link_type),
            child );
    }
  in
  (child, terminal_edge)

let frozen_execution_result (execution : Vir.function_execution) =
  execution.exits
  |> List.filter_map (fun exit ->
         match exit.Vir.result with
         | Vir.Aggregate_result symbol -> Some symbol
         | Vir.Unit_result | Vir.Integer_result _ | Vir.Boolean_result _
         | Vir.Parametric_result _ | Vir.Tuple_result _ ->
             None)
  |> List.sort_uniq compare
  |> function
  | [ symbol ] ->
      Some
        {
          Vir.aggregate_type =
            (match symbol.Vir.sort with
            | Vir.Aggregate aggregate_type -> aggregate_type
            | Vir.Integer | Vir.Boolean | Vir.Parametric _ -> assert false);
          aggregate_desc = Vir.Aggregate_symbol symbol;
        }
  | [] | _ :: _ :: _ -> None

let frozen_execution_path_digest execution =
  Marshal.to_string
    (List.map
       (fun (exit : Vir.exit) ->
         (exit.path_condition, exit.assumptions, exit.result))
       execution.Vir.exits)
    [ Marshal.No_sharing ]
  |> digest

let frozen_constructor_obligation_digest obligation =
  Marshal.to_string obligation [ Marshal.No_sharing ] |> digest

let frozen_constructor_obligation_roles definition frozen obligations =
  let postconditions, invariants =
    List.fold_left
      (fun (postconditions, invariants) (obligation : Vir.obligation) ->
        match obligation.kind with
        | Vir.Postcondition
            {
              postcondition_ordinal = 0;
              declaration_span;
            } -> (
            match definition.Sst.contracts.ensures with
            | [ ensure ] when declaration_span = ensure.span ->
                ( frozen_constructor_obligation_digest obligation
                  :: postconditions,
                  invariants )
            | [] | _ :: _ -> (postconditions, invariants))
        | Vir.Invariant_validity
            {
              abstract_type;
              model;
              predicate;
              operation;
              boundary = Vir.Constructor_establishment;
              _;
            }
          when
            abstract_type = vir_type frozen.Sst.frozen_root
            && model.function_index = frozen.frozen_model.function_index
            && String.equal model.function_name frozen.frozen_model.function_name
            && predicate.function_index
               = frozen.frozen_invariant.function_index
            && String.equal predicate.function_name
                 frozen.frozen_invariant.function_name
            && operation.function_index
               = frozen.frozen_constructor.function_index
            && String.equal operation.function_name
                 frozen.frozen_constructor.function_name ->
            ( postconditions,
              frozen_constructor_obligation_digest obligation :: invariants )
        | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Local_assertion _
        | Vir.Postcondition _ | Vir.Call_precondition _
        | Vir.Callback_precondition _ | Vir.Invariant_validity _ | Vir.Entry_measure_nonnegative _
        | Vir.Recursive_call_measure_nonnegative _
        | Vir.Recursive_call_strict_descent _ ->
            (postconditions, invariants))
      ([], []) obligations
  in
  (List.rev postconditions, List.rev invariants)

let authorize_frozen_constructor_obligations session
    (definition : Sst.function_definition)
    (execution : Vir.function_execution) =
  match frozen_descriptor_for_constructor session definition with
  | None when session.active -> Ok None
  | None -> Error "verification session is destroyed"
  | Some frozen ->
      let obligations =
        List.map frozen_constructor_obligation_digest execution.obligations
      in
      let postconditions, invariants =
        frozen_constructor_obligation_roles definition frozen
          execution.obligations
      in
      if
        List.exists
          (fun (obligation : Vir.obligation) ->
            obligation.function_ref.function_index
            <> definition.function_id.function_index
            || not
                 (String.equal obligation.function_ref.function_name
                    definition.function_id.function_name))
          execution.obligations
      then
        Error
          "frozen-spine constructor manifest contains a foreign obligation"
      else if postconditions = [] || invariants = [] then
        Error
          "frozen-spine constructor manifest lacks exact contents postcondition or invariant obligations"
      else
        let manifest =
          {
            frozen_constructor_manifest_issuer = private_issuer;
            frozen_constructor_manifest_session = session.session;
            frozen_constructor_manifest_token = ref ();
            frozen_constructor_manifest_program = session.identity;
            frozen_constructor_manifest_descriptor = frozen;
            frozen_constructor_manifest_definition = definition;
            frozen_constructor_manifest_body =
              callable_body_snapshot session definition;
            frozen_constructor_manifest_obligations = obligations;
            frozen_constructor_manifest_postconditions = postconditions;
            frozen_constructor_manifest_invariants = invariants;
          }
        in
        session.frozen_constructor_manifests <-
          manifest :: session.frozen_constructor_manifests;
        Ok (Some manifest)

let finalize_frozen_constructor session
    (definition : Sst.function_definition)
    (execution : Vir.function_execution) manifest results =
  match frozen_descriptor_for_constructor session definition with
  | None when session.active -> Ok ()
  | None -> Error "verification session is destroyed"
  | Some frozen ->
      let* manifest =
        match manifest with
        | Some (manifest : frozen_constructor_manifest)
          when
            manifest.frozen_constructor_manifest_issuer == private_issuer
            && manifest.frozen_constructor_manifest_session == session.session
            && List.exists
                 (fun (candidate : frozen_constructor_manifest) ->
                   candidate.frozen_constructor_manifest_token
                   == manifest.frozen_constructor_manifest_token)
                 session.frozen_constructor_manifests
            && String.equal
                 (program_fingerprint
                    manifest.frozen_constructor_manifest_program)
                 (program_fingerprint session.identity)
            && manifest.frozen_constructor_manifest_descriptor = frozen
            && manifest.frozen_constructor_manifest_definition.function_id
               = definition.function_id
            && String.equal manifest.frozen_constructor_manifest_body
                 (callable_body_snapshot session definition) ->
            Ok manifest
        | Some _ | None ->
            Error
              "frozen-spine constructor lacks an authenticated obligation manifest"
      in
      let* () =
        if
          manifest.frozen_constructor_manifest_obligations
          = List.map frozen_constructor_obligation_digest execution.obligations
          && List.length results = List.length execution.obligations
          && List.for_all2
               (fun (result : Solver_backend.obligation_result) obligation ->
                 result.obligation = obligation
                 && result.outcome = Solver_backend.Verified)
               results execution.obligations
        then Ok ()
        else
          Error
            "frozen-spine constructor obligation manifest is incomplete or unverified"
      in
      let postconditions =
        manifest.frozen_constructor_manifest_postconditions
      in
      let invariants = manifest.frozen_constructor_manifest_invariants in
      let* root =
        match frozen_execution_result execution with
        | Some root
          when root.aggregate_type = vir_type frozen.frozen_root ->
            Ok root
        | Some _ | None ->
            Error
              "frozen-spine constructor completion has no exact aggregate result"
      in
      let* callable = canonical_callable_key session definition in
      let* resolved_path =
        canonical_callable_resolved_path session definition
      in
      let* binding_uid =
        canonical_callable_binding_uid session definition
      in
      let child, terminal_edge = frozen_child_and_terminal frozen root in
      let template =
        {
          frozen_template_issuer = private_issuer;
          frozen_template_session = session.session;
          frozen_template_token = ref ();
          frozen_template_program = session.identity;
          frozen_template_descriptor = frozen;
          frozen_template_definition = definition;
          frozen_template_callable = callable;
          frozen_template_resolved_path = resolved_path;
          frozen_template_binding_uid = binding_uid;
          frozen_template_body = callable_body_snapshot session definition;
          frozen_template_definition_result = root;
          frozen_template_definition_child = child;
          frozen_template_definition_terminal_edge = terminal_edge;
          frozen_template_result_path =
            frozen_execution_path_digest execution;
          frozen_template_obligation_set =
            (String.concat "\000"
               manifest.frozen_constructor_manifest_obligations
            |> digest);
          frozen_template_postconditions = postconditions;
          frozen_template_invariants = invariants;
          frozen_template_structural_epoch = 0;
        }
      in
      if
        List.exists
          (fun existing ->
            existing.frozen_template_descriptor = frozen
            && existing.frozen_template_definition_result = root
            && String.equal existing.frozen_template_result_path
                 template.frozen_template_result_path)
          session.frozen_constructor_templates
      then Error "frozen-spine constructor template was replayed"
      else (
        session.frozen_constructor_templates <-
          template :: session.frozen_constructor_templates;
        session.frozen_constructor_manifests <-
          List.filter
            (fun (candidate : frozen_constructor_manifest) ->
              candidate.frozen_constructor_manifest_token
              != manifest.frozen_constructor_manifest_token)
            session.frozen_constructor_manifests;
        session.counters.frozen_constructor_template_issuances <-
          session.counters.frozen_constructor_template_issuances + 1;
        Ok ())

let same_frozen_function_id (left : Sst.function_id)
    (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let exact_frozen_template (session : t) frozen =
  List.filter
    (fun template ->
      template.frozen_template_issuer == private_issuer
      && template.frozen_template_session == session.session
      && template.frozen_template_token != private_issuer
      && String.equal
           (program_fingerprint template.frozen_template_program)
           (program_fingerprint session.identity)
      && template.frozen_template_descriptor = frozen
      && same_frozen_function_id
           template.frozen_template_definition.function_id
           frozen.frozen_constructor
      && template.frozen_template_callable <> ""
      && template.frozen_template_resolved_path <> ""
      && template.frozen_template_binding_uid <> ""
      && String.equal template.frozen_template_body
           (callable_body_snapshot session template.frozen_template_definition)
      && template.frozen_template_definition_child
         = fst
             (frozen_child_and_terminal frozen
                template.frozen_template_definition_result)
      && template.frozen_template_definition_terminal_edge
         = snd
             (frozen_child_and_terminal frozen
                template.frozen_template_definition_result)
      && template.frozen_template_obligation_set <> ""
      && template.frozen_template_postconditions <> []
      && template.frozen_template_invariants <> []
      && template.frozen_template_structural_epoch = 0)
    session.frozen_constructor_templates

let frozen_descriptor_for_type (session : t) type_id =
  match session.validated with
  | Some validated when session.active ->
      (Sst_validation.program validated).Sst.types
      |> List.find_map (fun (definition : Sst.type_definition) ->
             if definition.type_id <> type_id then None
             else
               match definition.representation with
               | Sst.Abstract_with_evidence
                   (Sst.Authenticated_same_cmt_abstraction evidence) ->
                   Sst.frozen_spine_prerequisite evidence
               | Sst.Revealed
               | Sst.Abstract_with_evidence
                   (Sst.Incomplete_abstraction_evidence _
                   | Sst.Proposed_same_cmt_abstraction _) ->
                   None)
  | Some _ | None -> None

let register_frozen_formal_scope (session : t)
    ~(definition : Sst.function_definition) ~(formal : Sst.binding)
    ~ordinal ~(root : Vir.aggregate_term) ~path_condition =
  if not session.active then Error "verification session is destroyed"
  else
    let* frozen =
      match formal.typ with
      | Sst.Aggregate type_id -> (
          match frozen_descriptor_for_type session type_id with
          | Some frozen
            when root.aggregate_type = vir_type frozen.frozen_root ->
              Ok frozen
          | Some _ | None ->
              Error
                "frozen-spine conditional formal has no exact same-CMT descriptor")
      | Sst.Unit | Sst.Int | Sst.Bool | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          Error
            "frozen-spine conditional formal has no exact same-CMT descriptor"
    in
    let* template =
      match exact_frozen_template session frozen with
      | [ template ] -> Ok template
      | [] ->
          Error
            "frozen-spine conditional formal has no completed constructor template"
      | _ :: _ :: _ ->
          Error "frozen-spine constructor template is ambiguous"
    in
    let* () =
      match root.aggregate_desc with
      | Vir.Aggregate_symbol { role = Vir.Input; _ } -> Ok ()
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
      | Vir.Aggregate_conditional _
      | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _
      | Vir.Aggregate_symbolic_application _ ->
          Error
            "frozen-spine conditional formal is not its definition input root"
    in
    let* callable = canonical_callable_key session definition in
    let* resolved_path = canonical_callable_resolved_path session definition in
    let* binding_uid = canonical_callable_binding_uid session definition in
    let path =
      Marshal.to_string
        ( path_condition,
          definition.function_id,
          callable,
          formal,
          ordinal,
          root,
          0 )
        [ Marshal.No_sharing ]
      |> digest
    in
    if
      List.exists
        (fun scope ->
          same_frozen_function_id
            scope.frozen_conditional_definition.function_id
            definition.function_id
          && scope.frozen_conditional_formal = formal
          && scope.frozen_conditional_ordinal = ordinal
          && scope.frozen_conditional_root = root
          && String.equal scope.frozen_conditional_path path)
        session.frozen_conditional_scopes
    then Error "frozen-spine conditional formal scope was replayed"
    else
      let child, terminal_edge = frozen_child_and_terminal frozen root in
      let scope =
        {
          frozen_conditional_issuer = private_issuer;
          frozen_conditional_session = session.session;
          frozen_conditional_token = ref ();
          frozen_conditional_template = template;
          frozen_conditional_definition = definition;
          frozen_conditional_callable = callable;
          frozen_conditional_resolved_path = resolved_path;
          frozen_conditional_binding_uid = binding_uid;
          frozen_conditional_body = callable_body_snapshot session definition;
          frozen_conditional_formal = formal;
          frozen_conditional_ordinal = ordinal;
          frozen_conditional_root = root;
          frozen_conditional_child = child;
          frozen_conditional_terminal_edge = terminal_edge;
          frozen_conditional_path = path;
          frozen_conditional_entry_path_condition = path_condition;
          frozen_conditional_structural_epoch = 0;
          frozen_conditional_verified = false;
          frozen_conditional_observation_permits = [];
        }
      in
      session.frozen_conditional_scopes <-
        scope :: session.frozen_conditional_scopes;
      session.counters.frozen_conditional_scope_issuances <-
        session.counters.frozen_conditional_scope_issuances + 1;
      Ok ()

let finalize_frozen_formal_scopes (session : t)
    (definition : Sst.function_definition) =
  if not session.active then Error "verification session is destroyed"
  else
    let scopes =
      List.filter
        (fun scope ->
          same_frozen_function_id
            scope.frozen_conditional_definition.function_id
            definition.function_id)
        session.frozen_conditional_scopes
    in
    let* callable = canonical_callable_key session definition in
    let* resolved_path =
      canonical_callable_resolved_path session definition
    in
    let* binding_uid =
      canonical_callable_binding_uid session definition
    in
    let body = callable_body_snapshot session definition in
    if
      List.for_all
        (fun scope ->
          scope.frozen_conditional_issuer == private_issuer
          && scope.frozen_conditional_session == session.session
          && scope.frozen_conditional_token != private_issuer
          && String.equal scope.frozen_conditional_callable callable
          && String.equal scope.frozen_conditional_resolved_path resolved_path
          && String.equal scope.frozen_conditional_binding_uid binding_uid
          && String.equal scope.frozen_conditional_body body
          && not scope.frozen_conditional_verified)
        scopes
    then (
      List.iter
        (fun scope -> scope.frozen_conditional_verified <- true)
        scopes;
      Ok ())
    else Error "frozen-spine conditional formal scope is stale or replayed"

let exact_validated_frozen_call (session : t)
    ~(definition : Sst.function_definition)
    ~(callee : Sst.function_definition) ~call_form ~call_path =
  match (session.validated, call_path) with
  | Some validated, _ :: _ when session.active ->
      let rec walk caller = function
        | [] -> Error "frozen-spine call path is empty"
        | span :: rest ->
            let edges =
              Sst_validation.call_edge_descriptors validated
              |> List.filter (fun edge ->
                     same_frozen_function_id
                       (Sst_validation.call_edge_caller edge
                       |> Sst_validation.callable_id)
                       caller
                     && Sst_validation.call_edge_span edge = span
                     && not (Sst_validation.call_edge_recursive edge))
            in
            (match (edges, rest) with
            | [ edge ], [] ->
                let edge_callee =
                  Sst_validation.call_edge_callee edge
                  |> Sst_validation.callable_definition
                in
                if
                  same_frozen_function_id edge_callee.function_id
                    callee.function_id
                  && Sst_validation.call_edge_form edge = call_form
                then
                  Ok
                    ( edge,
                      Sst_validation.call_edge_caller edge
                      |> Sst_validation.callable_definition )
                else Error "frozen-spine call edge/callee binding mismatch"
            | [ edge ], _ :: _
              when
                Sst_validation.call_edge_form edge = Sst.Exec_call
                || Sst_validation.call_edge_form edge
                   = Sst.Specification_call ->
                walk
                  (Sst_validation.call_edge_callee edge
                  |> Sst_validation.callable_id)
                  rest
            | [ _ ], _ :: _ ->
                Error "frozen-spine call path crosses an unsupported summary"
            | [], _ -> Error "frozen-spine call path has no validated edge"
            | _ :: _ :: _, _ ->
                Error "frozen-spine call path edge is ambiguous")
      in
      walk definition.function_id call_path
  | Some _, [] -> Error "frozen-spine call path is empty"
  | Some _, _ :: _ | None, _ -> Error "verification session is destroyed"

let exact_frozen_call_actual edge ordinal formal actual =
  let callee =
    Sst_validation.call_edge_callee edge
    |> Sst_validation.callable_definition
  in
  match
    ( List.nth_opt (Sst_validation.call_edge_actuals edge) ordinal,
      List.nth_opt callee.parameters ordinal )
  with
  | Some row, Some parameter
    when Sst_validation.formal_parameter row = parameter
         && Sst_validation.actual_expression row = actual ->
      (match
         (Sst.require_value_parameter (Sst_validation.formal_parameter row)).Sst.pattern.pattern_desc
       with
      | Sst.Bind binding when binding = formal -> Ok ()
      | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern | Sst.Tuple_pattern _
      | Sst.Record_pattern _ | Sst.Constructor_pattern _ | Sst.Int_pattern _
      | Sst.Bool_pattern _ | Sst.Owned_tree_cursor_pattern _
      | Sst.Or_pattern _ ->
          Error "frozen-spine call actual/formal binding mismatch")
  | (Some _ | None), (Some _ | None) ->
      Error "frozen-spine call actual/ordinal binding mismatch"

let frozen_origin_root_label (root : Vir.aggregate_term) =
  match root.aggregate_desc with
  | Vir.Aggregate_symbol symbol ->
      Printf.sprintf "%s#%d" symbol.source_name symbol.symbol_id
  | Vir.Aggregate_selector _ -> "selector"
  | Vir.Aggregate_constructor _ -> "constructor"
  | Vir.Aggregate_record _ -> "record"
  | Vir.Aggregate_conditional _ -> "conditional"
  | Vir.Aggregate_imported_model_application _ -> "imported-model-application"
  | Vir.Aggregate_recursive_spec_application _ -> "recursive-application"
  | Vir.Aggregate_symbolic_application _ -> "symbolic-application"

let frozen_instance_digest (template : frozen_constructor_template)
    (definition : Sst.function_definition)
    (caller : Sst.function_definition) (callee : Sst.function_definition)
    (edge : Sst_validation.call_edge_descriptor) call_path path_condition
    (root : Vir.aggregate_term) creation_epoch =
  Marshal.to_string
    ( program_fingerprint template.frozen_template_program,
      template.frozen_template_result_path,
      template.frozen_template_obligation_set,
      definition.function_id,
      caller.function_id,
      callee.function_id,
      edge,
      call_path,
      path_condition,
      root,
      creation_epoch,
      0 )
    [ Marshal.No_sharing ]
  |> digest

let issue_frozen_constructor_result_instance (session : t)
    ~(definition : Sst.function_definition)
    ~(callee : Sst.function_definition) ~call_path ~path_condition
    ~(root : Vir.aggregate_term) ~epoch ~allow_branch_reuse =
  if not session.active then Error "verification session is destroyed"
  else
    let* frozen =
      match frozen_descriptor_for_constructor session callee with
      | Some frozen -> Ok frozen
      | None -> Error "frozen-spine result call is not the exact constructor"
    in
    let* template =
      match exact_frozen_template session frozen with
      | [ template ] -> Ok template
      | [] ->
          Error
            "frozen-spine result call has no completed constructor template"
      | _ :: _ :: _ -> Error "frozen-spine constructor template is ambiguous"
    in
    let* edge, caller =
      exact_validated_frozen_call session ~definition ~callee
        ~call_form:Sst.Exec_call ~call_path
    in
    let call_span = List.hd (List.rev call_path) in
    let* () =
      match root.aggregate_desc with
      | Vir.Aggregate_symbol
          { role = Vir.Result; span; sort = Vir.Aggregate typ; _ }
        when span = call_span && typ = vir_type frozen.frozen_root ->
          Ok ()
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
      | Vir.Aggregate_conditional _
      | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _
      | Vir.Aggregate_symbolic_application _ ->
          Error
            "frozen-spine constructor call did not create its exact fresh result root"
    in
    let* () =
      if epoch = 0 then Ok ()
      else Error "frozen-spine constructor result has a stale structural epoch"
    in
    let* root_callable = canonical_callable_key session definition in
    let* caller_callable = canonical_callable_key session caller in
    let* callee_callable = canonical_callable_key session callee in
    let existing =
      List.filter
        (fun instance ->
          instance.frozen_instance_issuer == private_issuer
          && instance.frozen_instance_session == session.session
          && instance.frozen_instance_template == template
          && same_frozen_function_id
               instance.frozen_instance_root_definition.function_id
               definition.function_id
          && instance.frozen_instance_caller = caller
          && instance.frozen_instance_callee = callee
          && instance.frozen_instance_call_edge = edge
          && instance.frozen_instance_call_span = call_span
          && instance.frozen_instance_call_path = call_path
          && instance.frozen_instance_result_root = root
          && instance.frozen_instance_structural_epoch = epoch
          && String.equal instance.frozen_instance_digest
               (frozen_instance_digest template definition caller callee edge
                  call_path instance.frozen_instance_path_condition root epoch))
        session.frozen_constructor_result_instances
    in
    match (existing, allow_branch_reuse) with
    | [ _ ], true -> Ok ()
    | [ _ ], false ->
        Error "frozen-spine constructor result instance was replayed"
    | _ :: _ :: _, _ ->
        Error "frozen-spine constructor-result origin is ambiguous"
    | [], _ ->
        let instance_digest =
          frozen_instance_digest template definition caller callee edge call_path
            path_condition root epoch
        in
        let child, terminal_edge = frozen_child_and_terminal frozen root in
        let instance =
          {
            frozen_instance_issuer = private_issuer;
            frozen_instance_session = session.session;
            frozen_instance_token = ref ();
            frozen_instance_template = template;
            frozen_instance_program = session.identity;
            frozen_instance_root_definition = definition;
            frozen_instance_root_callable = root_callable;
            frozen_instance_root_body =
              callable_body_snapshot session definition;
            frozen_instance_caller = caller;
            frozen_instance_caller_callable = caller_callable;
            frozen_instance_caller_body =
              callable_body_snapshot session caller;
            frozen_instance_callee = callee;
            frozen_instance_callee_callable = callee_callable;
            frozen_instance_callee_body =
              callable_body_snapshot session callee;
            frozen_instance_call_edge = edge;
            frozen_instance_call_span = call_span;
            frozen_instance_call_path = call_path;
            frozen_instance_path_condition = path_condition;
            frozen_instance_result_root = root;
            frozen_instance_child = child;
            frozen_instance_terminal_edge = terminal_edge;
            frozen_instance_structural_epoch = 0;
            frozen_instance_digest = instance_digest;
          }
        in
        session.frozen_constructor_result_instances <-
          instance :: session.frozen_constructor_result_instances;
        session.counters.frozen_result_instance_issuances <-
          session.counters.frozen_result_instance_issuances + 1;
        (match Sys.getenv_opt "VEROCAML_TEST_FROZEN_ORIGIN_TRACE" with
        | Some "1" ->
            [%log.debug "frozen origin instance"
              ~caller_name:
                (Delator.Field.string caller.function_id.function_name)
              ~caller_index:
                (Delator.Field.int caller.function_id.function_index)
              ~callee_name:
                (Delator.Field.string callee.function_id.function_name)
              ~callee_index:
                (Delator.Field.int callee.function_id.function_index)
              ~root:(Delator.Field.string (frozen_origin_root_label root))
              ~epoch:(Delator.Field.int epoch)]
        | Some _ | None -> ());
        Ok ()

let live_frozen_conditional_scope ?(require_verified = false) (session : t)
    (definition : Sst.function_definition) (formal : Sst.binding) ordinal
    (frozen : Sst.frozen_spine_prerequisite) =
  let callable =
    canonical_callable_key session definition |> Result.value ~default:""
  in
  let resolved_path =
    canonical_callable_resolved_path session definition
    |> Result.value ~default:""
  in
  let binding_uid =
    canonical_callable_binding_uid session definition
    |> Result.value ~default:""
  in
  List.filter
    (fun scope ->
      scope.frozen_conditional_issuer == private_issuer
      && scope.frozen_conditional_session == session.session
      && scope.frozen_conditional_token != private_issuer
      && scope.frozen_conditional_template.frozen_template_issuer
         == private_issuer
      && scope.frozen_conditional_template.frozen_template_session
         == session.session
      && scope.frozen_conditional_template.frozen_template_descriptor = frozen
      && same_frozen_function_id
           scope.frozen_conditional_definition.function_id
           definition.function_id
      && String.equal scope.frozen_conditional_callable callable
      && String.equal scope.frozen_conditional_resolved_path resolved_path
      && String.equal scope.frozen_conditional_binding_uid binding_uid
      && String.equal scope.frozen_conditional_body
           (callable_body_snapshot session definition)
      && scope.frozen_conditional_formal = formal
      && scope.frozen_conditional_ordinal = ordinal
      && scope.frozen_conditional_structural_epoch = 0
      && ((not require_verified) || scope.frozen_conditional_verified)
      && scope.frozen_conditional_child
         = fst (frozen_child_and_terminal frozen scope.frozen_conditional_root)
      && scope.frozen_conditional_terminal_edge
         = snd (frozen_child_and_terminal frozen scope.frozen_conditional_root)
      && String.equal scope.frozen_conditional_path
           (Marshal.to_string
              ( scope.frozen_conditional_entry_path_condition,
                definition.function_id,
                scope.frozen_conditional_callable,
                formal,
                ordinal,
                scope.frozen_conditional_root,
                0 )
              [ Marshal.No_sharing ]
           |> digest))
    session.frozen_conditional_scopes

let live_frozen_result_instances (session : t)
    (definition : Sst.function_definition)
    (frozen : Sst.frozen_spine_prerequisite) (root : Vir.aggregate_term) =
  List.filter
    (fun instance ->
      let exact_edge =
        match
          exact_validated_frozen_call session
            ~definition:instance.frozen_instance_root_definition
            ~callee:instance.frozen_instance_callee ~call_form:Sst.Exec_call
            ~call_path:instance.frozen_instance_call_path
        with
        | Ok (edge, caller) ->
            edge = instance.frozen_instance_call_edge
            && caller = instance.frozen_instance_caller
        | Error _ -> false
      in
      instance.frozen_instance_issuer == private_issuer
      && instance.frozen_instance_session == session.session
      && instance.frozen_instance_token != private_issuer
      && String.equal
           (program_fingerprint instance.frozen_instance_program)
           (program_fingerprint session.identity)
      && instance.frozen_instance_template.frozen_template_issuer
         == private_issuer
      && instance.frozen_instance_template.frozen_template_session
         == session.session
      && instance.frozen_instance_template.frozen_template_descriptor = frozen
      && same_frozen_function_id
           instance.frozen_instance_root_definition.function_id
           definition.function_id
      && String.equal instance.frozen_instance_root_callable
           (canonical_callable_key session definition
           |> Result.value ~default:"")
      && String.equal instance.frozen_instance_root_body
           (callable_body_snapshot session definition)
      && String.equal instance.frozen_instance_caller_callable
           (canonical_callable_key session instance.frozen_instance_caller
           |> Result.value ~default:"")
      && String.equal instance.frozen_instance_caller_body
           (callable_body_snapshot session instance.frozen_instance_caller)
      && String.equal instance.frozen_instance_callee_callable
           (canonical_callable_key session instance.frozen_instance_callee
           |> Result.value ~default:"")
      && String.equal instance.frozen_instance_callee_body
           (callable_body_snapshot session instance.frozen_instance_callee)
      && same_frozen_function_id
           instance.frozen_instance_callee.function_id frozen.frozen_constructor
      &&
      (match List.rev instance.frozen_instance_call_path with
      | span :: _ -> instance.frozen_instance_call_span = span
      | [] -> false)
      && exact_edge
      && instance.frozen_instance_result_root = root
      && instance.frozen_instance_child
         = fst (frozen_child_and_terminal frozen root)
      && instance.frozen_instance_terminal_edge
         = snd (frozen_child_and_terminal frozen root)
      && instance.frozen_instance_structural_epoch = 0
      && String.equal instance.frozen_instance_digest
           (frozen_instance_digest instance.frozen_instance_template
              instance.frozen_instance_root_definition
              instance.frozen_instance_caller instance.frozen_instance_callee
              instance.frozen_instance_call_edge instance.frozen_instance_call_path
              instance.frozen_instance_path_condition
              instance.frozen_instance_result_root 0))
    session.frozen_constructor_result_instances

let frozen_discharge_digest (instance : frozen_constructor_result_instance)
    (definition : Sst.function_definition)
    (caller : Sst.function_definition) (callee : Sst.function_definition)
    (edge : Sst_validation.call_edge_descriptor) call_path
    (formal : Sst.binding) ordinal (actual : Sst.expression)
    (root : Vir.aggregate_term) path_condition epoch =
  Marshal.to_string
    ( instance.frozen_instance_digest,
      definition.function_id,
      caller.function_id,
      callee.function_id,
      edge,
      call_path,
      formal,
      ordinal,
      actual,
      root,
      path_condition,
      epoch,
      instance.frozen_instance_structural_epoch )
    [ Marshal.No_sharing ]
  |> digest

let foreign_frozen_span (span : Diagnostic.span) =
  { span with file = span.file ^ ".foreign-origin" }

let foreign_frozen_definition (definition : Sst.function_definition) label =
  {
    definition with
    function_id =
      {
        definition.function_id with
        function_name = definition.function_id.function_name ^ label;
      };
  }

let foreign_frozen_root (root : Vir.aggregate_term) =
  match root.aggregate_desc with
  | Vir.Aggregate_symbol symbol ->
      {
        root with
        aggregate_desc =
          Vir.Aggregate_symbol
            { symbol with symbol_id = symbol.symbol_id + 1_000_000 };
      }
  | Vir.Aggregate_selector _ | Vir.Aggregate_constructor _
  | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
  | Vir.Aggregate_imported_model_application _
  | Vir.Aggregate_recursive_spec_application _
  | Vir.Aggregate_symbolic_application _ ->
      root

let attack_frozen_call_discharge attack discharge =
  let copied_instance () =
    { discharge.frozen_discharge_instance with frozen_instance_token = ref () }
  in
  let stale_instance () =
    {
      discharge.frozen_discharge_instance with
      frozen_instance_structural_epoch =
        discharge.frozen_discharge_instance.frozen_instance_structural_epoch
        + 1;
    }
  in
  match attack with
  | "actual" ->
      {
        discharge with
        frozen_discharge_actual =
          {
            discharge.frozen_discharge_actual with
            span = foreign_frozen_span discharge.frozen_discharge_actual.span;
          };
      }
  | "formal" ->
      {
        discharge with
        frozen_discharge_formal =
          {
            discharge.frozen_discharge_formal with
            id = discharge.frozen_discharge_formal.id + 1_000_000;
          };
      }
  | "ordinal" ->
      {
        discharge with
        frozen_discharge_ordinal = discharge.frozen_discharge_ordinal + 1;
      }
  | "caller" ->
      {
        discharge with
        frozen_discharge_caller =
          foreign_frozen_definition discharge.frozen_discharge_caller
            ":foreign-caller";
      }
  | "callee" ->
      {
        discharge with
        frozen_discharge_callee =
          foreign_frozen_definition discharge.frozen_discharge_callee
            ":foreign-callee";
      }
  | "call" ->
      {
        discharge with
        frozen_discharge_call_path =
          List.map foreign_frozen_span discharge.frozen_discharge_call_path;
      }
  | "span" ->
      {
        discharge with
        frozen_discharge_call_span =
          foreign_frozen_span discharge.frozen_discharge_call_span;
      }
  | "path" ->
      {
        discharge with
        frozen_discharge_call_path =
          discharge.frozen_discharge_call_path
          @ [ foreign_frozen_span discharge.frozen_discharge_call_span ];
      }
  | "path-condition" ->
      {
        discharge with
        frozen_discharge_path_condition =
          Vir.Boolean_constant false
          :: discharge.frozen_discharge_path_condition;
      }
  | "epoch" ->
      {
        discharge with
        frozen_discharge_epoch = discharge.frozen_discharge_epoch + 1;
      }
  | "body" ->
      {
        discharge with
        frozen_discharge_callee_body =
          discharge.frozen_discharge_callee_body ^ ":foreign-body";
      }
  | "cmt" ->
      {
        discharge with
        frozen_discharge_program =
          {
            discharge.frozen_discharge_program with
            cmt_identity =
              discharge.frozen_discharge_program.cmt_identity ^ ":foreign";
          };
      }
  | "session" ->
      { discharge with frozen_discharge_session = ref () }
  | "wrong-root" | "unrelated-producer" ->
      {
        discharge with
        frozen_discharge_root =
          foreign_frozen_root discharge.frozen_discharge_root;
      }
  | "wrong-constructor" ->
      let instance = copied_instance () in
      let template =
        {
          instance.frozen_instance_template with
          frozen_template_definition =
            foreign_frozen_definition
              instance.frozen_instance_template.frozen_template_definition
              ":foreign-constructor";
        }
      in
      {
        discharge with
        frozen_discharge_instance =
          { instance with frozen_instance_template = template };
      }
  | "instance-copy" ->
      {
        discharge with
        frozen_discharge_instance = copied_instance ();
      }
  | "instance-stale" ->
      {
        discharge with
        frozen_discharge_instance = stale_instance ();
      }
  | _ -> discharge

let authenticate_frozen_call_discharge ?(require_registered = false)
    (session : t) discharge =
  let exact_edge =
    match
      exact_validated_frozen_call session
        ~definition:discharge.frozen_discharge_root_definition
        ~callee:discharge.frozen_discharge_callee
        ~call_form:
          (Sst_validation.call_edge_form discharge.frozen_discharge_call_edge)
        ~call_path:discharge.frozen_discharge_call_path
    with
    | Ok (edge, caller) ->
        edge = discharge.frozen_discharge_call_edge
        && caller = discharge.frozen_discharge_caller
    | Error _ -> false
  in
  let exact_conditional =
    let frozen =
      discharge.frozen_discharge_instance.frozen_instance_template
        .frozen_template_descriptor
    in
    match discharge.frozen_discharge_conditional with
    | Some scope ->
        List.memq scope
          (live_frozen_conditional_scope ~require_verified:true session
             discharge.frozen_discharge_callee
             discharge.frozen_discharge_formal
             discharge.frozen_discharge_ordinal frozen)
    | None -> discharge.frozen_discharge_callee.mode = Sst.Spec
  in
  let exact_actual =
    match
      exact_frozen_call_actual discharge.frozen_discharge_call_edge
        discharge.frozen_discharge_ordinal
        discharge.frozen_discharge_formal discharge.frozen_discharge_actual
    with
    | Ok () -> true
    | Error _ -> false
  in
  if not session.active then Error "verification session is destroyed"
  else if
    discharge.frozen_discharge_issuer != private_issuer
    || discharge.frozen_discharge_session != session.session
    || discharge.frozen_discharge_token == private_issuer
    || discharge.frozen_discharge_instance.frozen_instance_issuer
       != private_issuer
    || discharge.frozen_discharge_instance.frozen_instance_session
       != session.session
    || not
         (String.equal
            (program_fingerprint discharge.frozen_discharge_program)
            (program_fingerprint session.identity))
    || not
         (String.equal discharge.frozen_discharge_root_callable
            (canonical_callable_key session
               discharge.frozen_discharge_root_definition
            |> Result.value ~default:""))
    || not
         (String.equal discharge.frozen_discharge_root_body
            (callable_body_snapshot session
               discharge.frozen_discharge_root_definition))
    || not
         (String.equal discharge.frozen_discharge_caller_callable
            (canonical_callable_key session discharge.frozen_discharge_caller
            |> Result.value ~default:""))
    || not
         (String.equal discharge.frozen_discharge_caller_body
            (callable_body_snapshot session discharge.frozen_discharge_caller))
    || not
         (String.equal discharge.frozen_discharge_callee_callable
            (canonical_callable_key session discharge.frozen_discharge_callee
            |> Result.value ~default:""))
    || not
         (String.equal discharge.frozen_discharge_callee_body
            (callable_body_snapshot session discharge.frozen_discharge_callee))
    || not
         (match List.rev discharge.frozen_discharge_call_path with
         | span :: _ -> discharge.frozen_discharge_call_span = span
         | [] -> false)
    || not exact_edge
    || not exact_conditional
    || not exact_actual
    || discharge.frozen_discharge_structural_epoch <> 0
    || not
         (List.memq discharge.frozen_discharge_instance
            (live_frozen_result_instances session
               discharge.frozen_discharge_root_definition
               discharge.frozen_discharge_instance.frozen_instance_template
                 .frozen_template_descriptor
               discharge.frozen_discharge_root))
    || (require_registered
       && not (List.memq discharge session.frozen_call_discharges))
    || not
         (String.equal discharge.frozen_discharge_digest
            (frozen_discharge_digest discharge.frozen_discharge_instance
               discharge.frozen_discharge_root_definition
               discharge.frozen_discharge_caller discharge.frozen_discharge_callee
               discharge.frozen_discharge_call_edge
               discharge.frozen_discharge_call_path
               discharge.frozen_discharge_formal
               discharge.frozen_discharge_ordinal
               discharge.frozen_discharge_actual discharge.frozen_discharge_root
               discharge.frozen_discharge_path_condition
               discharge.frozen_discharge_epoch))
  then Error "frozen-spine call discharge binding mismatch"
  else Ok ()

let consume_frozen_call_discharge (session : t) discharge =
  let* () =
    authenticate_frozen_call_discharge ~require_registered:true session
      discharge
  in
  if not session.active then Error "verification session is destroyed"
  else if discharge.frozen_discharge_consumed then
    Error "frozen-spine call discharge permit was replayed"
  else (
    discharge.frozen_discharge_consumed <- true;
    session.counters.frozen_call_discharge_consumptions <-
      session.counters.frozen_call_discharge_consumptions + 1;
    (match Sys.getenv_opt "VEROCAML_TEST_FROZEN_ORIGIN_TRACE" with
    | Some "1" ->
        [%log.debug "frozen origin discharge"
          ~caller_name:
            (Delator.Field.string
               discharge.frozen_discharge_caller.function_id.function_name)
          ~caller_index:
            (Delator.Field.int
               discharge.frozen_discharge_caller.function_id.function_index)
          ~callee_name:
            (Delator.Field.string
               discharge.frozen_discharge_callee.function_id.function_name)
          ~callee_index:
            (Delator.Field.int
               discharge.frozen_discharge_callee.function_id.function_index)
          ~formal_name:
            (Delator.Field.string discharge.frozen_discharge_formal.name)
          ~formal_ordinal:
            (Delator.Field.int discharge.frozen_discharge_ordinal)
          ~root:
            (Delator.Field.string
               (frozen_origin_root_label discharge.frozen_discharge_root))
          ~epoch:(Delator.Field.int discharge.frozen_discharge_epoch)]
    | Some _ | None -> ());
    Ok ())

let authorize_frozen_formal_call (session : t)
    ~(definition : Sst.function_definition)
    ~(callee : Sst.function_definition) ~call_form ~call_path
    ~(actuals : (Sst.expression * Vir.aggregate_term option) list)
    ~path_condition ~epoch =
  if not session.active then Error "verification session is destroyed"
  else
    let origin_attack =
      match (call_form, Sys.getenv_opt "VEROCAML_TEST_FROZEN_ORIGIN_ATTACK") with
      | Sst.Exec_call, Some attack -> Some attack
      | (Sst.Specification_call | Sst.Proof_call | Sst.Unclassified_call), _
      | Sst.Exec_call, None ->
          None
    in
    let* validated =
      match session.validated with
      | Some validated -> Ok validated
      | None -> Error "verification session is destroyed"
    in
    let* callee_descriptor =
      match Sst_validation.find_callable validated callee.function_id with
      | Some descriptor -> Ok descriptor
      | None -> Error "frozen-spine call target is absent from validation"
    in
    let requirements =
      callee.parameters
      |> List.mapi (fun ordinal parameter ->
             match
               Sst_validation.frozen_formal_requirement validated
                 callee_descriptor ordinal
             with
             | None -> None
             | Some requirement -> Some (ordinal, parameter, requirement))
      |> List.filter_map Fun.id
    in
    let constructor_template_proof =
      requirements <> []
      && List.for_all
           (fun (ordinal, _parameter, requirement) ->
             let type_id =
               Sst_validation.frozen_formal_type requirement
             in
             match
               ( frozen_descriptor_for_type session type_id,
                 List.nth_opt actuals ordinal )
             with
             | ( Some frozen,
                 Some
                   ( _,
                     Some
                       {
                         Vir.aggregate_type;
                         aggregate_desc =
                           Vir.Aggregate_symbol
                             { role = (Vir.Local | Vir.Result); _ };
                       } ) ) ->
                 same_frozen_function_id definition.function_id
                   frozen.frozen_constructor
                 && aggregate_type = vir_type type_id
             | (Some _ | None), (Some _ | None) -> false)
           requirements
    in
    if constructor_template_proof then Ok ()
    else
      match requirements with
      | [] -> Ok ()
      | _ ->
        let* edge, caller =
          exact_validated_frozen_call session ~definition ~callee ~call_form
            ~call_path
        in
        let call_span = List.hd (List.rev call_path) in
        let* root_callable = canonical_callable_key session definition in
        let* caller_callable = canonical_callable_key session caller in
        let* callee_callable = canonical_callable_key session callee in
        let rec prepare prepared = function
          | [] -> Ok (List.rev prepared)
          | (ordinal, parameter, requirement) :: rest ->
              let parameter = Sst.require_value_parameter parameter in
              let* formal =
                match parameter.Sst.pattern.pattern_desc with
                | Sst.Bind formal -> Ok formal
                | Sst.Wildcard | Sst.Unit_pattern | Sst.Tuple_pattern _
                | Sst.Record_pattern _ | Sst.Constructor_pattern _
                | Sst.Int_pattern _ | Sst.Bool_pattern _
                | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
                    Error
                      "frozen-spine call formal is not an exact binding"
              in
              let* actual, root =
                match List.nth_opt actuals ordinal with
                | Some (actual, Some root) -> Ok (actual, root)
                | Some (_, None) | None ->
                    Error
                      "frozen-spine call actual is not an exact aggregate root"
              in
              let* () = exact_frozen_call_actual edge ordinal formal actual in
              let frozen_type =
                Sst_validation.frozen_formal_type requirement
              in
              let* frozen =
                match frozen_descriptor_for_type session frozen_type with
                | Some frozen
                  when parameter.pattern.typ = Sst.Aggregate frozen_type
                       && actual.typ = Sst.Aggregate frozen_type
                       && root.aggregate_type = vir_type frozen_type ->
                    Ok frozen
                | Some _ | None ->
                    Error "frozen-spine call formal/type binding mismatch"
              in
              let constructor_template_proof =
                same_frozen_function_id definition.function_id
                  frozen.frozen_constructor
                &&
                match root.aggregate_desc with
                | Vir.Aggregate_symbol
                    { role = (Vir.Local | Vir.Result); _ } ->
                    true
                | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
                | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
                | Vir.Aggregate_conditional _
                | Vir.Aggregate_imported_model_application _
                | Vir.Aggregate_recursive_spec_application _
                | Vir.Aggregate_symbolic_application _ ->
                    false
              in
              let caller_scopes =
                session.frozen_conditional_scopes
                |> List.filter (fun scope ->
                       scope.frozen_conditional_root = root
                       && List.memq scope
                            (live_frozen_conditional_scope session definition
                               scope.frozen_conditional_formal
                               scope.frozen_conditional_ordinal frozen))
              in
              if constructor_template_proof then prepare prepared rest
              else if caller_scopes <> [] then
                (match caller_scopes with
                | [ _ ] -> prepare prepared rest
                | _ :: _ :: _ ->
                    Error
                      "frozen-spine conditional call origin is ambiguous"
                | [] -> assert false)
              else
                let* callee_scope =
                  match callee.mode with
                  | Sst.Exec | Sst.Proof ->
                      (match
                         live_frozen_conditional_scope ~require_verified:true
                           session callee formal ordinal frozen
                       with
                      | [ scope ] -> Ok (Some scope)
                      | [] ->
                          Error
                            "frozen-spine callee has no conditional formal scope"
                      | _ :: _ :: _ ->
                          Error
                            "frozen-spine callee conditional scope is ambiguous")
                  | Sst.Spec ->
                      (* Specification bodies are expanded at the exact call;
                         their validated frozen-formal descriptor is the private
                         conditional requirement. *)
                      Ok None
                in
                let instances =
                  live_frozen_result_instances session definition frozen root
                in
                let instances =
                  match (origin_attack, instances) with
                  | Some "ambiguous-origin", instance :: _ ->
                      instance :: instances
                  | (Some _ | None), _ -> instances
                in
                let* instance =
                  match instances with
                  | [ instance ] -> Ok instance
                  | [] ->
                      Error
                        "frozen-spine call has no exact constructor-result instance"
                  | _ :: _ :: _ ->
                      Error
                        "frozen-spine constructor-result origin is ambiguous"
                in
                let* () =
                  match origin_attack with
                  | Some "instance-replay" -> (
                      match
                        issue_frozen_constructor_result_instance session
                          ~definition:instance.frozen_instance_root_definition
                          ~callee:instance.frozen_instance_callee
                          ~call_path:instance.frozen_instance_call_path
                          ~path_condition:
                            instance.frozen_instance_path_condition
                          ~root:instance.frozen_instance_result_root
                          ~epoch:instance.frozen_instance_structural_epoch
                          ~allow_branch_reuse:false
                      with
                      | Error message -> Error message
                      | Ok () ->
                          Error
                            "frozen-spine constructor result instance replay attack was accepted")
                  | Some _ | None -> Ok ()
                in
                let discharge_digest =
                  frozen_discharge_digest instance definition caller callee edge
                    call_path formal ordinal actual root path_condition epoch
                in
                if
                  List.exists
                    (fun discharge ->
                      String.equal discharge.frozen_discharge_digest
                        discharge_digest)
                    session.frozen_call_discharges
                then Error "frozen-spine call discharge permit was replayed"
                else
                  let discharge =
                    {
                      frozen_discharge_issuer = private_issuer;
                      frozen_discharge_session = session.session;
                      frozen_discharge_token = ref ();
                      frozen_discharge_instance = instance;
                      frozen_discharge_conditional = callee_scope;
                      frozen_discharge_program = session.identity;
                      frozen_discharge_root_definition = definition;
                      frozen_discharge_root_callable = root_callable;
                      frozen_discharge_root_body =
                        callable_body_snapshot session definition;
                      frozen_discharge_caller = caller;
                      frozen_discharge_caller_callable = caller_callable;
                      frozen_discharge_caller_body =
                        callable_body_snapshot session caller;
                      frozen_discharge_callee = callee;
                      frozen_discharge_callee_callable = callee_callable;
                      frozen_discharge_callee_body =
                        callable_body_snapshot session callee;
                      frozen_discharge_call_edge = edge;
                      frozen_discharge_call_span = call_span;
                      frozen_discharge_call_path = call_path;
                      frozen_discharge_formal = formal;
                      frozen_discharge_ordinal = ordinal;
                      frozen_discharge_actual = actual;
                      frozen_discharge_root = root;
                      frozen_discharge_path_condition = path_condition;
                      frozen_discharge_epoch = epoch;
                      frozen_discharge_structural_epoch =
                        instance.frozen_instance_structural_epoch;
                      frozen_discharge_digest = discharge_digest;
                      frozen_discharge_consumed = false;
                      frozen_discharge_observation_permits = [];
                      frozen_discharge_observed_epochs = [];
                    }
                  in
                  let attacked =
                    match origin_attack with
                    | Some attack ->
                        attack_frozen_call_discharge attack discharge
                    | None -> discharge
                  in
                  let* () =
                    authenticate_frozen_call_discharge session attacked
                  in
                  let* () =
                    match origin_attack with
                    | Some
                        ( "actual" | "formal" | "ordinal" | "caller"
                        | "callee" | "call" | "span" | "path"
                        | "path-condition" | "epoch" | "body" | "cmt"
                        | "session" | "wrong-root" | "unrelated-producer"
                        | "wrong-constructor" | "instance-copy"
                        | "instance-stale" ) ->
                        Error
                          "frozen-spine origin binding attack was accepted"
                    | Some _ | None -> Ok ()
                  in
                  prepare (discharge :: prepared) rest
        in
        let* discharges = prepare [] requirements in
        session.frozen_call_discharges <-
          List.rev_append discharges session.frozen_call_discharges;
        session.counters.frozen_call_discharge_issuances <-
          session.counters.frozen_call_discharge_issuances
          + List.length discharges;
        let rec consume = function
          | [] -> Ok ()
          | discharge :: rest -> (
              match origin_attack with
              | Some "permit-copy" ->
                  consume_frozen_call_discharge session
                    { discharge with frozen_discharge_consumed = false }
              | Some ("permit-replay" | "replayed-permit") ->
                  let* () =
                    consume_frozen_call_discharge session discharge
                  in
                  consume_frozen_call_discharge session discharge
              | Some _ | None ->
                  let* () =
                    consume_frozen_call_discharge session discharge
                  in
                  consume rest)
        in
        consume discharges

let frozen_observation_digest source (definition : Sst.function_definition)
    root call_path path_condition epoch =
  let source_digest =
    match source with
    | Frozen_conditional_proof scope -> scope.frozen_conditional_path
    | Frozen_discharged_call discharge -> discharge.frozen_discharge_digest
  in
  Marshal.to_string
    (source_digest, definition.function_id, root, call_path, path_condition, epoch)
    [ Marshal.No_sharing ]
  |> digest

let seal_frozen_observation (session : t)
    ~(definition : Sst.function_definition)
    ~(frozen : Sst.frozen_spine_prerequisite) ~root ~call_path
    ~path_condition ~epoch =
  if not session.active then Error "verification session is destroyed"
  else
    let* _edge, _caller =
      let* validated =
        match session.validated with
        | Some validated -> Ok validated
        | None -> Error "verification session is destroyed"
      in
      let* descriptor =
        match Sst_validation.find_callable validated frozen.frozen_model with
        | Some descriptor -> Ok descriptor
        | None -> Error "frozen-spine model is absent from validation"
      in
      exact_validated_frozen_call session ~definition
        ~callee:(Sst_validation.callable_definition descriptor)
        ~call_form:Sst.Specification_call ~call_path
    in
    let discharges =
      List.filter
        (fun discharge ->
          discharge.frozen_discharge_issuer == private_issuer
          && discharge.frozen_discharge_session == session.session
          && discharge.frozen_discharge_consumed
          && same_frozen_function_id
               discharge.frozen_discharge_root_definition.function_id
               definition.function_id
          && discharge.frozen_discharge_instance.frozen_instance_template
               .frozen_template_descriptor
             = frozen
          && same_frozen_function_id
               discharge.frozen_discharge_callee.function_id
               frozen.frozen_model
          && discharge.frozen_discharge_root = root
          && discharge.frozen_discharge_call_path = call_path
          && discharge.frozen_discharge_path_condition = path_condition
          && discharge.frozen_discharge_epoch = epoch
          && discharge.frozen_discharge_structural_epoch = 0
          && String.equal discharge.frozen_discharge_root_body
               (callable_body_snapshot session definition))
        session.frozen_call_discharges
    in
    let conditional =
      List.filter
        (fun scope ->
          scope.frozen_conditional_issuer == private_issuer
          && scope.frozen_conditional_session == session.session
          && same_frozen_function_id
               scope.frozen_conditional_definition.function_id
               definition.function_id
          && scope.frozen_conditional_template.frozen_template_descriptor
             = frozen
          && scope.frozen_conditional_root = root
          && String.equal scope.frozen_conditional_body
               (callable_body_snapshot session definition))
        session.frozen_conditional_scopes
    in
    let* source, permits =
      match (discharges, conditional) with
      | [ discharge ], [] ->
          Ok
            ( Frozen_discharged_call discharge,
              discharge.frozen_discharge_observation_permits )
      | [], [ scope ] ->
          Ok
            ( Frozen_conditional_proof scope,
              scope.frozen_conditional_observation_permits )
      | [], []
        when
          (match exact_frozen_template session frozen with
          | [ template ] ->
              same_frozen_function_id definition.function_id
                template.frozen_template_definition.function_id
          | [] | _ :: _ :: _ -> false) ->
          Error "frozen-spine constructor template observation is internal"
      | [], [] -> Error "frozen-spine observation lacks exact call authority"
      | _ -> Error "frozen-spine observation call authority is ambiguous"
    in
    let observation_digest =
      frozen_observation_digest source definition root call_path path_condition
        epoch
    in
    if
      List.exists
        (fun permit ->
          String.equal permit.frozen_observation_digest observation_digest)
        permits
    then Error "frozen-spine observation call/path permit was replayed"
    else
      let permit =
        {
          frozen_observation_issuer = private_issuer;
          frozen_observation_session = session.session;
          frozen_observation_token = ref ();
          frozen_observation_source = source;
          frozen_observation_definition = definition;
          frozen_observation_root = root;
          frozen_observation_call_path = call_path;
          frozen_observation_path_condition = path_condition;
          frozen_observation_epoch = epoch;
          frozen_observation_digest = observation_digest;
          frozen_observation_consumed = false;
        }
      in
      (match source with
      | Frozen_conditional_proof scope ->
          scope.frozen_conditional_observation_permits <-
            permit :: scope.frozen_conditional_observation_permits
      | Frozen_discharged_call discharge ->
          discharge.frozen_discharge_observation_permits <-
            permit :: discharge.frozen_discharge_observation_permits);
      Ok permit

let consume_frozen_observation (session : t) permit
    ~(definition : Sst.function_definition)
    ~(frozen : Sst.frozen_spine_prerequisite) ~root ~call_path
    ~path_condition ~epoch =
  let permits, concrete =
    match permit.frozen_observation_source with
    | Frozen_conditional_proof scope ->
        (scope.frozen_conditional_observation_permits, None)
    | Frozen_discharged_call discharge ->
        ( discharge.frozen_discharge_observation_permits,
          Some discharge )
  in
  if not session.active then Error "verification session is destroyed"
  else if
    permit.frozen_observation_issuer != private_issuer
    || permit.frozen_observation_session != session.session
    || permit.frozen_observation_definition.function_id
       <> definition.function_id
    || permit.frozen_observation_root <> root
    || permit.frozen_observation_call_path <> call_path
    || permit.frozen_observation_path_condition <> path_condition
    || permit.frozen_observation_epoch <> epoch
    || not
         (String.equal permit.frozen_observation_digest
            (frozen_observation_digest permit.frozen_observation_source
               definition root call_path path_condition epoch))
    || not
         (List.exists
            (fun candidate ->
              candidate.frozen_observation_token
              == permit.frozen_observation_token)
            permits)
    ||
    (match permit.frozen_observation_source with
    | Frozen_conditional_proof scope ->
        scope.frozen_conditional_issuer != private_issuer
        || scope.frozen_conditional_session != session.session
        || scope.frozen_conditional_template.frozen_template_descriptor
           <> frozen
    | Frozen_discharged_call discharge ->
        discharge.frozen_discharge_issuer != private_issuer
        || discharge.frozen_discharge_session != session.session
        || not discharge.frozen_discharge_consumed
        || discharge.frozen_discharge_instance.frozen_instance_template
             .frozen_template_descriptor
           <> frozen)
  then Error "frozen-spine observation permit call/path mismatch"
  else if permit.frozen_observation_consumed then
    Error "frozen-spine observation permit was replayed"
  else (
    permit.frozen_observation_consumed <- true;
    match concrete with
    | None -> Ok ()
    | Some discharge ->
        discharge.frozen_discharge_observed_epochs <-
          epoch :: discharge.frozen_discharge_observed_epochs;
        session.counters.frozen_descent_witness_issuances <-
          session.counters.frozen_descent_witness_issuances + 1;
        session.counters.frozen_descent_witness_consumptions <-
          session.counters.frozen_descent_witness_consumptions + 1;
        session.counters.frozen_observation_consumptions <-
          session.counters.frozen_observation_consumptions + 1;
        Ok ())

let owned_root_version aggregate =
  Marshal.to_string aggregate [ Marshal.No_sharing ] |> digest

let owned_root_path_digest path_condition =
  path_condition |> List.map Vir.boolean_term_to_string
  |> String.concat "\000" |> digest

let owned_contents_path path_condition =
  List.map Vir.boolean_term_to_string path_condition

let rec owned_contents_path_prefix prefix path =
  match (prefix, path) with
  | [], _ -> true
  | left :: prefix, right :: path when String.equal left right ->
      owned_contents_path_prefix prefix path
  | _ :: _, [] | _ :: _, _ :: _ -> false

let owned_root_model_signature (definition : Sst.function_definition) =
  Marshal.to_string
    ( definition.function_id,
      definition.parameters,
      definition.result_type,
      definition.mode,
      definition.recursive )
    [ Marshal.No_sharing ]
  |> digest

let rec pattern_bindings (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Bind binding -> [ binding ]
  | Sst.Owned_tree_cursor_pattern cursor -> [ cursor.cursor_binding ]
  | Sst.Tuple_pattern components ->
      List.concat_map (fun (_, nested) -> pattern_bindings nested) components
  | Sst.Record_pattern fields ->
      List.concat_map (fun (_, nested) -> pattern_bindings nested) fields
  | Sst.Constructor_pattern (_, arguments) ->
      List.concat_map pattern_bindings arguments
  | Sst.Or_pattern (left, right) ->
      pattern_bindings left @ pattern_bindings right
  | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Unit_pattern ->
      []

let exact_callable_root_binding definition binding =
  let same_binding (candidate : Sst.binding) =
    candidate.id = binding.Sst.id
    && String.equal candidate.name binding.name
    && candidate.typ = binding.typ
    && candidate.uniqueness = binding.uniqueness
    && candidate.span = binding.span
  in
  let parameters =
    definition.Sst.parameters
    |> List.concat_map (fun parameter ->
           pattern_bindings
             (Sst.require_value_parameter parameter).Sst.pattern)
  in
  let results =
    definition.contracts.ensures
    |> List.concat_map (fun clause ->
           match clause.Sst.binder with
           | Some binder -> pattern_bindings binder
           | None -> [])
  in
  List.exists same_binding (parameters @ results)

let reject_owned_root_scalar_plan session message =
  session.counters.owned_root_scalar_plans_rejected <-
    session.counters.owned_root_scalar_plans_rejected + 1;
  Error message

let mutate_owned_root_scalar_plan attack plan =
  let foreign suffix value = value ^ "\000foreign-" ^ suffix in
  match attack with
  | "copied" | "copy" -> { plan with owned_plan_token = ref () }
  | "wrong-root" | "root" | "rebound-root" | "rebound" | "branch-root"
  | "branch" ->
      {
        plan with
        owned_plan_root_identity =
          foreign attack plan.owned_plan_root_identity;
      }
  | "stale-root" | "stale" | "wrong-version" | "version" ->
      {
        plan with
        owned_plan_root_version = plan.owned_plan_root_version + 1;
      }
  | "wrong-path" | "path" | "wrong-field" | "field"
  | "wrong-constructor" | "constructor" ->
      {
        plan with
        owned_plan_model_paths =
          foreign attack plan.owned_plan_model_paths;
      }
  | "wrong-type" | "type" ->
      {
        plan with
        owned_plan_root_type =
          {
            plan.owned_plan_root_type with
            type_name = plan.owned_plan_root_type.type_name ^ ".foreign";
          };
      }
  | "wrong-model" | "model" ->
      {
        plan with
        owned_plan_model_callable =
          foreign attack plan.owned_plan_model_callable;
      }
  | "wrong-body" | "body" ->
      {
        plan with
        owned_plan_model_body = foreign attack plan.owned_plan_model_body;
      }
  | "wrong-signature" | "signature" ->
      {
        plan with
        owned_plan_signature_snapshot =
          foreign attack plan.owned_plan_signature_snapshot;
      }
  | "wrong-program" | "program" ->
      {
        plan with
        owned_plan_program_snapshot =
          foreign attack plan.owned_plan_program_snapshot;
      }
  | "wrong-cmt" | "cmt" ->
      {
        plan with
        owned_plan_cmt_identity = foreign attack plan.owned_plan_cmt_identity;
      }
  | "wrong-family" | "family" ->
      {
        plan with
        owned_plan_family_identity =
          foreign attack plan.owned_plan_family_identity;
      }
  | "wrong-session" | "session" ->
      { plan with owned_plan_session = ref () }
  | "substitution" | "import" ->
      {
        plan with
        owned_plan_unit_identity =
          foreign attack plan.owned_plan_unit_identity;
      }
  | _ -> { plan with owned_plan_token = ref () }

let issue_owned_root_scalar_observation_plan session ~validated ~model
    ~caller ~call ~actual ~root_binding ~(root : Vir.aggregate_term)
    ~root_version ~path_condition =
  if not session.active then
    reject_owned_root_scalar_plan session "verification session is destroyed"
  else if not (String.equal session.identity.family_identity "retained-v1") then
    reject_owned_root_scalar_plan session
      "owned-root scalar observation requires the retained-v1 family"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then
    reject_owned_root_scalar_plan session
      "owned-root scalar plan has the wrong validated program"
  else
    match Sst_validation_private.Public.owned_root_scalar_model model with
    | None ->
        reject_owned_root_scalar_plan session
          "callable has no admitted owned-root scalar model"
    | Some template ->
        if
          not
            (Sst_validation_private.Public.authenticate_owned_root_scalar_model
               ~validated model template)
        then
          reject_owned_root_scalar_plan session
            "owned-root scalar model template is stale or forged"
        else
          let model_definition =
            Sst_validation.callable_definition
              (Sst_validation.model_callable model)
          in
          let formal =
            Sst_validation_private.Public.owned_root_scalar_formal template
          in
          let domain =
            Sst_validation_private.Public.owned_root_scalar_domain template
          in
          let exact_call =
            match call.Sst.expression_desc with
            | Sst.Direct_call
                {
                  call_form = Sst.Specification_call;
                  callee;
                  arguments = [ Sst.Value_argument { label = None; value = argument } ];
                  recursive = false;
                  type_arguments = [];
                } ->
                callee = model_definition.function_id && argument == actual
            | _ -> false
          in
          let exact_actual =
            match actual.Sst.expression_desc with
            | Sst.Variable { binding; _ } ->
                binding == root_binding
                && exact_callable_root_binding caller binding
                && binding.typ = Sst.Aggregate domain
                && actual.typ = binding.typ
            | _ -> false
          in
          if
            not exact_call || not exact_actual
            || root.aggregate_type
               <> {
                    Vir.aggregate_type_index = domain.type_index;
                    aggregate_type_name = domain.type_name;
      aggregate_type_arguments = [];
                  }
            || formal.typ <> Sst.Aggregate domain
            || formal.uniqueness <> Sst.Definitely_aliased
          then
            reject_owned_root_scalar_plan session
              "owned-root scalar call does not bind the exact current abstract root"
          else
            let* model_callable =
              canonical_callable_key session model_definition
            in
            let* _caller_callable = canonical_callable_key session caller in
            let exact =
              {
                owned_plan_issuer = owned_root_scalar_plan_issuer;
                owned_plan_session = session.session;
                owned_plan_token = ref ();
                owned_plan_validated = validated;
                owned_plan_program_snapshot =
                  session.identity.program_snapshot;
                owned_plan_signature_snapshot =
                  session.identity.signature_snapshot;
                owned_plan_unit_identity = session.identity.unit_identity;
                owned_plan_cmt_identity = session.identity.cmt_identity;
                owned_plan_family_identity = session.identity.family_identity;
                owned_plan_model = template;
                owned_plan_model_definition = model_definition;
                owned_plan_model_callable = model_callable;
                owned_plan_model_body =
                  callable_body_snapshot session model_definition;
                owned_plan_model_signature =
                  owned_root_model_signature model_definition;
                owned_plan_model_paths =
                  Marshal.to_string
                    ( Sst_validation_private.Public
                      .owned_root_scalar_paths template,
                      Sst_validation_private.Public
                      .owned_root_scalar_reads template,
                      Sst_validation_private.Public
                      .owned_root_scalar_matches template )
                    [ Marshal.No_sharing ]
                  |> digest;
                owned_plan_result_fields =
                  Marshal.to_string
                    (Sst_validation_private.Public
                     .owned_root_scalar_result_fields template)
                    [ Marshal.No_sharing ]
                  |> digest;
                owned_plan_root_type = domain;
                owned_plan_formal = formal;
                owned_plan_caller = caller;
                owned_plan_call = call;
                owned_plan_actual = actual;
                owned_plan_root_binding = root_binding;
                owned_plan_root = root;
                owned_plan_root_identity = owned_root_version root;
                owned_plan_root_version = root_version;
                owned_plan_path_digest =
                  owned_root_path_digest path_condition;
              }
            in
            session.issued_owned_root_scalar_plans <-
              exact :: session.issued_owned_root_scalar_plans;
            session.counters.owned_root_scalar_plans_issued <-
              session.counters.owned_root_scalar_plans_issued + 1;
            Ok
              (match !owned_root_scalar_plan_attack_for_testing with
              | None -> exact
              | Some attack -> mutate_owned_root_scalar_plan attack exact)

let consume_owned_root_scalar_observation_plan session plan ~validated ~model
    ~caller ~call ~actual ~root_binding ~(root : Vir.aggregate_term)
    ~root_version ~path_condition =
  let model_definition =
    Sst_validation.callable_definition (Sst_validation.model_callable model)
  in
  let model_callable = canonical_callable_key session model_definition in
  let exact =
    List.find_opt
      (fun candidate -> candidate.owned_plan_token == plan.owned_plan_token)
      session.issued_owned_root_scalar_plans
  in
  match exact with
  | None ->
      reject_owned_root_scalar_plan session
        "owned-root scalar plan was not issued by this session"
  | Some exact
    when
      plan != exact || plan.owned_plan_issuer != owned_root_scalar_plan_issuer
      || plan.owned_plan_session != session.session
      || not session.active
      || plan.owned_plan_validated != validated
      ||
      not
        (match session.validated with
        | Some owned -> owned == validated
        | None -> false)
      || plan.owned_plan_model
         !=
         Option.get
           (Sst_validation_private.Public.owned_root_scalar_model model)
      || plan.owned_plan_model_definition != model_definition
      || model_callable <> Ok plan.owned_plan_model_callable
      || not
           (String.equal plan.owned_plan_model_body
              (callable_body_snapshot session model_definition))
      || not
           (String.equal plan.owned_plan_model_signature
              (owned_root_model_signature model_definition))
      || not
           (String.equal plan.owned_plan_model_paths
              (Marshal.to_string
                 ( Sst_validation_private.Public
                   .owned_root_scalar_paths plan.owned_plan_model,
                   Sst_validation_private.Public
                   .owned_root_scalar_reads plan.owned_plan_model,
                   Sst_validation_private.Public
                   .owned_root_scalar_matches plan.owned_plan_model )
                 [ Marshal.No_sharing ]
              |> digest))
      || not
           (String.equal plan.owned_plan_result_fields
              (Sst_validation_private.Public
               .owned_root_scalar_result_fields plan.owned_plan_model
              |> fun fields ->
              Marshal.to_string fields [ Marshal.No_sharing ]
              |> digest))
      || plan.owned_plan_root_type
         <> Sst_validation_private.Public.owned_root_scalar_domain
              plan.owned_plan_model
      || plan.owned_plan_formal
         != Sst_validation_private.Public.owned_root_scalar_formal
              plan.owned_plan_model
      || plan.owned_plan_caller != caller || plan.owned_plan_call != call
      || plan.owned_plan_actual != actual
      || plan.owned_plan_root_binding != root_binding
      || plan.owned_plan_root <> root
      || not
           (String.equal plan.owned_plan_root_identity
              (owned_root_version root))
      || plan.owned_plan_root_version <> root_version
      || not
           (String.equal plan.owned_plan_path_digest
              (owned_root_path_digest path_condition))
      || not
           (String.equal plan.owned_plan_program_snapshot
              session.identity.program_snapshot)
      || not
           (String.equal plan.owned_plan_signature_snapshot
              session.identity.signature_snapshot)
      || not
           (String.equal plan.owned_plan_unit_identity
              session.identity.unit_identity)
      || not
           (String.equal plan.owned_plan_cmt_identity
              session.identity.cmt_identity)
      || not
           (String.equal plan.owned_plan_family_identity
              session.identity.family_identity)
      || List.exists
           (( == ) plan.owned_plan_token)
           session.consumed_owned_root_scalar_plans ->
      reject_owned_root_scalar_plan session
        "owned-root scalar plan seal is wrong, stale, copied, or consumed"
  | Some _ ->
      session.consumed_owned_root_scalar_plans <-
        plan.owned_plan_token :: session.consumed_owned_root_scalar_plans;
      session.counters.owned_root_scalar_plans_consumed <-
        session.counters.owned_root_scalar_plans_consumed + 1;
      (* A positive version is assigned only to the exact successor produced
         by an authenticated owned-tree mutation.  Reconstruction is narrower:
         the executor records it only after an admitted tag path proves that a
         recursive child changed from a nonempty constructor to the written
         constructor. *)
      if root_version > 0 then
        session.counters.owned_root_scalar_fresh_successors <-
          session.counters.owned_root_scalar_fresh_successors + 1;
      Ok ()

let owned_root_scalar_plan_root plan = plan.owned_plan_root
let owned_root_scalar_plan_template plan = plan.owned_plan_model

let note_owned_root_scalar_observation session =
  session.counters.owned_root_scalar_observations <-
    session.counters.owned_root_scalar_observations + 1

let note_owned_root_scalar_equation session =
  session.counters.owned_root_scalar_equations <-
    session.counters.owned_root_scalar_equations + 1

let note_owned_root_scalar_bridge_path session =
  session.counters.owned_root_scalar_bridge_paths <-
    session.counters.owned_root_scalar_bridge_paths + 1

let note_owned_root_scalar_bridge_equation session =
  session.counters.owned_root_scalar_bridge_equations <-
    session.counters.owned_root_scalar_bridge_equations + 1

let note_owned_root_scalar_reconstruction session =
  session.counters.owned_root_scalar_reconstructions <-
    session.counters.owned_root_scalar_reconstructions + 1

let owned_contents_origin_fingerprint origin =
  Marshal.to_string
    ( origin.contents_origin_program_snapshot,
      origin.contents_origin_signature_snapshot,
      origin.contents_origin_unit_identity,
      origin.contents_origin_cmt_identity,
      origin.contents_origin_family_identity,
      origin.contents_origin_caller_snapshot,
      origin.contents_origin_expression_snapshot,
      origin.contents_origin_path_digest,
      origin.contents_origin_predecessor_root,
      origin.contents_origin_root,
      origin.contents_origin_version,
      Option.map
        (fun predecessor -> predecessor.contents_origin_digest)
        origin.contents_origin_predecessor )
    [ Marshal.No_sharing ]
  |> digest

let registered_owned_contents_origin (session : t)
    (origin : owned_contents_origin) =
  List.exists
    (fun issued ->
      issued == origin
      && issued.contents_origin_token == origin.contents_origin_token)
    session.issued_owned_contents_origins

let authenticate_owned_contents_origin (session : t)
    (origin : owned_contents_origin) =
  let rec authenticate_chain require_live seen origin =
    not (List.memq origin.contents_origin_token seen)
    &&
  origin.contents_origin_issuer == owned_contents_origin_issuer
  && origin.contents_origin_session == session.session
  && session.active
  && origin.contents_origin_validated
     ==
     Option.get session.validated
  && String.equal origin.contents_origin_program_snapshot
       session.identity.program_snapshot
  && String.equal origin.contents_origin_signature_snapshot
       session.identity.signature_snapshot
  && String.equal origin.contents_origin_unit_identity
       session.identity.unit_identity
  && String.equal origin.contents_origin_cmt_identity
       session.identity.cmt_identity
  && String.equal origin.contents_origin_family_identity
       session.identity.family_identity
  && String.equal origin.contents_origin_caller_snapshot
       (callable_body_snapshot session origin.contents_origin_caller)
  && String.equal origin.contents_origin_expression_snapshot
       (Marshal.to_string origin.contents_origin_expression
          [ Marshal.No_sharing ]
       |> digest)
  && String.equal origin.contents_origin_path_digest
       (String.concat "\000" origin.contents_origin_path |> digest)
  && String.equal origin.contents_origin_digest
       (owned_contents_origin_fingerprint origin)
  && registered_owned_contents_origin session origin
  &&
  ((not require_live)
  ||
  not
    (List.exists (( == ) origin)
       session.retired_owned_contents_origins))
  &&
  match origin.contents_origin_predecessor with
  | None -> origin.contents_origin_version = 0
  | Some predecessor ->
      authenticate_chain false
        (origin.contents_origin_token :: seen)
        predecessor
      && origin.contents_origin_predecessor_root
         = Some predecessor.contents_origin_root
      && origin.contents_origin_version
         = predecessor.contents_origin_version + 1
  in
  authenticate_chain true [] origin

let issue_closed_owned_contents_origin (session : t) ~validated ~caller
    ~expression ~root ~path_condition =
  if
    not session.active
    ||
    match session.validated with
    | Some owned -> owned != validated
    | None -> true
  then Error "owned-contents closed origin has the wrong active session"
  else
    let origin =
      {
        contents_origin_issuer = owned_contents_origin_issuer;
        contents_origin_session = session.session;
        contents_origin_token = ref ();
        contents_origin_validated = validated;
        contents_origin_program_snapshot = session.identity.program_snapshot;
        contents_origin_signature_snapshot =
          session.identity.signature_snapshot;
        contents_origin_unit_identity = session.identity.unit_identity;
        contents_origin_cmt_identity = session.identity.cmt_identity;
        contents_origin_family_identity = session.identity.family_identity;
        contents_origin_caller = caller;
        contents_origin_caller_snapshot =
          callable_body_snapshot session caller;
        contents_origin_expression = expression;
        contents_origin_expression_snapshot =
          Marshal.to_string expression [ Marshal.No_sharing ] |> digest;
        contents_origin_path = owned_contents_path path_condition;
        contents_origin_path_digest =
          owned_root_path_digest path_condition;
        contents_origin_predecessor = None;
        contents_origin_predecessor_root = None;
        contents_origin_root = root;
        contents_origin_version = 0;
        contents_origin_digest = "";
      }
    in
    let origin =
      {
        origin with
        contents_origin_digest = owned_contents_origin_fingerprint origin;
      }
    in
    session.issued_owned_contents_origins <-
      origin :: session.issued_owned_contents_origins;
    Ok origin

let transition_of_owned_contents_origin_expression expression =
  match expression.Sst.expression_desc with
  | Sst.Field_write { transition = Some transition; _ }
  | Sst.Owned_tree_nested_write { transition; _ }
  | Sst.Owned_tree_rebase { transition } ->
      Some transition
  | Sst.Field_write { transition = None; _ }
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Constructor_value _ | Sst.Field_read _
  | Sst.Shared_scalar_field_write _ | Sst.Match _ | Sst.Let _ | Sst.If _
  | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
  | Sst.Checked_arithmetic _ | Sst.Sequence _ | Sst.Old _
  | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
  | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
  | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
  | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
  | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _ ->
      None

let issue_successor_owned_contents_origin (session : t) ~validated ~caller
    ~expression ~(predecessor : owned_contents_origin) ~predecessor_root
    ~successor_root ~path_condition =
  match transition_of_owned_contents_origin_expression expression with
  | None ->
      Error
        "owned-contents successor origin is not an owned-tree transition"
  | Some transition ->
      if
        not (authenticate_owned_contents_origin session predecessor)
        || predecessor.contents_origin_validated != validated
        || predecessor.contents_origin_caller != caller
        || predecessor.contents_origin_root <> predecessor_root
        || not
             (owned_contents_path_prefix predecessor.contents_origin_path
                (owned_contents_path path_condition))
        || transition.pre_version <> predecessor.contents_origin_version
        || transition.successor_version
           <> predecessor.contents_origin_version + 1
      then
        Error
          "owned-contents successor lost its exact predecessor capability"
      else
        let origin =
          {
            contents_origin_issuer = owned_contents_origin_issuer;
            contents_origin_session = session.session;
            contents_origin_token = ref ();
            contents_origin_validated = validated;
            contents_origin_program_snapshot =
              session.identity.program_snapshot;
            contents_origin_signature_snapshot =
              session.identity.signature_snapshot;
            contents_origin_unit_identity = session.identity.unit_identity;
            contents_origin_cmt_identity = session.identity.cmt_identity;
            contents_origin_family_identity = session.identity.family_identity;
            contents_origin_caller = caller;
            contents_origin_caller_snapshot =
              callable_body_snapshot session caller;
            contents_origin_expression = expression;
            contents_origin_expression_snapshot =
              Marshal.to_string expression [ Marshal.No_sharing ] |> digest;
            contents_origin_path = owned_contents_path path_condition;
            contents_origin_path_digest =
              owned_root_path_digest path_condition;
            contents_origin_predecessor = Some predecessor;
            contents_origin_predecessor_root = Some predecessor_root;
            contents_origin_root = successor_root;
            contents_origin_version = transition.successor_version;
            contents_origin_digest = "";
          }
        in
        let origin =
          {
            origin with
            contents_origin_digest =
              owned_contents_origin_fingerprint origin;
          }
        in
        session.issued_owned_contents_origins <-
          origin :: session.issued_owned_contents_origins;
        Ok origin

let owned_contents_origin_root origin = origin.contents_origin_root
let owned_contents_origin_version origin = origin.contents_origin_version

let make_owned_contents_topology ~root ~nodes ~edges =
  let unique_paths =
    List.fold_left
      (fun paths node ->
        if List.mem node.contents_node_path paths then paths
        else node.contents_node_path :: paths)
      [] nodes
  in
  let unique_values =
    List.fold_left
      (fun values node ->
        if List.mem node.contents_node_value values then values
        else node.contents_node_value :: values)
      [] nodes
  in
  let roots =
    List.filter (fun node -> node.contents_node_path = []) nodes
  in
  let exact_edges =
    List.for_all
      (fun edge ->
        List.exists
          (fun node ->
            node.contents_node_path = edge.contents_edge_parent_path
            && node.contents_node_value = edge.contents_edge_parent)
          nodes
        &&
        List.exists
          (fun node ->
            node.contents_node_path
            = edge.contents_edge_parent_path @ [ edge.contents_edge_field ]
            && node.contents_node_value = edge.contents_edge_child)
          nodes)
      edges
  in
  let exact_incoming =
    List.for_all
      (fun node ->
        let incoming =
          List.filter
            (fun edge ->
              edge.contents_edge_parent_path
                @ [ edge.contents_edge_field ]
              = node.contents_node_path
              && edge.contents_edge_child = node.contents_node_value)
            edges
        in
        if node.contents_node_path = [] then incoming = []
        else List.length incoming = 1)
      nodes
  in
  let unique_edges =
    List.fold_left
      (fun keys edge ->
        let key =
          (edge.contents_edge_parent_path, edge.contents_edge_field)
        in
        if List.mem key keys then keys else key :: keys)
      [] edges
  in
  if nodes = [] || List.length roots <> 1
     || (List.hd roots).contents_node_value <> root
     || List.length unique_paths <> List.length nodes
     || List.length unique_values <> List.length nodes
     || List.length edges <> List.length nodes - 1
     || List.length unique_edges <> List.length edges
     || not exact_edges || not exact_incoming
  then Error "owned-contents topology is not one exact closed rooted tree"
  else
    Ok
      {
        contents_topology_root = root;
        contents_topology_nodes = nodes;
        contents_topology_edges = edges;
      }

let owned_contents_case grammar constructor =
  Sst_validation_private.Owned_recursive_contents_private.cases grammar
  |> List.find_opt (fun case ->
         Sst_validation_private.Owned_recursive_contents_private.case_carrier_constructor case
         = constructor)

let authenticate_owned_contents_topology grammar topology =
  let owned = Sst_validation_private.Owned_recursive_contents_private.owned grammar in
  (match
     make_owned_contents_topology ~root:topology.contents_topology_root
       ~nodes:topology.contents_topology_nodes
       ~edges:topology.contents_topology_edges
   with
  | Ok exact -> exact = topology
  | Error _ -> false)
  &&
  List.for_all
    (fun node ->
      match owned_contents_case grammar node.contents_node_constructor with
      | None -> false
      | Some case ->
          let reached =
            topology.contents_topology_edges
            |> List.filter (fun edge ->
                   edge.contents_edge_parent_path = node.contents_node_path)
            |> List.map (fun edge -> edge.contents_edge_field)
            |> List.sort compare
          in
          List.sort compare
            (Sst_validation_private.Owned_recursive_contents_private.case_recursive_edges case)
          = reached)
    topology.contents_topology_nodes
  &&
  List.for_all
    (fun edge ->
      List.mem edge.contents_edge_field owned.Sst.recursive_edges)
    topology.contents_topology_edges

let make_owned_contents_mapped_graph (session : t) topology =
  let mapped_nodes =
    List.map
      (fun node ->
        {
          contents_mapped_issuer = owned_contents_issuer;
          contents_mapped_session = session.session;
          contents_mapped_token = ref ();
          contents_mapped_path = node.contents_node_path;
          contents_mapped_value = node.contents_node_value;
          contents_mapped_constructor = node.contents_node_constructor;
        })
      topology.contents_topology_nodes
  in
  let mapped_at path value =
    List.find_opt
      (fun candidate ->
        candidate.contents_mapped_path = path
        && candidate.contents_mapped_value = value)
      mapped_nodes
  in
  let rec map_edges mapped = function
    | [] -> Ok (List.rev mapped)
    | edge :: edges -> (
        match
          ( mapped_at edge.contents_edge_parent_path
              edge.contents_edge_parent,
            mapped_at
              (edge.contents_edge_parent_path
              @ [ edge.contents_edge_field ])
              edge.contents_edge_child )
        with
        | Some parent, Some child ->
            map_edges
              ({
                 contents_mapped_edge_parent = parent;
                 contents_mapped_edge_field = edge.contents_edge_field;
                 contents_mapped_edge_child = child;
               }
              :: mapped)
              edges
        | (Some _ | None), (Some _ | None) ->
            Error
              "owned-contents topology cannot map one exact parent/child candidate edge")
  in
  let* mapped_edges =
    map_edges [] topology.contents_topology_edges
  in
  Ok (mapped_nodes, mapped_edges)

let authenticate_owned_contents_mapped_graph (session : t)
    (candidate : owned_contents_candidate) =
  let topology = candidate.contents_candidate_topology in
  let mapped_nodes = candidate.contents_candidate_mapped_nodes
  and mapped_edges = candidate.contents_candidate_mapped_edges in
  let unique_tokens =
    List.fold_left
      (fun tokens mapped ->
        if List.memq mapped.contents_mapped_token tokens then tokens
        else mapped.contents_mapped_token :: tokens)
      [] mapped_nodes
  in
  let mapped_node_exact mapped =
    mapped.contents_mapped_issuer == owned_contents_issuer
    && mapped.contents_mapped_session == session.session
    &&
    List.exists
      (fun node ->
        node.contents_node_path = mapped.contents_mapped_path
        && node.contents_node_value = mapped.contents_mapped_value
        && node.contents_node_constructor
           = mapped.contents_mapped_constructor)
      topology.contents_topology_nodes
  in
  let topology_node_mapped_once node =
    List.length
      (List.filter
         (fun mapped ->
           mapped.contents_mapped_path = node.contents_node_path
           && mapped.contents_mapped_value = node.contents_node_value
           && mapped.contents_mapped_constructor
              = node.contents_node_constructor)
         mapped_nodes)
    = 1
  in
  let mapped_edge_exact mapped =
    List.exists (( == ) mapped.contents_mapped_edge_parent) mapped_nodes
    && List.exists (( == ) mapped.contents_mapped_edge_child) mapped_nodes
    &&
    List.exists
      (fun edge ->
        edge.contents_edge_parent_path
          = mapped.contents_mapped_edge_parent.contents_mapped_path
        && edge.contents_edge_field
           = mapped.contents_mapped_edge_field
        && edge.contents_edge_parent
           = mapped.contents_mapped_edge_parent.contents_mapped_value
        && edge.contents_edge_child
           = mapped.contents_mapped_edge_child.contents_mapped_value
        && mapped.contents_mapped_edge_child.contents_mapped_path
           = mapped.contents_mapped_edge_parent.contents_mapped_path
             @ [ mapped.contents_mapped_edge_field ])
      topology.contents_topology_edges
  in
  let topology_edge_mapped_once edge =
    List.length
      (List.filter
         (fun mapped ->
           mapped.contents_mapped_edge_parent.contents_mapped_path
             = edge.contents_edge_parent_path
           && mapped.contents_mapped_edge_field
              = edge.contents_edge_field
           && mapped.contents_mapped_edge_parent.contents_mapped_value
              = edge.contents_edge_parent
           && mapped.contents_mapped_edge_child.contents_mapped_value
              = edge.contents_edge_child)
         mapped_edges)
    = 1
  in
  List.length mapped_nodes
    = List.length topology.contents_topology_nodes
  && List.length unique_tokens = List.length mapped_nodes
  && List.length mapped_edges
     = List.length topology.contents_topology_edges
  && List.length
       (List.filter
          (fun mapped -> mapped.contents_mapped_path = [])
          mapped_nodes)
     = 1
  && List.for_all mapped_node_exact mapped_nodes
  && List.for_all topology_node_mapped_once
       topology.contents_topology_nodes
  && List.for_all mapped_edge_exact mapped_edges
  && List.for_all topology_edge_mapped_once
       topology.contents_topology_edges

let rec field_path_prefix prefix path =
  match (prefix, path) with
  | [], _ -> true
  | left :: prefix, right :: path when left = right ->
      field_path_prefix prefix path
  | _ :: _, [] | _ :: _, _ :: _ -> false

let strip_field_path_prefix prefix path =
  let rec strip prefix path =
    match (prefix, path) with
    | [], path -> Some path
    | left :: prefix, right :: path when left = right ->
        strip prefix path
    | _ :: _, [] | _ :: _, _ :: _ -> None
  in
  strip prefix path

let guarded_field_path (cursor : Sst.owned_tree_cursor) =
  List.filter_map
    (function
      | Sst.Owned_tree_field field -> Some field
      | Sst.Owned_tree_constructor _ -> None)
    cursor.guarded_path

let topology_node_at topology path =
  List.find_opt
    (fun node -> node.contents_node_path = path)
    topology.contents_topology_nodes

let topology_subtree_matches ~predecessor ~selected ~successor =
  let predecessor_nodes =
    List.filter_map
      (fun node ->
        Option.map
          (fun path -> (path, node))
          (strip_field_path_prefix selected node.contents_node_path))
      predecessor.contents_topology_nodes
  and predecessor_edges =
    List.filter_map
      (fun edge ->
        Option.map
          (fun path -> (path, edge))
          (strip_field_path_prefix selected
             edge.contents_edge_parent_path))
      predecessor.contents_topology_edges
  in
  List.length predecessor_nodes
    = List.length successor.contents_topology_nodes
  && List.length predecessor_edges
     = List.length successor.contents_topology_edges
  && List.for_all
       (fun (path, predecessor_node) ->
         match topology_node_at successor path with
         | Some successor_node ->
             successor_node.contents_node_value
             = predecessor_node.contents_node_value
             && successor_node.contents_node_constructor
                = predecessor_node.contents_node_constructor
         | None -> false)
       predecessor_nodes
  && List.for_all
       (fun (path, predecessor_edge) ->
         List.exists
           (fun successor_edge ->
             successor_edge.contents_edge_parent_path = path
             && successor_edge.contents_edge_field
                = predecessor_edge.contents_edge_field
             && successor_edge.contents_edge_parent
                = predecessor_edge.contents_edge_parent
             && successor_edge.contents_edge_child
                = predecessor_edge.contents_edge_child)
           successor.contents_topology_edges)
       predecessor_edges

let topology_embeds_predecessor predecessor successor =
  let roots =
    List.filter
      (fun node ->
        node.contents_node_value
        = predecessor.contents_topology_root)
      successor.contents_topology_nodes
  in
  match roots with
  | [ root ] ->
      topology_subtree_matches ~predecessor:successor
        ~selected:root.contents_node_path ~successor:predecessor
  | [] | _ :: _ :: _ -> false

let nested_topology_matches ~recursive_target ~changed_parent predecessor
    successor =
  let changed =
    if recursive_target then Some changed_parent else None
  in
  let outside_changed path =
    match changed with
    | Some changed -> not (field_path_prefix changed path)
    | None -> true
  in
  let reconstructed path =
    if recursive_target then
      field_path_prefix path changed_parent
    else field_path_prefix path changed_parent
  in
  let structural_match source target =
    source.contents_node_constructor = target.contents_node_constructor
    &&
    if reconstructed source.contents_node_path then true
    else source.contents_node_value = target.contents_node_value
  in
  List.for_all
    (fun node ->
      (not (outside_changed node.contents_node_path))
      ||
      match topology_node_at successor node.contents_node_path with
      | Some successor_node -> structural_match node successor_node
      | None -> false)
    predecessor.contents_topology_nodes
  && List.for_all
       (fun node ->
         (not (outside_changed node.contents_node_path))
         ||
         match topology_node_at predecessor node.contents_node_path with
         | Some predecessor_node ->
             structural_match predecessor_node node
         | None -> false)
       successor.contents_topology_nodes

let authenticate_owned_contents_successor_relation grammar origin_class
    predecessor successor expression =
  let root_path = Sst_validation_private.Owned_recursive_contents_private.root_path grammar in
  match (origin_class, expression.Sst.expression_desc) with
  | ( Sst_validation_private.Owned_recursive_contents_private
      .Exact_predecessor_transfer_construction,
      Sst.Field_write _ ) ->
      topology_embeds_predecessor predecessor successor
  | ( Sst_validation_private.Owned_recursive_contents_private.Exact_successor_reconstruction,
      Sst.Field_write _ ) ->
      nested_topology_matches ~recursive_target:false ~changed_parent:[]
        predecessor successor
  | ( Sst_validation_private.Owned_recursive_contents_private.Exact_child_reroot,
      Sst.Owned_tree_rebase { transition } ) -> (
      match transition.rhs_provenance with
      | Sst.Guarded_descendant_move cursor -> (
          match strip_field_path_prefix root_path (guarded_field_path cursor) with
          | Some selected ->
              topology_subtree_matches ~predecessor ~selected ~successor
          | None -> false)
      | Sst.Ground_owned_tree_value -> false)
  | ( Sst_validation_private.Owned_recursive_contents_private.Exact_successor_reconstruction,
      Sst.Owned_tree_nested_write { transition; _ } ) -> (
      match transition.cursor with
      | Some cursor -> (
          match strip_field_path_prefix root_path (guarded_field_path cursor) with
          | Some parent ->
              let recursive_target =
                List.mem transition.target_field
                  (Sst_validation_private.Owned_recursive_contents_private.owned grammar)
                    .Sst.recursive_edges
              in
              let changed_parent =
                if recursive_target then
                  parent @ [ transition.target_field ]
                else parent
              in
              nested_topology_matches ~recursive_target ~changed_parent
                predecessor successor
          | None -> false)
      | None -> false)
  | ( Sst_validation_private.Owned_recursive_contents_private.Closed_origin_construction,
      _ )
  | ( Sst_validation_private.Owned_recursive_contents_private
      .Exact_predecessor_transfer_construction,
      _ )
  | ( Sst_validation_private.Owned_recursive_contents_private.Exact_successor_reconstruction,
      _ )
  | ( Sst_validation_private.Owned_recursive_contents_private.Exact_child_reroot,
      _ ) ->
      false

let registered_owned_contents_lineage (session : t)
    (lineage : owned_contents_lineage) =
  List.exists
    (fun exact ->
      exact == lineage
      && exact.contents_lineage_token == lineage.contents_lineage_token)
    session.owned_contents_lineages

let authenticate_owned_contents_lineage (session : t)
    (lineage : owned_contents_lineage) =
  lineage.contents_lineage_issuer == owned_contents_issuer
  && lineage.contents_lineage_session == session.session
  && registered_owned_contents_lineage session lineage
  && session.active
  && lineage.contents_lineage_validated
     ==
     Option.get session.validated
  && String.equal lineage.contents_lineage_program_snapshot
       session.identity.program_snapshot
  && String.equal lineage.contents_lineage_signature_snapshot
       session.identity.signature_snapshot
  && String.equal lineage.contents_lineage_unit_identity
       session.identity.unit_identity
  && String.equal lineage.contents_lineage_cmt_identity
       session.identity.cmt_identity
  && String.equal lineage.contents_lineage_family_identity
       session.identity.family_identity
  && String.equal lineage.contents_lineage_grammar_digest
       (Sst_validation_private.Owned_recursive_contents_private.digest
          lineage.contents_lineage_grammar)
  && String.equal lineage.contents_lineage_model_snapshot
       (callable_body_snapshot session
          (Sst_validation_private.Owned_recursive_contents_private.model
             lineage.contents_lineage_grammar))
  && String.equal lineage.contents_lineage_helper_snapshot
       (callable_body_snapshot session
          (Sst_validation_private.Owned_recursive_contents_private.helper
             lineage.contents_lineage_grammar))
  && String.equal lineage.contents_lineage_caller_snapshot
       (callable_body_snapshot session lineage.contents_lineage_caller)
  && not lineage.contents_lineage_closed
  && not lineage.contents_lineage_invalidated

let canonical_owned_contents_candidate candidate =
  match
    List.find_opt
      (fun exact ->
        exact.contents_candidate_origin
        == candidate.contents_candidate_origin
        && exact.contents_candidate_path
           = candidate.contents_candidate_path)
      candidate.contents_candidate_lineage.contents_lineage_candidates
  with
  | Some exact -> exact == candidate
  | None -> false

let unique_canonical_owned_contents_candidates lineage =
  List.filter canonical_owned_contents_candidate
    lineage.contents_lineage_candidates

let authenticate_owned_contents_lineage_graph session lineage =
  let candidates =
    unique_canonical_owned_contents_candidates lineage
  in
  let roots =
    List.filter
      (fun candidate ->
        Option.is_none candidate.contents_candidate_predecessor)
      candidates
  in
  let successor_keys predecessor =
    candidates
    |> List.filter (fun candidate ->
           match candidate.contents_candidate_predecessor with
           | Some exact -> exact == predecessor
           | None -> false)
    |> List.fold_left
         (fun keys candidate ->
           if
             List.exists
               (fun (origin, path) ->
                 origin == candidate.contents_candidate_origin
                 && path = candidate.contents_candidate_path)
               keys
           then keys
           else
             ( candidate.contents_candidate_origin,
               candidate.contents_candidate_path )
             :: keys)
         []
  in
  candidates <> []
  && authenticate_owned_contents_lineage session lineage
  && List.length roots = 1
  && List.for_all
       (fun candidate ->
         authenticate_owned_contents_topology
           candidate.contents_candidate_grammar
           candidate.contents_candidate_topology
         && authenticate_owned_contents_mapped_graph session candidate
         &&
         match candidate.contents_candidate_predecessor with
         | None ->
             candidate.contents_candidate_root_version = 0
             || Option.is_some lineage.contents_lineage_seed_receipt
         | Some predecessor ->
             predecessor.contents_candidate_lineage == lineage
             && List.exists (( == ) predecessor) candidates
             && predecessor.contents_candidate_root_version
                < candidate.contents_candidate_root_version
             && owned_contents_path_prefix
                  predecessor.contents_candidate_path
                  candidate.contents_candidate_origin.contents_origin_path)
       candidates
  && List.for_all
       (fun predecessor ->
         List.length (successor_keys predecessor) <= 1)
       candidates

let authenticate_owned_contents_receipt (session : t)
    (receipt : owned_contents_receipt) =
  receipt.contents_receipt_issuer == owned_contents_issuer
  && receipt.contents_receipt_session == session.session
  && session.active
  && List.exists
       (fun exact ->
         exact == receipt
         && exact.contents_receipt_token
            == receipt.contents_receipt_token)
       session.owned_contents_receipts
  && receipt.contents_receipt_candidate.contents_candidate_finalized
  && receipt.contents_receipt_origin
     == receipt.contents_receipt_candidate.contents_candidate_origin
  && receipt.contents_receipt_grammar
     == receipt.contents_receipt_candidate.contents_candidate_grammar
  && receipt.contents_receipt_caller
     == receipt.contents_receipt_candidate.contents_candidate_caller
  && receipt.contents_receipt_root
     = receipt.contents_receipt_candidate.contents_candidate_root
  && receipt.contents_receipt_root_version
     = receipt.contents_receipt_candidate.contents_candidate_root_version
  && receipt.contents_receipt_path
     = receipt.contents_receipt_candidate.contents_candidate_path
  && String.equal receipt.contents_receipt_topology_digest
       receipt.contents_receipt_candidate.contents_candidate_topology_digest
  && not receipt.contents_receipt_retired

type owned_contents_predecessor_capability =
  | Provisional_predecessor of owned_contents_candidate
  | Finalized_predecessor of owned_contents_receipt

let select_exact_owned_contents_predecessor candidates path =
  let candidates =
    List.filter
      (fun (candidate_path, _) ->
        owned_contents_path_prefix candidate_path path)
      candidates
  in
  match candidates with
  | [] -> Ok None
  | _ ->
      let depth =
        List.fold_left
          (fun depth (candidate_path, _) ->
            max depth (List.length candidate_path))
          0 candidates
      in
      let exact =
        List.filter
          (fun (candidate_path, _) ->
            List.length candidate_path = depth)
          candidates
      in
      (match exact with
      | [ (_, capability) ] -> Ok (Some capability)
      | [] -> assert false
      | _ :: _ :: _ ->
          Error
            "owned-contents predecessor candidates cross branches or are ambiguous")

let owned_contents_predecessor_capability session grammar caller
    origin_class topology origin =
  match origin.contents_origin_predecessor with
  | None ->
      if
        origin_class
        = Sst_validation_private.Owned_recursive_contents_private.Closed_origin_construction
      then Ok None
      else
        Error
          "owned-contents closed origin has a successor-only classification"
  | Some predecessor ->
      let provisional =
        session.owned_contents_candidates
        |> List.filter (fun candidate ->
               candidate.contents_candidate_origin == predecessor
               && candidate.contents_candidate_grammar == grammar
               && candidate.contents_candidate_caller == caller
               && candidate.contents_candidate_root
                  = predecessor.contents_origin_root
               && candidate.contents_candidate_root_version
                  = predecessor.contents_origin_version
               && candidate.contents_candidate_started
               && candidate.contents_candidate_finished
               && not candidate.contents_candidate_finalized
               && not candidate.contents_candidate_retired
               && canonical_owned_contents_candidate candidate
               && authenticate_owned_contents_lineage session
                    candidate.contents_candidate_lineage
               && authenticate_owned_contents_successor_relation grammar
                    origin_class candidate.contents_candidate_topology
                    topology origin.contents_origin_expression)
        |> List.map (fun candidate ->
               ( candidate.contents_candidate_path,
                 Provisional_predecessor candidate ))
      in
      let finalized =
        session.owned_contents_receipts
        |> List.filter (fun receipt ->
               receipt.contents_receipt_origin == predecessor
               && receipt.contents_receipt_grammar == grammar
               && receipt.contents_receipt_caller == caller
               && authenticate_owned_contents_receipt session receipt
               && authenticate_owned_contents_successor_relation grammar
                    origin_class
                    receipt.contents_receipt_candidate
                      .contents_candidate_topology
                    topology origin.contents_origin_expression)
        |> List.map (fun receipt ->
               ( receipt.contents_receipt_path,
                 Finalized_predecessor receipt ))
      in
      let* provisional =
        select_exact_owned_contents_predecessor provisional
          origin.contents_origin_path
      in
      (match provisional with
      | Some _ -> Ok provisional
      | None ->
          let* finalized =
            select_exact_owned_contents_predecessor finalized
              origin.contents_origin_path
          in
          (match finalized with
          | Some _ -> Ok finalized
          | None ->
              Error
                "owned-contents successor lacks one exact provisional predecessor or finalized seed receipt"))

let reject_owned_contents_candidate session message =
  session.owned_contents_counters.candidates_rejected <-
    session.owned_contents_counters.candidates_rejected + 1;
  Error message

let reject_owned_contents_permit session message =
  session.owned_contents_counters.permits_rejected <-
    session.owned_contents_counters.permits_rejected + 1;
  Error message

let mutate_owned_contents_candidate attack candidate =
  let foreign value = value ^ "\000foreign-" ^ attack in
  match attack with
  | "copy" | "copied" | "replay" ->
      { candidate with contents_candidate_token = ref () }
  | "wrong-session" | "session" ->
      { candidate with contents_candidate_session = ref () }
  | "wrong-program" | "program" ->
      {
        candidate with
        contents_candidate_program_snapshot =
          foreign candidate.contents_candidate_program_snapshot;
      }
  | "wrong-cmt" | "cmt" ->
      {
        candidate with
        contents_candidate_cmt_identity =
          foreign candidate.contents_candidate_cmt_identity;
      }
  | "wrong-family" | "family" ->
      {
        candidate with
        contents_candidate_family_identity =
          foreign candidate.contents_candidate_family_identity;
      }
  | "wrong-model" | "model" ->
      {
        candidate with
        contents_candidate_model_snapshot =
          foreign candidate.contents_candidate_model_snapshot;
      }
  | "wrong-helper" | "helper" | "wrong-body" | "body"
  | "wrong-signature" | "signature" ->
      {
        candidate with
        contents_candidate_helper_snapshot =
          foreign candidate.contents_candidate_helper_snapshot;
      }
  | "wrong-root" | "root" ->
      {
        candidate with
        contents_candidate_root_identity =
          foreign candidate.contents_candidate_root_identity;
      }
  | "stale" | "stale-root" | "wrong-version" | "version" ->
      {
        candidate with
        contents_candidate_root_version =
          candidate.contents_candidate_root_version + 1;
      }
  | "wrong-path" | "path" ->
      {
        candidate with
        contents_candidate_path_digest =
          foreign candidate.contents_candidate_path_digest;
      }
  | "wrong-grammar" | "grammar" | "wrong-edge" | "edge"
  | "wrong-result" | "result" ->
      {
        candidate with
        contents_candidate_grammar_digest =
          foreign candidate.contents_candidate_grammar_digest;
      }
  | "cyclic" | "missing-construction" | "failed-transition"
  | "ambiguous-successor" ->
      {
        candidate with
        contents_candidate_topology_digest =
          foreign candidate.contents_candidate_topology_digest;
      }
  | _ -> { candidate with contents_candidate_token = ref () }

let mutate_owned_contents_topology attack topology =
  match (attack, topology.contents_topology_nodes, topology.contents_topology_edges) with
  | ("cycle" | "cyclic"), root :: _, edge :: _ ->
      {
        topology with
        contents_topology_edges =
          {
            edge with
            contents_edge_parent_path = [];
            contents_edge_parent = root.contents_node_value;
            contents_edge_child = root.contents_node_value;
          }
          :: topology.contents_topology_edges;
      }
  | "shared", root :: child :: rest, _ ->
      {
        topology with
        contents_topology_nodes =
          root
          :: { child with contents_node_value = root.contents_node_value }
          :: rest;
      }
  | "detached", _, _ :: edges ->
      { topology with contents_topology_edges = edges }
  | "duplicate-root", root :: nodes, _ ->
      {
        topology with
        contents_topology_nodes =
          root :: { root with contents_node_value = root.contents_node_value }
          :: nodes;
      }
  | "cross-branch-union", _, edge :: edges ->
      {
        topology with
        contents_topology_edges = edge :: edge :: edges;
      }
  | (("cycle" | "cyclic" | "shared" | "detached" | "duplicate-root"
     | "cross-branch-union"), _, _) ->
      { topology with contents_topology_nodes = [] }
  | _, _, _ -> topology

let open_owned_contents_lineage (session : t) ~validated ~grammar ~caller
    ~seed_receipt =
  let lineage =
    {
      contents_lineage_issuer = owned_contents_issuer;
      contents_lineage_session = session.session;
      contents_lineage_token = ref ();
      contents_lineage_validated = validated;
      contents_lineage_program_snapshot = session.identity.program_snapshot;
      contents_lineage_signature_snapshot =
        session.identity.signature_snapshot;
      contents_lineage_unit_identity = session.identity.unit_identity;
      contents_lineage_cmt_identity = session.identity.cmt_identity;
      contents_lineage_family_identity = session.identity.family_identity;
      contents_lineage_grammar = grammar;
      contents_lineage_grammar_digest =
        Sst_validation_private.Owned_recursive_contents_private.digest
          grammar;
      contents_lineage_model_snapshot =
        callable_body_snapshot session
          (Sst_validation_private.Owned_recursive_contents_private.model
             grammar);
      contents_lineage_helper_snapshot =
        callable_body_snapshot session
          (Sst_validation_private.Owned_recursive_contents_private.helper
             grammar);
      contents_lineage_caller = caller;
      contents_lineage_caller_snapshot =
        callable_body_snapshot session caller;
      contents_lineage_seed_receipt =
        Option.map
          (fun receipt -> receipt.contents_receipt_token)
          seed_receipt;
      contents_lineage_candidates = [];
      contents_lineage_manifest = None;
      contents_lineage_closed = false;
      contents_lineage_invalidated = false;
    }
  in
  session.owned_contents_lineages <-
    lineage :: session.owned_contents_lineages;
  session.owned_contents_counters.lineages_opened <-
    session.owned_contents_counters.lineages_opened + 1;
  lineage

let lineage_for_closed_candidate (session : t) ~validated ~grammar ~caller
    ~origin ~path ~topology_digest =
  let same_root =
    session.owned_contents_candidates
    |> List.filter (fun candidate ->
           candidate.contents_candidate_origin == origin
           && candidate.contents_candidate_grammar == grammar
           && candidate.contents_candidate_caller == caller
           && candidate.contents_candidate_path = path
           && authenticate_owned_contents_lineage session
                candidate.contents_candidate_lineage)
  in
  if
    List.exists
      (fun candidate ->
        not
          (String.equal candidate.contents_candidate_topology_digest
             topology_digest))
      same_root
  then
    Error
      "owned-contents duplicate root has a substituted provisional topology"
  else
  let matches =
    same_root
    |> List.map (fun candidate -> candidate.contents_candidate_lineage)
    |> List.fold_left
         (fun lineages lineage ->
           if List.exists (( == ) lineage) lineages then lineages
           else lineage :: lineages)
         []
  in
  match matches with
  | [ lineage ] -> Ok lineage
  | [] ->
      Ok
        (open_owned_contents_lineage session ~validated ~grammar ~caller
           ~seed_receipt:None)
  | _ :: _ :: _ ->
      Error
        "owned-contents closed candidate is duplicated across provisional lineages"

let lineage_for_finalized_seed (session : t) ~validated ~grammar ~caller
    ~receipt ~origin ~path ~topology_digest =
  let matches =
    List.filter
      (fun lineage ->
        authenticate_owned_contents_lineage session lineage
        &&
        match lineage.contents_lineage_seed_receipt with
        | Some token -> token == receipt.contents_receipt_token
        | None -> false)
      session.owned_contents_lineages
  in
  match matches with
  | [] ->
      Ok
        (open_owned_contents_lineage session ~validated ~grammar ~caller
           ~seed_receipt:(Some receipt))
  | [ lineage ] ->
      if
        List.for_all
          (fun candidate ->
            candidate.contents_candidate_origin == origin
            && candidate.contents_candidate_path = path
            && String.equal candidate.contents_candidate_topology_digest
                 topology_digest)
          lineage.contents_lineage_candidates
      then Ok lineage
      else
        Error
          "owned-contents finalized seed already has a distinct provisional successor"
  | _ :: _ :: _ ->
      Error
        "owned-contents finalized seed was replayed across provisional lineages"

let issue_owned_contents_candidate session ~validated ~grammar ~caller ~call
    ~actual ~root_binding ~(root : Vir.aggregate_term) ~root_version
    ~path_condition ~topology ~origin =
  let topology =
    match !owned_contents_attack_for_testing with
    | Some
        (("cycle" | "cyclic" | "shared" | "detached" | "duplicate-root"
         | "cross-branch-union") as attack) ->
        mutate_owned_contents_topology attack topology
    | Some _ | None -> topology
  in
  let program = Sst_validation.program validated in
  let model = Sst_validation_private.Owned_recursive_contents_private.model grammar in
  let candidate_path = owned_contents_path path_condition in
  let topology_digest =
    Marshal.to_string topology [ Marshal.No_sharing ] |> digest
  in
  let authenticated_origin =
    if not (authenticate_owned_contents_origin session origin) then
      Error "owned-contents origin is copied, stale, foreign, or forged"
    else if origin.contents_origin_root <> root then
      Error "owned-contents origin has the wrong exact current root"
    else if origin.contents_origin_version <> root_version then
      Error "owned-contents origin has a stale current version"
    else
      match origin.contents_origin_predecessor with
      | None ->
          Sst_validation_private.Owned_recursive_contents_private
          .authenticate_closed_origin_expression ~program ~grammar
            ~caller:origin.contents_origin_caller
            ~expression:origin.contents_origin_expression
      | Some _ ->
          Sst_validation_private.Owned_recursive_contents_private
          .authenticate_successor_origin_expression ~program ~grammar
            ~caller:origin.contents_origin_caller
            ~expression:origin.contents_origin_expression
  in
  let exact_call =
    match call.Sst.expression_desc with
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          callee;
          arguments = [ Sst.Value_argument { label = None; value = argument } ];
          recursive = false;
          type_arguments = [];
        } ->
        callee = model.function_id && argument == actual
    | _ -> false
  in
  let exact_actual =
    match actual.Sst.expression_desc with
    | Sst.Variable { binding; _ } ->
        binding == root_binding
        && binding.typ
           = Sst.Aggregate
               (Sst_validation_private.Owned_recursive_contents_private.root_type grammar)
    | _ -> false
  in
  if not session.active then
    reject_owned_contents_candidate session
      "owned-contents verification session is destroyed"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then
    reject_owned_contents_candidate session
      "owned-contents candidate has the wrong validated program"
  else if
    not
      (Sst_validation_private.Owned_recursive_contents_private.authenticate ~program ~model grammar)
  then
    reject_owned_contents_candidate session
      "owned-contents grammar is stale, foreign, or forged"
  else if not exact_call || not exact_actual then
    reject_owned_contents_candidate session
      "owned-contents call is not the exact current rooted model call"
  else if root_version < 0 then
    reject_owned_contents_candidate session
      "owned-contents root version is invalid"
  else if
    origin.contents_origin_caller != caller
    || not
         (owned_contents_path_prefix origin.contents_origin_path
            candidate_path)
  then
    reject_owned_contents_candidate session
      "owned-contents candidate crossed its exact function or feasible origin path"
  else if
    List.exists (( == ) origin) session.retired_owned_contents_origins
  then
    reject_owned_contents_candidate session
      "owned-contents predecessor origin is stale or retired"
  else if
    topology.contents_topology_root.aggregate_type
    <> {
         Vir.aggregate_type_index =
           (Sst_validation_private.Owned_recursive_contents_private.carrier_type grammar).type_index;
         aggregate_type_name =
           (Sst_validation_private.Owned_recursive_contents_private.carrier_type grammar).type_name;
      aggregate_type_arguments = [];
       }
    || not (authenticate_owned_contents_topology grammar topology)
  then
    reject_owned_contents_candidate session
      "owned-contents topology is incomplete, cyclic, shared, or substituted"
  else
    match authenticated_origin with
    | Error message ->
        reject_owned_contents_candidate session message
    | Ok origin_class ->
        let* predecessor =
          match
            owned_contents_predecessor_capability session grammar caller
              origin_class topology origin
          with
          | Ok predecessor -> Ok predecessor
          | Error message ->
              reject_owned_contents_candidate session message
        in
        let* lineage =
          match predecessor with
          | Some (Provisional_predecessor predecessor) ->
              let lineage = predecessor.contents_candidate_lineage in
              if
                List.exists
                  (fun candidate ->
                    canonical_owned_contents_candidate candidate
                    &&
                    match candidate.contents_candidate_predecessor with
                    | Some exact when exact == predecessor ->
                        candidate.contents_candidate_origin != origin
                        || candidate.contents_candidate_path
                           <> candidate_path
                    | Some _ | None -> false)
                  lineage.contents_lineage_candidates
              then
                reject_owned_contents_candidate session
                  "owned-contents predecessor already has a distinct provisional successor branch"
              else Ok lineage
          | Some (Finalized_predecessor receipt) ->
              lineage_for_finalized_seed session ~validated ~grammar
                ~caller ~receipt ~origin ~path:candidate_path
                ~topology_digest
          | None ->
              lineage_for_closed_candidate session ~validated ~grammar
                ~caller ~origin ~path:candidate_path ~topology_digest
        in
        if not (authenticate_owned_contents_lineage session lineage) then
          reject_owned_contents_candidate session
            "owned-contents provisional lineage is stale, foreign, or closed"
        else
        let* mapped_nodes, mapped_edges =
          match make_owned_contents_mapped_graph session topology with
          | Ok mapped -> Ok mapped
          | Error message ->
              reject_owned_contents_candidate session message
        in
        let candidate_caller =
          match !owned_contents_attack_for_testing with
          | Some "cross-function" ->
              Sst_validation_private.Owned_recursive_contents_private
              .helper grammar
          | Some _ | None -> caller
        in
        let candidate_lineage =
          match !owned_contents_attack_for_testing with
          | Some "stale-lineage-replay" ->
              { lineage with contents_lineage_token = ref () }
          | Some _ | None -> lineage
        in
        let candidate_mapped_edges =
          match
            ( !owned_contents_attack_for_testing,
              mapped_edges )
          with
          | Some "wrong-mapped-child", edge :: edges ->
              {
                edge with
                contents_mapped_edge_child =
                  edge.contents_mapped_edge_parent;
              }
              :: edges
          | Some "wrong-mapped-child", [] | (Some _ | None), _ ->
              mapped_edges
        in
        let candidate =
          {
        contents_candidate_issuer = owned_contents_issuer;
        contents_candidate_session = session.session;
        contents_candidate_token = ref ();
        contents_candidate_validated = validated;
        contents_candidate_program_snapshot = session.identity.program_snapshot;
        contents_candidate_signature_snapshot =
          session.identity.signature_snapshot;
        contents_candidate_unit_identity = session.identity.unit_identity;
        contents_candidate_cmt_identity = session.identity.cmt_identity;
        contents_candidate_family_identity = session.identity.family_identity;
        contents_candidate_grammar = grammar;
        contents_candidate_grammar_digest =
          Sst_validation_private.Owned_recursive_contents_private.digest grammar;
        contents_candidate_model_snapshot =
          callable_body_snapshot session model;
        contents_candidate_helper_snapshot =
          callable_body_snapshot session
            (Sst_validation_private.Owned_recursive_contents_private.helper grammar);
        contents_candidate_caller = candidate_caller;
        contents_candidate_call = call;
        contents_candidate_actual = actual;
        contents_candidate_root_binding = root_binding;
        contents_candidate_root = root;
        contents_candidate_root_identity = owned_root_version root;
        contents_candidate_root_version = root_version;
        contents_candidate_path_digest =
          owned_root_path_digest path_condition;
        contents_candidate_path = candidate_path;
        contents_candidate_topology = topology;
        contents_candidate_topology_digest = topology_digest;
        contents_candidate_mapped_nodes = mapped_nodes;
        contents_candidate_mapped_edges = candidate_mapped_edges;
        contents_candidate_origin = origin;
        contents_candidate_origin_class = origin_class;
        contents_candidate_origin_digest = origin.contents_origin_digest;
        contents_candidate_lineage = candidate_lineage;
        contents_candidate_predecessor =
          (match predecessor with
          | Some (Provisional_predecessor predecessor) ->
              Some predecessor
          | Some (Finalized_predecessor _) | None -> None);
        contents_candidate_started = false;
        contents_candidate_finished = false;
        contents_candidate_finalized = false;
        contents_candidate_retired = false;
          }
        in
        session.owned_contents_candidates <-
          candidate :: session.owned_contents_candidates;
        lineage.contents_lineage_candidates <-
          lineage.contents_lineage_candidates @ [ candidate ];
        session.owned_contents_counters.lineage_graph_authentications <-
          session.owned_contents_counters.lineage_graph_authentications + 1;
        session.owned_contents_counters.mapped_candidates_authenticated <-
          session.owned_contents_counters.mapped_candidates_authenticated
          + List.length mapped_nodes;
        session.owned_contents_counters.candidates_issued <-
          session.owned_contents_counters.candidates_issued + 1;
        Ok
          (match !owned_contents_attack_for_testing with
          | None
          | Some
              ("cross-function" | "stale-lineage-replay"
              | "wrong-mapped-child") ->
              candidate
          | Some attack -> mutate_owned_contents_candidate attack candidate)

let registered_owned_contents_candidate session candidate =
  List.find_opt
    (fun exact ->
      exact.contents_candidate_token == candidate.contents_candidate_token)
    session.owned_contents_candidates

let authenticate_owned_contents_candidate session candidate =
  match registered_owned_contents_candidate session candidate with
  | None -> false
  | Some exact ->
      candidate == exact
      && candidate.contents_candidate_issuer == owned_contents_issuer
      && candidate.contents_candidate_session == session.session
      && session.active
      && candidate.contents_candidate_validated
         ==
         Option.get session.validated
      && String.equal candidate.contents_candidate_program_snapshot
           session.identity.program_snapshot
      && String.equal candidate.contents_candidate_signature_snapshot
           session.identity.signature_snapshot
      && String.equal candidate.contents_candidate_unit_identity
           session.identity.unit_identity
      && String.equal candidate.contents_candidate_cmt_identity
           session.identity.cmt_identity
      && String.equal candidate.contents_candidate_family_identity
           session.identity.family_identity
      &&
      let program = Sst_validation.program candidate.contents_candidate_validated in
      let grammar = candidate.contents_candidate_grammar in
      Sst_validation_private.Owned_recursive_contents_private.authenticate ~program
        ~model:(Sst_validation_private.Owned_recursive_contents_private.model grammar) grammar
      && String.equal candidate.contents_candidate_grammar_digest
           (Sst_validation_private.Owned_recursive_contents_private.digest grammar)
      && String.equal candidate.contents_candidate_model_snapshot
           (callable_body_snapshot session
              (Sst_validation_private.Owned_recursive_contents_private.model grammar))
      && String.equal candidate.contents_candidate_helper_snapshot
           (callable_body_snapshot session
              (Sst_validation_private.Owned_recursive_contents_private.helper grammar))
      &&
      (match candidate.contents_candidate_call.Sst.expression_desc with
      | Sst.Direct_call
          {
            call_form = Sst.Specification_call;
            callee;
            arguments = [ Sst.Value_argument { label = None; value = actual } ];
            recursive = false;
            type_arguments = [];
          } ->
          callee
          = (Sst_validation_private.Owned_recursive_contents_private.model grammar).function_id
          && actual == candidate.contents_candidate_actual
          &&
          (match actual.Sst.expression_desc with
          | Sst.Variable { binding; _ } ->
              binding == candidate.contents_candidate_root_binding
          | _ -> false)
      | _ -> false)
      && String.equal candidate.contents_candidate_root_identity
           (owned_root_version candidate.contents_candidate_root)
      && String.equal candidate.contents_candidate_path_digest
           (String.concat "\000" candidate.contents_candidate_path
           |> digest)
      && String.equal candidate.contents_candidate_topology_digest
           (Marshal.to_string candidate.contents_candidate_topology
              [ Marshal.No_sharing ]
           |> digest)
      && String.equal candidate.contents_candidate_origin_digest
           candidate.contents_candidate_origin.contents_origin_digest
      && authenticate_owned_contents_origin session
           candidate.contents_candidate_origin
      && candidate.contents_candidate_origin.contents_origin_caller
         == candidate.contents_candidate_caller
      && owned_contents_path_prefix
           candidate.contents_candidate_origin.contents_origin_path
           candidate.contents_candidate_path
      && authenticate_owned_contents_topology grammar
           candidate.contents_candidate_topology
      && authenticate_owned_contents_mapped_graph session candidate
      && authenticate_owned_contents_lineage session
           candidate.contents_candidate_lineage
      && candidate.contents_candidate_lineage.contents_lineage_grammar
         == grammar
      && candidate.contents_candidate_lineage.contents_lineage_caller
         == candidate.contents_candidate_caller
      && List.exists (( == ) candidate)
           candidate.contents_candidate_lineage.contents_lineage_candidates
      &&
      (match
         candidate.contents_candidate_origin.contents_origin_predecessor
       with
      | None -> (
          match
            Sst_validation_private.Owned_recursive_contents_private
            .authenticate_closed_origin_expression ~program ~grammar
              ~caller:
                candidate.contents_candidate_origin.contents_origin_caller
              ~expression:
                candidate.contents_candidate_origin.contents_origin_expression
          with
          | Ok origin_class ->
              origin_class = candidate.contents_candidate_origin_class
          | Error _ -> false)
      | Some _ -> (
          match
            Sst_validation_private.Owned_recursive_contents_private
            .authenticate_successor_origin_expression ~program ~grammar
              ~caller:
                candidate.contents_candidate_origin.contents_origin_caller
              ~expression:
                candidate.contents_candidate_origin.contents_origin_expression
          with
          | Ok origin_class ->
              origin_class = candidate.contents_candidate_origin_class
          | Error _ -> false))
      &&
      (match
         owned_contents_predecessor_capability session grammar
           candidate.contents_candidate_caller
           candidate.contents_candidate_origin_class
           candidate.contents_candidate_topology
           candidate.contents_candidate_origin
       with
      | Ok None ->
          candidate.contents_candidate_predecessor = None
          && candidate.contents_candidate_lineage
               .contents_lineage_seed_receipt
             = None
      | Ok (Some (Provisional_predecessor predecessor)) ->
          (match candidate.contents_candidate_predecessor with
          | Some exact -> exact == predecessor
          | None -> false)
          && predecessor.contents_candidate_lineage
             == candidate.contents_candidate_lineage
      | Ok (Some (Finalized_predecessor receipt)) ->
          Option.is_none candidate.contents_candidate_predecessor
          &&
          (match
             candidate.contents_candidate_lineage
               .contents_lineage_seed_receipt
           with
          | Some token -> token == receipt.contents_receipt_token
          | None -> false)
      | Error _ -> false)
      && not
           (List.exists (( == ) candidate.contents_candidate_origin)
              session.retired_owned_contents_origins)
      && not candidate.contents_candidate_retired

let consume_owned_contents_candidate session candidate =
  if not (authenticate_owned_contents_candidate session candidate) then
    reject_owned_contents_candidate session
      "owned-contents candidate is copied, stale, foreign, or forged"
  else if candidate.contents_candidate_started then
    reject_owned_contents_candidate session
      "owned-contents candidate was replayed"
  else (
    candidate.contents_candidate_started <- true;
    session.owned_contents_counters.candidates_consumed <-
      session.owned_contents_counters.candidates_consumed + 1;
    Ok ())

let owned_contents_candidate_grammar candidate =
  candidate.contents_candidate_grammar

let owned_contents_candidate_topology_root candidate =
  candidate.contents_candidate_topology.contents_topology_root

let issue_owned_contents_permit session candidate ~parent_path ~field ~parent
    ~child ~call ~call_span ~path_condition =
  let permit_path = owned_contents_path path_condition in
  let exact_edge =
    List.exists
      (fun edge ->
        edge.contents_edge_parent_path = parent_path
        && edge.contents_edge_field = field
        && edge.contents_edge_parent = parent
        && edge.contents_edge_child = child)
      candidate.contents_candidate_topology.contents_topology_edges
  in
  let exact_mapped_edges =
    List.filter
      (fun edge ->
        edge.contents_mapped_edge_parent.contents_mapped_path
          = parent_path
        && edge.contents_mapped_edge_field = field
        && edge.contents_mapped_edge_parent.contents_mapped_value
           = parent
        && edge.contents_mapped_edge_child.contents_mapped_value
           = child)
      candidate.contents_candidate_mapped_edges
  in
  let exact_grammar_position =
    match
      topology_node_at candidate.contents_candidate_topology parent_path
    with
    | None -> false
    | Some node -> (
        match
          owned_contents_case candidate.contents_candidate_grammar
            node.contents_node_constructor
        with
        | None -> false
        | Some case ->
            let sources =
              Sst_validation_private.Owned_recursive_contents_private.case_sources case
              |> List.filter (function
                   | Sst_validation_private.Owned_recursive_contents_private.Recursive_source
                       (candidate, source) ->
                       candidate = field && source == call
                   | Sst_validation_private.Owned_recursive_contents_private.Scalar_source _
                   | Sst_validation_private.Owned_recursive_contents_private.Constant_source ->
                       false)
            in
            List.length sources = 1)
  in
  if
    not (authenticate_owned_contents_candidate session candidate)
    || not
         (authenticate_owned_contents_lineage session
            candidate.contents_candidate_lineage)
    || not
         (authenticate_owned_contents_lineage_graph session
            candidate.contents_candidate_lineage)
    || not
         (authenticate_owned_contents_topology
            candidate.contents_candidate_grammar
            candidate.contents_candidate_topology)
    || not
         (owned_contents_path_prefix
            candidate.contents_candidate_path permit_path)
    || not candidate.contents_candidate_started
    || candidate.contents_candidate_finished
  then
    reject_owned_contents_permit session
      "owned-contents permit candidate is unavailable or stale"
  else if not exact_edge then
    reject_owned_contents_permit session
      "owned-contents permit does not name one exact authenticated child"
  else if not exact_grammar_position then
    reject_owned_contents_permit session
      "owned-contents permit is not bound to the exact retained recursive-result position"
  else if
    List.exists
      (fun permit ->
        permit.contents_permit_candidate == candidate
        && permit.contents_permit_parent_path = parent_path
        && permit.contents_permit_field = field)
      session.owned_contents_permits
  then
    reject_owned_contents_permit session
      "owned-contents exact-child permit was duplicated"
  else
    match exact_mapped_edges with
    | [] | _ :: _ :: _ ->
        reject_owned_contents_permit session
          "owned-contents permit lacks one exact mapped parent/child candidate"
    | [ mapped_edge ] ->
    let permit =
      {
        contents_permit_issuer = owned_contents_issuer;
        contents_permit_session = session.session;
        contents_permit_token = ref ();
        contents_permit_lineage =
          candidate.contents_candidate_lineage;
        contents_permit_candidate = candidate;
        contents_permit_parent_candidate =
          mapped_edge.contents_mapped_edge_parent;
        contents_permit_child_candidate =
          mapped_edge.contents_mapped_edge_child;
        contents_permit_parent_path = parent_path;
        contents_permit_field = field;
        contents_permit_parent = parent;
        contents_permit_child = child;
        contents_permit_call = call;
        contents_permit_call_snapshot =
          Marshal.to_string call [ Marshal.No_sharing ] |> digest;
        contents_permit_call_span = call_span;
        contents_permit_path_digest =
          owned_root_path_digest path_condition;
        contents_permit_consumed = false;
        contents_permit_retired = false;
      }
    in
    session.owned_contents_permits <-
      permit :: session.owned_contents_permits;
    session.owned_contents_counters.permits_issued <-
      session.owned_contents_counters.permits_issued + 1;
    Ok permit

let consume_owned_contents_permit session permit ~candidate ~parent_path
    ~field ~parent ~child ~call ~call_span ~path_condition =
  let exact =
    List.find_opt
      (fun exact ->
        exact.contents_permit_token == permit.contents_permit_token)
      session.owned_contents_permits
  in
  match exact with
  | Some exact
    when permit == exact
         && permit.contents_permit_issuer == owned_contents_issuer
         && permit.contents_permit_session == session.session
         && permit.contents_permit_lineage
            == candidate.contents_candidate_lineage
         && authenticate_owned_contents_lineage session
              permit.contents_permit_lineage
         && authenticate_owned_contents_lineage_graph session
              permit.contents_permit_lineage
         && permit.contents_permit_candidate == candidate
         && authenticate_owned_contents_candidate session candidate
         && List.exists
              (( == ) permit.contents_permit_parent_candidate)
              candidate.contents_candidate_mapped_nodes
         && List.exists
              (( == ) permit.contents_permit_child_candidate)
              candidate.contents_candidate_mapped_nodes
         && List.exists
              (fun edge ->
                edge.contents_mapped_edge_parent
                  == permit.contents_permit_parent_candidate
                && edge.contents_mapped_edge_field
                   = permit.contents_permit_field
                && edge.contents_mapped_edge_child
                   == permit.contents_permit_child_candidate)
              candidate.contents_candidate_mapped_edges
         && permit.contents_permit_parent_candidate.contents_mapped_path
            = parent_path
         && permit.contents_permit_parent_candidate.contents_mapped_value
            = parent
         && permit.contents_permit_child_candidate.contents_mapped_path
            = parent_path @ [ field ]
         && permit.contents_permit_child_candidate.contents_mapped_value
            = child
         && permit.contents_permit_parent_path = parent_path
         && permit.contents_permit_field = field
         && permit.contents_permit_parent = parent
         && permit.contents_permit_child = child
         && permit.contents_permit_call == call
         && String.equal permit.contents_permit_call_snapshot
              (Marshal.to_string call [ Marshal.No_sharing ] |> digest)
         && permit.contents_permit_call_span = call_span
         && String.equal permit.contents_permit_path_digest
              (owned_root_path_digest path_condition)
         && not permit.contents_permit_consumed
         && not permit.contents_permit_retired ->
      permit.contents_permit_consumed <- true;
      session.owned_contents_counters.permits_consumed <-
        session.owned_contents_counters.permits_consumed + 1;
      Ok ()
  | Some _ | None ->
      reject_owned_contents_permit session
        "owned-contents child permit is copied, replayed, stale, or foreign"

let note_owned_contents_recursive_route session candidate =
  if
    not (authenticate_owned_contents_candidate session candidate)
    || not
         (authenticate_owned_contents_lineage_graph session
            candidate.contents_candidate_lineage)
    || not
         (authenticate_owned_contents_topology
            candidate.contents_candidate_grammar
            candidate.contents_candidate_topology)
  then
    reject_owned_contents_candidate session
      "owned-contents route lost its candidate"
  else (
    session.owned_contents_counters.recursive_routes <-
      session.owned_contents_counters.recursive_routes + 1;
    Ok ())

let note_owned_contents_ground_equation session candidate =
  if
    not (authenticate_owned_contents_candidate session candidate)
    || not
         (authenticate_owned_contents_lineage_graph session
            candidate.contents_candidate_lineage)
    || not
         (authenticate_owned_contents_topology
            candidate.contents_candidate_grammar
            candidate.contents_candidate_topology)
  then
    reject_owned_contents_candidate session
      "owned-contents equation lost its candidate"
  else (
    session.owned_contents_counters.ground_equations <-
      session.owned_contents_counters.ground_equations + 1;
    Ok ())

let note_owned_contents_model_result session candidate =
  if
    not (authenticate_owned_contents_candidate session candidate)
    || not
         (authenticate_owned_contents_lineage_graph session
            candidate.contents_candidate_lineage)
    || not
         (authenticate_owned_contents_topology
            candidate.contents_candidate_grammar
            candidate.contents_candidate_topology)
  then
    reject_owned_contents_candidate session
      "owned-contents result lost its candidate"
  else (
    session.owned_contents_counters.model_results <-
      session.owned_contents_counters.model_results + 1;
    Ok ())

let finish_owned_contents_candidate session candidate =
  let permits =
    List.filter
      (fun permit -> permit.contents_permit_candidate == candidate)
      session.owned_contents_permits
  in
  if
    not (authenticate_owned_contents_candidate session candidate)
    || not
         (authenticate_owned_contents_lineage_graph session
            candidate.contents_candidate_lineage)
    || not candidate.contents_candidate_started
    || candidate.contents_candidate_finished
  then
    reject_owned_contents_candidate session
      "owned-contents candidate cannot finish twice or while unauthenticated"
  else if
    List.length permits
    <> List.length
         candidate.contents_candidate_topology.contents_topology_edges
    || not
         (List.for_all
            (fun permit -> permit.contents_permit_consumed)
            permits)
  then
    reject_owned_contents_candidate session
      "owned-contents candidate has incomplete exact-child permit coverage"
  else (
    candidate.contents_candidate_finished <- true;
    session.owned_contents_counters.candidates_finished <-
      session.owned_contents_counters.candidates_finished + 1;
    Ok ())

let owned_contents_obligation_fingerprint obligation =
  Marshal.to_string obligation [ Marshal.No_sharing ] |> digest

let owned_contents_obligation_set_fingerprint obligations =
  String.concat "\000" obligations |> digest

let unique_owned_contents_lineages candidates =
  List.fold_left
    (fun lineages candidate ->
      let lineage = candidate.contents_candidate_lineage in
      if List.exists (( == ) lineage) lineages then lineages
      else lineage :: lineages)
    [] candidates

let authenticate_complete_owned_contents_lineage session lineage =
  authenticate_owned_contents_lineage_graph session lineage
  && List.for_all
       (fun candidate ->
         candidate.contents_candidate_started
         && candidate.contents_candidate_finished
         && not candidate.contents_candidate_finalized
         && not candidate.contents_candidate_retired)
       lineage.contents_lineage_candidates

let invalidate_owned_contents_lineages session lineages =
  List.iter
    (fun lineage ->
      if
        registered_owned_contents_lineage session lineage
        && not lineage.contents_lineage_closed
        && not lineage.contents_lineage_invalidated
      then (
        lineage.contents_lineage_invalidated <- true;
        session.owned_contents_counters.lineages_invalidated <-
          session.owned_contents_counters.lineages_invalidated + 1;
        List.iter
          (fun candidate ->
            if not candidate.contents_candidate_finalized then
              candidate.contents_candidate_retired <- true)
          lineage.contents_lineage_candidates;
        List.iter
          (fun permit ->
            if
              permit.contents_permit_lineage == lineage
              && not permit.contents_permit_retired
            then (
              permit.contents_permit_retired <- true;
              session.owned_contents_counters.permit_retirements <-
                session.owned_contents_counters.permit_retirements + 1))
          session.owned_contents_permits))
    lineages

let retire_owned_contents_receipt session receipt =
  if not (authenticate_owned_contents_receipt session receipt) then false
  else (
    receipt.contents_receipt_retired <- true;
    let origin = receipt.contents_receipt_origin in
    if
      not
        (List.exists (( == ) origin)
           session.retired_owned_contents_origins)
    then (
      session.retired_owned_contents_origins <-
        origin :: session.retired_owned_contents_origins;
      session.owned_contents_counters.predecessor_retirements <-
        session.owned_contents_counters.predecessor_retirements + 1);
    true)

let authorize_owned_contents_obligations session definition execution =
  let candidates =
    List.filter
      (fun candidate ->
        candidate.contents_candidate_caller == definition
        && candidate.contents_candidate_finished
        && not candidate.contents_candidate_finalized
        && not candidate.contents_candidate_retired)
      session.owned_contents_candidates
    |> List.sort (fun left right ->
           Int.compare left.contents_candidate_root_version
             right.contents_candidate_root_version)
  in
  if candidates = [] then Ok None
  else if
    List.exists
      (fun manifest -> manifest.contents_manifest_definition == definition)
      session.owned_contents_manifests
  then Error "owned-contents obligation manifest was duplicated"
  else
    let lineages = unique_owned_contents_lineages candidates in
    let complete_lineages =
      List.for_all
        (fun lineage ->
          lineage.contents_lineage_caller == definition
          && Option.is_none lineage.contents_lineage_manifest
          && authenticate_complete_owned_contents_lineage session lineage
          && List.for_all
               (fun candidate ->
                 candidate.contents_candidate_caller == definition
                 && candidate.contents_candidate_finished
                 && List.exists (( == ) candidate) candidates)
               lineage.contents_lineage_candidates)
        lineages
    in
    if not complete_lineages then
      Error
        "owned-contents complete provisional lineage graph is detached, ambiguous, or unfinished"
    else
    let obligations =
      List.map owned_contents_obligation_fingerprint execution.Vir.obligations
    in
    let manifest =
      {
        contents_manifest_issuer = owned_contents_issuer;
        contents_manifest_session = session.session;
        contents_manifest_token = ref ();
        contents_manifest_definition = definition;
        contents_manifest_lineages = lineages;
        contents_manifest_candidates = candidates;
        contents_manifest_obligations = obligations;
        contents_manifest_set_digest =
          owned_contents_obligation_set_fingerprint obligations;
      }
    in
    List.iter
      (fun lineage ->
        lineage.contents_lineage_manifest <-
          Some manifest.contents_manifest_token)
      lineages;
    session.owned_contents_manifests <-
      manifest :: session.owned_contents_manifests;
    session.owned_contents_counters.manifests_issued <-
      session.owned_contents_counters.manifests_issued + 1;
    Ok (Some manifest)

let complete_owned_contents session manifest execution results =
  match manifest with
  | None -> Ok true
  | Some manifest ->
      let registered =
        List.exists
          (fun exact ->
            exact.contents_manifest_token
            == manifest.contents_manifest_token)
          session.owned_contents_manifests
      in
      let result_obligations =
        List.map
          (fun (result : Solver_backend.obligation_result) ->
            owned_contents_obligation_fingerprint result.obligation)
          results
      in
      let authenticated =
        not registered
        = false
        && manifest.contents_manifest_issuer == owned_contents_issuer
        && manifest.contents_manifest_session == session.session
        && manifest.contents_manifest_obligations
           = List.map owned_contents_obligation_fingerprint
               execution.Vir.obligations
        && String.equal manifest.contents_manifest_set_digest
             (owned_contents_obligation_set_fingerprint
                manifest.contents_manifest_obligations)
        && manifest.contents_manifest_obligations = result_obligations
        && List.for_all
             (fun lineage ->
               authenticate_complete_owned_contents_lineage session lineage
               &&
               match lineage.contents_lineage_manifest with
               | Some token -> token == manifest.contents_manifest_token
               | None -> false)
             manifest.contents_manifest_lineages
        && unique_owned_contents_lineages
             manifest.contents_manifest_candidates
           |> fun lineages ->
           List.length lineages
             = List.length manifest.contents_manifest_lineages
           && List.for_all
                (fun lineage ->
                  List.exists (( == ) lineage)
                    manifest.contents_manifest_lineages)
                lineages
        && List.for_all
             (fun candidate ->
               candidate.contents_candidate_caller
               == manifest.contents_manifest_definition
               && authenticate_owned_contents_candidate session candidate
               && candidate.contents_candidate_started
               && candidate.contents_candidate_finished
               && not candidate.contents_candidate_finalized
               && not candidate.contents_candidate_retired)
             manifest.contents_manifest_candidates
        && List.length manifest.contents_manifest_candidates
           = (List.fold_left
                (fun tokens candidate ->
                  if List.memq candidate.contents_candidate_token tokens
                  then tokens
                  else candidate.contents_candidate_token :: tokens)
                [] manifest.contents_manifest_candidates
             |> List.length)
      in
      let verified =
        List.for_all
          (fun (result : Solver_backend.obligation_result) ->
            result.outcome = Solver_backend.Verified)
          results
      in
      if not authenticated || not verified then (
        invalidate_owned_contents_lineages session
          manifest.contents_manifest_lineages;
        session.owned_contents_manifests <-
          List.filter
            (fun exact ->
              exact.contents_manifest_token
              != manifest.contents_manifest_token)
            session.owned_contents_manifests;
        Ok false)
      else (
        Option.iter
          (fun observe -> observe manifest execution results)
          !owned_contents_seed_completion_observer;
        session.owned_contents_counters.completions <-
          session.owned_contents_counters.completions + 1;
        let new_receipts =
          manifest.contents_manifest_lineages
          |> List.concat_map
               unique_canonical_owned_contents_candidates
          |> List.filter_map (fun candidate ->
                 if
                   List.exists
                     (fun receipt ->
                       receipt.contents_receipt_origin
                       == candidate.contents_candidate_origin
                       && receipt.contents_receipt_path
                          = candidate.contents_candidate_path)
                     session.owned_contents_receipts
                 then None
                 else
                   Some
                     {
                       contents_receipt_issuer = owned_contents_issuer;
                       contents_receipt_session = session.session;
                       contents_receipt_token = ref ();
                       contents_receipt_candidate = candidate;
                       contents_receipt_origin =
                         candidate.contents_candidate_origin;
                       contents_receipt_grammar =
                         candidate.contents_candidate_grammar;
                       contents_receipt_caller =
                         candidate.contents_candidate_caller;
                       contents_receipt_root =
                         candidate.contents_candidate_root;
                       contents_receipt_root_version =
                         candidate.contents_candidate_root_version;
                       contents_receipt_path =
                         candidate.contents_candidate_path;
                       contents_receipt_topology_digest =
                         candidate.contents_candidate_topology_digest;
                       contents_receipt_retired = false;
                     })
        in
        List.iter
          (fun candidate ->
            candidate.contents_candidate_finalized <- true)
          manifest.contents_manifest_candidates;
        session.owned_contents_receipts <-
          new_receipts @ session.owned_contents_receipts;
        session.owned_contents_counters.receipts_finalized <-
          session.owned_contents_counters.receipts_finalized
          + List.length new_receipts;
        session.owned_contents_counters.successor_receipts <-
          session.owned_contents_counters.successor_receipts
          + List.fold_left
              (fun count receipt ->
                if receipt.contents_receipt_root_version > 0 then
                  count + 1
                else count)
              0 new_receipts;
        List.iter
          (fun receipt ->
            match receipt.contents_receipt_origin.contents_origin_predecessor with
            | None -> ()
            | Some predecessor_origin ->
                let predecessors =
                  session.owned_contents_receipts
                  |> List.filter (fun predecessor ->
                         predecessor.contents_receipt_origin
                         == predecessor_origin
                         && not predecessor.contents_receipt_retired
                         && owned_contents_path_prefix
                              predecessor.contents_receipt_path
                              receipt.contents_receipt_origin
                                .contents_origin_path)
                in
                let predecessors =
                  match predecessors with
                  | [] -> []
                  | _ ->
                      let depth =
                        List.fold_left
                          (fun depth predecessor ->
                            max depth
                              (List.length
                                 predecessor.contents_receipt_path))
                          0 predecessors
                      in
                      List.filter
                        (fun predecessor ->
                          List.length predecessor.contents_receipt_path
                          = depth)
                        predecessors
                in
                (match predecessors with
                | [ predecessor ] ->
                    ignore
                      (retire_owned_contents_receipt session predecessor)
                | [] -> ()
                | _ :: _ :: _ -> assert false))
          new_receipts;
        List.iter
          (fun lineage ->
            lineage.contents_lineage_closed <- true;
            session.owned_contents_counters.lineages_closed <-
              session.owned_contents_counters.lineages_closed + 1;
            List.iter
              (fun permit ->
                if
                  permit.contents_permit_lineage == lineage
                  && not permit.contents_permit_retired
                then (
                  permit.contents_permit_retired <- true;
                  session.owned_contents_counters.permit_retirements <-
                    session.owned_contents_counters.permit_retirements + 1))
              session.owned_contents_permits)
          manifest.contents_manifest_lineages;
        session.owned_contents_manifests <-
          List.filter
            (fun exact ->
              exact.contents_manifest_token
              != manifest.contents_manifest_token)
            session.owned_contents_manifests;
        Ok true)

let callee_snapshot session ~validated ~invariants
    (definition : Sst.function_definition) =
  if not session.active then Error "verification session is destroyed"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then
    Error "validated program does not belong to this verification session"
  else if
    not
      (match session.invariants with
      | Some owned -> owned == invariants
      | None -> false)
  then
    Error "invariant environment does not belong to this verification session"
  else if
    not
      (String.equal
         (Sst.to_string (Sst_validation.program validated) |> digest)
         session.identity.program_snapshot)
  then Error "validated program does not belong to this verification session"
  else if definition.mode <> Sst.Exec then
    Error "only an executable callee may issue a receipt"
  else
    match definition.body with
    | Sst.Checked_exec
        { provenance = Sst.Authenticated_typedtree _ as body_provenance; _ } -> (
        match definition.result_type with
        | (Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
          | Sst.Application _) ->
            Error "callee result is not an invariant-bearing aggregate"
        | Sst.Aggregate result_type -> (
        match Type_invariant.find_for_type invariants result_type with
        | None -> Error "callee result has no authenticated invariant"
        | Some handle ->
            let descriptor =
              Sst_validation.find_callable validated definition.function_id
            in
            (match descriptor with
            | None -> Error "callee is absent from the validated callable registry"
            | Some descriptor ->
                let result_mode =
                  Sst_validation.result_instance_mode validated descriptor
                in
                (match result_mode with
                | Sst.Ghost_instance ->
                    Error "Ghost callee results cannot issue receipts"
                | Sst.Exec_instance | Sst.Tracked_instance ->
                    let program = Sst_validation.program validated in
                    let* identities =
                      match session.callable_identities with
                      | Some identities -> Ok identities
                      | None -> Error "verification session is destroyed"
                    in
                    let* callable =
                      Verification_identity.find identities definition
                    in
                    let* callable_key =
                      canonical_callable_key session definition
                    in
                    let snapshot =
                      {
                        program = session.identity;
                        resolved_path =
                          Verification_identity.resolved_path callable;
                        binding_uid =
                          Verification_identity.binding_uid callable;
                        callable_key;
                        callee = definition.function_id;
                        body_snapshot = body_snapshot program definition;
                        body_provenance;
                        mode = definition.mode;
                        result_mode;
                        result_type = definition.result_type;
                        invariant = invariant_snapshot handle;
                      }
                    in
                    if
                      not
                        (List.exists
                           (fun existing ->
                             String.equal
                               (callee_snapshot_fingerprint existing)
                               (callee_snapshot_fingerprint snapshot))
                           session.authorized_callees)
                    then
                      session.authorized_callees <-
                        snapshot :: session.authorized_callees;
                    Ok snapshot))))
    | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ } ->
        Error "raw checked-body metadata cannot issue a receipt"
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
    | Sst.Proof_body _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        Error "only an authenticated local checked executable body may issue a receipt"

let finite_result_snapshot session ~validated
    (definition : Sst.function_definition) ~rank =
  if not session.active then Error "verification session is destroyed"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then Error "validated program does not belong to this verification session"
  else if definition.mode <> Sst.Exec then
    Error "only an executable callee may promote a finite result"
  else
    match definition.body with
    | Sst.Checked_exec
        { provenance = Sst.Authenticated_typedtree _ as body_provenance; _ } -> (
        match definition.result_type with
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
            Error "finite result is not an aggregate"
        | Sst.Aggregate _ | Sst.Application _ ->
            let descriptor =
              Sst_validation.find_callable validated definition.function_id
            in
            (match descriptor with
            | None ->
                Error
                  "finite-result callee is absent from the validated callable registry"
            | Some descriptor -> (
                match
                  Sst_validation.result_instance_mode validated descriptor
                with
                | Sst.Ghost_instance ->
                    Error "Ghost callee results cannot promote finite authority"
                | (Sst.Exec_instance | Sst.Tracked_instance) as result_mode ->
                    let program = Sst_validation.program validated in
                    let* identities =
                      match session.callable_identities with
                      | Some identities -> Ok identities
                      | None -> Error "verification session is destroyed"
                    in
                    let* callable =
                      Verification_identity.find identities definition
                    in
                    let* callable_key =
                      canonical_callable_key session definition
                    in
                    let finite_body_snapshot =
                      body_snapshot program definition
                    in
                    let* finite_candidate =
                      Direct_candidate.register
                        session.finite_candidate_lifecycle
                        ~callable:(function_id_string definition.function_id)
                        ~body_snapshot:finite_body_snapshot
                        ~profile_snapshot:(rank_fingerprint rank)
                    in
                    let snapshot =
                      {
                        finite_snapshot_issuer = private_issuer;
                        finite_snapshot_session = session.session;
                        finite_snapshot_token = ref ();
                        finite_program = session.identity;
                        finite_resolved_path =
                          Verification_identity.resolved_path callable;
                        finite_binding_uid =
                          Verification_identity.binding_uid callable;
                        finite_callable_key = callable_key;
                        finite_callee = definition.function_id;
                        finite_body_snapshot;
                        finite_body_provenance = body_provenance;
                        finite_mode = definition.mode;
                        finite_result_mode = result_mode;
                        finite_result_type = definition.result_type;
                        finite_rank = rank;
                        finite_candidate;
                        finite_result_exits = [];
                        finite_result_facts = [];
                      }
                    in
                    let existing =
                      List.find_opt
                        (fun candidate ->
                          String.equal
                            (finite_result_snapshot_core_fingerprint candidate)
                            (finite_result_snapshot_core_fingerprint snapshot))
                        session.finite_result_snapshots
                    in
                    (match existing with
                    | Some existing -> Ok existing
                    | None ->
                        session.finite_result_snapshots <-
                          snapshot :: session.finite_result_snapshots;
                        Ok snapshot))))
    | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ } ->
        Error "raw checked-body metadata cannot promote a finite result"
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
    | Sst.Proof_body _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        Error
          "only an authenticated local checked executable body may promote a finite result"

let operation_name = function
  | Vir.Add -> "add"
  | Vir.Subtract -> "sub"
  | Vir.Negate -> "neg"
  | Vir.Multiply_constant value -> "mul:" ^ Z.to_string value
  | Vir.Successor -> "succ"
  | Vir.Predecessor -> "pred"
  | Vir.Absolute_value -> "abs"

let transition_name = function
  | Vir.Direct_root_transition -> "direct"
  | Vir.Nested_transition -> "nested"
  | Vir.Rebase_transition -> "rebase"

let boundary_string = function
  | Vir.Constructor_establishment -> "constructor"
  | Vir.Transition_preservation
      { transition_kind; root_binding_id; pre_version; successor_version } ->
      Printf.sprintf "transition:%s:%d:%d:%d"
        (transition_name transition_kind) root_binding_id pre_version
        successor_version
  | Vir.Call_argument { callee; argument_index } ->
      Printf.sprintf "call-argument:%s:%d" (function_ref_string callee)
        argument_index
  | Vir.Call_result { callee } ->
      "call-result:" ^ function_ref_string callee
  | Vir.Function_return -> "function-return"
  | Vir.Shared_invariant_close { entry_epoch; final_epoch } ->
      Printf.sprintf "shared-invariant-close:%d:%d" entry_epoch final_epoch
  | Vir.Terminal_observation { operation; snapshot } ->
      Printf.sprintf "terminal:%s:%b" (function_ref_string operation) snapshot

let kind_string = function
  | Vir.Arithmetic_safety { operation; mathematical_result; violated_bound } ->
      Printf.sprintf "arithmetic:%s:%s:%s" (operation_name operation)
        (Vir.integer_term_to_string mathematical_result)
        (match violated_bound with Vir.Lower_bound -> "lower" | Upper_bound -> "upper")
  | Vir.Assertion { assertion_ordinal } ->
      Printf.sprintf "assertion:%d" assertion_ordinal
  | Vir.Local_assertion { local_assertion_ordinal } ->
      Printf.sprintf "local-assertion:%d" local_assertion_ordinal
  | Vir.Postcondition { postcondition_ordinal; declaration_span } ->
      Printf.sprintf "post:%d:%s" postcondition_ordinal
        (span_string declaration_span)
  | Vir.Call_precondition
      { callee; precondition_ordinal; declaration_span; call_span } ->
      Printf.sprintf "pre:%s:%d:%s:%s" (function_ref_string callee)
        precondition_ordinal (span_string declaration_span) (span_string call_span)
  | Vir.Callback_precondition { callback; call_span } ->
      Sst_callback_private.precondition_name ~span:span_string callback call_span
  | Vir.Invariant_validity
      { invariant_id; abstract_type; model; predicate; operation; boundary } ->
      String.concat ":"
        [
          "invariant";
          invariant_id;
          type_id_string
            {
              Sst.type_index = abstract_type.aggregate_type_index;
              type_name = abstract_type.aggregate_type_name;
            };
          function_ref_string model;
          function_ref_string predicate;
          function_ref_string operation;
          boundary_string boundary;
        ]
  | Vir.Entry_measure_nonnegative { declaration_span } ->
      "entry-measure:" ^ span_string declaration_span
  | Vir.Recursive_call_measure_nonnegative
      { callee; declaration_span; call_span } ->
      Printf.sprintf "recursive-nonnegative:%s:%s:%s"
        (function_ref_string callee) (span_string declaration_span)
        (span_string call_span)
  | Vir.Recursive_call_strict_descent
      { callee; declaration_span; call_span } ->
      Printf.sprintf "recursive-descent:%s:%s:%s"
        (function_ref_string callee) (span_string declaration_span)
        (span_string call_span)

let symbol_string = Parametric_logic_private.symbol_string ~span_string

let obligation_fingerprint (obligation : Vir.obligation) =
  String.concat "\000"
    [
      string_of_int obligation.obligation_index;
      function_ref_string obligation.function_ref;
      kind_string obligation.kind;
      span_string obligation.span;
      String.concat "|" (List.map Vir.boolean_term_to_string obligation.assumptions);
      String.concat "|"
        (List.map Vir.boolean_term_to_string obligation.required_preceding_safety);
      String.concat "|"
        (List.map Vir.boolean_term_to_string obligation.path_condition);
      Vir.boolean_term_to_string obligation.goal;
      String.concat "|" (List.map symbol_string obligation.projection_symbols);
    ]
  |> digest

let obligation_set_fingerprint fingerprints =
  String.concat "\000" fingerprints |> digest

let same_function_ref_id (reference : Vir.function_ref) (id : Sst.function_id) =
  reference.function_index = id.function_index
  && String.equal reference.function_name id.function_name

let proof_semantic_terms =
  List.filter (function Vir.Logical_adt_schema _ -> false | _ -> true)

let proof_assumptions_digest assumptions required_preceding_safety =
  let assumptions = proof_semantic_terms assumptions
  and required_preceding_safety =
    proof_semantic_terms required_preceding_safety
  in
  String.concat "\000"
    [
      String.concat "\001" (List.map Vir.boolean_term_to_string assumptions);
      String.concat "\001"
        (List.map Vir.boolean_term_to_string required_preceding_safety);
    ]
  |> digest

let proof_path_digest path_condition =
  String.concat "\000" (List.map Vir.boolean_term_to_string path_condition)
  |> digest

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name

let same_constructor_id (left : Sst.constructor_id)
    (right : Sst.constructor_id) =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name

let constructor_digest (constructor : Sst.constructor_id) =
  String.concat "\000"
    [
      type_id_string constructor.constructor_type;
      string_of_int constructor.constructor_index;
      constructor.constructor_name;
    ]
  |> digest

let activation_digest activations =
  activations
  |> List.map (fun (activation : Spec_unfolding.activation) ->
         String.concat "\000"
           [
             function_id_string activation.function_id;
             string_of_int activation.depth;
             span_string activation.span;
           ])
  |> String.concat "\001" |> digest

let rec expression_contains_case target (expression : Sst.expression) =
  let contains = expression_contains_case target in
  match expression.expression_desc with
  | Sst.Match (scrutinee, cases) ->
      contains scrutinee
      || List.exists
           (fun candidate ->
             candidate == target
             || Option.fold ~none:false ~some:contains candidate.Sst.case_guard
             || contains candidate.case_body)
           cases
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      contains quantifier.quantifier_body
  | _ ->
      List.exists contains
        (Sst_callback_private.expression_children expression)

let proof_authority_contains_case authority definition target =
  match (authority.proof_authority_scope, definition.Sst.body) with
  | Full_proof_execution, Sst.Proof_body { body; _ } ->
      expression_contains_case target body.expression
  | ( Exec_proof_region scope,
      Sst.Checked_exec
        { provenance = Sst.Authenticated_typedtree _; _ } ) ->
      scope.proof_scope_definition == definition
      && scope.proof_scope_definition.function_id
         = authority.proof_authority_callable
      && expression_contains_case target scope.proof_scope_region
  | ( Full_proof_execution,
      ( Sst.Checked_exec _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ) )
  | ( Exec_proof_region _,
      ( Sst.Proof_body _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _
      | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ } ) ) ->
      false

let position_le left right =
  left.Diagnostic.line < right.Diagnostic.line
  || (left.line = right.line && left.column <= right.column)

let span_contains outer inner =
  String.equal outer.Diagnostic.file inner.Diagnostic.file
  && position_le outer.start_pos inner.start_pos
  && position_le inner.end_pos outer.end_pos

let proof_scope_allows_obligation session authority obligation =
  match authority.proof_authority_scope with
  | Full_proof_execution -> true
  | Exec_proof_region scope ->
      (match session.validated with
      | Some validated -> validated == scope.proof_scope_validated
      | None -> false)
      && scope.proof_scope_definition.function_id
         = authority.proof_authority_callable
      && span_contains scope.proof_scope_region.span obligation.Vir.span
      &&
      let program = Sst_validation.program scope.proof_scope_validated in
      Typedtree_adapter_private.Public.authenticate_proof_region ~program
        ~definition:scope.proof_scope_definition
        ~expression:scope.proof_scope_region

let constructor_is_nullary program constructor =
  List.exists
    (fun (definition : Sst.type_definition) ->
      same_type_id definition.type_id constructor.Sst.constructor_type
      &&
      match definition.type_kind with
      | Sst.Variant_definition constructors ->
          List.exists
            (fun (candidate : Sst.constructor_definition) ->
              same_constructor_id candidate.constructor_id constructor
              && candidate.constructor_fields = [])
            constructors
      | Sst.Record_definition _ -> false)
    program.Sst.types

let rec path_prefix prefix path =
  match (prefix, path) with
  | [], _ -> true
  | expected :: prefix, actual :: path when expected = actual ->
      path_prefix prefix path
  | _ -> false

let same_ground_proof_authority left right =
  left.proof_authority_issuer == right.proof_authority_issuer
  && left.proof_authority_session == right.proof_authority_session
  && String.equal
       (program_fingerprint left.proof_authority_program)
       (program_fingerprint right.proof_authority_program)
  && String.equal left.proof_authority_resolved_path
       right.proof_authority_resolved_path
  && String.equal left.proof_authority_binding_uid
       right.proof_authority_binding_uid
  && String.equal left.proof_authority_callable_key
       right.proof_authority_callable_key
  && left.proof_authority_callable = right.proof_authority_callable
  && String.equal left.proof_authority_body_snapshot
       right.proof_authority_body_snapshot
  && left.proof_authority_body_provenance
     = right.proof_authority_body_provenance
  &&
  match
    (left.proof_authority_scope, right.proof_authority_scope)
  with
  | Full_proof_execution, Full_proof_execution -> true
  | Exec_proof_region left, Exec_proof_region right ->
      left.proof_scope_validated == right.proof_scope_validated
      && left.proof_scope_definition == right.proof_scope_definition
      && left.proof_scope_region == right.proof_scope_region
      && String.equal left.proof_scope_region_snapshot
           right.proof_scope_region_snapshot
  | Full_proof_execution, Exec_proof_region _
  | Exec_proof_region _, Full_proof_execution ->
      false

let registered_ground_proof_authority (session : t) authority =
  authority.proof_authority_issuer == private_issuer
  && authority.proof_authority_session == session.session
  && List.exists
       (same_ground_proof_authority authority)
       session.proof_activation_authorities

let issue_ground_constructor_match (session : t) ~authority ~validated
    ~(definition : Sst.function_definition) ~(case : Sst.case) ~value ~mode
    ~rank ~receipt ~path_condition =
  let program = Sst_validation.program validated in
  if not session.active || session.issuer != private_issuer then
    Error "ground constructor match has a stale or foreign session"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
    || not (registered_ground_proof_authority session authority)
    || authority.proof_authority_callable <> definition.function_id
  then Error "ground constructor match authority mismatch"
  else if
    not
      (List.exists
         (fun candidate -> candidate == definition)
         program.Sst.functions)
    || not (proof_authority_contains_case authority definition case)
  then
    Error
      "ground constructor match is outside its authenticated proof activation scope"
  else
    match (case.case_pattern.pattern_desc, value.Vir.aggregate_desc) with
    | ( Sst.Constructor_pattern (constructor, []),
        Vir.Aggregate_symbol symbol )
      when symbol.role = Vir.Input
           && Logical_spec_evaluation_private.vir_aggregate_type_of_sst
                program.Sst.parametric_adts case.case_pattern.typ
              = Some value.aggregate_type
           && value.aggregate_type.aggregate_type_index
              = constructor.constructor_type.type_index
           && symbol.sort = Vir.Aggregate value.aggregate_type
           && constructor_is_nullary program constructor -> (
        match
          Finite_value_registry.authenticate session.finite_registry
            ~facts:[ receipt ] ~callable:authority.proof_authority_callable_key
            ~value ~mode ~typ:case.case_pattern.typ ~rank
        with
        | Error message -> Error message
        | Ok authenticated when
            not (Finite_value_registry.same_receipt authenticated receipt) ->
            Error "ground constructor match finite receipt changed identity"
        | Ok _ ->
            let path_digest = proof_path_digest path_condition in
            let issued =
              {
                ground_match_issuer = ground_constructor_match_issuer;
                ground_match_session = session.session;
                ground_match_token = ref ();
                ground_match_authority = authority;
                ground_match_validated = validated;
                ground_match_definition = definition;
                ground_match_case = case;
                ground_match_value = value;
                ground_match_symbol = symbol;
                ground_match_constructor = constructor;
                ground_match_constructor_digest =
                  constructor_digest constructor;
                ground_match_mode = mode;
                ground_match_rank = rank;
                ground_match_receipt = receipt;
                ground_match_path_condition = path_condition;
                ground_match_path_digest = path_digest;
              }
            in
            session.issued_ground_constructor_matches <-
              issued :: session.issued_ground_constructor_matches;
            (match !ground_constructor_match_attack_for_testing with
            | Some "forged" ->
                Ok { issued with ground_match_token = ref () }
            | Some "stale" ->
                Ok { issued with ground_match_session = ref () }
            | Some "wrong-constructor" ->
                Ok
                  {
                    issued with
                    ground_match_constructor_digest =
                      issued.ground_match_constructor_digest ^ ":wrong";
                  }
            | Some "path" ->
                Ok
                  {
                    issued with
                    ground_match_path_digest =
                      issued.ground_match_path_digest ^ ":wrong";
                  }
            | Some "escape" -> (
                match issued.ground_match_authority.proof_authority_scope with
                | Full_proof_execution -> Ok issued
                | Exec_proof_region scope ->
                    Ok
                      {
                        issued with
                        ground_match_authority =
                          {
                            issued.ground_match_authority with
                            proof_authority_scope =
                              Exec_proof_region
                                {
                                  scope with
                                  proof_scope_region_snapshot =
                                    scope.proof_scope_region_snapshot
                                    ^ ":escaped";
                                  };
                          };
                      })
            | Some "reuse" -> (
                match session.issued_ground_constructor_matches with
                | _current :: prior :: _ -> Ok prior
                | _ -> Ok issued)
            | Some _ | None -> Ok issued))
    | Sst.Constructor_pattern (_, _ :: _), Vir.Aggregate_symbol _
    | Sst.Constructor_pattern _, _
    | ( Sst.Wildcard | Sst.Bind _ | Sst.Owned_tree_cursor_pattern _
      | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
      | Sst.Tuple_pattern _ | Sst.Record_pattern _ | Sst.Or_pattern _ ),
      _ ->
        Error
          "ground constructor match requires an exact nullary Input constructor"

let validate_ground_constructor_match (session : t) authority
    (obligation : Vir.obligation) match_ =
  let program = Sst_validation.program match_.ground_match_validated in
  match
    Finite_value_registry.authenticate session.finite_registry
      ~facts:[ match_.ground_match_receipt ]
      ~callable:
        match_.ground_match_authority.proof_authority_callable_key
      ~value:match_.ground_match_value ~mode:match_.ground_match_mode
      ~typ:match_.ground_match_case.case_pattern.typ
      ~rank:match_.ground_match_rank
  with
  | Error _ -> false
  | Ok receipt ->
      match_.ground_match_issuer == ground_constructor_match_issuer
      && match_.ground_match_session == session.session
      && match_.ground_match_token != ground_constructor_match_issuer
      && List.exists
           (fun issued -> issued == match_)
           session.issued_ground_constructor_matches
      && Finite_value_registry.same_receipt receipt
           match_.ground_match_receipt
      &&
      (match session.validated with
      | Some validated -> match_.ground_match_validated == validated
      | None -> false)
      && registered_ground_proof_authority session
           match_.ground_match_authority
      && same_ground_proof_authority authority
           match_.ground_match_authority
      && same_function_ref_id obligation.Vir.function_ref
           match_.ground_match_definition.function_id
      && proof_authority_contains_case match_.ground_match_authority
           match_.ground_match_definition
           match_.ground_match_case
      && proof_scope_allows_obligation session
           match_.ground_match_authority obligation
      && constructor_is_nullary program match_.ground_match_constructor
      && String.equal match_.ground_match_constructor_digest
           (constructor_digest match_.ground_match_constructor)
      && String.equal match_.ground_match_path_digest
           (proof_path_digest match_.ground_match_path_condition)
      && path_prefix match_.ground_match_path_condition
           obligation.path_condition
let authenticate_direct_exec_local_assertion_source ~validated ~definition ~expression = Typedtree_adapter_private.Public.is_direct_exec_builtin_local_assertion ~program:(Sst_validation.program validated) ~definition ~expression

let direct_exec_scope_matches (session : t) scope ~validated ~definition ~expression ~ordinal ~path_condition ~goal =
  let program = Sst_validation.program validated in
  scope.direct_scope_issuer == direct_exec_local_assertion_scope_issuer
  && scope.direct_scope_session == session.session
  && scope.direct_scope_token != direct_exec_local_assertion_scope_issuer
  && scope.direct_scope_active
  && Option.fold ~none:false ~some:(fun active -> active == scope) session.active_direct_exec_local_assertion_scope
  && scope.direct_scope_validated == validated
  && scope.direct_scope_program == program
  && scope.direct_scope_definition == definition
  && scope.direct_scope_expression == expression
  && String.equal scope.direct_scope_program_snapshot (Sst.to_string program)
  && String.equal scope.direct_scope_definition_snapshot (body_snapshot program definition)
  && scope.direct_scope_ordinal = ordinal
  && String.equal scope.direct_scope_path_digest (proof_path_digest path_condition)
  && String.equal scope.direct_scope_goal_digest (Vir.boolean_term_to_string goal |> digest)
  && definition.mode = Sst.Exec
  && List.exists (fun candidate -> candidate == definition) program.Sst.functions
  &&
  match expression.Sst.expression_desc with
  | Sst.Local_assert { assertion_ordinal; predicate } ->
      assertion_ordinal = ordinal
      && predicate == scope.direct_scope_predicate
      && Typedtree_adapter_private.Public.is_direct_exec_builtin_local_assertion
           ~program ~definition ~expression
      && Typedtree_adapter_private.Public.authenticate_local_assertion ~program ~definition ~expression
  | _ -> false

let open_direct_exec_local_assertion_scope (session : t) ~validated ~(definition : Sst.function_definition)
    ~(expression : Sst.expression) ~ordinal ~path_condition ~goal =
  let program = Sst_validation.program validated in
  let predicate =
    match expression.Sst.expression_desc with
    | Sst.Local_assert { assertion_ordinal; predicate } when assertion_ordinal = ordinal -> Some predicate
    | Sst.Local_assert _ | _ -> None
  in
  let attacked name = !local_assertion_instance_attack_for_testing = Some name in
  if
    not session.active || session.issuer != private_issuer
    || Option.fold ~none:true ~some:(fun owned -> owned != validated) session.validated
  then Error "direct Exec assertion scope has a stale or foreign session"
  else if Option.is_some session.active_direct_exec_local_assertion_scope then
    Error "direct Exec assertion scope is already active"
  else
    match predicate with
    | None -> Error "direct Exec assertion source or ordinal mismatch"
    | Some predicate ->
        let scope =
          {
            direct_scope_issuer =
              (if attacked "direct-issuer" then ref () else direct_exec_local_assertion_scope_issuer);
            direct_scope_session = session.session;
            direct_scope_token =
              (if attacked "direct-scope" then direct_exec_local_assertion_scope_issuer else ref ());
            direct_scope_validated = validated;
            direct_scope_program = program;
            direct_scope_definition = definition;
            direct_scope_expression = expression;
            direct_scope_predicate = predicate;
            direct_scope_program_snapshot = Sst.to_string program;
            direct_scope_definition_snapshot = body_snapshot program definition;
            direct_scope_ordinal =
              (if attacked "direct-ordinal" then ordinal + 1 else ordinal);
            direct_scope_path_digest =
              proof_path_digest path_condition ^ if attacked "direct-path" then ":foreign" else "";
            direct_scope_goal_digest =
              (Vir.boolean_term_to_string goal |> digest) ^ if attacked "direct-goal" then ":foreign" else "";
            direct_scope_active = true;
          }
        in
        session.active_direct_exec_local_assertion_scope <- Some scope;
        incr observed_direct_exec_local_assertion_scopes_issued;
        Ok scope

let close_direct_exec_local_assertion_scope (session : t) scope =
  let registered =
    Option.fold ~none:false ~some:(fun active -> active == scope) session.active_direct_exec_local_assertion_scope
  in
  let authentic =
    registered && session.active && session.issuer == private_issuer
    && scope.direct_scope_issuer == direct_exec_local_assertion_scope_issuer
    && scope.direct_scope_session == session.session
    && scope.direct_scope_token != direct_exec_local_assertion_scope_issuer
    && scope.direct_scope_active
  in
  if registered then (
    scope.direct_scope_active <- false;
    session.active_direct_exec_local_assertion_scope <- None;
    incr observed_direct_exec_local_assertion_scopes_closed);
  if authentic then Ok () else Error "direct Exec assertion scope issuer or affinity mismatch"

let record_local_assertion_instance (session : t) ~source ~validated
    ~(definition : Sst.function_definition) ~(expression : Sst.expression)
    ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal =
  let program = Sst_validation.program validated in
  match expression.expression_desc with
  | Sst.Local_assert { assertion_ordinal; predicate = _ }
    when assertion_ordinal = ordinal ->
      if !local_assertion_instance_attack_for_testing = Some "missing" then
        Error "local assertion reached instance is missing"
      else
        let attacked name =
          !local_assertion_instance_attack_for_testing = Some name
        in
        let path_digest =
          proof_path_digest path_condition
          ^ if attacked "path-mismatch" then ":foreign" else ""
        in
        let instance =
          {
            local_instance_issuer = local_assertion_instance_issuer;
            local_instance_session =
              (if attacked "foreign" then ref () else session.session);
            local_instance_token =
              (if attacked "stale" then local_assertion_instance_issuer
               else ref ());
            local_instance_validated = validated;
            local_instance_program = program;
            local_instance_definition = definition;
            local_instance_expression = expression;
            local_instance_source = source;
            local_instance_ordinal = ordinal;
            local_instance_assumptions_digest =
              proof_assumptions_digest assumptions required_preceding_safety;
            local_instance_path_digest = path_digest;
            local_instance_goal_digest =
              Vir.boolean_term_to_string goal |> digest;
          }
        in
        session.issued_local_assertion_instances <-
          instance :: session.issued_local_assertion_instances;
        session.counters.local_assertion_instances_issued <-
          session.counters.local_assertion_instances_issued + 1;
        incr observed_local_assertion_instances_issued;
        if attacked "copied" then
          Ok { instance with local_instance_token = ref () }
        else Ok instance
  | Sst.Local_assert _ -> Error "local assertion instance ordinal mismatch"
  | _ -> Error "local assertion instance source is not a local assertion"

let issue_local_assertion_instance (session : t) ~authority ~validated ~(definition : Sst.function_definition)
    ~(expression : Sst.expression) ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal =
  let program = Sst_validation.program validated in
  let authenticated_scope =
    match authority.proof_authority_scope with
    | Full_proof_execution -> (
        definition.mode = Sst.Proof
        &&
        match definition.body with
        | Sst.Proof_body { provenance = Sst.Authenticated_typedtree _; _ } ->
            Option.is_none (Typedtree_adapter_private.Public.local_assertion_parent_proof_region
              ~program ~definition ~expression)
        | Sst.Proof_body { provenance = Sst.Raw_semantic_body _; _ }
        | Sst.Checked_exec _ | Sst.Spec_definition _
        | Sst.Recursive_spec_definition _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            false)
    | Exec_proof_region scope -> (
        definition.mode = Sst.Exec
        && scope.proof_scope_validated == validated
        && scope.proof_scope_definition == definition
        &&
        match definition.body with
        | Sst.Checked_exec
            { provenance = Sst.Authenticated_typedtree _; _ } ->
            Option.fold ~none:false
              ~some:(fun region -> region == scope.proof_scope_region)
              (Typedtree_adapter_private.Public.local_assertion_parent_proof_region
                 ~program ~definition ~expression)
        | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ }
        | Sst.Proof_body _ | Sst.Spec_definition _
        | Sst.Recursive_spec_definition _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            false)
  in
  if
    not session.active || session.issuer != private_issuer
    || Option.fold ~none:true ~some:(fun owned -> owned != validated) session.validated
  then Error "local assertion instance has a stale or foreign session"
  else if
    not (registered_ground_proof_authority session authority)
    || authority.proof_authority_callable <> definition.function_id
    || not authenticated_scope
  then Error "local assertion instance requires its exact authenticated proof scope"
  else if
    not (List.exists (fun candidate -> candidate == definition) program.Sst.functions)
  then Error "local assertion definition is not the session-owned callable"
  else if
    not
      (Typedtree_adapter_private.Public.authenticate_local_assertion
         ~program ~definition ~expression)
  then Error "local assertion static carrier authentication failed"
  else
    record_local_assertion_instance session
      ~source:(Proof_local_assertion authority) ~validated ~definition
      ~expression ~ordinal ~assumptions ~required_preceding_safety
      ~path_condition ~goal

let issue_direct_exec_local_assertion_instance (session : t) ~scope ~validated
    ~(definition : Sst.function_definition) ~(expression : Sst.expression)
    ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal =
  if
    not
      (direct_exec_scope_matches session scope ~validated ~definition
         ~expression ~ordinal ~path_condition ~goal)
  then Error "direct Exec assertion scope identity mismatch"
  else
    record_local_assertion_instance session
      ~source:(Direct_exec_local_assertion scope) ~validated ~definition
      ~expression ~ordinal ~assumptions ~required_preceding_safety
      ~path_condition ~goal

let rec consume_local_assertion_instance (session : t) instance ~expression ~ordinal
    ~assumptions ~required_preceding_safety ~path_condition ~goal =
  let source_valid =
    match instance.local_instance_source with
    | Proof_local_assertion authority ->
        registered_ground_proof_authority session authority
        && authority.proof_authority_callable = instance.local_instance_definition.function_id
    | Direct_exec_local_assertion scope ->
        direct_exec_scope_matches session scope
          ~validated:instance.local_instance_validated ~definition:instance.local_instance_definition ~expression
          ~ordinal ~path_condition ~goal
  in
  if
    not session.active || session.issuer != private_issuer
    || instance.local_instance_issuer != local_assertion_instance_issuer
    || instance.local_instance_session != session.session
    || instance.local_instance_token == local_assertion_instance_issuer
    || not source_valid
    || not (List.exists (fun issued -> issued == instance)
              session.issued_local_assertion_instances)
  then Error "local assertion instance issuer or session mismatch"
  else if
    List.exists (fun token -> token == instance.local_instance_token)
      session.consumed_local_assertion_instances
  then Error "local assertion instance was already consumed"
  else if
    instance.local_instance_expression != expression
    || instance.local_instance_ordinal <> ordinal
    || instance.local_instance_program != Sst_validation.program instance.local_instance_validated
    || not
         (Typedtree_adapter_private.Public.authenticate_local_assertion ~program:instance.local_instance_program
            ~definition:instance.local_instance_definition ~expression)
  then Error "local assertion instance static identity mismatch"
  else if
    not
      (String.equal instance.local_instance_assumptions_digest
         (proof_assumptions_digest assumptions required_preceding_safety))
  then Error "local assertion instance assumptions mismatch"
  else if
    not
      (String.equal instance.local_instance_path_digest (proof_path_digest path_condition))
  then Error "local assertion instance path mismatch"
  else if
    not
      (String.equal instance.local_instance_goal_digest (Vir.boolean_term_to_string goal |> digest))
  then Error "local assertion instance goal mismatch"
  else (
    session.consumed_local_assertion_instances <-
      instance.local_instance_token :: session.consumed_local_assertion_instances;
    session.counters.local_assertion_instances_consumed <-
      session.counters.local_assertion_instances_consumed + 1;
    incr observed_local_assertion_instances_consumed;
    if !local_assertion_instance_attack_for_testing = Some "duplicate" then
      let saved = !local_assertion_instance_attack_for_testing in
      local_assertion_instance_attack_for_testing := None;
      Fun.protect
        ~finally:(fun () -> local_assertion_instance_attack_for_testing := saved)
        (fun () ->
          consume_local_assertion_instance session instance ~expression
            ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal)
    else Ok ())

let issue_and_consume_local_assertion (session : t) ~direct_exec ~authority ~validated ~definition
    ~expression ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal =
  let issue () =
    if direct_exec then
      let* scope =
        open_direct_exec_local_assertion_scope session ~validated ~definition ~expression
          ~ordinal ~path_condition ~goal
      in
      (match
         issue_direct_exec_local_assertion_instance session ~scope ~validated
           ~definition ~expression ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal
       with
      | Ok instance -> Ok (instance, Some scope)
      | Error message ->
          ignore (close_direct_exec_local_assertion_scope session scope); Error message)
    else
      match authority with
      | None -> Error "local assertion lacks an authenticated proof scope"
      | Some authority ->
          let* instance =
            issue_local_assertion_instance session ~authority ~validated ~definition ~expression
              ~ordinal ~assumptions ~required_preceding_safety ~path_condition ~goal
          in
          Ok (instance, None)
  in
  let* instance, direct_scope = issue () in
  match
    consume_local_assertion_instance session instance ~expression ~ordinal ~assumptions
      ~required_preceding_safety ~path_condition ~goal
  with
  | Ok () -> Ok direct_scope
  | Error message ->
      Option.iter (fun scope -> ignore (close_direct_exec_local_assertion_scope session scope))
        direct_scope;
      Error message

let same_proof_activation_scope left right =
  match (left, right) with
  | Full_proof_execution, Full_proof_execution -> true
  | ( Exec_proof_region left,
      Exec_proof_region right ) ->
      left.proof_scope_validated == right.proof_scope_validated
      && left.proof_scope_definition == right.proof_scope_definition
      && left.proof_scope_region == right.proof_scope_region
      && String.equal left.proof_scope_region_snapshot
           right.proof_scope_region_snapshot
  | Full_proof_execution, Exec_proof_region _
  | Exec_proof_region _, Full_proof_execution ->
      false

let same_proof_activation_authority left right =
  left.proof_authority_issuer == right.proof_authority_issuer
  && left.proof_authority_session == right.proof_authority_session
  && String.equal
       (program_fingerprint left.proof_authority_program)
       (program_fingerprint right.proof_authority_program)
  && String.equal left.proof_authority_resolved_path
       right.proof_authority_resolved_path
  && String.equal left.proof_authority_binding_uid
       right.proof_authority_binding_uid
  && String.equal left.proof_authority_callable_key
       right.proof_authority_callable_key
  && left.proof_authority_callable = right.proof_authority_callable
  && String.equal left.proof_authority_body_snapshot
       right.proof_authority_body_snapshot
  && left.proof_authority_body_provenance
     = right.proof_authority_body_provenance
  && same_proof_activation_scope left.proof_authority_scope
       right.proof_authority_scope

let proof_activation_authority (session : t) ~validated
    (definition : Sst.function_definition) =
  if not session.active then Error "verification session is destroyed"
  else if session.issuer != private_issuer then
    Error "proof activation issuer mismatch"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then Error "proof activation validated program mismatch"
  else if definition.mode <> Sst.Proof then
    Error "proof activation authority requires a Proof callable"
  else
    match definition.body with
    | Sst.Proof_body
        { provenance = Sst.Authenticated_typedtree _ as body_provenance; _ } ->
        let program = Sst_validation.program validated in
        let* identities =
          match session.callable_identities with
          | Some identities -> Ok identities
          | None -> Error "verification session is destroyed"
        in
        let* callable = Verification_identity.find identities definition in
        let* callable_key = canonical_callable_key session definition in
        let authority =
          {
            proof_authority_issuer = private_issuer;
            proof_authority_session = session.session;
            proof_authority_program = session.identity;
            proof_authority_resolved_path =
              Verification_identity.resolved_path callable;
            proof_authority_binding_uid =
              Verification_identity.binding_uid callable;
            proof_authority_callable_key = callable_key;
            proof_authority_callable = definition.function_id;
            proof_authority_body_snapshot = body_snapshot program definition;
            proof_authority_body_provenance = body_provenance;
            proof_authority_scope = Full_proof_execution;
          }
        in
        if
          not
            (List.exists
               (same_proof_activation_authority authority)
               session.proof_activation_authorities)
        then
          session.proof_activation_authorities <-
            authority :: session.proof_activation_authorities;
        Ok authority
    | Sst.Proof_body { provenance = Sst.Raw_semantic_body _; _ } ->
        Error
          "raw Proof body metadata cannot issue reached activation authority"
    | Sst.Checked_exec _ | Sst.Spec_definition _
    | Sst.Recursive_spec_definition _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        Error "proof activation authority requires a Proof body"

let exec_proof_region_activation_authority (session : t) ~validated
    (definition : Sst.function_definition) ~(region : Sst.expression) =
  if not session.active then Error "verification session is destroyed"
  else if session.issuer != private_issuer then
    Error "proof activation issuer mismatch"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then Error "proof activation validated program mismatch"
  else if definition.mode <> Sst.Exec then
    Error "Exec proof-region authority requires an Exec callable"
  else
    match definition.body with
    | Sst.Checked_exec
        { provenance = Sst.Authenticated_typedtree _ as body_provenance; _ } ->
        let program = Sst_validation.program validated in
        if
          not
            (Typedtree_adapter_private.Public.authenticate_proof_region
               ~program ~definition ~expression:region)
        then Error "Exec proof-region static authentication failed"
        else
          let* identities =
            match session.callable_identities with
            | Some identities -> Ok identities
            | None -> Error "verification session is destroyed"
          in
          let* callable = Verification_identity.find identities definition in
          let* callable_key = canonical_callable_key session definition in
          let region_snapshot =
            String.concat ":"
              [
                span_string region.span;
                body_snapshot program definition;
              ]
            |> digest
          in
          let authority =
            {
              proof_authority_issuer = private_issuer;
              proof_authority_session = session.session;
              proof_authority_program = session.identity;
              proof_authority_resolved_path =
                Verification_identity.resolved_path callable;
              proof_authority_binding_uid =
                Verification_identity.binding_uid callable;
              proof_authority_callable_key = callable_key;
              proof_authority_callable = definition.function_id;
              proof_authority_body_snapshot = body_snapshot program definition;
              proof_authority_body_provenance = body_provenance;
              proof_authority_scope =
                Exec_proof_region
                  {
                    proof_scope_validated = validated;
                    proof_scope_definition = definition;
                    proof_scope_region = region;
                    proof_scope_region_snapshot = region_snapshot;
                  };
            }
          in
          if
            not
              (List.exists
                 (same_proof_activation_authority authority)
                 session.proof_activation_authorities)
          then
            session.proof_activation_authorities <-
              authority :: session.proof_activation_authorities;
          Ok authority
    | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ } ->
        Error "raw Exec body cannot issue proof-region activation authority"
    | Sst.Proof_body _ | Sst.Spec_definition _
    | Sst.Recursive_spec_definition _ | Sst.External_specification _
    | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        Error "Exec proof-region authority requires an Exec body"

let proof_prefinal_obligation_digest (obligation : Vir.obligation) =
  obligation_fingerprint
    {
      obligation with
      obligation_index = 0;
      assumptions = proof_semantic_terms obligation.assumptions;
      required_preceding_safety =
        proof_semantic_terms obligation.required_preceding_safety;
      path_condition = proof_semantic_terms obligation.path_condition;
    }

let snapshot_proof_activation ?(ground_matches = []) authority ~activations
    (obligation : Vir.obligation) =
  let attacked name value =
    if !ground_constructor_match_attack_for_testing = Some name
    then value ^ ":wrong"
    else value
  in
  {
    proof_snapshot_issuer = private_issuer;
    proof_snapshot_session = authority.proof_authority_session;
    proof_snapshot_token = ref ();
    proof_snapshot_authority = authority;
    proof_snapshot_activations = activations;
    proof_snapshot_ground_matches = ground_matches;
    proof_snapshot_ground_obligation_digest =
      (match ground_matches with
      | [] -> None
      | _ ->
          Some
            (attacked "obligation"
               (proof_prefinal_obligation_digest obligation)));
    proof_snapshot_ground_activation_digest =
      (match ground_matches with
      | [] -> None
      | _ -> Some (attacked "reveal" (activation_digest activations)));
    proof_snapshot_provisional_index = obligation.obligation_index;
    proof_snapshot_obligation_digest =
      proof_prefinal_obligation_digest obligation;
    proof_snapshot_assumptions_digest =
      proof_assumptions_digest obligation.assumptions
        obligation.required_preceding_safety;
    proof_snapshot_path_digest =
      proof_path_digest obligation.path_condition;
  }

let registered_proof_authority (session : t) authority =
  authority.proof_authority_issuer == private_issuer
  && authority.proof_authority_session == session.session
  && List.exists
       (same_proof_activation_authority authority)
       session.proof_activation_authorities

let proof_scope_string = function
  | Full_proof_execution -> "full-proof"
  | Exec_proof_region scope ->
      "exec-region:" ^ scope.proof_scope_region_snapshot

let proof_scope_prefix = function
  | Full_proof_execution -> ""
  | Exec_proof_region _ as scope ->
      "scope=" ^ proof_scope_string scope ^ " "

let finalize_proof_activation (session : t) snapshot
    (obligation : Vir.obligation) =
  let authority = snapshot.proof_snapshot_authority in
  let snapshot_obligation = Broadcast_vc_private.base_obligation obligation in
  if not session.active then Error "verification session is destroyed"
  else if
    snapshot.proof_snapshot_issuer != private_issuer
    || snapshot.proof_snapshot_session != session.session
    || snapshot.proof_snapshot_token == private_issuer
  then Error "proof activation snapshot issuer or session mismatch"
  else if not (registered_proof_authority session authority) then
    Error "proof activation authority is stale or foreign"
  else if
    not
      (match
         ( snapshot.proof_snapshot_ground_matches,
           snapshot.proof_snapshot_ground_obligation_digest,
           snapshot.proof_snapshot_ground_activation_digest )
       with
      | [], None, None -> true
      | matches, Some obligation_digest, Some routed_activation_digest ->
          matches <> []
          && List.for_all
               (validate_ground_constructor_match session authority obligation)
               matches
          && String.equal obligation_digest
               (proof_prefinal_obligation_digest snapshot_obligation)
          && String.equal routed_activation_digest
               (activation_digest snapshot.proof_snapshot_activations)
      | _ -> false)
  then Error "ground constructor witness authentication failed"
  else if
    List.exists
      (fun token -> token == snapshot.proof_snapshot_token)
      session.finalized_proof_activation_snapshots
  then Error "proof activation snapshot was replayed"
  else if
    not
      (same_function_ref_id obligation.function_ref
         authority.proof_authority_callable)
  then Error "proof activation snapshot belongs to another callable"
  else if not (proof_scope_allows_obligation session authority obligation) then
    Error "proof activation snapshot is outside its authenticated scope"
  else if
    not
      (String.equal snapshot.proof_snapshot_assumptions_digest
         (proof_assumptions_digest snapshot_obligation.assumptions
            snapshot_obligation.required_preceding_safety))
  then Error "proof activation snapshot assumptions mismatch"
  else if
    not
      (String.equal snapshot.proof_snapshot_obligation_digest
         (proof_prefinal_obligation_digest snapshot_obligation))
  then Error "proof activation snapshot obligation mismatch"
  else
    let path_digest = proof_path_digest obligation.path_condition in
    if
      not
        (String.equal snapshot.proof_snapshot_path_digest path_digest)
    then Error "proof activation snapshot path mismatch"
    else
      let manifest =
        {
          proof_manifest_issuer = private_issuer;
          proof_manifest_session = session.session;
          proof_manifest_token = ref ();
          proof_manifest_snapshot = snapshot;
          proof_manifest_obligation_index = obligation.obligation_index;
          proof_manifest_kind = kind_string obligation.kind;
          proof_manifest_path_digest = path_digest;
          proof_manifest_obligation_fingerprint =
            obligation_fingerprint obligation;
        }
      in
      session.proof_activation_manifests <-
        manifest :: session.proof_activation_manifests;
      session.finalized_proof_activation_snapshots <-
        snapshot.proof_snapshot_token
        :: session.finalized_proof_activation_snapshots;
      (match !proof_activation_observation_events with
      | None -> ()
      | Some events ->
          let activations =
            snapshot.proof_snapshot_activations
            |> List.map (fun activation ->
                   Printf.sprintf "%s#%d:%d"
                     activation.Spec_unfolding.function_id.function_name
                     activation.function_id.function_index activation.depth)
            |> String.concat ","
          in
          proof_activation_observation_events :=
            Some
              (Printf.sprintf
                 "issued %scallable=%s#%d final=%d provisional=%d kind=%s path=%s activations=[%s]"
                 (proof_scope_prefix authority.proof_authority_scope)
                 authority.proof_authority_callable.function_name
                 authority.proof_authority_callable.function_index
                 obligation.obligation_index
                 snapshot.proof_snapshot_provisional_index
                 manifest.proof_manifest_kind path_digest activations
              :: events));
      Ok manifest

let proof_activation_batch (session : t) members manifests =
  let encoded_members =
    List.map
      (fun (authority, obligation) ->
        (authority, obligation_fingerprint obligation))
      members
  in
  let batch =
    {
    proof_batch_issuer = private_issuer;
    proof_batch_session = session.session;
    proof_batch_token = ref ();
    proof_batch_expected_members = encoded_members;
    proof_batch_members = encoded_members;
    proof_batch_manifests = manifests;
    }
  in
  if members = [] then batch
  else
  match !proof_activation_batch_attack_for_testing with
  | Some "missing" ->
      {
        batch with
        proof_batch_manifests =
          (match manifests with [] -> [] | _ :: rest -> rest);
      }
  | Some "duplicate" ->
      {
        batch with
        proof_batch_manifests =
          (match manifests with
          | [] -> []
          | first :: _ -> first :: manifests);
      }
  | Some "foreign" -> { batch with proof_batch_session = ref () }
  | Some "stale" -> { batch with proof_batch_token = private_issuer }
  | Some "path-mismatch" ->
      {
        batch with
        proof_batch_members =
          (match batch.proof_batch_members with
          | [] -> []
          | (authority, fingerprint) :: rest ->
              (authority, fingerprint ^ ":wrong") :: rest);
      }
  | Some "scope-mismatch" ->
      {
        batch with
        proof_batch_members =
          (match batch.proof_batch_members with
          | [] -> []
          | (authority, fingerprint) :: rest ->
              ( {
                  authority with
                  proof_authority_scope = Full_proof_execution;
                },
                fingerprint )
              :: rest);
      }
  | Some "duplicate-member" ->
      {
        batch with
        proof_batch_members =
          (match batch.proof_batch_members with
          | [] -> []
          | first :: _ -> first :: batch.proof_batch_members);
      }
  | Some "paired-missing" ->
      {
        batch with
        proof_batch_members =
          (match batch.proof_batch_members with
          | [] -> []
          | _ :: rest -> rest);
        proof_batch_manifests =
          (match batch.proof_batch_manifests with
          | [] -> []
          | _ :: rest -> rest);
      }
  | Some _ | None -> batch

let consume_proof_activation_batch (session : t) batch
    (execution : Vir.function_execution) =
  let manifests = batch.proof_batch_manifests in
  let expected_members = batch.proof_batch_expected_members in
  let obligations = execution.obligations in
  let members =
    match (!proof_activation_batch_attack_for_testing, execution.obligations) with
    | Some "outer-member", _ :: _ ->
        let outer = List.hd (List.rev execution.obligations) in
        (match batch.proof_batch_members with
        | [] -> []
        | (authority, _) :: rest ->
            (authority, obligation_fingerprint outer) :: rest)
    | (Some _ | None), _ -> batch.proof_batch_members
  in
  let registered manifest =
    manifest.proof_manifest_issuer == private_issuer
    && manifest.proof_manifest_session == session.session
    && List.exists
         (fun candidate ->
           candidate.proof_manifest_token == manifest.proof_manifest_token)
         session.proof_activation_manifests
  in
  let rec select_members selected members obligations =
    match (members, obligations) with
    | [], _ -> Ok (List.rev selected)
    | _ :: _, [] -> Error "proof activation batch member is not emitted"
    | ((_, expected_fingerprint) as member) :: members,
      obligation :: obligations ->
        if
          String.equal expected_fingerprint
            (obligation_fingerprint obligation)
        then
          select_members ((member, obligation) :: selected) members
            obligations
        else select_members selected (member :: members) obligations
  in
  let scope_matches_execution authority =
    execution.body_provenance = authority.proof_authority_body_provenance
    && same_function_ref_id execution.function_ref
         authority.proof_authority_callable
    &&
    match authority.proof_authority_scope with
    | Full_proof_execution -> execution.mode = Sst.Proof
    | Exec_proof_region scope ->
        execution.mode = Sst.Exec
        && scope.proof_scope_definition.function_id
           = authority.proof_authority_callable
        &&
        (match session.validated with
        | Some validated -> validated == scope.proof_scope_validated
        | None -> false)
  in
  let same_member (left_authority, left_fingerprint)
      (right_authority, right_fingerprint) =
    same_proof_activation_authority left_authority right_authority
    && String.equal left_fingerprint right_fingerprint
  in
  let rec same_members left right =
    match (left, right) with
    | [], [] -> true
    | left :: lefts, right :: rights ->
        same_member left right && same_members lefts rights
    | [], _ :: _ | _ :: _, [] -> false
  in
  let expected_authorities =
    List.fold_left
      (fun authorities (authority, _) ->
        if
          List.exists
            (same_proof_activation_authority authority)
            authorities
        then authorities
        else authorities @ [ authority ])
      [] expected_members
  in
  let derive_expected_members () =
    let rec loop derived = function
      | [] -> Ok (List.rev derived)
      | obligation :: obligations ->
          let matching =
            List.filter
              (fun authority ->
                scope_matches_execution authority
                && proof_scope_allows_obligation session authority obligation)
              expected_authorities
          in
          (match (execution.mode, matching) with
          | Sst.Proof, [ authority ]
          | Sst.Exec, [ authority ] ->
              loop
                ((authority, obligation_fingerprint obligation) :: derived)
                obligations
          | Sst.Exec, [] | Sst.Spec, [] ->
              loop derived obligations
          | Sst.Proof, [] ->
              Error "Proof activation batch omits an expected obligation"
          | (Sst.Proof | Sst.Exec | Sst.Spec), _ ->
              Error "proof activation batch has overlapping scope authorities")
    in
    loop [] obligations
  in
  let rec exact routes manifests selected =
    match (manifests, selected) with
    | [], [] -> Ok (List.rev routes)
    | manifest :: manifests, ((member_authority, _), obligation) :: selected ->
        let snapshot = manifest.proof_manifest_snapshot in
        let authority = snapshot.proof_snapshot_authority in
        let fingerprint = obligation_fingerprint obligation in
        let path_digest = proof_path_digest obligation.path_condition in
        if not (registered manifest) then
          Error "proof activation manifest is stale, foreign, or counterfeit"
        else if
          not
            (registered_proof_authority session authority)
        then Error "proof activation manifest authority is stale or foreign"
        else if
          not
            (same_proof_activation_authority member_authority authority)
        then Error "proof activation manifest scope mismatch"
        else if
          manifest.proof_manifest_obligation_index
          <> obligation.obligation_index
        then Error "proof activation manifest obligation index mismatch"
        else if
          not
            (String.equal manifest.proof_manifest_kind
               (kind_string obligation.kind))
        then Error "proof activation manifest obligation kind mismatch"
        else if
          not
            (same_function_ref_id obligation.function_ref
               authority.proof_authority_callable)
        then Error "proof activation manifest callable mismatch"
        else if
          not (scope_matches_execution authority)
        then Error "proof activation manifest body or mode mismatch"
        else if not (proof_scope_allows_obligation session authority obligation)
        then Error "proof activation manifest is outside its region scope"
        else if
          not
            (String.equal
               (program_fingerprint authority.proof_authority_program)
               (program_fingerprint session.identity))
        then Error "proof activation manifest program or artifact mismatch"
        else if
          not
            (String.equal manifest.proof_manifest_path_digest path_digest)
          || not
               (String.equal snapshot.proof_snapshot_path_digest path_digest)
        then Error "proof activation manifest path mismatch"
        else if
          not
            (String.equal manifest.proof_manifest_obligation_fingerprint
               fingerprint)
        then Error "proof activation manifest obligation mismatch"
        else
          exact
            ({
               proof_route_obligation_fingerprint = fingerprint;
               proof_route_activations = snapshot.proof_snapshot_activations;
               proof_route_ground_matches =
                 snapshot.proof_snapshot_ground_matches;
             }
            :: routes)
            manifests selected
    | [], _ :: _ -> Error "proof activation manifest is missing"
    | _ :: _, [] -> Error "proof activation manifest is unused"
  in
  if not session.active then Error "verification session is destroyed"
  else if
    batch.proof_batch_issuer != private_issuer
    || batch.proof_batch_session != session.session
    || batch.proof_batch_token == private_issuer
  then Error "proof activation batch issuer or session mismatch"
  else if
    List.exists
      (fun token -> token == batch.proof_batch_token)
      session.consumed_proof_activation_batches
  then Error "proof activation batch was replayed"
  else if
    List.exists
      (fun manifest ->
        List.exists
          (fun token -> token == manifest.proof_manifest_token)
          session.consumed_proof_activation_manifests)
      manifests
  then Error "proof activation manifest was replayed"
  else if
    not
      (List.for_all
         (fun (authority, _) ->
           registered_proof_authority session authority
           && scope_matches_execution authority)
         expected_members)
  then Error "proof activation expected scope is stale or foreign"
  else
    let* derived_expected_members = derive_expected_members () in
    if not (same_members expected_members derived_expected_members) then
      Error "proof activation expected scope coverage mismatch"
    else if not (same_members members expected_members) then
      Error "proof activation supplied member coverage mismatch"
    else if List.length manifests <> List.length expected_members then
    Error "proof activation manifest cardinality mismatch"
  else if
    match execution.mode with
    | Sst.Proof ->
        List.length expected_members <> List.length obligations
        || List.exists
             (fun (authority, _) ->
               match authority.proof_authority_scope with
               | Full_proof_execution -> false
               | Exec_proof_region _ -> true)
             expected_members
    | Sst.Exec ->
        List.exists
          (fun (authority, _) ->
            match authority.proof_authority_scope with
            | Exec_proof_region _ -> false
            | Full_proof_execution -> true)
          expected_members
    | Sst.Spec -> expected_members <> []
  then Error "proof activation batch scope coverage mismatch"
  else
    let tokens =
      List.map (fun manifest -> manifest.proof_manifest_token) manifests
    in
    let rec has_duplicate = function
      | [] -> false
      | token :: rest ->
          List.exists (fun candidate -> candidate == token) rest
          || has_duplicate rest
    in
    if has_duplicate tokens then
      Error "proof activation manifest is duplicated"
    else
      let* selected = select_members [] expected_members obligations in
      let* routes = exact [] manifests selected in
      observed_proof_activation_routes_consumed :=
        !observed_proof_activation_routes_consumed + List.length routes;
      session.consumed_proof_activation_batches <-
        batch.proof_batch_token
        :: session.consumed_proof_activation_batches;
      session.consumed_proof_activation_manifests <-
        List.rev_append
          (List.map
             (fun manifest -> manifest.proof_manifest_token)
             manifests)
          session.consumed_proof_activation_manifests;
      (match !proof_activation_observation_events with
      | None -> ()
      | Some events ->
          let consumed =
            List.map
              (fun manifest ->
                let callable =
                  manifest.proof_manifest_snapshot.proof_snapshot_authority
                    .proof_authority_callable
                in
                let scope =
                  manifest.proof_manifest_snapshot.proof_snapshot_authority
                    .proof_authority_scope
                in
                Printf.sprintf
                  "consumed %scallable=%s#%d final=%d kind=%s path=%s"
                  (proof_scope_prefix scope)
                  callable.function_name callable.function_index
                  manifest.proof_manifest_obligation_index
                  manifest.proof_manifest_kind
                  manifest.proof_manifest_path_digest)
              manifests
          in
          proof_activation_observation_events :=
            Some
              (Printf.sprintf
                 "bijection callable=%s#%d issued=%d consumed=%d unused=0 fallback=0"
                 execution.function_ref.function_name
                 execution.function_ref.function_index
                 (List.length manifests) (List.length manifests)
              :: List.rev_append consumed events));
      Ok routes

let proof_activation_route route (obligation : Vir.obligation) =
  if
    String.equal route.proof_route_obligation_fingerprint
      (obligation_fingerprint obligation)
  then
    Ok
      {
        routed_proof_activations = route.proof_route_activations;
        routed_ground_constructors =
          List.map
            (fun match_ ->
              (match_.ground_match_symbol, match_.ground_match_constructor))
            route.proof_route_ground_matches;
      }
  else Error "proof activation route does not match the dispatched obligation"

let proof_activation_route_matches route (obligation : Vir.obligation) =
  String.equal route.proof_route_obligation_fingerprint
    (obligation_fingerprint obligation)

let routed_proof_activations route = route.routed_proof_activations
let routed_ground_constructors route = route.routed_ground_constructors

let exact_return_boundary snapshot (obligation : Vir.obligation) =
  match (obligation.kind, obligation.goal) with
  | ( Vir.Invariant_validity
        {
          invariant_id;
          abstract_type;
          model;
          predicate;
          operation;
          boundary = Vir.Function_return;
        },
      Vir.Boolean_invariant_application
        {
          invariant_id = goal_invariant;
          model = goal_model;
          predicate = goal_predicate;
          _;
        } ) ->
      String.equal invariant_id snapshot.invariant.invariant_id
      && String.equal goal_invariant invariant_id
      && abstract_type.aggregate_type_index = snapshot.invariant.abstract_type.type_index
      && String.equal abstract_type.aggregate_type_name
           snapshot.invariant.abstract_type.type_name
      && same_function_ref_id model snapshot.invariant.model
      && goal_model = snapshot.invariant.model
      && same_function_ref_id predicate snapshot.invariant.predicate
      && goal_predicate = snapshot.invariant.predicate
      && same_function_ref_id operation snapshot.callee
  | _ -> false

let same_callee_snapshot left right =
  String.equal
    (callee_snapshot_fingerprint left)
    (callee_snapshot_fingerprint right)

let authorize_obligations session snapshot
    (execution : Vir.function_execution) =
  if not session.active then Error "verification session is destroyed"
  else if session.issuer != private_issuer then Error "receipt issuer mismatch"
  else if
    not
      (String.equal
         (program_fingerprint snapshot.program)
         (program_fingerprint session.identity))
  then Error "receipt program snapshot mismatch"
  else if
    not
      (List.exists
         (fun authorized -> same_callee_snapshot authorized snapshot)
         session.authorized_callees)
  then Error "receipt callee snapshot is not authenticated for this session"
  else if not (same_function_ref_id execution.function_ref snapshot.callee) then
    Error "receipt callee execution mismatch"
  else if execution.mode <> snapshot.mode then
    Error "receipt callee execution mode mismatch"
  else if execution.body_provenance <> snapshot.body_provenance then
    Error "receipt callee body provenance mismatch"
  else if
    List.exists
      (fun (obligation : Vir.obligation) ->
        not (same_function_ref_id obligation.Vir.function_ref snapshot.callee))
      execution.obligations
  then Error "receipt obligation belongs to a different callable"
  else if
    List.exists
      (fun (receipt : receipt) ->
        same_callee_snapshot receipt.callee snapshot)
      session.receipts
  then Error "receipt was already issued for this callee snapshot"
  else
    let obligation_fingerprints =
      List.map obligation_fingerprint execution.obligations
    in
    let return_boundary_fingerprints =
      execution.obligations
      |> List.filter (exact_return_boundary snapshot)
      |> List.map obligation_fingerprint
    in
    if return_boundary_fingerprints = [] then
      Error "receipt obligation set lacks the exact invariant return boundary"
    else
      let manifest =
        {
          issuer = private_issuer;
          session = session.session;
          token = ref ();
          callee = snapshot;
          obligation_fingerprints;
          obligation_set_fingerprint =
            obligation_set_fingerprint obligation_fingerprints;
          return_boundary_fingerprints;
        }
      in
      session.obligation_manifests <-
        manifest :: session.obligation_manifests;
      Ok manifest

let registered_manifest (session : t) (manifest : obligation_manifest) =
  manifest.issuer == private_issuer
  && manifest.session == session.session
  && List.exists
       (fun (candidate : obligation_manifest) ->
         candidate.token == manifest.token)
       session.obligation_manifests

let complete session manifest results =
  if not session.active then Error "verification session is destroyed"
  else if not (registered_manifest session manifest) then
    Error "receipt obligation manifest is not authenticated for this session"
  else
    let completed =
      List.map
        (fun (result : Solver_backend.obligation_result) ->
          obligation_fingerprint result.obligation)
        results
    in
    if manifest.obligation_fingerprints <> completed then
      Error "receipt obligation set is incomplete or mismatched"
    else if
      not
        (List.for_all
           (fun (result : Solver_backend.obligation_result) ->
             result.outcome = Solver_backend.Verified)
           results)
    then Error "receipt obligation set did not complete as Verified"
    else
      let completion =
        {
          issuer = private_issuer;
          session = session.session;
          token = ref ();
          manifest;
        }
      in
      session.verified_completions <-
        completion :: session.verified_completions;
      Ok completion

let issue session completion =
  let manifest = completion.manifest in
  if not session.active then Error "verification session is destroyed"
  else if completion.issuer != private_issuer then Error "receipt issuer mismatch"
  else if completion.session != session.session then
    Error "verified completion belongs to a different session"
  else if
    not
      (List.exists
         (fun (candidate : verified_completion) ->
           candidate.token == completion.token)
         session.verified_completions)
  then Error "verified completion is not authenticated for this session"
  else if not (registered_manifest session manifest) then
    Error "verified completion manifest is unavailable"
  else if
    List.exists
      (fun (receipt : receipt) ->
        same_callee_snapshot receipt.callee manifest.callee)
      session.receipts
  then Error "receipt was already issued for this callee snapshot"
  else
    let receipt : receipt =
      {
        issuer = private_issuer;
        session = session.session;
        token = ref ();
        callee = manifest.callee;
        obligation_fingerprints = manifest.obligation_fingerprints;
        obligation_set_fingerprint = manifest.obligation_set_fingerprint;
        return_boundary_fingerprints = manifest.return_boundary_fingerprints;
      }
    in
    session.obligation_manifests <-
      List.filter
        (fun (candidate : obligation_manifest) ->
          candidate.token != manifest.token)
        session.obligation_manifests;
    session.verified_completions <-
      List.filter
        (fun (candidate : verified_completion) ->
          candidate.token != completion.token)
        session.verified_completions;
    session.receipts <- receipt :: session.receipts;
    session.counters.receipts_issued <- session.counters.receipts_issued + 1;
    let transition_results =
      List.filter
        (fun capability ->
          capability.predecessor_callee = manifest.callee.callee
          && not
               (List.memq capability.predecessor_token
                  session.receipted_transition_predecessors))
        session.consumed_transition_predecessors
    in
    session.receipted_transition_predecessors <-
      List.map
        (fun capability -> capability.predecessor_token)
        transition_results
      @ session.receipted_transition_predecessors;
    session.counters.transition_result_receipts <-
      session.counters.transition_result_receipts
      + List.length transition_results;
    Ok ()

let find_receipt (session : t) snapshot =
  List.find_opt
    (fun (receipt : receipt) ->
      receipt.issuer == private_issuer
      && receipt.session == session.session
      && receipt.token != session.session
      && same_callee_snapshot receipt.callee snapshot
      && String.equal receipt.obligation_set_fingerprint
           (obligation_set_fingerprint receipt.obligation_fingerprints)
      && receipt.return_boundary_fingerprints <> [])
    session.receipts

let binding_string (binding : Sst.binding) =
  String.concat "\000"
    [
      string_of_int binding.id;
      binding.name;
      Sst.string_of_type binding.typ;
      (match binding.uniqueness with
      | Sst.Definitely_unique -> "unique"
      | Sst.Definitely_aliased -> "aliased");
      span_string binding.span;
    ]

let transition_predecessor_fingerprint capability =
  String.concat "\000"
    [
      program_fingerprint capability.predecessor_program;
      function_id_string capability.predecessor_caller;
      capability.predecessor_caller_key;
      capability.predecessor_caller_path;
      capability.predecessor_caller_binding_uid;
      capability.predecessor_caller_body;
      function_id_string capability.predecessor_callee;
      capability.predecessor_callee_key;
      capability.predecessor_callee_path;
      capability.predecessor_callee_binding_uid;
      capability.predecessor_callee_body;
      span_string capability.predecessor_call_span;
      capability.predecessor_call_path;
      capability.predecessor_call_instance;
      callee_snapshot_fingerprint capability.predecessor_source;
      span_string capability.predecessor_source_call_span;
      binding_string capability.predecessor_actual;
      capability.predecessor_actual_symbol;
      string_of_int capability.predecessor_actual_version;
      capability.predecessor_actual_path;
      binding_string capability.predecessor_formal;
      binding_string capability.predecessor_root;
      instance_mode_name capability.predecessor_mode;
      Sst.string_of_type capability.predecessor_type;
      invariant_fingerprint capability.predecessor_invariant;
      string_of_int capability.predecessor_owned_version;
      String.concat "\001" capability.predecessor_obligation_snapshot;
      capability.predecessor_obligation_fingerprint;
      string_of_bool capability.predecessor_branch_intersection;
    ]
  |> digest

let preview_transition_predecessor session ~validated ~invariants ~caller
    ~callee ~source ~call_span ~source_call_span ~call_path ~actual
    ~actual_version ~actual_path ~formal ~root ~mode ~typ ~owned_version
    ~obligation_snapshot ~branch_intersection =
  let actual : Sst.binding = actual in
  let formal : Sst.binding = formal in
  let root : Sst.binding = root in
  if not session.active then Error "verification session is destroyed"
  else if session.issuer != private_issuer then
    Error "transition predecessor issuer mismatch"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
  then Error "transition predecessor validated program mismatch"
  else if
    not
      (match session.invariants with
      | Some owned -> owned == invariants
      | None -> false)
  then Error "transition predecessor invariant environment mismatch"
  else
    let* caller_key = canonical_callable_key session caller in
    let* caller_path = canonical_callable_resolved_path session caller in
    let* caller_binding_uid =
      canonical_callable_binding_uid session caller
    in
    let* callee_key = canonical_callable_key session callee in
    let* callee_path = canonical_callable_resolved_path session callee in
    let* callee_binding_uid =
      canonical_callable_binding_uid session callee
    in
    let* source_snapshot =
      callee_snapshot session ~validated ~invariants source
    in
    let* handle =
      match Type_invariant.find_for_operation invariants callee.function_id with
      | Some (handle, Sst.Unique_transition) -> Ok handle
      | Some (_, _) | None ->
          Error
            "transition predecessor target is not an authenticated local transition"
    in
    let* () =
      match (callee.body, source.body) with
      | ( Sst.Checked_exec
            { provenance = Sst.Authenticated_typedtree _; _ },
          Sst.Checked_exec
            { provenance = Sst.Authenticated_typedtree _; _ } ) ->
          Ok ()
      | _ ->
          Error
            "transition predecessor requires authenticated local checked bodies"
    in
    let* () =
      if
        source_snapshot.result_mode = mode
        && source_snapshot.result_type = typ
        && same_type_id source_snapshot.invariant.abstract_type
             (Type_invariant.abstract_type handle)
        && source.function_id <> callee.function_id
      then Ok ()
      else
        Error
          "transition predecessor source result identity or dependency is invalid"
    in
    let* () =
      match find_receipt session source_snapshot with
      | Some _ -> Ok ()
      | None ->
          Error
            "transition predecessor source has no completed local result receipt"
    in
    let* () =
      if
        caller.mode = Sst.Exec && callee.mode = Sst.Exec
        && (mode = Sst.Exec_instance || mode = Sst.Tracked_instance)
        && actual.uniqueness = Sst.Definitely_unique
        && formal.uniqueness = Sst.Definitely_unique
        && root.id = formal.id && root.typ = typ && actual.typ = typ
        && obligation_snapshot <> []
      then Ok ()
      else
        Error
          "transition predecessor call/formal/root/mode snapshot is not exact"
    in
    let predecessor_actual_symbol =
      String.concat ":"
        [
          caller_key;
          string_of_int actual.id;
          actual.name;
          span_string source_call_span;
        ]
    in
    let predecessor_call_instance =
      String.concat "\000"
        [
          caller_key;
          callee_key;
          span_string call_span;
          call_path;
          predecessor_actual_symbol;
          string_of_int actual_version;
          actual_path;
          string_of_int formal.id;
        ]
      |> digest
    in
    let obligation_fingerprint =
      obligation_set_fingerprint obligation_snapshot
    in
    let capability =
      {
        predecessor_issuer = private_issuer;
        predecessor_affinity = transition_predecessor_affinity;
        predecessor_session = session.session;
        predecessor_token = ref ();
        predecessor_program = session.identity;
        predecessor_caller = caller.function_id;
        predecessor_caller_key = caller_key;
        predecessor_caller_path = caller_path;
        predecessor_caller_binding_uid = caller_binding_uid;
        predecessor_caller_body = callable_body_snapshot session caller;
        predecessor_callee = callee.function_id;
        predecessor_callee_key = callee_key;
        predecessor_callee_path = callee_path;
        predecessor_callee_binding_uid = callee_binding_uid;
        predecessor_callee_body = callable_body_snapshot session callee;
        predecessor_call_span = call_span;
        predecessor_call_path = call_path;
        predecessor_call_instance;
        predecessor_source = source_snapshot;
        predecessor_source_call_span = source_call_span;
        predecessor_actual = actual;
        predecessor_actual_symbol;
        predecessor_actual_version = actual_version;
        predecessor_actual_path = actual_path;
        predecessor_formal = formal;
        predecessor_root = root;
        predecessor_mode = mode;
        predecessor_type = typ;
        predecessor_invariant = invariant_snapshot handle;
        predecessor_owned_version = owned_version;
        predecessor_obligation_snapshot = obligation_snapshot;
        predecessor_obligation_fingerprint = obligation_fingerprint;
        predecessor_branch_intersection = branch_intersection;
      }
    in
    if
      List.exists
        (fun existing ->
          String.equal existing.predecessor_call_instance
            capability.predecessor_call_instance)
        (session.previewed_transition_predecessors
        @ session.active_transition_predecessors
        @ session.consumed_transition_predecessors)
    then Error "transition predecessor call instance was already previewed"
    else (
      session.previewed_transition_predecessors <-
        capability :: session.previewed_transition_predecessors;
      session.previewed_transition_predecessor_seals <-
        ( capability.predecessor_token,
          transition_predecessor_fingerprint capability )
        :: session.previewed_transition_predecessor_seals;
      Ok capability)

let mutate_transition_predecessor_for_testing attack capability =
  let wrong_id (id : Sst.function_id) =
    { id with function_name = id.function_name ^ ".wrong" }
  in
  match attack with
  | "session" ->
      capability.predecessor_session <- ref ();
      capability
  | "program" ->
      capability.predecessor_program <-
        {
          capability.predecessor_program with
          program_snapshot =
            capability.predecessor_program.program_snapshot ^ ":wrong";
        };
      capability
  | "cmt" ->
      capability.predecessor_program <-
        {
          capability.predecessor_program with
          cmt_identity = capability.predecessor_program.cmt_identity ^ ":wrong";
        };
      capability
  | "family" ->
      capability.predecessor_program <-
        {
          capability.predecessor_program with
          family_identity =
            capability.predecessor_program.family_identity ^ ":wrong";
        };
      capability
  | "caller" ->
      capability.predecessor_caller <- wrong_id capability.predecessor_caller;
      capability
  | "caller-key" ->
      capability.predecessor_caller_key <-
        capability.predecessor_caller_key ^ ":wrong";
      capability
  | "caller-path" ->
      capability.predecessor_caller_path <-
        capability.predecessor_caller_path ^ ":wrong";
      capability
  | "caller-binding" ->
      capability.predecessor_caller_binding_uid <-
        capability.predecessor_caller_binding_uid ^ ":wrong";
      capability
  | "caller-body" ->
      capability.predecessor_caller_body <-
        capability.predecessor_caller_body ^ ":wrong";
      capability
  | "callee" ->
      capability.predecessor_callee <- wrong_id capability.predecessor_callee;
      capability
  | "callee-key" ->
      capability.predecessor_callee_key <-
        capability.predecessor_callee_key ^ ":wrong";
      capability
  | "callee-path" ->
      capability.predecessor_callee_path <-
        capability.predecessor_callee_path ^ ":wrong";
      capability
  | "callee-binding" ->
      capability.predecessor_callee_binding_uid <-
        capability.predecessor_callee_binding_uid ^ ":wrong";
      capability
  | "callee-body" ->
      capability.predecessor_callee_body <-
        capability.predecessor_callee_body ^ ":wrong";
      capability
  | "call" ->
      capability.predecessor_call_span <-
        { capability.predecessor_call_span with file = "wrong-call.ml" };
      capability
  | "path" ->
      capability.predecessor_call_path <-
        capability.predecessor_call_path ^ ":wrong";
      capability
  | "instance" ->
      capability.predecessor_call_instance <-
        capability.predecessor_call_instance ^ ":wrong";
      capability
  | "source" ->
      capability.predecessor_source <-
        {
          capability.predecessor_source with
          callee = wrong_id capability.predecessor_source.callee;
        };
      capability
  | "source-call" ->
      capability.predecessor_source_call_span <-
        { capability.predecessor_source_call_span with file = "wrong-source.ml" };
      capability
  | "actual" ->
      capability.predecessor_actual <-
        { capability.predecessor_actual with id = capability.predecessor_actual.id + 1 };
      capability
  | "actual-path" ->
      capability.predecessor_actual_path <-
        capability.predecessor_actual_path ^ ":wrong";
      capability
  | "actual-symbol" ->
      capability.predecessor_actual_symbol <-
        capability.predecessor_actual_symbol ^ ":wrong";
      capability
  | "formal" ->
      capability.predecessor_formal <-
        { capability.predecessor_formal with id = capability.predecessor_formal.id + 1 };
      capability
  | "root" ->
      capability.predecessor_root <-
        { capability.predecessor_root with id = capability.predecessor_root.id + 1 };
      capability
  | "version" ->
      capability.predecessor_owned_version <-
        capability.predecessor_owned_version + 1;
      capability
  | "mode" ->
      capability.predecessor_mode <- Sst.Ghost_instance;
      capability
  | "type" ->
      capability.predecessor_type <- Sst.Int;
      capability
  | "invariant" ->
      capability.predecessor_invariant <-
        {
          capability.predecessor_invariant with
          invariant_id = capability.predecessor_invariant.invariant_id ^ ":wrong";
        };
      capability
  | "model" ->
      capability.predecessor_invariant <-
        {
          capability.predecessor_invariant with
          model = wrong_id capability.predecessor_invariant.model;
        };
      capability
  | "predicate" ->
      capability.predecessor_invariant <-
        {
          capability.predecessor_invariant with
          predicate = wrong_id capability.predecessor_invariant.predicate;
        };
      capability
  | "obligation" ->
      capability.predecessor_obligation_snapshot <-
        capability.predecessor_obligation_snapshot @ [ "extra" ];
      capability
  | "obligation-fingerprint" ->
      capability.predecessor_obligation_fingerprint <-
        capability.predecessor_obligation_fingerprint ^ ":wrong";
      capability
  | "issuer" ->
      capability.predecessor_issuer <- ref ();
      capability
  | "affinity" ->
      capability.predecessor_affinity <- ref ();
      capability
  | "branch-only" ->
      capability.predecessor_call_path <-
        capability.predecessor_call_path ^ ":branch-only";
      capability
  | "divergent-join" ->
      capability.predecessor_branch_intersection <-
        not capability.predecessor_branch_intersection;
      capability
  | "stale" ->
      capability.predecessor_actual_version <-
        capability.predecessor_actual_version + 1;
      capability
  | "copied" -> { capability with predecessor_token = ref () }
  | _ -> capability

let transition_predecessor_seal_matches (session : t)
    (capability : transition_predecessor_capability) =
  capability.predecessor_issuer == private_issuer
  && capability.predecessor_affinity == transition_predecessor_affinity
  && capability.predecessor_session == session.session
  && String.equal
       (program_fingerprint capability.predecessor_program)
       (program_fingerprint session.identity)
  &&
  match
    List.find_map
      (fun (token, seal) ->
        if token == capability.predecessor_token then Some seal else None)
      session.previewed_transition_predecessor_seals
  with
  | Some seal ->
      String.equal seal (transition_predecessor_fingerprint capability)
      && String.equal capability.predecessor_obligation_fingerprint
           (obligation_set_fingerprint
              capability.predecessor_obligation_snapshot)
      && Option.is_some
           (find_receipt session capability.predecessor_source)
  | None -> false

let activate_transition_predecessors (session : t) capabilities =
  let capabilities =
    match (!transition_predecessor_attack_for_testing, capabilities) with
    | Some "replay", _ -> (
        match !transition_predecessor_replay_for_testing with
        | Some replay -> [ replay ]
        | None -> capabilities)
    | Some "capture", capability :: _ ->
        transition_predecessor_replay_for_testing := Some capability;
        capabilities
    | Some attack, capability :: rest ->
        mutate_transition_predecessor_for_testing attack capability :: rest
    | Some _, [] | None, _ -> capabilities
  in
  let valid capability =
    transition_predecessor_seal_matches session capability
    && List.exists (( == ) capability)
         session.previewed_transition_predecessors
  in
  if not session.active then Error "verification session is destroyed"
  else if capabilities = [] then
    Error "transition predecessor transfer has no exact caller argument"
  else if not (List.for_all valid capabilities) then
    Error "transition predecessor capability failed exact reauthentication"
  else
    let tokens =
      List.map (fun capability -> capability.predecessor_token) capabilities
    in
    session.previewed_transition_predecessors <-
      List.filter
        (fun capability ->
          not (List.memq capability.predecessor_token tokens))
        session.previewed_transition_predecessors;
    session.active_transition_predecessors <-
      capabilities @ session.active_transition_predecessors;
    session.counters.transition_predecessor_transfers <-
      session.counters.transition_predecessor_transfers
      + List.length capabilities;
    Ok ()

let consume_transition_predecessors (session : t) ~validated ~invariants ~callee
    ~formal ~root ~root_value ~mode ~typ ~owned_version
    ~obligation_snapshot =
  let callee : Sst.function_definition = callee in
  let formal : Sst.binding = formal in
  let root : Sst.binding = root in
  let handle =
    Type_invariant.find_for_operation invariants callee.function_id
  in
  let root_is_exact_input =
    match root_value with
    | {
     Vir.aggregate_type;
     aggregate_desc =
       Vir.Aggregate_symbol
         { sort = Vir.Aggregate symbol_type; role = Vir.Input; _ };
    } ->
        aggregate_type = symbol_type
        &&
        (match typ with
        | Sst.Aggregate type_id ->
            aggregate_type.aggregate_type_index = type_id.type_index
            && String.equal aggregate_type.aggregate_type_name type_id.type_name
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ | Sst.Application _ -> false)
    | _ -> false
  in
  let matches capability =
    transition_predecessor_seal_matches session capability
    && capability.predecessor_callee = callee.function_id
    && String.equal capability.predecessor_callee_body
         (callable_body_snapshot session callee)
    && capability.predecessor_formal = formal
    && capability.predecessor_root = root
    && capability.predecessor_mode = mode
    && capability.predecessor_type = typ
    && capability.predecessor_owned_version = owned_version
    && capability.predecessor_obligation_snapshot = obligation_snapshot
    && String.equal capability.predecessor_obligation_fingerprint
         (obligation_set_fingerprint obligation_snapshot)
    &&
    match handle with
    | Some (handle, Sst.Unique_transition) ->
        invariant_fingerprint capability.predecessor_invariant
        = invariant_fingerprint (invariant_snapshot handle)
    | Some (_, _) | None -> false
  in
  if not session.active then Error "verification session is destroyed"
  else if
    not
      (match session.validated with
      | Some owned -> owned == validated
      | None -> false)
    || not
         (match session.invariants with
         | Some owned -> owned == invariants
         | None -> false)
  then Error "transition predecessor session affinity changed"
  else if not root_is_exact_input then
    Error "transition predecessor formal root is not the exact input symbol"
  else
    let matching =
      List.filter matches session.active_transition_predecessors
    in
    if matching = [] then
      if
        List.exists matches session.consumed_transition_predecessors
        && not !transition_predecessor_double_consume_for_testing
      then
        (* One affine consumption authorizes the compiler-generated path fanout
           of this single lowered call body.  It does not create another
           capability or another consumption event. *)
        Ok ()
      else
        Error
          "invariant transition predecessor has no authenticated closed validity fact"
    else
      let tokens =
        List.map (fun capability -> capability.predecessor_token) matching
      in
      session.active_transition_predecessors <-
        List.filter
          (fun capability ->
            not (List.memq capability.predecessor_token tokens))
          session.active_transition_predecessors;
      session.consumed_transition_predecessors <-
        matching @ session.consumed_transition_predecessors;
      session.counters.transition_predecessor_consumptions <-
        session.counters.transition_predecessor_consumptions
        + List.length matching;
      if !transition_predecessor_double_consume_for_testing then
        Error "consumed transition predecessor is not reusable"
      else Ok ()

let note_transition_preservation session = function
  | Vir.Nested_transition ->
      session.counters.transition_preservation_obligations <-
        session.counters.transition_preservation_obligations + 1;
      session.counters.transition_nested_reconstructions <-
        session.counters.transition_nested_reconstructions + 1
  | Vir.Direct_root_transition | Vir.Rebase_transition ->
      session.counters.transition_preservation_obligations <-
        session.counters.transition_preservation_obligations + 1;
      session.counters.transition_root_reconstructions <-
        session.counters.transition_root_reconstructions + 1

let result_symbol snapshot = function
  | {
   Vir.aggregate_type;
   aggregate_desc = Vir.Aggregate_symbol
     ({ sort = Vir.Aggregate symbol_type; role = Vir.Result; _ } as symbol);
  }
    when aggregate_type = symbol_type
         && aggregate_type.aggregate_type_index
            = snapshot.invariant.abstract_type.type_index
         && String.equal aggregate_type.aggregate_type_name
              snapshot.invariant.abstract_type.type_name ->
      Ok symbol
  | { Vir.aggregate_desc = Vir.Aggregate_symbol _; _ } ->
      Error "receipt result mode or aggregate type mismatch"
  | _ -> Error "receipt result is not a fresh aggregate symbol"

let consume (session : t) snapshot ~caller ~call_span ~path_condition ~result =
  if not session.active then Error "verification session is destroyed"
  else if
    not
      (String.equal
         (program_fingerprint snapshot.program)
         (program_fingerprint session.identity))
  then Error "receipt program snapshot mismatch"
  else
    match find_receipt session snapshot with
    | None -> Error "verified-callee receipt is unavailable"
    | Some receipt ->
        let* result_symbol = result_symbol snapshot result in
        let path_digest =
          String.concat "\000"
            (List.map Vir.boolean_term_to_string path_condition)
          |> digest
        in
        let* () =
          if
            List.exists
              (fun (symbol, previous_caller, previous_span, previous_path) ->
                symbol = result_symbol && previous_caller = caller
                && previous_span = call_span
                && String.equal previous_path path_digest)
              session.consumed_result_routes
          then
            Error "receipt result symbol was already consumed"
          else Ok ()
        in
        let closed_fact =
          Vir.Boolean_invariant_application
            {
              invariant_id = snapshot.invariant.invariant_id;
              model = snapshot.invariant.model;
              predicate = snapshot.invariant.predicate;
              value = result;
            }
        in
        let call_instance =
          {
            token = ref ();
            session = session.session;
            caller;
            call_span;
            path_digest;
            receipt;
            result_symbol;
          }
        in
        session.consumed_result_symbols <-
          result_symbol :: session.consumed_result_symbols;
        session.consumed_result_routes <-
          (result_symbol, caller, call_span, path_digest)
          :: session.consumed_result_routes;
        session.counters.receipts_consumed <-
          session.counters.receipts_consumed + 1;
        Ok { call_instance; closed_fact }

let consumed_closed_fact consumed = consumed.closed_fact
let consumed_matches_closed_fact consumed fact = consumed.closed_fact = fact

let same_consumed_fact left right =
  left.call_instance.token == right.call_instance.token
  && left.call_instance.session == right.call_instance.session
  && left.call_instance.caller = right.call_instance.caller
  && left.call_instance.call_span = right.call_instance.call_span
  && String.equal left.call_instance.path_digest right.call_instance.path_digest
  && left.call_instance.receipt == right.call_instance.receipt
  && left.call_instance.result_symbol = right.call_instance.result_symbol
  && left.closed_fact = right.closed_fact

let finite_registry session =
  if session.active && Finite_value_registry.is_active session.finite_registry
  then Ok session.finite_registry
  else Error "verification session is destroyed"

let install_recursive_spec_preservation session capabilities =
  if not session.active then Error "verification session is destroyed"
  else
    let* validated =
      match session.validated with
      | Some validated -> Ok validated
      | None -> Error "verification session lost its validated program"
    in
    let program = Sst_validation.program validated in
    let rec validate = function
      | [] -> Ok ()
      | capability :: rest ->
          let id = Recursive_spec_preservation.function_id capability in
          let* definition =
            match
              List.find_opt
                (fun definition ->
                  definition.Sst.function_id.function_index = id.function_index
                  && String.equal definition.function_id.function_name
                       id.function_name)
                program.Sst.functions
            with
            | Some definition -> Ok definition
            | None ->
                Error
                  "recursive Spec preservation capability has no exact local definition"
          in
          if
            definition.Sst.result_type
            <> Recursive_spec_preservation.result_type capability
          then Error "recursive Spec preservation result type is mismatched"
          else validate rest
    in
    let* () = validate capabilities in
    session.recursive_spec_preservation <-
      (if !suppress_recursive_spec_preservation_for_testing then []
       else if !suppress_recursive_spec_construction_route_for_testing then
         List.filter
           (fun capability ->
             Recursive_spec_preservation.construction_route_count capability
             = 0)
           capabilities
       else capabilities);
    Ok ()

let issue_recursive_spec_result session ~caller_callable
    ~(caller : Sst.function_id) ~(callee : Sst.function_id)
    ~call_span ~path_condition ~application_identity ~argument_receipts ~result
    ~mode ~typ ~rank =
  if !suppress_recursive_spec_result_issuance_for_testing then
    Error "recursive Spec result receipt issuance is suppressed"
  else if not session.active then Error "verification session is destroyed"
  else
    let* validated =
      match session.validated with
      | Some validated -> Ok validated
      | None -> Error "verification session lost its validated program"
    in
    let program = Sst_validation.program validated in
    let* caller_descriptor =
      match Sst_validation.find_callable validated caller with
      | Some descriptor -> Ok descriptor
      | None ->
          Error
            "recursive Spec result caller is absent from validated authority"
    in
    let* expected_caller_callable =
      canonical_callable_key session
        (Sst_validation.callable_definition caller_descriptor)
    in
    let* () =
      if String.equal expected_caller_callable caller_callable then Ok ()
      else Error "recursive Spec result caller callable identity mismatch"
    in
    let* definition =
      match
        List.find_opt
          (fun definition ->
            definition.Sst.function_id.function_index = callee.function_index
            && String.equal definition.function_id.function_name
                 callee.function_name)
          program.Sst.functions
      with
      | Some definition -> Ok definition
      | None -> Error "recursive Spec result callee is not exact local source"
    in
    let* capability =
      match
        List.find_opt
          (fun capability ->
            let id = Recursive_spec_preservation.function_id capability in
            id.function_index = callee.function_index
            && String.equal id.function_name callee.function_name)
          session.recursive_spec_preservation
      with
      | Some capability -> Ok capability
      | None ->
          Error
            "recursive Spec result lacks verified finite-preservation authority"
    in
    Finite_value_registry.issue_recursive_spec_result session.finite_registry
      ~capability ~program ~definition ~caller_callable ~caller ~call_span
      ~call_path_digest:
        (Finite_value_registry.result_path_digest path_condition)
      ~application_identity ~argument_receipts ~result ~mode ~typ ~rank

let same_finite_result_snapshot left right =
  String.equal
    (finite_result_snapshot_fingerprint left)
    (finite_result_snapshot_fingerprint right)

let registered_finite_result_snapshot (session : t)
    (snapshot : finite_result_snapshot) =
  snapshot.finite_snapshot_issuer == private_issuer
  && snapshot.finite_snapshot_session == session.session
  && List.exists
       (fun registered ->
         registered.finite_snapshot_token == snapshot.finite_snapshot_token
         && same_finite_result_snapshot registered snapshot)
       session.finite_result_snapshots

let record_finite_result_exit session snapshot ~result ~path_condition =
  if not (registered_finite_result_snapshot session snapshot) then
    Error "finite result snapshot is not authenticated"
  else
    let path_digest =
      Finite_value_registry.result_path_digest path_condition
    in
    if
      List.exists
        (fun (path, _) -> String.equal path path_digest)
        snapshot.finite_result_exits
    then Error "finite result exit path is duplicated"
    else
      let result_snapshot =
        Marshal.to_string result [ Marshal.No_sharing ] |> digest
      in
      snapshot.finite_result_exits <-
        (path_digest, result_snapshot) :: snapshot.finite_result_exits;
      Ok path_digest

let promote_finite_result session snapshot ~source ~result ~path_digest =
  let* registry = finite_registry session in
  if not (registered_finite_result_snapshot session snapshot) then
    Error "finite result snapshot is not authenticated"
  else
    let* receipt =
      Finite_value_registry.promote_result registry
        ~callee:(finite_result_snapshot_core_fingerprint snapshot)
        ~callable:snapshot.finite_callable_key
        ~body_snapshot:snapshot.finite_body_snapshot ~source ~result
        ~mode:snapshot.finite_result_mode ~typ:snapshot.finite_result_type
        ~rank:snapshot.finite_rank ~path_digest
    in
    let fact_snapshot =
      String.concat "\000"
        [ path_digest;
          snapshot.finite_callable_key;
          rank_fingerprint snapshot.finite_rank;
          Marshal.to_string result [ Marshal.No_sharing ] ]
      |> digest
    in
    snapshot.finite_result_facts <-
      (path_digest, fact_snapshot) :: snapshot.finite_result_facts;
    Ok receipt

let registered_finite_manifest (session : t)
    (manifest : finite_result_manifest) =
  manifest.finite_manifest_issuer == private_issuer
  && manifest.finite_manifest_session == session.session
  && registered_finite_result_snapshot session
       manifest.finite_manifest_snapshot
  && List.exists
       (fun candidate ->
         candidate.finite_manifest_token == manifest.finite_manifest_token
         && candidate.finite_manifest_candidate
            == manifest.finite_manifest_candidate
         && candidate.finite_obligation_fingerprints
            = manifest.finite_obligation_fingerprints
         && String.equal candidate.finite_obligation_set_fingerprint
              manifest.finite_obligation_set_fingerprint)
       session.finite_result_manifests

let authorize_finite_result_obligations session snapshot
    (execution : Vir.function_execution) =
  if not (registered_finite_result_snapshot session snapshot) then
    Error "finite result snapshot is not authenticated"
  else if
    not (same_function_ref_id execution.function_ref snapshot.finite_callee)
  then Error "finite result callee execution mismatch"
  else if execution.mode <> snapshot.finite_mode then
    Error "finite result callee execution mode mismatch"
  else if execution.body_provenance <> snapshot.finite_body_provenance then
    Error "finite result callee body provenance mismatch"
  else if
    List.exists
      (fun (obligation : Vir.obligation) ->
        not (same_function_ref_id obligation.function_ref snapshot.finite_callee))
      execution.obligations
  then Error "finite result obligation belongs to a different callable"
  else if Option.is_some !finite_result_snapshot_attack_for_testing then
    Error "finite result candidate identity was substituted"
  else if
    snapshot.finite_result_exits = []
    || List.exists
         (fun (path, _) ->
           not
             (List.exists
                (fun (fact_path, _) -> String.equal fact_path path)
                snapshot.finite_result_facts))
         snapshot.finite_result_exits
  then Ok None
  else
    let* () =
      let rec record = function
        | [] -> Ok ()
        | (path_digest, result_snapshot) :: rest ->
            let fact_snapshot =
              List.find_map
                (fun (path, fact) ->
                  if String.equal path path_digest then Some fact else None)
                snapshot.finite_result_facts
              |> Option.get
            in
            let* () =
              Direct_candidate.record_exit
                session.finite_candidate_lifecycle snapshot.finite_candidate
                ~path_digest ~result_snapshot ~fact_snapshot
            in
            record rest
      in
      record (List.rev snapshot.finite_result_exits)
    in
    let obligation_fingerprints =
      List.map obligation_fingerprint execution.obligations
    in
    let* finite_manifest_candidate =
      Direct_candidate.authorize_obligations
        session.finite_candidate_lifecycle snapshot.finite_candidate
        ~obligation_fingerprints
    in
    match finite_manifest_candidate with
    | None -> Ok None
    | Some finite_manifest_candidate ->
        let manifest =
          {
            finite_manifest_issuer = private_issuer;
            finite_manifest_session = session.session;
            finite_manifest_token = ref ();
            finite_manifest_snapshot = snapshot;
            finite_manifest_candidate;
            finite_obligation_fingerprints = obligation_fingerprints;
            finite_obligation_set_fingerprint =
              obligation_set_fingerprint obligation_fingerprints;
          }
        in
        session.finite_result_manifests <-
          manifest :: session.finite_result_manifests;
        session.counters.finite_result_manifests <-
          session.counters.finite_result_manifests + 1;
        Ok (Some manifest)

let complete_finite_result session manifest results =
  if not session.active then Error "verification session is destroyed"
  else if not (registered_finite_manifest session manifest) then
    Error "finite result manifest is not authenticated for this session"
  else
    let completed_fingerprints =
      List.map
        (fun (result : Solver_backend.obligation_result) ->
          obligation_fingerprint result.obligation)
        results
    in
    let all_verified =
      List.for_all
        (fun (result : Solver_backend.obligation_result) ->
          result.outcome = Solver_backend.Verified)
        results
    in
    let* finite_candidate_completion =
      Direct_candidate.complete
        session.finite_candidate_lifecycle manifest.finite_manifest_candidate
        ~completed_fingerprints ~all_verified
    in
    let completion =
      {
        finite_completion_issuer = private_issuer;
        finite_completion_session = session.session;
        finite_completion_manifest = manifest;
        finite_candidate_completion;
      }
    in
    session.finite_result_completions <-
      completion :: session.finite_result_completions;
    session.counters.finite_result_completions <-
      session.counters.finite_result_completions + 1;
    Ok completion

let issue_finite_result session completion =
  let manifest = completion.finite_completion_manifest in
  if not session.active then Error "verification session is destroyed"
  else if completion.finite_completion_issuer != private_issuer then
    Error "finite result completion issuer mismatch"
  else if completion.finite_completion_session != session.session then
    Error "finite result completion belongs to a different session"
  else if not (registered_finite_manifest session manifest) then
    Error "finite result completion manifest is unavailable"
  else
    let* _ =
      Direct_candidate.publish session.finite_candidate_lifecycle
        completion.finite_candidate_completion
    in
    Ok ()

let same_finite_result_call_instance left right =
  left.finite_call_issuer == right.finite_call_issuer
  && left.finite_call_session == right.finite_call_session
  && left.finite_call_caller.function_index
     = right.finite_call_caller.function_index
  && String.equal left.finite_call_caller.function_name
       right.finite_call_caller.function_name
  && String.equal left.finite_call_caller_callable
       right.finite_call_caller_callable
  && String.equal left.finite_call_callee_snapshot
       right.finite_call_callee_snapshot
  && left.finite_call_span = right.finite_call_span
  && String.equal left.finite_call_path_digest right.finite_call_path_digest
  && left.finite_call_result_symbol = right.finite_call_result_symbol

let authenticate_finite_result_call_instance candidate ~session_token
    ~(caller : Sst.function_id)
    ~caller_callable ~callee_snapshot ~call_span ~path_digest ~result_symbol =
  candidate.finite_call_issuer == private_issuer
  && candidate.finite_call_session == session_token
  && candidate.finite_call_caller.function_index = caller.function_index
  && String.equal candidate.finite_call_caller.function_name
       caller.function_name
  && String.equal candidate.finite_call_caller_callable caller_callable
  && String.equal candidate.finite_call_callee_snapshot callee_snapshot
  && candidate.finite_call_span = call_span
  && String.equal candidate.finite_call_path_digest path_digest
  && candidate.finite_call_result_symbol = result_symbol

let authenticate_finite_result_call_site session snapshot
    ~(caller : Sst.function_id)
    ~caller_callable ~call_span =
  let* validated =
    match session.validated with
    | Some validated when session.active -> Ok validated
    | Some _ | None -> Error "verification session is destroyed"
  in
  let* caller_descriptor =
    match Sst_validation.find_callable validated caller with
    | Some descriptor -> Ok descriptor
    | None -> Error "finite result caller is absent from validated authority"
  in
  let caller_definition =
    Sst_validation.callable_definition caller_descriptor
  in
  let* expected_caller_callable =
    canonical_callable_key session caller_definition
  in
  if not (String.equal expected_caller_callable caller_callable) then
    Error "finite result caller callable identity mismatch"
  else
    let exact_edge =
      Sst_validation.call_edge_descriptors validated
      |> List.exists (fun edge ->
             Sst_validation.call_edge_region edge
             = Sst_validation.Body_region
             && Sst_validation.call_edge_form edge = Sst.Exec_call
             &&
             let edge_caller =
               Sst_validation.callable_id
                 (Sst_validation.call_edge_caller edge)
             in
             let edge_callee =
               Sst_validation.callable_id
                 (Sst_validation.call_edge_callee edge)
             in
             edge_caller.function_index = caller.function_index
             && String.equal edge_caller.function_name caller.function_name
             && edge_callee.function_index
                = snapshot.finite_callee.function_index
             && String.equal edge_callee.function_name
                  snapshot.finite_callee.function_name
             && Sst_validation.call_edge_span edge = call_span)
    in
    if exact_edge then Ok ()
    else Error "finite result call site is not authenticated"

let mutate_finite_result_call_instance_for_testing candidate =
  match !finite_result_call_instance_attack_for_testing with
  | None -> candidate
  | Some "caller" ->
      {
        candidate with
        finite_call_caller =
          {
            candidate.finite_call_caller with
            function_name =
              candidate.finite_call_caller.function_name ^ ".substituted";
          };
      }
  | Some "call-site" ->
      {
        candidate with
        finite_call_span =
          {
            candidate.finite_call_span with
            start_pos =
              {
                candidate.finite_call_span.start_pos with
                column = candidate.finite_call_span.start_pos.column + 1;
              };
          };
      }
  | Some "sibling-path" ->
      {
        candidate with
        finite_call_path_digest =
          digest
            (String.concat "\000"
               [ candidate.finite_call_path_digest; "sibling" ]);
      }
  | Some "path-substitution" ->
      {
        candidate with
        finite_call_path_digest =
          digest
            (String.concat "\000"
               [ candidate.finite_call_path_digest; "substituted" ]);
      }
  | Some "result" ->
      {
        candidate with
        finite_call_result_symbol =
          {
            candidate.finite_call_result_symbol with
            source_name =
              candidate.finite_call_result_symbol.source_name
              ^ ".substituted";
          };
      }
  | Some "callee" ->
      {
        candidate with
        finite_call_callee_snapshot =
          candidate.finite_call_callee_snapshot ^ ".substituted";
      }
  | Some attack ->
      invalid_arg ("unknown finite result call-instance attack: " ^ attack)

let record_finite_result_call_observation instance =
  match !finite_result_call_observation_events with
  | None -> ()
  | Some events ->
      let symbol = instance.finite_call_result_symbol in
      let event =
        Printf.sprintf
          "caller=%s callee=%s call=%s result=%s#%d path=%s"
          (function_id_string instance.finite_call_caller)
          instance.finite_call_callee_snapshot
          (span_string instance.finite_call_span)
          symbol.source_name symbol.symbol_id
          instance.finite_call_path_digest
      in
      finite_result_call_observation_events := Some (event :: events)

let consume_finite_result session snapshot ~caller ~caller_callable ~call_span
    ~path_condition ~result =
  let* registry = finite_registry session in
  session.counters.finite_result_consumption_attempts <-
    session.counters.finite_result_consumption_attempts + 1;
  if not (registered_finite_result_snapshot session snapshot) then
    Error "finite result snapshot is not authenticated"
  else
    let* () =
      authenticate_finite_result_call_site session snapshot ~caller
        ~caller_callable ~call_span
    in
    let* result_symbol =
      match result.Vir.aggregate_desc with
      | Vir.Aggregate_symbol
          ({ role = Vir.Result; sort = Vir.Aggregate aggregate_type; _ } as symbol)
        when aggregate_type = result.aggregate_type ->
          Ok symbol
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
      | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _
      | Vir.Aggregate_symbolic_application _ ->
          Error "finite call result is not an exact fresh result symbol"
    in
    let call_path_digest =
      Finite_value_registry.result_path_digest path_condition
    in
    let callee_snapshot = finite_result_snapshot_core_fingerprint snapshot in
    let exact_call_instance =
      {
        finite_call_issuer = private_issuer;
        finite_call_session = session.session;
        finite_call_token = ref ();
        finite_call_caller = caller;
        finite_call_caller_callable = caller_callable;
        finite_call_callee_snapshot = callee_snapshot;
        finite_call_span = call_span;
        finite_call_path_digest = call_path_digest;
        finite_call_result_symbol = result_symbol;
      }
    in
    let candidate =
      mutate_finite_result_call_instance_for_testing exact_call_instance
    in
    if
      not
        (authenticate_finite_result_call_instance candidate
           ~session_token:session.session ~caller ~caller_callable
           ~callee_snapshot ~call_span ~path_digest:call_path_digest
           ~result_symbol)
    then Error "finite result call instance is not authenticated"
    else if
      List.exists
        (same_finite_result_call_instance exact_call_instance)
        session.finite_consumed_call_instances
    then Error "finite call instance was already consumed"
    else
      let result_snapshot =
        Marshal.to_string result [ Marshal.No_sharing ] |> digest
      in
      let* publication =
        Direct_candidate.consume session.finite_candidate_lifecycle
          snapshot.finite_candidate ~caller_snapshot:caller_callable
          ~call_path_digest ~result_snapshot
      in
      match publication with
      | None -> Ok None
      | Some publication ->
          let* receipt =
            Finite_value_registry.issue_published_result registry
              ~candidate_snapshot:callee_snapshot ~publication
              ~call_instance_token:exact_call_instance.finite_call_token
              ~caller_callable ~call_span ~call_path_digest ~result
              ~mode:snapshot.finite_result_mode ~typ:snapshot.finite_result_type
              ~rank:snapshot.finite_rank
          in
          session.finite_consumed_call_instances <-
            exact_call_instance :: session.finite_consumed_call_instances;
          record_finite_result_call_observation exact_call_instance;
          if !finite_result_double_consume_for_testing then
            Error "finite call instance was already consumed"
          else Ok (Some receipt)
let note_callee_solver_attempt session =
  session.counters.callee_solver_attempts <-
    session.counters.callee_solver_attempts + 1
let note_callee_result session = function
  | Solver_backend.Verified ->
      session.counters.callee_verified_results <-
        session.counters.callee_verified_results + 1
  | Solver_backend.Counterexample _ | Solver_backend.Inconclusive _ ->
      session.counters.callee_failed_results <-
        session.counters.callee_failed_results + 1
let note_dependent_lowering session =
  session.counters.dependent_lowerings <-
    session.counters.dependent_lowerings + 1
let note_dependent_backend_context session =
  session.counters.dependent_backend_contexts <-
    session.counters.dependent_backend_contexts + 1
let note_dependent_solver_attempt session =
  session.counters.dependent_solver_attempts <-
    session.counters.dependent_solver_attempts + 1
let note_shared_heap_issuance session =
  session.counters.shared_heap_issuances <-
    session.counters.shared_heap_issuances + 1
let note_shared_heap_write session =
  session.counters.shared_heap_writes <-
    session.counters.shared_heap_writes + 1
let note_shared_heap_read_log session =
  session.counters.shared_heap_read_logs <-
    session.counters.shared_heap_read_logs + 1
let note_shared_heap_epoch_advance session =
  session.counters.shared_heap_epoch_advances <-
    session.counters.shared_heap_epoch_advances + 1
let note_shared_heap_teardown session =
  session.counters.shared_heap_teardowns <-
    session.counters.shared_heap_teardowns + 1
let note_invariant_cell_entry_eligibility session =
  session.counters.invariant_cell_entry_eligibilities <-
    session.counters.invariant_cell_entry_eligibilities + 1
let note_invariant_cell_constructor_eligibility session =
  session.counters.invariant_cell_constructor_eligibilities <-
    session.counters.invariant_cell_constructor_eligibilities + 1

let note_invariant_cell_closed_initialization session =
  session.counters.invariant_cell_closed_initializations <-
    session.counters.invariant_cell_closed_initializations + 1

let note_invariant_cell_open session =
  session.counters.invariant_cell_opens <-
    session.counters.invariant_cell_opens + 1

let note_invariant_cell_update session =
  session.counters.invariant_cell_updates <-
    session.counters.invariant_cell_updates + 1

let note_invariant_cell_close session =
  session.counters.invariant_cell_closes <-
    session.counters.invariant_cell_closes + 1

let note_invariant_cell_effect_instantiation session =
  session.counters.invariant_cell_effect_instantiations <-
    session.counters.invariant_cell_effect_instantiations + 1

let note_invariant_cell_terminal_read session =
  session.counters.invariant_cell_terminal_reads <-
    session.counters.invariant_cell_terminal_reads + 1

let note_invariant_cell_teardown session =
  session.counters.invariant_cell_teardowns <-
    session.counters.invariant_cell_teardowns + 1

let counters session : counters =
  let finite = Finite_value_registry.counters session.finite_registry in
  let lifecycle =
    Direct_candidate.counters session.finite_candidate_lifecycle
  in
  {
    callee_solver_attempts = session.counters.callee_solver_attempts;
    callee_verified_results = session.counters.callee_verified_results;
    callee_failed_results = session.counters.callee_failed_results;
    dependent_lowerings = session.counters.dependent_lowerings;
    dependent_backend_contexts = session.counters.dependent_backend_contexts;
    dependent_solver_attempts = session.counters.dependent_solver_attempts;
    receipts_issued = session.counters.receipts_issued;
    receipts_consumed = session.counters.receipts_consumed;
    finite_witness_issuances = finite.witness_issuances;
    finite_parent_issuances = finite.parent_issuances;
    finite_child_derivations = finite.child_derivations;
    finite_result_promotions = finite.result_promotions;
    (* Retain the source-witness observation for existing
       private gates; path-level promotion has its own counter below. *)
    finite_result_witness_records = session.counters.finite_result_manifests;
    finite_result_path_records = lifecycle.exits_recorded;
    finite_result_manifests = session.counters.finite_result_manifests;
    finite_result_completions = session.counters.finite_result_completions;
    finite_result_finalizations = lifecycle.candidates_published;
    finite_result_consumption_attempts = lifecycle.consumption_attempts;
    finite_result_consumptions = lifecycle.consumptions;
    finite_consumptions = finite.consumptions;
    finite_formal_assumption_issuances = finite.formal_assumption_issuances;
    finite_formal_transfer_batches = finite.formal_transfer_batches;
    finite_formal_transfers = finite.formal_transfers;
    finite_formal_transfer_consumptions =
      finite.formal_transfer_consumptions;
    proof_call_visits = finite.proof_call_visits;
    proof_call_summaries = finite.proof_call_summaries;
    recursive_spec_result_issuances =
      finite.recursive_spec_result_issuances;
    recursive_spec_result_consumptions =
      finite.recursive_spec_result_consumptions;
    local_assertion_instances_issued =
      session.counters.local_assertion_instances_issued;
    local_assertion_instances_consumed =
      session.counters.local_assertion_instances_consumed;
    owned_root_scalar_plans_issued =
      session.counters.owned_root_scalar_plans_issued;
    owned_root_scalar_plans_consumed =
      session.counters.owned_root_scalar_plans_consumed;
    owned_root_scalar_plans_rejected =
      session.counters.owned_root_scalar_plans_rejected;
    owned_root_scalar_observations =
      session.counters.owned_root_scalar_observations;
    owned_root_scalar_equations =
      session.counters.owned_root_scalar_equations;
    owned_root_scalar_bridge_paths =
      session.counters.owned_root_scalar_bridge_paths;
    owned_root_scalar_bridge_equations =
      session.counters.owned_root_scalar_bridge_equations;
    owned_root_scalar_fresh_successors =
      session.counters.owned_root_scalar_fresh_successors;
    owned_root_scalar_reconstructions =
      session.counters.owned_root_scalar_reconstructions;
    transition_predecessor_transfers =
      session.counters.transition_predecessor_transfers;
    transition_predecessor_consumptions =
      session.counters.transition_predecessor_consumptions;
    transition_result_receipts =
      session.counters.transition_result_receipts;
    transition_teardown_removals =
      session.counters.transition_teardown_removals;
    transition_preservation_obligations =
      session.counters.transition_preservation_obligations;
    transition_nested_reconstructions =
      session.counters.transition_nested_reconstructions;
    transition_root_reconstructions =
      session.counters.transition_root_reconstructions;
    shared_heap_issuances = session.counters.shared_heap_issuances;
    shared_heap_writes = session.counters.shared_heap_writes;
    shared_heap_read_logs = session.counters.shared_heap_read_logs;
    shared_heap_epoch_advances =
      session.counters.shared_heap_epoch_advances;
    shared_heap_teardowns = session.counters.shared_heap_teardowns;
    invariant_cell_entry_eligibilities =
      session.counters.invariant_cell_entry_eligibilities;
    invariant_cell_constructor_eligibilities =
      session.counters.invariant_cell_constructor_eligibilities;
    invariant_cell_closed_initializations =
      session.counters.invariant_cell_closed_initializations;
    invariant_cell_opens = session.counters.invariant_cell_opens;
    invariant_cell_updates = session.counters.invariant_cell_updates;
    invariant_cell_closes = session.counters.invariant_cell_closes;
    invariant_cell_effect_instantiations =
      session.counters.invariant_cell_effect_instantiations;
    invariant_cell_terminal_reads =
      session.counters.invariant_cell_terminal_reads;
    invariant_cell_teardowns = session.counters.invariant_cell_teardowns;
    frozen_constructor_template_issuances =
      session.counters.frozen_constructor_template_issuances;
    frozen_constructor_template_teardowns =
      session.counters.frozen_constructor_template_teardowns;
    frozen_conditional_scope_issuances =
      session.counters.frozen_conditional_scope_issuances;
    frozen_conditional_scope_teardowns =
      session.counters.frozen_conditional_scope_teardowns;
    frozen_result_instance_issuances =
      session.counters.frozen_result_instance_issuances;
    frozen_result_instance_teardowns =
      session.counters.frozen_result_instance_teardowns;
    frozen_call_discharge_issuances =
      session.counters.frozen_call_discharge_issuances;
    frozen_call_discharge_consumptions =
      session.counters.frozen_call_discharge_consumptions;
    frozen_call_discharge_teardowns =
      session.counters.frozen_call_discharge_teardowns;
    frozen_descent_witness_issuances =
      session.counters.frozen_descent_witness_issuances;
    frozen_descent_witness_consumptions =
      session.counters.frozen_descent_witness_consumptions;
    frozen_descent_witness_teardowns =
      session.counters.frozen_descent_witness_teardowns;
    frozen_observation_consumptions =
      session.counters.frozen_observation_consumptions;
    frozen_observation_teardowns =
      session.counters.frozen_observation_teardowns;
  }

let render_counters session =
  let counters = counters session in
  let base =
    Printf.sprintf
      "callee-solver-attempts=%d callee-verified-results=%d callee-failed-results=%d dependent-lowerings=%d dependent-backends=%d dependent-solver-attempts=%d receipts-issued=%d receipts-consumed=%d finite-formal-assumptions=%d finite-formal-batches=%d finite-formal-transfers=%d finite-formal-consumptions=%d"
      counters.callee_solver_attempts counters.callee_verified_results
      counters.callee_failed_results counters.dependent_lowerings
      counters.dependent_backend_contexts counters.dependent_solver_attempts
      counters.receipts_issued counters.receipts_consumed
      counters.finite_formal_assumption_issuances
      counters.finite_formal_transfer_batches counters.finite_formal_transfers
      counters.finite_formal_transfer_consumptions
  in
  let with_proof_calls =
    if
      counters.finite_formal_assumption_issuances > 0
      || counters.proof_call_visits > 0 || counters.proof_call_summaries > 0
    then
      Printf.sprintf "%s proof-call-visits=%d proof-call-summaries=%d" base
        counters.proof_call_visits counters.proof_call_summaries
    else base
  in
  let with_local_assertions =
    if
    counters.local_assertion_instances_issued > 0
    || counters.local_assertion_instances_consumed > 0
    then
      Printf.sprintf
        "%s local-assert-issued=%d local-assert-consumed=%d"
        with_proof_calls
        counters.local_assertion_instances_issued
        counters.local_assertion_instances_consumed
    else with_proof_calls
  in
  let with_transition_predecessors =
    if
      counters.transition_predecessor_transfers > 0
      || counters.transition_teardown_removals > 0
    then
      Printf.sprintf
        "%s transition-predecessors=%d/%d transition-results=%d teardown=%d transition-preservations=%d reconstructions=%d/%d"
        with_local_assertions counters.transition_predecessor_transfers
        counters.transition_predecessor_consumptions
        counters.transition_result_receipts
        counters.transition_teardown_removals
        counters.transition_preservation_obligations
        counters.transition_nested_reconstructions
        counters.transition_root_reconstructions
    else with_local_assertions
  in
  let with_owned_scalar =
    if
      counters.owned_root_scalar_plans_issued > 0
      || counters.owned_root_scalar_plans_rejected > 0
    then
      Printf.sprintf
        "%s owned-scalar-plans=%d/%d/%d observations=%d equations=%d bridge-paths=%d bridge-equations=%d fresh=%d reconstructions=%d"
        with_transition_predecessors counters.owned_root_scalar_plans_issued
        counters.owned_root_scalar_plans_consumed
        counters.owned_root_scalar_plans_rejected
        counters.owned_root_scalar_observations
        counters.owned_root_scalar_equations
        counters.owned_root_scalar_bridge_paths
        counters.owned_root_scalar_bridge_equations
        counters.owned_root_scalar_fresh_successors
        counters.owned_root_scalar_reconstructions
    else with_transition_predecessors
  in
  let with_shared_heap =
    if counters.shared_heap_issuances > 0 || counters.shared_heap_teardowns > 0
    then
      Printf.sprintf
        "%s shared-heap=%d/%d/%d/%d/%d"
        with_owned_scalar counters.shared_heap_issuances
        counters.shared_heap_writes counters.shared_heap_read_logs
        counters.shared_heap_epoch_advances counters.shared_heap_teardowns
    else with_owned_scalar
  in
  if
    counters.frozen_constructor_template_issuances > 0
    || counters.frozen_constructor_template_teardowns > 0
    || counters.frozen_conditional_scope_issuances > 0
    || counters.frozen_conditional_scope_teardowns > 0
    || counters.frozen_result_instance_issuances > 0
    || counters.frozen_result_instance_teardowns > 0
    || counters.frozen_call_discharge_issuances > 0
    || counters.frozen_call_discharge_teardowns > 0
  then
    Printf.sprintf
      "%s frozen-template=%d/%d frozen-conditional=%d/%d frozen-instance=%d/%d frozen-discharge=%d/%d/%d frozen-witness=%d/%d/%d frozen-observation=%d/%d"
      with_shared_heap counters.frozen_constructor_template_issuances
      counters.frozen_constructor_template_teardowns
      counters.frozen_conditional_scope_issuances
      counters.frozen_conditional_scope_teardowns
      counters.frozen_result_instance_issuances
      counters.frozen_result_instance_teardowns
      counters.frozen_call_discharge_issuances
      counters.frozen_call_discharge_consumptions
      counters.frozen_call_discharge_teardowns
      counters.frozen_descent_witness_issuances
      counters.frozen_descent_witness_consumptions
      counters.frozen_descent_witness_teardowns
      counters.frozen_observation_consumptions counters.frozen_observation_teardowns
  else with_shared_heap

type blocked_reason =
  | Blocked_frozen_constructor
  | Blocked_frozen_formal
  | Blocked_invariant_cell
  | Blocked_dependent

type trace_event =
  | Blocked of {
      reason : blocked_reason;
      function_id : Sst.function_id;
    }
  | Lowering of {
      function_id : Sst.function_id;
      source : bool;
      dependent : bool;
    }
  | Issued
  | Destroy

exception Private_receipt_trace_disabled

let trace session event =
  try
    [%log.debug "private receipt"
        ~event_kind:
          (Delator.Field.string
             (* Keeping the opt-in gate in the first field makes static
                Delator elision remove the gate check with the event. *)
             (match Sys.getenv_opt "VEROCAML_TEST_PRIVATE_RECEIPT_TRACE" with
             | Some "1" -> (
                 match event with
                 | Blocked { reason; _ } -> (
                     match reason with
                     | Blocked_frozen_constructor ->
                         "blocked-frozen-constructor"
                     | Blocked_frozen_formal -> "blocked-frozen-formal"
                     | Blocked_invariant_cell -> "blocked-invariant-cell"
                     | Blocked_dependent -> "blocked-dependent")
                 | Lowering _ -> "lower"
                 | Issued -> "issued"
                 | Destroy -> "destroy")
             | Some _ | None ->
                 raise_notrace Private_receipt_trace_disabled))
        ~has_blocked_reason:
          (Delator.Field.bool
             (match event with Blocked _ -> true | Lowering _ | Issued | Destroy -> false))
        ~blocked_reason:
          (Delator.Field.string
             (match event with
             | Blocked { reason; _ } -> (
                 match reason with
                 | Blocked_frozen_constructor -> "frozen-constructor"
                 | Blocked_frozen_formal -> "frozen-formal"
                 | Blocked_invariant_cell -> "invariant-cell"
                 | Blocked_dependent -> "dependent")
             | Lowering _ | Issued | Destroy -> ""))
        ~has_function:
          (Delator.Field.bool
             (match event with Blocked _ | Lowering _ -> true | Issued | Destroy -> false))
        ~function_name:
          (Delator.Field.string
             (match event with
             | Blocked { function_id; _ } | Lowering { function_id; _ } ->
                 function_id.Sst.function_name
             | Issued | Destroy -> ""))
        ~function_index:
          (Delator.Field.int
             (match event with
             | Blocked { function_id; _ } | Lowering { function_id; _ } ->
                 function_id.Sst.function_index
             | Issued | Destroy -> -1))
        ~has_schedule:
          (Delator.Field.bool
             (match event with Lowering _ -> true | Blocked _ | Issued | Destroy -> false))
        ~source:
          (Delator.Field.bool
             (match event with
             | Lowering { source; _ } -> source
             | Blocked _ | Issued | Destroy -> false))
        ~dependent:
          (Delator.Field.bool
             (match event with
             | Lowering { dependent; _ } -> dependent
             | Blocked _ | Issued | Destroy -> false))
        ~callee_solver_attempts:
          (Delator.Field.int (counters session).callee_solver_attempts)
        ~callee_verified_results:
          (Delator.Field.int (counters session).callee_verified_results)
        ~callee_failed_results:
          (Delator.Field.int (counters session).callee_failed_results)
        ~dependent_lowerings:(Delator.Field.int (counters session).dependent_lowerings)
        ~dependent_backend_contexts:
          (Delator.Field.int (counters session).dependent_backend_contexts)
        ~dependent_solver_attempts:
          (Delator.Field.int (counters session).dependent_solver_attempts)
        ~receipts_issued:(Delator.Field.int (counters session).receipts_issued)
        ~receipts_consumed:(Delator.Field.int (counters session).receipts_consumed)
        ~finite_witness_issuances:
          (Delator.Field.int (counters session).finite_witness_issuances)
        ~finite_parent_issuances:
          (Delator.Field.int (counters session).finite_parent_issuances)
        ~finite_child_derivations:
          (Delator.Field.int (counters session).finite_child_derivations)
        ~finite_result_promotions:
          (Delator.Field.int (counters session).finite_result_promotions)
        ~finite_result_witness_records:
          (Delator.Field.int (counters session).finite_result_witness_records)
        ~finite_result_path_records:
          (Delator.Field.int (counters session).finite_result_path_records)
        ~finite_result_manifests:
          (Delator.Field.int (counters session).finite_result_manifests)
        ~finite_result_completions:
          (Delator.Field.int (counters session).finite_result_completions)
        ~finite_result_finalizations:
          (Delator.Field.int (counters session).finite_result_finalizations)
        ~finite_result_consumption_attempts:
          (Delator.Field.int (counters session).finite_result_consumption_attempts)
        ~finite_result_consumptions:
          (Delator.Field.int (counters session).finite_result_consumptions)
        ~finite_consumptions:(Delator.Field.int (counters session).finite_consumptions)
        ~finite_formal_assumption_issuances:
          (Delator.Field.int (counters session).finite_formal_assumption_issuances)
        ~finite_formal_transfer_batches:
          (Delator.Field.int (counters session).finite_formal_transfer_batches)
        ~finite_formal_transfers:
          (Delator.Field.int (counters session).finite_formal_transfers)
        ~finite_formal_transfer_consumptions:
          (Delator.Field.int (counters session).finite_formal_transfer_consumptions)
        ~proof_call_visits:(Delator.Field.int (counters session).proof_call_visits)
        ~proof_call_summaries:(Delator.Field.int (counters session).proof_call_summaries)
        ~recursive_spec_result_issuances:
          (Delator.Field.int (counters session).recursive_spec_result_issuances)
        ~recursive_spec_result_consumptions:
          (Delator.Field.int (counters session).recursive_spec_result_consumptions)
        ~local_assertion_instances_issued:
          (Delator.Field.int (counters session).local_assertion_instances_issued)
        ~local_assertion_instances_consumed:
          (Delator.Field.int (counters session).local_assertion_instances_consumed)
        ~owned_root_scalar_plans_issued:
          (Delator.Field.int (counters session).owned_root_scalar_plans_issued)
        ~owned_root_scalar_plans_consumed:
          (Delator.Field.int (counters session).owned_root_scalar_plans_consumed)
        ~owned_root_scalar_plans_rejected:
          (Delator.Field.int (counters session).owned_root_scalar_plans_rejected)
        ~owned_root_scalar_observations:
          (Delator.Field.int (counters session).owned_root_scalar_observations)
        ~owned_root_scalar_equations:
          (Delator.Field.int (counters session).owned_root_scalar_equations)
        ~owned_root_scalar_bridge_paths:
          (Delator.Field.int (counters session).owned_root_scalar_bridge_paths)
        ~owned_root_scalar_bridge_equations:
          (Delator.Field.int (counters session).owned_root_scalar_bridge_equations)
        ~owned_root_scalar_fresh_successors:
          (Delator.Field.int (counters session).owned_root_scalar_fresh_successors)
        ~owned_root_scalar_reconstructions:
          (Delator.Field.int (counters session).owned_root_scalar_reconstructions)
        ~transition_predecessor_transfers:
          (Delator.Field.int (counters session).transition_predecessor_transfers)
        ~transition_predecessor_consumptions:
          (Delator.Field.int (counters session).transition_predecessor_consumptions)
        ~transition_result_receipts:
          (Delator.Field.int (counters session).transition_result_receipts)
        ~transition_teardown_removals:
          (Delator.Field.int (counters session).transition_teardown_removals)
        ~transition_preservation_obligations:
          (Delator.Field.int (counters session).transition_preservation_obligations)
        ~transition_nested_reconstructions:
          (Delator.Field.int (counters session).transition_nested_reconstructions)
        ~transition_root_reconstructions:
          (Delator.Field.int (counters session).transition_root_reconstructions)
        ~shared_heap_issuances:
          (Delator.Field.int (counters session).shared_heap_issuances)
        ~shared_heap_writes:(Delator.Field.int (counters session).shared_heap_writes)
        ~shared_heap_read_logs:
          (Delator.Field.int (counters session).shared_heap_read_logs)
        ~shared_heap_epoch_advances:
          (Delator.Field.int (counters session).shared_heap_epoch_advances)
        ~shared_heap_teardowns:
          (Delator.Field.int (counters session).shared_heap_teardowns)
        ~invariant_cell_entry_eligibilities:
          (Delator.Field.int (counters session).invariant_cell_entry_eligibilities)
        ~invariant_cell_constructor_eligibilities:
          (Delator.Field.int (counters session).invariant_cell_constructor_eligibilities)
        ~invariant_cell_closed_initializations:
          (Delator.Field.int (counters session).invariant_cell_closed_initializations)
        ~invariant_cell_opens:
          (Delator.Field.int (counters session).invariant_cell_opens)
        ~invariant_cell_updates:
          (Delator.Field.int (counters session).invariant_cell_updates)
        ~invariant_cell_closes:
          (Delator.Field.int (counters session).invariant_cell_closes)
        ~invariant_cell_effect_instantiations:
          (Delator.Field.int (counters session).invariant_cell_effect_instantiations)
        ~invariant_cell_terminal_reads:
          (Delator.Field.int (counters session).invariant_cell_terminal_reads)
        ~invariant_cell_teardowns:
          (Delator.Field.int (counters session).invariant_cell_teardowns)
        ~frozen_constructor_template_issuances:
          (Delator.Field.int (counters session).frozen_constructor_template_issuances)
        ~frozen_constructor_template_teardowns:
          (Delator.Field.int (counters session).frozen_constructor_template_teardowns)
        ~frozen_conditional_scope_issuances:
          (Delator.Field.int (counters session).frozen_conditional_scope_issuances)
        ~frozen_conditional_scope_teardowns:
          (Delator.Field.int (counters session).frozen_conditional_scope_teardowns)
        ~frozen_result_instance_issuances:
          (Delator.Field.int (counters session).frozen_result_instance_issuances)
        ~frozen_result_instance_teardowns:
          (Delator.Field.int (counters session).frozen_result_instance_teardowns)
        ~frozen_call_discharge_issuances:
          (Delator.Field.int (counters session).frozen_call_discharge_issuances)
        ~frozen_call_discharge_consumptions:
          (Delator.Field.int (counters session).frozen_call_discharge_consumptions)
        ~frozen_call_discharge_teardowns:
          (Delator.Field.int (counters session).frozen_call_discharge_teardowns)
        ~frozen_descent_witness_issuances:
          (Delator.Field.int (counters session).frozen_descent_witness_issuances)
        ~frozen_descent_witness_consumptions:
          (Delator.Field.int (counters session).frozen_descent_witness_consumptions)
        ~frozen_descent_witness_teardowns:
          (Delator.Field.int (counters session).frozen_descent_witness_teardowns)
        ~frozen_observation_consumptions:
          (Delator.Field.int (counters session).frozen_observation_consumptions)
        ~frozen_observation_teardowns:
          (Delator.Field.int (counters session).frozen_observation_teardowns)]
  with Private_receipt_trace_disabled -> ()

type owned_contents_seed_control_capture = {
  seed_capture_manifest : owned_contents_manifest;
  seed_capture_execution : Vir.function_execution;
  seed_capture_results : Solver_backend.obligation_result list;
}

type owned_contents_seed_control_counts = {
  seed_lineages : int;
  seed_graphs : int;
  seed_mapped : int;
  seed_invalidated : int;
  seed_closed : int;
  seed_candidates : int;
  seed_consumed : int;
  seed_finished : int;
  seed_permits : int;
  seed_permits_consumed : int;
  seed_routes : int;
  seed_equations : int;
  seed_models : int;
  seed_manifests : int;
  seed_completions : int;
  seed_receipts : int;
  seed_successors : int;
  seed_retirements : int;
  seed_permit_retirements : int;
  seed_dependent_lowerings : int;
  seed_dependent_backends : int;
  seed_dependent_solver_attempts : int;
}

let owned_contents_seed_control_counts session =
  let owned = session.owned_contents_counters in
  let generic = session.counters in
  {
    seed_lineages = owned.lineages_opened;
    seed_graphs = owned.lineage_graph_authentications;
    seed_mapped = owned.mapped_candidates_authenticated;
    seed_invalidated = owned.lineages_invalidated;
    seed_closed = owned.lineages_closed;
    seed_candidates = owned.candidates_issued;
    seed_consumed = owned.candidates_consumed;
    seed_finished = owned.candidates_finished;
    seed_permits = owned.permits_issued;
    seed_permits_consumed = owned.permits_consumed;
    seed_routes = owned.recursive_routes;
    seed_equations = owned.ground_equations;
    seed_models = owned.model_results;
    seed_manifests = owned.manifests_issued;
    seed_completions = owned.completions;
    seed_receipts = owned.receipts_finalized;
    seed_successors = owned.successor_receipts;
    seed_retirements = owned.predecessor_retirements;
    seed_permit_retirements = owned.permit_retirements;
    seed_dependent_lowerings = generic.dependent_lowerings;
    seed_dependent_backends = generic.dependent_backend_contexts;
    seed_dependent_solver_attempts = generic.dependent_solver_attempts;
  }

let subtract_owned_contents_seed_control_counts after before =
  {
    seed_lineages = after.seed_lineages - before.seed_lineages;
    seed_graphs = after.seed_graphs - before.seed_graphs;
    seed_mapped = after.seed_mapped - before.seed_mapped;
    seed_invalidated = after.seed_invalidated - before.seed_invalidated;
    seed_closed = after.seed_closed - before.seed_closed;
    seed_candidates = after.seed_candidates - before.seed_candidates;
    seed_consumed = after.seed_consumed - before.seed_consumed;
    seed_finished = after.seed_finished - before.seed_finished;
    seed_permits = after.seed_permits - before.seed_permits;
    seed_permits_consumed =
      after.seed_permits_consumed - before.seed_permits_consumed;
    seed_routes = after.seed_routes - before.seed_routes;
    seed_equations = after.seed_equations - before.seed_equations;
    seed_models = after.seed_models - before.seed_models;
    seed_manifests = after.seed_manifests - before.seed_manifests;
    seed_completions = after.seed_completions - before.seed_completions;
    seed_receipts = after.seed_receipts - before.seed_receipts;
    seed_successors = after.seed_successors - before.seed_successors;
    seed_retirements = after.seed_retirements - before.seed_retirements;
    seed_permit_retirements =
      after.seed_permit_retirements - before.seed_permit_retirements;
    seed_dependent_lowerings =
      after.seed_dependent_lowerings - before.seed_dependent_lowerings;
    seed_dependent_backends =
      after.seed_dependent_backends - before.seed_dependent_backends;
    seed_dependent_solver_attempts =
      after.seed_dependent_solver_attempts
      - before.seed_dependent_solver_attempts;
  }

let owned_contents_seed_template_permits session candidate =
  List.filter
    (fun permit -> permit.contents_permit_candidate == candidate)
    session.owned_contents_permits

let owned_contents_seed_control_template session captures =
  List.find_map
    (fun capture ->
      if capture.seed_capture_results = [] then None
      else
        List.find_map
          (fun successor ->
            match
              successor.contents_candidate_origin
                .contents_origin_predecessor
            with
            | None -> None
            | Some predecessor_origin ->
                let predecessor =
                  List.find_opt
                    (fun candidate ->
                      candidate.contents_candidate_origin
                      == predecessor_origin
                      && candidate.contents_candidate_origin
                           .contents_origin_predecessor
                         = None
                      && candidate.contents_candidate_root_version = 0
                      && candidate.contents_candidate_path = []
                      && candidate.contents_candidate_origin
                           .contents_origin_path
                         = []
                      && candidate.contents_candidate_caller
                         == successor.contents_candidate_caller
                      && candidate.contents_candidate_grammar
                         == successor.contents_candidate_grammar)
                    session.owned_contents_candidates
                in
                (match predecessor with
                | None -> None
                | Some predecessor ->
                    let successor_permits =
                      owned_contents_seed_template_permits session
                        successor
                    in
                    if
                      successor.contents_candidate_path <> []
                      || successor.contents_candidate_origin
                           .contents_origin_path
                         <> []
                      || successor.contents_candidate_root_version <> 1
                      || successor.contents_candidate_topology
                           .contents_topology_edges
                         = []
                      || List.length successor_permits
                         <> List.length
                              successor.contents_candidate_topology
                                .contents_topology_edges
                    then None
                    else
                      Some
                        ( predecessor,
                          successor,
                          capture.seed_capture_execution,
                          capture.seed_capture_results )))
          capture.seed_capture_manifest.contents_manifest_candidates)
    captures

let issue_and_finish_owned_contents_seed_control_candidate session template
    origin =
  let* candidate =
    issue_owned_contents_candidate session
      ~validated:template.contents_candidate_validated
      ~grammar:template.contents_candidate_grammar
      ~caller:template.contents_candidate_caller
      ~call:template.contents_candidate_call
      ~actual:template.contents_candidate_actual
      ~root_binding:template.contents_candidate_root_binding
      ~root:template.contents_candidate_root
      ~root_version:template.contents_candidate_root_version
      ~path_condition:[] ~topology:template.contents_candidate_topology
      ~origin
  in
  let* () = consume_owned_contents_candidate session candidate in
  let template_permits =
    owned_contents_seed_template_permits session template
  in
  let rec issue_permits = function
    | [] -> Ok ()
    | template_permit :: permits ->
        let* permit =
          issue_owned_contents_permit session candidate
            ~parent_path:template_permit.contents_permit_parent_path
            ~field:template_permit.contents_permit_field
            ~parent:template_permit.contents_permit_parent
            ~child:template_permit.contents_permit_child
            ~call:template_permit.contents_permit_call
            ~call_span:template_permit.contents_permit_call_span
            ~path_condition:[]
        in
        let* () =
          consume_owned_contents_permit session permit ~candidate
            ~parent_path:template_permit.contents_permit_parent_path
            ~field:template_permit.contents_permit_field
            ~parent:template_permit.contents_permit_parent
            ~child:template_permit.contents_permit_child
            ~call:template_permit.contents_permit_call
            ~call_span:template_permit.contents_permit_call_span
            ~path_condition:[]
        in
        issue_permits permits
  in
  let* () = issue_permits template_permits in
  let rec note_nodes = function
    | 0 -> Ok ()
    | count ->
        let* () =
          note_owned_contents_recursive_route session candidate
        in
        let* () =
          note_owned_contents_ground_equation session candidate
        in
        note_nodes (count - 1)
  in
  let* () =
    note_nodes
      (List.length
         candidate.contents_candidate_topology.contents_topology_nodes)
  in
  let* () = note_owned_contents_model_result session candidate in
  let* () = finish_owned_contents_candidate session candidate in
  Ok candidate

let authorize_owned_contents_seed_control session candidate execution =
  let* manifest =
    authorize_owned_contents_obligations session
      candidate.contents_candidate_caller execution
  in
  match manifest with
  | Some manifest -> (
      match manifest.contents_manifest_candidates with
      | [ exact ] when exact == candidate -> Ok manifest
      | [] | _ :: _ ->
          Error
            "seed control manifest did not isolate its exact candidate")
  | None -> Error "seed control candidate issued no manifest"

let valid_owned_contents_seed_control_path counts ~nodes ~permits
    ~failed ~successor =
  counts.seed_lineages = 1
  && counts.seed_graphs = 1
  && counts.seed_mapped = nodes
  && counts.seed_invalidated = (if failed then 1 else 0)
  && counts.seed_closed = (if failed then 0 else 1)
  && counts.seed_candidates = 1
  && counts.seed_consumed = 1
  && counts.seed_finished = 1
  && counts.seed_permits = permits
  && counts.seed_permits_consumed = permits
  && counts.seed_routes = nodes
  && counts.seed_equations = nodes
  && counts.seed_models = 1
  && counts.seed_manifests = 1
  && counts.seed_completions = (if failed then 0 else 1)
  && counts.seed_receipts = (if failed then 0 else 1)
  && counts.seed_successors =
     (if failed || not successor then 0 else 1)
  && counts.seed_retirements =
     (if failed || not successor then 0 else 1)
  && counts.seed_permit_retirements = permits
  && counts.seed_dependent_lowerings = 0
  && counts.seed_dependent_backends = 0
  && counts.seed_dependent_solver_attempts = 0

type owned_contents_seed_control_phase = {
  seed_phase_candidate : owned_contents_candidate;
  seed_phase_manifest : owned_contents_manifest;
  seed_phase_completed : bool;
  seed_phase_counts : owned_contents_seed_control_counts;
}

let owned_contents_seed_control_get stage = function
  | Ok value -> value
  | Error message -> failwith (stage ^ ": " ^ message)

let owned_contents_seed_control_require condition message =
  if not condition then failwith message

let run_owned_contents_seed_control_phase session template origin execution
    results =
  let before = owned_contents_seed_control_counts session in
  let candidate =
    issue_and_finish_owned_contents_seed_control_candidate session template
      origin
    |> owned_contents_seed_control_get "seed candidate"
  in
  let manifest =
    authorize_owned_contents_seed_control session candidate execution
    |> owned_contents_seed_control_get "seed manifest"
  in
  let completed =
    complete_owned_contents session (Some manifest) execution results
    |> owned_contents_seed_control_get "seed completion"
  in
  {
    seed_phase_candidate = candidate;
    seed_phase_manifest = manifest;
    seed_phase_completed = completed;
    seed_phase_counts =
      subtract_owned_contents_seed_control_counts
        (owned_contents_seed_control_counts session)
        before;
  }

let owned_contents_seed_successor_origin session template seed_origin =
  issue_successor_owned_contents_origin session
    ~validated:template.contents_candidate_validated
    ~caller:template.contents_candidate_caller
    ~expression:template.contents_candidate_origin.contents_origin_expression
    ~predecessor:seed_origin
    ~predecessor_root:
      (Option.get
         template.contents_candidate_origin.contents_origin_predecessor_root)
    ~successor_root:template.contents_candidate_root ~path_condition:[]

let owned_contents_seed_lifecycle_control session captures =
  let seed_template, successor_template, execution, verified_results =
    match owned_contents_seed_control_template session captures with
    | Some template -> template
    | None -> failwith "seed control lacks one production template"
  in
  let seed_origin =
    issue_closed_owned_contents_origin session
      ~validated:seed_template.contents_candidate_validated
      ~caller:seed_template.contents_candidate_caller
      ~expression:
        seed_template.contents_candidate_origin.contents_origin_expression
      ~root:seed_template.contents_candidate_root ~path_condition:[]
    |> owned_contents_seed_control_get "seed origin"
  in
  let seed_phase =
    run_owned_contents_seed_control_phase session seed_template seed_origin
      execution verified_results
  in
  let seed_receipt =
    match
      List.find_opt
        (fun receipt ->
          receipt.contents_receipt_origin == seed_origin
          && receipt.contents_receipt_path = [])
        session.owned_contents_receipts
    with
    | Some receipt -> receipt
    | None -> failwith "seed control issued no exact receipt"
  in
  let seed_version = seed_receipt.contents_receipt_root_version in
  let seed_nodes =
    List.length
      seed_template.contents_candidate_topology.contents_topology_nodes
  and seed_permits =
    List.length
      seed_template.contents_candidate_topology.contents_topology_edges
  in
  owned_contents_seed_control_require
    (seed_phase.seed_phase_completed
    && seed_version = 0
    && authenticate_owned_contents_receipt session seed_receipt
    && valid_owned_contents_seed_control_path
         seed_phase.seed_phase_counts ~nodes:seed_nodes
         ~permits:seed_permits ~failed:false ~successor:false)
    "seed control did not finalize one exact production seed";

  let receipts_before_failure =
    List.length session.owned_contents_receipts
  in
  let failed_origin =
    owned_contents_seed_successor_origin session successor_template
      seed_origin
    |> owned_contents_seed_control_get "failed successor origin"
  in
  let failed_results =
    match verified_results with
    | [] -> failwith "seed control production template has no obligation"
    | result :: results ->
        { result with outcome = Solver_backend.Counterexample [] }
        :: results
  in
  let failure_phase =
    run_owned_contents_seed_control_phase session successor_template
      failed_origin execution failed_results
  in
  let successor_nodes =
    List.length
      successor_template.contents_candidate_topology.contents_topology_nodes
  and successor_permits =
    List.length
      successor_template.contents_candidate_topology.contents_topology_edges
  in
  let failure_candidate = failure_phase.seed_phase_candidate
  and failure_manifest = failure_phase.seed_phase_manifest in
  owned_contents_seed_control_require
    ((not failure_phase.seed_phase_completed)
    && valid_owned_contents_seed_control_path
         failure_phase.seed_phase_counts ~nodes:successor_nodes
         ~permits:successor_permits ~failed:true ~successor:true
    && authenticate_owned_contents_receipt session seed_receipt
    && seed_receipt.contents_receipt_root_version = seed_version
    && List.length session.owned_contents_receipts
       = receipts_before_failure
    && not
         (List.exists
            (fun receipt -> receipt.contents_receipt_origin == failed_origin)
            session.owned_contents_receipts)
    && not failure_candidate.contents_candidate_finalized
    && failure_candidate.contents_candidate_retired
    && failure_candidate.contents_candidate_lineage
         .contents_lineage_invalidated
    && not
         (List.exists
            (fun manifest ->
              manifest.contents_manifest_token
              == failure_manifest.contents_manifest_token)
            session.owned_contents_manifests))
    "failed seeded successor retained authority or missed production lifecycle";

  let receipts_before_success =
    List.map
      (fun receipt -> (receipt, receipt.contents_receipt_retired))
      session.owned_contents_receipts
  and retired_origins_before_success =
    session.retired_owned_contents_origins
  in
  let successful_origin =
    owned_contents_seed_successor_origin session successor_template
      seed_origin
    |> owned_contents_seed_control_get "successful successor origin"
  in
  let success_phase =
    run_owned_contents_seed_control_phase session successor_template
      successful_origin execution verified_results
  in
  let retired_receipts =
    List.filter_map
      (fun (receipt, was_retired) ->
        if not was_retired && receipt.contents_receipt_retired then
          Some receipt
        else None)
      receipts_before_success
  and retired_origins =
    List.filter
      (fun origin ->
        not
          (List.exists (( == ) origin)
             retired_origins_before_success))
      session.retired_owned_contents_origins
  and successful_receipt =
    List.find_opt
      (fun receipt ->
        receipt.contents_receipt_origin == successful_origin)
      session.owned_contents_receipts
  in
  owned_contents_seed_control_require
    (success_phase.seed_phase_completed
    && valid_owned_contents_seed_control_path
         success_phase.seed_phase_counts ~nodes:successor_nodes
         ~permits:successor_permits ~failed:false ~successor:true
    && success_phase.seed_phase_candidate.contents_candidate_finalized
    && seed_receipt.contents_receipt_retired
    && not (authenticate_owned_contents_receipt session seed_receipt)
    && (match retired_receipts with
       | [ exact ] -> exact == seed_receipt
       | [] | _ :: _ :: _ -> false)
    && (match retired_origins with
       | [ exact ] -> exact == seed_origin
       | [] | _ :: _ :: _ -> false)
    &&
    match successful_receipt with
    | Some receipt -> authenticate_owned_contents_receipt session receipt
    | None -> false)
    "successful successor did not retire only its exact seed";

  let retirement_count =
    session.owned_contents_counters.predecessor_retirements
  in
  let second_retirement =
    retire_owned_contents_receipt session seed_receipt
  and replay =
    owned_contents_seed_successor_origin session successor_template
      seed_origin
  in
  owned_contents_seed_control_require
    ((not second_retirement)
    && session.owned_contents_counters.predecessor_retirements
       = retirement_count
    && match replay with Ok _ -> false | Error _ -> true)
    "successful seed retirement was replayable";

  let seed_counts = seed_phase.seed_phase_counts
  and failure_counts = failure_phase.seed_phase_counts
  and success_counts = success_phase.seed_phase_counts in
  Printf.sprintf
    "seed-control=production seed-path=%d/%d/%d/%d failure-path=%d/%d/%d/%d failure-authority=%d/%d/%d/%d failure-dependent=%d/%d/%d seed-survived=live@%d success-path=%d/%d/%d/%d success-authority=%d/%d/%d/%d seed-retirement=exactly-once second-retirement=rejected replay=rejected"
    seed_counts.seed_lineages seed_counts.seed_candidates
    seed_counts.seed_permits seed_counts.seed_manifests
    failure_counts.seed_lineages failure_counts.seed_candidates
    failure_counts.seed_permits failure_counts.seed_manifests
    failure_counts.seed_receipts failure_counts.seed_successors
    failure_counts.seed_retirements failure_counts.seed_completions
    failure_counts.seed_dependent_lowerings
    failure_counts.seed_dependent_backends
    failure_counts.seed_dependent_solver_attempts seed_version
    success_counts.seed_lineages success_counts.seed_candidates
    success_counts.seed_permits success_counts.seed_manifests
    success_counts.seed_receipts success_counts.seed_successors
    success_counts.seed_retirements success_counts.seed_completions

module For_testing = struct
  let set_owned_root_scalar_plan_attack_for_testing attack =
    owned_root_scalar_plan_attack_for_testing := attack

  let set_owned_contents_attack_for_testing attack =
    owned_contents_attack_for_testing := attack

  let observe_owned_contents_counters callback =
    if Option.is_some !owned_contents_counter_observer then
      invalid_arg "owned-contents counter observer is already active";
    let observations = ref [] in
    owned_contents_counter_observer :=
      Some (fun counters -> observations := counters :: !observations);
    Fun.protect
      ~finally:(fun () -> owned_contents_counter_observer := None)
      (fun () ->
        let result = callback () in
        (result, List.rev !observations))

  let observe_owned_contents_seed_control callback =
    if
      Option.is_some !owned_contents_seed_observer
      || Option.is_some !owned_contents_seed_completion_observer
    then
      invalid_arg "owned-contents seed observer is already active";
    let observations = ref [] in
    let captures = ref [] in
    owned_contents_seed_completion_observer :=
      Some
        (fun manifest execution results ->
          captures :=
            {
              seed_capture_manifest = manifest;
              seed_capture_execution = execution;
              seed_capture_results = results;
            }
            :: !captures);
    owned_contents_seed_observer :=
      Some (fun session ->
        observations :=
          owned_contents_seed_lifecycle_control session
            (List.rev !captures)
          :: !observations);
    Fun.protect
      ~finally:(fun () ->
        owned_contents_seed_observer := None;
        owned_contents_seed_completion_observer := None)
      (fun () ->
        let result = callback () in
        (result, List.rev !observations))

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

  let observe_owned_root_scalar_counters callback =
    if Option.is_some !owned_root_scalar_counter_observer then
      invalid_arg "owned-root scalar counter observer is already active";
    let observations = ref [] in
    owned_root_scalar_counter_observer :=
      Some
        (fun counters ->
          observations :=
            {
              plans_issued = counters.owned_root_scalar_plans_issued;
              plans_consumed = counters.owned_root_scalar_plans_consumed;
              plans_rejected = counters.owned_root_scalar_plans_rejected;
              observations = counters.owned_root_scalar_observations;
              equations = counters.owned_root_scalar_equations;
              bridge_paths = counters.owned_root_scalar_bridge_paths;
              bridge_equations =
                counters.owned_root_scalar_bridge_equations;
              dependent_lowerings = counters.dependent_lowerings;
              dependent_backend_contexts =
                counters.dependent_backend_contexts;
              dependent_solver_attempts =
                counters.dependent_solver_attempts;
            }
            :: !observations);
    Fun.protect
      ~finally:(fun () -> owned_root_scalar_counter_observer := None)
      (fun () ->
        let result = callback () in
        (result, List.rev !observations))

  let observe_transition_predecessor_counters callback =
    if Option.is_some !transition_predecessor_counter_observer then
      invalid_arg "transition predecessor counter observer is already active";
    let observations = ref [] in
    transition_predecessor_counter_observer :=
      Some
        (fun counters ->
          observations :=
            {
              transfers = counters.transition_predecessor_transfers;
              consumptions = counters.transition_predecessor_consumptions;
              result_receipts = counters.transition_result_receipts;
              teardown_removals = counters.transition_teardown_removals;
              preservation_obligations =
                counters.transition_preservation_obligations;
              nested_reconstructions =
                counters.transition_nested_reconstructions;
              root_reconstructions =
                counters.transition_root_reconstructions;
              dependent_lowerings = counters.dependent_lowerings;
              dependent_backend_contexts =
                counters.dependent_backend_contexts;
              dependent_solver_attempts =
                counters.dependent_solver_attempts;
            }
            :: !observations);
    Fun.protect
      ~finally:(fun () -> transition_predecessor_counter_observer := None)
      (fun () ->
        let result = callback () in
        (result, List.rev !observations))

  let set_transition_predecessor_attack_for_testing attack =
    transition_predecessor_attack_for_testing := attack

  let set_transition_predecessor_double_consume_for_testing enabled =
    transition_predecessor_double_consume_for_testing := enabled

  let reset_transition_predecessor_replay_for_testing () =
    transition_predecessor_replay_for_testing := None

  let observe_finite_result_lifecycle_counters callback =
    if Option.is_some !finite_result_lifecycle_counter_observer then
      invalid_arg "finite result lifecycle counter observer is already active";
    let observations = ref [] in
    finite_result_lifecycle_counter_observer :=
      Some
        (fun counters ->
          observations :=
            {
              manifests = counters.finite_result_manifests;
              completions = counters.finite_result_completions;
              consumption_attempts =
                counters.finite_result_consumption_attempts;
            }
            :: !observations);
    Fun.protect
      ~finally:(fun () -> finite_result_lifecycle_counter_observer := None)
      (fun () ->
        let result = callback () in
        (result, List.rev !observations))

  let reset_local_assertion_instance_observation () =
    observed_local_assertion_instances_issued := 0;
    observed_local_assertion_instances_consumed := 0;
    observed_direct_exec_local_assertion_scopes_issued := 0;
    observed_direct_exec_local_assertion_scopes_closed := 0;
    observed_proof_activation_routes_consumed := 0

  let local_assertion_instance_observation () =
    ( !observed_local_assertion_instances_issued,
      !observed_local_assertion_instances_consumed )

  let direct_exec_local_assertion_scope_observation () =
    ( !observed_direct_exec_local_assertion_scopes_issued,
      !observed_direct_exec_local_assertion_scopes_closed,
      !observed_direct_exec_local_assertion_scopes_issued
      - !observed_direct_exec_local_assertion_scopes_closed )

  let proof_activation_route_consumption_count () =
    !observed_proof_activation_routes_consumed

  let set_local_assertion_instance_attack_for_testing attack =
    local_assertion_instance_attack_for_testing := attack

  let set_proof_activation_batch_attack_for_testing attack =
    proof_activation_batch_attack_for_testing := attack

  let set_ground_constructor_match_attack_for_testing attack =
    ground_constructor_match_attack_for_testing := attack

  let suppress_recursive_spec_preservation suppress =
    suppress_recursive_spec_preservation_for_testing := suppress

  let suppress_recursive_spec_result_issuance suppress =
    suppress_recursive_spec_result_issuance_for_testing := suppress

  let suppress_recursive_spec_construction_route suppress =
    suppress_recursive_spec_construction_route_for_testing := suppress

  let observe_finite_result_calls callback =
    if !finite_result_call_observation_events <> None then
      invalid_arg "finite result call observer is already active";
    finite_result_call_observation_events := Some [];
    Fun.protect
      ~finally:(fun () -> finite_result_call_observation_events := None)
      (fun () ->
        let result = callback () in
        let events =
          Option.value ~default:[] !finite_result_call_observation_events
          |> List.rev
        in
        (result, events))

  let observe_proof_activations callback =
    if !proof_activation_observation_events <> None then
      invalid_arg "proof activation observer is already active";
    proof_activation_observation_events := Some [];
    Fun.protect
      ~finally:(fun () -> proof_activation_observation_events := None)
      (fun () ->
        let result = callback () in
        let events =
          Option.value ~default:[] !proof_activation_observation_events
          |> List.rev
        in
        (result, events))

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

  let test_span =
    {
      Diagnostic.file = "private_receipt_test.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

  let finite_result_call_instance_matrix () =
    let session_token = ref () in
    let caller = { Sst.function_index = 7; function_name = "Caller.run" } in
    let caller_callable = "authenticated-caller" in
    let callee_snapshot = "authenticated-callee" in
    let result_symbol =
      {
        Vir.symbol_id = 19;
        source_name = "callee.result";
        sort =
          Vir.Aggregate
            {
              aggregate_type_index = 4;
              aggregate_type_name = "node";
      aggregate_type_arguments = [];
            };
        role = Vir.Result;
        span = test_span;
      }
    in
    let exact path_digest =
      {
        finite_call_issuer = private_issuer;
        finite_call_session = session_token;
        finite_call_token = ref ();
        finite_call_caller = caller;
        finite_call_caller_callable = caller_callable;
        finite_call_callee_snapshot = callee_snapshot;
        finite_call_span = test_span;
        finite_call_path_digest = path_digest;
        finite_call_result_symbol = result_symbol;
      }
    in
    let authenticated candidate ~path_digest ~result_symbol ~caller
        ~call_span ~callee_snapshot =
      authenticate_finite_result_call_instance candidate ~session_token
        ~caller ~caller_callable ~callee_snapshot ~call_span ~path_digest
        ~result_symbol
    in
    let first = exact "path-a" in
    let spent = [ first ] in
    let second = exact "path-b" in
    let distinct_path =
      authenticated second ~path_digest:"path-b" ~result_symbol ~caller
        ~call_span:test_span ~callee_snapshot
      && not (List.exists (same_finite_result_call_instance second) spent)
    in
    let replay =
      List.exists (same_finite_result_call_instance first) spent
    in
    let sibling_copy =
      { first with finite_call_path_digest = "path-b"; finite_call_token = ref () }
    in
    let wrong_caller =
      {
        first with
        finite_call_caller =
          { caller with function_name = "Caller.sibling" };
        finite_call_token = ref ();
      }
    in
    let wrong_call =
      {
        first with
        finite_call_span =
          {
            test_span with
            start_pos = { test_span.start_pos with column = 1 };
          };
        finite_call_token = ref ();
      }
    in
    let wrong_result_symbol =
      { result_symbol with source_name = "other.result" }
    in
    let wrong_result =
      {
        first with
        finite_call_result_symbol = wrong_result_symbol;
        finite_call_token = ref ();
      }
    in
    let rejects candidate =
      not
        (authenticated candidate ~path_digest:"path-a" ~result_symbol ~caller
           ~call_span:test_span ~callee_snapshot)
    in
    let downstream = ref 0 in
    List.iter
      (fun rejected -> if not rejected then incr downstream)
      [
        replay;
        rejects sibling_copy;
        rejects wrong_caller;
        rejects wrong_call;
        rejects wrong_result;
      ];
    [
      Printf.sprintf
        "finite-result-call-instance=distinct-path copied-symbol=%b"
        distinct_path;
      Printf.sprintf
        "finite-result-call-instance=same-path-replay result=%s"
        (if replay then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=same-path-double-consume result=%s"
        (if replay then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=sibling-path-substitution result=%s"
        (if rejects sibling_copy then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=wrong-caller result=%s"
        (if rejects wrong_caller then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=wrong-call-site result=%s"
        (if rejects wrong_call then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=wrong-result result=%s"
        (if rejects wrong_result then "rejected" else "accepted");
      Printf.sprintf
        "finite-result-call-instance=rejected-downstream-work value=%d"
        !downstream;
    ]

  let test_identity =
    {
      unit_identity = "Private_receipt_test:interface";
      cmt_identity = "private_receipt_test.cmt:source:program";
      family_identity = "retained-v1";
      interface_digest = "interface";
      source_digest = "source";
      program_snapshot = "program";
      signature_snapshot = "signature";
    }

  let test_invariant =
    {
      invariant_id = "invariant:Box.t";
      abstract_type = { Sst.type_index = 0; type_name = "Box.t" };
      certificate_id = "certificate";
      model = { Sst.function_index = 1; function_name = "Box.model" };
      model_type = Sst.Int;
      predicate = { Sst.function_index = 2; function_name = "Box.invariant" };
      predicate_digest = "predicate";
    }

  let test_callee =
    { Sst.function_index = 3; function_name = "Box.make" }

  let test_resolved_path = "dot(ident(Private_receipt_test$Box),make)"
  let test_binding_uid = "Private_receipt_test.3"

  let test_snapshot =
    {
      program = test_identity;
      resolved_path = test_resolved_path;
      binding_uid = test_binding_uid;
      callable_key =
        String.concat "\000"
          [
            test_identity.unit_identity;
            String.concat "\000" [ test_resolved_path; test_binding_uid ];
            test_identity.signature_snapshot;
            string_of_int test_callee.function_index;
          ];
      callee = test_callee;
      body_snapshot = "body";
      body_provenance =
        Sst.Authenticated_typedtree
          { source_file = test_span.file; declaration_span = test_span };
      mode = Sst.Exec;
      result_mode = Sst.Exec_instance;
      result_type = Sst.Aggregate test_invariant.abstract_type;
      invariant = test_invariant;
    }

  let test_session () =
    let session = ref () in
    let finite_registry =
      Finite_value_registry.create (finite_program_identity test_identity)
        ~session
        ~types:[]
    in
    {
      issuer = private_issuer;
      session;
      imported_registration = None;
      identity = test_identity;
      active = true;
      validated = None;
      invariants = None;
      callable_identities = None;
      authorized_callees = [ test_snapshot ];
      obligation_manifests = [];
      verified_completions = [];
      receipts = [];
      consumed_result_symbols = [];
      consumed_result_routes = [];
      finite_result_snapshots = [];
      finite_result_manifests = [];
      finite_result_completions = [];
      finite_consumed_call_instances = [];
      proof_activation_authorities = [];
      proof_activation_manifests = [];
      consumed_proof_activation_batches = [];
      consumed_proof_activation_manifests = [];
      finalized_proof_activation_snapshots = [];
      issued_ground_constructor_matches = [];
      active_direct_exec_local_assertion_scope = None;
      issued_local_assertion_instances = [];
      consumed_local_assertion_instances = [];
      issued_owned_root_scalar_plans = [];
      consumed_owned_root_scalar_plans = [];
      owned_contents_lineages = [];
      owned_contents_candidates = [];
      owned_contents_permits = [];
      owned_contents_manifests = [];
      owned_contents_receipts = [];
      issued_owned_contents_origins = [];
      retired_owned_contents_origins = [];
      owned_contents_counters = empty_owned_contents_counters ();
      previewed_transition_predecessors = [];
      previewed_transition_predecessor_seals = [];
      active_transition_predecessors = [];
      consumed_transition_predecessors = [];
      receipted_transition_predecessors = [];
      recursive_spec_preservation = [];
      frozen_constructor_manifests = [];
      frozen_constructor_templates = [];
      frozen_conditional_scopes = [];
      frozen_constructor_result_instances = [];
      frozen_call_discharges = [];
      shared_heap_teardowns = [];
      finite_candidate_lifecycle = Direct_candidate.create ~session;
      finite_registry;
      counters = empty_counters ();
    }

  let proof_test_callable =
    { Sst.function_index = 11; function_name = "proof_activation_test" }

  let proof_test_function_ref =
    {
      Vir.function_index = proof_test_callable.function_index;
      function_name = proof_test_callable.function_name;
    }

  let proof_test_provenance =
    Sst.Authenticated_typedtree
      { source_file = test_span.file; declaration_span = test_span }

  let proof_test_authority (session : t) =
    let authority =
      {
        proof_authority_issuer = private_issuer;
        proof_authority_session = session.session;
        proof_authority_program = session.identity;
        proof_authority_resolved_path = "proof_activation_test";
        proof_authority_binding_uid = "Proof_activation_test.11";
        proof_authority_callable_key = "proof-activation-test-key";
        proof_authority_callable = proof_test_callable;
        proof_authority_body_snapshot = "proof-body";
        proof_authority_body_provenance = proof_test_provenance;
        proof_authority_scope = Full_proof_execution;
      }
    in
    session.proof_activation_authorities <- [ authority ];
    authority

  let proof_test_obligation index path goal =
    {
      Vir.obligation_index = index;
      function_ref = proof_test_function_ref;
      kind = Vir.Assertion { assertion_ordinal = 0 };
      span = test_span;
      assumptions = [ Vir.Boolean_constant true ];
      required_preceding_safety = [];
      path_condition = [ Vir.Boolean_constant path ];
      goal = Vir.Boolean_constant goal;
      projection_symbols = [];
    }

  let proof_test_execution obligations =
    {
      Vir.function_ref = proof_test_function_ref;
      mode = Sst.Proof;
      body_provenance = proof_test_provenance;
      policy = Sst.Default_linear_z3;
      trusted_summary_uses = [];
      reached_callback_calls = [];
      owned_tree_transitions = [];
      shared_scalar_heap_reads = [];
      shared_scalar_heap_writes = [];
      obligations;
      exits = [];
    }

  let proof_test_fixture () =
    let session = test_session () in
    let authority = proof_test_authority session in
    let activation =
      {
        Spec_unfolding.function_id =
          { Sst.function_index = 1; function_name = "recursive_spec" };
        depth = 2;
        span = test_span;
      }
    in
    let obligations =
      [
        proof_test_obligation 0 false true;
        proof_test_obligation 1 true false;
      ]
    in
    let manifests =
      List.map
        (fun (obligation : Vir.obligation) ->
          let snapshot =
            snapshot_proof_activation authority ~activations:[ activation ]
              { obligation with Vir.obligation_index = 0 }
          in
          match finalize_proof_activation session snapshot obligation with
          | Ok manifest -> manifest
          | Error message -> failwith message)
        obligations
    in
    (session, manifests, proof_test_execution obligations)

  let proof_test_members session (execution : Vir.function_execution) =
    let authority =
      match session.proof_activation_authorities with
      | authority :: _ -> authority
      | [] -> failwith "proof test authority is absent"
    in
    List.map
      (fun obligation -> (authority, obligation))
      execution.obligations

  let proof_activation_adversarial_matrix ~on_valid =
    let rejected name mutate =
      let session, manifests, execution = proof_test_fixture () in
      let session, manifests, execution = mutate session manifests execution in
      let batch = proof_activation_batch session (proof_test_members session execution) manifests in
      match consume_proof_activation_batch session batch execution with
      | Error _ ->
          Printf.sprintf "%s rejected dispatch=0 fallback=0" name
      | Ok _ -> failwith (name ^ " proof activation attack was accepted")
    in
    let control =
      let session, manifests, execution = proof_test_fixture () in
      let batch = proof_activation_batch session (proof_test_members session execution) manifests in
      match consume_proof_activation_batch session batch execution with
      | Error message -> failwith ("valid proof activation control: " ^ message)
      | Ok _ ->
          on_valid ();
          "valid accepted dispatch=1 fallback=0"
    in
    let replay =
      let session, manifests, execution = proof_test_fixture () in
      let first = proof_activation_batch session (proof_test_members session execution) manifests in
      (match consume_proof_activation_batch session first execution with
      | Error message -> failwith ("replay setup: " ^ message)
      | Ok _ -> ());
      let second = proof_activation_batch session (proof_test_members session execution) manifests in
      match consume_proof_activation_batch session second execution with
      | Error _ -> "replayed rejected dispatch=0 fallback=0"
      | Ok _ -> failwith "replayed proof activation manifest was accepted"
    in
    let foreign =
      let session, manifests, execution = proof_test_fixture () in
      let foreign = test_session () in
      ignore (proof_test_authority foreign);
      let batch = proof_activation_batch session (proof_test_members session execution) manifests in
      match consume_proof_activation_batch foreign batch execution with
      | Error _ -> "foreign rejected dispatch=0 fallback=0"
      | Ok _ -> failwith "foreign proof activation manifest was accepted"
    in
    let snapshot_replay =
      let session, manifests, execution = proof_test_fixture () in
      let manifest = List.hd manifests in
      let obligation = List.hd execution.Vir.obligations in
      match
        finalize_proof_activation session
          manifest.proof_manifest_snapshot obligation
      with
      | Error _ -> "snapshot-replayed rejected dispatch=0 fallback=0"
      | Ok _ -> failwith "replayed proof activation snapshot was accepted"
    in
    let substituted_snapshot =
      let session = test_session () in
      let authority = proof_test_authority session in
      let first = proof_test_obligation 0 false true in
      let second = proof_test_obligation 1 true false in
      let snapshot =
        snapshot_proof_activation authority ~activations:[] first
      in
      match finalize_proof_activation session snapshot second with
      | Error _ ->
          "substituted-snapshot rejected dispatch=0 fallback=0"
      | Ok _ -> failwith "substituted proof activation snapshot was accepted"
    in
    [
      control;
      rejected "absent" (fun session _ execution ->
          (session, [], execution));
      rejected "missing" (fun session manifests execution ->
          (session, [ List.hd manifests ], execution));
      rejected "duplicate" (fun session manifests execution ->
          let first = List.hd manifests in
          (session, [ first; first ], execution));
      rejected "counterfeit-issuer" (fun session manifests execution ->
          let first = List.hd manifests in
          let counterfeit =
            { first with proof_manifest_issuer = ref () }
          in
          (session, counterfeit :: List.tl manifests, execution));
      rejected "unused" (fun session manifests execution ->
          (session, manifests @ [ List.hd manifests ], execution));
      rejected "stale" (fun session manifests execution ->
          session.proof_activation_manifests <- [];
          (session, manifests, execution));
      foreign;
      rejected "wrong-artifact-family"
        (fun (session : t) manifests execution ->
          let identity =
            {
              session.identity with
              family_identity = "foreign-artifact-family";
            }
          in
          ({ session with identity }, manifests, execution));
      replay;
      snapshot_replay;
      substituted_snapshot;
      rejected "reordered" (fun session manifests execution ->
          (session, List.rev manifests, execution));
      rejected "swapped" (fun session manifests execution ->
          (session, List.rev manifests, execution));
      rejected "wrong-callable" (fun session manifests execution ->
          let wrong =
            {
              execution with
              Vir.function_ref =
                { Vir.function_index = 99; function_name = "other" };
            }
          in
          (session, manifests, wrong));
      rejected "wrong-obligation" (fun session manifests execution ->
          let first = List.hd execution.Vir.obligations in
          let changed =
            { first with Vir.goal = Vir.Boolean_constant false }
          in
          ( session,
            manifests,
            { execution with Vir.obligations = changed :: List.tl execution.obligations }
          ));
      rejected "wrong-index" (fun session manifests execution ->
          let first = List.hd execution.Vir.obligations in
          let changed = { first with Vir.obligation_index = 7 } in
          ( session,
            manifests,
            { execution with Vir.obligations = changed :: List.tl execution.obligations }
          ));
      rejected "wrong-kind" (fun session manifests execution ->
          let first = List.hd execution.Vir.obligations in
          let changed =
            {
              first with
              Vir.kind =
                Vir.Postcondition
                  {
                    postcondition_ordinal = 0;
                    declaration_span = test_span;
                  };
            }
          in
          ( session,
            manifests,
            { execution with Vir.obligations = changed :: List.tl execution.obligations }
          ));
      rejected "wrong-body" (fun session manifests execution ->
          let wrong =
            {
              execution with
              Vir.body_provenance =
                Sst.Raw_semantic_body test_span;
            }
          in
          (session, manifests, wrong));
      rejected "substituted-same-kind-ordinal"
        (fun session manifests execution ->
          let first = List.hd execution.Vir.obligations in
          let changed =
            {
              first with
              Vir.assumptions = [ Vir.Boolean_constant false ];
            }
          in
          ( session,
            manifests,
            { execution with Vir.obligations = changed :: List.tl execution.obligations }
          ));
      rejected "path-mismatch" (fun session manifests execution ->
          let first = List.hd execution.Vir.obligations in
          let changed =
            {
              first with
              Vir.path_condition = [ Vir.Boolean_constant true ];
            }
          in
          ( session,
            manifests,
            { execution with Vir.obligations = changed :: List.tl execution.obligations }
          ));
    ]

  let test_symbol =
    {
      Vir.symbol_id = 0;
      source_name = "Box.make.result";
      sort =
        Vir.Aggregate
          {
            aggregate_type_index = test_invariant.abstract_type.type_index;
            aggregate_type_name = test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
          };
      role = Vir.Result;
      span = test_span;
    }

  let test_aggregate =
    {
      Vir.aggregate_type =
        {
          aggregate_type_index = test_invariant.abstract_type.type_index;
          aggregate_type_name = test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
        };
      aggregate_desc = Vir.Aggregate_symbol test_symbol;
    }

  let return_goal =
    Vir.Boolean_invariant_application
      {
        invariant_id = test_invariant.invariant_id;
        model = test_invariant.model;
        predicate = test_invariant.predicate;
        value = test_aggregate;
      }

  let test_obligations =
    [
      {
        Vir.obligation_index = 0;
        function_ref =
          {
            function_index = test_callee.function_index;
            function_name = test_callee.function_name;
          };
        kind = Vir.Assertion { assertion_ordinal = 0 };
        span = test_span;
        assumptions = [];
        required_preceding_safety = [];
        path_condition = [];
        goal = Vir.Boolean_constant true;
        projection_symbols = [];
      };
      {
        Vir.obligation_index = 1;
        function_ref =
          {
            function_index = test_callee.function_index;
            function_name = test_callee.function_name;
          };
        kind =
          Vir.Invariant_validity
            {
              invariant_id = test_invariant.invariant_id;
              abstract_type = test_aggregate.aggregate_type;
              model =
                {
                  function_index = test_invariant.model.function_index;
                  function_name = test_invariant.model.function_name;
                };
              predicate =
                {
                  function_index = test_invariant.predicate.function_index;
                  function_name = test_invariant.predicate.function_name;
                };
              operation =
                {
                  function_index = test_callee.function_index;
                  function_name = test_callee.function_name;
                };
              boundary = Vir.Function_return;
            };
        span = test_span;
        assumptions = [];
        required_preceding_safety = [];
        path_condition = [];
        goal = return_goal;
        projection_symbols = [ test_symbol ];
      };
    ]

  let test_execution obligations =
    {
      Vir.function_ref =
        {
          function_index = test_callee.function_index;
          function_name = test_callee.function_name;
        };
      mode = Sst.Exec;
      body_provenance = test_snapshot.body_provenance;
      policy = Sst.Default_linear_z3;
      trusted_summary_uses = [];
      reached_callback_calls = [];
      owned_tree_transitions = [];
      shared_scalar_heap_reads = [];
      shared_scalar_heap_writes = [];
      obligations;
      exits = [];
    }

  let verified obligation =
    {
      Solver_backend.obligation;
      outcome = Solver_backend.Verified;
    }

  let mutated_results mutation =
    let complete = List.map verified test_obligations in
    match mutation with
    | Complete -> complete
    | Missing -> []
    | Incomplete -> [ List.hd complete ]
    | Failed ->
        [
          List.hd complete;
          {
            Solver_backend.obligation = List.nth test_obligations 1;
            outcome = Solver_backend.Counterexample [];
          };
        ]
    | Timeout ->
        [
          List.hd complete;
          {
            Solver_backend.obligation = List.nth test_obligations 1;
            outcome =
              Solver_backend.Inconclusive
                {
                  configured_timeout_ms = 1;
                  configured_rlimit = 1;
                  reason = Solver_backend.Timed_out;
                };
          };
        ]
    | Reordered -> List.rev complete
    | Duplicated -> [ List.hd complete; List.hd complete ]
    | Extra -> complete @ [ List.hd complete ]
    | Fingerprint_mismatch ->
        let original = List.hd test_obligations in
        let obligation =
          { original with Vir.goal = Vir.Boolean_constant false }
        in
        verified obligation :: List.tl complete
    | Wrong_return_boundary ->
        let original = List.nth test_obligations 1 in
        let kind =
          match original.Vir.kind with
          | Vir.Invariant_validity validity ->
              Vir.Invariant_validity
                {
                  validity with
                  boundary =
                    Vir.Call_result
                      {
                        callee =
                          {
                            function_index = test_callee.function_index;
                            function_name = test_callee.function_name;
                          };
                      };
                }
          | _ -> assert false
        in
        [ List.hd complete; verified { original with Vir.kind } ]

  let authorize_complete_issue session snapshot execution results =
    let* manifest = authorize_obligations session snapshot execution in
    let* completion = complete session manifest results in
    issue session completion

  let issue_case mutation =
    let session = test_session () in
    let results = mutated_results mutation in
    let execution =
      match mutation with
      | Wrong_return_boundary ->
          test_execution
            (List.map
               (fun (result : Solver_backend.obligation_result) ->
                 result.obligation)
               results)
      | Complete | Missing | Incomplete | Failed | Timeout | Reordered
      | Duplicated | Extra | Fingerprint_mismatch ->
          test_execution test_obligations
    in
    let result =
      authorize_complete_issue session test_snapshot execution results
    in
    let issued = (counters session).receipts_issued in
    destroy session;
    (result, issued)

  let mutation_name = function
    | Complete -> "complete"
    | Missing -> "missing"
    | Incomplete -> "incomplete"
    | Failed -> "failed"
    | Timeout -> "timeout"
    | Reordered -> "reordered"
    | Duplicated -> "duplicated"
    | Extra -> "extra"
    | Fingerprint_mismatch -> "fingerprint-mismatch"
    | Wrong_return_boundary -> "wrong-return-boundary"

  let mutation_line mutation =
    let result, issued = issue_case mutation in
    Printf.sprintf "obligations=%s result=%s receipts=%d"
      (mutation_name mutation)
      (match result with Ok () -> "issued" | Error _ -> "rejected")
      issued

  let snapshot_cases =
    [
      ( "program",
        { test_snapshot with
          program = { test_identity with program_snapshot = "wrong" } } );
      ( "cmt",
        { test_snapshot with
          program = { test_identity with cmt_identity = "wrong" } } );
      ( "family",
        { test_snapshot with
          program = { test_identity with family_identity = "ordinary" } } );
      ( "callee",
        { test_snapshot with
          callee = { test_callee with function_name = "Box.copy" } } );
      ( "resolved-path",
        { test_snapshot with resolved_path = "dot(ident(Substituted),make)" } );
      ( "path-substitution",
        {
          test_snapshot with
          resolved_path = "dot(ident(Substituted),make)";
          callable_key =
            String.concat "\000"
              [
                test_identity.unit_identity;
                "dot(ident(Substituted),make)";
                test_identity.signature_snapshot;
                string_of_int test_callee.function_index;
              ];
        } );
      ("binding-uid", { test_snapshot with binding_uid = "Substituted.3" });
      ("body", { test_snapshot with body_snapshot = "wrong" });
      ( "body-provenance",
        {
          test_snapshot with
          body_provenance = Sst.Raw_semantic_body test_span;
        } );
      ( "result-mode",
        { test_snapshot with result_mode = Sst.Tracked_instance } );
      ("result-type", { test_snapshot with result_type = Sst.Int });
      ( "invariant",
        { test_snapshot with
          invariant =
            { test_invariant with invariant_id = "invariant:Other.t" } } );
      ( "model",
        { test_snapshot with
          invariant =
            {
              test_invariant with
              model = { test_invariant.model with function_name = "Other.model" };
            } } );
      ( "predicate",
        { test_snapshot with
          invariant =
            {
              test_invariant with
              predicate =
                {
                  test_invariant.predicate with
                  function_name = "Other.invariant";
                };
            } } );
      ( "return-boundary",
        { test_snapshot with
          invariant =
            { test_invariant with predicate_digest = "wrong-return" } } );
    ]

  let snapshot_line (name, snapshot) =
    let session = test_session () in
    let result =
      authorize_complete_issue session snapshot
        (test_execution test_obligations)
        (mutated_results Complete)
    in
    let issued = (counters session).receipts_issued in
    destroy session;
    Printf.sprintf "snapshot=%s result=%s receipts=%d" name
      (match result with Ok () -> "issued" | Error _ -> "rejected")
      issued

  let session_isolation_line () =
    let first = test_session () in
    let first_issue =
      authorize_complete_issue first test_snapshot
        (test_execution test_obligations)
        (mutated_results Complete)
    in
    let first_consume =
      consume first test_snapshot
        ~caller:{ Sst.function_index = 4; function_name = "run" }
        ~call_span:test_span ~path_condition:[] ~result:test_aggregate
    in
    destroy first;
    let second = test_session () in
    let second_issue =
      authorize_complete_issue second test_snapshot
        (test_execution test_obligations)
        (mutated_results Missing)
    in
    let second_consume =
      consume second test_snapshot
        ~caller:{ Sst.function_index = 4; function_name = "run" }
        ~call_span:test_span ~path_condition:[] ~result:test_aggregate
    in
    let second_counters = counters second in
    destroy second;
    Printf.sprintf
      "two-sessions first-issue=%s first-consume=%s second-issue=%s second-consume=%s second-receipts=%d"
      (match first_issue with Ok () -> "yes" | Error _ -> "no")
      (match first_consume with Ok _ -> "yes" | Error _ -> "no")
      (match second_issue with Ok () -> "yes" | Error _ -> "no")
      (match second_consume with Ok _ -> "yes" | Error _ -> "no")
      second_counters.receipts_issued

  let metadata_without_receipt_line () =
    let session = test_session () in
    let consumed =
      consume session test_snapshot
        ~caller:{ Sst.function_index = 4; function_name = "run" }
        ~call_span:test_span ~path_condition:[] ~result:test_aggregate
    in
    destroy session;
    Printf.sprintf
      "metadata-only authenticated-snapshot=true same-session=true receipt-consume=%s"
      (match consumed with Ok _ -> "accepted" | Error _ -> "rejected")

  let call_instance_line () =
    let session = test_session () in
    let _ =
      authorize_complete_issue session test_snapshot
        (test_execution test_obligations)
        (mutated_results Complete)
    in
    let consume_once result =
      consume session test_snapshot
        ~caller:{ Sst.function_index = 4; function_name = "run" }
        ~call_span:test_span ~path_condition:[] ~result
    in
    let second_symbol =
      { test_symbol with symbol_id = 1; source_name = "Box.make.result.second" }
    in
    let second_aggregate =
      {
        test_aggregate with
        aggregate_desc = Vir.Aggregate_symbol second_symbol;
      }
    in
    let first = consume_once test_aggregate in
    let second = consume_once second_aggregate in
    let reused = consume_once test_aggregate in
    let fresh =
      match (first, second) with
      | Ok first, Ok second -> not (same_consumed_fact first second)
      | _ -> false
    in
    let consumed = (counters session).receipts_consumed in
    destroy session;
    Printf.sprintf
      "two-calls fresh-call-instances=%b reused-result=%s consumed=%d" fresh
      (match reused with Ok _ -> "accepted" | Error _ -> "rejected")
      consumed

  let fact_transfer_line () =
    let session = test_session () in
    let _ =
      authorize_complete_issue session test_snapshot
        (test_execution test_obligations)
        (mutated_results Complete)
    in
    let consume_result snapshot result =
      consume session snapshot
        ~caller:{ Sst.function_index = 4; function_name = "run" }
        ~call_span:test_span ~path_condition:[] ~result
    in
    let second_symbol =
      { test_symbol with symbol_id = 10; source_name = "second-call" }
    in
    let second_aggregate =
      {
        test_aggregate with
        aggregate_desc = Vir.Aggregate_symbol second_symbol;
      }
    in
    let first = consume_result test_snapshot test_aggregate in
    let second = consume_result test_snapshot second_aggregate in
    let aggregate symbol_id source_name aggregate_type =
      {
        Vir.aggregate_type;
        aggregate_desc =
          Vir.Aggregate_symbol
            {
              Vir.symbol_id;
              source_name;
              sort = Vir.Aggregate aggregate_type;
              role = Vir.Result;
              span = test_span;
            };
      }
    in
    let copied =
      aggregate 1 "copied"
        {
          Vir.aggregate_type_index = test_invariant.abstract_type.type_index;
          aggregate_type_name = test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
        }
    in
    let widened =
      aggregate 2 "widened"
        {
          Vir.aggregate_type_index = 99;
          aggregate_type_name = "Widened.t";
      aggregate_type_arguments = [];
        }
    in
    let rebound =
      aggregate 3 "rebound"
        {
          Vir.aggregate_type_index = test_invariant.abstract_type.type_index;
          aggregate_type_name = test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
        }
    in
    let invariant_fact value =
      Vir.Boolean_invariant_application
        {
          invariant_id = test_invariant.invariant_id;
          model = test_invariant.model;
          predicate = test_invariant.predicate;
          value;
        }
    in
    let true_alias, copied_transfer, widened_transfer, rebound_transfer,
        swapped_transfer =
      match (first, second) with
      | Ok first, Ok second ->
          ( consumed_matches_closed_fact first
              (consumed_closed_fact first),
            consumed_matches_closed_fact first (invariant_fact copied),
            consumed_matches_closed_fact first (invariant_fact widened),
            consumed_matches_closed_fact first (invariant_fact rebound),
            same_consumed_fact first second )
      | _ -> (false, false, false, false, false)
    in
    let ghost_transfer =
      match
        consume_result
          { test_snapshot with result_mode = Sst.Ghost_instance }
          (aggregate 4 "ghost"
             {
               Vir.aggregate_type_index =
                 test_invariant.abstract_type.type_index;
               aggregate_type_name = test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
             })
      with
      | Ok _ -> true
      | Error _ -> false
    in
    destroy session;
    Printf.sprintf
      "fact-transfer true-alias=%b copied=%b widened=%b rebound=%b swapped=%b ghost=%b"
      true_alias copied_transfer widened_transfer rebound_transfer
      swapped_transfer ghost_transfer

  let destroyed_line () =
    let session = test_session () in
    destroy session;
    let result =
      authorize_complete_issue session test_snapshot
        (test_execution test_obligations)
        (mutated_results Complete)
    in
    Printf.sprintf "destroyed-session issue=%s active=%b"
      (match result with Ok () -> "accepted" | Error _ -> "rejected")
      (is_active session)

  let set_finite_result_snapshot_attack_for_testing attack =
    finite_result_snapshot_attack_for_testing := attack

  let set_finite_result_call_instance_attack_for_testing attack =
    finite_result_call_instance_attack_for_testing := attack

  let set_finite_result_double_consume_for_testing enabled =
    finite_result_double_consume_for_testing := enabled

  let retained_abi_lines attacks =
    let delta (before : counters) (after : counters) =
      ( after.finite_formal_assumption_issuances
        - before.finite_formal_assumption_issuances,
        after.finite_formal_transfer_batches
        - before.finite_formal_transfer_batches,
        after.finite_formal_transfers - before.finite_formal_transfers,
        after.finite_formal_transfer_consumptions
        - before.finite_formal_transfer_consumptions )
    in
    let downstream_delta (before : counters) (after : counters) =
      ( after.callee_solver_attempts - before.callee_solver_attempts,
        after.dependent_lowerings - before.dependent_lowerings,
        after.dependent_backend_contexts - before.dependent_backend_contexts,
        after.dependent_solver_attempts - before.dependent_solver_attempts )
    in
    let prepare_formal_entry_baseline session =
      let typ = Sst.Aggregate test_invariant.abstract_type in
      let rank =
        {
          Finite_value_registry.domain_id = "retained-abi-node";
          domain_version = "1";
          domain_digest = "retained-abi-node-rank";
          component_snapshot = [ "node" ];
          profile_actual_snapshot = [ "node" ];
        }
      in
      List.iter
        (fun ordinal ->
          let slot =
            match
              Finite_value_registry.register_formal session.finite_registry
                ~callee:"Provider.checked" ~ordinal ~label:None
                ~pattern_digest:(Printf.sprintf "formal-%d" ordinal)
                ~binding_ids:[ ordinal + 1 ] ~mode:Sst.Exec_instance ~typ
                ~rank
                ~requirement_digest:
                  (Printf.sprintf "requirement-%d" ordinal)
            with
            | Ok slot -> slot
            | Error message -> failwith message
          in
          let symbol =
            {
              Vir.symbol_id = ordinal;
              source_name = Printf.sprintf "formal.%d" ordinal;
              sort =
                Vir.Aggregate
                  {
                    aggregate_type_index =
                      test_invariant.abstract_type.type_index;
                    aggregate_type_name =
                      test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
                  };
              role = Vir.Input;
              span = test_span;
            }
          in
          let value =
            {
              Vir.aggregate_type =
                {
                  aggregate_type_index =
                    test_invariant.abstract_type.type_index;
                  aggregate_type_name =
                    test_invariant.abstract_type.type_name;
      aggregate_type_arguments = [];
                };
              aggregate_desc = Vir.Aggregate_symbol symbol;
            }
          in
          match
            Finite_value_registry.assume_formal session.finite_registry ~slot
              ~callable:"Provider.checked" ~value
              ~mode:Sst.Exec_instance ~typ ~rank
          with
          | Ok _ -> ()
          | Error message -> failwith message)
        [ 0; 1 ]
    in
    List.map
      (fun attack ->
        let session = test_session () in
        prepare_formal_entry_baseline session;
        let before = counters session in
        if before.finite_formal_assumption_issuances <> 2 then
          failwith "retained ABI attack lacks the exact post-entry baseline";
        let result = Imported_callable.For_testing.run_abi_attack attack in
        let after = counters session in
        let formal = delta before after in
        let downstream = downstream_delta before after in
        if formal <> (0, 0, 0, 0) || downstream <> (0, 0, 0, 0) then
          failwith
            "retained ABI rejection changed production call-boundary counters";
        destroy session;
        let diagnostic =
          match result with
          | Error message -> message
          | Ok () -> failwith "retained ABI attack was unexpectedly accepted"
        in
        Printf.sprintf
          "%s diagnostic=%s delta=+0/+0/+0/+0 downstream=+0/+0/+0/+0"
          (Imported_callable.For_testing.abi_attack_name attack)
          diagnostic)
      attacks

  let retained_abi_matrix () =
    retained_abi_lines Imported_callable.For_testing.abi_attacks

  let retained_abi_attack attack =
    match retained_abi_lines [ attack ] with
    | [ line ] -> line
    | [] | _ :: _ :: _ -> assert false

  let adversarial_matrix () =
    let mutations =
      [
        Complete;
        Missing;
        Incomplete;
        Failed;
        Timeout;
        Reordered;
        Duplicated;
        Extra;
        Fingerprint_mismatch;
        Wrong_return_boundary;
      ]
    in
    List.map mutation_line mutations
    @ List.map snapshot_line snapshot_cases
    @ [
        session_isolation_line ();
        metadata_without_receipt_line ();
        call_instance_line ();
        fact_transfer_line ();
        destroyed_line ();
      ]
end
