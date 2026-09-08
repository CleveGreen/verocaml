type error_kind = Sst_validation_private.Public.error_kind =
  | Unsupported_policy of Sst.verification_policy
  | Duplicate_type_id of Sst.type_id
  | Duplicate_function_id of Sst.function_id
  | Duplicate_binding_id of int
  | Unknown_type_id of Sst.type_id
  | Unknown_function_id of Sst.function_id
  | Conflicting_identity of string
  | Invalid_clause of string
  | Invalid_body of string
  | Invalid_call of string
  | Invalid_call_stage of {
      callee : Sst.function_id;
      stage : Sst.expression_stage;
      callee_mode : Sst.verification_mode;
    }
  | Unannotated_erased_call of Instance_mode.unannotated_erased_call
  | Invalid_recursive_marker of string
  | Invalid_unique_return of string
  | Invalid_target_link of string
  | Forged_abstract_evidence of string
  | Forged_rank_domain of string
  | Unsupported_rank_application_actual of string
  | Invalid_instance_mode of string
  | Invalid_finite_requirement of string
  | Unbound_binding of Sst.binding
  | Malformed_expression of string

type error = Sst_validation_private.Public.error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  kind : error_kind;
}

type validated_program = Sst_validation_private.Public.validated_program

type type_descriptor = Sst_validation_private.Public.type_descriptor
type logical_type_descriptor = Sst_validation_private.Public.logical_type_descriptor
type callable_descriptor = Sst_validation_private.Public.callable_descriptor
type finite_formal_descriptor = Sst_validation_private.Public.finite_formal_descriptor
type frozen_formal_descriptor = Sst_validation_private.Public.frozen_formal_descriptor
type model_descriptor = Sst_validation_private.Public.model_descriptor
type call_edge_descriptor = Sst_validation_private.Public.call_edge_descriptor
type formal_actual_descriptor = Sst_validation_private.Public.formal_actual_descriptor
type visibility_descriptor = Sst_validation_private.Public.visibility_descriptor
type feature_descriptor = Sst_validation_private.Public.feature_descriptor
type contract_descriptor = Sst_validation_private.Public.contract_descriptor
type contract_clause_descriptor = Sst_validation_private.Public.contract_clause_descriptor
type decrease_descriptor = Sst_validation_private.Public.decrease_descriptor
type validated_rank_domain = Sst_validation_private.Public.validated_rank_domain

type call_edge_region = Sst_validation_private.Public.call_edge_region =
  | Requires_region
  | Ensures_region
  | Decreases_region
  | Assertions_region
  | Body_region

type decrease_disposition = Sst_validation_private.Public.decrease_disposition =
  | No_decrease
  | Direct_integer_decrease of decrease_descriptor
  | Direct_structural_decrease of
      decrease_descriptor * validated_rank_domain
  | Direct_parametric_decrease of
      decrease_descriptor * Parametric_adt.t
  | Direct_frozen_spine_decrease of
      decrease_descriptor * Sst.frozen_spine_prerequisite
  | Missing_decrease of Diagnostic.span
  | Duplicate_decrease of Diagnostic.span
  | Inapplicable_decrease of Diagnostic.span
  | Non_integer_decrease of Diagnostic.span
  | Recursive_decrease of Diagnostic.span

type semantic_feature_requirement = Sst_validation_private.Public.semantic_feature_requirement =
  | Specification_semantics
  | Proof_semantics
  | External_specification_trust
  | Trusted_external_body
  | Owned_tree_reconstruction
  | Direct_recursion

val validate : Sst.program -> (validated_program, error) result
val program : validated_program -> Sst.program
val external_target_identity :
  validated_program -> Sst.function_definition -> Sst.target_link option

val type_descriptors : validated_program -> type_descriptor list
val find_type : validated_program -> Sst.type_id -> type_descriptor option
val type_id : type_descriptor -> Sst.type_id
val type_definition : type_descriptor -> Sst.type_definition
val type_visibility : type_descriptor -> visibility_descriptor
val type_logical_type : type_descriptor -> logical_type_descriptor option
val visibility_representation :
  visibility_descriptor -> Sst.representation_visibility
val find_logical_type :
  validated_program -> Sst.typ -> logical_type_descriptor option
val logical_source_type : logical_type_descriptor -> Sst.typ
val logical_nominal_type :
  logical_type_descriptor -> Sst.type_id option

val callable_descriptors : validated_program -> callable_descriptor list
val find_callable :
  validated_program -> Sst.function_id -> callable_descriptor option
val callable_id : callable_descriptor -> Sst.function_id
val callable_mode : callable_descriptor -> Sst.verification_mode
val callable_definition : callable_descriptor -> Sst.function_definition
val callable_contract : callable_descriptor -> contract_descriptor
val callable_decrease : callable_descriptor -> decrease_disposition

val formal_requires_finite :
  validated_program -> callable_descriptor -> int -> bool

val finite_formal_requirement :
  validated_program ->
  callable_descriptor ->
  int ->
  finite_formal_descriptor option

val frozen_formal_requirement :
  validated_program ->
  callable_descriptor ->
  int ->
  frozen_formal_descriptor option

val frozen_formal_type : frozen_formal_descriptor -> Sst.type_id

val finite_formal_ordinal : finite_formal_descriptor -> int
val finite_formal_label : finite_formal_descriptor -> string option
val finite_formal_pattern_digest : finite_formal_descriptor -> string
val finite_formal_binding_ids : finite_formal_descriptor -> int list
val finite_formal_type : finite_formal_descriptor -> Sst.typ
val finite_formal_mode : finite_formal_descriptor -> Sst.instance_mode
val finite_formal_rank_domain :
  finite_formal_descriptor -> validated_rank_domain
val finite_formal_digest : finite_formal_descriptor -> string
val finite_formal_dump : validated_program -> string

val binding_instance_mode :
  validated_program ->
  Sst.function_id ->
  Sst.binding ->
  Sst.instance_mode

val expression_instance_mode :
  validated_program ->
  Sst.function_id ->
  Sst.expression ->
  Sst.instance_mode

val expression_mode_explicit :
  validated_program -> Sst.function_id -> Sst.expression -> bool

val field_instance_mode :
  validated_program -> Sst.field_definition -> Sst.instance_mode

val formal_instance_mode :
  validated_program ->
  callable_descriptor ->
  int ->
  Sst.parameter ->
  Sst.instance_mode

val formal_has_closed_invariant_authority :
  validated_program -> callable_descriptor -> int -> bool

val result_instance_mode :
  validated_program -> callable_descriptor -> Sst.instance_mode

val instance_modes_authenticated : validated_program -> bool
val instance_mode_dump : validated_program -> string

val model_descriptors : validated_program -> model_descriptor list
val find_model :
  validated_program -> Sst.function_id -> model_descriptor option
val model_callable : model_descriptor -> callable_descriptor
val model_domain : model_descriptor -> type_descriptor
val model_result : model_descriptor -> logical_type_descriptor
val model_visibility : model_descriptor -> visibility_descriptor

val contract_requires :
  contract_descriptor -> contract_clause_descriptor list
val contract_ensures :
  contract_descriptor -> contract_clause_descriptor list
val contract_decreases : contract_descriptor -> decrease_descriptor list
val contract_assertions :
  contract_descriptor -> contract_clause_descriptor list
val contract_clause_index : contract_clause_descriptor -> int
val contract_clause_span : contract_clause_descriptor -> Diagnostic.span
val contract_clause_binder :
  contract_clause_descriptor -> Sst.pattern option
val contract_clause_expression :
  contract_clause_descriptor -> Sst.expression
val decrease_clause : decrease_descriptor -> contract_clause_descriptor

val call_edge_descriptors : validated_program -> call_edge_descriptor list
val call_edge_caller : call_edge_descriptor -> callable_descriptor
val call_edge_callee : call_edge_descriptor -> callable_descriptor
val call_edge_form : call_edge_descriptor -> Sst.call_form
val call_edge_recursive : call_edge_descriptor -> bool
val call_edge_span : call_edge_descriptor -> Diagnostic.span
val call_edge_region : call_edge_descriptor -> call_edge_region
val call_edge_actuals :
  call_edge_descriptor -> formal_actual_descriptor list
val formal_parameter : formal_actual_descriptor -> Sst.parameter
val actual_label : formal_actual_descriptor -> string option
val actual_expression : formal_actual_descriptor -> Sst.expression

val feature_descriptors : validated_program -> feature_descriptor list
val feature_callable : feature_descriptor -> callable_descriptor
val feature_requirement :
  feature_descriptor -> semantic_feature_requirement

val abstraction_evidence :
  validated_program ->
  Sst.type_id ->
  Sst.same_cmt_abstraction_evidence option

val rank_domains : validated_program -> validated_rank_domain list
val rank_domain_id : validated_rank_domain -> string
val rank_domain_version : validated_rank_domain -> string
val rank_snapshot_digest : validated_rank_domain -> string
val rank_component :
  validated_rank_domain -> Typedtree_adapter.rank_type_identity list
val rank_positive_children :
  validated_rank_domain -> Typedtree_adapter.rank_positive_child list
val rank_ground_witnesses :
  validated_rank_domain -> Typedtree_adapter.rank_ground_witness list
val rank_immutable : validated_rank_domain -> bool
val error_to_string : error -> string
val to_diagnostic : error -> Diagnostic.t

module For_testing : sig
  val reset_ghost_formal_flow_count : unit -> unit
  val ghost_formal_flow_count : unit -> int
  val copied_instance_mode_descriptor_rejected : validated_program -> bool
end
