type reason =
  | Completion_mismatch
  | Missing_coordinator_evidence
  | Missing_local_identity of Sst.function_id
  | Missing_execution_evidence of Sst.function_id
  | Missing_broadcast_evidence of Sst.function_id
  | Unsupported_dependency of Sst.function_id
  | Native_bv_dependency of Sst.function_id
  | Logical_constant_context
  | Dependency_cycle of Sst.function_id

type dependency = private {
  owner_artifact_full_key : string;
  definition : Sst.function_definition;
  compiler_uid : string;
  calls : Sst_validation.call_edge_descriptor list;
  broadcasts : Verification_proof_evidence_private.insertion list;
  trusted : bool;
}

type t = private {
  owner_artifact_full_key : string;
  root : Sst.function_definition;
  dependencies : dependency list;
  trusted_dependencies : dependency list;
}

type imported_spec_leaf
val imported_spec_leaf : registration:Imported_callable.registration -> origin:Cmt_input.implementation ->
  evidence:t -> path:string -> interface_uid:string -> Sst.expression -> (imported_spec_leaf, string) result
val complete_definition_with_imported : completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> validated:Sst_validation.validated_program ->
  imported_leaves:imported_spec_leaf list -> Sst.function_definition -> (t, reason) result

type context = Ghost_context | Runtime_context
val definition_expressions : Sst.function_definition -> Sst.expression list

(** Completes proof dependencies only. Runtime context additionally permits
    independently checked scalar executable bodies or explicit external-body
    trust. It does not establish any numeric equivalence or effect contract. *)
val complete_definition :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program -> context:context ->
  Sst.function_definition -> (t, reason) result

(** Completes a conservative local dependency closure for the matched theorem.
    Source calls include contracts; inserted proved broadcasts are followed just
    like ordinary proof dependencies. Explicit axiom dependencies remain visible.
    Successful evidence authorizes only the exact ghost theorem under that TCB,
    not runtime equivalence or native lowering.

    Imported, recursive, callback, heap and logical-constant contexts currently
    return unavailable evidence, without changing ordinary verification. *)
val complete_unsigned_range :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  Numeric_law_match_private.unsigned_range -> (t, reason) result

val complete_signed_view :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  Numeric_law_match_private.signed_view -> (t, reason) result
