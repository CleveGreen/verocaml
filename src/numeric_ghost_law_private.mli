type authority = Checked_proof | Explicit_axiom
type meaning = Unsigned_range | Signed_view | Relation of Numeric_relation_match_private.kind
val meaning_name : meaning -> string

type dependency = private {
  compiler_uid : string;
  trusted : bool;
  full_key : string;
}

type t = private {
  issuer_unit : string;
  issuer_artifact_full_key : string;
  source_evidence : Numeric_proof_dependencies_private.t;
  base_int : Numeric_base_int_binding_private.t;
  carrier : Numeric_artifact_binding_private.facts;
  role : Numeric_interface_claim_private.role;
  target : Build_target_profile_private.instance;
  semantic_width : Numeric_semantic_width_private.t;
  result_interpretation : Numeric_law_match_private.result_interpretation;
  authority : authority;
  meaning : meaning;
  prerequisites : t list;
  relation : Numeric_relation_match_private.t option;
  dependencies : dependency list;
  full_key : string;
  checked_digest : string;
}

type reason =
  | Completion_mismatch
  | Unsupported_role
  | Nonlocal_role
  | Carrier_domain_unavailable
  | Base_unavailable of string
  | Base_not_imported
  | Carrier_unavailable of string
  | Target_unavailable of string
  | Statement_mismatch
  | Prerequisite_mismatch
  | Dependencies_unavailable of Numeric_proof_dependencies_private.reason

val reason_message : reason -> string

(** Admits the exact local ghost proposition [0 <= view x < 2^semantic_width],
    combining compiler carrier/callable identities, an existing base Int receipt
    at the same or an exact directly imported provider, and completed proof/TCB
    dependencies. Equality uses the complete frozen key, not its display digest.

    This does not establish carrier/profile layout compatibility, a runtime
    implementation refinement, reveal/inline permission or native candidacy.
    Those source requests remain claims. Imported laws/carriers, non-nominal
    domains, generic and unsupported proof contexts remain unavailable here;
    ordinary verification is unaffected. *)
val admit_unsigned_range :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  base_provider:Cmt_input.implementation ->
  logical_sort:Logical_sort_private.t ->
  target:Build_target_profile_private.instance ->
  Numeric_semantics_binding_private.t -> (t, reason) result

(** Admits the exact signed two's-complement relation to a prior unsigned law
    for the same artifact, carrier, base Int and target. That prerequisite is
    independently re-admitted under the current completion; its full identity
    and transitive trust remain part of the signed receipt. This still grants
    no runtime or native authority. *)
val admit_signed_view :
  completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  base_provider:Cmt_input.implementation ->
  logical_sort:Logical_sort_private.t ->
  target:Build_target_profile_private.instance ->
  unsigned:t ->
  Numeric_semantics_binding_private.t -> (t, reason) result

val equal : t -> t -> bool
val admit_relation :
  completion:Verification_driver_private.completion -> implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program -> base_provider:Cmt_input.implementation ->
  logical_sort:Logical_sort_private.t -> target:Build_target_profile_private.instance ->
  kind:Numeric_relation_match_private.kind -> view:t -> bounds:t option ->
  Numeric_semantics_binding_private.t -> (t, reason) result
val compare : t -> t -> int

val admit_operation_extension : completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> validated:Sst_validation.validated_program ->
  imported:Imported_callable.environment -> origin:Cmt_input.implementation ->
  view:t -> Numeric_semantics_binding_private.t -> (t, reason) result
