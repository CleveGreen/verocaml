type semantic_authority = Checked_proof | Explicit_axiom
type provenance = Imported_registry | Same_unit_prior_closure
type trusted_dependency_origin = Imported_law | Same_unit_law
type trusted_dependency = private {
  origin : trusted_dependency_origin;
  compiler_identity : string;
  evidence_full_key : string;
}

type operation_observation = private {
  descriptor_origin : string;
  occurrence_identity : string;
  call_span : Diagnostic.span;
  callable_uid : string;
  callable_tag : string;
  callable_abi : string;
  law_evidence_full_key : string;
  authority : semantic_authority;
}

type runtime_equivalence = Runtime_unknown

type source_observation = private {
  occurrence_identity : string;
  semantic_width : Bv_width.t;
  provenance : provenance;
  trusted_dependencies : trusted_dependency list;
  outer_operation : operation_observation;
  modular_operation : operation_observation;
  carrier_binding_full_key : string;
  base_int_full_key : string;
  source_witness_full_key : string;
  profile_full_key : string;
  target_full_key : string;
  backend_capability_full_key : string;
  backend_capability_id : string;
  backend_abi_receipt : string;
  runtime_equivalence : runtime_equivalence;
}

(** A sealed, source-occurrence-specific authority for the one admitted
    unsigned modular projection.  The complete target, width, semantic trust
    axes, and source occurrence are retained by the value; its printable key
    is identity only and cannot be used to reconstruct authority. *)
type t

module For_source_admission : sig
  val trusted_dependency :
    origin:trusted_dependency_origin ->
    compiler_identity:string ->
    evidence_full_key:string ->
    trusted_dependency

  val operation :
    descriptor_origin:string ->
    occurrence_identity:string ->
    call_span:Diagnostic.span ->
    callable_uid:string ->
    callable_tag:string ->
    callable_abi:string ->
    law_evidence_full_key:string ->
    authority:semantic_authority ->
    operation_observation

  val issue :
    target:Build_target_profile_private.instance ->
    width:Bv_width.t ->
    provenance:provenance ->
    trusted_dependencies:trusted_dependency list ->
    occurrence_identity:string ->
    outer_operation:operation_observation ->
    modular_operation:operation_observation ->
    carrier_binding_full_key:string ->
    base_int_full_key:string ->
    source_witness_full_key:string ->
    occurrence_full_key:string ->
    t
end

val validate : width:Bv_width.t -> t -> (unit, string) result
val full_key : t -> string
val provenance : t -> provenance
val semantic_authorities : t -> semantic_authority * semantic_authority
val trusted_dependencies : t -> trusted_dependency list
val source_observation : t -> source_observation
