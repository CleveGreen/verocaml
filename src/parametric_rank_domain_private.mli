(** Process-local authority for structural rank over canonical parametric ADT
    schemas.  Values of [validated_rank_domain] are never portable. *)

type validated_rank_domain
type portable_claim

type nominal_spec = {
  component : Typedtree_adapter_issuance_private.rank_type_identity list;
  positive_children :
    Typedtree_adapter_issuance_private.rank_positive_child list;
  ground_witnesses :
    Typedtree_adapter_issuance_private.rank_ground_witness list;
  actual_evidence : string list;
}

type error_kind = Invalid_authority | Unsupported_application_actual

type error = {
  span : Diagnostic.span;
  detail : string;
  kind : error_kind;
}

val seal_local_schemas :
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  (unit, error) result

val issue_nominal :
  program:Sst.program -> nominal_spec list -> validated_rank_domain list

val nominal_domains : Sst.program -> validated_rank_domain list

val derive_application :
  program:Sst.program ->
  span:Diagnostic.span ->
  Parametric_type.t ->
  (validated_rank_domain, error) result

val authenticate :
  program:Sst.program -> validated_rank_domain -> bool

val domain_id : validated_rank_domain -> string
val domain_version : validated_rank_domain -> string
val snapshot_digest : validated_rank_domain -> string
val component :
  validated_rank_domain ->
  Typedtree_adapter_issuance_private.rank_type_identity list
val positive_children :
  validated_rank_domain ->
  Typedtree_adapter_issuance_private.rank_positive_child list
val ground_witnesses :
  validated_rank_domain ->
  Typedtree_adapter_issuance_private.rank_ground_witness list
val immutable : validated_rank_domain -> bool
val application : validated_rank_domain -> Parametric_type.t option
val actual_arguments : validated_rank_domain -> Parametric_type.t list

val register_profiles :
  structure:Typedtree.structure ->
  Typedtree_adapter_issuance_private.issued_rank_profile list ->
  unit

val profiles :
  Typedtree.structure ->
  Typedtree_adapter_issuance_private.issued_rank_profile list

val authenticate_profile :
  structure:Typedtree.structure ->
  Typedtree_adapter_issuance_private.issued_rank_profile ->
  bool

val portable_claim :
  provider_unit:string ->
  provider_artifact:string ->
  validated_rank_domain ->
  portable_claim

val portable_claim_digest : portable_claim -> string
val portable_claim_application : portable_claim -> Parametric_type.t option

val reauthenticate_claim :
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  descriptor:Parametric_adt.t ->
  portable_claim ->
  (validated_rank_domain, error) result
