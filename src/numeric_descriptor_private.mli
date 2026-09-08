type base_int_claim_v1 = private {
  base_int_full_key : string;
  validated_base_int_digest : string;
  outer_artifact_binding_key_claim : string;
}

type representation = Immediate | Boxed | Unboxed | Unsupported of string

type carrier_claim = private {
  type_path_claim : string;
  type_uid_claim : string;
  constructor_abi_claim : string;
  binder_abi_claim : string;
  representation_claim : representation;
  type_argument_claims : string list;
  fully_instantiated_claim : bool;
}

type callable_claim = private {
  callable_path_claim : string;
  callable_uid_claim : string;
  binder_abi_claim : string;
  parameter_abi_claims : string list;
  result_abi_claim : string;
  total_claim : bool;
  effect_claims : string list;
  exception_claims : string list;
}

type semantic_role =
  | Unsigned_view
  | Signed_view
  | Representable_bounds
  | Checked_conversion
  | Partial_conversion of { success_abi : string; failure_abi : string }
  | Modular_conversion
  | Operation of { role_schema : string; role_identity : string }

type int_semantics =
  | Unsigned_range of { width : int }
  | Signed_twos_complement of { width : int }
  | Bounds of { minimum : Z.t; maximum : Z.t }
  | Checked_exact
  | Partial_exact
  | Modular_quotient of { width : int }
  | Int_relation of { relation_full_key_claim : string }

type proof_claim = private {
  proof_full_key_claim : string;
  dependency_closure_claims : string list;
}

type semantic_authority_claim =
  | Kernel_schema_claim of { dual_role_receipt_claim : string }
  | Proved_law_claim of proof_claim
  | Trusted_axiom_claim of { trust_receipt_claim : string }
  | Ordinary_specification_claim

type implementation_refinement_claim =
  | Kernel_refinement_claim of { dual_role_receipt_claim : string }
  | Proved_refinement_claim of {
      proof_claim : proof_claim;
      implementation_artifact_claim : string;
      implementation_body_claim : string;
    }
  | Trusted_refinement_claim of {
      trust_receipt_claim : string;
      implementation_artifact_claim : string;
      implementation_body_claim : string;
    }
  | Tested_only_claim of { evidence_claim : string }
  | Unknown_refinement_claim

type visibility = Visible_body | Opaque_body
type reveal_permission = Reveal_allowed | Reveal_forbidden
type inline_permission = Inline_allowed | Inline_forbidden

type binding_claim = private {
  law_identity_claim : string;
  callable_claim : callable_claim;
  semantic_role : semantic_role;
  int_semantics : int_semantics;
  semantic_authority_claim : semantic_authority_claim;
  implementation_refinement_claim : implementation_refinement_claim;
  visibility : visibility;
  reveal_permission : reveal_permission;
  inline_permission : inline_permission;
}

type claim = private {
  schema : string;
  base_int_claim : base_int_claim_v1;
  provider_unit_claim : string;
  provider_origin_claim : string;
  provider_interface_receipt_claim : string;
  direct_import_provenance_claim : string;
  carrier_claim : carrier_claim;
  profile_claim : Target_profile_private.claim;
  target_claim : Target_profile_private.instance_claim;
  binding_claims : binding_claim list;
  issuer_claim : string;
  immutable_fingerprint_claim : string;
  full_key : string;
  checked_digest : string;
}

val schema : string

val base_int_claim :
  logical_sort:Logical_sort_private.t ->
  validated_digest:string ->
  outer_artifact_binding_key_claim:string ->
  (base_int_claim_v1, string) result

val carrier_claim :
  type_path_claim:string ->
  type_uid_claim:string ->
  constructor_abi_claim:string ->
  binder_abi_claim:string ->
  representation_claim:representation ->
  type_argument_claims:string list ->
  fully_instantiated_claim:bool ->
  (carrier_claim, string) result

val callable_claim :
  callable_path_claim:string ->
  callable_uid_claim:string ->
  binder_abi_claim:string ->
  parameter_abi_claims:string list ->
  result_abi_claim:string ->
  total_claim:bool ->
  effect_claims:string list ->
  exception_claims:string list ->
  (callable_claim, string) result

val proof_claim :
  proof_full_key_claim:string ->
  dependency_closure_claims:string list ->
  forbidden_dependency_claims:string list ->
  (proof_claim, string) result

val binding_claim :
  law_identity_claim:string ->
  callable_claim:callable_claim ->
  semantic_role:semantic_role ->
  int_semantics:int_semantics ->
  semantic_authority_claim:semantic_authority_claim ->
  implementation_refinement_claim:implementation_refinement_claim ->
  visibility:visibility ->
  reveal_permission:reveal_permission ->
  inline_permission:inline_permission ->
  (binding_claim, string) result

val issue_claim :
  base_int_claim:base_int_claim_v1 ->
  provider_unit_claim:string ->
  provider_origin_claim:string ->
  provider_interface_receipt_claim:string ->
  direct_import_provenance_claim:string ->
  carrier_claim:carrier_claim ->
  profile_claim:Target_profile_private.claim ->
  target_claim:Target_profile_private.instance_claim ->
  binding_claims:binding_claim list ->
  issuer_claim:string ->
  (claim, string) result

val decode_claim :
  expected_profile_claim:Target_profile_private.claim ->
  expected_base_int_claim:base_int_claim_v1 ->
  string ->
  (claim, string) result

val semantic_role_material : semantic_role -> string
val binding_claim_material : binding_claim -> string
val compare_claim : claim -> claim -> int
val equal_claim : claim -> claim -> bool
