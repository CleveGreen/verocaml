type id

type receipt = private {
  schema : string;
  issuer_authority : string;
  build_compatibility : string;
  ocaml_abi : string;
  ocaml_max_int : Z.t;
  z3_package : string;
  c_unsigned_width : int;
  c_unsigned_maximum : Z.t;
  maximum_bv_width : int;
  full_key : string;
  checked_id : id;
}

type artifact_binding = private { full_key : string }
type reference
type capability
type worker_receipt : value mod portable = private {
  maximum_bv_width : int;
  full_key : string;
  checked_id : string;
  checked_id_hex : string;
  abi_supports_ceiling : bool;
  artifact_binding_key : string;
}

val schema : string
val maximum_bv_width : int
val capability : unit -> capability
val authenticate :
  capability -> (receipt * artifact_binding option, string) result
val authenticate_profile :
  capability -> Build_target_profile_private.profile -> (unit, string) result
val authenticate_instance :
  capability -> Build_target_profile_private.instance -> (unit, string) result
val id_to_hex : id -> string
val reference : capability -> (reference, string) result
val encode_reference : reference -> string
val decode_reference : capability -> string -> (reference, string) result
val decode_reference_for_worker :
  string -> (worker_receipt, string) result @@ portable
val equal_receipt : receipt -> receipt -> bool
val equal_artifact_binding : artifact_binding -> artifact_binding -> bool
