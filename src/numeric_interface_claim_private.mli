(** Canonical serialization of compiler-reconstruction claims only.  Decoding
    validates grammar and self-consistency, not artifact adjacency or authority;
    consumers must correlate a claim through [Cmt_input]. *)

type owner = private {
  owner_unit : string;
  owner_cmi_full_key : string;
  owner_cmi_checked_digest : string;
  import_routes : string list;
}

type base_reference = private {
  type_path : string;
  type_uid : string;
  base_owner : owner;
}

type carrier = private {
  base_reference : base_reference option;
  source_claim : string;
  carrier_path : string;
  carrier_uid : string;
  owner : owner;
  constructor_abi : string;
  binder_abi : string;
  compiler_jkind_abi : string;
  compiler_representation : string;
  claim_key : string;
  transport_key : string;
}

type role = private {
  source_claim : string;
  callable_path : string;
  callable_uid : string;
  callable_type_abi : string;
  callable_mode_abi : string;
  callable_owner : owner;
  semantics_path : string;
  semantics_uid : string;
  semantics_type_abi : string;
  semantics_mode_abi : string;
  semantics_owner : owner;
  carrier_uid : string;
  carrier_owner : owner;
  claim_key : string;
  transport_key : string;
}

val owner :
  owner_unit:string ->
  owner_cmi_full_key:string ->
  owner_cmi_checked_digest:string ->
  import_routes:string list ->
  (owner, string) result

val owner_cmi_full_key : unit_name:string -> self_crc:string -> content_receipt:string ->
  imports:(string * string option) list -> string

val carrier :
  source_claim:string ->
  carrier_path:string ->
  carrier_uid:string ->
  owner:owner ->
  constructor_abi:string ->
  binder_abi:string ->
  compiler_jkind_abi:string ->
  compiler_representation:string ->
  (carrier, string) result

val base_reference : type_path:string -> type_uid:string -> base_owner:owner ->
  (base_reference, string) result
val carrier_with_base :
  base_reference:base_reference option -> source_claim:string ->
  carrier_path:string -> carrier_uid:string -> owner:owner -> constructor_abi:string ->
  binder_abi:string -> compiler_jkind_abi:string -> compiler_representation:string ->
  (carrier, string) result

val role :
  source_claim:string ->
  callable_path:string ->
  callable_uid:string ->
  callable_type_abi:string ->
  callable_mode_abi:string ->
  callable_owner:owner ->
  semantics_path:string ->
  semantics_uid:string ->
  semantics_type_abi:string ->
  semantics_mode_abi:string ->
  semantics_owner:owner ->
  carrier_uid:string ->
  carrier_owner:owner ->
  (role, string) result

val decode_carrier : string -> (carrier, string) result
val decode_role : string -> (role, string) result
val compare_carrier : carrier -> carrier -> int
val compare_role : role -> role -> int
