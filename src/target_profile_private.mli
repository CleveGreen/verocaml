type layout = private {
  layout_class : string;
  layout_abi : string;
  width : int;
  signed : bool;
}

type logical_bv_width_permission = private {
  schema : string;
  domain : string;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

type claim = private {
  schema : string;
  compiler_abi : string;
  toolchain_abi : string;
  target_identity : string;
  layouts : layout list;
  supported_widths : int list;
  logical_bv_width_permission : logical_bv_width_permission;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

type instance_claim = private {
  schema : string;
  profile_full_key : string;
  profile_checked_digest : string;
  target_identity : string;
  layout_class : string;
  layout_abi : string;
  width : int;
  signed : bool;
  logical_bv_width_permission_full_key : string;
  logical_bv_width_permission_checked_digest : string;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

val schema : string
val instance_schema : string
val logical_bv_width_permission_schema : string

val issue_logical_bv_width_permission :
  issuer_claim:string -> (logical_bv_width_permission, string) result

val logical_bv_width_permitted :
  logical_bv_width_permission -> Z.t -> bool

val layout :
  layout_class:string ->
  layout_abi:string ->
  width:int ->
  signed:bool ->
  (layout, string) result

val issue_claim :
  compiler_abi:string ->
  toolchain_abi:string ->
  target_identity:string ->
  layouts:layout list ->
  supported_widths:int list ->
  logical_bv_width_permission:logical_bv_width_permission ->
  issuer_claim:string ->
  (claim, string) result

val decode_claim : string -> (claim, string) result
val instantiate_claim : claim -> layout:layout -> (instance_claim, string) result
val decode_instance_claim :
  profile:claim -> string -> (instance_claim, string) result
val compare_claim : claim -> claim -> int
val equal_claim : claim -> claim -> bool
val compare_instance_claim : instance_claim -> instance_claim -> int
val equal_instance_claim : instance_claim -> instance_claim -> bool
