type dependency = {
  dependency_unit : string;
  dependency_compiler_receipt : string option;
  dependency_interface_receipt : string;
  dependency_authority_receipt : string option;
}

type payload =
  | Broadcast_witnesses of Retained_broadcast_private.interface_member list
  | Logical_sorts of Logical_sort_private.t list

type t = {
  provider_unit : string;
  provider_origin : string;
  compiler_abi : string;
  cmi_receipt : string;
  cmi_self_crc : string;
  cmi_imports : (string * string option) list;
  cmi_identity : string;
  cmti_receipt : string;
  cmti_interface_digest : string option;
  cmti_imports : (string * string option) list;
  cmti_identity : string;
  ordinary_cmi_receipts : string list;
  dependencies : dependency list;
  payloads : payload list;
}

val extension : string
val encode : t -> string
val decode : string -> (t, string) result
val equal : t -> t -> bool
val index : t -> string
val broadcast_witnesses : t -> Retained_broadcast_private.interface_member list
val logical_sorts : t -> Logical_sort_private.t list
