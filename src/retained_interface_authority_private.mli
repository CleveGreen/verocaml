type dependency = {
  dependency_unit : string;
  dependency_compiler_receipt : string option;
  dependency_interface_receipt : string;
  dependency_authority_receipt : string option;
}

type logical_value_class = Symbolic_value | Defined_value
type logical_value_visibility = Symbolic_opaque | Defined_opaque | Defined_revealed

type logical_value = {
  logical_value_path : string;
  logical_value_uid : string;
  logical_value_marker : string;
  logical_value_typed_abi : string;
  logical_value_class : logical_value_class;
  logical_value_visibility : logical_value_visibility;
  logical_value_descriptor_receipt : string;
}

type numeric_claims = {
  carrier_reconstruction_claims : string list;
  role_reconstruction_claims : string list;
}

type payload =
  | Broadcast_witnesses of Retained_broadcast_private.interface_member list
  | Logical_sorts of Logical_sort_private.t list
  | Logical_values of logical_value list
  | Numeric_claims of numeric_claims
  | Unknown_optional_section of {
      section_name : string;
      section_version : string;
      section_payload : string;
    }

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
val equal_known : t -> t -> bool
val index : t -> string
val broadcast_witnesses : t -> Retained_broadcast_private.interface_member list
val logical_sorts : t -> Logical_sort_private.t list
val logical_values : t -> logical_value list
val numeric_claims : t -> numeric_claims list
val logical_value_receipt :
  path:string ->
  uid:string ->
  marker:string ->
  typed_abi:string ->
  logical_value_class ->
  logical_value_visibility ->
  string
