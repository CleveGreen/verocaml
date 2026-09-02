type import = {
  unit_name : string;
  crc : string option;
}

type interface_broadcast_group = {
  group_path : string;
  group_targets : string list;
}

type interface_symbolic_declaration = {
  symbolic_path : string;
  symbolic_uid : string;
  symbolic_marker : string;
  symbolic_type_digest : string;
  symbolic_typed_abi : string;
}

val import_receipt : import array -> string

type implementation = private {
  (* Exact compiler metadata retained as an untrusted decoding result. *)
  metadata : Cmt_format.cmt_infos;
  embedded_interface_metadata : Cmi_format.cmi_infos_lazy option;
  raw_artifact_digest : string;
  filename : string;
  source_file : string;
  unit_name : string;
  interface_digest : string option;
  interface_filename : string option;
  retained_authority_filename : string option;
  retained_authority : Retained_interface_authority_private.t option;
  retained_authority_receipt : string option;
  retained_authority_index : string option;
  interface_view_receipts : string list;
  structure : Typedtree.structure;
  imports : import array;
  compiler_arguments : string array;
  source_digest : string option;
  build_directory : string;
  load_path_visible : string list;
  load_path_hidden : string list;
  declared_artifact_directories : string list;
  embedded_interface : bool;
  explicit_interface : bool;
  interface_unit_name : string option;
  interface_implementation_unit_name : string option;
  interface_parameter_count : int;
  interface_imports : import array;
  implementation_family_markers : string list;
  implementation_family_issuers : string list;
  ppxlib_context : bool;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
  interface_family_markers : string list;
  interface_family_issuers : string list;
  interface_mode_signatures : (string * string option) list;
  interface_finite_signatures : (string * string option) list;
  interface_broadcast_declarations : string list;
  interface_broadcast_groups : interface_broadcast_group list;
  interface_symbolic_declarations : interface_symbolic_declaration list;
  interface_logical_sorts : Logical_sort_private.t list;
  interface_broadcasts : Retained_broadcast_private.interface_member list;
  declaration_dependency_count : int;
  has_implementation_shape : bool;
  identifier_occurrence_count : int;
}

val retained_ppx_artifact : implementation -> bool
val ordinary_ppx_artifact : implementation -> bool
val retained_preprocessing : implementation -> bool
val advertises_external_type_specification : implementation -> bool

val exact_import :
  owner:implementation -> dependency:implementation -> import -> bool
val exact_imports : implementation -> implementation -> bool
val retained_authority_identity_is_exact : implementation -> bool
val retained_authority_import : implementation -> import -> bool

val authenticated_retained_family_attribute :
  implementation -> Parsetree.attribute -> bool

val classify_annots :
  source_file:string ->
  Cmt_format.binary_annots ->
  (Typedtree.structure, Diagnostic.t) result

val load :
  ?int_size:int -> string -> (implementation, Diagnostic.t) result

val load_with_interface :
  ?int_size:int ->
  ?cmti:string ->
  ?vri:string ->
  ?vri_candidates:string list ->
  ?artifact_directories:string list ->
  ?implicit_authority_discovery:bool ->
  cmt:string ->
  cmi:string ->
  unit ->
  (implementation, Diagnostic.t) result

val normalize_value_path :
  implementation -> Location.t -> Env.t -> Path.t -> Path.t option

val broadcast_complete_receipt : implementation -> string

val emit_retained_interface_authority :
  ?int_size:int ->
  ?ordinary_cmis:string list ->
  ?artifact_directories:string list ->
  cmt:string ->
  cmi:string ->
  cmti:string ->
  output:string ->
  unit ->
  (unit, Diagnostic.t) result

val retained_authority_matches : vri:string -> cmt:string -> cmi:string -> bool

val erase_retained_interface_authority :
  input:string -> output:string -> (unit, Diagnostic.t) result
