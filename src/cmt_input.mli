type import = {
  unit_name : string;
  crc : string option;
}

type implementation = private {
  (* Exact compiler metadata retained as an untrusted decoding result. *)
  metadata : Cmt_format.cmt_infos;
  embedded_interface_metadata : Cmi_format.cmi_infos_lazy option;
  raw_artifact_digest : string;
  filename : string;
  source_file : string;
  unit_name : string;
  interface_digest : string option;
  structure : Typedtree.structure;
  imports : import array;
  compiler_arguments : string array;
  source_digest : string option;
  build_directory : string;
  load_path_visible : string list;
  load_path_hidden : string list;
  embedded_interface : bool;
  explicit_interface : bool;
  interface_unit_name : string option;
  interface_implementation_unit_name : string option;
  interface_parameter_count : int;
  interface_imports : import array;
  implementation_family_markers : string list;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
  interface_family_markers : string list;
  interface_mode_signatures : (string * string option) list;
  interface_finite_signatures : (string * string option) list;
  declaration_dependency_count : int;
  has_implementation_shape : bool;
  identifier_occurrence_count : int;
}

val classify_annots :
  source_file:string ->
  Cmt_format.binary_annots ->
  (Typedtree.structure, Diagnostic.t) result

val load :
  ?int_size:int -> string -> (implementation, Diagnostic.t) result

val load_with_interface :
  ?int_size:int ->
  cmt:string ->
  cmi:string ->
  unit ->
  (implementation, Diagnostic.t) result

val retained_preprocessing : implementation -> bool
