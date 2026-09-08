type declaration
type t

type error = {
  location : Location.t;
  message : string;
}

val empty : source_file:string -> t

val authenticate :
  source_file:string ->
  authenticated_source_text:string option ->
  artifact:Typedtree_adapter_issuance_private.proof_capture_artifact option ->
  compilation_identity:
    (Callback_certificate_private.compilation_identity, string) result option ->
  Typedtree.structure ->
  (t, error) result

val declarations : t -> declaration list
val find_binding : t -> Typedtree.value_binding -> declaration option
val binding : declaration -> Typedtree.value_binding
val body : declaration -> Typedtree.expression
val source_type : declaration -> Typedtree.core_type
val ident : declaration -> Ident.t
val resolved_paths : declaration -> Path.t list
val value_uid : declaration -> string
val marker_id : declaration -> string
val source_name : declaration -> string
val canonical_path : declaration -> string
val provider_unit : declaration -> string
val provider_interface : declaration -> string
val source_file : declaration -> string
val compilation_identity : declaration -> string
val source_body_digest : declaration -> string
val declaration_location : declaration -> Location.t

val authenticate_use :
  t ->
  declaration ->
  path:Path.t ->
  value_uid:string ->
  location:Location.t ->
  (unit, error) result
