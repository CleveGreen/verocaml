type declaration
type t
type error = { location : Location.t; message : string }
type use_error_kind = Authentication | Declaration | Application
type use_error = {
  use_error_kind : use_error_kind;
  location : Location.t;
  message : string;
}

val empty : source_file:string -> t

val authenticate :
  source_file:string ->
  artifact:Typedtree_adapter_issuance_private.proof_capture_artifact option ->
  compilation_identity:
    (Callback_certificate_private.compilation_identity, string) result option ->
  resolves_to_spec_carrier:(Path.t -> bool) ->
  Typedtree.structure ->
  (t, error) result

val declarations : t -> declaration list
val find_binding : t -> Typedtree.value_binding -> declaration option
val binding : declaration -> Typedtree.value_binding
val definition_body : declaration -> Typedtree.expression
val ident : declaration -> Ident.t
val value_uid : declaration -> string
val marker_id : declaration -> string
val source_name : declaration -> string
val canonical_path : declaration -> string
val source_file : declaration -> string
val compilation_identity : declaration -> string
val declaration_location : declaration -> Location.t

val authenticate_use :
  t ->
  declaration ->
  path:Path.t ->
  value_uid:string ->
  location:Location.t ->
  (unit, error) result

val authenticate_candidate :
  logical:bool ->
  candidates:
    (Path.t ->
    ( 'owner
    * t
    * declaration
    * Sst.function_definition option )
    list) ->
  path:Path.t ->
  value_uid:string ->
  location:Location.t ->
  (('owner * Symbolic_application_private.declaration), use_error) result

val identifier_expression :
  Symbolic_application_private.declaration ->
  result_type:Sst.typ ->
  span:Sst.span ->
  (Sst.expression, string) result

val application_expression :
  Symbolic_application_private.declaration ->
  result_type:Sst.typ ->
  span:Sst.span ->
  Sst.expression ->
  (Sst.expression, string) result
