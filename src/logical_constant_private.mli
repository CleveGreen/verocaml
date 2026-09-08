val issue_id :
  source:Typedtree_logical_constant_private.declaration ->
  constant_index:int ->
  constant_name:string ->
  Sst.logical_constant_id

val interface_abi :
  path:string ->
  uid:string ->
  Sst.logical_constant_definition ->
  (string, string) result

val make_definition :
  source:Typedtree_logical_constant_private.declaration ->
  constant_id:Sst.logical_constant_id ->
  type_binders:Parametric_type.binder list ->
  declared_type:Sst.typ ->
  body:Sst.staged_expression ->
  span:Diagnostic.span ->
  (Sst.logical_constant_definition, string) result

val seal :
  imported_definitions:Sst.logical_constant_definition list ->
  program:Sst.program ->
  (unit, string) result
val authenticate_program : Sst.program -> (unit, string) result
val authenticate_definition :
  Sst.logical_constant_definition -> (unit, string) result
val validate_instance_type : Sst.typ -> (unit, string) result
val dependency_closure :
  program:Sst.program ->
  Sst.logical_constant_definition ->
  ((string * string list), string) result
val remap_authenticated :
  definition:Sst.logical_constant_definition ->
  constant_id:Sst.logical_constant_id ->
  type_binders:Parametric_type.binder list ->
  trust_dependencies:string list ->
  map_type:(Sst.typ -> Sst.typ) ->
  map_expression:(Sst.expression -> Sst.expression) ->
  (Sst.logical_constant_definition, string) result
val find_definition :
  Sst.program ->
  Sst.logical_constant_id ->
  Sst.logical_constant_definition option

val validate_reference :
  program:Sst.program ->
  logical:bool ->
  expression_type:Sst.typ ->
  Sst.logical_constant_id ->
  type_arguments:Sst.typ list ->
  (unit, string) result

val destroy : Sst.program -> unit

module For_testing : sig
  val definition_rejected :
    program:Sst.program -> Sst.logical_constant_definition -> bool
end
