type public_surface = {
  public_type_names : string list;
  public_revealed_type_names : string list;
  public_callable_names : string list;
  public_external_type_constructors : Parametric_type.constructor list;
  public_symbolic_names : string list;
  public_logical_values :
    (string * Retained_interface_authority_private.logical_value) list;
}

val strict_candidate :
  require_public_interface:bool ->
  Cmt_input.implementation ->
  (string, Interface_specification_environment_private.error) result

val graph_order :
  Cmt_input.implementation list ->
  Cmt_input.implementation ->
  (Cmt_input.implementation list,
   Interface_specification_environment_private.error) result

val exact_import :
  owner:Cmt_input.implementation ->
  dependency:Cmt_input.implementation ->
  Cmt_input.import ->
  bool

val exact_imports :
  Cmt_input.implementation -> Cmt_input.implementation -> bool

val retained_authority_identity_is_exact : Cmt_input.implementation -> bool

val embedded_public_surface :
  unit_name:string ->
  Cmt_input.implementation ->
  (public_surface, Interface_specification_environment_private.error) result

val surface_has_type : public_surface -> Sst.type_id -> bool
val surface_reveals_type : public_surface -> Sst.type_id -> bool
val public_typ :
  public_surface -> Parametric_type.binder list -> Sst.typ -> bool
val public_function : public_surface -> Sst.function_id -> bool
val surface_type_kind_is_public :
  public_surface -> Parametric_type.binder list -> Sst.type_kind -> bool
val public_callable_descriptor :
  ?imported_specification_call:(Sst.expression -> bool) ->
  public_surface -> Sst_validation.callable_descriptor -> bool

module For_testing : sig
  val reset_strict_candidate_entries : unit -> unit
  val strict_candidate_entries : unit -> int
end
