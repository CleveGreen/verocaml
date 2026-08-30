type source

val with_load_path :
  visible:string list -> hidden:string list -> (unit -> 'a) -> 'a

type lowered = {
  descriptor : Parametric_adt.t;
  definition : Sst.type_definition;
  paths : Path.t list;
  fields : (Types.Uid.t * Sst.type_id * Sst.field_id) list;
  constructors : (Types.Uid.t * Sst.type_id * Sst.constructor_id) list;
}

val local_source :
  paths:Path.t list ->
  type_id:Sst.type_id ->
  Typedtree.type_declaration ->
  source

val external_source :
  paths:Path.t list ->
  type_id:Sst.type_id ->
  load_path_visible:string list ->
  load_path_hidden:string list ->
  Env.t ->
  Typedtree.type_declaration ->
  (source, string) result

val covers_path : source -> Path.t -> bool

val lower :
  span:(Location.t -> Diagnostic.span) ->
  aggregate:(Path.t -> Sst.type_id option) ->
  modalities:
    (Location.t ->
    Mode.Modality.Const.t ->
    (Sst.field_modalities, Diagnostic.t) result) ->
  source list ->
  (lowered list, Diagnostic.t) result

val find_by_path : lowered list -> Path.t -> lowered option
val find_by_constructor :
  lowered list -> Parametric_type.constructor -> lowered option

val instantiate_field_type :
  lowered list ->
  application:Sst.typ ->
  Sst.field_id ->
  (Sst.typ, string) result

val application_type_id : lowered list -> Sst.typ -> Sst.type_id option

val authenticate_direct_recursion :
  descriptor:Parametric_adt.t ->
  definition:Sst.function_definition ->
  measure:Sst.expression ->
  (unit, string) result
