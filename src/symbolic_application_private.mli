type declaration
type 'argument t

val declare :
  marker_id:string ->
  declaration_index:int ->
  declaration_name:string ->
  canonical_path:string ->
  value_uid:string ->
  source_file:string ->
  compilation_identity:string ->
  declaration_span:Diagnostic.span ->
  type_binders:Parametric_type.binder list ->
  parameter_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (declaration, string) result

val create :
  declaration ->
  type_arguments:Parametric_type.t list ->
  arguments:'argument list ->
  argument_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  ('argument t, string) result

val rebase_declaration :
  declaration ->
  declaration_index:int ->
  declaration_name:string ->
  canonical_path:string ->
  value_uid:string ->
  type_binders:Parametric_type.binder list ->
  parameter_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (declaration, string) result

val symbol_name_for_application :
  declaration ->
  type_arguments:Parametric_type.t list ->
  argument_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (string, string) result

val backend_head_for_instantiation :
  declaration ->
  type_arguments:Parametric_type.t list ->
  argument_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (string, string) result

val map_arguments : ('a -> 'b) -> 'a t -> 'b t
val replace_arguments : 'b list -> 'a t -> ('b t, string) result
val map_types : (Parametric_type.t -> Parametric_type.t) -> 'a t -> ('a t, string) result
val validate : argument_type:('a -> Parametric_type.t) -> 'a t -> (unit, string) result
val declaration : 'a t -> declaration
val declaration_name : declaration -> string
val declaration_index : declaration -> int
val canonical_path : declaration -> string
val value_uid : declaration -> string
val source_file : declaration -> string
val compilation_identity : declaration -> string
val declaration_span : declaration -> Diagnostic.span
val marker_id : declaration -> string
val type_binders : declaration -> Parametric_type.binder list
val parameter_types : declaration -> Parametric_type.t list
val declaration_result_type : declaration -> Parametric_type.t
val declaration_identity_material : declaration -> string
val declaration_abi_material :
  canonical_path:string ->
  value_uid:string ->
  type_binders:Parametric_type.binder list ->
  parameter_labels:string list ->
  parameter_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (string, string) result
val type_arguments : 'a t -> Parametric_type.t list
val arguments : 'a t -> 'a list
val argument_types : 'a t -> Parametric_type.t list
val result_type : 'a t -> Parametric_type.t
val span : 'a t -> Diagnostic.span
val identity_material : 'a t -> string
val identity_digest : 'a t -> string
val symbol_name : 'a t -> string
val backend_head : 'a t -> string
val same_declaration : declaration -> declaration -> bool
val same_head : 'a t -> 'b t -> bool
val is_nullary : 'a t -> bool
val to_string : ('a -> string) -> 'a t -> string
