val reserved : Parsetree.expression -> bool

val rewrite :
  map:(Parsetree.expression -> Parsetree.expression) ->
  Parsetree.expression ->
  Parsetree.expression option

type logical_scope

val create_logical_scope : unit -> logical_scope
val with_logical_payload : logical_scope -> (unit -> 'a) -> 'a
val with_ensures : logical_scope -> (unit -> 'a) -> 'a
val inside_ensures : logical_scope -> bool

val map_declaration_body :
  logical_scope ->
  role:string ->
  map:(Parsetree.expression -> Parsetree.expression) ->
  Parsetree.expression ->
  Parsetree.expression

val require_function_body :
  attribute:string -> Parsetree.expression -> unit

val rewrite_in_payload :
  logical_scope ->
  map:(Parsetree.expression -> Parsetree.expression) ->
  Parsetree.expression ->
  Parsetree.expression option

val map_let :
  reject_attributes:(Parsetree.attributes -> unit) ->
  map_binding:(Parsetree.value_binding -> Parsetree.value_binding) ->
  map_expression:(Parsetree.expression -> Parsetree.expression) ->
  lexical_bindings:string Location.loc list ref ->
  Parsetree.expression ->
  Asttypes.mutable_flag ->
  Asttypes.rec_flag ->
  Parsetree.value_binding list ->
  Parsetree.expression ->
  Parsetree.expression

val pattern_value_bindings :
  Parsetree.pattern -> string Location.loc list

val visible_parameter_bindings :
  Parsetree.function_param list -> string Location.loc list

val aliased_shadow : string Location.loc -> Parsetree.function_param
val mentioned_identifiers : Parsetree.expression -> string list

val map_function :
  visible_parameters:
    (Parsetree.function_param list -> string Location.loc list) ->
  lexical_bindings:string Location.loc list ref ->
  function_scopes:
    (Parsetree.expression * string Location.loc list) list ref ->
  map:(Parsetree.expression -> Parsetree.expression) ->
  Parsetree.expression ->
  Parsetree.function_param list ->
  Parsetree.expression ->
  Parsetree.expression

val rewrite_scoped_body :
  function_scopes:
    (Parsetree.expression * string Location.loc list) list ref ->
  lexical_bindings:string Location.loc list ref ->
  rewrite:
    (string Location.loc list ->
    Parsetree.expression ->
    Parsetree.expression) ->
  Parsetree.expression ->
  Parsetree.expression option
