open Parsetree

val rewrite_structure :
  keep_ghost:bool ->
  family_attribute:(Location.t -> attribute) ->
  structure ->
  structure
val rewrite_signature : keep_ghost:bool -> signature -> signature

val reject_misplaced_attribute : attribute -> unit

val rewrite_expression :
  keep_ghost:bool ->
  map:(expression -> expression) ->
  expression ->
  expression option
