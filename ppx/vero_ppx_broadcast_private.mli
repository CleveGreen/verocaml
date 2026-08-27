open Parsetree

val rewrite_structure : keep_ghost:bool -> structure -> structure

val reject_misplaced_attribute : attribute -> unit

val rewrite_expression :
  keep_ghost:bool ->
  map:(expression -> expression) ->
  expression ->
  expression option
