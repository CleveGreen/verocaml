type role = Root | Dependency

type artifact

type plan

type error

val artifact :
  role:role ->
  cmt:string ->
  cmi:string ->
  Cmt_input.implementation ->
  artifact

val plan : artifact list -> (plan, error) result
val roots : plan -> artifact list
val dependencies_for_root : plan -> artifact -> artifact list
val dependencies : plan -> artifact list
val skipped : plan -> artifact list
val role : artifact -> role
val cmt : artifact -> string
val cmi : artifact -> string
val implementation : artifact -> Cmt_input.implementation
val unit_name : artifact -> string
val error_unit_name : error -> string option
val error_message : error -> string
