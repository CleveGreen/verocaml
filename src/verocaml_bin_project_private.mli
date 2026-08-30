val main :
  startup_classification:(unit, Diagnostic.t) result ->
  default_threads:(unit -> int) ->
  string array ->
  int

val verify_inventory :
  startup_classification:(unit, Diagnostic.t) result ->
  configuration:Verifier_service.configuration ->
  (Verifier_service.scope_role * string * string * Cmt_input.implementation) list ->
  int
