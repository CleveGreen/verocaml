val main :
  startup_classification:(unit, Diagnostic.t) result ->
  default_threads:(unit -> int) ->
  string array ->
  int
