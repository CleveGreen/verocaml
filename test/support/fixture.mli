type file = { path : string; contents : string }

type dune_project = {
  files : file list;
  libraries : string list;
  targets : string list;
  selected_units : string list;
}

type input

val dune_project : dune_project -> input

val single_source :
  module_name:string -> source:string -> libraries:string list -> input

val prepared_cmt :
  declared_dependencies:string list -> string -> (input, string) result

val lower_single_source :
  module_name:string -> source:string -> libraries:string list -> dune_project

val retained_providers :
  environment:Project_environment.t ->
  libraries:string list ->
  (Cmt_input.implementation list, Failure.t) result

val run :
  environment:Project_environment.t ->
  workspace:string ->
  input ->
  (Outcome.t, Failure.t) result
