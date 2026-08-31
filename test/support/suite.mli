type case

val case :
  name:string ->
  expectation:Expectation.t ->
  (environment:Project_environment.t ->
   workspace:string ->
   (Outcome.t, Failure.t) result) ->
  case

val canonical_identity : suite_path:string -> case_name:string -> string
val validate_cases : suite_path:string -> case list -> (unit, Failure.t) result

val select :
  suite_path:string ->
  case list ->
  string ->
  (case, Failure.t) result

val run_cli :
  suite_path:string ->
  manifest:string ->
  expected_environment:Project_environment.expected ->
  case list ->
  unit

val validate_report : report:string -> archive:string -> (unit, string) result
