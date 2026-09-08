type result
type error = Interface_specification_environment_private.error

val authenticate :
  external_targets:Cmt_input.implementation list ->
  solver_policy:Solver_policy_private.t ->
  dependencies:Cmt_input.implementation list ->
  consumer:Cmt_input.implementation ->
  (Interface_specification_environment_private.environment * Cmt_input.implementation,
   error) Stdlib.result

val verify :
  threads:int ->
  solver_policy:Solver_policy_private.t ->
  external_specifications:External_target_specification_private.environment option ->
  external_targets:Cmt_input.implementation list ->
  consumer:Cmt_input.implementation ->
  dependencies:Cmt_input.implementation list ->
  (result, error) Stdlib.result

val verify_for_numeric_target :
  threads:int ->
  solver_policy:Solver_policy_private.t ->
  external_specifications:External_target_specification_private.environment option ->
  external_targets:Cmt_input.implementation list ->
  target:Build_target_profile_private.instance ->
  consumer:Cmt_input.implementation ->
  dependencies:Cmt_input.implementation list ->
  (result, error) Stdlib.result

val driver : result -> Verification_driver_private.report
val numeric_registry : result -> Numeric_ghost_registry_private.t option
val numeric_target_sets : result -> Numeric_required_target_set_private.t list
val provenance : result -> (string * string * (string * string) list * (string * string) list) list
val error_unit_name : error -> string option
val error_message : error -> string
val error_diagnostic : error -> Diagnostic.t option
val error_is_internal : error -> bool
val error :
  ?unit_name:string ->
  ?diagnostic:Diagnostic.t ->
  ?internal:bool ->
  string ->
  ('a, error) Stdlib.result

module For_testing : sig
  val reset_provider_verification_entries : unit -> unit
  val provider_verification_entries : unit -> int
end
