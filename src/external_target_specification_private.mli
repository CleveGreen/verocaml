type environment
type candidate
type sealed_summary
type summary
type registration

val environment :
  consumer:Cmt_input.implementation ->
  targets:Cmt_input.implementation list ->
  (environment, string) result

val resolve_candidate :
  environment ->
  canonical_path:string ->
  value_uid:string ->
  (candidate, string) result
val candidate_path : candidate -> string
val candidate_uid : candidate -> string
val candidate_has_compatible_compiler_modes : candidate -> Types.type_expr -> bool

val seal :
  environment ->
  candidate:candidate ->
  resolve_application:(Path.t -> Parametric_adt.t option) ->
  wrapper:Sst.function_definition ->
  signature:Parametric_signature_private.t ->
  target_span:Diagnostic.span ->
  declaration_span:Diagnostic.span ->
  witness_span:Diagnostic.span ->
  wrapper_is_unannotated_exec:bool ->
  (sealed_summary, string) result

val link : sealed_summary -> Sst.target_link

val bind_definition :
  environment ->
  sealed_summary ->
  definition:Sst.function_definition ->
  (summary, string) result

val complete_summary :
  environment ->
  candidate:candidate ->
  resolve_application:(Path.t -> Parametric_adt.t option) ->
  wrapper_id:Sst.function_id ->
  type_binders:Parametric_type.binder list ->
  parameter_nodes:Typedtree.function_param list ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  terminal_arguments:(string option * Sst.expression) list ->
  result_type:Sst.typ ->
  target_span:Diagnostic.span ->
  declaration_span:Diagnostic.span ->
  witness_span:Diagnostic.span ->
  wrapper_is_unannotated_exec:bool ->
  (Sst.function_definition, string) result

val find_summary :
  environment -> canonical_path:string -> value_uid:string -> summary option

val definition : summary -> Sst.function_definition
val signature : summary -> Parametric_signature_private.t
val canonical_path : summary -> string
val value_uid : summary -> string
val witness_span : summary -> Diagnostic.span
val call_is_after_summary : summary -> Diagnostic.span -> bool

val has_mode_bearing_syntax : Typedtree.value_binding -> bool

val adopt_imported :
  environment ->
  provider_unit:string ->
  provider_interface:string ->
  definition:Sst.function_definition ->
  signature:Parametric_signature_private.t ->
  target_link:Sst.target_link ->
  (unit, string) result

val adopted_definitions : registration -> Sst.function_definition list

val register :
  environment ->
  consumer:Cmt_input.implementation ->
  program:Sst.program ->
  (registration option, string) result

val invalidate : registration -> unit

val authenticates_definition :
  registration ->
  program:Sst.program ->
  definition:Sst.function_definition ->
  Sst.target_link ->
  bool

val authenticate_call :
  registration ->
  program:Sst.program ->
  caller:Sst.function_definition ->
  callee:Sst.function_definition ->
  expression:Sst.expression ->
  (Parametric_signature_private.instantiated, string) result

val validate_call :
  registration option ->
  program:Sst.program ->
  functions:Sst.function_definition list ->
  caller_id:Sst.function_id ->
  callee:Sst.function_definition ->
  expression:Sst.expression ->
  (unit, string) result

val validate_definition :
  registration option ->
  program:Sst.program ->
  definition:Sst.function_definition ->
  expected_wrapper:bool ->
  Sst.target_link ->
  (unit, string) result

val signature_for_definition :
  registration ->
  program:Sst.program ->
  Sst.function_definition ->
  Parametric_signature_private.t option

val external_target_identity :
  registration ->
  program:Sst.program ->
  Sst.function_definition ->
  Sst.target_link option

module For_testing : sig
  val compiler_target_modes_are_exact_default :
    parameter_modes:Mode.Alloc.lr list ->
    result_modes:Mode.Alloc.lr list ->
    bool

  val compiler_callable_modes_are_compatible :
    target_parameter_modes:Mode.Alloc.lr list ->
    target_result_modes:Mode.Alloc.lr list ->
    wrapper_parameter_modes:Mode.Alloc.lr list ->
    wrapper_result_modes:Mode.Alloc.lr list ->
    bool

  val live_registrations : unit -> int
  val reset_live_registrations : unit -> unit
end
