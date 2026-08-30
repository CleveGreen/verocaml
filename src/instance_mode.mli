type environment

type unannotated_erased_call = {
  caller : Sst.function_id;
  callee : Sst.function_id;
  callee_mode : Sst.verification_mode;
}

type error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  message : string;
  unannotated_erased_call : unannotated_erased_call option;
}

val to_diagnostic : error -> Diagnostic.t

val prepare :
  structure:Typedtree.structure -> program:Sst.program -> unit

val seal :
  Cmt_input.implementation -> Sst.program -> (unit, error) result

val has_sealed_registration : Sst.program -> bool
val validate : Sst.program -> (environment, error) result
val authenticated : environment -> bool
val snapshot_digest : environment -> string
val to_string : environment -> string

val binding_mode :
  environment -> Sst.function_id -> Sst.binding -> Sst.instance_mode

val expression_mode :
  environment -> Sst.function_id -> Sst.expression -> Sst.instance_mode

val expression_explicit :
  environment -> Sst.function_id -> Sst.expression -> bool

val pattern_mode :
  environment -> Sst.function_id -> Sst.pattern -> Sst.instance_mode

val pattern_explicit :
  environment -> Sst.function_id -> Sst.pattern -> bool

val field_mode :
  environment -> Sst.field_definition -> Sst.instance_mode

val field_explicit : environment -> Sst.field_definition -> bool

val formal_mode :
  environment ->
  Sst.function_definition ->
  int ->
  Sst.parameter ->
  Sst.instance_mode

val formal_has_closed_authority :
  environment -> Sst.function_definition -> int -> bool

val result_mode :
  environment -> Sst.function_definition -> Sst.instance_mode

val register_builtin_local_assertion :
  program:Sst.program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  predicate:Sst.expression ->
  direct_exec:bool ->
  unit

module For_testing : sig
  val reset_ghost_formal_flow_count : unit -> unit
  val ghost_formal_flow_count : unit -> int

  val reset_builtin_local_assertion_mode_observations : unit -> unit

  val builtin_local_assertion_mode_observations :
    unit ->
    (Sst.function_id * int * Sst.instance_mode * Sst.instance_mode) list

  val copied_descriptor_rejected : environment -> bool
end
