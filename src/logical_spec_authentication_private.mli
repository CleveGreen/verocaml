type call_target = Logical_spec_capability_private.call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of Logical_spec_capability_private.model_capability
  | Opaque_recursive
  | Unsupported

type permit

type authorization =
  | Ordinary of permit
  | Strict of
      Logical_spec_capability_private.permit
      * Sst_validation.validated_program

val authenticate :
  classify:(Sst.function_id -> call_target) ->
  Sst.function_definition ->
  permit option

val find_definition :
  permit -> Sst.function_id -> Sst.function_definition option

val classify_definition :
  excluded:(Sst.function_definition -> bool) ->
  Sst.function_definition ->
  call_target

val classify_invariant_call :
  imported:(Sst.function_id -> bool) ->
  excluded:(Sst.function_definition -> bool) ->
  models:Logical_spec_capability_private.model_capability list ->
  (int * Sst_validation.callable_descriptor) list ->
  Sst.function_id ->
  call_target

module type Invariant_source = sig
  type handle
  type environment

  val handles : environment -> handle list
  val model_callable : handle -> Sst.function_id
  val public_operations :
    handle -> (Sst.function_id * Sst.abstract_operation_role) list
  val find_for_operation :
    environment ->
    Sst.function_id ->
    (handle * Sst.abstract_operation_role) option
end

module Make_invariant_policy (Source : Invariant_source) : sig
  val materialization_decisions :
    imports:Imported_callable.registration option ->
    validated:Sst_validation.validated_program ->
    invariants:Source.environment ->
    frozen:(Sst.function_id -> 'a option) ->
    owned:(Sst.function_definition -> 'b option) ->
    (Source.handle * bool * string option) list

  val classify :
    imports:Imported_callable.registration option ->
    validated:Sst_validation.validated_program ->
    invariants:Source.environment ->
    models:Logical_spec_capability_private.model_capability list ->
    (int * Sst_validation.callable_descriptor) list ->
    Sst.function_id ->
    call_target
end

val excluded_contract_root :
  Imported_callable.registration option -> Sst.function_definition -> bool

val authenticate_expression :
  classify:(Sst.function_id -> call_target) -> Sst.expression -> permit option

val authorize_definition :
  authorization ->
  classify:(Sst.function_id -> call_target) ->
  Sst.function_id ->
  result_type:Sst.typ ->
  (Logical_spec_capability_private.authorized_call option, string) result

val is_strict : authorization -> bool
