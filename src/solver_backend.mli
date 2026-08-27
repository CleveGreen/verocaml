type config

type model_value = Integer of Z.t | Boolean of bool | Aggregate_identity of Z.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type inconclusive_reason =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type inconclusive = {
  configured_timeout_ms : int;
  configured_rlimit : int;
  reason : inconclusive_reason;
}

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive

type error =
  | Invalid_configuration of string
  | Malformed_vir of string
  | Backend_failure of string

type obligation_result = {
  obligation : Vir.obligation;
  outcome : outcome;
}

val config : timeout_ms:int -> (config, error) result
val config_with_rlimit :
  timeout_ms:int -> rlimit:int -> (config, error) result
val timeout_ms : config -> int
val rlimit : config -> int

val solve_obligation :
  config -> Vir.obligation -> (outcome, error) result

val solve_in_order :
  config -> Vir.obligation list -> (obligation_result list, error) result

val error_to_string : error -> string

module For_testing : sig
  type controlled_failure = Unknown | Backend_failure

  val solve_after_translation :
    controlled_failure ->
    config ->
    Vir.obligation ->
    (outcome, error) result

  val translated_integer_term :
    function_index:int -> Vir.integer_term -> (string, error) result

  val parameter_snapshot : config -> string
  val reset_count : unit -> int
  val solver_creation_count : unit -> int
  val reset_solver_creation_count : unit -> unit
end
