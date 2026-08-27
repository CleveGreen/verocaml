type config = {
  timeout_ms : int;
  model : bool;
}

type model_value =
  | Integer of Z.t
  | Boolean of bool
  | Aggregate_identity of Z.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type inconclusive_reason : value mod contended portable =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive_reason

type error =
  | Invalid_configuration of string
  | Unsupported_features of Logic_ir.feature list
  | Malformed_logic_ir of string
  | Malformed_vir of string
  | Backend_failure of string

val error_to_string : error -> string

type controlled : value mod contended portable =
  | Real
  | Force_unknown
  | Force_timeout
  | Force_backend_failure

type counters : value mod contended portable = {
  capability_resolutions : int;
  translations : int;
  contexts_created : int;
  solvers_created : int;
  solver_resets : int;
  contexts_cleaned : int;
  contexts_live : int;
  maximum_contexts_live : int;
  selected_logics : string list;
}

type detached_query : value mod contended portable

type detached_model_value : value mod contended portable =
  | Detached_integer of string
  | Detached_boolean of bool
  | Detached_aggregate of string

type detached_outcome : value mod contended portable =
  | Detached_verified
  | Detached_counterexample of detached_model_value option list
  | Detached_inconclusive of inconclusive_reason

type detached_attempt : value mod contended portable = {
  detached_result : (detached_outcome, string) result;
  detached_telemetry : counters;
}

type 'a local_attempt = {
  result : ('a, error) result;
  telemetry : counters;
}

val commit_local_counters : counters -> unit

val detach_vir :
  requires:Logic_ir.feature list ->
  Vir.obligation ->
  (detached_query * Vir.symbol list, error) result

val detach_query : Logic_ir.query -> detached_query

val solve_detached_query_local :
  controlled:controlled ->
  timeout_ms:int ->
  rlimit:int ->
  model:bool ->
  detached_query ->
  detached_attempt
  @@ portable

val solve_query :
  ?controlled:controlled ->
  ?rlimit:int ->
  config ->
  Logic_ir.query ->
  (outcome, error) result

val solve_query_local :
  controlled:controlled ->
  rlimit:int ->
  config ->
  Logic_ir.query ->
  outcome local_attempt

val preflight : Logic_ir.feature list -> (unit, error) result

val render_query :
  config -> Logic_ir.query -> (string * string, error) result

val solve_vir :
  ?controlled:controlled ->
  ?rlimit:int ->
  ?requires:Logic_ir.feature list ->
  config ->
  Vir.obligation ->
  (outcome, error) result

val solve_vir_local :
  controlled:controlled ->
  rlimit:int ->
  ?requires:Logic_ir.feature list ->
  config ->
  Vir.obligation ->
  outcome local_attempt

val render_vir :
  ?requires:Logic_ir.feature list ->
  config ->
  Vir.obligation ->
  (string * string, error) result

val diagnostic_snapshot : config -> Logic_ir.query -> string
val counters : unit -> counters
val reset_counters : unit -> unit
val version : unit -> int * int * int * string
