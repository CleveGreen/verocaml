type config = {
  timeout_ms : int;
  model : bool;
}

type model_value =
  | Integer of Z.t
  | Boolean of bool
  | Aggregate_identity of Z.t
  | Bit_vector of Bv_value.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type bv_model_binding = {
  projection_identity : string;
  width : Bv_width.t;
  value : Bv_value.t;
}

type inconclusive_reason : value mod contended portable =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive_reason

type bv_query_outcome =
  | Bv_verified
  | Bv_counterexample of bv_model_binding list
  | Bv_inconclusive of inconclusive_reason

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
  | Detached_integer of string * string
  | Detached_boolean of string * bool
  | Detached_aggregate of string * string
  | Detached_bit_vector of string * string * string

type vir_projection_metadata

val projection_symbol : vir_projection_metadata -> Vir.symbol
val projection_identity : vir_projection_metadata -> string
val projection_order : vir_projection_metadata -> int
val projection_width : vir_projection_metadata -> Bv_width.t option
val projection_width_reference : vir_projection_metadata -> string option

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
  (detached_query * vir_projection_metadata list, error) result

val detach_query : Logic_ir.query -> detached_query
val detach_bv_query : Logic_ir.query -> detached_query * string list

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

val solve_bv_query :
  ?controlled:controlled ->
  ?rlimit:int ->
  config ->
  Logic_ir.query ->
  (bv_query_outcome, error) result

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

module For_testing : sig
  type defect =
    | Mixed_bv_add_widths
    | Omitted_bv_capability
    | Wrong_int_to_bv_operand
    | Bv_projection_width_mismatch

  val malformed_detached_bv_query :
    width:Bv_width.t ->
    other_width:Bv_width.t ->
    defect ->
    detached_query

  type model_fault : value mod contended portable =
    | Missing_evaluation
    | Wrong_sort
    | Non_numeral
    | Width_mismatch
    | Residue_text of string

  val solve_bv_query_with_model_fault :
    fault:model_fault ->
    ?controlled:controlled ->
    ?rlimit:int ->
    config ->
    Logic_ir.query ->
    (bv_query_outcome, error) result

  val solve_detached_query_local_with_model_fault :
    fault:model_fault ->
    controlled:controlled ->
    timeout_ms:int ->
    rlimit:int ->
    model:bool ->
    detached_query ->
    detached_attempt
    @@ portable

  val solve_vir_local_with_model_fault :
    fault:model_fault ->
    controlled:controlled ->
    rlimit:int ->
    ?requires:Logic_ir.feature list ->
    config ->
    Vir.obligation ->
    outcome local_attempt
end
