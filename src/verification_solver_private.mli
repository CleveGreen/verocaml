type preflight
type threaded
type prepared_function

val preflight :
  solver_policy:Solver_policy_private.t ->
  Sst.program ->
  (preflight, Verification_pipeline.setup_error) result

val termination_obligations : preflight -> int
val terminal_result : preflight -> Solver_backend.obligation_result option
val preservation_capabilities :
  preflight -> Recursive_spec_preservation.capability list
val proof_entry_activations :
  preflight -> Sst.function_id -> Spec_unfolding.activation list

val configure :
  solver_policy:Solver_policy_private.t ->
  preflight ->
  (Verification_pipeline.solve, Verification_pipeline.setup_error) result

val configure_threaded :
  solver_policy:Solver_policy_private.t ->
  preflight ->
  (threaded, Verification_pipeline.setup_error) result

val prepare_function :
  threaded ->
  source_ordinal:int ->
  Verification_pipeline.solve_request ->
  (prepared_function, string) result

val worker_request :
  prepared_function -> Function_vc_worker_private.request

val commit_function :
  prepared_function ->
  Function_vc_worker_private.result ->
  (Solver_backend.obligation_result list, string) result

module For_testing : sig
  val force_recursive_local_inconclusive : bool -> unit
  val recursive_retry_rlimit : int option -> unit
  val inject_preparation_error : int option -> unit
  val observe_local_counter_commits :
    (canonical_index:int ->
    attempt_index:int ->
    Z3_bridge.counters ->
    unit) ->
    (unit -> 'a) ->
    'a
end
