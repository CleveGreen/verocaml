(** Private production verification-session driver shared by the CLI and focused
    verifier tests. *)

type solve_request = {
  definition : Sst.function_definition;
  receipt_source : bool;
  receipt_dependent : bool;
  proof_activation_routes : Verification_session.proof_activation_route list;
  execution : Vir.function_execution;
}

type solve =
  solve_request -> (Solver_backend.obligation_result list, string) result

type threaded_solve =
  | Threaded_solve : {
      prepare :
        source_ordinal:int -> solve_request -> ('prepared, string) result;
      worker_request :
        'prepared -> Function_vc_worker_private.request;
      commit :
        'prepared ->
        Function_vc_worker_private.result ->
        (Solver_backend.obligation_result list, string) result;
    }
      -> threaded_solve

type setup_error =
  | Internal_setup_error of string
  | Solver_configuration_error of Solver_backend.error

type status = Verified | Counterexample | Inconclusive | Incomplete_source

type completion = {
  status : status;
  vir : Vir.program;
  functions : int;
  obligations : int;
}

type error =
  | Setup_error of setup_error
  | Engine_error of Symbolic_executor_private.error
  | Solve_error of string

type post_validation_invariant_breach

val post_validation_invariant_breach :
  error -> post_validation_invariant_breach option

type report = {
  outcome : (completion, error) result;
  proof_evidence : Verification_proof_evidence_private.t option;
  counters : Verification_session.counters;
  session_destroyed : bool;
}

val run_validated :
  imports:Imported_callable.registration option ->
  numeric_bv_source:Numeric_bv_source_admission_private.source option ->
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  preflight:(unit -> (int, setup_error) result) ->
  proof_entry_activations:
    (unit -> Sst.function_id -> Spec_unfolding.activation list) ->
  configure_solver:(unit -> (solve, setup_error) result) ->
  on_function_commit:
    (Sst.function_definition ->
    Vir.function_execution ->
    Solver_backend.obligation_result list ->
    unit) ->
  on_result:(Solver_backend.obligation_result -> unit) ->
  (report, string) result

val run_validated_with_threads :
  threads:int ->
  imports:Imported_callable.registration option ->
  numeric_bv_source:Numeric_bv_source_admission_private.source option ->
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  validated:Sst_validation.validated_program ->
  invariants:Type_invariant.environment ->
  preflight:(unit -> (int, setup_error) result) ->
  proof_entry_activations:
    (unit -> Sst.function_id -> Spec_unfolding.activation list) ->
  configure_solver:(unit -> (threaded_solve, setup_error) result) ->
  on_function_commit:
    (Sst.function_definition ->
    Vir.function_execution ->
    Solver_backend.obligation_result list ->
    unit) ->
  on_result:(Solver_backend.obligation_result -> unit) ->
  (report, string) result

module For_testing : sig
  val reset_validated_pipeline_entries : unit -> unit
  val validated_pipeline_entries : unit -> int

  type frontier_event =
    | Materialized of int * string
    | Committed of int * string
    | Blocked of int * string

  val reset_scheduler_counts : unit -> unit
  val scheduler_creations : unit -> int
  val scheduler_stops : unit -> int
  val reset_function_observation : unit -> unit
  val set_function_spin_iterations : int -> unit
  val peak_active_functions : unit -> int
  val inject_materialization_error : int option -> unit
  val reset_frontier_events : unit -> unit
  val frontier_events : unit -> frontier_event list
end
