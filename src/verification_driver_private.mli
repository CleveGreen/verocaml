type completion
type report

type error =
  | Frontend_error of Diagnostic.t
  | Validation_error of Sst_validation.error
  | Invariant_error of Type_invariant.error
  | Provider_surface_error of string
  | Pipeline_error of Verification_pipeline.error
  | Internal_error of string

val run :
  ?rlimit:int ->
  timeout_ms:int ->
  allow_imported_opens:bool ->
  ?external_specifications:External_target_specification_private.environment ->
  ?imported:Imported_callable.environment ->
  Cmt_input.implementation ->
  (report, error) result

val run_with_policy :
  solver_policy:Solver_policy_private.t ->
  allow_imported_opens:bool ->
  ?allow_public_parametric_signatures:bool ->
  ?capture_numeric_obligations:bool ->
  ?numeric_native:Numeric_bv_source_admission_private.request ->
  ?validate_provider_surface:(Sst_validation.validated_program ->
    (unit, string) result) ->
  ?external_specifications:External_target_specification_private.environment ->
  ?imported:Imported_callable.environment ->
  Cmt_input.implementation ->
  (report, error) result

val run_with_threads :
  threads:int ->
  ?rlimit:int ->
  timeout_ms:int ->
  allow_imported_opens:bool ->
  ?external_specifications:External_target_specification_private.environment ->
  ?imported:Imported_callable.environment ->
  Cmt_input.implementation ->
  (report, error) result

val run_with_policy_and_threads :
  threads:int ->
  solver_policy:Solver_policy_private.t ->
  allow_imported_opens:bool ->
  ?allow_public_parametric_signatures:bool ->
  ?capture_numeric_obligations:bool ->
  ?numeric_native:Numeric_bv_source_admission_private.request ->
  ?external_specifications:External_target_specification_private.environment ->
  ?imported:Imported_callable.environment ->
  Cmt_input.implementation ->
  (report, error) result

val status : report -> Verification_pipeline.status
val semantic_sst : report -> string
val semantic_sst_lazy : report -> string Lazy.t
val vir : report -> Vir.program
val functions : report -> int
val obligations : report -> int
val results : report -> Solver_backend.obligation_result list
val counters : report -> Verification_session.counters
val validated : report -> Sst_validation.validated_program
val original_obligations : report -> Numeric_original_obligation_private.t list
val prior_law_evidence :
  report -> Numeric_prior_law_closure_private.coordinator option
val verified_completion : report -> completion option

val completion_matches :
  completion ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  bool

val provider_completion :
  completion -> Verified_provider_completion_private.t option

(** Evidence captured by the coordinator before session cleanup, available
    only for an authentic successful completion of the same program. *)
val proof_evidence : completion -> Verification_proof_evidence_private.t option

module For_testing : sig
  val reset_driver_entries : unit -> unit
  val driver_entries : unit -> int
end
