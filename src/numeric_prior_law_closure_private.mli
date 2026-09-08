type coordinator
type t

(** A scoped successful function closure recorded by the existing coordinator.
    It is neither whole-program completion nor export authority. *)
val matches :
  t ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  bool

val authorizes_definition : t -> Sst.function_definition -> bool

val function_evidence :
  t ->
  Sst.function_definition ->
  Verification_proof_evidence_private.function_evidence option

(** Exact dependency evidence retained by the successful ordinary closure.
    Trusted leaves remain explicit so downstream source authority does not
    relabel a checked root as wholly proof-derived. *)
val dependency_evidence :
  t -> Numeric_proof_closure_walk_private.dependency_evidence list

val full_key : t -> string

module For_pipeline : sig
  val create :
    implementation:Cmt_input.implementation ->
    program:Sst.program ->
    validated:Sst_validation.validated_program ->
    coordinator

  val observe :
    coordinator ->
    definition:Sst.function_definition ->
    execution:Vir.function_execution ->
    Solver_backend.obligation_result list ->
    unit

  val complete :
    coordinator -> root:Sst.function_definition -> (t, string) result

  val close : coordinator -> unit
end
