type request
type source

type projection = private {
  input : Sst.expression;
  width : Bv_width.t;
  authority : Numeric_bv_projection_evidence_private.t;
}

module For_registry : sig
  (** Builds an imported request only from the actual descriptors admitted by
      the registry.  Law keys and callable UIDs are observations of those
      sealed values, never caller-supplied authority. *)
  val request :
    consumer_artifact_full_key:string ->
    registry_full_key:string ->
    target:Build_target_profile_private.instance ->
    projections:Numeric_bv_imported_law_evidence_private.t list ->
    (request, string) result
end

(** Adds the exact current provider and its authenticated mathematical-base
    providers to a target-specific request. No source declaration or retained
    claim is proof authority. *)
val with_local_provider :
  imported:request option ->
  implementation:Cmt_input.implementation ->
  base_providers:Cmt_input.implementation list ->
  target:Build_target_profile_private.instance ->
  (request option, string) result

val create :
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  registration:Imported_callable.registration option ->
  request ->
  (source option, string) result

(** Attempts to activate local source routes from exact successful scoped law
    closures. Pending, failed, cyclic, or incomplete roots activate nothing. *)
val observe_prior_law :
  source ->
  Numeric_prior_law_closure_private.coordinator ->
  definition:Sst.function_definition ->
  unit

val unsigned_modular_projection :
  source ->
  caller:Sst.function_definition ->
  occurrence:Sst.expression ->
  (projection option, string) result

(** Extra function-index ordering edges for the approved local ordinary-law
    bootstrap phase.  These edges schedule exact selected laws and their
    acyclic local call dependencies before source occurrences that may consume
    them.  They grant no authority; only successful scoped closure observation
    activates a projection. *)
val prior_law_scheduling_dependencies :
  source -> definition:Sst.function_definition -> int list

val full_key : source -> string

module For_testing : sig
  (** Pure observation of the exact-identity gate used before local source
      projection.  Equal spans or structurally similar copies are not enough. *)
  val original_logical_occurrence :
    caller:Sst.function_definition -> Sst.expression -> bool
end
