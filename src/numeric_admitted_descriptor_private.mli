type body_visibility = Visible | Opaque
type native_candidacy = Bounded_mathematical_view | Unavailable_layout

type t = private {
  law : Numeric_ghost_law_private.t;
  refinements : Numeric_runtime_refinement_private.t list;
  visibility : body_visibility;
  reveal : bool;
  inline : bool;
  lowering_candidate : native_candidacy;
  full_key : string;
}
type occurrence_authority = Ghost_semantics | Runtime_equivalence of Numeric_runtime_refinement_private.t

val complete : completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> carrier_origin:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  law:Numeric_ghost_law_private.t -> refinements:Numeric_runtime_refinement_private.t list ->
  Numeric_semantics_binding_private.t -> (t, string) result

(** Authenticates a real local AST occurrence and its retained stage. This grants
    no primitive operation, inlining, query construction or native lowering. *)
val authorize_occurrence : completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> validated:Sst_validation.validated_program ->
  descriptor:t -> caller:Sst.function_definition -> occurrence:Sst.expression ->
  (occurrence_authority, string) result

module For_registry : sig
  val authorize_occurrence : completion:Verification_driver_private.completion ->
    implementation:Cmt_input.implementation -> origin:Cmt_input.implementation ->
    carrier_origin:Cmt_input.implementation ->
    imported:Imported_callable.environment -> validated:Sst_validation.validated_program ->
    descriptor:t -> caller:Sst.function_definition -> occurrence:Sst.expression ->
    (occurrence_authority, string) result
end
