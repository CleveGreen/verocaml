type unavailable = private { callable_path : string; role : string; width : int; reason : string }
type t = private {
  implementation : Cmt_input.implementation;
  laws : Numeric_ghost_law_private.t list;
  refinements : Numeric_runtime_refinement_private.t list;
  descriptors : Numeric_admitted_descriptor_private.t list;
  unavailable : unavailable list;
  full_key : string;
}

(** Freeze at the original completed provider, while its compiler instances are
    live. Dependency artifacts come from the ordinary verified-library closure. *)
val complete :
  completion:Verification_driver_private.completion -> implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program -> dependencies:Cmt_input.implementation list ->
  declarations:Numeric_semantics_binding_private.t list -> (t, string) result

val complete_with_imports : completion:Verification_driver_private.completion -> implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program -> imported:Imported_callable.environment ->
  prerequisites:Numeric_ghost_law_private.t list -> declarations:Numeric_semantics_binding_private.t list -> (t, string) result
