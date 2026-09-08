type t = private {
  base_int_full_key : string;
  validated_base_int_digest : string;
  authenticated_base_int_artifact_binding_key : string;
}

(** Consumes an existing mathematical-Int receipt at its original authenticated
    provider. The base receipt's full key and digest are preserved unchanged;
    the outer binding is derived from the loaded artifact, never a source claim.
    This frozen reference does not establish a consumer import edge, a numeric
    law, runtime refinement or lowering authority. *)
val correlate :
  provider:Cmt_input.implementation ->
  logical_sort:Logical_sort_private.t ->
  (t, string) result

val resolve : providers:Cmt_input.implementation list ->
  Numeric_interface_claim_private.base_reference ->
  (Cmt_input.implementation * Logical_sort_private.t * t, string) result
