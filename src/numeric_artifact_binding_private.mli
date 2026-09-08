type facts = private {
  provider_artifact_full_key : string;
  provider_artifact_checked_digest : string;
  carrier_claim : Numeric_interface_claim_private.carrier;
  source_request : Numeric_source_claim_private.carrier;
  compiler_layout_abi : string;
  representation : Cmt_input.numeric_artifact_representation;
  binding_full_key : string;
  binding_checked_digest : string;
}

type t

(** Correlates the exact loaded provider and compiler-derived layout. This does
    not establish verified-provider completion, law, refinement or target authority. *)
val correlate :
  Cmt_input.implementation -> Numeric_interface_claim_private.carrier ->
  (t, string) result

val facts : t -> (facts, string) result
