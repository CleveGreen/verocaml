type t = private {
  original_full_key : string;
  profile_full_key : string;
  target_full_keys : string list;
  trusted_dependencies : string list;
  full_key : string;
}

(** Identity specialization for an already verified, exclusively mathematical
    local proof context. Runtime-int, imported, heap and other target-sensitive
    contexts cannot acquire all-target coverage through this schema. *)
val complete :
  capability:Build_target_profile_private.capability ->
  consumer:Cmt_input.implementation ->
  report:Verification_driver_private.report ->
  Numeric_original_obligation_private.t -> (t, string) result
