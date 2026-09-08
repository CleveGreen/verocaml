type authority = Checked_implementation | Explicit_external_body
type t = private {
  law : Numeric_ghost_law_private.t;
  executable : Sst.function_definition;
  compiler_uid : string;
  interface_uid : string;
  authority : authority;
  trusted_dependency_keys : string list;
  full_key : string;
}

(** A completed, unconditional scalar runtime-view equivalence. Ordinary
    semantic trust, source visibility and numeric candidacy cannot issue it. *)
val admit : completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> validated:Sst_validation.validated_program ->
  law:Numeric_ghost_law_private.t -> Numeric_semantics_binding_private.t -> (t, string) result
