type mode = Concrete | Abstract
type selector
type child = private {
  ordinal : int;
  target : Build_target_profile_private.instance;
  original_full_key : string;
  parent_full_key : string;
  full_key : string;
  checked_digest : string;
}
type t = private {
  mode : mode;
  original : Numeric_original_obligation_private.t;
  target_full_keys : string list;
  children : child list;
  abstract_edge : string option;
  full_key : string;
  checked_digest : string;
}

(** The caller is the configuration owner, not source metadata. Exactly one
    explicit mode is required; discovery order never chooses coverage. *)
val select :
  capability:Build_target_profile_private.capability -> modes:mode list ->
  (selector, string) result

val create :
  capability:Build_target_profile_private.capability -> selector:selector ->
  registry:Numeric_ghost_registry_private.t ->
  report:Verification_driver_private.report ->
  original:Numeric_original_obligation_private.t ->
  coverage:Numeric_abstract_coverage_private.t option -> (t, string) result

val encode : t -> string

(** Reconstructs from the same authenticated inputs, then validates full parent,
    membership, edge order, ordinals and digests. No query or fallback is made. *)
val decode :
  capability:Build_target_profile_private.capability -> selector:selector ->
  registry:Numeric_ghost_registry_private.t ->
  report:Verification_driver_private.report ->
  original:Numeric_original_obligation_private.t ->
  coverage:Numeric_abstract_coverage_private.t option -> string -> (t, string) result
