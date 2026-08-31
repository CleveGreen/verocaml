type category =
  | Project_materialization
  | Project_environment_missing
  | Project_environment_provenance_mismatch
  | Dune_build
  | Selected_cmt_discovery
  | Selected_cmt_load
  | Verifier_outcome
  | Expectation_mismatch
  | Process_protocol
  | Runner_duplicate_identity
  | Runner_malformed_identity
  | Runner_unknown_identity
  | Runner_selector_protocol
  | Runner_internal

type t

val make : category -> string -> t
val category : t -> category
val message : t -> string
val category_name : category -> string
val to_string : t -> string
