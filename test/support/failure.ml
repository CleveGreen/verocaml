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

type t = { category : category; message : string }

let make category message = { category; message }
let category failure = failure.category
let message failure = failure.message

let category_name = function
  | Project_materialization -> "project-materialization"
  | Project_environment_missing -> "project-environment-missing"
  | Project_environment_provenance_mismatch ->
      "project-environment-provenance-mismatch"
  | Dune_build -> "dune-build"
  | Selected_cmt_discovery -> "selected-cmt-discovery"
  | Selected_cmt_load -> "selected-cmt-load"
  | Verifier_outcome -> "verifier-outcome"
  | Expectation_mismatch -> "expectation-mismatch"
  | Process_protocol -> "process-protocol"
  | Runner_duplicate_identity -> "runner-duplicate-identity"
  | Runner_malformed_identity -> "runner-malformed-identity"
  | Runner_unknown_identity -> "runner-unknown-identity"
  | Runner_selector_protocol -> "runner-selector-protocol"
  | Runner_internal -> "runner-internal"

let to_string failure =
  Printf.sprintf "%s: %s" (category_name failure.category) failure.message
