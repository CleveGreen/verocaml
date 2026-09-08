type reason =
  | Missing_local_identity of Sst.function_id
  | Missing_successful_evidence of Sst.function_id
  | Missing_broadcast_evidence of Sst.function_id
  | Unsupported_dependency of Sst.function_id
  | Logical_constant_context
  | Dependency_cycle of Sst.function_id

type t

type dependency_evidence = private {
  function_index : int;
  function_name : string;
  compiler_uid : string;
  trusted : bool;
  full_key : string;
}

val complete :
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  successful:(Sst.function_definition -> bool) ->
  evidence_for:
    (Sst.function_definition ->
    Verification_proof_evidence_private.function_evidence option) ->
  Sst.function_definition ->
  (t, reason) result

val authorizes_definition : t -> Sst.function_definition -> bool
val full_key : t -> string
val dependency_count : t -> int
val trusted_dependency_count : t -> int
val dependency_evidence : t -> dependency_evidence list
val reason_name : reason -> string
