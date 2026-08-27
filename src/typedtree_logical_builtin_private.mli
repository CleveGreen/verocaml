(** Structural authentication for compiler-typed logical-builtin carriers.

    This interface assigns no logical semantics. *)

type kind = Call_requires | Call_ensures | Forall | Exists
type t

val authenticate :
  artifact:Typedtree_adapter_issuance_private.proof_capture_artifact option ->
  source_file:string ->
  canonical_marker:(Path.t -> bool) ->
  Typedtree.expression ->
  (t option, string) result

val kind : t -> kind
val span : t -> Location.t
val callback_application : t -> Typedtree.expression option
val callback_result : t -> Typedtree.expression option
val quantifier : t -> Typedtree.expression option

type 'error lower_services = {
  lower_expression :
    Typedtree.expression -> (Sst.expression, 'error) result;
  application_binding :
    Typedtree.expression ->
    ((Typedtree_callback_private.application * Sst.callback_binding), 'error)
    result;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
  unsupported_quantifier : Location.t -> 'error;
}

type projection = [ `Call | `Requires | `Ensures ]

val lower_application :
  'error lower_services ->
  projection ->
  Typedtree.expression ->
  Typedtree.expression option ->
  (Sst.expression, 'error) result

val lower :
  'error lower_services ->
  expression:Typedtree.expression ->
  t ->
  (Sst.expression, 'error) result

type proof_capture_manifest_slot = {
  proof_capture_slot_name : string;
  proof_capture_slot_start : int;
  proof_capture_slot_end : int;
}

type proof_capture_kind =
  | Captured_proof_region
  | Captured_local_assert of {
      assertion_ordinal : int;
      predicate_start : int;
      predicate_end : int;
    }

type proof_capture_manifest = {
  proof_capture_callable_name : string;
  proof_capture_binding_start : int;
  proof_capture_binding_end : int;
  proof_capture_body_start : int;
  proof_capture_body_end : int;
  proof_capture_region_start : int;
  proof_capture_region_end : int;
  proof_capture_slots : proof_capture_manifest_slot list;
  proof_capture_kind : proof_capture_kind;
}

type local_assertion_discovery =
  | Builtin_assertion_discovery of {
      source : Typedtree.expression;
      predicate : Typedtree.expression;
      keyword_location : Location.t;
    }
  | Retained_assertion_discovery of proof_capture_manifest

type issued_proof_capture = {
  proof_capture_token : unit ref;
  proof_capture_artifact : Typedtree_adapter_issuance_private.proof_capture_artifact;
  proof_capture_function : Sst_callback_private.top_function;
  proof_capture_manifest : proof_capture_manifest;
  proof_capture_bindings : Sst.binding list;
  proof_capture_shadow_idents : Ident.t list;
  mutable proof_capture_program : Sst.program option;
  mutable proof_capture_expression : Sst.expression option;
}

type issued_local_assertion = {
  local_assertion_token : unit ref;
  local_assertion_program : Sst.program;
  local_assertion_definition : Sst.function_definition;
  local_assertion_expression : Sst.expression;
  local_assertion_parent_region : Sst.expression option;
  local_assertion_source : local_assertion_static_source;
}

and local_assertion_static_source =
  | Retained_ppx_assertion of proof_capture_manifest
  | Compiler_builtin_assertion of {
      builtin_source_expression : Typedtree.expression;
      builtin_predicate_expression : Typedtree.expression;
      builtin_keyword_location : Location.t;
      builtin_source_location : Location.t;
      builtin_predicate_location : Location.t;
      builtin_assertion_ordinal : int;
    }

type pending_builtin_assertion = {
  pending_builtin_token : unit ref;
  pending_builtin_program : Sst.program;
  pending_builtin_definition : Sst.function_definition;
  pending_builtin_expression : Sst.expression;
  pending_builtin_parent_region : Sst.expression option;
  pending_builtin_source : local_assertion_static_source;
  pending_builtin_program_snapshot : string;
}

type lowered_builtin_assertion = {
  lowered_builtin_token : unit ref;
  lowered_builtin_function : Sst_callback_private.top_function;
  lowered_builtin_source : Sst_callback_private.planned_builtin_assertion;
  lowered_builtin_expression : Sst.expression;
}


type issued_proof_region = {
  proof_region_token : unit ref;
  proof_region_program : Sst.program;
  proof_region_definition : Sst.function_definition;
  proof_region_expression : Sst.expression;
  proof_region_manifest : proof_capture_manifest;
}

val proof_capture_issuer : unit ref
val issued_proof_captures : issued_proof_capture list ref
val proof_capture_issuance_count : int ref
val proof_capture_remapping_count : int ref
val proof_capture_sst_count : int ref
val local_assertion_sst_count : int ref
val local_assertion_issuer : unit ref
val issued_local_assertions : issued_local_assertion list ref
val pending_builtin_issuer : unit ref
val pending_builtin_assertions : pending_builtin_assertion list ref
val lowered_builtin_issuer : unit ref
val lowered_builtin_assertions : lowered_builtin_assertion list ref
val proof_region_issuer : unit ref
val issued_proof_regions : issued_proof_region list ref
val local_assertion_static_issuance_count : int ref

type assertion_authentication =
  program:Sst.program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  bool

val authenticate_proof_region : assertion_authentication
val authenticate_local_assertion : assertion_authentication
val authenticate_local_assertion_candidate : assertion_authentication
val is_builtin_local_assertion : assertion_authentication
val is_direct_exec_builtin_local_assertion : assertion_authentication

val local_assertion_parent_proof_region :
  program:Sst.program ->
  definition:Sst.function_definition ->
  expression:Sst.expression ->
  Sst.expression option

val issue_builtin_local_assertions :
  program:Sst.program -> (unit, string) result
