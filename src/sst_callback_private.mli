(** Exact validation for direct callback IR edges. *)

val same_binding : Sst.callback_binding -> Sst.callback_binding -> bool

val validate_application :
  known:Sst.callback_binding list ->
  Sst.callback_application ->
  result_type:Sst.typ ->
  (unit, string) result

val split_direct_arguments :
  substitutions:(Parametric_type.binder * Parametric_type.t) list ->
  Sst.parameter list ->
  Sst.call_argument list ->
  ((Sst.value_parameter * (string option * Sst.expression)) list, string) result

val value_parameters : Sst.parameter list -> Sst.value_parameter list
val has_callback_parameters : Sst.parameter list -> bool
val has_callback_arguments : Sst.call_argument list -> bool

val expression_children : Sst.expression -> Sst.expression list

val validate_expression :
  known:Sst.callback_binding list ->
  stage:Sst.expression_stage ->
  Sst.expression ->
  (Sst.expression list, string) result


(** Stable callback relation identities for SST/VIR lowering. *)

val requires_relation : Sst.callback_binding -> string
val ensures_relation : Sst.callback_binding -> string
val precondition_name :
  span:(Sst.span -> string) -> Sst.callback_binding -> Sst.span -> string

val has_callbacks : Sst.program -> bool

val bindings_in_definition :
  Sst.function_definition -> Sst.callback_binding list

val authenticate_program :
  compilation_identity:Callback_certificate_private.compilation_identity ->
  session:unit ref ->
  Sst.program ->
  (unit, string) result

val authenticate_implementation :
  implementation:Cmt_input.implementation ->
  session:unit ref ->
  Sst.program ->
  (unit, string) result

val authenticated_captures :
  Sst.program ->
  Sst.function_definition ->
  ( (Callback_certificate_private.capture * Sst.binding) list,
    Sst.span * string )
  result

val authenticated_local_binding :
  Sst.program ->
  Sst.function_definition ->
  (Sst.callback_binding option, Sst.span * string) result

val local_scope_type_binders :
  Sst.program ->
  Sst.function_definition ->
  (Parametric_type.binder list option, Sst.span * string) result

type local_callable_instance = {
  definition : Sst.function_definition;
  source_paths : Path.t list;
  binding_uid : string;
  relation_identity : string;
}

val local_callable_instances :
  Sst.program ->
  owners:(Sst.function_id * Sst.span * Path.t list) list ->
  (local_callable_instance list, string) result

type authenticated_spec_carrier = {
  definition_body : Typedtree.expression;
  witness_location : Location.t;
  recursive_visibility : [ `Opaque | `Revealed ] option;
}

type top_function_kind =
  | Top_exec
  | Top_spec of authenticated_spec_carrier
  | Top_type_invariant of authenticated_spec_carrier
  | Top_recursive_spec of authenticated_spec_carrier
  | Top_proof of authenticated_spec_carrier
  | Top_external_specification of authenticated_spec_carrier
  | Top_external_body of Sst.verification_mode * authenticated_spec_carrier

type planned_builtin_assertion = {
  builtin_source_expression : Typedtree.expression;
  builtin_predicate_expression : Typedtree.expression;
  builtin_keyword_location : Location.t;
  builtin_source_location : Location.t;
  builtin_predicate_location : Location.t;
  builtin_assertion_ordinal : int;
}

type top_function = {
  ident : Ident.t;
  resolved_paths : Path.t list;
  function_id : Sst.function_id;
  value_binding : Typedtree.value_binding;
  rec_flag : Asttypes.rec_flag;
  function_kind : top_function_kind;
  type_substitutions : (int * Types.type_expr) list;
    parametric_type_binders : (int * Parametric_type.binder) list;
  proof_capture_hints : (Location.t * Types.type_expr) list;
  builtin_assertions : planned_builtin_assertion list;
  mutable semantic_parameters : (Sst.typ * string option) list option;
  mutable semantic_result_type : Sst.typ option;
}

val source_definition_body : top_function -> Typedtree.expression option
