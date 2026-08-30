(** Compiler-backed direct callback call lowering. *)

type direct_candidate = {
  ident : Ident.t;
  function_id : Sst.function_id;
  value_binding : Typedtree.value_binding;
  type_substitutions : (int * Types.type_expr) list;
  type_binders : Parametric_type.binder list;
}

type retained_proof_region = {
  proof_manifest_text : string;
  proof_shadow_parameters : Typedtree.function_param list;
  proof_region_application : Typedtree.expression;
}

val select_candidate :
  'candidate list -> 'candidate option

val has_callback_formal : direct_candidate -> bool

type 'error direct_lowering_services = {
  normalized_type :
    direct_candidate ->
    Location.t ->
    Types.type_expr ->
    (Parametric_type.t, 'error) result;
  optional_carrier :
    Location.t -> Parametric_type.t -> (Parametric_type.t, 'error) result;
  lower_expression :
    Typedtree.expression -> (Sst.expression, 'error) result;
  callback_actual :
    Callback_shape_private.t ->
    string option ->
    Typedtree.expression ->
    (Sst.call_argument, 'error) result;
  callback_candidate_contract : (unit, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  policy_error : Location.t -> string -> 'error;
  polymorphic_error : Location.t -> 'error;
  higher_order_error : Location.t -> 'error;
  span : Location.t -> Sst.span;
  current : Ident.t option;
}

val lower_direct_candidate :
  'error direct_lowering_services ->
  application:Typedtree.expression ->
  result_type:Sst.typ ->
  source_arguments:
    (Typedtree.arg_label * Typedtree.apply_arg) list ->
  direct_candidate ->
  (Sst.expression, 'error) result

type direct_clause = Callback_contract_private.execution_clause = {
  ordinal : int;
  span : Sst.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type ('obligation, 'path) evaluation = {
  obligations : 'obligation list;
  paths : 'path list;
}

type
  ( 'context,
    'state,
    'summary,
    'evaluated,
    'value,
    'function_ref,
    'assumption,
    'vc_kind,
    'symbol_role,
    'obligation,
    'error )
  direct_runtime = {
  evaluate :
    'context ->
    Sst.expression ->
    'state ->
    (('obligation, 'evaluated) evaluation, 'error) result;
  value : 'evaluated -> 'value;
  state : 'evaluated -> 'state;
  evaluated : 'value -> 'state -> 'evaluated;
  function_ref : 'context -> 'function_ref;
  function_name : 'context -> string;
  callback_environment :
    'context -> (Sst.callback_binding * Sst.callback_binding) list;
  definition : 'summary -> Sst.function_definition;
  requires : 'summary -> direct_clause list;
  ensures : 'summary -> direct_clause list;
  contract_context :
    'context ->
    Sst.function_definition ->
    (int * 'value) list ->
    (int * 'value) list option ->
    (Sst.callback_binding * Sst.callback_binding) list ->
    'context;
  environment : 'state -> (int * 'value) list;
  with_environment : 'state -> (int * 'value) list -> 'state;
  with_assumptions : 'state -> 'assumption list -> 'state;
  bind_pattern :
    string ->
    (int * 'value) list ->
    Sst.pattern ->
    'value ->
    (((int * 'value) list * 'assumption list), 'error) result;
  fresh_value :
    'state ->
    source_name:string ->
    role:'symbol_role ->
    span:Sst.span ->
    project:bool ->
    Sst.typ ->
    (('value * 'state), 'error) result;
  result_role : 'symbol_role;
  expect_boolean :
    string -> Sst.span -> 'value -> ('assumption, 'error) result;
  precondition_kind :
    Sst.function_definition -> direct_clause -> Sst.span -> 'vc_kind;
  emit_goal :
    'function_ref ->
    'vc_kind ->
    Sst.span ->
    'assumption ->
    'state ->
    'obligation * 'state;
  malformed : string -> Sst.span -> string -> 'error;
}

val evaluate_direct :
  ( 'context,
    'state,
    'summary,
    'evaluated,
    'value,
    'function_ref,
    'assumption,
    'vc_kind,
    'symbol_role,
    'obligation,
    'error )
  direct_runtime ->
  'context ->
  Sst.expression ->
  'summary ->
  Sst.call_form ->
  Sst.call_argument list ->
  'state ->
  (('obligation, 'evaluated) evaluation, 'error) result
