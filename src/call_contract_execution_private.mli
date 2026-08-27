type value = Logical_spec_evaluation_private.value
type environment = (int * value) list

type clause = Callback_contract_private.execution_clause = {
  ordinal : int;
  span : Diagnostic.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type call_summary = {
  definition : Sst.function_definition;
  requires : clause list;
  ensures : clause list;
  assertions : clause list;
  executable_body : Sst.expression;
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
    'obligation,
    'error )
  runtime = {
  evaluate :
    'context ->
    Sst.expression ->
    'state ->
    (('obligation, 'evaluated) evaluation, 'error) result;
  value : 'evaluated -> value;
  state : 'evaluated -> 'state;
  evaluated : value -> 'state -> 'evaluated;
  function_ref : 'context -> Vir.function_ref;
  logical : 'context -> bool;
  callback_environment :
    'context -> (Sst.callback_binding * Sst.callback_binding) list;
  find_summary : 'context -> Sst.function_id -> 'summary option;
  definition : 'summary -> Sst.function_definition;
  requires_clauses : 'summary -> clause list;
  ensures_clauses : 'summary -> clause list;
  contract_context :
    'context ->
    Sst.function_definition ->
    environment ->
    environment option ->
    'context;
  environment : 'state -> environment;
  with_environment : 'state -> environment -> 'state;
  with_assumptions : 'state -> Vir.boolean_term list -> 'state;
  bind_pattern :
    string ->
    environment ->
    Sst.pattern ->
    value ->
    ((environment * Vir.boolean_term list), 'error) result;
  fresh_value :
    'state ->
    source_name:string ->
    role:Vir.symbol_role ->
    span:Sst.span ->
    project:bool ->
    Sst.typ ->
    ((value * 'state), 'error) result;
  expect_boolean :
    string -> Sst.span -> value -> (Vir.boolean_term, 'error) result;
  emit_goal :
    Vir.function_ref ->
    Vir.vc_kind ->
    Sst.span ->
    Vir.boolean_term ->
    'state ->
    'obligation * 'state;
  malformed : string -> Sst.span -> string -> 'error;
  record : Vir.reached_callback_call -> unit;
}

val evaluate_contexts :
  ('input -> (('obligation, 'output) evaluation, 'error) result) ->
  'input list ->
  (('obligation, 'output) evaluation, 'error) result

val relation_argument :
  value -> (Vir.recursive_spec_argument, string) result

val callback_result : value -> (Vir.result_value, string) result

val call_precondition :
  Sst.function_definition -> clause -> Sst.span -> Vir.vc_kind

val instantiate_summary :
  call_summary -> Sst.typ list -> (call_summary, string) result

val evaluate_callback :
  ( 'context,
    'state,
    'summary,
    'evaluated,
    'obligation,
    'error )
  runtime ->
  'context ->
  Sst.expression ->
  'state ->
  (('obligation, 'evaluated) evaluation, 'error) result

val application :
  Sst.callback_binding ->
  Vir.recursive_spec_argument list ->
  Sst.span ->
  Vir.callback_application

val requires :
  Sst.callback_binding ->
  Vir.recursive_spec_argument list ->
  Sst.span ->
  Vir.boolean_term

val ensures :
  Sst.callback_binding ->
  Vir.recursive_spec_argument list ->
  Vir.recursive_spec_argument ->
  Sst.span ->
  Vir.boolean_term
