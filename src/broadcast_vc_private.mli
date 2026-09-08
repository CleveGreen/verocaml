type inserted = {
  broadcast_id : string;
  theorem_function_id : Sst.function_id;
  type_vector : Parametric_type.t list;
  qid : string;
  skid : string;
  insertion_ordinal : int;
  trusted : bool;
  declaration_span : Diagnostic.span;
  witness_span : Diagnostic.span option;
  requires_count : int;
  ensures_count : int;
  selecting_paths : string list list;
}

type report = {
  active_declarations : int;
  trusted_broadcast_declarations : int;
  trusted_broadcast_uses : int;
  inserted : inserted list;
}

val make_binders :
  schema:Logic_quantifier_private.vector ->
  formals:(int * string * Parametric_type.t * Diagnostic.span) list ->
  sort_of_type:(Parametric_type.t -> (Vir.sort, string) result) ->
  ((int * Vir.symbol) list, string) result

val make_theorem_quantifier :
  sort_of_type:(Parametric_type.t -> (Vir.sort, string) result) ->
  schema:Logic_quantifier_private.vector ->
  binders:Vir.symbol list ->
  requires:Vir.boolean_term list ->
  ensures:Vir.boolean_term list ->
  trigger:Vir.application_term ->
  (Vir.boolean_quantifier, string) result

type ('state, 'value, 'error) lowering = {
  error : Diagnostic.span -> string -> 'error;
  sort_of_type : Parametric_type.t -> (Vir.sort, string) result;
  value_of_symbol :
    Parametric_type.t -> Vir.symbol -> ('value, 'error) result;
  initial_state : (int * 'value) list -> 'state;
  evaluate :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.boolean_term * 'state, 'error) result;
  evaluate_ensure :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.ensures_clause ->
    (Vir.boolean_term * 'state, 'error) result;
  evaluate_trigger :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.application_term * 'state, 'error) result;
}

val make_lowering :
  error:(Diagnostic.span -> string -> 'error) ->
  aggregate_of_type:(Parametric_type.t -> Vir.aggregate_type option) ->
  integer_value:(Vir.integer_term -> 'value) ->
  boolean_value:(Vir.boolean_term -> 'value) ->
  bit_vector_value:(Vir.bit_vector_term -> 'value) ->
  parametric_value:(Vir.parametric_term -> 'value) ->
  spec_function_value:(Parametric_type.t -> Vir.spec_function_term -> 'value) ->
  aggregate_value:(Vir.aggregate_term -> 'value) ->
  initial_state:((int * 'value) list -> 'state) ->
  evaluate:
    ((Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.boolean_term * 'state, 'error) result) ->
  evaluate_ensure:
    ((Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.ensures_clause ->
    (Vir.boolean_term * 'state, 'error) result) ->
  evaluate_trigger:
    ((Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.application_term * 'state, 'error) result) ->
  ('state, 'value, 'error) lowering

val make_evaluator_lowering :
  error:(Diagnostic.span -> string -> 'error) ->
  aggregate_of_type:(Parametric_type.t -> Vir.aggregate_type option) ->
  integer_value:(Vir.integer_term -> 'value) ->
  boolean_value:(Vir.boolean_term -> 'value) ->
  bit_vector_value:(Vir.bit_vector_term -> 'value) ->
  parametric_value:(Vir.parametric_term -> 'value) ->
  spec_function_value:(Parametric_type.t -> Vir.spec_function_term -> 'value) ->
  aggregate_value:(Vir.aggregate_term -> 'value) ->
  map_expression:
    ((Parametric_type.binder * Parametric_type.t) list ->
    Sst.expression ->
    Sst.expression) ->
  environment:('state -> 'environment) ->
  reset:((int * 'value) list -> 'state) ->
  context:('environment -> 'context) ->
  evaluate_formula:
    ('context ->
    Sst.expression ->
    'state ->
    (Vir.boolean_term * 'state, 'error) result) ->
  prepare_ensure:
    ('state -> Sst.pattern option -> ('state, 'error) result) ->
  restore:('environment -> 'state -> 'state) ->
  evaluate_trigger_formula:
    ('context ->
    Sst.expression ->
    'state ->
    (Vir.application_term * 'state, 'error) result) ->
  ('state, 'value, 'error) lowering

val build_quantifier :
  ('state, 'value, 'error) lowering ->
  Broadcast_declaration_private.theorem ->
  Parametric_type.t list ->
  (Vir.boolean_quantifier, 'error) result

val materialize :
  program:Sst.program ->
  function_id:Sst.function_id ->
  obligation:Vir.obligation ->
  build:
    (Broadcast_declaration_private.theorem ->
    Parametric_type.t list ->
    (Vir.boolean_quantifier, string) result) ->
  (Vir.obligation, string) result

val materialize_and_attach :
  program:Sst.program ->
  function_id:Sst.function_id ->
  descriptors:Parametric_adt.t list ->
  obligation:Vir.obligation ->
  build:
    (Broadcast_declaration_private.theorem ->
    Parametric_type.t list ->
    (Vir.boolean_quantifier, string) result) ->
  map_error:(string -> 'error) ->
  (Vir.obligation, 'error) result

val report : Vir.obligation -> report option
val pre_materialization_obligation : Vir.obligation -> Vir.obligation option
val base_obligation : Vir.obligation -> Vir.obligation
val transfer_report : from:Vir.obligation -> to_:Vir.obligation -> unit
val clear_program : Sst.program -> unit
val instance_cap : int
