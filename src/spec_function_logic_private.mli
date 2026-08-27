(** Deterministic VIR identities for first-order specification functions. *)

type value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of function_value

and function_value = {
  function_term : Vir.spec_function_term;
  function_arrow : Sst.typ;
  function_closure : function_closure;
}

and function_closure =
  | Abstract_function
  | Lambda_function of {
      lambda : Spec_function_sst_private.lambda;
      lambda_environment : (int * value) list;
    }
  | Named_function of {
      definition : Sst.function_definition;
      named_arguments : (string option * value) list;
      named_argument_types : Sst.typ list;
      named_type_arguments : Sst.typ list;
    }

val conditional :
  Vir.boolean_term ->
  function_value ->
  function_value ->
  (function_value, string) result

val binder : Parametric_type.t -> Parametric_type.binder
val is_binder_for : Parametric_type.t -> Parametric_type.binder -> bool

val of_symbol :
  arrow:Parametric_type.t ->
  Vir.symbol ->
  (Vir.spec_function_term, string) result

val closure :
  lambda:Spec_function_sst_private.lambda ->
  captures:Vir.recursive_spec_argument list ->
  (Vir.spec_function_term, string) result

val named :
  arrow:Parametric_type.t ->
  function_id:Sst.function_id ->
  type_arguments:Parametric_type.t list ->
  arguments:Vir.recursive_spec_argument list ->
  argument_types:Parametric_type.t list ->
  span:Diagnostic.span ->
  (Vir.spec_function_term, string) result

val application :
  arrow:Parametric_type.t ->
  function_:Vir.spec_function_term ->
  argument:Vir.recursive_spec_argument ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  (Vir.recursive_spec_argument Symbolic_application_private.t, string) result

val application_symbol_name :
  arrow:Parametric_type.t ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  (string, string) result

val is_application :
  'argument Symbolic_application_private.t -> Parametric_type.t option

val recursive_argument :
  error:(Diagnostic.span -> string -> 'error) ->
  span:Diagnostic.span ->
  value ->
  (Vir.recursive_spec_argument, 'error) result

val application_term :
  error:(Diagnostic.span -> string -> 'error) ->
  span:Diagnostic.span ->
  value ->
  (Vir.application_term, 'error) result

val value_of_application :
  aggregate_type:(Sst.typ -> Vir.aggregate_type option) ->
  error:(Diagnostic.span -> string -> 'error) ->
  span:Diagnostic.span ->
  Sst.typ ->
  Vir.recursive_spec_argument Symbolic_application_private.t ->
  (value, 'error) result

val sort_of_type :
  aggregate_type:(Sst.typ -> Vir.aggregate_type option) ->
  Sst.typ ->
  (Vir.sort, string) result

val quantified_value :
  aggregate_type:(Sst.typ -> Vir.aggregate_type option) ->
  error:(Diagnostic.span -> string -> 'error) ->
  Sst.binding ->
  (Vir.symbol * value, 'error) result

type ('context, 'state, 'error) axiom_services = {
  evaluate :
    'context -> Sst.expression -> 'state -> (value * 'state, 'error) result;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  environment : 'state -> (int * value) list;
  with_environment : 'state -> (int * value) list -> 'state;
  assume : 'state -> Vir.boolean_term list -> 'state;
  enter_definition : 'context -> Sst.function_definition -> 'context;
  bind_pattern :
    (int * value) list ->
    Sst.pattern ->
    value ->
    ((int * value) list, 'error) result;
  equality : value -> value -> Vir.boolean_term option;
  error : Diagnostic.span -> string -> 'error;
}

val lambda_value :
  environment:(int * value) list ->
  error:(Diagnostic.span -> string -> 'error) ->
  span:Diagnostic.span ->
  Spec_function_sst_private.lambda ->
  (value, 'error) result

val materialize_lambda_axiom :
  ('context, 'state, 'error) axiom_services ->
  context:'context ->
  span:Diagnostic.span ->
  function_value ->
  'state ->
  ('state, 'error) result

val materialize_named_axioms :
  ('context, 'state, 'error) axiom_services ->
  context:'context ->
  span:Diagnostic.span ->
  function_value ->
  'state ->
  ('state, 'error) result

type ('value, 'error) application_value_services = {
  integer :
    Vir.recursive_spec_argument Symbolic_application_private.t -> 'value;
  boolean :
    Vir.recursive_spec_argument Symbolic_application_private.t -> 'value;
  parametric :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  function_ :
    Sst.typ ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  aggregate :
    Vir.aggregate_type ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'value;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  invalid : string -> 'error;
}

val value_of_logic_application :
  ('value, 'error) application_value_services ->
  Sst.typ ->
  Vir.recursive_spec_argument Symbolic_application_private.t ->
  ('value, 'error) result
