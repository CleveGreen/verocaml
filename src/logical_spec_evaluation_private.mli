(** Private compositional evaluation for authenticated ordinary pure Specs. *)

type value = Spec_function_logic_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of function_value

and function_value = Spec_function_logic_private.function_value = {
  function_term : Vir.spec_function_term;
  function_arrow : Sst.typ;
  function_closure : function_closure;
}

and function_closure = Spec_function_logic_private.function_closure =
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

type model_capability = Logical_spec_capability_private.model_capability

type contract_clause_kind =
  Logical_spec_capability_private.contract_clause_kind =
  | Requires
  | Ensures

type root_identity = Logical_spec_capability_private.root_identity

type call_target = Logical_spec_capability_private.call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of model_capability
  | Opaque_recursive
  | Unsupported

type permit = Logical_spec_authentication_private.permit
type strict_permit = Logical_spec_capability_private.permit

val authenticate :
  classify:(Sst.function_id -> call_target) ->
  Sst.function_definition ->
  permit option

val classify_definition :
  excluded:(Sst.function_definition -> bool) ->
  Sst.function_definition ->
  call_target

val authenticate_expression :
  classify:(Sst.function_id -> call_target) -> Sst.expression -> permit option

val authenticate_invariant_contract :
  validated:Sst_validation.validated_program ->
  type_definitions:Sst.type_definition list ->
  classify:(Sst.function_id -> call_target) ->
  root_identity:root_identity ->
  Sst.expression ->
  (strict_permit, string) result

val root_identity_to_string : root_identity -> string

type ('context, 'state, 'error) callbacks = {
  classify : Sst.function_id -> call_target;
  aggregate_type : Sst.typ -> Vir.aggregate_type option;
  option_instance : Sst.typ -> Parametric_adt.option_instance option;
  environment : 'state -> (int * value) list;
  with_environment : 'state -> (int * value) list -> 'state;
  assume : 'state -> Vir.boolean_term list -> 'state;
  observe_field_read :
    'context ->
    'state ->
    Sst.expression ->
    Sst.field_id ->
    Vir.aggregate_term ->
    value ->
    ('state, 'error) result;
  observe_construction :
    'context ->
    'state ->
    Sst.expression ->
    (Sst.expression * value) list ->
    Vir.aggregate_term ->
    (Vir.aggregate_term * 'state, 'error) result;
  enter_definition : 'context -> Sst.function_definition -> 'context;
  evaluate_recursive :
    'context -> Sst.expression -> 'state -> (value * 'state, 'error) result;
  error : Diagnostic.span -> string -> 'error;
}

val vir_aggregate_type_of_sst :
  Parametric_adt.t list -> Sst.typ -> Vir.aggregate_type option

val evaluate :
  permit ->
  ('context, 'state, 'error) callbacks ->
  'context ->
  Sst.expression ->
  'state ->
  (value * 'state, 'error) result

val evaluate_invariant_contract :
  strict_permit ->
  validated:Sst_validation.validated_program ->
  root_identity:root_identity ->
  ('context, 'state, 'error) callbacks ->
  'context ->
  Sst.expression ->
  'state ->
  (value * 'state, 'error) result

val vir_aggregate_type : Sst.type_id -> Vir.aggregate_type
val field_selector : Sst.field_id -> int list -> Vir.sort -> Vir.selector

val argument_selector :
  Sst.constructor_id -> int -> int list -> Vir.sort -> Vir.selector

val equality : value -> value -> Vir.boolean_term option
val vir_comparison : Sst.comparison -> Vir.comparison

val scalar_variant_equality :
  Parametric_adt.t list ->
  Sst.typ ->
  Vir.aggregate_term ->
  Vir.aggregate_term ->
  Vir.boolean_term option

val selected_value_without_state :
  Vir.aggregate_term ->
  (int list -> Vir.sort -> Vir.selector) ->
  int list ->
  Sst.typ ->
  value

val selected_parametric_value_without_state :
  aggregate_type:(Sst.typ -> Vir.aggregate_type option) ->
  Vir.aggregate_term ->
  (int list -> Vir.sort -> Vir.selector) ->
  int list ->
  Sst.typ ->
  value

val ranges_of_value : Sst.typ -> value -> Vir.boolean_term list

val resolve_optional :
  error:(string -> 'error) ->
  default:value ->
  presence:Vir.boolean_term ->
  payload:value ->
  (value, 'error) result
