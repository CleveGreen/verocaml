type translation
type 'error user_quantifier_services = {
  quantifier_builder : Logic_ir.builder;
  quantifier_sort : Vir.sort -> Logic_ir.sort;
  translate :
    (int * Logic_ir.binder) list ->
    Vir.boolean_term ->
    (Logic_ir.term, 'error) result;
  translate_application :
    (int * Logic_ir.binder) list ->
    Vir.application_term ->
    (Logic_ir.term, 'error) result;
  bind_result :
    (Logic_ir.binder, Logic_ir.error) result ->
    (Logic_ir.binder, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : Diagnostic.span -> string -> 'error;
}
val translate_user_quantifier :
  'error user_quantifier_services ->
  universal:bool ->
  Vir.boolean_quantifier ->
  (Logic_ir.term, 'error) result
type 'error relation_services = {
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  declare :
    string ->
    Logic_ir.sort list ->
    Logic_ir.sort ->
    Diagnostic.span ->
    (Logic_ir.function_symbol, 'error) result;
  translate_arguments :
    Vir.recursive_spec_argument list -> (Logic_ir.term list, 'error) result;
  apply :
    Diagnostic.span ->
    Logic_ir.function_symbol ->
    Logic_ir.term list ->
    (Logic_ir.term, 'error) result;
}

val translate_relation :
  'error relation_services ->
  name:string ->
  range:Logic_ir.sort ->
  arguments:Vir.recursive_spec_argument list ->
  span:Diagnostic.span ->
  (Logic_ir.term, 'error) result

type 'error scalar_selector_services = {
  span : Diagnostic.span;
  logic_sort : Vir.sort -> Logic_ir.sort;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  selector_function :
    Vir.selector ->
    Logic_ir.sort ->
    Logic_ir.sort ->
    (Logic_ir.function_symbol, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_aggregate : Vir.aggregate_term -> (Logic_ir.term, 'error) result;
  translate_symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_symbolic_application :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}
val translate_integer_selector :
  'error scalar_selector_services ->
  Vir.selector ->
  Vir.aggregate_term ->
  (Logic_ir.term, 'error) result
val translate_boolean_selector :
  'error scalar_selector_services ->
  Vir.selector ->
  Vir.aggregate_term ->
  (Logic_ir.term, 'error) result

val translate_bit_vector_selector :
  'error scalar_selector_services ->
  width:Bv_width.t ->
  translate_bit_vector:
    (Vir.bit_vector_term -> (Logic_ir.term, 'error) result) ->
  Vir.selector ->
  Vir.aggregate_term ->
  (Logic_ir.term, 'error) result

val translate_parametric :
  'error scalar_selector_services ->
  Vir.parametric_term ->
  (Logic_ir.term, 'error) result

type 'error integer_core_services = {
  span : Diagnostic.span;
  symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_bit_vector :
    Vir.bit_vector_term -> (Logic_ir.term, 'error) result;
  translate_arguments :
    Vir.recursive_spec_argument list -> (Logic_ir.term list, 'error) result;
  recursive_function :
    Sst.function_id ->
    Parametric_type.t list ->
    (Logic_ir.function_symbol, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
}

val translate_integer_core :
  'error integer_core_services ->
  Vir.integer_term ->
  (Logic_ir.term option, 'error) result

type 'error bit_vector_core_services = {
  span : Diagnostic.span;
  symbol :
    Bv_width.t -> Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_bit_vector :
    Vir.bit_vector_term -> (Logic_ir.term, 'error) result;
  translate_selector :
    Bv_width.t ->
    Vir.selector ->
    Vir.aggregate_term ->
    (Logic_ir.term, 'error) result;
  translate_arguments :
    Vir.recursive_spec_argument list -> (Logic_ir.term list, 'error) result;
  recursive_function :
    Sst.function_id ->
    Parametric_type.t list ->
    Bv_width.t ->
    (Logic_ir.function_symbol, 'error) result;
  symbolic_application :
    Bv_width.t ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
}

val translate_bit_vector_core :
  'error bit_vector_core_services ->
  Vir.bit_vector_term ->
  (Logic_ir.term, 'error) result

type 'error boolean_core_services = {
  span : Diagnostic.span;
  symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_bit_vector :
    Vir.bit_vector_term -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
}

val translate_boolean_core :
  'error boolean_core_services ->
  Vir.boolean_term ->
  (Logic_ir.term option, 'error) result

type 'error parametric_equality_services = {
  span : Diagnostic.span;
  symbol :
    Parametric_type.binder ->
    Vir.symbol ->
    (Logic_ir.term, 'error) result;
  selector :
    Parametric_type.binder ->
    Vir.selector ->
    Vir.aggregate_term ->
    (Logic_ir.term, 'error) result;
  application :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}

val translate_parametric_equality :
  'error parametric_equality_services ->
  Vir.parametric_term ->
  Vir.parametric_term ->
  (Logic_ir.term, 'error) result

type ('source, 'opaque, 'exact, 'error) normalized_services = {
  span : Diagnostic.span;
  normalize :
    'source ->
    ( ('opaque, 'exact)
      Logical_aggregate_term_normalization_private.observation,
      string )
    result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_exact : 'exact -> (Logic_ir.term, 'error) result;
  translate_opaque : 'opaque -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}

val translate_normalized :
  ('source, 'opaque, 'exact, 'error) normalized_services ->
  'source ->
  (Logic_ir.term, 'error) result

val translate_arguments :
  integer:(Vir.integer_term -> ('term, 'error) result) ->
  boolean:(Vir.boolean_term -> ('term, 'error) result) ->
  bit_vector:(Vir.bit_vector_term -> ('term, 'error) result) ->
  aggregate:(Vir.aggregate_term -> ('term, 'error) result) ->
  parametric:(Vir.parametric_term -> ('term, 'error) result) ->
  Vir.recursive_spec_argument list ->
  ('term list, 'error) result

(** Translate first-order VIR, including authenticated callback precondition
    and postcondition relations. Callback bodies are never declared. *)
val translate :
  requires:Logic_ir.feature list ->
  Vir.obligation ->
  (translation, string) result

val query : translation -> Logic_ir.query

val projected :
  translation -> (Vir.symbol * Logic_ir.function_symbol) list
