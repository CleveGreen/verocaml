val sort_name : Parametric_type.binder -> string
val of_symbol : Vir.symbol -> (Vir.parametric_term, string) result
val validate : Vir.parametric_term -> (unit, string) result

val equal :
  Vir.parametric_term ->
  Vir.parametric_term ->
  (Vir.boolean_term, string) result

val conditional :
  Vir.boolean_term ->
  Vir.parametric_term ->
  Vir.parametric_term ->
  (Vir.parametric_term, string) result

val conditions : Vir.parametric_term -> Vir.boolean_term list

val fold_conditions_result :
  (unit, 'error) result ->
  (Vir.boolean_term -> (unit, 'error) result) ->
  Vir.parametric_term ->
  Vir.parametric_term ->
  (unit, 'error) result

val for_all_conditions :
  (Vir.boolean_term -> bool) ->
  Vir.parametric_term ->
  Vir.parametric_term ->
  bool

type logic_sort_registry

val create_logic_sort_registry : unit -> logic_sort_registry

val logic_sort :
  logic_sort_registry ->
  builder:Logic_ir.builder ->
  span:Diagnostic.span ->
  Parametric_type.binder ->
  Logic_ir.sort

val symbol_string :
  span_string:(Diagnostic.span -> string) -> Vir.symbol -> string

val fold_equal :
  symbol:(Parametric_type.binder -> Vir.symbol -> 'a) ->
  selector:(Parametric_type.binder -> Vir.selector -> Vir.aggregate_term -> 'a) ->
  conditional:(Vir.boolean_term -> 'a -> 'a -> 'a) ->
  application:
    (Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'a) ->
  Vir.parametric_term ->
  Vir.parametric_term ->
  ('a * 'a, string) result

val fold :
  symbol:(Vir.symbol -> 'a) ->
  selector:(Vir.selector -> Vir.aggregate_term -> 'a) ->
  conditional:(Vir.boolean_term -> 'a -> 'a -> 'a) ->
  application:
    (Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    'a) ->
  Vir.parametric_term ->
  'a

val fold_function :
  symbol:(Vir.symbol -> 'a) ->
  conditional:(Vir.boolean_term -> 'a -> 'a -> 'a) ->
  application:
    (Vir.recursive_spec_argument Symbolic_application_private.t -> 'a) ->
  Vir.parametric_term ->
  'a
