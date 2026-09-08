(** Canonical classification for ghost specification arrows. *)

type arrow = {
  label : string option;
  domain : Parametric_type.t;
  range : Parametric_type.t;
}

val make :
  label:string option ->
  domain:Parametric_type.t ->
  range:Parametric_type.t ->
  Parametric_type.t

val classify : Parametric_type.t -> arrow option
val require : Parametric_type.t -> (arrow, string) result
val equal : arrow -> arrow -> bool
val digest : arrow -> string
val sort_name : arrow -> string
val apply_name : arrow -> string
val constructor_name : site:string -> arrow -> string
val source_type : Types.type_expr -> bool
val contains_source_arrow : Types.type_expr -> bool
val contains : Parametric_type.t -> bool
val binder : Parametric_type.t -> Parametric_type.binder
val is_binder_for : Parametric_type.t -> Parametric_type.binder -> bool
val is_function_binder : Parametric_type.binder -> bool

val declaration :
  identity:string ->
  name:string ->
  span:Diagnostic.span ->
  parameter_types:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  (Symbolic_application_private.declaration, string) result

val lower_source_arrow :
  lower:(Types.type_expr -> (Parametric_type.t, 'error) result) ->
  Types.type_expr ->
  (Parametric_type.t, 'error) result option

val resolves_to_spec_carrier : Cmt_input.import array -> Path.t -> bool

val path_resolves_to :
  imports:Cmt_input.import array -> Path.t -> string -> string -> bool

val lower_arrow :
  logical:bool ->
  Types.arg_label ->
  Parametric_type.t ->
  Parametric_type.t ->
  (Parametric_type.t, Parametric_lowering_private.source_type_error) result

type source_application =
  | Runtime_scalar_application of [ `Int | `Bool | `Unit ]
  | Logical_sort_application of Logical_sort_private.t
  | Ambiguous_logical_sort_application
  | Conflicting_logical_sort_identity
  | Parametric_application of Parametric_adt.t
  | Aggregate_application of Sst.type_id
  | Polymorphic_application
  | Unsupported_application

val lower_compiler_type :
  substitutions:(int * Types.type_expr) list ->
  binders:(int * Parametric_type.binder) list ->
  logical:bool ->
  resolve:(Path.t -> source_application) ->
  Types.type_expr ->
  (Parametric_type.t, Parametric_lowering_private.source_type_error) result

val generic_eligible :
  kind:Parametric_function_selection_private.kind ->
  type_variables:int list ->
  signature:Types.type_expr list ->
  bool

type ('result, 'error) quantifier_value_services = {
  lower : Logic_quantifier_private.first_order_binder -> 'result option;
  value_error : Diagnostic.span -> string -> 'error;
}

val quantifier_value :
  ('result, 'error) quantifier_value_services ->
  typ:Parametric_type.t ->
  span:Diagnostic.span ->
  ('result, 'error) result

type 'term quantifier_body_services = {
  integer_range : unit -> 'term list;
  truth : 'term;
  conjunction : 'term -> 'term -> 'term;
  disjunction : 'term -> 'term -> 'term;
  negation : 'term -> 'term;
}

val quantifier_body :
  'term quantifier_body_services ->
  Logic_quantifier_private.kind ->
  Parametric_type.t ->
  'term ->
  'term
