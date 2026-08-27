(** Compiler-authenticated, position-restricted callback spines.

    This module deliberately owns no callback authority.  It records only
    compiler labels, canonical first-order endpoint types, the final
    first-order result, and full-saturation checks. *)

type label = Unlabelled | Labelled of string
type t

val create :
  endpoints:(label * Parametric_type.t) list ->
  result:Parametric_type.t ->
  (t, string) result

val endpoints : t -> (label * Parametric_type.t) list
val result : t -> Parametric_type.t
val arity : t -> int
val labels : t -> string option list
val endpoint_types : t -> Parametric_type.t list
val label_to_option : label -> string option

val order_arguments :
  t -> (string option * 'a) list -> ((string option * 'a) list, string) result

val instantiate :
  (Parametric_type.binder * Parametric_type.t) list -> t -> t

val validate_saturated :
  t ->
  labels:string option list ->
  arguments:Parametric_type.t list ->
  result:Parametric_type.t ->
  (unit, string) result

val equal : t -> t -> bool
val same_identity : t -> t -> bool
val to_string : t -> string

val compiler_callback_signature :
  first_order:(Types.type_expr -> bool) ->
  Types.type_expr ->
  bool

val canonical_type : Types.type_expr -> Types.type_expr

val compiler_type_has_path : Types.type_expr -> Path.t -> bool
val same_location : Location.t -> Location.t -> bool

val source_parameter_patterns :
  Typedtree.value_binding -> Typedtree.pattern list option

val source_signature_types :
  Typedtree.value_binding ->
  definition_body:Typedtree.expression option ->
  Types.type_expr list

val has_immediate_callback_actual :
  is_callback:(Types.type_expr -> bool) ->
  Typedtree.function_param list ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  bool

val expression_compares_type :
  resolves_equality:(Path.t -> bool) ->
  type_paths:Path.t list ->
  Typedtree.expression ->
  bool
