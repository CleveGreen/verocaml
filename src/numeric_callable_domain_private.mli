type t = Exact_carrier | Different_carrier | Unsupported
type endpoint = Carrier | Mathematical of string | Runtime_integer | Boolean | Unit
type shape = private { parameters : endpoint list; result : endpoint }
val shape : constructor_uid:(Path.t -> string option) ->
  logical_sorts:Logical_sort_private.t list -> carrier_uid:string ->
  carrier:Types.type_declaration -> Types.type_expr -> shape option

(** Recognizes a monomorphic, unlabelled unary callable whose domain is the
    exact nullary runtime carrier constructor. Transparent type-equality and
    generic instantiation are deliberately not inferred here. *)
val classify :
  constructor_uid:(Path.t -> string option) ->
  logical_sorts:Logical_sort_private.t list ->
  carrier_uid:string ->
  carrier:Types.type_declaration ->
  Types.type_expr -> t
