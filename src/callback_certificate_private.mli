(** Unforgeable callback authority and lifecycle.  The compilation identity is
    a narrow immutable digest, never a transported [Cmt_input.implementation]. *)

type origin_kind = Formal | Top_level | Local
type compilation_identity

type capture = {
  binding_id : int;
  binding_uid : string;
  typ : Parametric_type.t;
  compiler_mode : string;
  instance_mode : string;
  frozen_value_identity : string;
}

type origin
type t

val cmt_compilation_identity :
  Cmt_input.implementation -> (compilation_identity, string) result

val caller_identity : function_index:int -> function_name:string -> string

val edge_identity :
  caller:string ->
  start_line:int ->
  start_column:int ->
  end_line:int ->
  end_column:int ->
  string

val same_compilation : compilation_identity -> compilation_identity -> bool
val compilation_identity_string : compilation_identity -> string

val issue_formal :
  compilation_identity:compilation_identity ->
  callable_identity:string ->
  shape:Callback_shape_private.t ->
  t

val issue_origin :
  kind:origin_kind ->
  compilation_identity:compilation_identity ->
  callable_identity:string ->
  shape:Callback_shape_private.t ->
  compiler_mode:string ->
  contract_identity:string ->
  captures:capture list ->
  pure:bool ->
  total:bool ->
  complete:bool ->
  (origin, string) result

val seal_call_edge :
  origin -> caller_identity:string -> call_edge_identity:string -> t

val bind_session :
  t -> compilation_identity:compilation_identity -> session:unit ref -> (unit, string) result

val authenticate :
  t ->
  shape:Callback_shape_private.t ->
  compilation_identity:compilation_identity ->
  session:unit ref ->
  caller_identity:string option ->
  call_edge_identity:string option ->
  (unit, string) result

val authenticate_shape : t -> Callback_shape_private.t -> (unit, string) result
val kind : t -> origin_kind
val callable_identity : t -> string
val relation_identity : t -> string
val call_edge_identity : t -> string option
val captures : t -> capture list
val same_identity : t -> t -> bool
val same_origin : t -> t -> bool
val describe : t -> string
