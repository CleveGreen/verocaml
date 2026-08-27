(** Compiler-owned callback shape and lexical-identity extraction. *)

type application = {
  callee : Typedtree.expression;
  arguments : (Typedtree.arg_label * Typedtree.expression) list;
  result_type : Types.type_expr;
  span : Location.t;
}

type 'error shape_error =
  | Lowering_error of 'error
  | Invalid_shape of string

val shape :
  env:Env.t ->
  lower:(Location.t -> Types.type_expr -> (Parametric_type.t, 'error) result) ->
  location:Location.t ->
  Types.type_expr ->
  (Callback_shape_private.t, 'error shape_error) result

val application :
  Typedtree.expression -> (application, string) result

val free_idents : Typedtree.expression -> Ident.t list
val compiler_identity : Ident.t -> string
val type_evidence : Types.type_expr -> string

type lowering_state = {
  mutable callback_bindings : (Ident.t * Sst.callback_binding) list;
  mutable callback_origins :
    (int * Callback_certificate_private.origin) list;
  mutable callback_definitions : Sst.function_definition list;
  mutable next_callback_id : int;
  compilation_identity :
    (Callback_certificate_private.compilation_identity, string) result option;
}

type mixed_call_argument =
  | Lowered_call_argument of Sst.call_argument
  | Pending_callback_argument of {
      shape : Callback_shape_private.t;
      label : string option;
      source : Typedtree.expression;
    }

val create_state :
  ?next_callback_id:int ->
  ?compilation_identity:
    (Callback_certificate_private.compilation_identity, string) result ->
  unit ->
  lowering_state

val deeply_immutable :
  parametric_adts:Parametric_adt_lowering_private.lowered list ->
  definitions:Sst.type_definition list ->
  Sst.typ ->
  bool

type 'error local_services = {
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  lower_pattern :
    (Ident.t * Sst.binding) list ->
    Typedtree.pattern ->
    (Sst.pattern * (Ident.t * Sst.binding) list, 'error) result;
  lower_body :
    (Ident.t * Sst.binding) list ->
    Typedtree.expression ->
    (Sst.contracts * Sst.expression, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  find_value : Ident.t -> Sst.binding option;
  find_callback : Ident.t -> Sst.callback_binding option;
  source_type : Sst.binding -> Types.type_expr option;
  immutable : Sst.typ -> bool;
  fresh_id : unit -> int;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  seal :
    Callback_certificate_private.origin ->
    Location.t ->
    Callback_certificate_private.t;
  source_file : string;
  owner_name : string;
  type_binders : Parametric_type.binder list;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
  contract_error : Location.t -> string -> 'error;
}

val lower_local :
  'error local_services ->
  lowering_state ->
  (Ident.t * Sst.binding) list ->
  ?ident:Ident.t ->
  name:string ->
  Typedtree.expression ->
  (Sst.callback_binding, 'error) result

val callback_arrow_type : Types.type_expr -> bool
val find_binding : lowering_state -> Ident.t -> Sst.callback_binding option
val caller_identity : Sst.function_id option -> string option
val call_edge_identity : string -> Location.t -> string

val validate_top_level_contract :
  retained_pair:
    (Typedtree.expression -> Typedtree.expression -> 'retained option) ->
  application:('retained -> Typedtree.expression) ->
  resolves:(Path.t -> string -> string -> bool) ->
  error:(Location.t -> string -> 'error) ->
  Typedtree.value_binding ->
  (unit, 'error) result

val compilation_identity :
  lowering_state ->
  (Callback_certificate_private.compilation_identity, string) result

val fresh_id : lowering_state -> owner_index:int option -> int
val fresh_id_for_function :
  lowering_state -> Sst.function_id option -> int

val seal_binding :
  lowering_state ->
  caller_identity:string ->
  span:(Location.t -> Sst.span) ->
  Location.t ->
  Sst.callback_binding ->
  Sst.callback_binding

val application_binding :
  lowering_state ->
  caller_identity:string ->
  span:(Location.t -> Sst.span) ->
  Typedtree.expression ->
  ((application * Sst.callback_binding), string) result

type 'error formal_services = {
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  fresh_id : unit -> int;
  owner_identity : string;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
}

val issue_formal :
  'error formal_services ->
  lowering_state ->
  Typedtree.function_param ->
  Typedtree.pattern ->
  (Sst.parameter, 'error) result

val source_type_variables :
  parameters:Typedtree.pattern list ->
  result:Typedtree.expression option ->
  substitutions:(int * Types.type_expr) list ->
  int list

val retained_shadow_type_aliases :
  (int * Types.type_expr) list ->
  Types.type_expr ->
  Types.type_expr ->
  (int * Types.type_expr) list option
