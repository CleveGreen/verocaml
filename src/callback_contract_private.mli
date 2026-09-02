(** Contract policy and retained carrier extraction for callback definitions. *)

type carrier_kind = Requires | Ensures | Decreases | Assert

type carrier = {
  kind : carrier_kind;
  binder : Sst.pattern option;
  payload : Sst.expression;
  span : Sst.span;
}

type execution_clause = {
  ordinal : int;
  span : Sst.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type retained_ghost_clause = {
  clause_id : string;
  shadow_parameters : Typedtree.function_param list;
  contract_application : Typedtree.expression;
}

val contracts : carrier list -> Sst.contracts

val extract :
  carrier list ->
  Sst.expression ->
  (Sst.contracts * Sst.expression, Diagnostic.t) result

val validate_explicit : Sst.contracts -> (unit, string) result

val validate_top_level :
  retained_pair:
    (Typedtree.expression -> Typedtree.expression -> 'retained option) ->
  application:('retained -> Typedtree.expression) ->
  resolves:(Path.t -> string -> string -> bool) ->
  Typedtree.value_binding ->
  (unit, string) result

type ('candidate, 'error) top_level_services = {
  candidates : Path.t -> 'candidate list;
  eligible : 'candidate -> bool;
  candidate_identity : 'candidate -> string;
  candidate_id : 'candidate -> Sst.function_id;
  shape :
    Env.t ->
    Location.t ->
    Types.type_expr ->
    (Callback_shape_private.t, 'error) result;
  validate_contract : 'candidate -> (unit, 'error) result;
  compilation_identity :
    Location.t ->
    (Callback_certificate_private.compilation_identity, 'error) result;
  caller_identity : string;
  call_edge_identity : string;
  compiler_mode : Types.type_expr -> string;
  span : Location.t -> Sst.span;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
}

val top_level_argument :
  ('candidate, 'error) top_level_services ->
  Callback_shape_private.t ->
  string option ->
  Typedtree.expression ->
  (Sst.call_argument, 'error) result

type 'error actual_services = {
  find_binding : Ident.t -> Sst.callback_binding option;
  seal : Location.t -> Sst.callback_binding -> Sst.callback_binding;
  lower_local :
    name:string ->
    Typedtree.expression ->
    (Sst.callback_binding, 'error) result;
  top_level :
    Callback_shape_private.t ->
    string option ->
    Typedtree.expression ->
    (Sst.call_argument, 'error) result;
  call_location : Location.t;
  next_local_name : unit -> string;
  policy_error : Location.t -> string -> 'error;
  authentication_error : Location.t -> string -> 'error;
}

val resolve_actual :
  'error actual_services ->
  Callback_shape_private.t ->
  string option ->
  Typedtree.expression ->
  (Sst.call_argument, 'error) result

type definition_kind =
  | Checked_exec of int option
  | Spec_definition
  | Recursive_spec_definition of [ `Opaque | `Revealed ]
  | Proof_definition

val make_definition :
  source_file:string ->
  function_id:Sst.function_id ->
  recursive:bool ->
  parameters:Sst.parameter list ->
  contracts:Sst.contracts ->
  body:Sst.expression ->
  span:Sst.span ->
  definition_kind ->
  Sst.function_definition

type ('bindings, 'error) parameter_services = {
  normalized_type :
    Location.t -> Types.type_expr -> (Parametric_type.t, 'error) result;
  optional_carrier :
    Location.t ->
    Parametric_type.t ->
    Parametric_type.t ->
    (Parametric_type.t, 'error) result;
  lower_expression :
    'bindings -> Typedtree.expression -> (Sst.expression, 'error) result;
  lower_pattern :
    'bindings ->
    Typedtree.pattern ->
    ((Sst.pattern * 'bindings), 'error) result;
  issue_callback :
    Typedtree.function_param ->
    Typedtree.pattern ->
    (Sst.parameter, 'error) result;
  is_callback : Types.type_expr -> bool;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Sst.span;
  partial_error : Location.t -> 'error;
  parameter_pattern_error : Location.t -> 'error;
  parameter_type_error : Location.t -> string -> 'error;
}

val lower_parameters :
  ('bindings, 'error) parameter_services ->
  function_type:Types.type_expr ->
  'bindings ->
  Typedtree.function_param list ->
  ((Sst.parameter list * 'bindings), 'error) result

type recursive_helper_role =
  | Ordinary_direct_spec
  | Direct_type_invariant
  | Direct_model
  | Not_a_direct_spec

type recursive_helper_member = {
  helper_definition : Sst.function_definition;
  helper_source_order : int;
  helper_role : recursive_helper_role;
}

type recursive_helper_certificate = {
  (* The certificate deliberately retains physical source objects.  Names,
     indices, normalized snapshots, and imported descriptors are descriptive
     checks only and cannot replace these identities. *)
  helper_token : unit ref;
  helper_program : Sst.program;
  helper_program_snapshot : string;
  helper_root : recursive_helper_member;
  helper_members : recursive_helper_member list;
  helper_local_types : Sst.type_id list;
  mutable helper_expanded_body : Sst.expression option;
  mutable helper_adversary_edges :
    (recursive_helper_member * recursive_helper_member) list option;
  mutable helper_adversary_body :
    (recursive_helper_member * Sst.body_disposition) option;
  mutable helper_adversary_call :
    ( recursive_helper_member
    * recursive_helper_member
    * (string option * Sst.expression) list )
    option;
}


val recursive_helper_error :
  ('a, unit, string, ('b, string) result) format4 -> 'a

val recursive_helper_expression_children :
  Sst.expression -> Sst.expression list

val validate_builtin_assertion_predicate :
  Sst.expression -> (unit, Sst.span * string) result
