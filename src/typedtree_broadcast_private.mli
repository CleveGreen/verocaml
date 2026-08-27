type target = Broadcast_scope_private.target = {
  target_id : string;
  target_group : bool;
}

type group = {
  group_id : string;
  group_name : string;
  group_targets : target list;
  group_span : Diagnostic.span;
}

type expression_scope = {
  expression_scope_id : string;
  expression_scope_span : Diagnostic.span;
  expression_scope_targets : target list;
}

type t
type error = { location : Location.t; message : string }

val authenticate :
  source_file:string ->
  artifact:Typedtree_adapter_issuance_private.proof_capture_artifact option ->
  resolves_to_marker:(Path.t -> bool) ->
  Typedtree.structure ->
  (t, error) result

val carrier_binding : t -> Typedtree.value_binding -> bool
val declaration_id : t -> Typedtree.value_binding -> string option
val trigger_locations : t -> Typedtree.value_binding -> Location.t list
val active_targets : t -> Typedtree.value_binding -> target list
val expression_scopes : t -> Typedtree.value_binding -> expression_scope list
val activation_body : t -> Typedtree.expression -> Typedtree.expression option
val groups : t -> group list
