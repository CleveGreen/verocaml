type declaration_kind = Proved | Trusted

type declaration = {
  declaration_id : string;
  function_id : Sst.function_id;
  kind : declaration_kind;
  declaration_span : Diagnostic.span;
  witness_span : Diagnostic.span option;
  preverified : bool;
}

type target = { target_id : string; target_group : bool }

type group = {
  group_id : string;
  group_name : string;
  targets : target list;
  span : Diagnostic.span;
}

type expression_scope = {
  scope_id : string;
  scope_span : Diagnostic.span;
  targets : target list;
}

type function_scope = {
  function_id : Sst.function_id;
  targets : target list;
  expressions : expression_scope list;
}

type selection = {
  declaration : declaration;
  selecting_paths : string list list;
}

type counters = {
  active_declarations : int;
  trusted_declarations : int;
  proved_declarations : int;
}

type typedtree_structure = {
  structure_path : string list;
  structure : Typedtree.structure;
  direct_bindings : Typedtree.value_binding list;
  bindings : Typedtree.value_binding list;
  carrier_attributes : int;
  declaration_attributes : int;
  scope_attributes : int;
  public_attributes : int;
}

type typedtree_inventory = {
  structures : typedtree_structure list;
  carrier_attributes : int;
  declaration_attributes : int;
  scope_attributes : int;
  public_attributes : int;
}

type typedtree_syntax = Absent | Retained | Raw

type carrier_kind = Group_carrier | Structure_carrier | Expression_carrier

type carrier_metadata = {
  carrier_kind : carrier_kind;
  carrier_id : string;
  carrier_name : string;
}

val typedtree_inventory : Typedtree.structure -> typedtree_inventory
val typedtree_syntax : typedtree_inventory -> typedtree_syntax
val canonical_marker_path : Cmt_input.import array -> Path.t -> bool
val carrier_payload : Parsetree.attribute -> string option
val carrier_stable_id : string -> Location.t -> string -> string

val authenticate_carrier_attribute :
  Typedtree.value_binding ->
  Parsetree.attribute ->
  (carrier_metadata, Location.t * string) result

val validate_target_graph :
  declaration_ids:string list ->
  groups:(string * string * target list) list ->
  (unit, string) result

val register :
  program:Sst.program ->
  declarations:declaration list ->
  groups:group list ->
  scopes:function_scope list ->
  (unit, string) result

val active :
  program:Sst.program ->
  function_id:Sst.function_id ->
  span:Diagnostic.span ->
  (selection list, string) result

val proved_prerequisites :
  program:Sst.program -> Sst.function_id -> Sst.function_id list

val add_proved_dependencies :
  program:Sst.program ->
  definitions:Sst.function_definition list ->
  (int * Sst.function_id list) list ->
  (int * Sst.function_id list) list

val mark_completed :
  program:Sst.program -> Sst.function_id -> verified:bool -> unit

val counters : selection list -> counters
val merge_targets : target list -> target list -> target list
val destroy : Sst.program -> unit
val registered : Sst.program -> bool
