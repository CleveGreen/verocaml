type theorem_kind = Proved_lemma | Trusted_axiom
type source_role = Proved_body | Trusted_proof_body | Other_body
type theorem

type imported_declaration = {
  imported_path : string;
  imported_definition : Sst.function_definition;
  imported_trigger_span : Diagnostic.span;
}

type imported_group = {
  imported_group_path : string;
  imported_target_paths : string list;
}

val authenticate_typedtree :
  ?imported_declarations:(string * string) list ->
  ?imported_groups:(string * string) list ->
  source_file:string ->
  imports:Cmt_input.import array ->
  artifact:Typedtree_adapter_issuance_private.proof_capture_artifact option ->
  Typedtree.structure ->
  (Typedtree_broadcast_private.t, Diagnostic.t) result

val validate_source_role :
  scan:Typedtree_broadcast_private.t ->
  binding:Typedtree.value_binding ->
  recursive:bool ->
  role:source_role ->
  span:Diagnostic.span ->
  (unit, Diagnostic.t) result

val is_declaration :
  Typedtree_broadcast_private.t -> Typedtree.value_binding -> bool

val source_bindings :
  Typedtree_broadcast_private.t ->
  Typedtree.value_binding list ->
  Typedtree.value_binding list

val activation_body :
  Typedtree_broadcast_private.t option ->
  Typedtree.expression ->
  Typedtree.expression option

val register :
  source_file:string ->
  scan:Typedtree_broadcast_private.t ->
  program:Sst.program ->
  sources:(Typedtree.value_binding * Sst.function_id) list ->
  imported_declarations:imported_declaration list ->
  imported_groups:imported_group list ->
  (unit, Diagnostic.t) result

val theorems : program:Sst.program -> theorem list
val find : program:Sst.program -> id:string -> theorem option
val id : theorem -> string
val kind : theorem -> theorem_kind
val definition : theorem -> Sst.function_definition
val formals : theorem -> Sst.binding list
val trigger : theorem -> Sst.expression
val trigger_head : theorem -> Sst.function_id
val trigger_type_pattern : theorem -> Parametric_type.t list
val declaration_span : theorem -> Diagnostic.span
val witness_span : theorem -> Diagnostic.span option

val infer_type_vector :
  theorem ->
  Parametric_type.t list ->
  (Parametric_type.t list option, string) result

val schema :
  theorem ->
  actual_types:Parametric_type.t list ->
  (Logic_quantifier_private.vector, string) result

val destroy : Sst.program -> unit
