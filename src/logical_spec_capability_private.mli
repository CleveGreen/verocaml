type model_capability
type contract_clause_kind = Requires | Ensures
type root_identity

val model_capability :
  model_handle_token:unit ref ->
  model_invariant_id:string ->
  model_abstract_type:Sst.type_id ->
  model_representation_root:Sst.type_id ->
  predicate_callable:Sst.function_id ->
  model_descriptor:Sst_validation.model_descriptor ->
  callable_descriptor:Sst_validation.callable_descriptor ->
  definition:Sst.function_definition ->
  model_callable:Sst.function_id ->
  model_result_type:Sst.typ ->
  predicate_digest:string ->
  model_capability

module type Model_source = sig
  type handle

  val model_callable : handle -> Sst.function_id
  val invariant_id : handle -> string
  val abstract_type : handle -> Sst.type_id
  val predicate_callable : handle -> Sst.function_id
  val model_snapshot_type : handle -> Sst.typ
  val predicate_digest : handle -> string
  val predicate_definition : handle -> Sst.function_definition
end

module Make_model_capture (Source : Model_source) : sig
  val identity : Source.handle -> root_identity

  val materialize :
    Sst_validation.validated_program ->
    (Source.handle * bool * string option) list ->
    model_capability list
    * (root_identity * Sst.expression * string option) list
end

val model_definition : model_capability -> Sst.function_definition
val model_callable : model_capability -> Sst.function_id
val model_result_type : model_capability -> Sst.typ
val model_representation_root : model_capability -> Sst.type_id

val validate_model_capture :
  Sst_validation.validated_program -> model_capability -> (unit, string) result

val invariant_root :
  invariant_id:string ->
  predicate_callable:Sst.function_id ->
  predicate_digest:string ->
  root_identity

val contract_root :
  callable:Sst.function_id ->
  clause_kind:contract_clause_kind ->
  ordinal:int ->
  root_identity

val root_identity_to_string : root_identity -> string

type call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of model_capability
  | Opaque_recursive
  | Unsupported

type permit

val create_permit :
  validated:Sst_validation.validated_program ->
  root_identity:root_identity ->
  root:Sst.expression ->
  definitions:(int * Sst.function_definition) list ->
  callable_descriptors:Sst_validation.callable_descriptor list ->
  aggregate_descriptors:Sst.type_definition list ->
  option_descriptors:Sst.typ list ->
  field_descriptors:Sst.field_definition list ->
  model_capabilities:(model_capability * Sst.field_id list) list ->
  permit

val validate_root :
  permit ->
  validated:Sst_validation.validated_program ->
  root_identity:root_identity ->
  Sst.expression ->
  (unit, string) result

type authorized_call = {
  definition : Sst.function_definition;
  model : model_capability option;
}

val authorize_call :
  permit ->
  validated:Sst_validation.validated_program ->
  Sst.function_id ->
  result_type:Sst.typ ->
  (authorized_call, string) result

val authorize_aggregate :
  permit ->
  validated:Sst_validation.validated_program ->
  Sst.type_id ->
  (unit, string) result

val validate_aggregate_value :
  permit ->
  validated:Sst_validation.validated_program ->
  descriptor:Sst.type_id ->
  Sst.typ ->
  Vir.aggregate_type ->
  (unit, string) result

val authorize_option :
  permit ->
  validated:Sst_validation.validated_program ->
  Sst.typ ->
  (unit, string) result

val validate_option_value :
  permit ->
  validated:Sst_validation.validated_program ->
  Sst.typ ->
  Parametric_adt.option_instance ->
  Vir.aggregate_type ->
  (unit, string) result

val authorize_field_read :
  permit ->
  validated:Sst_validation.validated_program ->
  current_model:model_capability option ->
  Sst.field_id ->
  result_type:Sst.typ ->
  (bool, string) result

module For_testing : sig
  val has_model_capability : permit -> bool
  val stale_model_control :
    permit -> Sst_validation.validated_program -> string option
  val capability_negative_controls :
    permit -> Sst_validation.validated_program -> string list

  val reset_formula_observations : unit -> unit
  val before_authentication : root_identity -> authority_snapshot:string -> unit
  val note_evaluation : root_identity -> unit
  val trace_profile : root_identity -> string -> unit
  val trace_registry : unchanged:bool -> entries:int -> unit
  val trace_authority : string -> unit
  val trace_abstention :
    root_identity -> reason:string -> authority_snapshot:string -> unit
  val trace_evaluation :
    root_identity -> function_name:string -> authority_unchanged:bool -> unit
end
