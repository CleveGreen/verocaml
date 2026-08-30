type formal_requirement = {
  ordinal : int;
  label : string option;
  pattern_digest : string;
  binding_ids : int list;
  mode : Sst.instance_mode;
  typ : Sst.typ;
  rank_domain_id : string;
  rank_domain_version : string;
  rank_domain_digest : string;
  rank_component_snapshot : string list;
  profile_actual_snapshot : string list;
  requirement_digest : string;
  rank_component : Typedtree_adapter.rank_type_identity list;
  rank_positive_children : Typedtree_adapter.rank_positive_child list;
  rank_ground_witnesses : Typedtree_adapter.rank_ground_witness list;
}

type parameter_kind = Parametric_signature_private.parameter_kind =
  | Positional_parameter
  | Labelled_parameter
  | Optional_parameter
  | Default_parameter

type provider_model = {
  domain : Sst.type_id;
  result_type : Sst.typ;
  closure_type_ids : Sst.type_id list;
  closure_digest : string;
}

type provider_callable = {
  resolved_path : string;
  binding_uid : string;
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  finite_requirements : formal_requirement list;
  model : provider_model option;
}

type provider_type = {
  resolved_path : string;
  source_path : string;
  source_name : string;
  binding_uid : string;
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
  revealed : bool;
  logical : bool;
  field_modes : (Sst.field_id * Sst.instance_mode) list;
  rank_profile_digest : string option;
}

type provider_external_specification = {
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  target_link : Sst.target_link;
}

type provider_description = {
  unit_name : string;
  interface_digest : string;
  source_digest : string;
  family_digest : string;
  import_digest : string;
  callables : provider_callable list;
  types : provider_type list;
  external_specifications : provider_external_specification list;
}

type provider
type environment
type registration
type call
type aggregate_application_identity

type callable_snapshot = {
  path : string;
  binding_uid : string;
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  finite_requirements : formal_requirement list;
  provider_unit : string;
  provider_interface : string;
  provider_source : string;
  provider_family : string;
  provider_import : string;
  summary_digest : string;
  model : provider_model option;
}

type type_snapshot = {
  path : string;
  binding_uid : string;
  source_type_id : Sst.type_id;
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
}

type external_specification_snapshot = {
  definition : Sst.function_definition;
  signature : Parametric_signature_private.t;
  target_link : Sst.target_link;
  provider_unit : string;
  provider_interface : string;
}

val seal_provider :
  provider_completion:Verified_provider_completion_private.t ->
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  direct_dependencies:provider list ->
  provider_description ->
  (provider, string) result

val provider_matches :
  provider ->
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  bool

val create : provider list -> (environment, string) result
val callables : environment -> callable_snapshot list
val types : environment -> type_snapshot list
val external_specifications : environment -> external_specification_snapshot list
val empty : environment

val seal_calls :
  environment ->
  implementation:Cmt_input.implementation ->
  program:Sst.program ->
  (registration, string) result

val registration_environment : registration -> environment
val find_call : registration -> Sst.expression -> call option
val call_summary : call -> callable_snapshot
val ephemeral_contract_view :
  registration option ->
  Sst.expression ->
  Sst.function_definition ->
  Sst.function_definition
val call_invocation_ordinal : call -> int
val call_snapshot : call -> string
val call_is_aggregate_model : call -> bool
val begin_consumer_session : registration -> session:unit ref -> (unit, string) result
val end_consumer_session : registration -> session:unit ref -> unit
val invalidate_registration : registration -> unit
val consume_aggregate_model_call :
  registration ->
  session:unit ref ->
  expression:Sst.expression ->
  actual_types:Sst.typ list ->
  application_snapshot:string ->
  (callable_snapshot * string * string * aggregate_application_identity,
   string)
  result
val authenticate_aggregate_application_identity :
  aggregate_application_identity -> bool
val aggregate_application_logical_digest :
  aggregate_application_identity -> string
val aggregate_application_identity_matches :
  aggregate_application_identity ->
  logical_digest:string ->
  call_snapshot:string ->
  registration_snapshot:string ->
  invocation_ordinal:int ->
  application_snapshot:string ->
  bool
val is_imported : registration -> Sst.function_id -> bool
val finite_requirement :
  registration -> Sst.function_id -> int -> formal_requirement option
val dump : environment -> string

module For_testing : sig
  type aggregate_lifecycle = {
    descriptors_issued : int;
    applications_admitted : int;
    descriptors_invalidated : int;
  }

  val reset_aggregate_lifecycle : unit -> unit
  val aggregate_lifecycle : unit -> aggregate_lifecycle
  val test_aggregate_application_identity :
    logical_digest:string ->
    call_snapshot:string ->
    registration_snapshot:string ->
    invocation_ordinal:int ->
    application_snapshot:string ->
    aggregate_application_identity
  val invalidate_aggregate_application_identity :
    aggregate_application_identity -> unit
  val aggregate_closure_matrix : unit -> string list
  val generic_family_sealing_matrix : unit -> string list

  type authority_attack =
    | Raw_retained_summary
    | Forged_retained_summary
    | Textual_specialization_authority

  val authority_attacks : authority_attack list
  val authority_attack_name : authority_attack -> string
  val run_authority_attack : environment -> authority_attack -> (unit, string) result
  val stale_recursive_evidence : environment -> (unit, string) result

  type abi_attack =
    | Wrong_label
    | Reordered_same_type
    | Binding_id_collision
    | Omitted_argument
    | Default_argument
    | Optional_argument
    | Destructured_formal
    | Wildcard_formal
    | Invalid_result_binder
    | Partial_application
    | Ambiguous_alias
    | Ambiguous_open
    | Functor_path
    | Public_executor_only_provider

  val abi_attacks : abi_attack list
  val abi_attack_name : abi_attack -> string
  val abi_attack_named : string -> abi_attack option
  val run_abi_attack : abi_attack -> (unit, string) result
end
