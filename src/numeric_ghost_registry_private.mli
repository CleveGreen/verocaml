type conflict = private {
  slot_full_key : string;
  law_full_keys : string list;
  carrier_path : string;
  callable_paths : string list;
  target_width : int;
  semantic_width : int;
  meaning : Numeric_ghost_law_private.meaning;
}

type rejected_law = private {
  provider_unit : string;
  callable_path : string;
  law_full_key : string;
}

type error =
  | Invalid_dependency_graph of Interface_specification_environment_private.error
  | Unreachable_law of rejected_law
  | Artifact_mismatch of rejected_law
  | Conflicting_laws of conflict list

type t

(** Imports already admitted ghost laws through the consumer's exact compiler
    dependency graph. Unrelated supplied candidates grant no visibility. The
    original law identity is preserved through aliases/reexports and diamonds;
    equivalent input orders produce the same scope key. Exact duplicate laws
    coalesce; different laws for the same carrier/role/target slot conflict.
    Prior-law prerequisites are included before visibility and conflict checks.

    This creates no solver and issues no law, runtime refinement or lowering
    authority. It does not discover files, admit additional numeric roles or
    replace ordinary specification-library import behavior. *)
val import_laws :
  consumer:Cmt_input.implementation ->
  dependencies:Cmt_input.implementation list ->
  ?descriptors:Numeric_admitted_descriptor_private.t list ->
  Numeric_ghost_law_private.t list -> (t, error) result

val laws : t -> Numeric_ghost_law_private.t list
val descriptors : t -> Numeric_admitted_descriptor_private.t list
val consumer : t -> Cmt_input.implementation
val reached_artifacts : t -> string list
val dependency_artifact_keys : t -> string list
val full_key : t -> string
val equal : t -> t -> bool
val native_bv_source_request :
  t ->
  target:Build_target_profile_private.instance ->
  (Numeric_bv_source_admission_private.request option, string) result
val authorize_occurrence : t -> completion:Verification_driver_private.completion ->
  implementation:Cmt_input.implementation -> imported:Imported_callable.environment ->
  validated:Sst_validation.validated_program -> descriptor:Numeric_admitted_descriptor_private.t ->
  caller:Sst.function_definition -> occurrence:Sst.expression ->
  (Numeric_admitted_descriptor_private.occurrence_authority, string) result
val error_message : error -> string
