type parameter_kind =
  | Positional_parameter
  | Labelled_parameter
  | Optional_parameter
  | Default_parameter

type recursive_evidence

type formal = {
  ordinal : int;
  label : string option;
  kind : parameter_kind;
  mode : Sst.instance_mode;
  typ : Parametric_type.t;
  default_digest : string option;
}

type t

type instantiated = {
  type_arguments : Parametric_type.t list;
  parameters : Sst.parameter list;
  parameter_types : Parametric_type.t list;
  result_type : Parametric_type.t;
  contracts : Sst.contracts;
  semantic_fingerprint : string;
  semantic_dump : string;
}

val verified_recursion :
  definition:Sst.function_definition ->
  provider_completion:Verified_provider_completion_private.t ->
  (recursive_evidence, string) result

val create :
  definition:Sst.function_definition ->
  parameter_kinds:parameter_kind list ->
  parameter_modes:Sst.instance_mode list ->
  result_mode:Sst.instance_mode ->
  recursive_evidence:recursive_evidence option ->
  (t, string) result

val rebind_definition : t -> Sst.function_definition -> (t, string) result
val rebase_definition :
  t ->
  source_definition:Sst.function_definition ->
  function_id:Sst.function_id ->
  Sst.function_definition ->
  (Sst.function_definition * t, string) result
val binders : t -> Parametric_type.binder list
val formals : t -> formal list
val result_type : t -> Parametric_type.t
val result_mode : t -> Sst.instance_mode
val modes : t -> Sst.instance_mode list * Sst.instance_mode
val contracts : t -> Sst.contracts
val recursive : t -> bool
val recursive_verified : t -> bool
val source_definition : t -> Sst.function_definition
val semantic_fingerprint : t -> string
val semantic_dump : t -> string

val infer_type_arguments :
  t ->
  actual_types:Parametric_type.t list ->
  actual_labels:string option list ->
  actual_result:Parametric_type.t ->
  (Parametric_type.t list, string) result

val infer_partial_type_arguments :
  t ->
  actual_types:Parametric_type.t option list ->
  actual_labels:string option list ->
  actual_result:Parametric_type.t ->
  (Parametric_type.t list, string) result

val instantiate : t -> Parametric_type.t list -> (instantiated, string) result

val validate_call :
  t ->
  type_arguments:Parametric_type.t list ->
  actual_result:Parametric_type.t ->
  arguments:(string option * Sst.expression) list ->
  (instantiated, string) result

module For_testing : sig
  val forged_recursive_signature :
    definition:Sst.function_definition ->
    parameter_kinds:parameter_kind list ->
    parameter_modes:Sst.instance_mode list ->
    result_mode:Sst.instance_mode ->
    (t, string) result

  val stale_recursive_body : t -> (unit, string) result
end

val remap_descriptor_type_id :
  Parametric_adt.t -> Parametric_type.type_id ->
  (Parametric_adt.t, string) result
