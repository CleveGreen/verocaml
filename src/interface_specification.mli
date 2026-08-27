type handle
type environment
type public_type
type public_callable
type public_model
type public_invariant
type public_contract
type public_clause
type verification_report

type visibility = Revealed | Abstract

type provenance = {
  unit_name : string;
  interface_digest : string;
  direct_dependencies : (string * string) list;
  transitive_dependencies : (string * string) list;
}

type error = {
  unit_name : string option;
  message : string;
}

val authenticate :
  timeout_ms:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (environment, error) result

val authenticate_with_rlimit :
  timeout_ms:int ->
  rlimit:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (environment, error) result

val verify_consumer :
  timeout_ms:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (verification_report, error) result

val verify_consumer_with_rlimit :
  timeout_ms:int ->
  rlimit:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (verification_report, error) result

val verify_consumer_with_threads :
  threads:int ->
  timeout_ms:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (verification_report, error) result

val verify_consumer_with_rlimit_and_threads :
  threads:int ->
  timeout_ms:int ->
  rlimit:int ->
  dependency_files:string list ->
  consumer_file:string ->
  (verification_report, error) result


val error_to_string : error -> string
val error_diagnostic : error -> Diagnostic.t option
val provenance : environment -> provenance list
val handles : environment -> handle list
val find_handle : environment -> string -> handle option
val handle_is_authentic : handle -> bool

val handle_unit_name : handle -> string
val handle_interface_digest : handle -> string
val handle_mode_signature_digest : handle -> string
val handle_transitive_dependencies : handle -> handle list

val public_types : handle -> public_type list
val public_type_id : public_type -> Sst.type_id
val public_type_visibility : public_type -> visibility
val public_type_kind : public_type -> Sst.type_kind option
val public_type_is_logical : public_type -> bool
val public_type_field_modes :
  public_type -> (Sst.field_id * Sst.instance_mode) list

val public_callables : handle -> public_callable list
val find_public_callable :
  handle -> Sst.function_id -> public_callable option
val public_callable_id : public_callable -> Sst.function_id
val public_callable_mode : public_callable -> Sst.verification_mode
val public_callable_parameter_types : public_callable -> Sst.typ list
val public_callable_parameter_modes :
  public_callable -> Sst.instance_mode list
val public_callable_finite_formals : public_callable -> int list
val public_callable_result_type : public_callable -> Sst.typ
val public_callable_result_mode : public_callable -> Sst.instance_mode
val public_callable_contract : public_callable -> public_contract

val contract_requires : public_contract -> public_clause list
val contract_ensures : public_contract -> public_clause list
val contract_decreases : public_contract -> public_clause list
val clause_index : public_clause -> int
val clause_span : public_clause -> Diagnostic.span
val clause_expression : public_clause -> Sst.expression

val public_models : handle -> public_model list
val find_public_model :
  handle -> Sst.function_id -> public_model option
val public_model_callable : public_model -> public_callable
val public_model_domain : public_model -> Sst.type_id
val public_model_result_type : public_model -> Sst.typ

val public_invariants : handle -> public_invariant list
val public_invariant_id : public_invariant -> string
val public_invariant_abstract_type : public_invariant -> Sst.type_id
val public_invariant_certificate_id : public_invariant -> string
val public_invariant_model : public_invariant -> Sst.function_id
val public_invariant_model_snapshot_type : public_invariant -> Sst.typ
val public_invariant_predicate : public_invariant -> Sst.function_id
val public_invariant_digest : public_invariant -> string
val public_invariant_operations :
  public_invariant ->
  (Sst.function_id * Sst.abstract_operation_role) list
