type visibility = Revealed | Abstract

type public_clause = {
  clause_index : int;
  clause_span : Diagnostic.span;
  clause_expression : Sst.expression;
}

type public_contract = {
  requires : public_clause list;
  ensures : public_clause list;
  decreases : public_clause list;
}

type public_callable = {
  definition : Sst.function_definition;
  callable_id : Sst.function_id;
  callable_mode : Sst.verification_mode;
  parameter_types : Sst.typ list;
  parameter_modes : Sst.instance_mode list;
  finite_formals : int list;
  result_type : Sst.typ;
  result_mode : Sst.instance_mode;
  contract : public_contract;
}

type public_type = {
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
  type_id : Sst.type_id;
  source_name : string;
  visibility : visibility;
  kind : Sst.type_kind option;
  logical : bool;
  field_modes : (Sst.field_id * Sst.instance_mode) list;
}

type public_model = {
  callable : public_callable;
  domain : Sst.type_id;
  result_type : Sst.typ;
}

type public_invariant = {
  invariant_id : string;
  abstract_type : Sst.type_id;
  certificate_id : string;
  model_callable : Sst.function_id;
  model_snapshot_type : Sst.typ;
  predicate_callable : Sst.function_id;
  predicate_digest : string;
  public_operations : (Sst.function_id * Sst.abstract_operation_role) list;
}

type handle = {
  issuer : unit ref;
  nonserializable : unit -> unit;
  unit_name : string;
  interface_digest : string;
  mode_signature_digest : string;
  types : public_type list;
  callables : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  direct_dependencies : handle list;
  transitive_dependencies : handle list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  private_implementation : Cmt_input.implementation;
  source_digest : string;
  family_digest : string;
  import_digest : string;
}

type environment = { issuer : unit ref; handles : handle list }
type verification_report = { report_driver : Verification_driver_private.report }
type provenance = {
  unit_name : string;
  interface_digest : string;
  direct_dependencies : (string * string) list;
  transitive_dependencies : (string * string) list;
}
type error = { unit_name : string option; message : string }

type staged_dependency = {
  candidate : Cmt_input.implementation;
  interface_digest : string;
  mode_signature_digest : string;
  types : public_type list;
  callables : public_callable list;
  models : public_model list;
  invariants : public_invariant list;
  semantic_snapshot : Sst_validation.validated_program;
  private_driver_completion : Verification_driver_private.completion;
  direct_dependencies : staged_dependency list;
}

val process_issuer : unit ref
val error : ?unit_name:string -> ?diagnostic:Diagnostic.t -> string -> ('a, error) result
val error_to_string : error -> string
val error_diagnostic : error -> Diagnostic.t option
val require_handle : handle -> unit
val require_environment : environment -> unit
val handle_is_authentic : handle -> bool
val same_function_id : Sst.function_id -> Sst.function_id -> bool
val unique_handles : handle list -> handle list
val construct_handle : staged_dependency -> handle list -> handle list -> handle
val imported_environment : environment -> (Imported_callable.environment, string) result
val imported_environment_of_staged : staged_dependency list -> (Imported_callable.environment, string) result
val provenance : environment -> provenance list
