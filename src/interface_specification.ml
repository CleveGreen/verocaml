module Environment = Interface_specification_environment_private
module Loaded = Interface_specification_loaded_private

type visibility = Environment.visibility = Revealed | Abstract
type public_clause = Environment.public_clause
type public_contract = Environment.public_contract
type public_callable = Environment.public_callable
type public_type = Environment.public_type
type public_model = Environment.public_model
type public_invariant = Environment.public_invariant
type handle = Environment.handle
type environment = Environment.environment
type verification_report = Environment.verification_report
type provenance = Environment.provenance = {
  unit_name : string;
  interface_digest : string;
  direct_dependencies : (string * string) list;
  transitive_dependencies : (string * string) list;
}
type error = Environment.error = { unit_name : string option; message : string }

let error_to_string = Environment.error_to_string
let error_diagnostic = Environment.error_diagnostic
let handle_is_authentic = Environment.handle_is_authentic

let policy_error_to_interface_error policy_error =
  {
    unit_name = None;
    message = Solver_policy_private.error_to_string policy_error;
  }

let resolve_default_solver_policy ~timeout_ms =
  Solver_policy_private.create_default ~timeout_ms
  |> Result.map_error policy_error_to_interface_error

let resolve_explicit_solver_policy ~timeout_ms ~rlimit =
  Solver_policy_private.create ~timeout_ms ~rlimit
  |> Result.map_error policy_error_to_interface_error

let handle_unit_name (handle : handle) =
  Environment.require_handle handle;
  handle.unit_name

let handle_interface_digest (handle : handle) =
  Environment.require_handle handle;
  handle.interface_digest

let handle_mode_signature_digest (handle : handle) =
  Environment.require_handle handle;
  handle.mode_signature_digest

let handle_transitive_dependencies (handle : handle) =
  Environment.require_handle handle;
  handle.transitive_dependencies

let handles (environment : environment) =
  Environment.require_environment environment;
  environment.handles

let find_handle environment unit_name =
  List.find_opt
    (fun handle -> String.equal (handle_unit_name handle) unit_name)
    (handles environment)

let public_types handle =
  Environment.require_handle handle;
  handle.types

let public_type_id (descriptor : public_type) = descriptor.Environment.type_id
let public_type_visibility (descriptor : public_type) =
  descriptor.Environment.visibility
let public_type_kind (descriptor : public_type) = descriptor.Environment.kind
let public_type_is_logical (descriptor : public_type) =
  descriptor.Environment.logical
let public_type_field_modes (descriptor : public_type) =
  descriptor.Environment.field_modes

let public_callables handle =
  Environment.require_handle handle;
  handle.callables

let find_public_callable handle function_id =
  List.find_opt
    (fun callable -> Environment.same_function_id callable.Environment.callable_id function_id)
    (public_callables handle)

let public_callable_id (callable : public_callable) =
  callable.Environment.callable_id
let public_callable_mode (callable : public_callable) =
  callable.Environment.callable_mode
let public_callable_parameter_types (callable : public_callable) =
  callable.Environment.parameter_types
let public_callable_parameter_modes (callable : public_callable) =
  callable.Environment.parameter_modes
let public_callable_finite_formals (callable : public_callable) =
  callable.Environment.finite_formals
let public_callable_result_type (callable : public_callable) =
  callable.Environment.result_type
let public_callable_result_mode (callable : public_callable) =
  callable.Environment.result_mode
let public_callable_contract (callable : public_callable) =
  callable.Environment.contract
let contract_requires (contract : public_contract) = contract.Environment.requires
let contract_ensures (contract : public_contract) = contract.Environment.ensures
let contract_decreases (contract : public_contract) = contract.Environment.decreases
let clause_index (clause : public_clause) = clause.Environment.clause_index
let clause_span (clause : public_clause) = clause.Environment.clause_span
let clause_expression (clause : public_clause) =
  clause.Environment.clause_expression

let public_models handle =
  Environment.require_handle handle;
  handle.models

let find_public_model handle function_id =
  List.find_opt
    (fun model ->
      Environment.same_function_id model.Environment.callable.callable_id function_id)
    (public_models handle)

let public_model_callable (model : public_model) = model.Environment.callable
let public_model_domain (model : public_model) = model.Environment.domain
let public_model_result_type (model : public_model) = model.Environment.result_type

let public_invariants handle =
  Environment.require_handle handle;
  handle.invariants

let public_invariant_id (invariant : public_invariant) =
  invariant.Environment.invariant_id
let public_invariant_abstract_type (invariant : public_invariant) =
  invariant.Environment.abstract_type
let public_invariant_certificate_id (invariant : public_invariant) =
  invariant.Environment.certificate_id
let public_invariant_model (invariant : public_invariant) =
  invariant.Environment.model_callable
let public_invariant_model_snapshot_type (invariant : public_invariant) =
  invariant.Environment.model_snapshot_type
let public_invariant_predicate (invariant : public_invariant) =
  invariant.Environment.predicate_callable
let public_invariant_digest (invariant : public_invariant) =
  invariant.Environment.predicate_digest
let public_invariant_operations (invariant : public_invariant) =
  invariant.Environment.public_operations

let load_inputs
    ~dependency_files:
      (dependency_files
        [@delator.field (fun files -> string_of_int (List.length files))])
    ~consumer_file:(consumer_file [@delator.skip])
    ~consumer_error:(consumer_error [@delator.skip]) =
  let rec load_dependencies loaded = function
    | [] ->
        [%log.debug "completed interface specification dependency loading"
          ~stage:(Delator.Field.string "interface-input-load")
          ~input_role:(Delator.Field.string "dependency")
          ~dependency_count:(Delator.Field.int (List.length loaded))
          ~decision:(Delator.Field.string "accepted")];
        Ok (List.rev loaded)
    | filename :: rest -> (
        match Cmt_input.load filename with
        | Ok dependency -> load_dependencies (dependency :: loaded) rest
        | Error diagnostic ->
            [%log.warn "rejected interface specification dependency input"
              ~stage:(Delator.Field.string "interface-input-load")
              ~input_role:(Delator.Field.string "dependency")
              ~dependency_ordinal:(Delator.Field.int (List.length loaded))
              ~dependency_count:(Delator.Field.int (List.length dependency_files))
              ~diagnostic_code:(Delator.Field.string diagnostic.Diagnostic.code)
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "artifact-load")];
            Environment.error ~diagnostic
              (Printf.sprintf "dependency CMT %S rejected" filename))
  in
  match Cmt_input.load consumer_file with
  | Error diagnostic ->
      [%log.warn "rejected interface specification consumer input"
        ~stage:(Delator.Field.string "interface-input-load")
        ~input_role:(Delator.Field.string "consumer")
        ~dependency_count:(Delator.Field.int (List.length dependency_files))
        ~diagnostic_code:(Delator.Field.string diagnostic.Diagnostic.code)
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "artifact-load")];
      consumer_error diagnostic
  | Ok consumer ->
      Result.map (fun dependencies -> (consumer, dependencies))
        (load_dependencies [] dependency_files)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let authentication_consumer_error consumer_file diagnostic =
  Environment.error
    (Printf.sprintf "consumer CMT %S rejected [%s]: %s" consumer_file
       diagnostic.Diagnostic.code diagnostic.message)

let authenticate_with_policy ~solver_policy ~dependency_files ~consumer_file =
  match
    load_inputs ~dependency_files ~consumer_file
      ~consumer_error:(authentication_consumer_error consumer_file)
  with
  | Error _ as error -> error
  | Ok (consumer, dependencies) ->
      Result.map fst
        (Loaded.authenticate ~external_targets:[] ~solver_policy ~dependencies
           ~consumer)

let authenticate ~timeout_ms ~dependency_files ~consumer_file =
  match resolve_default_solver_policy ~timeout_ms with
  | Error _ as error -> error
  | Ok solver_policy ->
      authenticate_with_policy ~solver_policy ~dependency_files ~consumer_file

let authenticate_with_rlimit ~timeout_ms ~rlimit ~dependency_files ~consumer_file =
  match resolve_explicit_solver_policy ~timeout_ms ~rlimit with
  | Error _ as error -> error
  | Ok solver_policy ->
      authenticate_with_policy ~solver_policy ~dependency_files ~consumer_file

let verification_consumer_error ~dependency_files consumer_file diagnostic =
  let message =
    Printf.sprintf "consumer CMT %S rejected [%s]: %s" consumer_file
      diagnostic.Diagnostic.code diagnostic.message
  in
  if dependency_files = [] then Environment.error ~diagnostic message
  else Environment.error message

let verify_with_policy ~threads ~solver_policy ~dependency_files ~consumer_file =
  match
    load_inputs ~dependency_files ~consumer_file
      ~consumer_error:
        (verification_consumer_error ~dependency_files consumer_file)
  with
  | Error _ as error -> error
  | Ok (consumer, dependencies) -> (
      let external_targets =
        List.filter
          (fun candidate -> not (Cmt_input.retained_preprocessing candidate))
          dependencies
      in
      match
        External_target_specification_private.environment ~consumer
          ~targets:external_targets
      with
      | Error message -> Environment.error ~unit_name:consumer.unit_name message
      | Ok external_specifications -> (
          match
            Loaded.verify ~threads ~solver_policy
              ~external_specifications:(Some external_specifications)
              ~external_targets ~consumer ~dependencies
          with
          | Error _ as error -> error
          | Ok result -> Ok { Environment.report_driver = Loaded.driver result }))

let verify_consumer ~timeout_ms ~dependency_files ~consumer_file =
  match resolve_default_solver_policy ~timeout_ms with
  | Error _ as error -> error
  | Ok solver_policy ->
      verify_with_policy ~threads:1 ~solver_policy ~dependency_files ~consumer_file

let verify_consumer_with_rlimit ~timeout_ms ~rlimit ~dependency_files ~consumer_file =
  match resolve_explicit_solver_policy ~timeout_ms ~rlimit with
  | Error _ as error -> error
  | Ok solver_policy ->
      verify_with_policy ~threads:1 ~solver_policy ~dependency_files ~consumer_file

let verify_consumer_with_threads ~threads ~timeout_ms ~dependency_files ~consumer_file =
  match Physical_core_count_private.validate_threads threads with
  | Error message -> Environment.error message
  | Ok () -> (
      match resolve_default_solver_policy ~timeout_ms with
      | Error _ as error -> error
      | Ok solver_policy ->
          verify_with_policy ~threads ~solver_policy ~dependency_files ~consumer_file)

let verify_consumer_with_rlimit_and_threads ~threads ~timeout_ms ~rlimit
    ~dependency_files ~consumer_file =
  match Physical_core_count_private.validate_threads threads with
  | Error message -> Environment.error message
  | Ok () -> (
      match resolve_explicit_solver_policy ~timeout_ms ~rlimit with
      | Error _ as error -> error
      | Ok solver_policy ->
          verify_with_policy ~threads ~solver_policy ~dependency_files ~consumer_file)

let provenance = Environment.provenance
