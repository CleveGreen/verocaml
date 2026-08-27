type configuration
type configuration_error
type request
type result
type error
type function_ref
type provenance
type diagnostic
type model_binding
type trusted_external_observation
type scope_role = Scope_root | Scope_dependency
type scoped_plan_error
type scoped_request
type scoped_result
type scoped_row

type scoped_classification =
  | Scoped_verified
  | Scoped_verified_dependency
  | Scoped_skipped

type scoped_outcome =
  | Scoped_verification of result
  | Scoped_dependency_success
  | Scoped_skip
  | Scoped_rejection of error

type status =
  | Verified
  | Counterexample
  | Inconclusive
  | Incomplete_source

type operation =
  | Add
  | Subtract
  | Negate
  | Multiply_constant of string
  | Successor
  | Predecessor
  | Absolute_value

type violated_bound = Lower | Upper

type invariant_boundary =
  | Constructor_establishment
  | Transition_preservation
  | Call_argument
  | Call_result
  | Function_return
  | Shared_invariant_close
  | Terminal_observation

type diagnostic_kind =
  | Arithmetic_safety of {
      operation : operation;
      mathematical_result : string;
      violated_bound : violated_bound;
    }
  | Assertion of { assertion_ordinal : int }
  | Local_assertion of { local_assertion_ordinal : int }
  | Postcondition of { postcondition_ordinal : int }
  | Call_precondition of {
      callee : function_ref;
      precondition_ordinal : int;
    }
  | Callback_precondition of { callback_name : string; callback_id : int }
  | Invariant_validity of {
      invariant_id : string;
      boundary : invariant_boundary;
    }
  | Entry_measure_nonnegative
  | Recursive_call_measure_nonnegative of { callee : function_ref }
  | Recursive_call_strict_descent of { callee : function_ref }

type model_value =
  | Integer of string
  | Boolean of bool
  | Aggregate_identity of string

type inconclusive_reason =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type diagnostic_outcome =
  | Diagnostic_counterexample
  | Diagnostic_inconclusive of {
      configured_timeout_ms : int;
      configured_rlimit : int;
      reason : inconclusive_reason;
    }

type trusted_external_view =
  | Trusted_external_specification_use of {
      target : function_ref;
      wrapper : function_ref;
      target_span : Diagnostic.span;
      wrapper_span : Diagnostic.span;
      witness_span : Diagnostic.span;
      call_span : Diagnostic.span;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_target_specification_use of {
      consumer_artifact_digest : string;
      target_unit : string;
      target_interface_digest : string;
      import_crc : string;
      canonical_path : string;
      value_uid : string;
      callable_abi_digest : string;
      wrapper : function_ref;
      target_span : Diagnostic.span;
      wrapper_span : Diagnostic.span;
      witness_span : Diagnostic.span;
      call_span : Diagnostic.span;
      summary_digest : string;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_body_use of {
      proof_call : bool;
      function_ : function_ref;
      declaration_span : Diagnostic.span;
      witness_span : Diagnostic.span;
      call_span : Diagnostic.span;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_body_declaration of {
      proof_mode : bool;
      function_ : function_ref;
      declaration_span : Diagnostic.span;
      witness_span : Diagnostic.span;
      requires_count : int;
      ensures_count : int;
    }

val configuration :
  threads:int ->
  timeout_ms:int ->
  rlimit:int option ->
  (configuration, configuration_error) Stdlib.result

val configuration_error_message : configuration_error -> string

val request :
  configuration:configuration ->
  consumer:Cmt_input.implementation ->
  dependencies:Cmt_input.implementation list ->
  request

val scoped_request :
  configuration:configuration ->
  inventory:(scope_role * string * string * Cmt_input.implementation) list ->
  (scoped_request, scoped_plan_error) Stdlib.result

val scoped_plan_error_unit_name : scoped_plan_error -> string option
val scoped_plan_error_message : scoped_plan_error -> string

val verify : request -> (result, error) Stdlib.result
val verify_scope : scoped_request -> scoped_result
val scoped_rows : scoped_result -> scoped_row list
val scoped_row_classification : scoped_row -> scoped_classification
val scoped_row_outcome : scoped_row -> scoped_outcome
val scoped_row_unit_name : scoped_row -> string
val scoped_row_cmt : scoped_row -> string
val error_unit_name : error -> string option
val error_message : error -> string
val error_diagnostic : error -> Diagnostic.t option

val status : result -> status
val semantic_sst : result -> string
val vir : result -> Vir.program
val functions : result -> int
val obligations : result -> int

val provenance : result -> provenance list
val provenance_unit_name : provenance -> string
val provenance_interface_digest : provenance -> string
val provenance_direct_dependencies : provenance -> (string * string) list
val provenance_transitive_dependencies : provenance -> (string * string) list

val diagnostics : result -> diagnostic list
val diagnostic_function : diagnostic -> function_ref
val function_name : function_ref -> string
val function_index : function_ref -> int
val diagnostic_kind : diagnostic -> diagnostic_kind
val diagnostic_span : diagnostic -> Diagnostic.span
val diagnostic_outcome : diagnostic -> diagnostic_outcome
val diagnostic_model_bindings : diagnostic -> model_binding list
val model_binding_source_name : model_binding -> string
val model_binding_symbol_id : model_binding -> int
val model_binding_value : model_binding -> model_value option

val trusted_external_observations :
  result -> trusted_external_observation list

val trusted_external_view :
  trusted_external_observation -> trusted_external_view
