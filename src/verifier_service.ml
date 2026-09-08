type configuration = {
  threads : int;
  solver_policy : Solver_policy_private.t;
}

type configuration_error = string

type request = {
  configuration : configuration;
  consumer : Cmt_input.implementation;
  dependencies : Cmt_input.implementation list;
}

type function_ref = {
  function_name : string;
  function_index : int;
}

type provenance = {
  unit_name : string;
  interface_digest : string;
  direct_dependencies : (string * string) list;
  transitive_dependencies : (string * string) list;
}

type status =
  | Verified
  | Counterexample
  | Inconclusive
  | Incomplete_source

type operation =
  | Add
  | Subtract
  | Negate
  | Multiply
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
  | Bit_vector of Bv_value.t

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

type model_binding = {
  source_name : string;
  symbol_id : int;
  source_span : Diagnostic.span;
  value : model_value option;
}

type native_bv_provenance =
  Numeric_bv_projection_evidence_private.source_observation

type native_bv_result_outcome =
  | Native_bv_verified
  | Native_bv_counterexample
  | Native_bv_inconclusive of {
      configured_timeout_ms : int;
      configured_rlimit : int;
      reason : inconclusive_reason;
    }

type native_bv_result_provenance = {
  function_ : function_ref;
  obligation_identity : string;
  obligation_span : Diagnostic.span;
  outcome : native_bv_result_outcome;
  source_observations : native_bv_provenance list;
}

type diagnostic = {
  function_ : function_ref;
  kind : diagnostic_kind;
  span : Diagnostic.span;
  outcome : diagnostic_outcome;
  model_bindings : model_binding list;
  native_bv_provenance : native_bv_provenance list;
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
      broadcast_use : bool;
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

type trusted_external_observation = trusted_external_view

type result = {
  status : status;
  semantic_sst : string Lazy.t;
  vir : Vir.program;
  functions : int;
  obligations : int;
  provenance : provenance list;
  diagnostics : diagnostic list;
  native_bv_provenance : native_bv_provenance list;
  native_bv_results : native_bv_result_provenance list;
  trusted_external_observations : trusted_external_observation list;
}

type error = Interface_specification_loaded_private.error
type error_classification = Source_error | Dependency_error | Internal_error

let classify_error error =
  match Interface_specification_loaded_private.error_diagnostic error with
  | Some diagnostic -> (
      match Diagnostic.failure_class diagnostic.Diagnostic.classification with
      | Diagnostic.Source_failure -> Source_error
      | Diagnostic.Artifact_failure -> Dependency_error
      | Diagnostic.Internal_failure -> Internal_error)
  | None
    when Interface_specification_loaded_private.error_is_internal error ->
      Internal_error
  | None -> Dependency_error

type scope_role = Scope_root | Scope_dependency

type scoped_plan_error = Verification_scope_private.error

type scoped_request = {
  scoped_configuration : configuration;
  scoped_plan : Verification_scope_private.plan;
}

type scoped_classification =
  | Scoped_verified
  | Scoped_verified_dependency
  | Scoped_skipped

type scoped_outcome =
  | Scoped_verification of result
  | Scoped_dependency_success
  | Scoped_skip
  | Scoped_rejection of error

type scoped_row = {
  scoped_artifact : Verification_scope_private.artifact;
  scoped_classification : scoped_classification;
  scoped_outcome : scoped_outcome;
}

type scoped_result = { scoped_rows : scoped_row list }

let configuration ~threads ~timeout_ms ~rlimit =
  match Physical_core_count_private.validate_threads threads with
  | Error message -> Error message
  | Ok () ->
      let policy =
        match rlimit with
        | None -> Solver_policy_private.create_default ~timeout_ms
        | Some rlimit -> Solver_policy_private.create ~timeout_ms ~rlimit
      in
      Result.map
        (fun solver_policy -> { threads; solver_policy })
        policy
      |> Result.map_error Solver_policy_private.error_to_string

let configuration_error_message error = error

let request ~configuration ~consumer ~dependencies =
  { configuration; consumer; dependencies }

let function_ref (value : Vir.function_ref) =
  {
    function_name = value.function_name;
    function_index = value.function_index;
  }

let operation = function
  | Vir.Add -> Add
  | Subtract -> Subtract
  | Negate -> Negate
  | Multiply -> Multiply
  | Multiply_constant value -> Multiply_constant (Z.to_string value)
  | Successor -> Successor
  | Predecessor -> Predecessor
  | Absolute_value -> Absolute_value

let invariant_boundary = function
  | Vir.Constructor_establishment -> Constructor_establishment
  | Transition_preservation _ -> Transition_preservation
  | Call_argument _ -> Call_argument
  | Call_result _ -> Call_result
  | Function_return -> Function_return
  | Shared_invariant_close _ -> Shared_invariant_close
  | Terminal_observation _ -> Terminal_observation

let diagnostic_kind = function
  | Vir.Arithmetic_safety
      { operation = value; mathematical_result; violated_bound } ->
      Arithmetic_safety
        {
          operation = operation value;
          mathematical_result =
            Vir.integer_term_to_string mathematical_result;
          violated_bound =
            (match violated_bound with Lower_bound -> Lower | Upper_bound -> Upper);
        }
  | Assertion { assertion_ordinal } -> Assertion { assertion_ordinal }
  | Local_assertion { local_assertion_ordinal } ->
      Local_assertion { local_assertion_ordinal }
  | Postcondition { postcondition_ordinal; _ } ->
      Postcondition { postcondition_ordinal }
  | Call_precondition { callee; precondition_ordinal; _ } ->
      Call_precondition
        { callee = function_ref callee; precondition_ordinal }
  | Callback_precondition { callback; _ } ->
      Callback_precondition
        { callback_name = callback.callback_name; callback_id = callback.callback_id }
  | Invariant_validity { invariant_id; boundary; _ } ->
      Invariant_validity
        { invariant_id; boundary = invariant_boundary boundary }
  | Entry_measure_nonnegative _ -> Entry_measure_nonnegative
  | Recursive_call_measure_nonnegative { callee; _ } ->
      Recursive_call_measure_nonnegative { callee = function_ref callee }
  | Recursive_call_strict_descent { callee; _ } ->
      Recursive_call_strict_descent { callee = function_ref callee }

let model_value = function
  | Solver_backend.Integer value -> Integer (Z.to_string value)
  | Boolean value -> Boolean value
  | Aggregate_identity value -> Aggregate_identity (Z.to_string value)
  | Bit_vector value -> Bit_vector value

let model_binding (binding : Solver_backend.model_binding) =
  {
    source_name = binding.symbol.source_name;
    symbol_id = binding.symbol.symbol_id;
    source_span = binding.symbol.span;
    value = Option.map model_value binding.value;
  }

let inconclusive_reason = function
  | Solver_backend.Resource_exhausted -> Resource_exhausted
  | Timed_out -> Timed_out
  | Backend_unknown detail -> Backend_unknown detail

let diagnostic (value : Solver_backend.obligation_result) =
  let native_bv_provenance =
    Vir.obligation_native_bv_source_observations value.obligation
  in
  match value.outcome with
  | Solver_backend.Verified -> None
  | Counterexample bindings ->
      Some
        {
          function_ = function_ref value.obligation.function_ref;
          kind = diagnostic_kind value.obligation.kind;
          span = value.obligation.span;
          outcome = Diagnostic_counterexample;
          model_bindings = List.map model_binding bindings;
          native_bv_provenance;
        }
  | Inconclusive { configured_timeout_ms; configured_rlimit; reason } ->
      Some
        {
          function_ = function_ref value.obligation.function_ref;
          kind = diagnostic_kind value.obligation.kind;
          span = value.obligation.span;
          outcome =
            Diagnostic_inconclusive
              {
                configured_timeout_ms;
                configured_rlimit;
                reason = inconclusive_reason reason;
              };
          model_bindings = [];
          native_bv_provenance;
        }

let native_bv_result_outcome = function
  | Solver_backend.Verified -> Native_bv_verified
  | Counterexample _ -> Native_bv_counterexample
  | Inconclusive { configured_timeout_ms; configured_rlimit; reason } ->
      Native_bv_inconclusive
        { configured_timeout_ms; configured_rlimit;
          reason = inconclusive_reason reason }

let associate_native_bv_results
    (results : Solver_backend.obligation_result list) =
  let associations =
    results
    |> List.filter_map (fun (result : Solver_backend.obligation_result) ->
           let source_observations =
             Vir.obligation_native_bv_source_observations result.obligation
           in
           if source_observations = [] then None
           else
             Some
               { function_ = function_ref result.obligation.function_ref;
                 obligation_identity =
                   Vir_identity_private.obligation result.obligation;
                 obligation_span = result.obligation.span;
                 outcome = native_bv_result_outcome result.outcome;
                 source_observations })
  in
  let[@log_value.debug] source_occurrences =
    List.fold_left
      (fun count association ->
        count + List.length association.source_observations)
      0 associations
  in
  [%log.debug "associated native BV source provenance with coordinator results"
    ~stage:(Delator.Field.string "verification-service-provenance")
    ~coordinator_results:(Delator.Field.int (List.length results))
    ~associated_results:(Delator.Field.int (List.length associations))
    ~source_occurrences:
      (Delator.Field.int (source_occurrences [@log_value.debug]))
    ~decision:(Delator.Field.string "preserved-original-results")];
  associations
[@@delator.instrument] [@@delator.level debug]

let native_bv_provenance native_bv_results =
  native_bv_results
  |> List.concat_map (fun result -> result.source_observations)
  |> List.sort_uniq (fun (left : native_bv_provenance) right ->
         String.compare left.occurrence_identity right.occurrence_identity)

let trusted_external_observations (vir : Vir.program) =
  let uses =
    List.concat_map
      (fun execution -> execution.Vir.trusted_summary_uses)
      vir.functions
    |> List.map (function
         | Vir.Trusted_external_specification_use use ->
             Trusted_external_specification_use
               {
                 target = function_ref use.target;
                 wrapper = function_ref use.wrapper;
                 target_span = use.target_span;
                 wrapper_span = use.wrapper_span;
                 witness_span = use.witness_span;
                 call_span = use.call_span;
                 requires_count = use.requires_count;
                 ensures_count = use.ensures_count;
               }
         | Vir.Trusted_external_target_specification_use use ->
             Trusted_external_target_specification_use
               {
                 consumer_artifact_digest = use.consumer_artifact_digest;
                 target_unit = use.target_unit;
                 target_interface_digest = use.target_interface_digest;
                 import_crc = use.import_crc;
                 canonical_path = use.canonical_path;
                 value_uid = use.value_uid;
                 callable_abi_digest = use.callable_abi_digest;
                 wrapper = function_ref use.wrapper;
                 target_span = use.target_span;
                 wrapper_span = use.wrapper_span;
                 witness_span = use.witness_span;
                 call_span = use.call_span;
                 summary_digest = use.summary_digest;
                 requires_count = use.requires_count;
                 ensures_count = use.ensures_count;
               }
         | Vir.Trusted_external_body_use use ->
             Trusted_external_body_use
               {
                 proof_call =
                   use.mode = Sst.Proof && use.call_form = Sst.Proof_call;
                 broadcast_use = use.broadcast_use;
                 function_ = function_ref use.function_ref;
                 declaration_span = use.declaration_span;
                 witness_span = use.witness_span;
                 call_span = use.call_span;
                 requires_count = use.requires_count;
                 ensures_count = use.ensures_count;
               })
  in
  let uses =
    List.fold_left
      (fun observations observation ->
        match observation with
        | Trusted_external_body_use use when use.broadcast_use ->
            if
              List.exists
                (function
                  | Trusted_external_body_use existing
                    when existing.broadcast_use ->
                      existing.function_ = use.function_
                      && existing.declaration_span = use.declaration_span
                      && existing.witness_span = use.witness_span
                  | Trusted_external_specification_use _
                  | Trusted_external_target_specification_use _
                  | Trusted_external_body_use _
                  | Trusted_external_body_declaration _ ->
                      false)
                observations
            then observations
            else observation :: observations
        | Trusted_external_specification_use _
        | Trusted_external_target_specification_use _
        | Trusted_external_body_use _
        | Trusted_external_body_declaration _ ->
            observation :: observations)
      [] uses
    |> List.rev
  in
  let declarations =
    List.map
      (fun (declaration : Vir.trusted_external_body_declaration) ->
        Trusted_external_body_declaration
          {
            proof_mode = declaration.mode = Sst.Proof;
            function_ = function_ref declaration.function_ref;
            declaration_span = declaration.declaration_span;
            witness_span = declaration.witness_span;
            requires_count = declaration.requires_count;
            ensures_count = declaration.ensures_count;
          })
      vir.trusted_external_body_declarations
  in
  uses @ declarations

let status = function
  | Verification_pipeline.Verified -> Verified
  | Counterexample -> Counterexample
  | Inconclusive -> Inconclusive
  | Incomplete_source -> Incomplete_source

let verify_with_external ?external_specifications ?(external_targets = []) request =
  let configuration = request.configuration in
  match
    Interface_specification_loaded_private.verify
      ~threads:configuration.threads
      ~solver_policy:configuration.solver_policy ~consumer:request.consumer
      ~external_specifications ~external_targets ~dependencies:request.dependencies
  with
  | Error error ->
      [%log.debug "verification loading failed"
        ~unit_name:(Delator.Field.string request.consumer.unit_name)
        ~stage:(Delator.Field.string "verification-load")
        ~failure_class:
          (Delator.Field.string
             (match classify_error error with
             | Source_error -> "source"
             | Dependency_error -> "artifact"
             | Internal_error -> "internal"))
        ~diagnostic_code:
          (Delator.Field.string
             (Interface_specification_loaded_private.error_diagnostic error
             |> Option.map (fun diagnostic -> diagnostic.Diagnostic.code)
             |> Option.value ~default:
                  (match classify_error error with
                  | Source_error -> "VERO_SOURCE"
                  | Dependency_error -> "VERO_DEPENDENCY"
                  | Internal_error -> "VERO_INTERNAL")))
        ~decision:(Delator.Field.string "rejected")];
      Error error
  | Ok loaded ->
      let driver = Interface_specification_loaded_private.driver loaded in
      let vir = Verification_driver_private.vir driver in
      let provenance =
        Interface_specification_loaded_private.provenance loaded
        |> List.map
             (fun
               ( unit_name,
                 interface_digest,
                 direct_dependencies,
                 transitive_dependencies ) ->
               {
                 unit_name;
                 interface_digest;
                 direct_dependencies;
                 transitive_dependencies;
               })
      in
      let result =
        let obligation_results = Verification_driver_private.results driver in
        let native_bv_results = associate_native_bv_results obligation_results in
        {
          status = status (Verification_driver_private.status driver);
          semantic_sst =
            Verification_driver_private.semantic_sst_lazy driver;
          vir;
          functions = Verification_driver_private.functions driver;
          obligations = Verification_driver_private.obligations driver;
          provenance;
          diagnostics = obligation_results |> List.filter_map diagnostic;
          native_bv_provenance = native_bv_provenance native_bv_results;
          native_bv_results;
          trusted_external_observations = trusted_external_observations vir;
        }
      in
      [%log.info "verification completed"
        ~unit_name:(Delator.Field.string request.consumer.unit_name)
        ~outcome:
          (Delator.Field.string
             (match result.status with
             | Verified -> "verified"
             | Counterexample -> "counterexample"
             | Inconclusive -> "inconclusive"
             | Incomplete_source -> "incomplete-source"))
        ~functions:(Delator.Field.int result.functions)
        ~obligations:(Delator.Field.int result.obligations)
        ~native_bv_source_occurrences:
          (Delator.Field.int (List.length result.native_bv_provenance))
        ~native_bv_result_associations:
          (Delator.Field.int (List.length result.native_bv_results))
        ~dependency_count:(Delator.Field.int (List.length request.dependencies))];
      Ok result
[@@delator.instrument] [@@delator.level info]

let verify request =
  let external_targets =
    List.filter
      (fun candidate -> not (Cmt_input.retained_preprocessing candidate))
      request.dependencies
  in
  match
    External_target_specification_private.environment ~consumer:request.consumer
      ~targets:external_targets
  with
  | Error message ->
      Interface_specification_loaded_private.error
        ~unit_name:request.consumer.unit_name message
  | Ok external_specifications ->
      verify_with_external ~external_specifications ~external_targets request
[@@delator.instrument] [@@delator.level info]

let scoped_request ~configuration ~inventory =
  let artifacts =
    List.map
      (fun (role, cmt, cmi, implementation) ->
        let role =
          match role with
          | Scope_root -> Verification_scope_private.Root
          | Scope_dependency -> Dependency
        in
        Verification_scope_private.artifact ~role ~cmt ~cmi implementation)
      inventory
  in
  Result.map
    (fun scoped_plan -> { scoped_configuration = configuration; scoped_plan })
    (Verification_scope_private.plan artifacts)

let scoped_plan_error_unit_name =
  Verification_scope_private.error_unit_name

let scoped_plan_error_message = Verification_scope_private.error_message

let verify_scoped_root ~external_targets request consumer dependencies =
  let verification_request =
    {
      configuration = request.scoped_configuration;
      consumer;
      dependencies =
        List.map Verification_scope_private.implementation dependencies;
    }
  in
  match
    External_target_specification_private.environment ~consumer
      ~targets:external_targets
  with
  | Ok external_specifications ->
      verify_with_external ~external_specifications ~external_targets
        verification_request
  | Error message ->
      Interface_specification_loaded_private.error ~unit_name:consumer.unit_name
        message

let verify_scope request =
  let plan = request.scoped_plan in
  let external_targets =
    Verification_scope_private.skipped plan
    |> List.map Verification_scope_private.implementation
  in
  let rec verify_roots completed = function
    | [] -> List.rev completed
    | artifact :: rest ->
        let dependencies =
          Verification_scope_private.dependencies_for_root plan artifact
        in
        let consumer = Verification_scope_private.implementation artifact in
        let verification =
          verify_scoped_root ~external_targets request consumer dependencies
        in
        verify_roots ((artifact, verification) :: completed) rest
  in
  let root_results =
    verify_roots [] (Verification_scope_private.roots plan)
  in
  let root_rows =
    List.map
      (fun (artifact, outcome) ->
        {
          scoped_artifact = artifact;
          scoped_classification = Scoped_verified;
          scoped_outcome =
            (match outcome with
            | Ok result -> Scoped_verification result
            | Error error -> Scoped_rejection error);
        })
      root_results
  in
  let dependency_rows =
    Verification_scope_private.dependencies plan
    |> List.map (fun artifact ->
           let name = Verification_scope_private.unit_name artifact in
           let rejection =
             List.find_map
               (function
                 | _, Error error
                   when Interface_specification_loaded_private.error_unit_name
                          error
                        = Some name ->
                     Some error
                 | _ -> None)
               root_results
           in
           {
             scoped_artifact = artifact;
             scoped_classification = Scoped_verified_dependency;
             scoped_outcome =
               Option.fold ~none:Scoped_dependency_success
                 ~some:(fun error -> Scoped_rejection error)
                 rejection;
           })
  in
  let skipped_rows =
    Verification_scope_private.skipped plan
    |> List.map (fun artifact ->
           {
             scoped_artifact = artifact;
             scoped_classification = Scoped_skipped;
             scoped_outcome = Scoped_skip;
           })
  in
  { scoped_rows = root_rows @ dependency_rows @ skipped_rows }
[@@delator.instrument] [@@delator.level info]

let error_unit_name = Interface_specification_loaded_private.error_unit_name
let error_message = Interface_specification_loaded_private.error_message
let error_diagnostic = Interface_specification_loaded_private.error_diagnostic
let error_is_internal = Interface_specification_loaded_private.error_is_internal
let error_classification = classify_error
let status result = result.status
let semantic_sst result = Lazy.force result.semantic_sst
let vir result = result.vir
let functions result = result.functions
let obligations result = result.obligations
let provenance result = result.provenance
let provenance_unit_name value = value.unit_name
let provenance_interface_digest value = value.interface_digest
let provenance_direct_dependencies value = value.direct_dependencies
let provenance_transitive_dependencies value = value.transitive_dependencies
let diagnostics result = result.diagnostics
let native_bv_provenance result = result.native_bv_provenance
let native_bv_results result = result.native_bv_results
let native_bv_result_function (value : native_bv_result_provenance) =
  value.function_
let native_bv_result_obligation_identity
    (value : native_bv_result_provenance) =
  value.obligation_identity
let native_bv_result_obligation_span (value : native_bv_result_provenance) =
  value.obligation_span
let native_bv_result_outcome (value : native_bv_result_provenance) = value.outcome
let native_bv_result_source_observations
    (value : native_bv_result_provenance) =
  value.source_observations
let diagnostic_function value = value.function_
let function_name value = value.function_name
let function_index value = value.function_index
let diagnostic_kind value = value.kind
let diagnostic_span value = value.span
let diagnostic_outcome value = value.outcome
let diagnostic_model_bindings value = value.model_bindings
let diagnostic_native_bv_provenance (value : diagnostic) =
  value.native_bv_provenance
let model_binding_source_name value = value.source_name
let model_binding_symbol_id value = value.symbol_id
let model_binding_source_span value = value.source_span
let model_binding_value value = value.value

let trusted_external_observations result =
  result.trusted_external_observations

let trusted_external_view value = value

let scoped_rows result = result.scoped_rows
let scoped_row_classification row = row.scoped_classification
let scoped_row_outcome row = row.scoped_outcome
let scoped_row_unit_name row =
  Verification_scope_private.unit_name row.scoped_artifact
let scoped_row_cmt row = Verification_scope_private.cmt row.scoped_artifact

module For_testing = struct
  let native_bv_results = associate_native_bv_results
end
