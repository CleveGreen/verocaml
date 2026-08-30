let fail format = Printf.ksprintf failwith format

let require condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "load %s [%s]: %s" filename diagnostic.code diagnostic.message

let configuration ~threads ~rlimit =
  match
    Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit
  with
  | Ok configuration -> configuration
  | Error error ->
      fail "configuration: %s"
        (Verifier_service.configuration_error_message error)

let verify ~configuration ~consumer ~dependencies =
  Verifier_service.request ~configuration ~consumer ~dependencies
  |> Verifier_service.verify

let require_result = function
  | Ok result -> result
  | Error error ->
      fail "verification%s: %s"
        (Option.fold ~none:""
           ~some:(fun unit_name -> " unit=" ^ unit_name)
           (Verifier_service.error_unit_name error))
        (Verifier_service.error_message error)

let snapshot result =
  ( Verifier_service.status result,
    Verifier_service.semantic_sst result,
    Verifier_service.vir result |> Vir.to_string,
    Verifier_service.functions result,
    Verifier_service.obligations result )

let configuration_matrix consumer =
  let default_one =
    verify ~configuration:(configuration ~threads:1 ~rlimit:None) ~consumer
      ~dependencies:[]
    |> require_result
  in
  let explicit_one =
    verify
      ~configuration:(configuration ~threads:1 ~rlimit:(Some 3_000_000))
      ~consumer ~dependencies:[]
    |> require_result
  in
  let default_two =
    verify ~configuration:(configuration ~threads:2 ~rlimit:None) ~consumer
      ~dependencies:[]
    |> require_result
  in
  require
    (snapshot default_one = snapshot explicit_one)
    "default and explicit rlimit results differ";
  require
    (snapshot default_one = snapshot default_two)
    "serial and higher-thread results differ";
  require
    (Verifier_service.status default_one = Verifier_service.Verified)
    "matrix input did not verify";
  Printf.printf
    "configuration=mandatory validation=threads/timeout/rlimit policy=once \
     routes=serial/higher rlimit=default/explicit parity=exact\n"

let invalid_configuration_matrix () =
  let rejects result =
    match result with
    | Ok _ -> false
    | Error error ->
        String.length
          (Verifier_service.configuration_error_message error)
        > 0
  in
  require
    (rejects
       (Verifier_service.configuration ~threads:0 ~timeout_ms:60_000
          ~rlimit:None))
    "zero threads accepted";
  require
    (rejects
       (Verifier_service.configuration ~threads:1 ~timeout_ms:0 ~rlimit:None))
    "zero timeout accepted";
  require
    (rejects
       (Verifier_service.configuration ~threads:1 ~timeout_ms:60_000
          ~rlimit:(Some 0)))
    "zero rlimit accepted";
  print_endline "configuration-errors=threads/timeout/rlimit opaque=messages"

let provenance_matrix consumer dependency =
  let result =
    verify ~configuration:(configuration ~threads:1 ~rlimit:None) ~consumer
      ~dependencies:[ dependency ]
    |> require_result
  in
  let provenance = Verifier_service.provenance result in
  require (List.length provenance = 1) "expected one provider provenance";
  let provider = List.hd provenance in
  require
    (String.equal
       (Verifier_service.provenance_unit_name provider)
       dependency.Cmt_input.unit_name)
    "provider unit projection differs";
  require
    (String.length
       (Verifier_service.provenance_interface_digest provider)
    > 0)
    "provider digest projection is empty";
  ignore (Verifier_service.provenance_direct_dependencies provider);
  ignore (Verifier_service.provenance_transitive_dependencies provider);
  Printf.printf
    "provenance=unit/digest/direct/transitive count=%d status=verified\n"
    (List.length provenance)

let inspect_function function_ =
  require
    (String.length (Verifier_service.function_name function_) > 0)
    "empty function name";
  ignore (Verifier_service.function_index function_)

let inspect_kind = function
  | Verifier_service.Arithmetic_safety
      { operation; mathematical_result; violated_bound } ->
      ignore operation;
      ignore mathematical_result;
      ignore violated_bound
  | Assertion { assertion_ordinal }
  | Local_assertion { local_assertion_ordinal = assertion_ordinal }
  | Postcondition { postcondition_ordinal = assertion_ordinal } ->
      ignore assertion_ordinal
  | Call_precondition { callee; precondition_ordinal } ->
      inspect_function callee;
      ignore precondition_ordinal
  | Callback_precondition { callback_name; callback_id } ->
      ignore callback_name; ignore callback_id
  | Invariant_validity { invariant_id; boundary } ->
      ignore invariant_id;
      ignore boundary
  | Entry_measure_nonnegative -> ()
  | Recursive_call_measure_nonnegative { callee }
  | Recursive_call_strict_descent { callee } ->
      inspect_function callee

let diagnostic_matrix consumer =
  let result =
    verify ~configuration:(configuration ~threads:1 ~rlimit:None) ~consumer
      ~dependencies:[]
    |> require_result
  in
  require
    (Verifier_service.status result = Verifier_service.Counterexample)
    "failure input did not produce a counterexample";
  let diagnostics = Verifier_service.diagnostics result in
  require (diagnostics <> []) "counterexample has no diagnostics";
  List.iter
    (fun diagnostic ->
      Verifier_service.diagnostic_function diagnostic |> inspect_function;
      Verifier_service.diagnostic_kind diagnostic |> inspect_kind;
      ignore (Verifier_service.diagnostic_span diagnostic);
      (match Verifier_service.diagnostic_outcome diagnostic with
      | Diagnostic_counterexample -> ()
      | Diagnostic_inconclusive
          { configured_timeout_ms; configured_rlimit; reason } ->
          ignore configured_timeout_ms;
          ignore configured_rlimit;
          ignore reason);
      Verifier_service.diagnostic_model_bindings diagnostic
      |> List.iter (fun binding ->
             ignore (Verifier_service.model_binding_source_name binding);
             ignore (Verifier_service.model_binding_symbol_id binding);
             ignore (Verifier_service.model_binding_value binding)))
    diagnostics;
  Printf.printf
    "diagnostics=function/kind/span/outcome/model count=%d status=counterexample\n"
    (List.length diagnostics)

let trusted_matrix consumer =
  let result =
    verify ~configuration:(configuration ~threads:1 ~rlimit:None) ~consumer
      ~dependencies:[]
    |> require_result
  in
  let observations =
    Verifier_service.trusted_external_observations result
  in
  require (observations <> []) "trusted input has no observations";
  List.iter
    (fun observation ->
      match Verifier_service.trusted_external_view observation with
      | Trusted_external_specification_use use ->
          inspect_function use.target;
          inspect_function use.wrapper;
          ignore use.target_span;
          ignore use.wrapper_span;
          ignore use.witness_span;
          ignore use.call_span;
          ignore use.requires_count;
          ignore use.ensures_count
      | Trusted_external_target_specification_use use ->
          ignore use.consumer_artifact_digest;
          ignore use.target_unit;
          ignore use.target_interface_digest;
          ignore use.import_crc;
          ignore use.canonical_path;
          ignore use.value_uid;
          ignore use.callable_abi_digest;
          inspect_function use.wrapper;
          ignore use.target_span;
          ignore use.wrapper_span;
          ignore use.witness_span;
          ignore use.call_span;
          ignore use.summary_digest;
          ignore use.requires_count;
          ignore use.ensures_count
      | Trusted_external_body_use use ->
          ignore use.proof_call;
          ignore use.broadcast_use;
          inspect_function use.function_;
          ignore use.declaration_span;
          ignore use.witness_span;
          ignore use.call_span;
          ignore use.requires_count;
          ignore use.ensures_count
      | Trusted_external_body_declaration declaration ->
          ignore declaration.proof_mode;
          inspect_function declaration.function_;
          ignore declaration.declaration_span;
          ignore declaration.witness_span;
          ignore declaration.requires_count;
          ignore declaration.ensures_count)
    observations;
  Printf.printf "trusted-observations=immutable count=%d views=complete\n"
    (List.length observations)

let error_matrix consumer unused_dependency =
  match
    verify ~configuration:(configuration ~threads:1 ~rlimit:None) ~consumer
      ~dependencies:[ unused_dependency ]
  with
  | Ok _ -> fail "unused dependency request unexpectedly verified"
  | Error error ->
      ignore (Verifier_service.error_unit_name error);
      require
        (String.length (Verifier_service.error_message error) > 0)
        "service error message is empty";
      ignore (Verifier_service.error_diagnostic error);
      print_endline "errors=unit/message/diagnostic opaque=projected"

let () =
  match Array.to_list Sys.argv with
  | [ _; verified; failed; trusted; consumer; dependency ] ->
      let verified = load verified
      and failed = load failed
      and trusted = load trusted
      and consumer = load consumer
      and dependency = load dependency in
      invalid_configuration_matrix ();
      configuration_matrix verified;
      provenance_matrix consumer dependency;
      diagnostic_matrix failed;
      trusted_matrix trusted;
      error_matrix verified dependency
  | _ ->
      fail
        "usage: %s VERIFIED.cmt FAILED.cmt TRUSTED.cmt CONSUMER.cmt \
         DEPENDENCY.cmt"
        Sys.argv.(0)
