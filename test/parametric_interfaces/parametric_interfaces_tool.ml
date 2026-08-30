let fail format = Printf.ksprintf failwith format

let span = Diagnostic.file_span "parametric_interfaces_unit.ml"

let expression typ expression_desc = { Sst.expression_desc; typ; span }

let definition ~index ~name =
  let function_id = { Sst.function_index = index; function_name = name } in
  let owner = Parametric_type.owner ~index ~name in
  let binder = Parametric_type.binder owner ~ordinal:0 in
  let typ = Parametric_type.Parameter binder in
  let binding =
    { Sst.id = 0; name = "value"; typ;
      uniqueness = Sst.Definitely_aliased; span }
  in
  let pattern = { Sst.pattern_desc = Sst.Bind binding; typ; span } in
  let body =
    expression typ
      (Sst.Variable
         { binding; use_uniqueness = Sst.Definitely_aliased })
  in
  { Sst.function_id; type_binders = [ binder ]; mode = Sst.Exec;
    recursive = false;
    parameters = [ Sst.Value_parameter
      { Sst.label = None; pattern; optional_default = None } ];
    contracts = Sst.empty_contracts;
    body =
      Sst.Checked_exec
        { body = { Sst.stage = Sst.Runtime; expression = body };
          provenance =
            Sst.Authenticated_typedtree
              { source_file = span.file; declaration_span = span } };
    policy = Sst.Default_linear_z3; result_type = typ;
    returns_unique_parameter = None; span }

let create definition =
  Parametric_signature_private.create ~definition
    ~parameter_kinds:[ Parametric_signature_private.Positional_parameter ]
    ~parameter_modes:[ Sst.Exec_instance ] ~result_mode:Sst.Exec_instance
    ~recursive_evidence:None

let reject label = function
  | Ok _ -> fail "%s accepted" label
  | Error _ -> Printf.printf "%s=rejected\n" label

let signature_unit () =
  Verification_driver_private.For_testing.reset_driver_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let first = definition ~index:11 ~name:"Alpha.id" in
  let second =
    definition ~index:29 ~name:"Beta.id"
  in
  let first_signature =
    match create first with Ok value -> value | Error message -> fail "%s" message
  and second_signature =
    match create second with Ok value -> value | Error message -> fail "%s" message
  in
  if
    not
      (String.equal
         (Parametric_signature_private.semantic_fingerprint first_signature)
         (Parametric_signature_private.semantic_fingerprint second_signature))
    || not
         (String.equal
            (Parametric_signature_private.semantic_dump first_signature)
            (Parametric_signature_private.semantic_dump second_signature))
  then fail "alpha-canonical signatures differ";
  Printf.printf "alpha-semantic=equal fingerprint=%s\n"
    (Parametric_signature_private.semantic_fingerprint first_signature);
  let actual = expression Sst.Int (Sst.Int_constant Z.zero) in
  let valid =
    Parametric_signature_private.validate_call first_signature
      ~type_arguments:[ Sst.Int ] ~actual_result:Sst.Int
      ~arguments:[ (None, actual) ]
  in
  (match valid with
  | Ok instantiated ->
      if instantiated.result_type <> Sst.Int then fail "bad substitution";
      Printf.printf "complete-substitution=int\n"
  | Error message -> fail "%s" message);
  reject "missing-type-argument"
    (Parametric_signature_private.validate_call first_signature
       ~type_arguments:[] ~actual_result:Sst.Int
       ~arguments:[ (None, actual) ]);
  reject "extra-type-argument"
    (Parametric_signature_private.validate_call first_signature
       ~type_arguments:[ Sst.Int; Sst.Bool ] ~actual_result:Sst.Int
       ~arguments:[ (None, actual) ]);
  reject "wrong-label"
    (Parametric_signature_private.validate_call first_signature
       ~type_arguments:[ Sst.Int ] ~actual_result:Sst.Int
       ~arguments:[ (Some "forged", actual) ]);
  reject "wrong-result"
    (Parametric_signature_private.validate_call first_signature
       ~type_arguments:[ Sst.Int ] ~actual_result:Sst.Bool
       ~arguments:[ (None, actual) ]);
  let bad_owner =
    { first with
      Sst.type_binders =
        [ Parametric_type.binder
            (Parametric_type.owner ~index:11 ~name:"forged")
            ~ordinal:0 ] }
  in
  reject "binder-owner" (create bad_owner);
  let bad_ordinal =
    let owner = Parametric_type.owner ~index:11 ~name:"Alpha.id" in
    { first with
      Sst.type_binders = [ Parametric_type.binder owner ~ordinal:1 ] }
  in
  reject "binder-ordinal" (create bad_ordinal);
  let second_binder =
    Parametric_type.binder
      (Parametric_type.owner ~index:11 ~name:"Alpha.id")
      ~ordinal:1
  in
  let ordered = { first with Sst.type_binders = first.type_binders @ [ second_binder ] } in
  (match create ordered with
  | Ok _ -> ()
  | Error message -> fail "ordered binder vector rejected: %s" message);
  reject "binder-order"
    (create { ordered with Sst.type_binders = List.rev ordered.type_binders });
  reject "mode-vector"
    (Parametric_signature_private.create ~definition:first
       ~parameter_kinds:[ Parametric_signature_private.Positional_parameter ]
       ~parameter_modes:[] ~result_mode:Sst.Exec_instance
       ~recursive_evidence:None);
  reject "label-default-vector"
    (Parametric_signature_private.create ~definition:first
       ~parameter_kinds:[ Parametric_signature_private.Labelled_parameter ]
       ~parameter_modes:[ Sst.Exec_instance ] ~result_mode:Sst.Exec_instance
       ~recursive_evidence:None);
  let recursive = { first with Sst.recursive = true } in
  reject "missing-recursion-evidence" (create recursive);
  reject "forged-nonempty-recursion-evidence"
    (Parametric_signature_private.For_testing.forged_recursive_signature
       ~definition:recursive
       ~parameter_kinds:
         [ Parametric_signature_private.Positional_parameter ]
       ~parameter_modes:[ Sst.Exec_instance ]
       ~result_mode:Sst.Exec_instance);
  List.iter
    (fun attack ->
      match Imported_callable.For_testing.run_abi_attack attack with
      | Error _ -> ()
      | Ok () ->
          fail "retained ABI attack accepted: %s"
            (Imported_callable.For_testing.abi_attack_name attack))
    Imported_callable.For_testing.abi_attacks;
  Printf.printf "first-order-abi-negatives=%d names=%s\n"
    (List.length Imported_callable.For_testing.abi_attacks)
    (Imported_callable.For_testing.abi_attacks
    |> List.map Imported_callable.For_testing.abi_attack_name
    |> String.concat ",");
  let counters = Z3_bridge.counters () in
  Printf.printf "pre-sst-vir driver=%d pipeline=%d solver=%d z3=%d/%d\n"
    (Verification_driver_private.For_testing.driver_entries ())
    (Verification_pipeline.For_testing.validated_pipeline_entries ())
    (Solver_backend_counter_private.solver_creation_count ())
    counters.contexts_created counters.solvers_created

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message

let imported dependency consumer =
  let dependency = load dependency and consumer = load consumer in
  let policy =
    match Solver_policy_private.create_default ~timeout_ms:60000 with
    | Ok policy -> policy
    | Error error -> fail "%s" (Solver_policy_private.error_to_string error)
  in
  let environment, _ =
    match
      Interface_specification_loaded_private.authenticate ~external_targets:[]
        ~solver_policy:policy
        ~dependencies:[ dependency ] ~consumer
    with
    | Ok result -> result
    | Error error ->
        fail "%s"
          (Interface_specification_environment_private.error_to_string error)
  in
  match
    Interface_specification_environment_private.imported_environment
      environment
  with
  | Ok environment -> environment
  | Error message -> fail "%s" message

let sole_signature environment =
  match Imported_callable.callables environment with
  | [ callable ] -> callable
  | callables -> fail "expected one callable, got %d" (List.length callables)

let alpha_artifacts dependency_a consumer_a dependency_b consumer_b =
  let first = sole_signature (imported dependency_a consumer_a)
  and second = sole_signature (imported dependency_b consumer_b) in
  let first_semantic =
    Parametric_signature_private.semantic_fingerprint first.signature
  and second_semantic =
    Parametric_signature_private.semantic_fingerprint second.signature
  in
  if not (String.equal first_semantic second_semantic) then
    fail "alpha provider semantic fingerprints differ";
  if String.equal first.summary_digest second.summary_digest then
    fail "distinct artifacts share an exact summary digest";
  Printf.printf
    "alpha-artifacts semantic=equal provenance=distinct units=%s/%s\n"
    first.provider_unit second.provider_unit

let artifact_snapshot environment =
  Imported_callable.callables environment
  |> List.map (fun (callable : Imported_callable.callable_snapshot) ->
         ( callable.path,
           Parametric_signature_private.semantic_fingerprint callable.signature,
           Parametric_signature_private.semantic_dump callable.signature,
           callable.summary_digest ))
  |> List.sort compare

let authority_negatives dependency consumer =
  let environment = imported dependency consumer in
  Verification_driver_private.For_testing.reset_driver_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  List.iter
    (fun attack ->
      let name = Imported_callable.For_testing.authority_attack_name attack in
      match Imported_callable.For_testing.run_authority_attack environment attack with
      | Error _ -> Printf.printf "%s=rejected boundary=pre-sst-vir-backend-solver-z3\n" name
      | Ok () -> fail "%s unexpectedly authorized" name)
    Imported_callable.For_testing.authority_attacks;
  (match Imported_callable.For_testing.stale_recursive_evidence environment with
  | Error _ ->
      print_endline
        "stale-recursive-body-evidence=rejected boundary=pre-sst-vir-backend-solver-z3"
  | Ok () -> fail "stale recursive body/evidence unexpectedly authorized");
  let counters = Z3_bridge.counters () in
  Printf.printf "authority-zero-work driver=%d pipeline=%d solver=%d z3=%d/%d\n"
    (Verification_driver_private.For_testing.driver_entries ())
    (Verification_pipeline.For_testing.validated_pipeline_entries ())
    (Solver_backend_counter_private.solver_creation_count ())
    counters.contexts_created counters.solvers_created

let artifact_determinism dependency consumer copied_dependency copied_consumer =
  let first = artifact_snapshot (imported dependency consumer) in
  let reloaded = artifact_snapshot (imported dependency consumer) in
  let copied = artifact_snapshot (imported copied_dependency copied_consumer) in
  if first <> reloaded then fail "reloaded signature snapshot changed";
  if first <> copied then fail "copied signature snapshot changed";
  Printf.printf
    "artifact-signatures callables=%d reload=deterministic copy=deterministic\n"
    (List.length first)

let () =
  ignore Parametric_interfaces_prerequisites.ready;
  match Array.to_list Sys.argv with
  | [ _; "signature-unit" ] -> signature_unit ()
  | [ _; "alpha-artifacts"; dependency_a; consumer_a; dependency_b;
      consumer_b ] ->
      alpha_artifacts dependency_a consumer_a dependency_b consumer_b
  | [ _; "artifact-determinism"; dependency; consumer; copied_dependency;
      copied_consumer ] ->
      artifact_determinism dependency consumer copied_dependency copied_consumer
  | [ _; "authority-negatives"; dependency; consumer ] ->
      authority_negatives dependency consumer
  | _ ->
      fail
        "usage: parametric_interfaces_tool signature-unit|alpha-artifacts|artifact-determinism|authority-negatives ..."
