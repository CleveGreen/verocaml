let fail format = Printf.ksprintf failwith format
let require condition format = if not condition then fail format

let load cmt cmi =
  match Cmt_input.load_with_interface ~cmt ~cmi () with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "load %s/%s failed [%s]: %s" cmt cmi diagnostic.Diagnostic.code
        diagnostic.message

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
  | Ok configuration -> configuration
  | Error error ->
      fail "%s" (Verifier_service.configuration_error_message error)

let request ~threads root_cmt root_cmi dependency_cmt dependency_cmi =
  let root = load root_cmt root_cmi
  and dependency = load dependency_cmt dependency_cmi in
  let inventory =
    [
      (Verifier_service.Scope_root, root_cmt, root_cmi, root);
      (Verifier_service.Scope_dependency, dependency_cmt, dependency_cmi, dependency);
    ]
  in
  match
    Verifier_service.scoped_request ~configuration:(configuration threads)
      ~inventory
  with
  | Ok request -> (request, root, dependency)
  | Error error ->
      fail "scope failed: %s"
        (Verifier_service.scoped_plan_error_message error)

let rows result =
  Verifier_service.scoped_rows result
  |> List.map (fun row -> (Verifier_service.scoped_row_unit_name row, row))

let positive ~expected_uses threads root_cmt root_cmi dependency_cmt
    dependency_cmi =
  let request, root, dependency =
    request ~threads root_cmt root_cmi dependency_cmt dependency_cmi
  in
  Interface_specification_loaded_private.For_testing
  .reset_provider_verification_entries ();
  External_target_specification_private.For_testing.reset_live_registrations ();
  Typedtree_lowering_private.For_testing.reset_lowering_entries ();
  let result = Verifier_service.verify_scope request in
  let rows = rows result in
  let consumer = List.assoc root.Cmt_input.unit_name rows
  and target = List.assoc dependency.Cmt_input.unit_name rows in
  let verified =
    match
      ( Verifier_service.scoped_row_classification consumer,
        Verifier_service.scoped_row_outcome consumer )
    with
    | Scoped_verified, Scoped_verification result
      when Verifier_service.status result = Verifier_service.Verified -> result
    | _ -> fail "consumer was not verified"
  in
  (match
     ( Verifier_service.scoped_row_classification target,
       Verifier_service.scoped_row_outcome target )
   with
  | Scoped_skipped, Scoped_skip -> ()
  | _ -> fail "ordinary external target was not skipped");
  let external_target_uses =
    Verifier_service.trusted_external_observations verified
    |> List.fold_left
         (fun count observation ->
           match Verifier_service.trusted_external_view observation with
           | Trusted_external_target_specification_use use ->
               require
                 (String.equal use.target_unit dependency.unit_name
                 && String.equal use.target_interface_digest use.import_crc)
                 "imported trust identity mismatch";
               count + 1
           | Trusted_external_specification_use _
           | Trusted_external_body_use _
           | Trusted_external_body_declaration _ -> count)
         0
  in
  require
    (external_target_uses = expected_uses)
    "unexpected external-target trust-use count";
  require
    (Typedtree_lowering_private.For_testing.lowering_entries () = 1)
    "external target entered typedtree adaptation or semantic lowering";
  require
    (Interface_specification_loaded_private.For_testing
     .provider_verification_entries () = 0)
    "ordinary external target entered verified-provider processing";
  require
    (External_target_specification_private.For_testing.live_registrations () = 0)
    "private external registration leaked";
  let completion_root = load root_cmt root_cmi
  and completion_dependency = load dependency_cmt dependency_cmi in
  let external_specifications =
    match
      External_target_specification_private.environment ~consumer:completion_root
        ~targets:[ completion_dependency ]
    with
    | Ok environment -> environment
    | Error message -> fail "completion environment: %s" message
  in
  let solver_policy =
    match Solver_policy_private.create_default ~timeout_ms:60_000 with
    | Ok policy -> policy
    | Error error -> fail "%s" (Solver_policy_private.error_to_string error)
  in
  let completion_driver =
    match
      Interface_specification_loaded_private.verify ~threads ~solver_policy
        ~external_specifications:(Some external_specifications)
        ~consumer:completion_root
        ~dependencies:[]
    with
    | Ok loaded -> Interface_specification_loaded_private.driver loaded
    | Error error ->
        fail "completion probe: %s"
          (Interface_specification_loaded_private.error_message error)
  in
  require
    (Verification_driver_private.status completion_driver
    = Verification_pipeline.Verified)
    "completion probe did not verify";
  require
    (Option.is_none
       (Verification_driver_private.verified_completion completion_driver))
    "verified external specification minted a verified-provider completion";
  require
    (External_target_specification_private.For_testing.live_registrations () = 0)
    "completion probe leaked private external registration";
  Printf.printf
    "positive threads=%d consumer=verified target=skipped uses=%d functions=%d obligations=%d target-cmt=discovered target-semantic-lowering=0 verified-provider-processing=0 completion=none live-authority=0\n"
    threads external_target_uses (Verifier_service.functions verified)
    (Verifier_service.obligations verified)

let rejected root_cmt root_cmi dependency_cmt dependency_cmi =
  let request, root, dependency =
    request ~threads:1 root_cmt root_cmi dependency_cmt dependency_cmi
  in
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Imported_callable.For_testing.reset_aggregate_lifecycle ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Interface_specification_loaded_private.For_testing
  .reset_provider_verification_entries ();
  External_target_specification_private.For_testing.reset_live_registrations ();
  Typedtree_lowering_private.For_testing.reset_lowering_entries ();
  let sst_boundaries = ref 0 in
  let result =
    Sst_validation_private.Public.For_testing
    .with_program_mutation_at_validation_boundary ~mutate:Fun.id
      ~observe:(fun () -> incr sst_boundaries)
      (fun () -> Verifier_service.verify_scope request)
  in
  let rows = rows result in
  let consumer = List.assoc root.Cmt_input.unit_name rows
  and target = List.assoc dependency.Cmt_input.unit_name rows in
  let trusted_uses =
    List.fold_left
      (fun count (_, row) ->
        match Verifier_service.scoped_row_outcome row with
        | Scoped_verification verification ->
            count
            + List.length
                (Verifier_service.trusted_external_observations verification)
        | Scoped_dependency_success | Scoped_rejection _ | Scoped_skip -> count)
      0 rows
  in
  (match Verifier_service.scoped_row_outcome consumer with
  | Scoped_rejection _ -> ()
  | _ -> fail "negative consumer was not rejected");
  (match
     ( Verifier_service.scoped_row_classification target,
       Verifier_service.scoped_row_outcome target )
   with
  | Scoped_skipped, Scoped_skip -> ()
  | _ -> fail "negative ordinary external target was not skipped");
  let z3 = Z3_bridge.counters () in
  let imported_callable =
    Imported_callable.For_testing.aggregate_lifecycle ()
  in
  let symbolic =
    Symbolic_executor_private.For_testing.authority_observation ()
  in
  require
    (!sst_boundaries = 0
    && Typedtree_lowering_private.For_testing.lowering_entries () = 1
    && Verification_pipeline.For_testing.validated_pipeline_entries () = 0
    && Solver_backend.For_testing.solver_creation_count () = 0
    && z3.capability_resolutions = 0 && z3.translations = 0
    && z3.contexts_created = 0 && z3.solvers_created = 0)
    "negative reached SST/VIR/backend/solver/Z3";
  require
    (Interface_specification_loaded_private.For_testing
     .provider_verification_entries () = 0)
    "negative entered target-body semantic verification";
  require
    (External_target_specification_private.For_testing.live_registrations () = 0)
    "negative leaked private authority";
  require (trusted_uses = 0) "negative emitted trusted external authority";
  require
    (imported_callable.descriptors_issued = 0
    && imported_callable.applications_admitted = 0
    && imported_callable.descriptors_invalidated = 0
    && symbolic.recursive_spec_lowerings = 0
    && symbolic.recursive_proof_rank_lowerings = 0
    && Recursive_spec_encoding.For_testing.recursive_lowering_count () = 0)
    "negative minted retained, recursive, rank, or symbolic authority";
  Printf.printf
    "%s rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0\n"
    root.unit_name

let metadata consumer_cmt consumer_cmi target_cmt target_cmi =
  let consumer = load consumer_cmt consumer_cmi
  and target = load target_cmt target_cmi in
  let environment =
    match
      External_target_specification_private.environment ~consumer
        ~targets:[ target ]
    with
    | Ok environment -> environment
    | Error message -> fail "environment: %s" message
  in
  let rejected path uid =
    Result.is_error
      (External_target_specification_private.resolve_candidate environment
         ~canonical_path:path ~value_uid:uid)
  in
  require (rejected "Wrong.promised" "[intf]Legacy.0")
    "wrong unit/path resolved";
  require (rejected "Legacy.missing" "[intf]Legacy.0")
    "wrong value resolved";
  require (rejected "Legacy.promised" "forged-uid") "wrong UID resolved";
  Printf.printf
    "metadata-negatives wrong-unit/path/value/uid=rejected pre-summary-authority\n"

let authority consumer_cmt consumer_cmi target_cmt target_cmi =
  let consumer = load consumer_cmt consumer_cmi
  and target = load target_cmt target_cmi in
  let environment =
    match
      External_target_specification_private.environment ~consumer
        ~targets:[ target ]
    with
    | Ok environment -> environment
    | Error message -> fail "environment: %s" message
  in
  External_target_specification_private.For_testing.reset_live_registrations ();
  let lowered =
    match
      Typedtree_lowering_private.lower
        ~allow_public_parametric_signatures:true
        ~external_specifications:environment ~imported:Imported_callable.empty
        consumer
    with
    | Ok lowered -> lowered
    | Error diagnostic ->
        fail "lowering [%s]: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let program = Typedtree_lowering_private.program lowered in
  let registration =
    match Typedtree_lowering_private.external_registration lowered with
    | Some registration -> registration
    | None -> fail "lowering minted no imported external registration"
  in
  let summaries =
    program.Sst.functions
    |> List.filter_map (fun definition ->
           match definition.Sst.body with
           | Sst.External_specification
               (Sst.Imported_unverified_target _ as link) ->
               Some (definition, link)
           | Sst.Checked_exec _ | Sst.Spec_definition _
           | Sst.Recursive_spec_definition _ | Sst.Proof_body _
           | Sst.External_specification _
           | Sst.Trusted_external_spec_target _
           | Sst.Trusted_external_body _
           | Sst.Symbolic_declaration _ -> None)
  in
  let copy_definition (definition : Sst.function_definition) =
    Marshal.from_string (Marshal.to_string definition []) 0
  in
  require (List.length summaries = 2) "expected two bound summaries";
  List.iter
    (fun (definition, link) ->
      require
        (External_target_specification_private.authenticates_definition
           registration ~program ~definition link)
        "exact registered definition did not authenticate";
      let copied = copy_definition definition in
      require
        (not
           (External_target_specification_private.authenticates_definition
              registration ~program ~definition:copied link))
        "copied definition authenticated")
    summaries;
  let copied_program =
    {
      program with
      Sst.functions =
        List.map
          copy_definition
          program.functions;
    }
  in
  List.iter
    (fun (definition, link) ->
      require
        (not
           (External_target_specification_private.authenticates_definition
              registration ~program:copied_program ~definition link))
        "copied/replayed program authenticated")
    summaries;
  let first_definition, first_link = List.nth summaries 0
  and _, second_link = List.nth summaries 1 in
  require
    (not
       (External_target_specification_private.authenticates_definition
          registration ~program ~definition:first_definition second_link))
    "swapped or alpha-equal cross-summary link authenticated";
  require
    (Result.is_error (Sst_validation.validate program))
    "raw public semantic validation accepted private imported authority";
  External_target_specification_private.invalidate registration;
  require
    (not
       (External_target_specification_private.authenticates_definition
          registration ~program ~definition:first_definition first_link))
    "stale registration authenticated";
  require
    (External_target_specification_private.For_testing.live_registrations () = 0)
    "authority test leaked registration";
  Printf.printf
    "authority-negatives raw/forged/copied/replayed/swapped/stale=rejected private-delta=0\n"

let alpha_cross consumer_cmt consumer_cmi alpha_cmt alpha_cmi target_cmt
    target_cmi =
  let target = load target_cmt target_cmi in
  let lower cmt cmi =
    let consumer = load cmt cmi in
    let environment =
      match
        External_target_specification_private.environment ~consumer
          ~targets:[ target ]
      with
      | Ok environment -> environment
      | Error message -> fail "environment: %s" message
    in
    let lowered =
      match
        Typedtree_lowering_private.lower
          ~allow_public_parametric_signatures:true
          ~external_specifications:environment ~imported:Imported_callable.empty
          consumer
      with
      | Ok lowered -> lowered
      | Error diagnostic ->
          fail "lowering [%s]: %s" diagnostic.Diagnostic.code diagnostic.message
    in
    let program = Typedtree_lowering_private.program lowered in
    let registration =
      match Typedtree_lowering_private.external_registration lowered with
      | Some registration -> registration
      | None -> fail "alpha lowering minted no registration"
    in
    let summary =
      program.Sst.functions
      |> List.find (fun definition ->
             match definition.Sst.body with
             | Sst.External_specification
                 (Sst.Imported_unverified_target
                   { canonical_path = "Legacy.select"; _ }) ->
                 true
             | Sst.Checked_exec _ | Sst.Spec_definition _
             | Sst.Recursive_spec_definition _ | Sst.Proof_body _
             | Sst.External_specification _
             | Sst.Trusted_external_spec_target _
             | Sst.Trusted_external_body _
             | Sst.Symbolic_declaration _ -> false)
    in
    let signature =
      match
        External_target_specification_private.signature_for_definition
          registration ~program summary
      with
      | Some signature -> signature
      | None -> fail "alpha summary has no exact signature"
    in
    (program, registration, summary, signature)
  in
  External_target_specification_private.For_testing.reset_live_registrations ();
  let first_program, first_registration, first_summary, first_signature =
    lower consumer_cmt consumer_cmi
  and second_program, second_registration, second_summary, second_signature =
    lower alpha_cmt alpha_cmi
  in
  require
    (String.equal
       (Parametric_signature_private.semantic_fingerprint first_signature)
       (Parametric_signature_private.semantic_fingerprint second_signature))
    "alpha-renamed summary ABIs were not semantically equal";
  let second_link =
    match second_summary.Sst.body with
    | Sst.External_specification link -> link
    | _ -> assert false
  in
  require
    (not
       (External_target_specification_private.authenticates_definition
          first_registration ~program:first_program ~definition:second_summary
          second_link))
    "alpha-equal cross-artifact summary authenticated";
  require
    (not
       (External_target_specification_private.authenticates_definition
          second_registration ~program:second_program ~definition:first_summary
          second_link))
    "swapped alpha-equal artifact authenticated";
  External_target_specification_private.invalidate first_registration;
  External_target_specification_private.invalidate second_registration;
  require
    (External_target_specification_private.For_testing.live_registrations () = 0)
    "alpha-cross test leaked registration";
  Printf.printf
    "alpha-cross-artifact substitution=rejected semantic-abi=equal private-delta=0\n"

let wrong_cmi consumer_cmt consumer_cmi target_cmt target_cmi =
  let consumer = load consumer_cmt consumer_cmi
  and target = load target_cmt target_cmi in
  let environment =
    match
      External_target_specification_private.environment ~consumer
        ~targets:[ target ]
    with
    | Ok environment -> environment
    | Error message -> fail "wrong-CMI environment: %s" message
  in
  require
    (Result.is_error
       (External_target_specification_private.resolve_candidate environment
          ~canonical_path:"Legacy.promised" ~value_uid:"[intf]Legacy.0"))
    "wrong target CMI supplied a candidate";
  Printf.printf "wrong-CMI/unit artifact=rejected pre-summary-authority\n"

let compiler_mode_marker_count implementation =
  let marker = "verocaml.internal.compiler_mode_syntax" in
  let count = ref 0 in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          List.iter
            (fun attribute ->
              if String.equal attribute.Parsetree.attr_name.txt marker then (
                require
                  (attribute.attr_loc.loc_ghost
                  && attribute.attr_name.loc.loc_ghost
                  &&
                  match attribute.attr_payload with PStr [] -> true | _ -> false)
                  "compiler-mode syntax marker was not ghost/internal/empty";
                incr count))
            binding.Typedtree.vb_attributes;
          default.value_binding self binding);
    }
  in
  iterator.structure iterator implementation.Cmt_input.structure;
  !count

let marker_provenance retained_cmt retained_cmi ordinary_cmt ordinary_cmi =
  let retained = load retained_cmt retained_cmi
  and ordinary = load ordinary_cmt ordinary_cmi in
  require
    (compiler_mode_marker_count retained = 1)
    "retained verifier input lacks its single compiler-mode syntax marker";
  require
    (compiler_mode_marker_count ordinary = 0)
    "ordinary CMT retained compiler-mode syntax provenance";
  Printf.printf
    "compiler-mode-syntax-marker retained-cmt=1 ordinary-cmt=0 \
     ghost-empty=authenticated\n"

let compiler_mode_compatibility () =
  let legacy : Mode.Alloc.lr = Mode.Alloc.of_const Mode.Alloc.Const.legacy in
  let unique : Mode.Alloc.lr =
    Mode.Alloc.of_const
      { Mode.Alloc.Const.legacy with uniqueness = Mode.Uniqueness.Const.Unique }
  in
  let target_parameter_modes = [ legacy; legacy ]
  and target_result_modes = [ legacy; legacy ] in
  let shared_open_wrapper, _ = Mode.Alloc.newvar_above legacy in
  let wrapper_parameter_modes = [ shared_open_wrapper; shared_open_wrapper ]
  and wrapper_result_modes = [ shared_open_wrapper; shared_open_wrapper ] in
  let open_target, _ = Mode.Alloc.newvar_above legacy in
  let open_target_parameter_modes = [ open_target; open_target ]
  and open_target_result_modes = [ open_target; open_target ] in
  let nondefault_target_parameter_modes = [ unique; unique ]
  and nondefault_target_result_modes = [ unique; unique ] in
  let originals =
    Marshal.to_string
      ( target_parameter_modes,
        target_result_modes,
        wrapper_parameter_modes,
        wrapper_result_modes,
        open_target_parameter_modes,
        open_target_result_modes,
        nondefault_target_parameter_modes,
        nondefault_target_result_modes )
      []
  in
  require
    (External_target_specification_private.For_testing
     .compiler_target_modes_are_exact_default
       ~parameter_modes:target_parameter_modes ~result_modes:target_result_modes)
    "closed compiler-default target graph was rejected";
  require
    (not
       (External_target_specification_private.For_testing
        .compiler_target_modes_are_exact_default
          ~parameter_modes:open_target_parameter_modes
          ~result_modes:open_target_result_modes))
    "open compiler target graph was accepted as exact default";
  require
    (not
       (External_target_specification_private.For_testing
        .compiler_target_modes_are_exact_default
          ~parameter_modes:nondefault_target_parameter_modes
          ~result_modes:nondefault_target_result_modes))
    "nondefault compiler target graph was accepted as exact default";
  require
    (External_target_specification_private.For_testing
     .compiler_callable_modes_are_compatible ~target_parameter_modes
       ~target_result_modes ~wrapper_parameter_modes ~wrapper_result_modes)
    "shared open wrapper graph was incompatible with exact default target";
  require
    (not
       (External_target_specification_private.For_testing
        .compiler_callable_modes_are_compatible ~target_parameter_modes
          ~target_result_modes ~wrapper_parameter_modes:[ shared_open_wrapper ]
          ~wrapper_result_modes))
    "callable mode shape mismatch was accepted";
  for incompatible_slot = 0 to 3 do
    let shared_open, _ = Mode.Alloc.newvar_above legacy in
    let slots = Array.make 4 shared_open in
    slots.(incompatible_slot) <- unique;
    let incompatible_parameter_modes = [ slots.(0); slots.(1) ]
    and incompatible_result_modes = [ slots.(2); slots.(3) ] in
    let before =
      Marshal.to_string
        (incompatible_parameter_modes, incompatible_result_modes)
        []
    in
    if
      External_target_specification_private.For_testing
      .compiler_callable_modes_are_compatible ~target_parameter_modes
        ~target_result_modes
        ~wrapper_parameter_modes:incompatible_parameter_modes
        ~wrapper_result_modes:incompatible_result_modes
    then fail "closed nondefault wrapper slot %d was accepted" incompatible_slot;
    if
      not
        (String.equal before
           (Marshal.to_string
              (incompatible_parameter_modes, incompatible_result_modes)
              []))
    then
      fail "compatibility check mutated incompatible wrapper graph %d"
        incompatible_slot
  done;
  let after =
    Marshal.to_string
      ( target_parameter_modes,
        target_result_modes,
        wrapper_parameter_modes,
        wrapper_result_modes,
        open_target_parameter_modes,
        open_target_result_modes,
        nondefault_target_parameter_modes,
        nondefault_target_result_modes )
      []
  in
  require
    (String.equal originals after)
    "target exactness or wrapper compatibility mutated original mode graphs";
  Printf.printf
    "compiler-callable-mode target-exact open/nondefault=rejected \
     wrapper-compatible shared-open=accepted \
     closed-nondefault=all-slots-rejected slots=2+2 originals=unchanged\n"

let () =
  match Array.to_list Sys.argv with
  | [ _; "positive"; threads; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      positive ~expected_uses:5 (int_of_string threads) root_cmt root_cmi
        dep_cmt dep_cmi
  | [ _; "curried-positive"; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      positive ~expected_uses:1 1 root_cmt root_cmi dep_cmt dep_cmi
  | [ _; "rejected"; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      rejected root_cmt root_cmi dep_cmt dep_cmi
  | [ _; "metadata"; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      metadata root_cmt root_cmi dep_cmt dep_cmi
  | [ _; "authority"; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      authority root_cmt root_cmi dep_cmt dep_cmi
  | [ _;
      "alpha-cross";
      root_cmt;
      root_cmi;
      alpha_cmt;
      alpha_cmi;
      dep_cmt;
      dep_cmi ] ->
      alpha_cross root_cmt root_cmi alpha_cmt alpha_cmi dep_cmt dep_cmi
  | [ _; "wrong-cmi"; root_cmt; root_cmi; dep_cmt; dep_cmi ] ->
      wrong_cmi root_cmt root_cmi dep_cmt dep_cmi
  | [ _;
      "marker-provenance";
      retained_cmt;
      retained_cmi;
      ordinary_cmt;
      ordinary_cmi ] ->
      marker_provenance retained_cmt retained_cmi ordinary_cmt ordinary_cmi
  | [ _; "compiler-mode-compatibility" ] -> compiler_mode_compatibility ()
  | _ -> fail "invalid external target specification tool invocation"
