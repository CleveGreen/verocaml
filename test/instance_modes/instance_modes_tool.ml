let () = ignore Instance_modes_prerequisites.ready

let fail format = Printf.ksprintf failwith format

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "adapter:%s:%s" diagnostic.Diagnostic.code diagnostic.message

let load_implementation filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "input:%s:%s" diagnostic.Diagnostic.code diagnostic.message

let reset_counters () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Sst_validation.For_testing.reset_ghost_formal_flow_count ()

let assert_zero_solvers () =
  let direct = Z3_bridge.counters () in
  if
    Solver_backend.For_testing.solver_creation_count () <> 0
    || direct.contexts_created <> 0 || direct.solvers_created <> 0
  then fail "semantic rejection reached a solver"

let modes filename =
  let program = load filename in
  match Sst_validation.validate program with
  | Error error -> fail "%s" (Sst_validation.error_to_string error)
  | Ok validated -> print_string (Sst_validation.instance_mode_dump validated)

let lower filename =
  let program = load filename in
  match Symbolic_executor.lower_program program with
  | Ok vir -> (program, vir)
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let sst filename =
  let program = load filename in
  print_string (Sst.to_string program)

let vir filename =
  let _, program = lower filename in
  print_string (Vir.to_string program)

let solve filename =
  let _, program = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5_000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Ok results
        when List.for_all
               (fun result ->
                 match result.Solver_backend.outcome with
                 | Verified -> true
                 | Counterexample _ | Inconclusive _ -> false)
               results ->
          Printf.printf "%s: verified (%d obligations)\n"
            execution.function_ref.function_name
            (List.length execution.obligations)
      | Ok _ -> fail "%s was not verified" execution.function_ref.function_name
      | Error error -> fail "%s" (Solver_backend.error_to_string error))
    program.functions

let reject_zero filename =
  reset_counters ();
  let disposition =
    match Typedtree_lowering.lower_file filename with
    | Error _ -> "adapter"
    | Ok program -> (
        match Sst_validation.validate program with
        | Error _ -> "semantic"
        | Ok _ -> fail "attack was accepted")
  in
  assert_zero_solvers ();
  Printf.printf "%s rejection; solvers=0\n" disposition

let reject_zero_flow filename =
  reset_counters ();
  let disposition =
    match Typedtree_lowering.lower_file filename with
    | Error _ -> "adapter"
    | Ok program -> (
        match Sst_validation.validate program with
        | Error _ -> "semantic"
        | Ok _ -> fail "attack was accepted")
  in
  assert_zero_solvers ();
  let flows = Sst_validation.For_testing.ghost_formal_flow_count () in
  if
    (String.equal disposition "semantic" && flows <> 1)
    || (String.equal disposition "adapter" && flows <> 0)
  then fail "attack rejected before its exact Ghost-formal flow";
  Printf.printf "%s rejection; ghost-formal-flows=%d; solvers=0\n"
    disposition flows

let reject_zero_no_flow filename =
  reset_counters ();
  let disposition =
    match Typedtree_lowering.lower_file filename with
    | Error _ -> "adapter"
    | Ok program -> (
        match Sst_validation.validate program with
        | Error _ -> "semantic"
        | Ok _ -> fail "attack was accepted")
  in
  assert_zero_solvers ();
  let flows = Sst_validation.For_testing.ghost_formal_flow_count () in
  if flows <> 0 then fail "rejection crossed a Ghost-formal forgetting edge";
  Printf.printf "%s rejection; ghost-formal-flows=0; solvers=0\n" disposition

let reject_raw_proof_body_no_authority filename =
  reset_counters ();
  let implementation = load_implementation filename in
  let mutations = ref 0 in
  let mutate (program : Sst.program) =
    let functions =
      List.map
        (fun (definition : Sst.function_definition) ->
          if String.equal definition.function_id.function_name "caller" then
            match definition.body with
            | Sst.Proof_body
                ({ provenance = Sst.Authenticated_typedtree _; _ } as proof) ->
                incr mutations;
                {
                  definition with
                  body =
                    Sst.Proof_body
                      {
                        proof with
                        provenance = Sst.Raw_semantic_body definition.span;
                      };
                }
            | Sst.Proof_body { provenance = Sst.Raw_semantic_body _; _ } ->
                fail "caller proof body was already raw"
            | Sst.Checked_exec _ | Sst.Spec_definition _
            | Sst.Recursive_spec_definition _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _
            | Sst.Trusted_external_body _
            | Sst.Symbolic_declaration _ ->
                fail "caller is not an authenticated Proof body"
          else definition)
        program.functions
    in
    if !mutations <> 1 then
      fail "expected exactly one caller Proof-body provenance mutation";
    let mutated = { program with Sst.functions = functions } in
    Instance_mode.prepare ~structure:implementation.structure ~program:mutated;
    (match Instance_mode.seal implementation mutated with
    | Ok () -> mutated
    | Error error ->
        fail "raw Proof-body mutation did not reseal: %s" error.message)
  in
  let observed = ref false in
  let result =
    Sst_validation_private.Public.For_testing
    .with_program_mutation_at_validation_boundary ~mutate
      ~observe:(fun () -> observed := true)
      (fun () ->
        Verification_driver_private.run ~timeout_ms:5_000
          ~allow_imported_opens:false implementation)
  in
  if not !observed then fail "mutation did not reach the validation boundary";
  (match result with
  | Error (Verification_driver_private.Validation_error _) -> ()
  | Error _ -> fail "raw Proof body rejected outside semantic validation"
  | Ok _ -> fail "raw Proof body selected bare-Tracked authority");
  assert_zero_solvers ();
  let flows = Sst_validation.For_testing.ghost_formal_flow_count () in
  if flows <> 0 then fail "raw Proof body crossed a Ghost-formal forgetting edge";
  let direct = Z3_bridge.counters () in
  Printf.printf
    "raw-proof-body rejection; boundary=validation; ghost-formal-flows=0; \
     backend-solvers=%d; z3-contexts=%d; z3-solvers=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    direct.contexts_created direct.solvers_created

let copied filename =
  reset_counters ();
  let program = load filename in
  let validated =
    match Sst_validation.validate program with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  if
    not
      (Sst_validation.For_testing
       .copied_instance_mode_descriptor_rejected validated)
  then fail "copied instance-mode descriptor retained authority";
  let copied =
    {
      program with
      Sst.functions =
        List.map
          (fun (definition : Sst.function_definition) ->
            { definition with Sst.span = definition.span })
          program.functions;
    }
  in
  (match Sst_validation.validate copied with
  | Error _ -> ()
  | Ok _ -> fail "copied SST was accepted");
  assert_zero_solvers ();
  print_endline "copied descriptor and SST rejected; solvers=0"

let dependency_modes consumer dependency =
  let environment =
    match
      Interface_specification.authenticate ~timeout_ms:5_000
        ~dependency_files:[ dependency ] ~consumer_file:consumer
    with
    | Ok environment -> environment
    | Error error ->
        fail "%s" (Interface_specification.error_to_string error)
  in
  let handle =
    match Interface_specification.handles environment with
    | [ handle ] -> handle
    | _ -> fail "expected one authenticated retained dependency"
  in
  let callable name =
    match
      List.find_opt
        (fun descriptor ->
          String.equal
            (Interface_specification.public_callable_id descriptor)
              .Sst.function_name
            name)
        (Interface_specification.public_callables handle)
    with
    | Some descriptor -> descriptor
    | None -> fail "missing public callable %s" name
  in
  let tracked = callable "tracked_id" in
  if
    Interface_specification.public_callable_parameter_modes tracked
    <> [ Sst.Tracked_instance ]
    || Interface_specification.public_callable_result_mode tracked
       <> Sst.Tracked_instance
  then fail "tracked dependency signature changed";
  let erased = callable "erased_exec" in
  if
    Interface_specification.public_callable_parameter_modes erased
    <> [ Sst.Ghost_instance ]
    || Interface_specification.public_callable_result_mode erased
       <> Sst.Ghost_instance
  then fail "erased executable dependency signature changed";
  let field_modes =
    Interface_specification.public_types handle
    |> List.concat_map Interface_specification.public_type_field_modes
    |> List.map snd
  in
  if
    not
      (List.mem Sst.Ghost_instance field_modes
      && List.mem Sst.Tracked_instance field_modes)
  then fail "dependency field modes are incomplete";
  if Interface_specification.public_invariants handle <> [] then
    fail "mode descriptors unexpectedly granted invariant facts";
  print_endline
    "same-invocation retained modes authenticated; invariant-facts=0"

let implementation_interface_digest (info : Cmt_format.cmt_infos) =
  match info.cmt_interface_digest with
  | Some digest -> digest
  | None ->
      let self = Compilation_unit.name info.cmt_modname in
      Array.find_map
        (fun imported ->
          if Compilation_unit.Name.equal (Import_info.name imported) self then
            Import_info.crc imported
          else None)
        info.cmt_imports
      |> Option.get

let embed_ordinary_interface retained ordinary_interface_file output =
  let ordinary_interface =
    try Cmi_format.read_cmi_lazy ordinary_interface_file
    with _ -> fail "%s is not a readable CMI" ordinary_interface_file
  in
  let retained_cmt =
    match Cmt_format.read retained with
    | _, Some cmt -> cmt
    | _ -> fail "%s has no implementation CMT" retained
  in
  let expected = implementation_interface_digest retained_cmt in
  let self_name = Compilation_unit.name retained_cmt.cmt_modname in
  let self_unit =
    Compilation_unit.of_string (Compilation_unit.Name.to_string self_name)
  in
  let interface =
    {
      ordinary_interface with
      Cmi_format.cmi_crcs =
        Array.map
          (fun imported ->
            if
              Compilation_unit.Name.equal
                (Import_info.name imported)
                self_name
            then
              Import_info.create self_name
                ~crc_with_unit:(Some (self_unit, expected))
            else imported)
          ordinary_interface.cmi_crcs;
    }
  in
  let channel = open_out_bin output in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () ->
      ignore (Cmi_format.output_cmi output channel interface);
      output_string channel Config.cmt_magic_number;
      Marshal.to_channel channel retained_cmt [])

let () =
  match Array.to_list Sys.argv with
  | [ _; "modes"; filename ] -> modes filename
  | [ _; "sst"; filename ] -> sst filename
  | [ _; "vir"; filename ] -> vir filename
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "reject-zero"; filename ] -> reject_zero filename
  | [ _; "reject-zero-flow"; filename ] -> reject_zero_flow filename
  | [ _; "reject-zero-no-flow"; filename ] -> reject_zero_no_flow filename
  | [ _; "reject-raw-proof-body-no-authority"; filename ] ->
      reject_raw_proof_body_no_authority filename
  | [ _; "copied"; filename ] -> copied filename
  | [ _; "dependency-modes"; consumer; dependency ] ->
      dependency_modes consumer dependency
  | [ _; "embed-ordinary-interface"; retained; ordinary_interface; output ] ->
      embed_ordinary_interface retained ordinary_interface output
  | _ ->
      fail
        "usage: instance_modes_tool \
         (modes|sst|vir|solve|reject-zero|reject-zero-flow|reject-zero-no-flow|reject-raw-proof-body-no-authority|copied|dependency-modes|embed-ordinary-interface) \
         FILE.cmt [DEPENDENCY.cmt|ORDINARY.cmt OUTPUT.cmt]"
