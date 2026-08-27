let () =
  ignore Shared_recursive_frozen_spine_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.code

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.code

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let run filename =
  match
    Verification_driver_private.run ~timeout_ms:10_000
      ~allow_imported_opens:false (load filename)
  with
  | Ok report -> report
  | Error _ -> fail "driver rejected %s" filename

let verify filename =
  let report = run filename in
  let counters = Verification_driver_private.counters report in
  Printf.printf
    "status=%s functions=%d obligations=%d shared=%d/%d/%d/%d invariant=%d/%d/%d/%d/%d recursive-results=%d/%d generic-finite=%d/%d/%d frozen-template=%d/%d frozen-conditional=%d/%d frozen-instance=%d/%d frozen-discharge=%d/%d/%d frozen-witness=%d/%d/%d frozen-observation=%d/%d\n"
    (status (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)
    counters.Verification_session.shared_heap_issuances
    counters.shared_heap_writes counters.shared_heap_epoch_advances
    counters.shared_heap_teardowns
    counters.invariant_cell_opens counters.invariant_cell_updates
    counters.invariant_cell_closes
    counters.invariant_cell_effect_instantiations
    counters.invariant_cell_terminal_reads
    counters.recursive_spec_result_issuances
    counters.recursive_spec_result_consumptions
    counters.finite_formal_assumption_issuances
    counters.finite_formal_transfers counters.finite_consumptions
    counters.frozen_constructor_template_issuances
    counters.frozen_constructor_template_teardowns
    counters.frozen_conditional_scope_issuances
    counters.frozen_conditional_scope_teardowns
    counters.frozen_result_instance_issuances
    counters.frozen_result_instance_teardowns
    counters.frozen_call_discharge_issuances
    counters.frozen_call_discharge_consumptions
    counters.frozen_call_discharge_teardowns
    counters.frozen_descent_witness_issuances
    counters.frozen_descent_witness_consumptions
    counters.frozen_descent_witness_teardowns
    counters.frozen_observation_consumptions counters.frozen_observation_teardowns

let route filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  ignore (run filename);
  let z3 = Z3_bridge.counters () in
  Printf.printf "ordinary=%d direct-z3=%d recursive=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    z3.solvers_created
    (Recursive_spec_encoding.For_testing.proof_query_construction_count ())

let reject_boundary filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let rejected =
    match
      Verification_driver_private.run ~timeout_ms:10_000
        ~allow_imported_opens:false (load filename)
    with
    | Error _ -> true
    | Ok _ -> false
  in
  if not rejected then fail "boundary fixture was accepted"
  else
    let z3 = Z3_bridge.counters () in
    Printf.printf
      "rejected frozen=0/0/0/0/0 shared=0/0/0/0 invariant=0/0/0/0/0 vir=0 ordinary=%d direct-z3=%d recursive=%d\n"
      (Solver_backend.For_testing.solver_creation_count ())
      z3.solvers_created
      (Recursive_spec_encoding.For_testing.proof_query_construction_count ())

let dump_sst filename = lower filename |> Sst.to_string |> print_string
let dump_vir filename = run filename |> Verification_driver_private.vir |> Vir.to_string |> print_string

let forge filename =
  let program = lower filename in
  let changed = ref false in
  let types =
    List.map
      (fun (definition : Sst.type_definition) ->
        match definition.representation with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction evidence) -> (
            match Sst.frozen_spine_prerequisite evidence with
            | Some frozen when not !changed ->
                changed := true;
                let forged =
                  {
                    frozen with
                    frozen_edge_field =
                      {
                        frozen.frozen_edge_field with
                        field_index =
                          frozen.frozen_edge_field.field_index + 1;
                      };
                  }
                in
                let forged_evidence =
                  { evidence with authentication_token = ref () }
                in
                Sst.register_frozen_spine_prerequisite forged_evidence forged;
                {
                  definition with
                  representation =
                    Sst.Abstract_with_evidence
                      (Sst.Authenticated_same_cmt_abstraction forged_evidence);
                }
            | Some _ | None -> definition)
        | Sst.Revealed
        | Sst.Abstract_with_evidence
            (Sst.Incomplete_abstraction_evidence _
            | Sst.Proposed_same_cmt_abstraction _) ->
            definition)
      program.types
  in
  if not !changed then fail "no frozen descriptor";
  match Sst_validation.validate { program with types } with
  | Error _ -> print_endline "forged-descriptor: rejected"
  | Ok _ -> fail "forged descriptor accepted"

let raw_constructor filename =
  let program = lower filename in
  let constructors =
    program.Sst.types
    |> List.filter_map (fun (definition : Sst.type_definition) ->
           match definition.representation with
           | Sst.Abstract_with_evidence
               (Sst.Authenticated_same_cmt_abstraction evidence) ->
               Sst.frozen_spine_prerequisite evidence
               |> Option.map
                    (fun (frozen : Sst.frozen_spine_prerequisite) ->
                      frozen.frozen_constructor)
           | Sst.Revealed
           | Sst.Abstract_with_evidence
               (Sst.Incomplete_abstraction_evidence _
               | Sst.Proposed_same_cmt_abstraction _) ->
               None)
  in
  let changed = ref false in
  let functions =
    List.map
      (fun (definition : Sst.function_definition) ->
        if
          List.exists
            (fun constructor ->
              constructor = definition.function_id)
            constructors
        then
          match definition.body with
          | Sst.Checked_exec { body; provenance = _ } ->
              changed := true;
              {
                definition with
                body =
                  Sst.Checked_exec
                    {
                      body;
                      provenance = Sst.Raw_semantic_body definition.span;
                    };
              }
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.Proof_body _ | Sst.External_specification _
          | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _
          | Sst.Symbolic_declaration _ ->
              definition
        else definition)
      program.functions
  in
  if not !changed then fail "no checked frozen constructor";
  match Sst_validation.validate { program with functions } with
  | Error _ -> print_endline "raw-constructor: rejected"
  | Ok _ -> fail "raw constructor accepted"

let suppress_witness filename =
  Symbolic_executor_private.For_testing
  .suppress_frozen_spine_child_witness_for_testing true;
  Fun.protect
    ~finally:(fun () ->
      Symbolic_executor_private.For_testing
      .suppress_frozen_spine_child_witness_for_testing false)
    (fun () ->
      let report = run filename in
      Printf.printf "witness-suppressed=%s\n"
        (status (Verification_driver_private.status report)))

let () =
  match Array.to_list Sys.argv with
  | [ _; "verify"; filename ] -> verify filename
  | [ _; "route"; filename ] -> route filename
  | [ _; "reject-boundary"; filename ] -> reject_boundary filename
  | [ _; "dump-sst"; filename ] -> dump_sst filename
  | [ _; "dump-vir"; filename ] -> dump_vir filename
  | [ _; "forge"; filename ] -> forge filename
  | [ _; "raw-constructor"; filename ] -> raw_constructor filename
  | [ _; "suppress-witness"; filename ] -> suppress_witness filename
  | _ ->
      fail
        "usage: TOOL verify|route|reject-boundary|dump-sst|dump-vir|forge|raw-constructor|suppress-witness FILE"
