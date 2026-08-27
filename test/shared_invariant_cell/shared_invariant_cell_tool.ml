let () = ignore Shared_invariant_cell_prerequisites.ready

let fail format =
  Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let run filename =
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load filename)
  with
  | Error _ -> fail "production driver rejected %s" filename
  | Ok report -> report

let verify filename =
  let report = run filename in
  let c = Verification_driver_private.counters report in
  Printf.printf
    "status=%s functions=%d obligations=%d cell=%d/%d/%d/%d/%d/%d/%d/%d/%d heap=%d/%d/%d/%d/%d\n"
    (status_name (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)
    c.Verification_session.invariant_cell_entry_eligibilities
    c.invariant_cell_constructor_eligibilities
    c.invariant_cell_closed_initializations c.invariant_cell_opens
    c.invariant_cell_updates c.invariant_cell_closes
    c.invariant_cell_effect_instantiations c.invariant_cell_terminal_reads
    c.invariant_cell_teardowns c.shared_heap_issuances c.shared_heap_writes
    c.shared_heap_read_logs c.shared_heap_epoch_advances
    c.shared_heap_teardowns

let dump_vir filename =
  run filename |> Verification_driver_private.vir |> Vir.to_string
  |> print_string

let reject_boundary filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let rejected =
    match Typedtree_lowering.lower_file filename with
    | Error _ -> true
    | Ok program -> (
        match Sst_validation.validate program with
        | Error _ -> true
        | Ok _ -> false)
  in
  if not rejected then fail "boundary fixture was accepted"
  else
    let direct = Z3_bridge.counters () in
    Printf.printf
      "rejected cell=0/0/0/0/0/0/0/0/0 heap=0/0/0/0/0 vir=0 ordinary=%d direct-z3=%d recursive=%d\n"
      (Solver_backend.For_testing.solver_creation_count ())
      direct.solvers_created
      (Recursive_spec_encoding.For_testing.proof_query_construction_count ())

let route filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let _report = run filename in
  let direct = Z3_bridge.counters () in
  Printf.printf "ordinary=%d direct-z3=%d recursive=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    direct.solvers_created
    (Recursive_spec_encoding.For_testing.proof_query_construction_count ())

let lifecycle () =
  Shared_invariant_cell_private.For_testing.lifecycle_matrix ()
  |> List.iter print_endline

let rec mutate_first_transition change (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Shared_scalar_field_write write ->
      ( true,
        {
          expression with
          expression_desc =
            Sst.Shared_scalar_field_write
              { write with transition = change write.transition };
        } )
  | Sst.Sequence (left, right) ->
      let changed, left = mutate_first_transition change left in
      if changed then (true, { expression with expression_desc = Sst.Sequence (left, right) })
      else
        let changed, right = mutate_first_transition change right in
        (changed, { expression with expression_desc = Sst.Sequence (left, right) })
  | Sst.Let (bindings, body) ->
      let changed, bindings =
        let rec loop done_ = function
          | [] -> (false, List.rev done_)
          | (pattern, value) :: rest ->
              let changed, value = mutate_first_transition change value in
              if changed then
                (true, List.rev_append done_ ((pattern, value) :: rest))
              else loop ((pattern, value) :: done_) rest
        in
        loop [] bindings
      in
      if changed then (true, { expression with expression_desc = Sst.Let (bindings, body) })
      else
        let changed, body = mutate_first_transition change body in
        (changed, { expression with expression_desc = Sst.Let (bindings, body) })
  | _ -> (false, expression)

let mutate_effect change (program : Sst.program) =
  let changed = ref false in
  let functions =
    List.map
      (fun (definition : Sst.function_definition) ->
        if !changed then definition
        else
          match definition.body with
          | Sst.Checked_exec checked ->
              let found, expression =
                mutate_first_transition change checked.body.expression
              in
              if found then (
                changed := true;
                {
                  definition with
                  body =
                    Sst.Checked_exec
                      {
                        checked with
                        body = { checked.body with expression };
                      };
                })
              else definition
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.Proof_body _ | Sst.External_specification _
          | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _
          | Sst.Symbolic_declaration _ -> definition)
      program.functions
  in
  if not !changed then fail "forgery seed has no shared effect";
  { program with functions }

let mutate_role role_name (program : Sst.program) =
  let changed = ref false in
  let types =
    List.map
      (fun (definition : Sst.type_definition) ->
        match definition.representation with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction evidence) ->
            let surface =
              List.map
                (fun (operation : Sst.abstract_public_operation) ->
                  if
                    (not !changed)
                    &&
                    match (role_name, operation.public_role) with
                    | "role", Sst.Shared_invariant_transition
                    | "model", Sst.Current_model
                    | "invariant", Sst.Abstract_invariant -> true
                    | _ -> false
                  then (
                    changed := true;
                    { operation with public_role = Sst.Terminal_read })
                  else operation)
                evidence.public_surface
            in
            {
              definition with
              representation =
                Sst.Abstract_with_evidence
                  (Sst.Authenticated_same_cmt_abstraction
                     { evidence with public_surface = surface });
            }
        | Sst.Revealed
        | Sst.Abstract_with_evidence
            (Sst.Incomplete_abstraction_evidence _
            | Sst.Proposed_same_cmt_abstraction _) -> definition)
      program.types
  in
  if not !changed then fail "forgery seed has no %s role" role_name;
  { program with types }

let forgery_matrix filename =
  let program = lower filename in
  let attacks =
    [
      ("role", mutate_role "role");
      ("model", mutate_role "model");
      ("invariant", mutate_role "invariant");
      ( "callable",
        mutate_effect (fun transition ->
            { transition with shared_function_index = transition.shared_function_index + 1 }) );
      ( "path",
        mutate_effect (fun transition ->
            { transition with shared_path_id = transition.shared_path_id + 1 }) );
      ( "epoch",
        mutate_effect (fun transition ->
            { transition with shared_predecessor_epoch = transition.shared_predecessor_epoch + 1 }) );
      ( "type",
        mutate_effect (fun transition ->
            { transition with shared_record_type = { transition.shared_record_type with type_index = transition.shared_record_type.type_index + 1 } }) );
      ( "field",
        mutate_effect (fun transition ->
            { transition with shared_target_field = { transition.shared_target_field with field_index = transition.shared_target_field.field_index + 1 } }) );
      ( "formal",
        mutate_effect (fun transition ->
            { transition with shared_formal_roots = [] }) );
    ]
  in
  List.iter
    (fun (name, mutate) ->
      let result =
        Sst_validation_private.Public.For_testing
        .with_program_mutation_at_validation_boundary
          ~mutate ~observe:(fun () -> ())
          (fun () -> Sst_validation.validate program)
      in
      match result with
      | Error _ -> Printf.printf "forged-%s: rejected\n" name
      | Ok _ -> fail "forged-%s descriptor was accepted" name)
    attacks

let () =
  match Array.to_list Sys.argv with
  | [ _; "verify"; filename ] -> verify filename
  | [ _; "dump-sst"; filename ] -> print_string (Sst.to_string (lower filename))
  | [ _; "dump-vir"; filename ] -> dump_vir filename
  | [ _; "reject-boundary"; filename ] -> reject_boundary filename
  | [ _; "route"; filename ] -> route filename
  | [ _; "forgery-matrix"; filename ] -> forgery_matrix filename
  | [ _; "lifecycle" ] -> lifecycle ()
  | _ ->
      fail "usage: shared_invariant_cell_tool (verify|dump-sst|dump-vir|reject-boundary|route|forgery-matrix) FILE.cmt | lifecycle"
