let () = ignore Shared_scalar_heap_prerequisites.ready

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

let classify filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> (
      match Sst_validation.validate program with
      | Ok _ -> print_endline "accepted"
      | Error error ->
          Printf.printf "validation: %s\n"
            (Sst_validation.error_to_string error))
  | Error diagnostic ->
      Printf.printf "%s @ %s\n" diagnostic.Diagnostic.code
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
  let counters = Verification_driver_private.counters report in
  Printf.printf
    "status=%s functions=%d obligations=%d heap=%d/%d/%d/%d/%d\n"
    (status_name (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)
    counters.Verification_session.shared_heap_issuances
    counters.shared_heap_writes counters.shared_heap_read_logs
    counters.shared_heap_epoch_advances counters.shared_heap_teardowns

let route filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  let report = run filename in
  let counters = Verification_driver_private.counters report in
  let direct = Z3_bridge.counters () in
  let vir = Verification_driver_private.vir report |> Vir.to_string in
  let contains text fragment =
    let text_length = String.length text
    and fragment_length = String.length fragment in
    let rec search index =
      index + fragment_length <= text_length
      &&
      (String.sub text index fragment_length = fragment
      || search (index + 1))
    in
    fragment_length = 0 || search 0
  in
  Printf.printf
    "ordinary=%d direct-z3=%d recursive=%d finite=%d invariant=%d unique=%d recursive-authority=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    direct.solvers_created
    (Recursive_spec_encoding.For_testing.proof_query_construction_count ())
    (counters.finite_witness_issuances + counters.finite_parent_issuances
   + counters.finite_child_derivations + counters.finite_consumptions)
    (if contains vir "invariant" then 1 else 0)
    (counters.transition_predecessor_transfers
   + counters.transition_predecessor_consumptions
   + counters.transition_result_receipts
   + counters.owned_root_scalar_plans_issued)
    (counters.recursive_spec_result_issuances
   + counters.recursive_spec_result_consumptions)

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
      "rejected heap=0/0/0/0/0 vir=0 ordinary=%d direct-z3=%d recursive=%d\n"
      (Solver_backend.For_testing.solver_creation_count ())
      direct.solvers_created
      (Recursive_spec_encoding.For_testing.proof_query_construction_count ())

let dump_vir filename =
  run filename |> Verification_driver_private.vir |> Vir.to_string
  |> print_string

let lifecycle () =
  Shared_scalar_heap_private.For_testing.lifecycle_matrix ()
  |> List.iter print_endline

let backend_conditional () =
  let aggregate_type =
    { Vir.aggregate_type_index = 0; aggregate_type_name = "box" ; aggregate_type_arguments = []}
  in
  let symbol id name =
    {
      Vir.symbol_id = id;
      source_name = name;
      sort = Vir.Aggregate aggregate_type;
      role = Vir.Input;
      span = Diagnostic.file_span "conditional";
    }
  in
  let aggregate symbol =
    { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
  in
  let term =
    Vir_integer_conditional_private.create
      ~condition:
        (Vir.Aggregate_equal
           (aggregate (symbol 0 "x"), aggregate (symbol 1 "y")))
      ~consequent:(Vir.Integer_constant Z.one)
      ~alternative:(Vir.Integer_constant Z.zero)
  in
  match
    Solver_backend.For_testing.translated_integer_term ~function_index:0
      term
  with
  | Ok translated -> print_endline translated
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let forged_vir_conditional () =
  let aggregate_type =
    { Vir.aggregate_type_index = 0; aggregate_type_name = "box" ; aggregate_type_arguments = []}
  in
  let aggregate id name =
    let symbol =
      {
        Vir.symbol_id = id;
        source_name = name;
        sort = Vir.Aggregate aggregate_type;
        role = Vir.Input;
        span = Diagnostic.file_span "forged-conditional";
      }
    in
    { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
  in
  let term =
    Vir.Integer_conditional
      ( Vir.Aggregate_equal (aggregate 0 "x", aggregate 1 "y"),
        Vir.Integer_constant Z.one,
        Vir.Integer_constant Z.zero )
  in
  match
    Solver_backend.For_testing.translated_integer_term ~function_index:0
      term
  with
  | Error _ -> print_endline "forged-vir-conditional: rejected"
  | Ok _ -> fail "forged VIR integer conditional was accepted"

let mutate_seed transition program =
  match program.Sst.functions with
  | definition :: rest -> (
      match definition.body with
      | Sst.Checked_exec
          ({ body = { expression = { expression_desc =
                 Sst.Shared_scalar_field_write write; _ } as expression; _ }
           ; _ } as checked) ->
          let expression =
            {
              expression with
              Sst.expression_desc =
                Sst.Shared_scalar_field_write
                  { write with transition = transition write.transition };
            }
          in
          {
            program with
            Sst.functions =
              {
                definition with
                body =
                  Sst.Checked_exec
                    {
                      checked with
                      body = { checked.body with expression };
                    };
              }
              :: rest;
          }
      | Sst.Checked_exec _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.Proof_body _
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          fail "forgery seed does not contain one direct shared write")
  | [] -> fail "forgery seed has no function"

let forgery_matrix filename =
  let program = lower filename in
  let type_shift (transition : Sst.shared_scalar_heap_transition) =
    {
      transition with
      Sst.shared_record_type =
        {
          transition.shared_record_type with
          type_index = transition.shared_record_type.type_index + 1;
        };
    }
  in
  let field_shift (transition : Sst.shared_scalar_heap_transition) =
    {
      transition with
      Sst.shared_target_field =
        {
          transition.shared_target_field with
          field_index = transition.shared_target_field.field_index + 1;
        };
    }
  in
  let attacks =
    [
      ( "raw-policy",
        fun (program : Sst.program) ->
          (Obj.obj (Obj.dup (Obj.repr program)) : Sst.program) );
      ( "path",
        mutate_seed (fun (transition : Sst.shared_scalar_heap_transition) ->
            {
              transition with
              Sst.shared_path_id = transition.shared_path_id + 1;
            }) );
      ( "epoch",
        mutate_seed (fun (transition : Sst.shared_scalar_heap_transition) ->
            {
              transition with
              Sst.shared_predecessor_epoch =
                transition.shared_predecessor_epoch + 1;
            }) );
      ("type", mutate_seed type_shift);
      ("field", mutate_seed field_shift);
    ]
  in
  List.iter
    (fun (name, mutate) ->
      let result =
        Sst_validation_private.Public.For_testing
        .with_program_mutation_at_validation_boundary
          ~mutate
          ~observe:(fun () -> ())
          (fun () -> Sst_validation.validate program)
      in
      match result with
      | Error _ -> Printf.printf "forged-%s: rejected\n" name
      | Ok _ -> fail "forged-%s descriptor was accepted" name)
    attacks

let () =
  match Array.to_list Sys.argv with
  | [ _; "classify"; filename ] -> classify filename
  | [ _; "dump-sst"; filename ] -> print_string (Sst.to_string (lower filename))
  | [ _; "dump-vir"; filename ] -> dump_vir filename
  | [ _; "verify"; filename ] -> verify filename
  | [ _; "route"; filename ] -> route filename
  | [ _; "reject-boundary"; filename ] -> reject_boundary filename
  | [ _; "forgery-matrix"; filename ] -> forgery_matrix filename
  | [ _; "lifecycle" ] -> lifecycle ()
  | [ _; "backend-conditional" ] -> backend_conditional ()
  | [ _; "forged-vir-conditional" ] -> forged_vir_conditional ()
  | _ ->
      fail
        "usage: shared_scalar_heap_tool \
         (classify|dump-sst|dump-vir|verify|route|reject-boundary|forgery-matrix) \
         FILE.cmt | lifecycle | backend-conditional | forged-vir-conditional"
