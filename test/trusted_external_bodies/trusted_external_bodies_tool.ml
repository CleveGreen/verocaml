open Typedtree

let () = ignore Trusted_external_bodies_prerequisites.ready

let fail format = Printf.ksprintf failwith format

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok sst -> sst
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message

let lower filename =
  let sst = load filename in
  match Symbolic_executor.lower_program sst with
  | Ok vir -> (sst, vir)
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let reject filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic -> Printf.printf "adapter rejected: %s\n" diagnostic.code
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Error _ -> print_endline "semantic validation rejected before VIR"
      | Ok _ -> fail "accepted")

let inspect mode filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  in
  let carriers = ref 0
  and proof_carriers = ref 0
  and bindings = ref 0
  and raw = ref 0
  and nonghost = ref 0 in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          incr bindings;
          List.iter
            (fun attribute ->
              if String.equal attribute.Parsetree.attr_name.txt
                   "verocaml.external_body"
              then incr raw)
            binding.vb_attributes;
          default.value_binding self binding);
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                _, _, _, _ )
            when String.equal (Path.name path) "Vero_ghost.external_body" ->
              incr carriers;
              if not expression.exp_loc.loc_ghost then incr nonghost
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                _, _, _, _ )
            when String.equal (Path.name path) "Vero_ghost.proof_definition" ->
              incr proof_carriers;
              if not expression.exp_loc.loc_ghost then incr nonghost
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator implementation.structure;
  match mode with
  | "erased" when !carriers = 0 && !raw = 0 ->
      print_endline "ordinary CMT preserved the implementation and erased trust metadata"
  | "retained" when !carriers = 1 && !raw = 0 && !nonghost = 0 ->
      print_endline "retained CMT authenticated one trusted external body"
  | "proof-erased"
    when !carriers = 0 && !proof_carriers = 0 && !bindings = 0 && !raw = 0 ->
      print_endline
        "ordinary CMT erased proof external body, proof declarations, and calls"
  | "proof-retained"
    when !carriers = 1 && !proof_carriers = 2 && !raw = 0 && !nonghost = 0 ->
      print_endline
        "retained CMT authenticated one combined proof external body and two \
         proof declarations"
  | "proof-retained-exact"
    when !carriers = 1 && !proof_carriers = 1 && !raw = 0 && !nonghost = 0 ->
      print_endline
        "retained CMT authenticated the exact combined proof external body role"
  | _ -> fail "carrier mismatch carriers=%d raw=%d nonghost=%d" !carriers !raw !nonghost

let structural () =
  let line = Diagnostic.{ line = 1; column = 0 } in
  let span = Diagnostic.{ file = "raw.ml"; start_pos = line; end_pos = line } in
  let function_id = Sst.{ function_index = 0; function_name = "raw" } in
  let predicate =
    Sst.{ expression_desc = Bool_constant true; typ = Bool; span }
  in
  let ensures =
    Sst.{
      clause_index = 0;
      binder = None;
      predicate = { stage = Logical; expression = predicate };
      span;
    }
  in
  let definition =
    Sst.{
      function_id;
      type_binders = [];
      mode = Exec;
      recursive = false;
      parameters = [];
      contracts = { empty_contracts with ensures = [ ensures ] };
      body = Trusted_external_body (Raw_external_body span);
      policy = Default_linear_z3;
      result_type = Int;
      returns_unique_parameter = None;
      span;
    }
  in
  let program = Sst.{ policy = Default_linear_z3; parametric_adts = []; types = []; logical_constants = []; functions = [ definition ] } in
  match Sst_validation.validate program with
  | Error _ ->
      print_endline "raw semantic trusted-external-body provenance rejected";
      let proof_raw =
        { definition with Sst.mode = Proof; result_type = Unit }
      in
      let proof_program =
        { program with Sst.functions = [ proof_raw ] }
      in
      (match Sst_validation.validate proof_program with
      | Error _ ->
          print_endline
            "raw proof trusted-body provenance and mode replay rejected"
      | Ok _ -> fail "raw proof trusted body accepted");
      let trusted =
        Sst_normalize.authenticated_trusted_external_body_in_mode
          ~source_file:"raw.ml" ~function_id ~mode:Sst.Proof ~parameters:[]
          ~contracts:{ Sst.empty_contracts with ensures = [ ensures ] }
          ~result_type:Sst.Unit ~returns_unique_parameter:None
          ~declaration_span:span ~witness_span:span
      in
      let caller_id =
        Sst.{ function_index = 1; function_name = "wrong_stage" }
      in
      let call =
        Sst.
          {
            expression_desc =
              Direct_call
                {
                  call_form = Exec_call;
                  callee = function_id;
                  arguments = [];
                  type_arguments = [];
                  recursive = false;
                };
            typ = Unit;
            span;
          }
      in
      let caller =
        Sst_normalize.checked_exec_raw ~function_id:caller_id ~recursive:false
          ~parameters:[] ~contracts:Sst.empty_contracts ~body:call
          ~result_type:Sst.Unit ~returns_unique_parameter:None ~span
      in
      (match
         Sst_validation.validate
           { program with Sst.functions = [ trusted; caller ] }
       with
      | Error _ ->
          print_endline
            "wrong Exec call form cannot invoke a trusted Proof declaration"
      | Ok _ -> fail "wrong-stage trusted proof call accepted")
  | Ok _ -> fail "raw trusted body accepted"

let solve expect_counterexample filename =
  let _, vir = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let results =
    List.concat_map
      (fun execution ->
        match Solver_backend.solve_in_order config execution.Vir.obligations with
        | Ok results -> results
        | Error error -> fail "%s" (Solver_backend.error_to_string error))
      vir.functions
  in
  let has_counterexample =
    List.exists
      (fun result ->
        match result.Solver_backend.outcome with
        | Counterexample _ -> true
        | Verified | Inconclusive _ -> false)
      results
  in
  if has_counterexample = expect_counterexample then
    print_endline
      (if expect_counterexample then "counterexample: call precondition is enforced"
       else "verified: caller used only the trusted contract")
  else fail "unexpected solver outcome"

let kind_name (obligation : Vir.obligation) =
  match obligation.kind with
  | Vir.Entry_measure_nonnegative _ -> "entry-measure-nonnegative"
  | Recursive_call_measure_nonnegative _ -> "recursive-call-measure-nonnegative"
  | Recursive_call_strict_descent _ -> "recursive-call-strict-descent"
  | Arithmetic_safety _ -> "arithmetic-safety"
  | Call_precondition _ -> "call-precondition"
  | Callback_precondition _ -> "callback-precondition"
  | Invariant_validity _ -> "invariant-validity"
  | Assertion _ -> "assertion"
  | Local_assertion _ -> "local-assertion"
  | Postcondition _ -> "postcondition"

let outcomes filename =
  let _, vir = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let nonverified = ref 0 in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
      | Ok results ->
          List.iter
            (fun result ->
              match result.Solver_backend.outcome with
              | Verified -> ()
              | Counterexample _ ->
                  incr nonverified;
                  Printf.printf "%s: counterexample %s\n"
                    execution.function_ref.function_name
                    (kind_name result.obligation)
              | Inconclusive _ ->
                  incr nonverified;
                  Printf.printf "%s: inconclusive %s\n"
                    execution.function_ref.function_name
                    (kind_name result.obligation))
            results)
    vir.functions;
  if !nonverified = 0 then print_endline "all obligations verified"

let driver filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  in
  match
    Verification_driver_private.run ~timeout_ms:5000
      ~allow_imported_opens:false implementation
  with
  | Ok report -> report
  | Error _ -> fail "production verification driver rejected the fixture"

let suppress_postconditions filename =
  Symbolic_executor_private.For_testing
  .suppress_proof_summary_for_testing (Some ("assume", "admit"));
  let report =
    Fun.protect
      ~finally:(fun () ->
        Symbolic_executor_private.For_testing
        .suppress_proof_summary_for_testing None)
      (fun () -> driver filename)
  in
  let named_failure =
    Verification_driver_private.results report
    |> List.exists (fun result ->
           String.equal
             result.Solver_backend.obligation.Vir.function_ref.function_name
             "assume"
           &&
           match (result.obligation.kind, result.outcome) with
           | Vir.Postcondition _, Solver_backend.Counterexample _ -> true
           | _ -> false)
  in
  if
    Verification_driver_private.status report
    <> Verification_pipeline.Counterexample
    || not named_failure
  then fail "trusted proof postcondition suppression did not break assume";
  print_endline
    "suppressed admit postcondition: assume counterexample postcondition"

let observe filename =
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let report = driver filename in
  let counters = Verification_driver_private.counters report in
  let authority =
    Symbolic_executor_private.For_testing.authority_observation ()
  in
  let vir = Verification_driver_private.vir report in
  let trusted_execution =
    List.exists
      (fun execution ->
        String.equal execution.Vir.function_ref.function_name "trusted")
      vir.functions
  in
  let trusted_uses =
    List.concat_map
      (fun execution -> execution.Vir.trusted_summary_uses)
      vir.functions
    |> List.filter (function
         | Vir.Trusted_external_body_use _ -> true
         | Trusted_external_specification_use _
         | Trusted_external_target_specification_use _ ->
             false)
    |> List.length
  in
  if
    Verification_driver_private.status report <> Verification_pipeline.Verified
    || trusted_execution
    || List.length vir.trusted_external_body_declarations <> 1
    || trusted_uses <> 1
    || Verification_driver_private.functions report <> 1
    || Verification_driver_private.obligations report <> 1
    || counters.receipts_issued <> 0
    || counters.receipts_consumed <> 0
    || counters.finite_result_promotions <> 0
    || counters.finite_result_completions <> 0
    || counters.finite_result_finalizations <> 0
    || counters.finite_result_consumptions <> 0
    || authority.recursive_spec_lowerings <> 0
    || authority.recursive_proof_rank_lowerings <> 0
  then fail "trusted tail crossed a semantic or authority boundary";
  let z3 = Z3_bridge.counters () in
  Printf.printf
    "tail-observer functions=1 obligations=1 declarations=1 uses=1 \
     body-executions=0 receipts=0/0 finite=0/0/0/0 self=0/0/0 \
     recursive=0/0 solver=%d z3=%d/%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    z3.contexts_created z3.solvers_created

let replay filename =
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  in
  let bindings =
    implementation.structure.str_items
    |> List.filter_map (fun item ->
           match item.str_desc with
           | Tstr_value (Asttypes.Nonrecursive, [ binding ]) ->
               Some (item, binding)
           | _ -> None)
  in
  let first_item, first, second_item, second =
    match bindings with
    | (first_item, first) :: (second_item, second) :: _ ->
        (first_item, first, second_item, second)
    | _ -> fail "replay fixture did not retain two declarations"
  in
  let terminal binding =
    match binding.vb_expr.exp_desc with
    | Texp_function
        { body = Tfunction_body terminal; _ } ->
        terminal
    | _ -> fail "replay declaration did not retain a function carrier"
  in
  let replace binding terminal =
    match binding.vb_expr.exp_desc with
    | Texp_function function_ ->
        {
          binding with
          vb_expr =
            {
              binding.vb_expr with
              exp_desc =
                Texp_function
                  { function_ with body = Tfunction_body terminal };
            };
        }
    | _ -> assert false
  in
  let first' = replace first (terminal second) in
  let second' = replace second (terminal first) in
  let replace_item original binding =
    { original with str_desc = Tstr_value (Asttypes.Nonrecursive, [ binding ]) }
  in
  let items =
    implementation.structure.str_items
    |> List.map (fun item ->
           if item == first_item then replace_item item first'
           else if item == second_item then replace_item item second'
           else item)
  in
  let structure = { implementation.structure with str_items = items } in
  match
    Typedtree_adapter.lower ~source_file:implementation.source_file
      ~imports:implementation.imports structure
  with
  | Error diagnostic
    when String.equal diagnostic.Diagnostic.code "VERO_MALFORMED_GHOST_CALL" ->
      let authority =
        Symbolic_executor_private.For_testing.authority_observation ()
      in
      let z3 = Z3_bridge.counters () in
      if
        authority.recursive_spec_lowerings <> 0
        || authority.recursive_proof_rank_lowerings <> 0
        || Solver_backend.For_testing.solver_creation_count () <> 0
        || z3.contexts_created <> 0 || z3.solvers_created <> 0
      then fail "replay attack crossed an authority or solver boundary";
      print_endline
        "replayed combined-role carriers rejected in one process \
         recursive=0/0 solver=0 z3=0/0"
  | Error diagnostic -> fail "unexpected replay diagnostic %s" diagnostic.code
  | Ok _ -> fail "replayed combined-role carriers accepted"

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural" ] -> structural ()
  | [ _; "inspect"; mode; filename ] -> inspect mode filename
  | [ _; "reject"; filename ] -> reject filename
  | [ _; "sst"; filename ] -> let sst, _ = lower filename in print_string (Sst.to_string sst)
  | [ _; "vir"; filename ] -> let _, vir = lower filename in print_string (Vir.to_string vir)
  | [ _; "solve"; filename ] -> solve false filename
  | [ _; "counterexample"; filename ] -> solve true filename
  | [ _; "outcomes"; filename ] -> outcomes filename
  | [ _; "suppress-postconditions"; filename ] ->
      suppress_postconditions filename
  | [ _; "observe"; filename ] -> observe filename
  | [ _; "replay"; filename ] -> replay filename
  | _ -> fail "usage"
