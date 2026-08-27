let () = ignore Rank_backed_specifications_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let reset_counters () =
  Symbolic_executor_private.For_testing.reset_authority_observation ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

type solver_observation = {
  backend_solvers : int;
  z3_contexts : int;
  z3_solvers : int;
}

let solver_observation () =
  let direct = Z3_bridge.counters () in
  {
    backend_solvers =
      Solver_backend.For_testing.solver_creation_count ();
    z3_contexts = direct.contexts_created;
    z3_solvers = direct.solvers_created;
  }

let assert_zero_solvers () =
  let observation = solver_observation () in
  if
    observation.backend_solvers <> 0 || observation.z3_contexts <> 0
    || observation.z3_solvers <> 0
  then fail "frontend rejection reached a solver"

let authority_observation () =
  Symbolic_executor_private.For_testing.authority_observation ()

let assert_zero_private_authority observation =
  if observation.Symbolic_executor_private.recursive_spec_lowerings <> 0 then
    fail "rank-backed shape reached recursive production lowering"

let load_implementation filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let lower_implementation implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let load_program filename =
  lower_implementation (load_implementation filename)

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let lower_vir program =
  match Symbolic_executor.lower_program program with
  | Ok vir -> vir
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

type vir_observation = {
  mutable tags : int;
  mutable selectors : int;
  mutable recursive_applications : int;
  mutable rank_projections : int;
  mutable strict_rank_obligations : int;
  mutable invariant_facts : int;
}

let empty_vir_observation () =
  {
    tags = 0;
    selectors = 0;
    recursive_applications = 0;
    rank_projections = 0;
    strict_rank_obligations = 0;
    invariant_facts = 0;
  }

let rec observe_aggregate observation (term : Vir.aggregate_term) =
  match term.aggregate_desc with
  | Vir.Aggregate_symbol _ -> ()
  | Vir.Aggregate_selector (_, aggregate) ->
      observation.selectors <- observation.selectors + 1;
      observe_aggregate observation aggregate
  | Vir.Aggregate_imported_model_application { arguments; _ } ->
      List.iter (observe_recursive_argument observation) arguments
  | Vir.Aggregate_symbolic_application _ ->
      List.iter (observe_recursive_argument observation)
        (Option.get
           (Vir.symbolic_application_arguments
              (Aggregate_application term)))
  | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
  | Vir.Aggregate_conditional _
  | Vir.Aggregate_recursive_spec_application _ ->
      ()

and observe_recursive_argument observation = function
  | Vir.Recursive_integer_argument term ->
      observe_integer observation term
  | Vir.Recursive_boolean_argument term ->
      observe_boolean observation term
  | Vir.Recursive_aggregate_argument term ->
      observe_aggregate observation term
  | Vir.Recursive_parametric_argument term ->
      observe_parametric observation term

and observe_integer observation = function
  | Vir.Integer_constant _ | Vir.Integer_symbol _ -> ()
  | Vir.Integer_add (left, right)
  | Vir.Integer_subtract (left, right) ->
      observe_integer observation left;
      observe_integer observation right
  | Vir.Integer_negate term
  | Vir.Integer_multiply_constant (_, term)
  | Vir.Integer_absolute_value term ->
      observe_integer observation term
  | Vir.Integer_conditional (condition, consequent, alternative) ->
      observe_boolean observation condition;
      observe_integer observation consequent;
      observe_integer observation alternative
  | Vir.Integer_rank_project (_, aggregate) ->
      observation.rank_projections <- observation.rank_projections + 1;
      observe_aggregate observation aggregate
  | Vir.Aggregate_tag (_, aggregate) ->
      observation.tags <- observation.tags + 1;
      observe_aggregate observation aggregate
  | Vir.Integer_selector (_, aggregate) ->
      observation.selectors <- observation.selectors + 1;
      observe_aggregate observation aggregate
  | Vir.Integer_recursive_spec_application { arguments; _ } ->
      observation.recursive_applications <-
        observation.recursive_applications + 1;
      List.iter (observe_recursive_argument observation) arguments
  | (Vir.Integer_symbolic_application _ as application) ->
      List.iter (observe_recursive_argument observation)
        (Option.get
           (Vir.symbolic_application_arguments
              (Integer_application application)))

and observe_boolean observation = function
  | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
      observe_boolean observation quantifier.boolean_quantifier_body;
      Option.iter (observe_application observation)
        quantifier.boolean_quantifier_trigger
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _
  | Vir.Logical_adt_schema _ ->
      ()
  | Vir.Boolean_not term ->
      observe_boolean observation term
  | Vir.Boolean_and (left, right)
  | Vir.Boolean_or (left, right)
  | Vir.Boolean_equal (left, right)
  | Vir.Boolean_not_equal (left, right) ->
      observe_boolean observation left;
      observe_boolean observation right
  | Vir.Integer_compare (_, left, right) ->
      observe_integer observation left;
      observe_integer observation right
  | Vir.Parametric_equal (left, right) ->
      observe_parametric observation left;
      observe_parametric observation right
  | Vir.Boolean_selector (_, aggregate) ->
      observation.selectors <- observation.selectors + 1;
      observe_aggregate observation aggregate
  | Vir.Aggregate_equal (left, right) ->
      observe_aggregate observation left;
      observe_aggregate observation right
  | Vir.Boolean_invariant_application { value; _ } ->
      observation.invariant_facts <- observation.invariant_facts + 1;
      observe_aggregate observation value
  | Vir.Boolean_recursive_spec_application { arguments; _ } ->
      observation.recursive_applications <-
        observation.recursive_applications + 1;
      List.iter (observe_recursive_argument observation) arguments
  | Vir.Boolean_specification_application { arguments; _ } ->
      List.iter (observe_recursive_argument observation) arguments
  | (Vir.Boolean_symbolic_application _ as application) ->
      List.iter (observe_recursive_argument observation)
        (Option.get
           (Vir.symbolic_application_arguments
              (Boolean_application application)))
  | Vir.Callback_requires _ | Vir.Callback_ensures _ -> ()

and observe_parametric observation term =
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol _ -> ()
  | Vir.Parametric_selector (_, aggregate) ->
      observe_aggregate observation aggregate
  | Vir.Parametric_conditional (condition, consequent, alternative) ->
      observe_boolean observation condition;
      observe_parametric observation consequent;
      observe_parametric observation alternative
  | Vir.Parametric_symbolic_application _ ->
      List.iter (observe_recursive_argument observation)
        (Option.get
           (Vir.symbolic_application_arguments
              (Parametric_application term)))

and observe_application observation = function
  | Vir.Integer_application term -> observe_integer observation term
  | Vir.Boolean_application term -> observe_boolean observation term
  | Vir.Aggregate_application term -> observe_aggregate observation term
  | Vir.Parametric_application term -> observe_parametric observation term

let observe_obligation observation (obligation : Vir.obligation) =
  (match obligation.kind with
  | Vir.Recursive_call_strict_descent _ ->
      observation.strict_rank_obligations <-
        observation.strict_rank_obligations + 1
  | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Local_assertion _
  | Vir.Postcondition _
  | Vir.Call_precondition _ | Vir.Callback_precondition _
  | Vir.Invariant_validity _
  | Vir.Entry_measure_nonnegative _
  | Vir.Recursive_call_measure_nonnegative _ ->
      ());
  List.iter (observe_boolean observation) obligation.assumptions;
  List.iter (observe_boolean observation)
    obligation.required_preceding_safety;
  List.iter (observe_boolean observation) obligation.path_condition;
  observe_boolean observation obligation.goal

let observe_vir_program observation (program : Vir.program) =
  List.iter
    (fun (execution : Vir.function_execution) ->
      List.iter (observe_obligation observation) execution.obligations;
      List.iter
        (fun (exit : Vir.exit) ->
          List.iter (observe_boolean observation) exit.assumptions;
          List.iter (observe_boolean observation) exit.path_condition)
        execution.exits)
    program.functions

type sst_observation = {
  mutable reveals : int;
  mutable fuel_reveals : int;
}

let rec observe_sst_expression observation (expression : Sst.expression) =
  let observe = observe_sst_expression observation in
  match expression.expression_desc with
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Optional_absent | Sst.Variable _ | Sst.Mutable_read _
  | Sst.Owned_tree_rebase _ ->
      ()
  | Sst.Tuple_value fields ->
      List.iter (fun (_, field) -> observe field) fields
  | Sst.Record_value { fields; _ } ->
      List.iter (fun (_, field) -> observe field) fields
  | Sst.Constructor_value { arguments; _ }
  | Sst.Checked_arithmetic (_, arguments) ->
      List.iter observe arguments
  | Sst.Field_read { record; _ } ->
      observe record
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ }
  | Sst.Mutable_write { value; _ }
  | Sst.Use_type_invariant { value; _ }
  | Sst.Proof_region value | Sst.Old value ->
      observe value
  | Sst.Local_assert { predicate; _ } ->
      observe predicate
  | Sst.Let_mutable (_, initial, body) ->
      observe initial;
      observe body
  | Sst.Let (bindings, body) ->
      List.iter (fun (_, value) -> observe value) bindings;
      observe body
  | Sst.Sequence (first, second) | Sst.Compare (_, first, second)
  | Sst.Boolean_binary (_, first, second) ->
      observe first;
      observe second
  | Sst.If (condition, consequent, alternative) ->
      observe condition;
      observe consequent;
      Option.iter observe alternative
  | Sst.Match (scrutinee, cases) ->
      observe scrutinee;
      List.iter
        (fun (case : Sst.case) ->
          Option.iter observe case.case_guard;
          observe case.case_body)
        cases
  | Sst.Boolean_not value | Sst.Optional_present value
  | Sst.Optional_forward value ->
      observe value
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      observe quantifier.quantifier_body;
      Option.iter observe quantifier.quantifier_trigger
  | Sst.Direct_call { arguments; _ } ->
      List.iter
        (fun argument -> observe (snd (Sst.require_value_argument argument)))
        arguments
  | (Sst.Symbolic_application _ as application) ->
      List.iter observe
        (Option.get (Sst.symbolic_application_arguments application))
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ -> ()
  | Sst.Reveal _ ->
      observation.reveals <- observation.reveals + 1
  | Sst.Reveal_with_fuel _ ->
      observation.fuel_reveals <- observation.fuel_reveals + 1

let observe_staged observation (staged : Sst.staged_expression) =
  observe_sst_expression observation staged.expression

let observe_sst_program observation (program : Sst.program) =
  List.iter
    (fun (definition : Sst.function_definition) ->
      List.iter
        (fun (clause : Sst.predicate_clause) ->
          observe_staged observation clause.predicate)
        definition.contracts.requires;
      List.iter
        (fun (clause : Sst.ensures_clause) ->
          observe_staged observation clause.predicate)
        definition.contracts.ensures;
      List.iter
        (fun (clause : Sst.predicate_clause) ->
          observe_staged observation clause.predicate)
        definition.contracts.decreases;
      List.iter
        (fun (clause : Sst.predicate_clause) ->
          observe_staged observation clause.predicate)
        definition.contracts.assertions;
      match definition.body with
      | Sst.Checked_exec { body; _ } | Sst.Spec_definition body
      | Sst.Proof_body { body; _ } ->
          observe_staged observation body
      | Sst.Recursive_spec_definition { body; _ } ->
          observe_staged observation body
      | Sst.External_specification _
      | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          ())
    program.functions

let prepare_recursive_semantics program =
  match Spec_unfolding_private.prepare program with
  | Ok prepared -> prepared
  | Error error -> fail "%s" (Spec_unfolding_private.error_to_string error)

let prepare_recursive_encoding program =
  match Recursive_spec_encoding.prepare program with
  | Ok prepared -> prepared
  | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)

let dump_sst filename =
  print_string (Sst.to_string (load_program filename))

let dump_vir filename =
  print_string (Vir.to_string (lower_vir (load_program filename)))

let modes filename =
  let validated = validate (load_program filename) in
  print_string (Sst_validation.instance_mode_dump validated)

let descriptors filename =
  let program = load_program filename in
  let validated = validate program in
  let stack =
    Sst_validation.type_descriptors validated
    |> List.find (fun descriptor ->
           String.equal (Sst_validation.type_id descriptor).Sst.type_name
             "stack")
  in
  let type_id = Sst_validation.type_id stack in
  Printf.printf "ordinary-logical-descriptor %s#%d=%s\n" type_id.type_name
    type_id.type_index
    (match Sst_validation.type_logical_type stack with
    | None -> "none"
    | Some _ -> "published");
  let node =
    List.find (fun descriptor ->
        String.equal
          (Parametric_adt.type_constructor descriptor).constructor_path
          "node")
      program.parametric_adts
  in
  let application =
    Parametric_type.Application
      ( Parametric_adt.type_constructor node,
        List.map
          (fun binder -> Parametric_type.Parameter binder)
          (Parametric_adt.binders node) )
  in
  Printf.printf "parametric-rank-schema %s compiler-uid=%s visibility=private\n"
    (Parametric_type.to_string application) (Parametric_adt.compiler_uid node)

let authority filename =
  let program = load_program filename in
  let recursive_semantics = prepare_recursive_semantics program in
  let recursive_encoding = prepare_recursive_encoding program in
  let vir_observation = empty_vir_observation () in
  observe_vir_program vir_observation (lower_vir program);
  List.iter
    (observe_obligation vir_observation)
    (Spec_unfolding_private.termination_obligations recursive_semantics);
  let sst_observation = { reveals = 0; fuel_reveals = 0 } in
  observe_sst_program sst_observation program;
  let recursive_equations =
    Spec_unfolding_private.definitions recursive_semantics |> List.length
  in
  if
    Recursive_spec_encoding.has_definitions recursive_encoding
    <> (recursive_equations <> 0)
  then fail "recursive equation-source and encoding observations disagree";
  if vir_observation.tags = 0 || vir_observation.selectors = 0 then
    fail "ordinary constructor/tag/selector artifacts are absent";
  if
    vir_observation.recursive_applications <> 0 || recursive_equations <> 0
    || vir_observation.rank_projections <> 0
    || vir_observation.strict_rank_obligations <> 0
    || sst_observation.reveals <> 0
    || sst_observation.fuel_reveals <> 0
    || vir_observation.invariant_facts <> 0
  then fail "rank-backed shape emitted forbidden authority";
  Printf.printf "ordinary tags=%d selectors=%d\n" vir_observation.tags
    vir_observation.selectors;
  Printf.printf
    "forbidden recursive-applications=%d recursive-equations=%d \
     rank-projections=%d strict-rank-obligations=%d induction=%d reveals=%d \
     fuel=%d invariant-facts=%d\n"
    vir_observation.recursive_applications recursive_equations
    vir_observation.rank_projections
    vir_observation.strict_rank_obligations 0
    sst_observation.reveals sst_observation.fuel_reveals
    vir_observation.invariant_facts

let solver_config () =
  match Solver_backend.config ~timeout_ms:60_000 with
  | Ok config -> config
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let solve_obligation config obligation =
  let outcome =
    match Solver_backend.solve_obligation config obligation with
    | Ok outcome -> outcome
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  { Solver_backend.obligation; outcome }

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let pipeline filename =
  reset_counters ();
  let implementation = load_implementation filename in
  let program = lower_implementation implementation in
  let validated = validate program in
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  let configure_solver () =
    let config = solver_config () in
    Ok
      (fun (request : Verification_pipeline.solve_request) ->
        Ok
          (List.map (solve_obligation config)
             request.execution.Vir.obligations))
  in
  match
    Verification_pipeline.run_validated ~imports:None ~implementation ~program
      ~validated
      ~invariants ~preflight:(fun () -> Ok 0) ~configure_solver
      ~proof_entry_activations:(fun () _ -> [])
      ~on_result:(fun _ -> ())
  with
  | Error message -> fail "%s" message
  | Ok report -> (
      match report.outcome with
      | Error _ -> fail "private verification pipeline rejected positive shape"
      | Ok completion ->
          let counters = report.Verification_pipeline.counters in
          let authority = authority_observation () in
          if
            completion.status <> Verification_pipeline.Verified
            || counters.receipts_issued <> 0 || counters.receipts_consumed <> 0
            || counters.finite_witness_issuances <> 0
            || counters.finite_parent_issuances <> 0
            || counters.finite_child_derivations <> 0
            || counters.finite_result_witness_records <> 0
            || counters.finite_result_finalizations <> 0
            || counters.finite_consumptions <> 0
          then fail "shape-only pipeline acquired receipt authority";
          assert_zero_private_authority authority;
          Printf.printf
            "production-authority recursive-lowering=%d\n"
            authority.recursive_spec_lowerings;
          Printf.printf
            "pipeline status=%s functions=%d obligations=%d \
             completed-callee-issued=%d completed-callee-consumed=%d \
             finite-witnesses=%d finite-parents=%d finite-children=%d \
             finite-result-witnesses=%d finite-finalizations=%d \
             finite-consumptions=%d \
             dependent-lowerings=%d dependent-backends=%d \
             dependent-solvers=%d session-destroyed=%b\n"
            (status_name completion.status) completion.functions
            completion.obligations counters.receipts_issued
            counters.receipts_consumed counters.finite_witness_issuances
            counters.finite_parent_issuances counters.finite_child_derivations
            counters.finite_result_witness_records
            counters.finite_result_finalizations counters.finite_consumptions
            counters.dependent_lowerings
            counters.dependent_backend_contexts
            counters.dependent_solver_attempts report.session_destroyed)

let reject_in_private_pipeline implementation program validated =
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  let configure_solver () =
    Ok
      (fun (_ : Verification_pipeline.solve_request) ->
        Error "negative reached production solve")
  in
  match
    Verification_pipeline.run_validated ~imports:None ~implementation ~program
      ~validated
      ~invariants ~preflight:(fun () -> Ok 0) ~configure_solver
      ~proof_entry_activations:(fun () _ -> [])
      ~on_result:(fun _ -> ())
  with
  | Error message -> fail "%s" message
  | Ok report -> (
      if not report.session_destroyed then
        fail "negative production session was not destroyed";
      match report.outcome with
      | Error (Verification_pipeline.Engine_error _) -> report.counters
      | Error (Verification_pipeline.Setup_error _) ->
          fail "negative failed during private pipeline setup"
      | Error (Verification_pipeline.Solve_error _) ->
          fail "negative reached production solve"
      | Ok _ -> fail "negative rank-backed shape was accepted")

let finite_counter_values = function
  | None -> (0, 0, 0, 0, 0, 0)
  | Some (counters : Verification_session.counters) ->
      ( counters.finite_witness_issuances,
        counters.finite_parent_issuances,
        counters.finite_child_derivations,
        counters.finite_result_witness_records,
        counters.finite_result_finalizations,
        counters.finite_consumptions )

let assert_zero_finite = function
  | 0, 0, 0, 0, 0, 0 -> ()
  | _ -> fail "negative emitted finite receipt operations"

let reject filename =
  reset_counters ();
  let disposition, finite =
    match Cmt_input.load filename with
    | Error _ -> ("adapter", None)
    | Ok implementation -> (
        match Typedtree_lowering.lower implementation with
        | Error _ -> ("adapter", None)
        | Ok program -> (
            match Sst_validation.validate program with
            | Error _ -> ("semantic", None)
            | Ok validated ->
                ( "lowering",
                  Some
                    (reject_in_private_pipeline implementation program validated)
                )))
  in
  let finite = finite_counter_values finite in
  assert_zero_finite finite;
  let authority = authority_observation () in
  assert_zero_private_authority authority;
  assert_zero_solvers ();
  let solver = solver_observation () in
  let witnesses, parents, children, result_witnesses, finalizations, consumptions =
    finite
  in
  Printf.printf
    "%s rejection; finite=%d/%d/%d/%d/%d/%d recursive-lowering=%d backend=%d contexts=%d solvers=%d\n"
    disposition witnesses parents children result_witnesses finalizations
    consumptions
    authority.recursive_spec_lowerings
    solver.backend_solvers solver.z3_contexts solver.z3_solvers

let parametric_without_rank_authority filename =
  reset_counters ();
  let program = load_program filename in
  ignore (validate program);
  let binders =
    List.concat_map (fun function_ -> function_.Sst.type_binders) program.functions
  in
  if List.length binders <> 1 then
    fail "parametric control did not preserve its single canonical binder";
  if
    List.exists
      (fun function_ ->
        String.contains function_.Sst.function_id.function_name '<')
      program.functions
  then fail "parametric control entered legacy clone lowering";
  let authority = authority_observation () in
  assert_zero_private_authority authority;
  assert_zero_solvers ();
  let solver = solver_observation () in
  Printf.printf
    "parametric accepted binders=%d clones=0 finite=0/0/0/0/0/0 \
     recursive-lowering=%d backend=%d contexts=%d solvers=%d\n"
    (List.length binders) authority.recursive_spec_lowerings
    solver.backend_solvers solver.z3_contexts solver.z3_solvers

let expect_raw_rejection label program =
  reset_counters ();
  (match Sst_validation.validate program with
  | Error _ -> ()
  | Ok _ -> fail "%s attack retained rank authority" label);
  let authority = authority_observation () in
  assert_zero_private_authority authority;
  assert_zero_solvers ();
  let solver = solver_observation () in
  Printf.printf
    "%s rejected; finite=0/0/0/0/0/0 recursive-lowering=%d backend=%d contexts=%d solvers=%d\n"
    label authority.recursive_spec_lowerings
    solver.backend_solvers solver.z3_contexts solver.z3_solvers

let raw_attacks first_filename second_filename =
  let first = load_program first_filename in
  let second = load_program second_filename in
  ignore (validate first);
  ignore (validate second);
  expect_raw_rejection "raw-copy"
    { first with functions = List.map Fun.id first.functions };
  expect_raw_rejection "forged-rank-record"
    {
      first with
      types = List.map Fun.id first.types;
      functions = List.map Fun.id first.functions;
    };
  expect_raw_rejection "stale-snapshot"
    { first with types = List.rev first.types };
  expect_raw_rejection "cross-unit-same-name"
    { second with functions = first.functions }

let () =
  match Array.to_list Sys.argv with
  | [ _; "sst"; filename ] -> dump_sst filename
  | [ _; "vir"; filename ] -> dump_vir filename
  | [ _; "modes"; filename ] -> modes filename
  | [ _; "descriptors"; filename ] -> descriptors filename
  | [ _; "authority"; filename ] -> authority filename
  | [ _; "pipeline"; filename ] -> pipeline filename
  | [ _; "reject"; filename ] -> reject filename
  | [ _; "parametric-no-rank"; filename ] ->
      parametric_without_rank_authority filename
  | [ _; "raw-attacks"; first; second ] -> raw_attacks first second
  | _ ->
      fail
        "usage: rank_backed_specifications_tool \
         (sst|vir|modes|descriptors|authority|pipeline|reject|parametric-no-rank) \
         FILE | \
         raw-attacks FILE FILE"
