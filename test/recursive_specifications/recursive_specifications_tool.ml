let () = ignore Recursive_specifications_prerequisites.ready

let fail format = Printf.ksprintf failwith format
let check label condition = if not condition then fail "%s" label

let contains text fragment =
  let fragment_length = String.length fragment in
  let rec loop index =
    index + fragment_length <= String.length text
    &&
    (String.equal (String.sub text index fragment_length) fragment
    || loop (index + 1))
  in
  fragment_length = 0 || loop 0

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let recursive_definition program =
  List.find
    (fun definition ->
      match definition.Sst.body with
      | Sst.Recursive_spec_definition _ -> true
      | _ -> false)
    program.Sst.functions

let proof_definitions program =
  List.filter
    (fun definition -> definition.Sst.mode = Sst.Proof)
    program.Sst.functions

let prepare program =
  match Recursive_spec_encoding.prepare program with
  | Ok prepared -> prepared
  | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)

let verify prepared =
  match Recursive_spec_encoding.verify ~timeout_ms:60_000 prepared with
  | Ok verified -> verified
  | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)

let query verified function_id =
  match Recursive_spec_encoding.base_query verified function_id with
  | Ok query -> query
  | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)

let direct_config : Z3_bridge.config = { timeout_ms = 60000; model = false }

let unknown_reason = function
  | Z3_bridge.Resource_exhausted -> "resource-exhausted"
  | Timed_out -> "timeout"
  | Backend_unknown reason -> reason

let solve_verified label query =
  match Z3_bridge.solve_query direct_config query with
  | Ok Z3_bridge.Verified -> ()
  | Ok (Counterexample _) -> fail "%s was satisfiable" label
  | Ok (Inconclusive reason) ->
      fail "%s was inconclusive: %s" label (unknown_reason reason)
  | Error error -> fail "%s: %s" label (Z3_bridge.error_to_string error)

let rec term_contains_successor term =
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Apply (function_, arguments) ->
      String.equal (Logic_ir.View.function_name function_) "fuel.succ"
      || List.exists term_contains_successor arguments
  | Rank_project (_, _, _, value) -> term_contains_successor value
  | Add (left, right) | Subtract (left, right) | Less_than (left, right)
  | Less_or_equal (left, right) | Greater_than (left, right)
  | Greater_or_equal (left, right) | Equal (left, right)
  | Distinct (left, right) | Implies (left, right) ->
      term_contains_successor left || term_contains_successor right
  | Ite (condition, then_, else_) ->
      term_contains_successor condition || term_contains_successor then_
      || term_contains_successor else_
  | Negate value | Not value | Scale (_, value) ->
      term_contains_successor value
  | Multiply (left, right) ->
      term_contains_successor left || term_contains_successor right
  | And values | Or values -> List.exists term_contains_successor values
  | Forall_term quantifier | Exists_term quantifier ->
      term_contains_successor
        (Logic_ir.View.user_quantifier_body quantifier)
      ||
      (match Logic_ir.View.user_quantifier_trigger quantifier with
      | None -> false
      | Some trigger -> term_contains_successor trigger)
  | Integer _ | Boolean _ | Bound _ -> false

let reset_creation_counters () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_helper_expansion_count ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Recursive_spec_encoding.For_testing.reset_a2_builder_construction_count ()

let assert_zero_solvers label =
  check (label ^ ": smt.ml solver created")
    (Solver_backend.For_testing.solver_creation_count () = 0);
  let counters = Z3_bridge.counters () in
  check (label ^ ": direct solver/context created")
    (counters.contexts_created = 0 && counters.solvers_created = 0)

let assert_zero_helper_phases label =
  check (label ^ ": helper expanded")
    (Recursive_spec_encoding.For_testing.helper_expansion_count () = 0);
  check (label ^ ": recursive lowering started")
    (Recursive_spec_encoding.For_testing.recursive_lowering_count () = 0);
  check (label ^ ": A2 builder started")
    (Recursive_spec_encoding.For_testing.a2_builder_construction_count () = 0)

let expect_prepare_rejected label program =
  reset_creation_counters ();
  (match Recursive_spec_encoding.prepare program with
  | Error _ -> ()
  | Ok _ -> fail "%s was accepted" label);
  assert_zero_solvers label

type recursive_record = {
  body : Sst.staged_expression;
  visibility : [ `Opaque | `Revealed ];
  provenance : Sst.body_provenance;
}

let recursive_record definition =
  match definition.Sst.body with
  | Sst.Recursive_spec_definition { body; visibility; provenance } ->
      { body; visibility; provenance }
  | _ -> assert false

let replace_function (program : Sst.program)
    (replacement : Sst.function_definition) =
  {
    program with
    Sst.functions =
      List.map
        (fun definition ->
          if
            definition.Sst.function_id.function_index
            = replacement.Sst.function_id.function_index
          then replacement
          else definition)
        program.functions;
  }

let reveal_expression span function_id literal_depth =
  {
    Sst.expression_desc =
      Sst.Reveal_with_fuel { function_id; literal_depth };
    typ = Sst.Unit;
    span;
  }

let rec map_calls rewrite (expression : Sst.expression) =
  let map = map_calls rewrite in
  let expression_desc =
    match expression.expression_desc with
    | Sst.If (condition, consequent, alternative) ->
        Sst.If
          (map condition, map consequent, Option.map map alternative)
    | Sst.Let (bindings, body) ->
        Sst.Let
          (List.map (fun (pattern, value) -> (pattern, map value)) bindings, map body)
    | Sst.Sequence (first, second) -> Sst.Sequence (map first, map second)
    | Sst.Checked_arithmetic (operation, operands) ->
        Sst.Checked_arithmetic (operation, List.map map operands)
    | Sst.Compare (comparison, left, right) ->
        Sst.Compare (comparison, map left, map right)
    | Sst.Boolean_not operand -> Sst.Boolean_not (map operand)
    | Sst.Boolean_binary (operation, left, right) ->
        Sst.Boolean_binary (operation, map left, map right)
    | Sst.Direct_call { call_form; callee; arguments; recursive; type_arguments } ->
        rewrite ~call_form ~callee
          ~arguments:
            (List.map
               (fun argument ->
                 let label, value = Sst.require_value_argument argument in
                 Sst.Value_argument { label; value = map value })
               arguments)
          ~type_arguments ~recursive
    | desc -> desc
  in
  { expression with expression_desc }

let invalid_depth program literal =
  let target = recursive_definition program in
  let proof = List.hd (proof_definitions program) in
  let body =
    match proof.body with
    | Sst.Proof_body proof_body ->
        Sst.Proof_body
          {
            proof_body with
            body =
              {
                proof_body.body with
                expression =
                  reveal_expression proof.span target.function_id literal;
              };
          }
    | _ -> assert false
  in
  replace_function program { proof with body }

let run_unit filename _opaque_filename non_strict_filename =
  let program = load filename in
  let recursive = recursive_definition program in
  let recursive_body = recursive_record recursive in
  check "recursive source visibility was not retained"
    (recursive_body.visibility = `Revealed);
  check "recursive source body was not authenticated"
    (match recursive_body.provenance with
    | Sst.Authenticated_typedtree _ -> true
    | Raw_semantic_body _ -> false);
  let sst = Sst.to_string program in
  check "backend fuel leaked into semantic SST"
    (not (String.contains sst '$')
    && not (String.starts_with ~prefix:"Fuel" sst)
    && not (String.contains sst '\000'));
  let vir =
    match Symbolic_executor.lower_program program with
    | Ok vir -> Vir.to_string vir
    | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  in
  check "backend fuel leaked into executor/VIR state"
    (not (contains vir "Fuel")
    && not (contains vir "$fuel")
    && not (contains vir "$enabled")
    && not (contains vir "fuel-invariance"));

  reset_creation_counters ();
  let prepared = prepare program in
  check "preparation created a solver"
    (Solver_backend.For_testing.solver_creation_count () = 0
    && (Z3_bridge.counters ()).contexts_created = 0);
  let obligations =
    Recursive_spec_encoding.For_testing.termination_obligations prepared
  in
  check "termination did not emit exact entry/edge order"
    (match List.map (fun obligation -> obligation.Vir.kind) obligations with
    | [
     Vir.Entry_measure_nonnegative _;
     Recursive_call_measure_nonnegative _;
     Recursive_call_strict_descent _;
    ] ->
        true
    | _ -> false);
  let verified = verify prepared in
  check "ordered termination lifecycle did not solve exactly three obligations"
    (Solver_backend.For_testing.solver_creation_count () = 3);
  check "ordinary termination did not migrate to exactly three direct solvers"
    ((Z3_bridge.counters ()).contexts_created = 3);

  let function_id = recursive.function_id in
  let base = query verified function_id in
  let declarations = Logic_ir.View.declarations base in
  let declaration_names =
    List.map
      (function
        | Logic_ir.View.Sort_declaration sort ->
            Logic_ir.View.named_sort_name sort
        | Function_declaration function_ ->
            Logic_ir.View.function_name function_)
      declarations
  in
  check "recursive declaration order changed"
    (declaration_names
    = [
        "Fuel";
        "fuel.zero";
        "fuel.succ";
        "spec.0.recurse";
        "spec.0.recurse$fuel";
        "spec.0.recurse$enabled";
      ]);
  let axioms = Logic_ir.View.axioms base in
  check "A1/A2/A3 qids changed"
    (List.map Logic_ir.View.axiom_qid axioms
    = [
        "spec.0.recurse.fuel-invariance";
        "spec.0.recurse.fuel-body";
        "spec.0.recurse.public-link";
      ]);
  check "A1/A2/A3 skolem ids changed"
    (List.map Logic_ir.View.axiom_skid axioms
    = [
        "spec.0.recurse.fuel-invariance.skolem";
        "spec.0.recurse.fuel-body.skolem";
        "spec.0.recurse.public-link.skolem";
      ]);
  check "A1/A2/A3 explicit pattern shape changed"
    (List.map
       (fun axiom ->
         List.map List.length (Logic_ir.View.axiom_patterns axiom))
       axioms
    = [ [ 1 ]; [ 1 ]; [ 2 ] ]);
  let a2 = List.nth axioms 1 in
  let a2_pattern =
    List.hd (List.hd (Logic_ir.View.axiom_patterns a2))
  in
  check "A2 trigger lost syntactic successor fuel"
    (term_contains_successor a2_pattern);
  check "A2 body manufactures successor recursion"
    (match Logic_ir.View.term_node (Logic_ir.View.axiom_body a2) with
    | Logic_ir.View.Equal (_, expanded) ->
        not (term_contains_successor expanded)
    | _ -> false);
  check "opaque base query unexpectedly activates a definition"
    (Logic_ir.View.assertions base = []);

  List.iter
    (fun depth ->
      let activated =
        match
          Recursive_spec_encoding.activated_query verified function_id
            ~depths:[ depth ]
        with
        | Ok query -> query
        | Error error ->
            fail "%s" (Recursive_spec_encoding.error_to_string error)
      in
      check (Printf.sprintf "depth %d did not add one activation" depth)
        (List.length (Logic_ir.View.assertions activated) = 1))
    [ 0; 1; 2; 64 ];
  let equality =
    match
      Recursive_spec_encoding.depth_equality_query verified function_id
        ~arguments:[ `Int (Z.of_int 7); `Int (Z.of_int 11) ] ~left_depth:2
        ~right_depth:64
    with
    | Ok query -> query
    | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  solve_verified "A1 simultaneous-depth equality" equality;
  let zero =
    match
      Recursive_spec_encoding.public_link_query verified function_id
        ~arguments:[ `Int Z.zero; `Int Z.zero ] ~depth:0
    with
    | Ok query -> query
    | Error error -> fail "%s" (Recursive_spec_encoding.error_to_string error)
  in
  solve_verified "zero-fuel total public link" zero;
  let direct_counters = Z3_bridge.counters () in
  check "bounded recursive queries leaked or nested solver contexts"
    (direct_counters.contexts_live = 0
    && direct_counters.maximum_contexts_live = 1
    && direct_counters.solver_resets = direct_counters.solvers_created);

  List.iter
    (fun literal ->
      expect_prepare_rejected ("invalid depth " ^ literal)
        (invalid_depth program literal))
    [ "-1"; "65"; "not-a-literal"; "999999999999999999999999999999999999" ];
  let proof = List.hd (proof_definitions program) in
  let inaccessible =
    let body =
      match proof.body with
      | Sst.Proof_body proof_body ->
          Sst.Proof_body
            {
              proof_body with
              body =
                {
                  proof_body.body with
                  expression =
                    reveal_expression proof.span
                      { Sst.function_index = 999; function_name = "hidden" }
                      "1";
                };
            }
      | _ -> assert false
    in
    replace_function program { proof with body }
  in
  expect_prepare_rejected "inaccessible definition" inaccessible;
  let cross_stage =
    let decrease = List.hd recursive.contracts.decreases in
    let bad =
      {
        decrease with
        Sst.predicate =
          {
            stage = Sst.Logical;
            expression =
              reveal_expression decrease.span recursive.function_id "1";
          };
      }
    in
    replace_function program
      {
        recursive with
        contracts =
          { recursive.contracts with Sst.decreases = [ bad ] };
      }
  in
  expect_prepare_rejected "reveal in termination context" cross_stage;
  let executable =
    replace_function program { recursive with mode = Sst.Exec }
  in
  expect_prepare_rejected "executable recursive spec" executable;
  let aggregate_parameter =
    let parameter = List.hd recursive.parameters in
    let parameter = Sst.require_value_parameter parameter in
    let pattern =
      {
        parameter.pattern with
        Sst.typ =
          Sst.Aggregate { type_index = 99; type_name = "forged_aggregate" };
      }
    in
    replace_function program
      { recursive with
        parameters = [ Sst.Value_parameter { parameter with pattern } ] }
  in
  expect_prepare_rejected "aggregate recursive parameter" aggregate_parameter;

  let make_mutual () =
    let left_id = { Sst.function_index = 0; function_name = "left" } in
    let right_id = { Sst.function_index = 1; function_name = "right" } in
    let make source id target =
      let recursive_body = recursive_record source in
      let body =
        map_calls
          (fun ~call_form ~callee ~arguments ~type_arguments ~recursive ->
            Sst.Direct_call
              {
                call_form;
                callee = (if recursive then target else callee);
                arguments;
                type_arguments;
                recursive = false;
              })
          recursive_body.body.expression
      in
      {
        source with
        Sst.function_id = id;
        body =
          Sst.Recursive_spec_definition
            {
              body = { recursive_body.body with expression = body };
              visibility = `Opaque;
              provenance = recursive_body.provenance;
            };
      }
    in
    let left = make recursive left_id right_id in
    let right = make recursive right_id left_id in
    { program with Sst.functions = [ left; right ] }
  in
  expect_prepare_rejected "circular/mutual recursive specs" (make_mutual ());

  let nonstrict = load non_strict_filename in
  reset_creation_counters ();
  let nonstrict_prepared = prepare nonstrict in
  (match Recursive_spec_encoding.verify ~timeout_ms:60_000 nonstrict_prepared with
  | Error _ -> ()
  | Ok _ -> fail "non-strict recursion received definition authority");
  check "non-strict recursion did not reach its ordered termination solver"
    (Solver_backend.For_testing.solver_creation_count () > 0);
  check "non-strict recursion did not use the direct ordinary backend"
    ((Z3_bridge.counters ()).contexts_created
    = Solver_backend.For_testing.solver_creation_count ());

  reset_creation_counters ();
  let prepared = prepare program in
  (match
     Recursive_spec_encoding.For_testing.verify_with_requirements
       ~timeout_ms:60_000 [ Logic_ir.Nonlinear_integer_arithmetic ] prepared
   with
  | Error _ -> ()
  | Ok _ -> fail "unsupported recursive backend was accepted");
  assert_zero_solvers "unsupported backend";

  print_endline
    "recursive specs: authenticated scalar totality, opacity, depths, scopes, exact A1/A2/A3, and zero-solver failures"

let run_render filename =
  let program = load filename in
  let verified = verify (prepare program) in
  let function_id = (recursive_definition program).Sst.function_id in
  let query = query verified function_id in
  match Z3_bridge.render_query direct_config query with
  | Ok (declarations, smt) ->
      print_endline declarations;
      print_endline "---";
      print_string smt
  | Error error -> fail "%s" (Z3_bridge.error_to_string error)

let declaration_names query =
  List.map
    (function
      | Logic_ir.View.Sort_declaration sort ->
          Logic_ir.View.named_sort_name sort
      | Function_declaration function_ ->
          Logic_ir.View.function_name function_)
    (Logic_ir.View.declarations query)

let replace_all ~needle ~replacement source =
  let needle_length = String.length needle in
  if needle_length = 0 then source
  else
    let buffer = Buffer.create (String.length source) in
    let rec loop offset =
      if offset >= String.length source then ()
      else if
        offset + needle_length <= String.length source
        && String.sub source offset needle_length = needle
      then (
        Buffer.add_string buffer replacement;
        loop (offset + needle_length))
      else (
        Buffer.add_char buffer source.[offset];
        loop (offset + 1))
    in
    loop 0;
    Buffer.contents buffer

let normalize_identifier_digits ~prefix source =
  let prefix_length = String.length prefix in
  let buffer = Buffer.create (String.length source) in
  let rec loop offset =
    if offset >= String.length source then ()
    else if
      offset + prefix_length <= String.length source
      && String.sub source offset prefix_length = prefix
    then (
      Buffer.add_string buffer prefix;
      Buffer.add_char buffer '#';
      let rec skip_digits index =
        if index < String.length source then
          match source.[index] with
          | '0' .. '9' -> skip_digits (index + 1)
          | _ -> index
        else index
      in
      loop (skip_digits (offset + prefix_length)))
    else (
      Buffer.add_char buffer source.[offset];
      loop (offset + 1))
  in
  loop 0;
  Buffer.contents buffer

let normalize_rendered_root root rendered =
  replace_all ~needle:root ~replacement:"ROOT" rendered
  |> normalize_identifier_digits ~prefix:"spec."
  |> normalize_identifier_digits ~prefix:"verocaml_b"

let run_helper_unit exact scalar hygienic integer_actual non_strict =
  let positive filename expected_obligations_by_root expected_expansions =
    let program = load filename in
    let expected_roots = List.length expected_obligations_by_root in
    let expected_obligations =
      List.fold_left
        (fun total (_, count) -> total + count)
        0 expected_obligations_by_root
    in
    reset_creation_counters ();
    let prepared = prepare program in
    check (filename ^ ": helper closure expansion count changed")
      (Recursive_spec_encoding.For_testing.helper_expansion_count ()
      = expected_expansions);
    check (filename ^ ": recursive roots were not lowered exactly once")
      (Recursive_spec_encoding.For_testing.recursive_lowering_count ()
      = expected_roots);
    assert_zero_solvers (filename ^ ": preparation");
    let obligations =
      Recursive_spec_encoding.For_testing.termination_obligations prepared
    in
    check (filename ^ ": self-edge/strict obligation count changed")
      (List.length obligations = expected_obligations);
    List.iter
      (fun (root, expected) ->
        let actual =
          List.fold_left
            (fun count (obligation : Vir.obligation) ->
              if String.equal obligation.function_ref.function_name root
              then count + 1
              else count)
            0 obligations
        in
        if actual <> expected then
          fail "%s: %s obligation count changed (expected %d, got %d)"
            filename root expected actual)
      expected_obligations_by_root;
    let verified = verify prepared in
    let backend_count =
      Solver_backend.For_testing.solver_creation_count ()
    in
    let direct_count = (Z3_bridge.counters ()).solvers_created in
    if direct_count <> expected_obligations || backend_count > direct_count then
      fail
        "%s: termination solver count changed (expected direct=%d, backend=%d direct=%d)"
        filename expected_obligations backend_count direct_count;
    let ids = Recursive_spec_encoding.definition_ids verified in
    let rendered =
      List.map
        (fun id ->
          let base_query = query verified id in
          let stable =
            Printf.sprintf "spec.%d.%s" id.Sst.function_index
              id.function_name
          in
          let recursive_axioms =
            List.map
              (fun axiom ->
                ( Logic_ir.View.axiom_qid axiom,
                  List.map List.length
                    (Logic_ir.View.axiom_patterns axiom) ))
              (Logic_ir.View.axioms base_query)
            |> List.filter (fun (qid, _) ->
                   String.starts_with ~prefix:stable qid)
          in
          check (filename ^ ": A1/A2/A3 qid/pattern contract changed")
            (recursive_axioms
            = [
                (stable ^ ".fuel-invariance", [ 1 ]);
                (stable ^ ".fuel-body", [ 1 ]);
                (stable ^ ".public-link", [ 2 ]);
              ]);
          let names = declaration_names base_query in
          List.iter
            (fun definition ->
              if definition.Sst.mode = Sst.Spec && not definition.recursive
              then
                check
                  (filename ^ ": nonrecursive helper received a declaration")
                  (not
                     (List.exists
                        (fun name ->
                          contains name
                            definition.function_id.function_name)
                        names)))
            program.functions;
          let second = query verified id in
          match
            ( Z3_bridge.render_query direct_config base_query,
              Z3_bridge.render_query direct_config second )
          with
          | Ok (declarations, smt), Ok (second_declarations, second_smt) ->
              check (filename ^ ": repeated Logic IR was nondeterministic")
                (String.equal declarations second_declarations
                && String.equal smt second_smt);
              (id.Sst.function_name, smt)
          | (Error error, _) | (_, Error error) ->
              fail "%s" (Z3_bridge.error_to_string error))
        ids
    in
    check (filename ^ ": helper semantics absent from A2")
      (List.exists
         (fun (_, smt) ->
           contains smt ":qid"
           && (contains smt "ite" || contains smt "(-" || contains smt "<="))
         rendered);
    check (filename ^ ": A2 builder count changed")
      (Recursive_spec_encoding.For_testing.a2_builder_construction_count ()
      = expected_roots * 2);
    rendered
  in
  ignore (positive exact [ ("spec_node_eq", 5) ] 1);
  let scalar_rendered = positive scalar [ ("count_up", 3) ] 2 in
  check "scalar helper semantics are absent from A2"
    (List.exists
       (fun (_, smt) ->
         contains smt "(or (=" && contains smt ") (<")
       scalar_rendered);
  let hygienic_rendered =
    positive hygienic
      [ ("hygienic_count", 3); ("inline_hygienic_count", 3) ]
      1
  in
  let integer_rendered =
    positive integer_actual [ ("helper_count", 3); ("inline_count", 3) ] 1
  in
  let equivalent rendered left right =
    let find name =
      match List.assoc_opt name rendered with
      | Some smt -> normalize_rendered_root name smt
      | None -> fail "missing rendered recursive root %s" name
    in
    check (left ^ "/" ^ right ^ ": extracted and inline A2 differ")
      (String.equal (find left) (find right))
  in
  equivalent hygienic_rendered "hygienic_count" "inline_hygienic_count";
  equivalent integer_rendered "helper_count" "inline_count";
  let program = load non_strict in
  reset_creation_counters ();
  let prepared = prepare program in
  check "non-strict helper did not expand once"
    (Recursive_spec_encoding.For_testing.helper_expansion_count () = 1);
  check "non-strict helper did not produce the ordinary three obligations"
    (List.length
       (Recursive_spec_encoding.For_testing.termination_obligations prepared)
    = 3);
  (match Recursive_spec_encoding.verify ~timeout_ms:60_000 prepared with
  | Error _ -> ()
  | Ok _ -> fail "non-strict expanded helper received definition authority");
  check "non-strict helper bypassed the existing termination solver"
    (Solver_backend.For_testing.solver_creation_count () > 0);
  check "non-strict helper constructed post-totality A2"
    (Recursive_spec_encoding.For_testing.a2_builder_construction_count () = 0);
  check "non-strict helper did not use the direct ordinary backend"
    ((Z3_bridge.counters ()).contexts_created
    = Solver_backend.For_testing.solver_creation_count ());
  print_endline
    "recursive Spec helpers: authenticated closure, hygienic expansion, termination parity, and A2 semantics"

let run_helper_forgery filename =
  let program = load filename in
  let copied = { program with Sst.functions = program.functions } in
  reset_creation_counters ();
  (match Recursive_spec_encoding.prepare copied with
  | Error _ -> ()
  | Ok _ -> fail "copied recursive-helper program retained authority");
  assert_zero_helper_phases "copied recursive-helper program";
  assert_zero_solvers "copied recursive-helper program";
  print_endline "copied recursive-helper certificate rejected before expansion"

let run_helper_raw_forgery filename =
  let program = load filename in
  let definition =
    match
      List.find_opt
        (fun definition ->
          match definition.Sst.body with
          | Sst.Recursive_spec_definition _ -> true
          | _ -> false)
        program.Sst.functions
    with
    | Some definition -> definition
    | None -> fail "%s has no recursive Spec" filename
  in
  reset_creation_counters ();
  check "raw helper copy retained direct-source authority"
    (Recursive_spec_encoding.For_testing.raw_helper_copy_rejected ~program
       ~definition);
  assert_zero_helper_phases "raw recursive-helper copy";
  assert_zero_solvers "raw recursive-helper copy";
  print_endline "raw helper copy rejected before expansion and solver"

let run_helper_partial_adversary filename =
  let program = load filename in
  let definition =
    match
      List.find_opt
        (fun definition ->
          match definition.Sst.body with
          | Sst.Recursive_spec_definition _ -> true
          | _ -> false)
        program.Sst.functions
    with
    | Some definition -> definition
    | None -> fail "%s has no recursive Spec" filename
  in
  reset_creation_counters ();
  check "partial helper call bypassed exact arity"
    (Recursive_spec_encoding.For_testing.partial_helper_call_rejected ~program
       ~definition);
  assert_zero_helper_phases "partial recursive-helper call";
  assert_zero_solvers "partial recursive-helper call";
  print_endline "partial helper call rejected before expansion and solver"

let run_helper_graph_adversary filename =
  let program = load filename in
  let definition =
    match
      List.find_opt
        (fun definition ->
          match definition.Sst.body with
          | Sst.Recursive_spec_definition _ -> true
          | _ -> false)
        program.Sst.functions
    with
    | Some definition -> definition
    | None -> fail "%s has no recursive Spec" filename
  in
  reset_creation_counters ();
  check "reordered closure copy bypassed strict source order"
    (Recursive_spec_encoding.For_testing.reordered_helper_closure_copy_rejected
       ~program ~definition);
  assert_zero_helper_phases "reordered recursive-helper closure copy";
  assert_zero_solvers "reordered recursive-helper closure copy";
  print_endline
    "reordered helper closure copy rejected by private graph validation"

let run_helper_import_reject dependency consumer =
  reset_creation_counters ();
  (match
     Interface_specification.verify_consumer ~timeout_ms:60_000
       ~dependency_files:[ dependency ] ~consumer_file:consumer
   with
  | Error _ -> ()
  | Ok _ -> fail "%s retained imported helper authority" consumer);
  assert_zero_helper_phases consumer;
  assert_zero_solvers consumer;
  print_endline "imported helper rejected before expansion and solver"

let run_reject filename =
  reset_creation_counters ();
  (match Typedtree_lowering.lower_file filename with
  | Error _ -> ()
  | Ok program -> (
      match Recursive_spec_encoding.prepare program with
      | Error _ -> ()
      | Ok _ -> fail "%s was accepted" filename));
  assert_zero_solvers filename;
  print_endline "adapter rejected before solver"

let run_helper_reject filename =
  reset_creation_counters ();
  (match Typedtree_lowering.lower_file filename with
  | Error _ -> ()
  | Ok program -> (
      match Recursive_spec_encoding.prepare program with
      | Error _ -> ()
      | Ok _ -> fail "%s was accepted" filename));
  assert_zero_helper_phases filename;
  assert_zero_solvers filename;
  print_endline "helper rejected before expansion and solver"

let run_render_rank filename =
  let prepared = prepare (load filename) in
  let obligation =
    Recursive_spec_encoding.For_testing.termination_obligations prepared
    |> List.find (fun obligation ->
           match obligation.Vir.kind with
           | Vir.Recursive_call_strict_descent _ -> true
           | _ -> false)
  in
  match
    Z3_bridge.render_vir
      ~requires:
        [
          Logic_ir.Named_sorts;
          Logic_ir.Uninterpreted_functions;
          Logic_ir.Linear_integer_arithmetic;
          Logic_ir.Quantifiers;
          Logic_ir.Explicit_patterns;
          Logic_ir.Quantifier_ids;
          Logic_ir.Algebraic_datatypes;
        ]
      direct_config obligation
  with
  | Ok (_, smt) -> print_string smt
  | Error error -> fail "%s" (Z3_bridge.error_to_string error)

let run_forge filename =
  let channel = open_in_bin filename in
  let program : Sst.program = Marshal.from_channel channel in
  close_in channel;
  reset_creation_counters ();
  (match Recursive_spec_encoding.prepare program with
  | Error _ -> ()
  | Ok _ -> fail "installed recursive SST forgery was accepted");
  assert_zero_solvers "installed recursive SST forgery";
  print_endline "installed recursive SST forgery rejected before both solvers"

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit"; filename; opaque_filename; non_strict_filename ] ->
      run_unit filename opaque_filename non_strict_filename
  | [ _; "render"; filename ] -> run_render filename
  | [ _; "render-rank"; filename ] -> run_render_rank filename
  | [ _; "reject"; filename ] -> run_reject filename
  | [ _; "helper-reject"; filename ] -> run_helper_reject filename
  | [ _; "forge"; filename ] -> run_forge filename
  | [ _; "helper-unit"; exact; scalar; hygienic; integer_actual; non_strict ] ->
      run_helper_unit exact scalar hygienic integer_actual non_strict
  | [ _; "helper-forgery"; filename ] -> run_helper_forgery filename
  | [ _; "helper-raw-forgery"; filename ] -> run_helper_raw_forgery filename
  | [ _; "helper-partial-adversary"; filename ] ->
      run_helper_partial_adversary filename
  | [ _; "helper-graph-adversary"; filename ] ->
      run_helper_graph_adversary filename
  | [ _; "helper-import-reject"; dependency; consumer ] ->
      run_helper_import_reject dependency consumer
  | _ ->
      fail
        "usage: recursive_specifications_tool unit FILE.cmt OPAQUE.cmt \
         NON_STRICT.cmt | (render|reject) FILE.cmt"
