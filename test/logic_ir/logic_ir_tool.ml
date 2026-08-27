let () = ignore Logic_ir_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "logic_ir_unit.ml";
      start_pos = position 0;
      end_pos = position 1;
    }

let ok = function
  | Ok value -> value
  | Error error -> fail "unexpected logic IR error: %s" (Logic_ir.error_to_string error)

let expect_error label expected_line = function
  | Ok _ -> fail "%s: invalid construction was accepted" label
  | Error error ->
      if error.Logic_ir.span.Diagnostic.start_pos.line <> expected_line then
        fail "%s: error span changed" label

let assert_true label condition = if not condition then fail "%s" label

let () =
  Z3_bridge.reset_counters ();
  let builder = Logic_ir.create () in
  let fuel =
    Logic_ir.declare_sort builder ~name:"Fuel" ~span:(span 1) |> ok
  in
  let zero =
    Logic_ir.declare_function builder ~name:"fuel_zero" ~domain:[] ~range:fuel
      ~span:(span 2)
    |> ok
  in
  let succ =
    Logic_ir.declare_function builder ~name:"fuel_succ" ~domain:[ fuel ]
      ~range:fuel ~span:(span 3)
    |> ok
  in
  let observe =
    Logic_ir.declare_function builder ~name:"observe"
      ~domain:[ fuel; Logic_ir.Int ] ~range:Logic_ir.Int ~span:(span 4)
    |> ok
  in
  let predicate =
    Logic_ir.declare_function builder ~name:"predicate"
      ~domain:[ Logic_ir.Int ] ~range:Logic_ir.Bool ~span:(span 5)
    |> ok
  in
  let fuel_binder =
    Logic_ir.bind builder ~name:"fuel" ~sort:fuel ~span:(span 6) |> ok
  in
  let n_binder =
    Logic_ir.bind builder ~name:"n" ~sort:Logic_ir.Int ~span:(span 7) |> ok
  in
  let fuel_term = Logic_ir.bound fuel_binder in
  let n_term = Logic_ir.bound n_binder in
  let succ_fuel = Logic_ir.apply ~span:(span 8) succ [ fuel_term ] |> ok in
  let observed =
    Logic_ir.apply ~span:(span 9) observe [ succ_fuel; n_term ] |> ok
  in
  let pred = Logic_ir.apply ~span:(span 10) predicate [ n_term ] |> ok in
  let positive =
    Logic_ir.greater_or_equal ~span:(span 11) n_term
      (Logic_ir.int ~span:(span 11) Z.zero)
    |> ok
  in
  let selected =
    Logic_ir.ite ~span:(span 12) pred ~then_:n_term
      ~else_:(Logic_ir.int ~span:(span 12) Z.zero)
    |> ok
  in
  let equation = Logic_ir.equal ~span:(span 13) observed selected |> ok in
  let body = Logic_ir.implies ~span:(span 14) positive equation |> ok in
  let axiom =
    Logic_ir.forall builder ~binders:[ fuel_binder; n_binder ] ~body
      ~patterns:[ [ succ_fuel; observed ] ]
      ~qid:"verocaml.observe.body" ~skid:"verocaml.observe.body.skolem"
      ~span:(span 15)
    |> ok
  in
  let zero_term = Logic_ir.apply ~span:(span 16) zero [] |> ok in
  let closed_observation =
    Logic_ir.apply ~span:(span 16) observe
      [ zero_term; Logic_ir.int ~span:(span 16) Z.one ]
    |> ok
  in
  let assertion =
    Logic_ir.equal ~span:(span 16) closed_observation
      (Logic_ir.int ~span:(span 16) Z.one)
    |> ok
  in
  let query =
    Logic_ir.query builder ~axioms:[ axiom ] ~assertions:[ assertion ]
      ~requires:[ Logic_ir.Models ] ~span:(span 17)
    |> ok
  in
  let declarations = Logic_ir.View.declarations query in
  assert_true "declaration order changed"
    (match declarations with
    | [
     Logic_ir.View.Sort_declaration fuel_sort;
     Function_declaration zero_function;
     Function_declaration succ_function;
     Function_declaration observe_function;
     Function_declaration predicate_function;
    ] ->
        Logic_ir.View.named_sort_index fuel_sort = 0
        && Logic_ir.View.function_index zero_function = 1
        && Logic_ir.View.function_index succ_function = 2
        && Logic_ir.View.function_index observe_function = 3
        && Logic_ir.View.function_index predicate_function = 4
    | _ -> false);
  let expected_requirements =
    [
        Logic_ir.Named_sorts;
        Uninterpreted_functions;
        Linear_integer_arithmetic;
        Quantifiers;
        Explicit_patterns;
        Quantifier_ids;
        Models;
      ]
  in
  if Logic_ir.requirements query <> expected_requirements then
    fail "feature requirements changed: %s"
      (Logic_ir.requirements query
      |> List.map Logic_ir.feature_to_string
      |> String.concat ",");
  Logic_ir.apply ~span:(span 20) succ
    [ Logic_ir.int ~span:(span 20) Z.zero ]
  |> expect_error "ill-sorted application" 20;
  Logic_ir.add ~span:(span 21) (Logic_ir.bool ~span:(span 21) true) n_term
  |> expect_error "ill-sorted arithmetic" 21;
  Logic_ir.equal ~span:(span 22) fuel_term n_term
  |> expect_error "cross-sort equality" 22;
  Logic_ir.ite ~span:(span 23) pred ~then_:fuel_term ~else_:n_term
  |> expect_error "ill-sorted ITE" 23;
  Logic_ir.query builder ~axioms:[] ~assertions:[ n_term ] ~requires:[]
    ~span:(span 24)
  |> expect_error "non-Boolean assertion" 7;
  Logic_ir.query builder ~axioms:[] ~assertions:[ pred ] ~requires:[]
    ~span:(span 25)
  |> expect_error "unbound assertion" 10;
  Logic_ir.forall builder ~binders:[ fuel_binder; n_binder ] ~body
    ~patterns:[] ~qid:"qid" ~skid:"skid" ~span:(span 26)
  |> expect_error "missing explicit pattern" 26;
  Logic_ir.forall builder ~binders:[ fuel_binder; n_binder ] ~body
    ~patterns:[ [ fuel_term; observed ] ] ~qid:"qid" ~skid:"skid"
    ~span:(span 27)
  |> expect_error "bare-binder pattern" 6;
  Logic_ir.forall builder ~binders:[ fuel_binder; n_binder ] ~body
    ~patterns:[ [ succ_fuel ] ] ~qid:"qid" ~skid:"skid" ~span:(span 28)
  |> expect_error "incomplete pattern coverage" 28;
  Logic_ir.forall builder ~binders:[ fuel_binder; n_binder ] ~body
    ~patterns:[ [ succ_fuel; observed ] ] ~qid:"" ~skid:"skid"
    ~span:(span 29)
  |> expect_error "empty qid" 29;
  let duplicate_name =
    Logic_ir.bind builder ~name:"n" ~sort:Logic_ir.Int ~span:(span 30) |> ok
  in
  Logic_ir.forall builder ~binders:[ n_binder; duplicate_name ] ~body
    ~patterns:[ [ observed ] ] ~qid:"qid" ~skid:"skid" ~span:(span 30)
  |> expect_error "duplicate binder name" 30;
  let foreign = Logic_ir.create () in
  let foreign_sort =
    Logic_ir.declare_sort foreign ~name:"Foreign" ~span:(span 31) |> ok
  in
  Logic_ir.declare_function builder ~name:"cross_signature"
    ~domain:[ foreign_sort ] ~range:Logic_ir.Bool ~span:(span 32)
  |> expect_error "cross-signature declaration" 32;
  let foreign_binder =
    Logic_ir.bind foreign ~name:"foreign" ~sort:Logic_ir.Int ~span:(span 33)
    |> ok
  in
  let foreign_term = Logic_ir.bound foreign_binder in
  Logic_ir.equal ~span:(span 34) n_term foreign_term
  |> expect_error "cross-signature term" 33;
  let counters = Z3_bridge.counters () in
  assert_true "invalid IR construction reached the backend"
    (counters.capability_resolutions = 0
    && counters.translations = 0
    && counters.contexts_created = 0
    && counters.solvers_created = 0);
  (match
     Z3_bridge.solve_query { timeout_ms = 60000; model = true } query
   with
  | Ok (Z3_bridge.Counterexample _) -> ()
  | Ok _ -> fail "valid logic query returned an unexpected outcome"
  | Error _ -> fail "valid logic query did not reach the backend");
  let live_counters = Z3_bridge.counters () in
  assert_true "backend construction counters are not live"
    (live_counters.capability_resolutions = 1
    && live_counters.translations = 1
    && live_counters.contexts_created = 1
    && live_counters.solvers_created = 1
    && live_counters.solver_resets = 1
    && live_counters.contexts_cleaned = 1
    && live_counters.contexts_live = 0
    && live_counters.selected_logics = [ "AUFLIA" ]);
  print_endline
    "logic IR checks: declarations, sorts, scope, patterns, ids, spans, requirements"
