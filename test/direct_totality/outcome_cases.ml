open Outcome_test_support

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let base module_name status disposition =
  Expectation.empty |> Expectation.status status
  |> Expectation.require_unit module_name disposition

let require_functions names expectation =
  List.fold_left
    (fun expectation name ->
      Expectation.require_named_fact ("function:" ^ name)
        (Outcome.Function_exists name) expectation)
    expectation names

let require_kinds function_name kinds expectation =
  List.fold_left
    (fun expectation kind ->
      Expectation.require_named_fact ("obligation-kind:" ^ function_name)
        (Outcome.Obligation_kind_exists { function_name; kind }) expectation)
    expectation kinds

let source_case ~name ~module_name ~source expectation =
  let input =
    Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name ~expectation (run_fixture input)

let countdown_source =
  {vero|let rec countdown (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases n];
  if n = 0 then 0 else countdown (n - 1)

let run_countdown (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = 0];
  countdown n
|vero}

let overflowing_measure_source =
  {vero|let rec overflowing_measure (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n + 1];
  if n = 0 then 0 else overflowing_measure (n - 1)
|vero}

let harmless_let_rec_source =
  {vero|let rec harmless_let_rec (n : int) = n
|vero}

let mutual_recursion_source =
  {vero|let rec even (n : int) = if n = 0 then true else odd (n - 1)
and odd (n : int) = if n = 0 then false else even (n - 1)
|vero}

let loop_source =
  {vero|let loop (n : int) =
  while n > 0 do
    ()
  done;
  n
|vero}

let countdown =
  base "Countdown" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "countdown"; "run_countdown" ]
  |> require_kinds "countdown"
       [ Outcome.Entry_measure_nonnegative;
         Outcome.Recursive_call_measure_nonnegative { callee = "countdown" };
         Outcome.Recursive_call_strict_descent { callee = "countdown" };
         Outcome.Call_precondition { callee = "countdown" };
         Outcome.Postcondition ]
  |> require_kinds "run_countdown"
       [ Outcome.Call_precondition { callee = "countdown" };
         Outcome.Postcondition ]
  |> source_case ~name:"countdown-totality" ~module_name:"Countdown"
       ~source:countdown_source

let overflowing_measure =
  base "Overflowing_measure" Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ "overflowing_measure" ]
  |> require_kinds "overflowing_measure"
       [ Outcome.Entry_measure_nonnegative;
         Outcome.Arithmetic_safety (Outcome.Add, Outcome.Lower);
         Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper);
         Outcome.Recursive_call_measure_nonnegative
           { callee = "overflowing_measure" };
         Outcome.Recursive_call_strict_descent
           { callee = "overflowing_measure" } ]
  |> source_case ~name:"overflowing-measure-counterexample"
       ~module_name:"Overflowing_measure" ~source:overflowing_measure_source

let harmless_let_rec =
  base "Harmless_let_rec" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "harmless_let_rec" ]
  |> source_case ~name:"harmless-let-rec" ~module_name:"Harmless_let_rec"
       ~source:harmless_let_rec_source

let mutual_recursion =
  base "Mutual_recursion" Outcome.Frontend_rejected
    Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTUAL_RECURSION"
  |> source_case ~name:"mutual-recursion-rejected" ~module_name:"Mutual_recursion"
       ~source:mutual_recursion_source

let loop =
  base "Loop" Outcome.Frontend_rejected Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code "VERO_UNSUPPORTED_LOOP"
  |> source_case ~name:"loop-rejected" ~module_name:"Loop" ~source:loop_source

let () =
  Suite.run_cli ~suite_path:"test/direct_totality/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ countdown; overflowing_measure; harmless_let_rec; mutual_recursion; loop ]
