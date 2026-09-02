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
    Fixture.single_source ~module_name ~source
      ~libraries:[ "verocaml.vstd"; "verocaml.ghost" ]
  in
  Suite.case ~name ~expectation (run_fixture input)

let positive_source =
  {vero|let inc (x:int) = x + 1 [@@verocaml.spec]
let lemma (x:int) : unit =
  [%verocaml.requires x >= 0];
  [%verocaml.assert inc x > x];
  [%verocaml.ensures fun result -> inc x > x];
  ()
[@@verocaml.proof]
let lemma2 (x:int) : unit =
  [%verocaml.requires x >= 0];
  [%verocaml.assert inc x > x];
  lemma x;
  ()
[@@verocaml.proof]
let run (x:int) =
  [%verocaml.requires x >= 0];
  [%verocaml.proof lemma2 x];
  x
|vero}

let trusted_external_pfc_source =
  {vero|let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]

let caller (cond : bool) =
  [%verocaml.requires cond];
  [%verocaml.ensures fun _ -> cond];
  assume cond
[@@verocaml.proof]
|vero}

let positive_recursive_source =
  {vero|let positive (x : Vstd.Int.t) : unit =
  [%verocaml.requires x > 0];
  ()
[@@verocaml.proof]

let rec induct (n : Vstd.Int.t) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> n >= 0];
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    induct (n - 1);
    positive n)
[@@verocaml.proof]

let use_induct (n : Vstd.Int.t) : unit =
  [%verocaml.requires n >= 0];
  induct n
[@@verocaml.proof]

let run_recursive (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.proof use_induct n];
  n
|vero}

let recursive_bool_parameter_source =
  {vero|let rec bool_induct (n : Vstd.Int.t) (flag : bool) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n];
  if n = 0 then () else bool_induct (n - 1) (not flag)
[@@verocaml.proof]
|vero}

let recursive_negative_source =
  {vero|let rec negative (n : Vstd.Int.t) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases -1];
  if n = 0 then () else negative (n - 1)
[@@verocaml.proof]
|vero}

let recursive_non_strict_source =
  {vero|let rec non_strict (n : int) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n];
  if n = 0 then () else non_strict n
[@@verocaml.proof]
|vero}

let value_region_source =
  {vero|let lemma (x:int) : unit = () [@@verocaml.proof]
let bad x = let proof_value = [%verocaml.proof lemma x] in x
|vero}

let mutation_source =
  {vero|let bad x : unit = let mutable y = x in y <- y + 1 [@@verocaml.proof]
|vero}

let higher_order_source =
  {vero|let bad f x : unit = ignore (f x) [@@verocaml.proof]
|vero}

let loop_source =
  {vero|let bad (x : int) : unit = while false do () done [@@verocaml.proof]
|vero}

let external_source =
  {vero|let bad x : unit = print_int x [@@verocaml.proof]
|vero}

let recursive_polymorphic_source =
  {vero|let rec bad value : unit =
  [%verocaml.decreases 0];
  bad value
[@@verocaml.proof]
|vero}

let recursive_higher_order_source =
  {vero|let rec bad f (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    f n;
    bad f (n - 1))
[@@verocaml.proof]
|vero}

let recursive_external_source =
  {vero|let rec bad (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    print_int n;
    bad (n - 1))
[@@verocaml.proof]
|vero}

let positive =
  base "Positive" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "lemma"; "lemma2"; "run" ]
  |> require_kinds "lemma" [ Outcome.Assertion; Outcome.Postcondition ]
  |> require_kinds "lemma2"
       [ Outcome.Assertion; Outcome.Call_precondition { callee = "lemma" } ]
  |> require_kinds "run" [ Outcome.Call_precondition { callee = "lemma2" } ]
  |> source_case ~name:"proof-declarations-and-region" ~module_name:"Positive"
       ~source:positive_source

let trusted_external =
  base "Trusted_external_pfc" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "assume"; "caller" ]
  |> require_kinds "assume" [ Outcome.Postcondition ]
  |> require_kinds "caller" [ Outcome.Postcondition ]
  |> source_case ~name:"trusted-external-proof-call"
       ~module_name:"Trusted_external_pfc" ~source:trusted_external_pfc_source

let positive_recursive =
  base "Positive_recursive" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "positive"; "induct"; "use_induct"; "run_recursive" ]
  |> require_kinds "induct"
       [ Outcome.Entry_measure_nonnegative;
         Outcome.Call_precondition { callee = "induct" };
         Outcome.Recursive_call_measure_nonnegative { callee = "induct" };
         Outcome.Recursive_call_strict_descent { callee = "induct" };
         Outcome.Call_precondition { callee = "positive" };
         Outcome.Postcondition ]
  |> require_kinds "use_induct"
       [ Outcome.Call_precondition { callee = "induct" } ]
  |> require_kinds "run_recursive"
       [ Outcome.Call_precondition { callee = "use_induct" } ]
  |> source_case ~name:"recursive-proof-totality" ~module_name:"Positive_recursive"
       ~source:positive_recursive_source

let recursive_bool_parameter =
  base "Recursive_bool_parameter" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "bool_induct" ]
  |> require_kinds "bool_induct"
       [ Outcome.Entry_measure_nonnegative;
         Outcome.Recursive_call_measure_nonnegative { callee = "bool_induct" };
         Outcome.Recursive_call_strict_descent { callee = "bool_induct" } ]
  |> source_case ~name:"recursive-bool-proof"
       ~module_name:"Recursive_bool_parameter"
       ~source:recursive_bool_parameter_source

let recursive_negative =
  base "Recursive_negative" Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ "negative" ]
  |> Expectation.require_semantic ~function_name:"negative"
       Outcome.Entry_measure_nonnegative
  |> source_case ~name:"negative-entry-measure" ~module_name:"Recursive_negative"
       ~source:recursive_negative_source

let recursive_non_strict =
  base "Recursive_non_strict" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions [ "non_strict" ]
  |> Expectation.require_semantic ~function_name:"non_strict"
       (Outcome.Recursive_call_strict_descent { callee = "non_strict" })
  |> source_case ~name:"non-strict-recursive-measure"
       ~module_name:"Recursive_non_strict" ~source:recursive_non_strict_source

let frontend_case name module_name source code =
  base module_name Outcome.Frontend_rejected Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code code
  |> source_case ~name ~module_name ~source

let value_region =
  frontend_case "value-position-proof-region" "Value_region" value_region_source
    "VERO_MALFORMED_GHOST_CALL"
let mutation =
  frontend_case "proof-mutation" "Mutation" mutation_source
    "VERO_UNSUPPORTED_MUTATION"
let higher_order =
  frontend_case "proof-higher-order" "Higher_order" higher_order_source
    "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION"
let loop = frontend_case "proof-loop" "Loop" loop_source "VERO_UNSUPPORTED_LOOP"
let external_call =
  frontend_case "proof-external-call" "External" external_source
    "VERO_UNSUPPORTED_EXTERNAL_CALL"
let recursive_polymorphic =
  frontend_case "recursive-proof-polymorphism" "Recursive_polymorphic"
    recursive_polymorphic_source "VERO_UNSUPPORTED_GENERIC_USE"
let recursive_higher_order =
  frontend_case "recursive-proof-higher-order" "Recursive_higher_order"
    recursive_higher_order_source "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION"
let recursive_external =
  frontend_case "recursive-proof-external-call" "Recursive_external"
    recursive_external_source "VERO_UNSUPPORTED_EXTERNAL_CALL"

let () =
  Suite.run_cli ~suite_path:"test/proof_mode/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive; trusted_external; positive_recursive; recursive_bool_parameter;
      recursive_negative; recursive_non_strict; value_region; mutation;
      higher_order; loop; external_call; recursive_polymorphic;
      recursive_higher_order; recursive_external ]
