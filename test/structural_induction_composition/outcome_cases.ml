open Outcome_test_support

let suite_path = "test/structural_induction_composition/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/structural_induction_composition" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let project names =
  let units = List.map String.capitalize_ascii names in
  let files =
    {
      Fixture.path = "dune-project";
      contents = "(lang dune 3.17)\n(name structural_induction_outcomes)\n";
    }
    :: {
         path = "dune";
         contents =
           Printf.sprintf
             "(library\n (name structural_induction_outcomes)\n (wrapped false)\n \
              (modules %s)\n (libraries verocaml.ghost verocaml.vstd)\n (flags (:standard \
              -ppx \"verocaml-ppx --keep-ghost\")))\n"
             (String.concat " " units);
       }
       :: List.map
            (fun name ->
              {
                Fixture.path = name ^ ".ml";
                contents = read_file ("fixtures/" ^ name ^ ".ml");
              })
            names
  in
  ( Fixture.dune_project
      {
        files;
        libraries = [ "verocaml.ghost"; "verocaml.vstd" ];
        targets = [ "@all" ];
        selected_units = units;
      },
    units )

let case name names expectation =
  let input, _units = project names in
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace input)

let require_function function_name expectation =
  Expectation.require_named_fact ("function:" ^ function_name)
    (Outcome.Function_exists function_name) expectation

let require_kind function_name kind expectation =
  Expectation.require_named_fact ("obligation-kind:" ^ function_name)
    (Outcome.Obligation_kind_exists { function_name; kind }) expectation

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      case "positive-structural-proofs" [ "list_positive"; "tree_positive" ]
        (Expectation.empty |> Expectation.status Outcome.Verified
        |> Expectation.require_unit "List_positive" Outcome.Unit_verified
        |> Expectation.require_unit "Tree_positive" Outcome.Unit_verified
        |> require_function "equal_sum" |> require_function "nonnegative_sum"
        |> require_kind "equal_sum" Outcome.Postcondition
        |> require_kind "equal_sum"
             (Outcome.Recursive_call_strict_descent { callee = "equal_sum" })
        |> require_kind "nonnegative_sum" Outcome.Postcondition
        |> require_kind "nonnegative_sum"
             (Outcome.Recursive_call_strict_descent
                { callee = "nonnegative_sum" }));
      case "one-branch-inconclusive" [ "tree_one_branch" ]
        (Expectation.empty |> Expectation.status Outcome.Inconclusive
        |> Expectation.require_semantic ~function_name:"nonnegative_sum"
             Outcome.Postcondition
        |> Expectation.require_unit "Tree_one_branch" Outcome.Unit_inconclusive
        |> require_function "nonnegative_sum");
      case "premise-omitted-counterexample" [ "tree_premise_omitted" ]
        (Expectation.empty |> Expectation.status Outcome.Counterexample
        |> Expectation.require_semantic ~function_name:"nonnegative_sum"
             Outcome.Postcondition
        |> Expectation.require_unit "Tree_premise_omitted"
             Outcome.Unit_counterexample
        |> require_function "nonnegative_sum");
      case "early-assertion-inconclusive" [ "tree_early_assertion" ]
        (Expectation.empty |> Expectation.status Outcome.Inconclusive
        |> Expectation.require_semantic ~function_name:"nonnegative_sum"
             Outcome.Assertion
        |> Expectation.require_unit "Tree_early_assertion" Outcome.Unit_inconclusive
        |> require_function "nonnegative_sum");
      case "negative-value-counterexample" [ "tree_negative_value" ]
        (Expectation.empty |> Expectation.status Outcome.Counterexample
        |> Expectation.require_semantic
             ~function_name:"negative_leaf_has_nonnegative_sum"
             Outcome.Postcondition
        |> Expectation.require_unit "Tree_negative_value"
             Outcome.Unit_counterexample
        |> require_function "negative_leaf_has_nonnegative_sum");
    ]
