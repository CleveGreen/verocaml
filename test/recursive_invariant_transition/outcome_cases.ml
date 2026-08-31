open Outcome_test_support

let suite_path = "test/recursive_invariant_transition/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local
  else "test/recursive_invariant_transition/" ^ local

let project_input ~project_name fixtures =
  let modules = List.map module_name fixtures in
  let files =
    [
      {
        Fixture.path = "dune-project";
        contents =
          Printf.sprintf "(lang dune 3.17)\n(name %s)\n" project_name;
      };
      {
        path = "dune";
        contents =
          Printf.sprintf
            "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
             (libraries verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx \
             --keep-ghost\")))\n"
            project_name (String.concat " " modules);
      };
    ]
    @ List.map
        (fun fixture ->
          {
            Fixture.path = fixture ^ ".ml";
            contents = read_file (fixture_path fixture);
          })
        fixtures
  in
  Fixture.dune_project
    {
      files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = modules;
    }

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let require_units disposition fixtures expectation =
  List.fold_left
    (fun expectation fixture ->
      Expectation.require_unit (module_name fixture) disposition expectation)
    expectation fixtures

let require_functions names expectation =
  List.fold_left
    (fun expectation name ->
      Expectation.require_named_fact ("function:" ^ name)
        (Outcome.Function_exists name) expectation)
    expectation names

let positive_fixtures =
  [
    "constructor_drop";
    "constructor_zero_head";
    "root_rebase";
    "nested_cut";
    "equal_branch";
    "terminal_snapshot";
  ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions
         [
           "constructor_drop";
           "constructor_zero_head";
           "root_rebase";
           "nested_cut";
           "equal_branch";
           "terminal_snapshot";
         ]
    |> Expectation.require_named_fact "obligation-kind:Stack.drop"
         (Outcome.Obligation_kind_exists
            {
              function_name = "Stack.drop";
              kind =
                Outcome.Invariant_validity
                  {
                    invariant_id = "invariant:Stack.t:2:Stack.invariant:1";
                    boundary = "transition-preservation";
                  };
            })
  in
  Suite.case ~name:"verified-transition-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_transition_positive"
          positive_fixtures))

let counterexample_case fixture function_name =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit (module_name fixture) Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name
         (Outcome.Invariant_validity
            {
              invariant_id = "invariant:Stack.t:2:Stack.invariant:2";
              boundary = "transition-preservation";
            })
  in
  Suite.case ~name:(fixture ^ "-preservation-counterexample") ~expectation
    (run_fixture
       (project_input ~project_name:("recursive_transition_" ^ fixture)
          [ fixture ]))

let invalid_one_write = counterexample_case "invalid_one_write" "Stack.invalidate"

let temporarily_invalid_multi_write =
  counterexample_case "temporarily_invalid_multi_write" "Stack.repair_late"

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_matrix;
      invalid_one_write;
      temporarily_invalid_multi_write;
    ]
