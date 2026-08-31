open Outcome_test_support

let suite_path = "test/recursive_specifications/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local
  else "test/recursive_specifications/" ^ local

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
    "recursive_with_nonrecursive_helper";
    "higher_order";
    "proof_opaque_public";
    "proof_reveal_depth";
    "structural_positive";
    "structural_actual_positive";
    "structural_actual_unranked";
  ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions
         [
           "opaque_public_symbol_is_total";
           "unfold_two_layers";
           "chain_induction";
           "round_trip";
         ]
    |> Expectation.require_named_fact "obligation-kind:chain_induction"
         (Outcome.Obligation_kind_exists
            {
              function_name = "chain_induction";
              kind = Outcome.Entry_measure_nonnegative;
            })
    |> Expectation.require_named_fact "obligation-kind:chain_induction"
         (Outcome.Obligation_kind_exists
            {
              function_name = "chain_induction";
              kind =
                Outcome.Recursive_call_strict_descent
                  { callee = "chain_induction" };
            })
  in
  Suite.case ~name:"verified-recursive-specification-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_specifications_positive"
          positive_fixtures))

let no_reveal_counterexample =
  let fixture = "proof_no_reveal" in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit (module_name fixture) Outcome.Unit_counterexample
    |> Expectation.require_semantic
         ~function_name:"opaque_equation_is_unavailable" Outcome.Postcondition
  in
  Suite.case ~name:"opaque-equation-needs-reveal" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_specifications_no_reveal"
          [ fixture ]))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_matrix; no_reveal_counterexample ]
