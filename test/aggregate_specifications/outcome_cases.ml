open Outcome_test_support

let suite_path = "test/aggregate_specifications/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local else "test/aggregate_specifications/" ^ local

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
    "positive_aggregates";
    "positive_variant";
    "positive_model";
    "cyclic_spec";
    "polymorphic_spec";
    "function_spec";
  ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions [ "verified"; "verified_payload"; "observe" ]
  in
  Suite.case ~name:"positive-aggregate-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"aggregate_specifications_positive"
          positive_fixtures))

let rejected_fixtures =
  [
    "reference_spec";
    "object_spec";
    "foreign_spec";
    "aggregate_equality";
  ]

let frontend_rejection_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_AGGREGATE_EQUALITY"
    |> require_units Outcome.Unit_frontend_rejected rejected_fixtures
  in
  Suite.case ~name:"frontend-rejection-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"aggregate_specifications_rejections"
          rejected_fixtures))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_matrix; frontend_rejection_matrix ]
