open Outcome_test_support

let suite_path = "test/recursive_aggregates/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local else "test/recursive_aggregates/" ^ local

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

let positive_fixtures = [ "recursive_node_stack" ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions
         [
           "make_stack";
           "call_make_stack";
           "construct_and_match";
           "classify_node";
           "classify_nested_node";
           "record_pattern";
           "tuple_with_record";
         ]
    |> Expectation.require_named_fact "obligation-kind:make_stack"
         (Outcome.Obligation_kind_exists
            { function_name = "make_stack"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"construction-and-projection" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_aggregates_positive"
          positive_fixtures))

let rejected_fixtures =
  [
    "imported_abstract";
    "cyclic_alias";
    "structural_equality";
    "partial_match";
    "unboxed_variant";
  ]

let frontend_rejection_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_AGGREGATE_EQUALITY"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_PARTIAL_MATCH"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_AGGREGATE"
    |> require_units Outcome.Unit_frontend_rejected rejected_fixtures
  in
  Suite.case ~name:"frontend-rejection-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_aggregates_rejections"
          rejected_fixtures))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_matrix; frontend_rejection_matrix ]
