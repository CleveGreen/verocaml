open Outcome_test_support

let suite_path = "test/recursive_rank/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local else "test/recursive_rank/" ^ local

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

let accepted_fixtures =
  [
    "positive";
    "mutable_child";
    "generic_positive";
    "generic_closed_dependent";
  ]

let accepted_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified accepted_fixtures
  in
  Suite.case ~name:"accepted-rank-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_rank_positive" accepted_fixtures))

let invalid_rank_fixtures =
  [
    "direct_negative";
    "alias_negative";
    "wrapper_negative";
    "groundless";
    "record_cycle";
    "external_proxy";
    "generic_negative_launder";
    "generic_arrow_groundless";
    "generic_abstract";
    "generic_open";
    "generic_gadt";
    "generic_higher_kinded";
  ]

let invalid_rank_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_INVALID_RECURSIVE_RANK"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_AGGREGATE"
    |> require_units Outcome.Unit_frontend_rejected invalid_rank_fixtures
  in
  Suite.case ~name:"invalid-rank-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_rank_invalid"
          invalid_rank_fixtures))

let unsupported_fixtures = [ "gadt"; "object_proxy" ]

let unsupported_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_GENERIC_USE"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
    |> require_units Outcome.Unit_frontend_rejected unsupported_fixtures
  in
  Suite.case ~name:"unsupported-type-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"recursive_rank_unsupported"
          unsupported_fixtures))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ accepted_matrix; invalid_rank_matrix; unsupported_matrix ]
