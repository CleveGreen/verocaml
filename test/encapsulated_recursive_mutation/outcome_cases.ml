open Outcome_test_support

let suite_path = "test/encapsulated_recursive_mutation/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local
  else "test/encapsulated_recursive_mutation/" ^ local

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

let positive_fixtures = [ "encapsulated_stack"; "branch_join_equal" ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions [ "client"; "Stack.step" ]
  in
  Suite.case ~name:"verified-transition-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"encapsulated_mutation_positive"
          positive_fixtures))

let mutation_rejections =
  [
    "branch_join_match";
    "branch_join_if";
    "cursor_reuse";
    "cursor_copy";
    "retained_descendant";
    "cyclic_rhs";
    "ancestor_reinsertion";
    "unknown_nested_path";
  ]

let mutation_rejection_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTATION"
    |> require_units Outcome.Unit_frontend_rejected mutation_rejections
  in
  Suite.case ~name:"unsupported-mutation-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"encapsulated_mutation_rejections"
          mutation_rejections))

let surface_rejections =
  [
    "unconstrained";
    "manifest_signature";
    "private_signature";
    "alias_escape";
    "sharing_api";
    "closure_escape";
    "hidden_global";
    "imported_dependency";
    "ungrounded";
    "branching";
    "functor";
    "first_class";
  ]

let surface_rejection_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_INVALID_RECURSIVE_RANK"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_STRUCTURE_ITEM"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TOP_LEVEL_BINDING"
    |> require_units Outcome.Unit_frontend_rejected surface_rejections
  in
  Suite.case ~name:"unsupported-surface-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"encapsulated_surface_rejections"
          surface_rejections))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_matrix; mutation_rejection_matrix; surface_rejection_matrix ]
