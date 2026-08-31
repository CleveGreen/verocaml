open Outcome_test_support

let suite_path = "test/aggregate_recursive_specifications/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let fixture_path fixture =
  let local = "fixtures/" ^ fixture ^ ".ml" in
  if Sys.file_exists local then local
  else "test/aggregate_recursive_specifications/" ^ local

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
    "exact_helper";
    "exact_inline";
    "human_pfc";
    "variant_result";
    "pass_through_result";
    "rank_free_integer_result";
    "rank_free_structural_result";
    "index_minimal";
  ]

let positive_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> require_functions
         [
           "consume_recursive_result";
           "consume_inline";
           "checked_push_n_consumer";
           "consume_variant_result";
           "consume_pass_through";
           "observe_recursive_index";
           "observe_recursive_index_alt";
           "index";
         ]
    |> Expectation.require_named_fact "obligation-kind:index"
         (Outcome.Obligation_kind_exists
            { function_name = "index"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"positive-recursive-result-matrix" ~expectation
    (run_fixture
       (project_input ~project_name:"aggregate_recursive_positive"
          positive_fixtures))

let tuple_source = read_file (fixture_path "recursive_tuple_match")

let tuple_source_project_parity ~environment ~workspace =
  let single =
    Fixture.single_source ~module_name:"Recursive_tuple_match"
      ~source:tuple_source ~libraries:[ "verocaml.ghost" ]
  in
  let explicit =
    project_input ~project_name:"aggregate_recursive_tuple"
      [ "recursive_tuple_match" ]
  in
  let* source =
    Fixture.run ~environment
      ~workspace:(Filename.concat workspace "single-source") single
  in
  let* project =
    Fixture.run ~environment
      ~workspace:(Filename.concat workspace "explicit-project") explicit
  in
  match Outcome.semantic_parity ~except:[] source project with
  | Ok () -> Ok source
  | Error message -> Error (Failure.make Failure.Expectation_mismatch message)

let tuple_parity =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Recursive_tuple_match" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:check_empty_pair"
         (Outcome.Function_exists "check_empty_pair")
    |> Expectation.require_named_fact "obligation-kind:check_empty_pair"
         (Outcome.Obligation_kind_exists
            {
              function_name = "check_empty_pair";
              kind = Outcome.Local_assertion;
            })
  in
  Suite.case ~name:"tuple-source-project-parity" ~expectation
    tuple_source_project_parity

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_matrix; tuple_parity ]
