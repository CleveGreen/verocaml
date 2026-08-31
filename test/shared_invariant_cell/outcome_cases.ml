open Outcome_test_support

let suite_path = "test/shared_invariant_cell/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test/shared_invariant_cell" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let project fixtures =
  let modules = List.map module_name fixtures in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name shared_invariant_cell_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name shared_invariant_cell_outcomes)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx --keep-ghost\")))\n"
                (String.concat " " modules);
          };
        ]
        @ List.map
            (fun fixture ->
              {
                Fixture.path = fixture ^ ".ml";
                contents = read_file ("fixtures/" ^ fixture ^ ".ml");
              })
            fixtures;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = modules;
    }

let run fixtures ~environment ~workspace =
  Fixture.run ~environment ~workspace (project fixtures)

let require_units disposition fixtures expectation =
  List.fold_left
    (fun expectation fixture ->
      Expectation.require_unit (module_name fixture) disposition expectation)
    expectation fixtures

let rec files_below root =
  Sys.readdir root |> Array.to_list
  |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if Sys.is_directory path then files_below path else [ path ])

let prepared_projection fixture ~environment ~workspace =
  let* source = Fixture.run ~environment ~workspace (project [ fixture ]) in
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path -> Filename.basename path = fixture ^ ".cmt")
  in
  match matches with
  | [ artifact ] -> (
      match Fixture.prepared_cmt ~declared_dependencies:[ artifact ] artifact with
      | Error message -> Error (Failure.make Failure.Selected_cmt_load message)
      | Ok input ->
          let* prepared = Fixture.run ~environment ~workspace input in
          (match Outcome.semantic_parity ~except:[] source prepared with
          | Ok () -> Ok source
          | Error message ->
              Error (Failure.make Failure.Expectation_mismatch message)))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("prepared CMT discovery failed for " ^ fixture))

let exact_pfc_parity =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Exact_pfc" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:Cell.increment"
         (Outcome.Function_exists "Cell.increment")
    |> Expectation.require_named_fact "function:Cell.get"
         (Outcome.Function_exists "Cell.get")
    |> Expectation.require_named_fact "function:bump_and_get"
         (Outcome.Function_exists "bump_and_get")
  in
  Suite.case ~name:"exact-cell-project-prepared-cmt-parity" ~expectation
    (prepared_projection "exact_pfc")

let positive_fixtures =
  [
    "two_sequential_calls";
    "two_formal_sequential_alias";
    "local_alias";
    "post_call_invariant";
    "constructor_result";
    "mixed_unique_shared";
  ]

let positive_cells =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
  in
  Suite.case ~name:"supported-invariant-cell-programs-verify" ~expectation
    (run positive_fixtures)

let counterexample_fixtures =
  [
    "delete_second_write";
    "wrong_plus_two";
    "two_formal_counterexample";
    "overflow_rhs";
    "bad_constructor";
  ]

let invalid_cells =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> require_units Outcome.Unit_counterexample counterexample_fixtures
    |> Expectation.require_semantic ~function_name:"Cell.increment"
         (Outcome.Invariant_validity
            {
              invariant_id = "invariant:Cell.t:1:Cell.invariant:2";
              boundary = "shared-invariant-close";
            })
    |> Expectation.require_semantic ~function_name:"Cell.make"
         (Outcome.Invariant_validity
            {
              invariant_id = "invariant:Cell.t:1:Cell.invariant:2";
              boundary = "constructor-establishment";
            })
  in
  Suite.case ~name:"invalid-invariant-cell-claims-are-counterexamples"
    ~expectation (run counterexample_fixtures)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ exact_pfc_parity; positive_cells; invalid_cells ]
