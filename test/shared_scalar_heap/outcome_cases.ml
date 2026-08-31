open Outcome_test_support

let suite_path = "test/shared_scalar_heap/outcome_cases.ml"

let ( let* ) = Result.bind

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test/shared_scalar_heap" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let project ?selected fixtures =
  let modules = List.map module_name fixtures in
  let selected_units =
    Option.value selected ~default:fixtures |> List.map module_name
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name shared_scalar_heap_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name shared_scalar_heap_outcomes)\n (wrapped false)\n \
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
      selected_units;
    }

let run ?selected fixtures ~environment ~workspace =
  Fixture.run ~environment ~workspace (project ?selected fixtures)

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
  let input = project [ fixture ] in
  let* source = Fixture.run ~environment ~workspace input in
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let expected = fixture ^ ".cmt" in
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path -> Filename.basename path = expected)
  in
  match matches with
  | [ artifact ] -> (
      match Fixture.prepared_cmt ~declared_dependencies:[ artifact ] artifact with
      | Error message ->
          Error (Failure.make Failure.Selected_cmt_load message)
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

let increment_parity =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Increment_through_alias" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:increment_through_alias"
         (Outcome.Function_exists "increment_through_alias")
  in
  Suite.case ~name:"increment-project-prepared-cmt-parity" ~expectation
    (prepared_projection "increment_through_alias")

let positive_fixtures =
  [
    "two_write_exact_alias";
    "old_entry_views";
    "scalar_result_and_assert";
    "mixed_unique_shared";
  ]

let positive_shared_updates =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified positive_fixtures
    |> Expectation.require_named_fact "function:two_write_exact_alias"
         (Outcome.Function_exists "two_write_exact_alias")
    |> Expectation.require_named_fact "function:update_shared"
         (Outcome.Function_exists "update_shared")
    |> Expectation.require_named_fact "function:update_unique"
         (Outcome.Function_exists "update_unique")
  in
  Suite.case ~name:"supported-shared-scalar-updates-verify" ~expectation
    (run positive_fixtures)

let counterexample_fixtures =
  [
    "two_formal_counterexample";
    "two_formal_false_old";
    "local_no_disequality";
    "wrong_final_constant";
    "remove_first_write";
    "remove_second_write";
    "overflow_rhs";
  ]

let invalid_claims =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> require_units Outcome.Unit_counterexample counterexample_fixtures
    |> Expectation.require_named_fact "obligation-kind:overflow_rhs"
         (Outcome.Obligation_kind_exists
            {
              function_name = "overflow_rhs";
              kind = Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper);
            })
  in
  Suite.case ~name:"invalid-shared-scalar-claims-are-counterexamples"
    ~expectation (run counterexample_fixtures)

let trusted_body =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Reject_trusted_body" Outcome.Unit_verified
  in
  Suite.case ~name:"trusted-body-introduces-no-verification-work" ~expectation
    (run [ "reject_trusted_body" ])

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      increment_parity;
      positive_shared_updates;
      invalid_claims;
      trusted_body;
    ]
