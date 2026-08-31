open Outcome_test_support

let suite_path = "test/unique_mutation/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test/unique_mutation" path
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
            contents = "(lang dune 3.17)\n(name unique_mutation_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name unique_mutation_outcomes)\n (wrapped false)\n \
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

let verified_fixtures = [ "unique_records"; "destructured_unique" ]

let verified_mutations =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified verified_fixtures
    |> Expectation.require_named_fact "function:increment"
         (Outcome.Function_exists "increment")
    |> Expectation.require_named_fact "function:increment_destructured"
         (Outcome.Function_exists "increment_destructured")
  in
  Suite.case ~name:"supported-unique-mutations-verify" ~expectation
    (run verified_fixtures)

let counterexample_fixtures = [ "wrong_postcondition"; "overflow_update" ]

let mutation_failures =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> require_units Outcome.Unit_counterexample counterexample_fixtures
    |> Expectation.require_semantic ~function_name:"wrong" Outcome.Postcondition
    |> Expectation.require_semantic ~function_name:"overflow"
         (Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper))
  in
  Suite.case ~name:"invalid-mutation-claims-are-counterexamples" ~expectation
    (run counterexample_fixtures)

let unsupported_fixtures = [ "aliased_write"; "nested_write"; "ref_ops" ]

let unsupported_mutations =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> require_units Outcome.Unit_frontend_rejected unsupported_fixtures
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTATION"
  in
  Suite.case ~name:"unsupported-mutation-shapes-are-rejected" ~expectation
    (run unsupported_fixtures)

let imported_mutation =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_unit "Imported_write" Outcome.Unit_frontend_rejected
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTATION"
  in
  Suite.case ~name:"imported-mutation-is-rejected" ~expectation
    (run ~selected:[ "imported_write" ] [ "imported_box"; "imported_write" ])

let malformed_fixtures =
  [ "captured_sidecar"; "mismatched_sidecar"; "ambiguous_destructured_sidecar" ]

let malformed_sidecars =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> require_units Outcome.Unit_frontend_rejected malformed_fixtures
    |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
  in
  Suite.case ~name:"malformed-sidecars-are-rejected" ~expectation
    (run malformed_fixtures)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_mutations;
      mutation_failures;
      unsupported_mutations;
      imported_mutation;
      malformed_sidecars;
    ]
