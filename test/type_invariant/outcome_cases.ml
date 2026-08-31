open Outcome_test_support

let suite_path = "test/type_invariant/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test/type_invariant" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let project fixtures =
  let modules = List.map module_name fixtures in
  let files =
    List.map
      (fun fixture ->
        {
          Fixture.path = fixture ^ ".ml";
          contents = read_file ("fixtures/" ^ fixture ^ ".ml");
        })
      fixtures
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name type_invariant_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name type_invariant_outcomes)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx --keep-ghost\")))\n"
                (String.concat " " modules);
          };
        ]
        @ files;
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

let counterexample_fixtures =
  [ "model_consequence_without_use"; "direct_predicate_use" ]

let counterexamples =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> require_units Outcome.Unit_counterexample counterexample_fixtures
    |> Expectation.require_semantic ~function_name:"model_consequence_without_use"
         Outcome.Assertion
    |> Expectation.require_semantic ~function_name:"bad" Outcome.Assertion
  in
  Suite.case ~name:"authority-free-claims-are-counterexamples" ~expectation
    (run counterexample_fixtures)

let frontend_case ?code ~name fixture =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_unit (module_name fixture)
         Outcome.Unit_frontend_rejected
  in
  let expectation =
    match code with
    | None -> expectation
    | Some code -> Expectation.require_frontend_code code expectation
  in
  Suite.case ~name ~expectation (run [ fixture ])

let trusted_invariant =
  frontend_case ~code:"VERO_UNSUPPORTED_STRUCTURE_ITEM"
    ~name:"trusted-invariant-is-rejected" "trusted_invariant"

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ counterexamples; trusted_invariant ]
