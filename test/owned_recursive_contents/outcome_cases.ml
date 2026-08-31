open Outcome_test_support

let suite_path = "test/owned_recursive_contents/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/owned_recursive_contents" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name fixture = String.capitalize_ascii fixture

let source_project fixtures =
  let modules = List.map module_name fixtures in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name owned_recursive_contents_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name owned_recursive_contents_outcomes)\n (wrapped false)\n \
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

let positive_fixtures = [ "lifecycle_positive"; "invariant_positive" ]

let positives =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Lifecycle_positive" Outcome.Unit_verified
    |> Expectation.require_unit "Invariant_positive" Outcome.Unit_verified
  in
  Suite.case ~name:"same-project-recursive-contents-verify" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (source_project positive_fixtures))

let consumer_source =
  {|
let imported_contents (stack : Provider.Stack.t @ read) =
  match Provider.Stack.model stack with
  | Provider.End -> 0
  | Provider.More _ -> 1
|}

let cross_cmt_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name recursive_contents_import)\n";
          };
          {
            path = "dune";
            contents =
              {|
(library
 (name recursive_contents_import)
 (wrapped false)
 (modules Provider Consumer)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          };
          { path = "provider.ml"; contents = read_file "support/provider.ml" };
          { path = "consumer.ml"; contents = consumer_source };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider"; "Consumer" ];
    }

let cross_cmt_rejection =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_DEPENDENCY"
    |> Expectation.require_unit "Consumer" Outcome.Unit_frontend_rejected
    |> Expectation.require_unit "Provider" Outcome.Unit_verified
  in
  Suite.case ~name:"cross-cmt-hidden-observation-is-rejected" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace cross_cmt_project)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positives; cross_cmt_rejection ]
