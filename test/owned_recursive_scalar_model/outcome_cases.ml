open Outcome_test_support

let suite_path = "test/owned_recursive_scalar_model/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/owned_recursive_scalar_model" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let module_name fixture = String.capitalize_ascii fixture

let project sources selected_units =
  let modules = List.map fst sources in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name recursive_scalar_outcomes)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name recursive_scalar_outcomes)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
                 -ppx \"verocaml-ppx --keep-ghost\")))\n"
                (String.concat " " modules);
          };
        ]
        @ List.map
            (fun (name, contents) ->
              { Fixture.path = String.uncapitalize_ascii name ^ ".ml"; contents })
            sources;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units;
    }

let fixture_source fixture =
  (module_name fixture, read_file ("fixtures/" ^ fixture ^ ".ml"))

let positive_fixtures =
  [
    "scalar_model_baseline";
    "successor_freshness";
    "nonvacuous_cut";
    "opaque_local_client";
    "constant_head_no_value_path";
    "cross_template_demand_isolation";
    "uncontracted_mutation_no_demand";
    "direct_root_reconstruction_control";
    "nested_noop_reconstruction_control";
  ]

let positives =
  let expectation =
    List.fold_left
      (fun expectation fixture ->
        Expectation.require_unit (module_name fixture) Outcome.Unit_verified
          expectation)
      (Expectation.empty |> Expectation.status Outcome.Verified)
      positive_fixtures
  in
  Suite.case ~name:"supported-recursive-scalar-models-verify" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (project (List.map fixture_source positive_fixtures)
           (List.map module_name positive_fixtures)))

let replace_once ~needle ~replacement source =
  let needle_length = String.length needle in
  let rec find index =
    if index + needle_length > String.length source then None
    else if String.sub source index needle_length = needle then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> invalid_arg ("missing fixture marker: " ^ needle)
  | Some index ->
      String.sub source 0 index ^ replacement
      ^ String.sub source (index + needle_length)
          (String.length source - index - needle_length)

let nonvacuous_source = read_file "fixtures/nonvacuous_cut.ml"

let singleton_mutant =
  nonvacuous_source
  |> replace_once ~needle:"let two_nodes first second"
       ~replacement:"let two_nodes first (second : int)"
  |> replace_once ~needle:"next = Node { value = second; next = Empty }"
       ~replacement:"next = Empty"

let omit_write_mutant =
  replace_once ~needle:"  record.next <- Empty;\n" ~replacement:""
    nonvacuous_source

let mutant_counterexamples =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Singleton_mutant" Outcome.Unit_counterexample
    |> Expectation.require_unit "Omit_write_mutant" Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"Stack.two_nodes"
         Outcome.Postcondition
    |> Expectation.require_semantic ~function_name:"Stack.cut" Outcome.Postcondition
  in
  Suite.case ~name:"recursive-scalar-mutants-are-counterexamples" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (project
           [
             ("Singleton_mutant", singleton_mutant);
             ("Omit_write_mutant", omit_write_mutant);
           ]
           [ "Singleton_mutant"; "Omit_write_mutant" ]))

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let source_compile fixture ~environment ~workspace =
  let source_name = fixture ^ ".ml" in
  write_file (Filename.concat workspace source_name)
    (read_file ("fixtures/" ^ source_name));
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments =
        [
          "verify";
          source_name;
          "--dump-sst";
          fixture ^ ".sst";
          "--dump-vir";
          fixture ^ ".vir";
        ];
      forwarded = [ ("OCAML_COLOR", "never") ];
      cleanup_paths = [ fixture ^ ".sst"; fixture ^ ".vir" ];
      adjacency = [];
    }

let source_compile_case fixture =
  let expectation =
    Expectation.empty
    |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
    |> Expectation.require_process_fact
         (Outcome.Stable_code "VERO_SOURCE_COMPILE")
    |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")
    |> Expectation.require_process_fact (Outcome.Cleaned (fixture ^ ".sst"))
    |> Expectation.require_process_fact (Outcome.Cleaned (fixture ^ ".vir"))
  in
  Suite.case ~name:(fixture ^ "-source-compile-rejection") ~expectation
    (source_compile fixture)

let import_project provider consumer =
  project
    [
      ("Provider", read_file ("support/" ^ provider ^ ".ml"));
      ("Consumer", read_file ("support/" ^ consumer ^ ".ml"));
    ]
    [ "Consumer" ]

let dependency_case ~name provider consumer =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_unit "Consumer" Outcome.Unit_frontend_rejected
  in
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (import_project provider consumer))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positives;
      mutant_counterexamples;
      source_compile_case "client_representation_access";
      source_compile_case "node_escape_or_equality";
      source_compile_case "model_effect";
      source_compile_case "ghost_mode_recovery";
      dependency_case ~name:"retained-provider-model-is-not-transferable"
        "import_provider" "import_consumer";
      dependency_case ~name:"ordinary-provider-model-is-not-transferable"
        "ordinary_provider" "ordinary_consumer";
    ]
