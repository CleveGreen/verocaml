open Outcome_test_support

let suite_path = "test/trusted_external_bodies/outcome_cases.ml"

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let executable_directory () = absolute Sys.executable_name |> Filename.dirname

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  Filename.concat (executable_directory ()) (Filename.concat "fixtures" name)
  |> read_file

let module_name fixture =
  fixture |> Filename.remove_extension |> String.capitalize_ascii

let fixture_file fixture =
  { Fixture.path = fixture; contents = fixture_source fixture }

let project_input ?(preprocess = true) ~name fixtures =
  let modules = List.map module_name fixtures in
  let ppx =
    if preprocess then
      "\n (flags (:standard -ppx \"verocaml-ppx --keep-ghost\"))"
    else ""
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                 (libraries verocaml.ghost)%s)\n"
                name (String.concat " " modules) ppx;
          };
        ]
        @ List.map fixture_file fixtures;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = modules;
    }

let run_project ?(preprocess = true) ~name fixtures ~environment ~workspace =
  Fixture.run ~environment ~workspace
    (project_input ~preprocess ~name fixtures)

let require_units disposition fixtures expectation =
  List.fold_left
    (fun expectation fixture ->
      Expectation.require_unit (module_name fixture) disposition expectation)
    expectation fixtures

let require_functions functions expectation =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    expectation functions

let verified_fixtures =
  [
    "positive.ml";
    "proof_exact_pfc.ml";
    "proof_positive.ml";
    "proof_order_reversed.ml";
    "proof_unchecked_tail.ml";
    "proof_lying_trusted.ml";
  ]

let verified_contract_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified verified_fixtures
    |> require_functions [ "caller"; "assume" ]
  in
  Suite.case ~name:"verified-trusted-contract-matrix" ~expectation
    (run_project ~name:"trusted_external_verified" verified_fixtures)

let counterexample_fixtures =
  [
    "precondition_failure.ml";
    "proof_lying_unmarked.ml";
    "proof_weakened.ml";
    "proof_requires_failure.ml";
  ]

let counterexample_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> require_units Outcome.Unit_counterexample counterexample_fixtures
    |> Expectation.require_semantic ~function_name:"caller"
         (Outcome.Call_precondition { callee = "trusted" })
    |> Expectation.require_semantic ~function_name:"admit" Outcome.Postcondition
    |> Expectation.require_semantic ~function_name:"assume" Outcome.Postcondition
  in
  Suite.case ~name:"trusted-summary-counterexample-matrix" ~expectation
    (run_project ~name:"trusted_external_counterexamples"
       counterexample_fixtures)

let raw_rejection_fixtures =
  [
    "raw_attribute.ml";
    "counterfeit.ml";
    "proof_raw_attribute.ml";
    "proof_counterfeit.ml";
    "proof_foreign_carrier.ml";
  ]

let raw_rejection_matrix =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
    |> require_units Outcome.Unit_frontend_rejected raw_rejection_fixtures
  in
  Suite.case ~name:"raw-carrier-rejection-matrix" ~expectation
    (run_project ~preprocess:false ~name:"trusted_external_raw_rejections"
       raw_rejection_fixtures)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt project_root unit_name =
  let expected = String.uncapitalize_ascii unit_name ^ ".cmt" in
  match
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  with
  | [ path ] -> Ok path
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("no selected CMT for unit " ^ unit_name))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("ambiguous selected CMT for unit " ^ unit_name))

let tolerate_expected_verifier_rejection = function
  | Ok _ -> Ok ()
  | Error failure when Failure.category failure = Failure.Verifier_outcome -> Ok ()
  | Error failure -> Error failure

let prepared_rejection ~project_name ~fixtures ~unit_name ~dependencies
    ~environment ~workspace =
  let project_workspace = Filename.concat (absolute workspace) "dune-project" in
  let input = project_input ~name:project_name fixtures in
  Result.bind
    (Fixture.run ~environment ~workspace:project_workspace input
    |> tolerate_expected_verifier_rejection)
    (fun () ->
      let project_root = Filename.concat project_workspace "project" in
      Result.bind (discover_cmt project_root unit_name) (fun cmt ->
          let dependency_paths =
            dependencies
            |> List.fold_left
                 (fun result dependency ->
                   Result.bind result (fun paths ->
                       Result.map (fun path -> path :: paths)
                         (discover_cmt project_root dependency)))
                 (Ok [])
          in
          Result.bind dependency_paths (fun dependency_cmts ->
              let arguments =
                [ "verify"; cmt ]
                @ List.concat_map
                    (fun dependency -> [ "--dependency"; dependency ])
                    dependency_cmts
              in
              Process_adapter.run ~cwd:workspace
                {
                  program = installed_binary environment "verocaml";
                  arguments;
                  forwarded = [ ("OCAML_COLOR", "never") ];
                  cleanup_paths = [];
                  adjacency = [];
                })))

let process_rejection_expectation code =
  Expectation.empty
  |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
  |> Expectation.require_process_fact (Outcome.Stable_code code)
  |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")

let retained_rejection_case ~name ~fixture ~code =
  let unit_name = module_name fixture in
  Suite.case ~name ~expectation:(process_rejection_expectation code)
    (prepared_rejection ~project_name:("reject_" ^ Filename.remove_extension fixture)
       ~fixtures:[ fixture ] ~unit_name ~dependencies:[])

let imported_proof_rejection =
  Suite.case ~name:"imported-proof-body-rejected"
    ~expectation:(process_rejection_expectation "VERO_DEPENDENCY")
    (prepared_rejection ~project_name:"trusted_external_import"
       ~fixtures:[ "proof_provider.ml"; "proof_consumer.ml" ]
       ~unit_name:"Proof_consumer" ~dependencies:[ "Proof_provider" ])

let source_rejection_expectation =
  Expectation.empty
  |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
  |> Expectation.require_process_fact (Outcome.Stable_code "VERO_SOURCE_COMPILE")
  |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")

let ppx_rejection_fixtures =
  [
    "missing_ensures.ml";
    "decreases.ml";
    "assertion.ml";
    "recursive.ml";
    "payload.ml";
    "duplicate.ml";
    "nonfunction.ml";
    "misplaced.ml";
    "type_error.ml";
    "proof_conflict_spec.ml";
    "proof_conflict_type_invariant.ml";
    "proof_conflict_external_specification.ml";
    "proof_duplicate_modifier.ml";
    "proof_duplicate_role.ml";
    "proof_modifier_payload.ml";
    "proof_role_payload.ml";
    "proof_local.ml";
    "proof_misplaced.ml";
    "proof_recursive.ml";
    "proof_missing_ensures.ml";
    "proof_assertion.ml";
    "proof_decreases.ml";
    "proof_type_error.ml";
  ]

let with_source_marker fixture outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (("source-rejected:" ^ fixture, Outcome.Function_exists fixture)
      :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let run_ppx_rejection_matrix ~environment ~workspace =
  ppx_rejection_fixtures
  |> List.fold_left
       (fun result fixture ->
         Result.bind result (fun outcomes ->
             let source = Filename.concat workspace fixture in
             write_file source (fixture_source fixture);
             Result.bind
               (Process_adapter.run ~cwd:workspace
                  {
                    program = installed_binary environment "verocaml";
                    arguments = [ "verify"; fixture ];
                    forwarded = [ ("OCAML_COLOR", "never") ];
                    cleanup_paths = [];
                    adjacency = [];
                  })
               (fun outcome ->
                 match Expectation.check source_rejection_expectation outcome with
                 | Error message ->
                     Error
                       (Failure.make Failure.Expectation_mismatch
                          (fixture ^ ": " ^ message))
                 | Ok () -> Ok (with_source_marker fixture outcome :: outcomes))))
       (Ok [])
  |> Result.map Outcome.merge

let ppx_source_rejection_matrix =
  let expectation =
    List.fold_left
      (fun expectation fixture ->
        Expectation.require_named_fact ("source-rejected:" ^ fixture)
          (Outcome.Function_exists fixture) expectation)
      source_rejection_expectation ppx_rejection_fixtures
  in
  Suite.case ~name:"ppx-source-rejection-matrix" ~expectation
    run_ppx_rejection_matrix

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_contract_matrix;
      counterexample_matrix;
      retained_rejection_case ~name:"reject-unmarked-nested-mutation"
        ~fixture:"unmarked_nested.ml" ~code:"VERO_UNSUPPORTED_TYPE";
      retained_rejection_case ~name:"reject-generic-proof-body"
        ~fixture:"proof_generic.ml" ~code:"VERO_UNSUPPORTED_POLYMORPHISM";
      retained_rejection_case ~name:"reject-nonunit-proof-body"
        ~fixture:"proof_nonunit.ml" ~code:"VERO_MALFORMED_GHOST_CALL";
      retained_rejection_case ~name:"reject-proof-call-from-exec"
        ~fixture:"proof_wrong_stage.ml" ~code:"VERO_ERASED_CALL";
      raw_rejection_matrix;
      imported_proof_rejection;
      ppx_source_rejection_matrix;
    ]
