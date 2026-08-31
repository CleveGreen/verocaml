open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/parametric_programs/outcome_cases.ml"

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let executable_directory () =
  absolute Sys.executable_name |> Filename.dirname

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  Filename.concat (executable_directory ()) (Filename.concat "fixtures" name)
  |> read_file

let input module_name fixture =
  Fixture.single_source ~module_name ~source:(fixture_source fixture)
    ~libraries:[ "verocaml.ghost" ]

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

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
  | [] -> mismatch "no prepared CMT for unit %s" unit_name
  | _ -> mismatch "ambiguous prepared CMT for unit %s" unit_name

let disposition outcome =
  match Outcome.status outcome with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let with_modes modes outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (List.map (fun (key, value) -> (key, Outcome.Function_exists value)) modes
      @ Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let load_implementation cmt =
  let cmi = Filename.remove_extension cmt ^ ".cmi" in
  let loaded =
    if Sys.file_exists cmi then Cmt_input.load_with_interface ~cmt ~cmi ()
    else Cmt_input.load cmt
  in
  match loaded with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (Printf.sprintf "%s: %s" diagnostic.Diagnostic.code diagnostic.message))

let verify_prepared ~threads ~unit_name cmt =
  let* () =
    match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
    | Ok _ -> Ok ()
    | Error message -> mismatch "prepared CMT declaration: %s" message
  in
  let* implementation = load_implementation cmt in
  let* configuration =
    match
      Verifier_service.configuration ~threads ~timeout_ms:10_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        Error
          (Failure.make Failure.Runner_internal
             (Verifier_service.configuration_error_message error))
  in
  let request =
    Verifier_service.request ~configuration ~consumer:implementation
      ~dependencies:[]
  in
  match Verifier_service.verify request with
  | Ok result ->
      let outcome = Outcome.of_verifier_result result in
      Ok (Outcome.with_unit unit_name (disposition outcome) outcome)
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let require_parity ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let parity_runner module_name fixture ~environment ~workspace =
  let source_workspace = Filename.concat workspace "dune-source" in
  let* source =
    Fixture.run ~environment ~workspace:source_workspace
      (input module_name fixture)
  in
  let* cmt =
    discover_cmt (Filename.concat source_workspace "project") module_name
  in
  let* prepared = verify_prepared ~threads:2 ~unit_name:module_name cmt in
  let source =
    with_modes
      [ ("input-mode", "dune-project"); ("execution-mode", "threads-1") ]
      source
  in
  let prepared =
    with_modes
      [ ("input-mode", "prepared-cmt"); ("execution-mode", "threads-2") ]
      prepared
  in
  let* () =
    require_parity ~except:[ "input-mode"; "execution-mode" ] source prepared
  in
  Ok (Outcome.merge [ source; prepared ])

let mode_expectation module_name =
  Expectation.empty |> Expectation.status Outcome.Verified
  |> Expectation.require_unit module_name Outcome.Unit_verified
  |> Expectation.require_named_fact "input-mode"
       (Outcome.Function_exists "dune-project")
  |> Expectation.require_named_fact "input-mode"
       (Outcome.Function_exists "prepared-cmt")
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-1")
  |> Expectation.require_named_fact "execution-mode"
       (Outcome.Function_exists "threads-2")

let verified_parity_case ~name ~module_name ~fixture functions =
  let expectation =
    List.fold_left
      (fun expectation function_name ->
        Expectation.require_named_fact ("function:" ^ function_name)
          (Outcome.Function_exists function_name) expectation)
      (mode_expectation module_name) functions
  in
  Suite.case ~name ~expectation (parity_runner module_name fixture)

let workflow_project =
  let dune =
    {|
(library
 (name workflow_project)
 (wrapped false)
 (modules Workflow_provider Workflow_consumer)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|}
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name workflow_project)\n";
          };
          { path = "dune"; contents = dune };
          {
            path = "workflow_provider.mli";
            contents = fixture_source "workflow_provider.mli";
          };
          {
            path = "workflow_provider.ml";
            contents = fixture_source "workflow_provider.ml";
          };
          {
            path = "workflow_consumer.ml";
            contents = fixture_source "workflow_consumer.ml";
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Workflow_provider"; "Workflow_consumer" ];
    }

let declare_prepared cmt =
  match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
  | Ok _ -> Ok ()
  | Error message -> mismatch "prepared CMT declaration: %s" message

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:10_000 ~rlimit:None with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let verify_implementation ~configuration ~unit_name ~dependencies implementation =
  let request =
    Verifier_service.request ~configuration ~consumer:implementation ~dependencies
  in
  match Verifier_service.verify request with
  | Ok result ->
      let outcome = Outcome.of_verifier_result result in
      Ok (Outcome.with_unit unit_name (disposition outcome) outcome)
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let workflow_project_parity ~environment ~workspace =
  let source_workspace = Filename.concat workspace "dune-project" in
  let* source =
    Fixture.run ~environment ~workspace:source_workspace workflow_project
  in
  let project_root = Filename.concat source_workspace "project" in
  let* provider_cmt = discover_cmt project_root "Workflow_provider" in
  let* consumer_cmt = discover_cmt project_root "Workflow_consumer" in
  let* () = declare_prepared provider_cmt in
  let* () = declare_prepared consumer_cmt in
  let* provider = load_implementation provider_cmt in
  let* consumer = load_implementation consumer_cmt in
  let* configuration = configuration 2 in
  let* provider_outcome =
    verify_implementation ~configuration ~unit_name:"Workflow_provider"
      ~dependencies:[] provider
  in
  let* consumer_outcome =
    verify_implementation ~configuration ~unit_name:"Workflow_consumer"
      ~dependencies:[ provider ] consumer
  in
  let prepared = Outcome.merge [ provider_outcome; consumer_outcome ] in
  let source =
    with_modes
      [ ("input-mode", "dune-project"); ("execution-mode", "threads-1") ]
      source
  in
  let prepared =
    with_modes
      [ ("input-mode", "prepared-cmt"); ("execution-mode", "threads-2") ]
      prepared
  in
  let* () =
    require_parity ~except:[ "input-mode"; "execution-mode" ] source prepared
  in
  Ok (Outcome.merge [ source; prepared ])

let workflow_project_case =
  Suite.case ~name:"workflow-library-project-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Workflow_provider" Outcome.Unit_verified
      |> Expectation.require_unit "Workflow_consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity")
      |> Expectation.require_named_fact "function:relay"
           (Outcome.Function_exists "relay")
      |> Expectation.require_named_fact "function:route"
           (Outcome.Function_exists "route")
      |> Expectation.require_named_fact "function:use_abstract"
           (Outcome.Function_exists "use_abstract")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "dune-project")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "prepared-cmt")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-2"))
    workflow_project_parity

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_parity_case ~name:"persistent-collections-parity"
        ~module_name:"Persistent_collections"
        ~fixture:"persistent_collections.ml"
        [ "identity"; "copy_option"; "reverse_into"; "mirror" ];
      verified_parity_case ~name:"callback-workflows-parity"
        ~module_name:"Callback_workflows" ~fixture:"callback_workflows.ml"
        [
          "apply";
          "transform_option";
          "transform_result";
          "route_choice";
          "transform_envelope";
        ];
      verified_parity_case ~name:"workflow-provider-parity"
        ~module_name:"Workflow_provider" ~fixture:"workflow_provider.ml"
        [ "identity"; "copy_option"; "append"; "mirror" ];
      workflow_project_case;
    ]
