open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/parametric_interfaces/outcome_cases.ml"

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

let require_parity ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> mismatch "%s" message

let fixture_file path =
  { Fixture.path; contents = fixture_source path }

let project_input ~name ~modules ~files ~selected_units =
  let module_names = String.concat " " modules in
  let dune =
    Printf.sprintf
      "(library\n (name %s)\n (wrapped false)\n (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n"
      name module_names
  in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents =
              Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          { path = "dune"; contents = dune };
        ]
        @ List.map fixture_file files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units;
    }

let positive_project =
  project_input ~name:"parametric_interfaces_project"
    ~modules:[ "Provider"; "Consumer"; "Open_consumer" ]
    ~files:
      [
        "provider.mli";
        "provider.ml";
        "consumer.ml";
        "open_consumer.mli";
        "open_consumer.ml";
      ]
    ~selected_units:[ "Provider"; "Consumer"; "Open_consumer" ]

let declare_prepared cmt =
  match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
  | Ok _ -> Ok ()
  | Error message -> mismatch "prepared CMT declaration: %s" message

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
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

let positive_project_parity ~environment ~workspace =
  let source_workspace = Filename.concat workspace "dune-project" in
  let* source =
    Fixture.run ~environment ~workspace:source_workspace positive_project
  in
  let project_root = Filename.concat source_workspace "project" in
  let* provider_cmt = discover_cmt project_root "Provider" in
  let* consumer_cmt = discover_cmt project_root "Consumer" in
  let* open_cmt = discover_cmt project_root "Open_consumer" in
  let* () = declare_prepared provider_cmt in
  let* () = declare_prepared consumer_cmt in
  let* () = declare_prepared open_cmt in
  let* provider = load_implementation provider_cmt in
  let* consumer = load_implementation consumer_cmt in
  let* open_consumer = load_implementation open_cmt in
  let* configuration = configuration 2 in
  let* provider_outcome =
    verify_implementation ~configuration ~unit_name:"Provider"
      ~dependencies:[] provider
  in
  let* consumer_outcome =
    verify_implementation ~configuration ~unit_name:"Consumer"
      ~dependencies:[ provider ] consumer
  in
  let* open_outcome =
    verify_implementation ~configuration ~unit_name:"Open_consumer"
      ~dependencies:[ provider ] open_consumer
  in
  let prepared =
    Outcome.merge [ provider_outcome; consumer_outcome; open_outcome ]
  in
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

let positive_project_case =
  Suite.case ~name:"provider-consumers-project-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Open_consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:id"
           (Outcome.Function_exists "id")
      |> Expectation.require_named_fact "function:int_id"
           (Outcome.Function_exists "int_id")
      |> Expectation.require_named_fact "function:bool_id"
           (Outcome.Function_exists "bool_id")
      |> Expectation.require_named_fact "function:list_append"
           (Outcome.Function_exists "list_append")
      |> Expectation.require_named_fact "function:relay"
           (Outcome.Function_exists "relay")
      |> Expectation.require_named_fact "function:relay_tree"
           (Outcome.Function_exists "relay_tree")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "dune-project")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "prepared-cmt")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-2"))
    positive_project_parity

let rejection_project module_name fixture =
  project_input
    ~name:("reject_" ^ String.uncapitalize_ascii module_name)
    ~modules:[ "Provider"; module_name ]
    ~files:[ "provider.mli"; "provider.ml"; fixture ]
    ~selected_units:[ "Provider"; module_name ]

let rejection_case ~name ~module_name ~fixture ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (rejection_project module_name fixture))

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let wrong_mode_rejection ~environment ~workspace =
  let project_workspace = Filename.concat workspace "dune-project" in
  let input = rejection_project "Wrong_mode" "wrong_mode.ml" in
  let* () =
    match Fixture.run ~environment ~workspace:project_workspace input with
    | Error failure when Failure.category failure = Failure.Verifier_outcome ->
        Ok ()
    | Error failure -> Error failure
    | Ok _ -> mismatch "wrong-mode project unexpectedly verified"
  in
  let project_root = Filename.concat project_workspace "project" in
  let* provider_cmt = discover_cmt project_root "Provider" in
  let* wrong_mode_cmt = discover_cmt project_root "Wrong_mode" in
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments =
        [ "verify"; wrong_mode_cmt; "--dependency"; provider_cmt ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("OCAML_COLOR", "never");
        ];
      cleanup_paths = [];
      adjacency = [];
    }

let wrong_mode_case =
  Suite.case ~name:"reject-wrong-mode-provider-use"
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact (Outcome.Stable_code "VERO_DEPENDENCY")
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR"))
    wrong_mode_rejection

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_project_case;
      rejection_case ~name:"reject-partial-provider-use"
        ~module_name:"Partial" ~fixture:"partial.ml"
        ~code:"VERO_UNSUPPORTED_TOP_LEVEL_BINDING";
      rejection_case ~name:"reject-higher-order-provider-callback"
        ~module_name:"Callback" ~fixture:"callback.ml"
        ~code:"VERO_CALLBACK_CONTRACT";
      wrong_mode_case;
    ]
