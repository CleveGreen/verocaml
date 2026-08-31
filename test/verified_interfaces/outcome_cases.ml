open Outcome_test_support

let suite_path = "test/verified_interfaces/outcome_cases.ml"
let ( let* ) = Result.bind

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let executable_directory () = absolute Sys.executable_name |> Filename.dirname

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  let built =
    Filename.concat (executable_directory ()) (Filename.concat "fixtures" name)
  in
  if Sys.file_exists built then read_file built
  else read_file (Filename.concat "test/verified_interfaces/fixtures" name)

let file path contents = { Fixture.path; contents }

let module_name path =
  path |> Filename.remove_extension |> String.capitalize_ascii

let project_input ~name files =
  let modules = "Bootstrap" :: List.map module_name files in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            (Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name);
          file "dune"
            (Printf.sprintf
               "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                (libraries verocaml.ghost)\n (flags (:standard -ppx \
                \"verocaml-ppx --keep-ghost\")))\n"
               name (String.concat " " modules));
          file "bootstrap.ml" "let ready () = true\n";
        ]
        @ List.map (fun path -> file path (fixture_source path)) files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Bootstrap" ];
    }

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let prepare_project input ~environment ~workspace =
  let prepared_workspace = Filename.concat workspace "compiled" in
  let* bootstrap =
    Fixture.run ~environment ~workspace:prepared_workspace input
  in
  let* () =
    match
      Expectation.check
        (Expectation.empty |> Expectation.status Outcome.Verified
        |> Expectation.require_unit "Bootstrap" Outcome.Unit_verified)
        bootstrap
    with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Filename.concat (absolute prepared_workspace) "project")

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

type artifact = {
  cmt : string;
  implementation : Cmt_input.implementation;
}

let discover_artifact project_root unit_name =
  let expected = String.uncapitalize_ascii unit_name ^ ".cmt" in
  match
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  with
  | [ cmt ] ->
      let cmi = Filename.remove_extension cmt ^ ".cmi" in
      (match Cmt_input.load_with_interface ~cmt ~cmi () with
      | Ok implementation -> Ok { cmt; implementation }
      | Error diagnostic ->
          Error
            (Failure.make Failure.Selected_cmt_load
               (Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
                  diagnostic.message)))
  | [] -> mismatch "no artifact for %s" unit_name
  | _ -> mismatch "ambiguous artifact for %s" unit_name

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let disposition outcome =
  match Outcome.status outcome with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let scope_projection ~threads ~root_name ~dependencies artifacts =
  let* configuration = configuration threads in
  let root = List.assoc root_name artifacts in
  let dependency_implementations =
    List.map (fun name -> (List.assoc name artifacts).implementation) dependencies
  in
  let* result =
    match
      Verifier_service.verify
        (Verifier_service.request ~configuration ~consumer:root.implementation
           ~dependencies:dependency_implementations)
    with
    | Ok result -> Ok result
    | Error error ->
        Error
          (Failure.make Failure.Verifier_outcome
             (Verifier_service.error_message error))
  in
  let outcome = Outcome.of_verifier_result result in
  Ok
    (Outcome.observation ~status:(Outcome.status outcome)
       ~semantic_facts:(Outcome.semantic_facts outcome)
       ~units:
         ((root_name, disposition outcome)
         :: List.map
              (fun name -> (name, Outcome.Unit_dependency_success))
              dependencies)
       ~named_facts:
         (( "execution-mode",
            Outcome.Function_exists (Printf.sprintf "threads-%d" threads) )
         :: Outcome.named_facts outcome)
       ()
    |> Outcome.project)

let load_artifacts project_root names =
  names
  |> List.fold_left
       (fun result name ->
         let* artifacts = result in
         let* artifact = discover_artifact project_root name in
         Ok ((name, artifact) :: artifacts))
       (Ok [])

let dependency_project =
  project_input ~name:"verified_interfaces_dependency_graph"
    [ "base.ml"; "middle.ml"; "consumer.ml" ]

let dependency_graph_parity ~environment ~workspace =
  let* project_root = prepare_project dependency_project ~environment ~workspace in
  let run threads =
    let* artifacts =
      load_artifacts project_root [ "Consumer"; "Middle"; "Base" ]
    in
    scope_projection ~threads ~root_name:"Consumer"
      ~dependencies:[ "Middle"; "Base" ] artifacts
  in
  let* serial = run 1 in
  let* threaded = run 2 in
  let* () =
    match Outcome.semantic_parity ~except:[ "execution-mode" ] serial threaded with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Outcome.merge [ serial; threaded ])

let dependency_graph_case =
  Suite.case ~name:"dependency-graph-thread-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Middle" Outcome.Unit_dependency_success
      |> Expectation.require_unit "Base" Outcome.Unit_dependency_success
      |> Expectation.require_named_fact "function:one"
           (Outcome.Function_exists "one")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-2"))
    dependency_graph_parity

let model_case ~name ~consumer ~expected_status ~unit_disposition
    ~semantic_function =
  let input =
    project_input
      ~name:
        ("verified_interfaces_"
        ^ String.map (function '-' -> '_' | character -> character) name)
      [ "model_dependency.ml"; consumer ]
  in
  let run ~environment ~workspace =
    let* project_root = prepare_project input ~environment ~workspace in
    let root_name = module_name consumer in
    let* artifacts =
      load_artifacts project_root [ root_name; "Model_dependency" ]
    in
    scope_projection ~threads:1 ~root_name
      ~dependencies:[ "Model_dependency" ] artifacts
  in
  let expectation =
    Expectation.empty |> Expectation.status expected_status
    |> Expectation.require_unit (module_name consumer) unit_disposition
    |> Expectation.require_unit "Model_dependency"
         Outcome.Unit_dependency_success
  in
  let expectation =
    match semantic_function with
    | None -> expectation
    | Some function_name ->
        Expectation.require_semantic ~function_name Outcome.Postcondition
          expectation
  in
  Suite.case ~name ~expectation run

let rejection_projects =
  [
    ( "structural-consumer",
      [ "structural_dependency.ml"; "structural_consumer.ml" ],
      "Structural_consumer",
      Some "Structural_dependency" );
    ( "model-excluded",
      [ "model_dependency.ml"; "model_excluded_consumer.ml" ],
      "Model_excluded_consumer",
      Some "Model_dependency" );
    ( "model-structural",
      [ "model_dependency.ml"; "model_structural_consumer.ml" ],
      "Model_structural_consumer",
      Some "Model_dependency" );
    ( "recursive-structural",
      [ "recursive_model_dependency.ml"; "recursive_model_structural_consumer.ml" ],
      "Recursive_model_structural_consumer",
      Some "Recursive_model_dependency" );
    ( "recursive-finite",
      [ "recursive_model_dependency.ml"; "recursive_model_finite_consumer.ml" ],
      "Recursive_model_finite_consumer",
      Some "Recursive_model_dependency" );
    ( "recursive-constructor",
      [ "recursive_model_dependency.ml"; "recursive_model_constructor_consumer.ml" ],
      "Recursive_model_constructor_consumer",
      Some "Recursive_model_dependency" );
    ( "recursive-recursion",
      [ "recursive_model_dependency.ml"; "recursive_model_recursive_consumer.ml" ],
      "Recursive_model_recursive_consumer",
      Some "Recursive_model_dependency" );
    ( "recursive-invariant",
      [ "recursive_model_dependency.ml"; "recursive_model_invariant_consumer.ml" ],
      "Recursive_model_invariant_consumer",
      Some "Recursive_model_dependency" );
    ( "recursive-reveal",
      [ "recursive_model_dependency.ml"; "recursive_model_reveal_consumer.ml" ],
      "Recursive_model_reveal_consumer",
      Some "Recursive_model_dependency" );
    ( "generic-model-open",
      [ "generic_model_dependency.ml"; "generic_model_open_consumer.ml" ],
      "Generic_model_open_consumer",
      Some "Generic_model_dependency" );
  ]

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let process_exit_expectation =
  Expectation.empty
  |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
  |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")

let with_rejection_marker name outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (("rejected:" ^ name, Outcome.Function_exists name)
      :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let run_rejection_project (name, files, root_name, dependency_name) ~environment
    ~workspace =
  let input =
    project_input
      ~name:("reject_" ^ String.map (function '-' -> '_' | c -> c) name)
      files
  in
  let* project_root = prepare_project input ~environment ~workspace in
  let* root = discover_artifact project_root root_name in
  let* dependency =
    match dependency_name with
    | None -> Ok None
    | Some unit_name ->
        discover_artifact project_root unit_name |> Result.map Option.some
  in
  let arguments =
    [ "verify"; root.cmt ]
    @ Option.fold ~none:[]
        ~some:(fun artifact -> [ "--dependency"; artifact.cmt ])
        dependency
  in
  let* outcome =
    Process_adapter.run ~cwd:workspace
      {
        program = installed_binary environment "verocaml";
        arguments;
        forwarded = [ ("OCAML_COLOR", "never") ];
        cleanup_paths = [];
        adjacency = [];
      }
  in
  match Expectation.check process_exit_expectation outcome with
  | Ok () -> Ok (with_rejection_marker name outcome)
  | Error message -> mismatch "%s: %s" name message

let rejection_cases =
  List.map
    (fun ((name, _, _, _) as project) ->
      Suite.case ~name:("reject-" ^ name)
        ~expectation:
          (process_exit_expectation
          |> Expectation.require_named_fact ("rejected:" ^ name)
               (Outcome.Function_exists name))
        (run_rejection_project project))
    rejection_projects

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (dependency_graph_case
    :: model_case ~name:"retained-model-verifies" ~consumer:"model_consumer.ml"
         ~expected_status:Outcome.Verified
         ~unit_disposition:Outcome.Unit_verified ~semantic_function:None
    :: model_case ~name:"retained-model-postcondition-counterexample"
         ~consumer:"model_failure_consumer.ml"
         ~expected_status:Outcome.Counterexample
         ~unit_disposition:Outcome.Unit_counterexample
         ~semantic_function:(Some "observe")
    :: rejection_cases)
