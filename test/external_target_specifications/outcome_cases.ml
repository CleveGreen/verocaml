open Outcome_test_support

let suite_path = "test/external_target_specifications/outcome_cases.ml"
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
  else read_file (Filename.concat "test/external_target_specifications/fixtures" name)

let file path contents = { Fixture.path; contents }

let project_input ~name ~targets ~consumers =
  let target_modules = targets |> List.map fst |> String.concat " " in
  let consumer_modules = consumers |> List.map fst |> String.concat " " in
  let target_files =
    targets
    |> List.concat_map (fun (_, files) ->
           List.map (fun path -> file path (fixture_source path)) files)
  in
  let consumer_files =
    consumers
    |> List.concat_map (fun (_, files) ->
           List.map (fun path -> file path (fixture_source path)) files)
  in
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            (Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name);
          file "dune"
            (Printf.sprintf
               "(library\n (name target_fixture)\n (wrapped false)\n (modules \
                %s)\n (flags (:standard -ppx \"verocaml-ppx\")))\n\n\
                (library\n (name consumer_fixture)\n (wrapped false)\n (modules \
                %s)\n (libraries verocaml.ghost target_fixture)\n (flags \
                (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n\n\
                (library\n (name bootstrap_fixture)\n (wrapped false)\n \
                (modules Bootstrap)\n (libraries verocaml.ghost)\n (flags \
                (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n"
               target_modules consumer_modules);
          file "bootstrap.ml" "let ready () = true\n";
        ]
        @ target_files @ consumer_files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Bootstrap" ];
    }

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let bootstrap_expectation =
  Expectation.empty |> Expectation.status Outcome.Verified
  |> Expectation.require_unit "Bootstrap" Outcome.Unit_verified

let prepare_project input ~environment ~workspace =
  let fixture_workspace = Filename.concat workspace "compiled" in
  let* outcome = Fixture.run ~environment ~workspace:fixture_workspace input in
  let* () =
    match Expectation.check bootstrap_expectation outcome with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Filename.concat (absolute fixture_workspace) "project")

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

type artifact = {
  cmt : string;
  cmi : string;
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
      | Ok implementation -> Ok { cmt; cmi; implementation }
      | Error diagnostic ->
          Error
            (Failure.make Failure.Selected_cmt_load
               (Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
                  diagnostic.message)))
  | [] -> mismatch "no artifact for unit %s" unit_name
  | _ -> mismatch "ambiguous artifact for unit %s" unit_name

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let inventory_entry role artifact =
  (role, artifact.cmt, artifact.cmi, artifact.implementation)

let find_row unit_name scoped =
  match
    Verifier_service.scoped_rows scoped
    |> List.filter (fun row ->
           String.equal (Verifier_service.scoped_row_unit_name row) unit_name)
  with
  | [ row ] -> Ok row
  | [] -> mismatch "scope omitted unit %s" unit_name
  | _ -> mismatch "scope repeated unit %s" unit_name

let root_result unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Verifier_service.Scoped_verified,
    Verifier_service.Scoped_verification result ->
      Ok result
  | Verifier_service.Scoped_verified, Verifier_service.Scoped_rejection error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))
  | _ -> mismatch "%s was not a project root" unit_name

let require_skipped unit_name scoped =
  let* row = find_row unit_name scoped in
  match
    ( Verifier_service.scoped_row_classification row,
      Verifier_service.scoped_row_outcome row )
  with
  | Verifier_service.Scoped_skipped, Verifier_service.Scoped_skip -> Ok ()
  | _ -> mismatch "%s was not skipped" unit_name

let pair_projection ~threads ~root_name root ~dependency_name dependency =
  let* configuration = configuration threads in
  let inventory =
    [
      inventory_entry Verifier_service.Scope_root root;
      inventory_entry Verifier_service.Scope_dependency dependency;
    ]
  in
  let* request =
    match Verifier_service.scoped_request ~configuration ~inventory with
    | Ok request -> Ok request
    | Error error ->
        Error
          (Failure.make Failure.Verifier_outcome
             (Verifier_service.scoped_plan_error_message error))
  in
  let scoped = Verifier_service.verify_scope request in
  let* result = root_result root_name scoped in
  let* () = require_skipped dependency_name scoped in
  let outcome = Outcome.of_verifier_result result in
  Ok
    (Outcome.observation ~status:(Outcome.status outcome)
       ~semantic_facts:(Outcome.semantic_facts outcome)
       ~units:
         [
           (root_name, Outcome.Unit_verified);
           (dependency_name, Outcome.Unit_skipped);
         ]
       ~named_facts:(Outcome.named_facts outcome) ()
    |> Outcome.project)

let with_execution_mode mode outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (("execution-mode", Outcome.Function_exists mode)
      :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let positive_project =
  project_input ~name:"external_target_positive"
    ~targets:
      [
        ("Legacy", [ "legacy.mli"; "legacy.ml" ]);
        ("Curried_legacy", [ "curried_legacy.mli"; "curried_legacy.ml" ]);
      ]
    ~consumers:
      [
        ("Consumer", [ "consumer.mli"; "consumer.ml" ]);
        ("Curried_consumer", [ "curried_consumer.ml" ]);
        ("Explicit_type_consumer", [ "explicit_type_consumer.ml" ]);
      ]

let positive_parity ~environment ~workspace =
  let* project_root = prepare_project positive_project ~environment ~workspace in
  let run threads mode =
    let* consumer = discover_artifact project_root "Consumer" in
    let* consumer_legacy = discover_artifact project_root "Legacy" in
    let* consumer =
      pair_projection ~threads ~root_name:"Consumer" consumer
        ~dependency_name:"Legacy" consumer_legacy
    in
    let* curried = discover_artifact project_root "Curried_consumer" in
    let* curried_legacy = discover_artifact project_root "Curried_legacy" in
    let* curried =
      pair_projection ~threads ~root_name:"Curried_consumer" curried
        ~dependency_name:"Curried_legacy" curried_legacy
    in
    let* explicit = discover_artifact project_root "Explicit_type_consumer" in
    let* explicit_legacy = discover_artifact project_root "Legacy" in
    let* explicit =
      pair_projection ~threads ~root_name:"Explicit_type_consumer" explicit
        ~dependency_name:"Legacy" explicit_legacy
    in
    Outcome.merge [ consumer; curried; explicit ]
    |> with_execution_mode mode |> Result.ok
  in
  let* serial = run 1 "threads-1" in
  let* threaded = run 2 "threads-2" in
  let* () =
    match Outcome.semantic_parity ~except:[ "execution-mode" ] serial threaded with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Outcome.merge [ serial; threaded ])

let positive_case =
  Suite.case ~name:"imported-target-project-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Curried_consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Explicit_type_consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Legacy" Outcome.Unit_skipped
      |> Expectation.require_unit "Curried_legacy" Outcome.Unit_skipped
      |> Expectation.require_named_fact "function:promised"
           (Outcome.Function_exists "promised")
      |> Expectation.require_named_fact "function:combined"
           (Outcome.Function_exists "combined")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-2"))
    positive_parity

let summary_rejections =
  [
    "missing_summary";
    "call_before_summary";
    "duplicate_summary";
    "local_alias";
    "open_path";
    "module_alias";
    "functor_path";
    "wrong_order";
    "specialized_generic";
    "partial";
    "callback";
  ]

let summary_rejection_project =
  project_input ~name:"external_target_summary_rejections"
    ~targets:[ ("Legacy", [ "legacy.mli"; "legacy.ml" ]) ]
    ~consumers:
      (List.map
         (fun name -> (String.capitalize_ascii name, [ name ^ ".ml" ]))
         summary_rejections)

let rejection_outcome unit_name error =
  let code =
    Verifier_service.error_diagnostic error
    |> Option.map (fun diagnostic -> diagnostic.Diagnostic.code)
  in
  Outcome.observation ~status:Outcome.Frontend_rejected
    ?frontend_codes:(Option.map (fun value -> [ value ]) code)
    ~units:[ (unit_name, Outcome.Unit_frontend_rejected) ] ()
  |> Outcome.project

let summary_rejection_matrix ~environment ~workspace =
  let* project_root =
    prepare_project summary_rejection_project ~environment ~workspace
  in
  let* legacy = discover_artifact project_root "Legacy" in
  let* configuration = configuration 1 in
  summary_rejections
  |> List.fold_left
       (fun result name ->
         let unit_name = String.capitalize_ascii name in
         let* outcomes = result in
         let* consumer = discover_artifact project_root unit_name in
         let request =
           Verifier_service.request ~configuration
             ~consumer:consumer.implementation
             ~dependencies:[ legacy.implementation ]
         in
         match Verifier_service.verify request with
         | Ok _ -> mismatch "%s unexpectedly verified" unit_name
         | Error error -> Ok (rejection_outcome unit_name error :: outcomes))
       (Ok [])
  |> Result.map Outcome.merge

let summary_rejection_case =
  let expectation =
    List.fold_left
      (fun expectation name ->
        Expectation.require_unit (String.capitalize_ascii name)
          Outcome.Unit_frontend_rejected expectation)
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected)
      summary_rejections
  in
  Suite.case ~name:"summary-shape-rejection-matrix" ~expectation
    summary_rejection_matrix

let mode_rejections =
  [
    ("mode_summary_ghost", "Legacy");
    ("mode_summary_result_ghost", "Legacy");
    ("mode_summary_unique_parameter", "Legacy");
    ("mode_summary_unique_result", "Legacy");
    ("mode_summary_result_local", "Legacy");
    ("mode_summary_result_once", "Legacy");
    ("mode_summary_parameter_local", "Legacy");
    ("mode_summary_curried_unique_result", "Curried_legacy");
    ("mode_summary_labelled_curried_unique_result", "Curried_legacy");
    ("mode_summary_optional_curried_unique_result", "Curried_legacy");
    ("mode_summary_curried_local_result", "Curried_legacy");
    ("mode_summary_labelled_parameter_local", "Curried_legacy");
    ("mode_summary_optional_parameter_once", "Curried_legacy");
    ("mode_summary_labelled_curried_local_result", "Curried_legacy");
    ("mode_summary_optional_curried_once_result", "Curried_legacy");
    ("mode_target_curried_unique_result", "Mode_curried_legacy");
    ("mode_target_consumer", "Mode_legacy");
  ]

let mode_rejection_project =
  project_input ~name:"external_target_mode_rejections"
    ~targets:
      [
        ("Legacy", [ "legacy.mli"; "legacy.ml" ]);
        ("Curried_legacy", [ "curried_legacy.mli"; "curried_legacy.ml" ]);
        ( "Mode_curried_legacy",
          [ "mode_curried_legacy.mli"; "mode_curried_legacy.ml" ] );
        ("Mode_legacy", [ "mode_legacy.mli"; "mode_legacy.ml" ]);
      ]
    ~consumers:
      (List.map
         (fun (name, _) -> (String.capitalize_ascii name, [ name ^ ".ml" ]))
         mode_rejections)

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let mode_process_expectation =
  Expectation.empty
  |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
  |> Expectation.require_process_fact
       (Outcome.Stable_code "VERO_MALFORMED_GHOST_CALL")
  |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")

let with_mode_marker name outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (("rejected:" ^ name, Outcome.Function_exists name)
      :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let mode_rejection_matrix ~environment ~workspace =
  let* project_root =
    prepare_project mode_rejection_project ~environment ~workspace
  in
  mode_rejections
  |> List.fold_left
       (fun result (name, target_name) ->
         let* outcomes = result in
         let* root = discover_artifact project_root (String.capitalize_ascii name) in
         let* target = discover_artifact project_root target_name in
         let* outcome =
           Process_adapter.run ~cwd:workspace
             {
               program = installed_binary environment "verocaml";
               arguments =
                 [
                   "verify-project";
                   "--root";
                   root.cmt;
                   root.cmi;
                   "--dependency";
                   target.cmt;
                   target.cmi;
                   "--threads";
                   "1";
                   "--timeout-ms";
                   "60000";
                 ];
               forwarded = [ ("OCAML_COLOR", "never") ];
               cleanup_paths = [];
               adjacency = [];
             }
         in
         match Expectation.check mode_process_expectation outcome with
         | Error message -> mismatch "%s: %s" name message
         | Ok () -> Ok (with_mode_marker name outcome :: outcomes))
       (Ok [])
  |> Result.map Outcome.merge

let mode_rejection_case =
  let expectation =
    List.fold_left
      (fun expectation (name, _) ->
        Expectation.require_named_fact ("rejected:" ^ name)
          (Outcome.Function_exists name) expectation)
      mode_process_expectation mode_rejections
  in
  Suite.case ~name:"mode-rejection-matrix" ~expectation mode_rejection_matrix

let raw_carrier_project =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            "(lang dune 3.17)\n(name external_target_raw_carrier)\n";
          file "dune"
            "(library\n (name raw_target)\n (wrapped false)\n (modules Legacy))\n\n\
             (library\n (name raw_consumer)\n (wrapped false)\n (modules \
             Raw_carrier)\n (libraries raw_target))\n\n(library\n (name \
             raw_bootstrap)\n (wrapped false)\n (modules Bootstrap)\n (libraries \
             verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx \
             --keep-ghost\")))\n";
          file "legacy.mli" (fixture_source "legacy.mli");
          file "legacy.ml" (fixture_source "legacy.ml");
          file "raw_carrier.ml" (fixture_source "raw_carrier.ml");
          file "bootstrap.ml" "let ready () = true\n";
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Bootstrap" ];
    }

let raw_carrier_run ~environment ~workspace =
  let* project_root = prepare_project raw_carrier_project ~environment ~workspace in
  let* raw = discover_artifact project_root "Raw_carrier" in
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; raw.cmt ];
      forwarded = [ ("OCAML_COLOR", "never") ];
      cleanup_paths = [];
      adjacency = [];
    }

let raw_carrier_case =
  Suite.case ~name:"raw-carrier-rejected"
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact
           (Outcome.Stable_code "VERO_DEPENDENCY")
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR"))
    raw_carrier_run

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ positive_case; summary_rejection_case; mode_rejection_case; raw_carrier_case ]
