open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/parametric_core/outcome_cases.ml"

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

let rejection_case ~name ~module_name ~fixture ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let counterexample_case =
  Suite.case ~name:"logical-equality-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"false_identity"
           Outcome.Assertion
      |> Expectation.require_unit "Logical_equality_false"
           Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (input "Logical_equality_false" "logical_equality_false.ml"))

let alpha_renaming ~environment ~workspace =
  let* alpha_a =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "alpha-a")
      (input "Alpha" "alpha_a.ml")
  in
  let* alpha_b =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "alpha-b")
      (input "Alpha" "alpha_b.ml")
  in
  let alpha_a = with_modes [ ("source-variant", "alpha-a") ] alpha_a in
  let alpha_b = with_modes [ ("source-variant", "alpha-b") ] alpha_b in
  let* () = require_parity ~except:[ "source-variant" ] alpha_a alpha_b in
  Ok (Outcome.merge [ alpha_a; alpha_b ])

let alpha_case =
  Suite.case ~name:"alpha-renamed-source-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Alpha" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:id"
           (Outcome.Function_exists "id")
      |> Expectation.require_named_fact "function:choose"
           (Outcome.Function_exists "choose")
      |> Expectation.require_named_fact "function:use"
           (Outcome.Function_exists "use")
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "alpha-a")
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "alpha-b"))
    alpha_renaming

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let installed_ghost_directory environment =
  Filename.concat (Project_environment.package_root environment) "verocaml/ghost"

let process_rejection fixture ~environment ~workspace =
  let source = Filename.basename fixture in
  write_file (Filename.concat workspace source) (fixture_source fixture);
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("OCAML_COLOR", "never");
          ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
          ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
        ];
      cleanup_paths = [];
      adjacency = [];
    }

let process_rejection_case ~name ~fixture ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact (Outcome.Stable_code code)
      |> Expectation.require_process_fact (Outcome.Forwarded "VEROCAML_PPX")
      |> Expectation.require_process_fact (Outcome.Forwarded "VEROCAML_GHOST_DIR"))
    (process_rejection fixture)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_parity_case ~name:"core-source-cmt-thread-parity"
        ~module_name:"Core" ~fixture:"core.ml"
        [
          "id";
          "choose";
          "relay";
          "use_abstract";
          "omitted";
          "supplied";
          "forwarded_absent";
          "forwarded_present";
        ];
      verified_parity_case ~name:"logical-equality-parity"
        ~module_name:"Logical_equality" ~fixture:"logical_equality.ml"
        [ "reflexive"; "int_case"; "bool_case" ];
      verified_parity_case ~name:"abstract-adt-logical-equality"
        ~module_name:"Abstract_adt_logical_equality"
        ~fixture:"abstract_adt_logical_equality.ml"
        [ "reflexive"; "reconstruct" ];
      counterexample_case;
      alpha_case;
      rejection_case ~name:"reject-open-parameter-equality"
        ~module_name:"Eq_parameter" ~fixture:"eq_parameter.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-open-tuple-equality"
        ~module_name:"Eq_tuple" ~fixture:"eq_tuple.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-open-option-equality"
        ~module_name:"Eq_option" ~fixture:"eq_option.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-open-list-equality"
        ~module_name:"Eq_list" ~fixture:"eq_list.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-partial-application"
        ~module_name:"Partial_application" ~fixture:"partial_application.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-callback-contract"
        ~module_name:"Callback" ~fixture:"callback.ml"
        ~code:"VERO_CALLBACK_CONTRACT";
      rejection_case ~name:"reject-generic-mutation"
        ~module_name:"Generic_mutation" ~fixture:"generic_mutation.ml"
        ~code:"VERO_UNSUPPORTED_MUTATION";
      process_rejection_case ~name:"reject-inferred-generic-recursion"
        ~fixture:"inferred_generic_recursion.ml"
        ~code:"VERO_INVALID_RECURSIVE_RANK";
      rejection_case ~name:"reject-explicit-polymorphic-recursion"
        ~module_name:"Explicit_polymorphic_recursion"
        ~fixture:"explicit_polymorphic_recursion.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      process_rejection_case ~name:"reject-open-aggregate"
        ~fixture:"open_aggregate.ml" ~code:"VERO_REFUTABLE_PARAMETER";
      rejection_case ~name:"reject-missing-label"
        ~module_name:"Missing_label" ~fixture:"missing_label.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      process_rejection_case ~name:"source-compile-duplicate-label"
        ~fixture:"duplicate_label.ml" ~code:"VERO_SOURCE_COMPILE";
      process_rejection_case ~name:"source-compile-unknown-label"
        ~fixture:"unknown_label.ml" ~code:"VERO_SOURCE_COMPILE";
      process_rejection_case ~name:"source-compile-wrong-optional-forward"
        ~fixture:"wrong_optional_forward.ml" ~code:"VERO_SOURCE_COMPILE";
    ]
