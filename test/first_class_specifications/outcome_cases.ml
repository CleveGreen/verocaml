open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/first_class_specifications/outcome_cases.ml"

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let executable_directory () = absolute Sys.executable_name |> Filename.dirname

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_source name =
  let candidates =
    [
      Filename.concat (executable_directory ()) (Filename.concat "fixtures" name);
      Filename.concat (Sys.getcwd ()) (Filename.concat "fixtures" name);
      Filename.concat (Sys.getcwd ())
        (Filename.concat "test/first_class_specifications/fixtures" name);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None -> failwith ("fixture source is unavailable: " ^ name)

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

let verified_expectation module_name functions =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit module_name Outcome.Unit_verified)
    functions

let verified_case ~name ~module_name ~fixture functions =
  Suite.case ~name ~expectation:(verified_expectation module_name functions)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let parity_case ~name ~module_name ~fixture functions =
  let expectation =
    verified_expectation module_name functions
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "dune-project")
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "prepared-cmt")
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-1")
    |> Expectation.require_named_fact "execution-mode"
         (Outcome.Function_exists "threads-2")
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

let false_controls_case =
  Suite.case ~name:"function-identity-counterexamples"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"pointwise" Outcome.Assertion
      |> Expectation.require_semantic ~function_name:"captures" Outcome.Assertion
      |> Expectation.require_unit "False_controls" Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (input "False_controls" "false_controls.ml"))

let cross_unit_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents =
              "(lang dune 3.17)\n(name first_class_cross_unit_fixture)\n";
          };
          {
            path = "dune";
            contents =
              {|
(library
 (name first_class_cross_unit_provider)
 (wrapped false)
 (modules Negative_cross_unit)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name first_class_cross_unit_consumer)
 (wrapped false)
 (modules Cross_unit_consumer)
 (libraries first_class_cross_unit_provider verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          };
          {
            path = "negative_cross_unit.ml";
            contents = fixture_source "negative_cross_unit.ml";
          };
          {
            path = "cross_unit_consumer.ml";
            contents = fixture_source "cross_unit_consumer.ml";
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Cross_unit_consumer" ];
    }

let cross_unit_case =
  Suite.case ~name:"reject-cross-unit-specification-value"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
      |> Expectation.require_unit "Cross_unit_consumer"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace cross_unit_project)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      parity_case ~name:"core-source-cmt-thread-parity" ~module_name:"Core"
        ~fixture:"core.ml"
        [
          "returned_contract";
          "verify_bool";
          "verify_option";
          "verify_if_merge";
          "verify_match_merge";
          "verify_core";
        ];
      verified_case ~name:"recursive-collections-verify"
        ~module_name:"Recursive_collections"
        ~fixture:"recursive_collections.ml" [ "verify_map" ];
      verified_case ~name:"function-quantifier-verifies" ~module_name:"Quantified"
        ~fixture:"quantified.ml" [ "quantified_application" ];
      verified_case ~name:"symbolic-broadcast-verifies"
        ~module_name:"Broadcast_symbolic" ~fixture:"broadcast_symbolic.ml"
        [ "apply_reflexive"; "symbolic_functions" ];
      parity_case ~name:"parametric-branching-source-cmt-thread-parity"
        ~module_name:"Parametric_branching" ~fixture:"parametric_branching.ml"
        [ "verify" ];
      false_controls_case;
      rejection_case ~name:"reject-mutable-specification-capture"
        ~module_name:"Negative_capture" ~fixture:"negative_capture.ml"
        ~code:"VERO_UNSUPPORTED_TYPE";
      rejection_case ~name:"reject-stored-specification-function"
        ~module_name:"Storage" ~fixture:"storage.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-local-recursive-function-value"
        ~module_name:"Local_recursive" ~fixture:"local_recursive.ml"
        ~code:"VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
      rejection_case ~name:"reject-rank-two-function-storage"
        ~module_name:"Rank2" ~fixture:"rank2.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-specification-callback-coercion"
        ~module_name:"Callback_coercion" ~fixture:"callback_coercion.ml"
        ~code:"VERO_CALLBACK_CONTRACT";
      cross_unit_case;
    ]
