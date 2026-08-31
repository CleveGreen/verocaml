open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/verified_callback_traversals/outcome_cases.ml"

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
        (Filename.concat "test/verified_callback_traversals/fixtures" name);
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

let semantic_negative_case =
  let functions =
    [
      "false_universal";
      "false_existential";
      "false_nested";
      "false_abstract";
      "false_option";
      "false_sequence";
      "false_tree";
    ]
  in
  let expectation =
    List.fold_left
      (fun expectation function_name ->
        Expectation.require_semantic ~function_name Outcome.Postcondition
          expectation)
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Negative_quantifier_semantic"
           Outcome.Unit_counterexample)
      functions
  in
  Suite.case ~name:"quantified-postcondition-counterexamples" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (input "Negative_quantifier_semantic"
           "negative_quantifier_semantic.ml"))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      parity_case ~name:"quantifier-source-cmt-thread-parity"
        ~module_name:"Quantifiers" ~fixture:"quantifiers.ml"
        [ "quantifiers"; "proof_quantifier"; "callback_quantifier" ];
      parity_case ~name:"symbolic-quantifier-source-cmt-thread-parity"
        ~module_name:"Positive_symbolic_quantifiers"
        ~fixture:"positive_symbolic_quantifiers.ml"
        [
          "symbolic_integer_instantiation";
          "symbolic_boolean_instantiation";
          "symbolic_multiargument_instantiation";
          "direct_spec_triggers";
          "symbolic_parametric_instantiation";
          "symbolic_aggregate_instantiation";
          "symbolic_record_instantiation";
          "nested_quantifiers";
          "quantified_postcondition";
          "spec_function_quantifiers";
          "quantified_function_binder";
          "quantified_proof";
        ];
      parity_case ~name:"quantifier-statements-source-cmt-thread-parity"
        ~module_name:"Positive_quantifier_statements"
        ~fixture:"positive_quantifier_statements.ml"
        [
          "proof_forall_integer";
          "proof_forall_boolean";
          "proof_forall_parametric";
          "proof_forall_option";
          "proof_forall_box";
          "proof_forall_sequence";
          "proof_forall_pair";
          "proof_forall_function";
          "proof_exists_integer";
          "proof_exists_boolean";
          "proof_exists_parametric";
          "proof_exists_option";
          "proof_exists_box";
          "proof_exists_sequence";
          "proof_exists_pair";
          "proof_exists_function";
          "assert_forall_integer";
          "assert_forall_boolean";
          "assert_forall_parametric";
          "assert_forall_option";
          "assert_forall_box";
          "assert_forall_sequence";
          "assert_forall_pair";
          "assert_forall_function";
          "assert_exists_integer";
          "assert_exists_boolean";
          "assert_exists_parametric";
          "assert_exists_option";
          "assert_exists_box";
          "assert_exists_sequence";
          "assert_exists_pair";
          "assert_exists_function";
        ];
      semantic_negative_case;
    ]
