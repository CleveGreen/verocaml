open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/scoped_broadcasts/outcome_cases.ml"

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
        (Filename.concat "test/scoped_broadcasts/fixtures" name);
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

let counterexample_case ~name ~module_name ~fixture ~function_name ~kind =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name kind
      |> Expectation.require_unit module_name Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let source_variant_parity ~environment ~workspace =
  let* declaration_order =
    Fixture.run ~environment
      ~workspace:(Filename.concat workspace "declaration-order")
      (input "Reorder" "reorder_a.ml")
  in
  let* alpha_order =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "alpha-order")
      (input "Reorder" "reorder_b.ml")
  in
  let declaration_order =
    with_modes [ ("source-variant", "declaration-order") ] declaration_order
  in
  let alpha_order =
    with_modes [ ("source-variant", "alpha-order") ] alpha_order
  in
  let* () =
    require_parity ~except:[ "source-variant" ] declaration_order alpha_order
  in
  Ok (Outcome.merge [ declaration_order; alpha_order ])

let reorder_case =
  Suite.case ~name:"declaration-and-alpha-order-parity"
    ~expectation:
      (verified_expectation "Reorder" [ "use" ]
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "declaration-order")
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "alpha-order"))
    source_variant_parity

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      parity_case ~name:"parametric-source-cmt-thread-parity"
        ~module_name:"Parametric_positive" ~fixture:"parametric_positive.ml"
        [ "use_abstract"; "use_scalars"; "use_option_seq_tree"; "use_bool_option" ];
      verified_case ~name:"function-valued-trigger-verifies"
        ~module_name:"Function_valued_trigger"
        ~fixture:"negative_wrong_actual.ml" [ "function_binder" ];
      verified_case ~name:"broadcast-group-verifies"
        ~module_name:"Group_positive" ~fixture:"group_positive.ml" [ "grouped" ];
      verified_case ~name:"lexical-scopes-verify"
        ~module_name:"Scopes_positive" ~fixture:"scopes_positive.ml"
        [
          "before_activation";
          "Nested.nested_before";
          "Nested.nested_after";
          "parent_after_nested";
          "structure_scope";
          "expression_scope";
        ];
      verified_case ~name:"trusted-and-ordinary-axioms-verify"
        ~module_name:"Trusted_axiom" ~fixture:"trusted_axiom.ml"
        [ "sugar_use"; "long_use"; "ordinary_control" ];
      reorder_case;
      counterexample_case ~name:"inactive-broadcast-counterexample"
        ~module_name:"Inactive_control" ~fixture:"inactive_control.ml"
        ~function_name:"inactive" ~kind:Outcome.Assertion;
      counterexample_case ~name:"e-matching-counterexample"
        ~module_name:"Negative_e_matching" ~fixture:"negative_e_matching.ml"
        ~function_name:"false_pair" ~kind:Outcome.Postcondition;
    ]
