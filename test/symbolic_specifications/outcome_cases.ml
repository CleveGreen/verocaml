open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/symbolic_specifications/outcome_cases.ml"

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
        (Filename.concat "test/symbolic_specifications/fixtures" name);
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
      Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
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

let require_function function_name expectation =
  Expectation.require_named_fact ("function:" ^ function_name)
    (Outcome.Function_exists function_name) expectation

let require_obligation function_name kind expectation =
  Expectation.require_named_fact ("obligation-kind:" ^ function_name)
    (Outcome.Obligation_kind_exists { function_name; kind }) expectation

let require_functions names expectation =
  List.fold_left
    (fun expectation function_name -> require_function function_name expectation)
    expectation names

let parity_modes expectation =
  expectation
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
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit module_name Outcome.Unit_verified
    |> require_functions functions |> parity_modes
  in
  Suite.case ~name ~expectation (parity_runner module_name fixture)

let counterexample_parity_case ~name ~module_name ~fixture ~function_name ~kind =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_semantic ~function_name kind
    |> Expectation.require_unit module_name Outcome.Unit_counterexample
    |> require_function function_name |> require_obligation function_name kind
    |> parity_modes
  in
  Suite.case ~name ~expectation (parity_runner module_name fixture)

let frontend_case ~name ~module_name ~fixture ~code =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let source_variant_parity ~environment ~workspace =
  let* declaration_order =
    Fixture.run ~environment
      ~workspace:(Filename.concat workspace "declaration-order")
      (input "Reorder" "reorder_a.ml")
  in
  let* reversed_order =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "reversed-order")
      (input "Reorder" "reorder_b.ml")
  in
  let declaration_order =
    with_modes [ ("source-variant", "declaration-order") ] declaration_order
  in
  let reversed_order =
    with_modes [ ("source-variant", "reversed-order") ] reversed_order
  in
  let* () =
    require_parity ~except:[ "source-variant" ] declaration_order reversed_order
  in
  Ok (Outcome.merge [ declaration_order; reversed_order ])

let reorder_case =
  Suite.case ~name:"declaration-order-semantic-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Reorder" Outcome.Unit_verified
      |> require_function "use" |> require_obligation "use" Outcome.Postcondition
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "declaration-order")
      |> Expectation.require_named_fact "source-variant"
           (Outcome.Function_exists "reversed-order"))
    source_variant_parity

let foreign_symbolic_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents =
              "(lang dune 3.17)\n(name foreign_symbolic_fixture)\n";
          };
          {
            path = "dune";
            contents =
              {|(library
 (name foreign_provider_fixture)
 (wrapped false)
 (modules Foreign_provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name foreign_consumer_fixture)
 (wrapped false)
 (modules Foreign_consumer)
 (libraries verocaml.ghost foreign_provider_fixture)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          };
          {
            path = "foreign_provider.ml";
            contents =
              "[%%verocaml.symbolic val foreign_image : int -> int]\n";
          };
          {
            path = "foreign_consumer.ml";
            contents =
              {|let bad (value : int) : int =
  [%verocaml.ensures fun _ -> Foreign_provider.foreign_image value = value];
  value
|};
          };
        ];
      libraries = [ "verocaml.ghost"; "foreign_provider_fixture" ];
      targets = [ "@all" ];
      selected_units = [ "Foreign_consumer" ];
    }

let foreign_symbolic_case =
  Suite.case ~name:"foreign-unit-symbolic-use-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
      |> Expectation.require_unit "Foreign_consumer"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace foreign_symbolic_project)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_parity_case ~name:"scalar-source-cmt-thread-parity"
        ~module_name:"Scalar_positive" ~fixture:"scalar_positive.ml"
        [ "unary_integer"; "unary_boolean"; "ordered_integer"; "ordered_boolean" ];
      verified_parity_case ~name:"nullary-source-cmt-thread-parity"
        ~module_name:"Nullary_positive" ~fixture:"nullary_positive.ml"
        [ "integer_identity"; "boolean_identity"; "aggregate_identity" ];
      verified_parity_case ~name:"polymorphic-nullary-source-cmt-thread-parity"
        ~module_name:"Polymorphic_nullary_positive"
        ~fixture:"polymorphic_nullary_positive.ml"
        [
          "integer_option";
          "boolean_option";
          "integer_box";
          "boolean_box";
          "arbitrary_integer";
          "arbitrary_boolean";
          "arbitrary_unit";
          "symbolic_unit_argument";
          "symbolic_unit_result";
        ];
      verified_parity_case ~name:"parametric-source-cmt-thread-parity"
        ~module_name:"Parametric_positive" ~fixture:"parametric_positive.ml"
        [ "abstract_identity"; "integer_identity"; "boolean_identity" ];
      verified_parity_case ~name:"adt-source-cmt-thread-parity"
        ~module_name:"Adt_positive" ~fixture:"adt_positive.ml"
        [
          "option_identity";
          "sequence_identity";
          "multiargument_identity";
          "closed_identity";
        ];
      verified_parity_case ~name:"congruence-source-cmt-thread-parity"
        ~module_name:"Congruence_control" ~fixture:"congruence_control.ml"
        [ "integer_congruence"; "boolean_congruence"; "aggregate_congruence" ];
      verified_parity_case ~name:"broadcast-source-cmt-thread-parity"
        ~module_name:"Broadcast_axiom_positive"
        ~fixture:"broadcast_axiom_positive.ml" [ "active" ];
      verified_parity_case ~name:"general-trigger-source-cmt-thread-parity"
        ~module_name:"General_trigger_positive"
        ~fixture:"general_trigger_positive.ml"
        [
          "integer_trigger";
          "boolean_trigger";
          "aggregate_trigger";
          "parametric_trigger";
        ];
      counterexample_parity_case ~name:"inactive-axiom-counterexample-parity"
        ~module_name:"Inactive_axiom" ~fixture:"inactive_axiom.ml"
        ~function_name:"inactive" ~kind:Outcome.Assertion;
      counterexample_parity_case
        ~name:"symbolic-injectivity-counterexample-parity"
        ~module_name:"Negative_semantics" ~fixture:"negative_semantics.ml"
        ~function_name:"false_injectivity" ~kind:Outcome.Postcondition;
      verified_parity_case ~name:"nested-declaration-source-cmt-thread-parity"
        ~module_name:"Nested_positive" ~fixture:"nested_positive.ml"
        [ "Stack.singleton"; "Stack.keep"; "Stack.length" ];
      reorder_case;
      frontend_case ~name:"exec-use-rejected" ~module_name:"Negative_exec_use"
        ~fixture:"negative_exec_use.ml" ~code:"VERO_SYMBOLIC_APPLICATION";
      frontend_case ~name:"unsupported-type-rejected"
        ~module_name:"Negative_type" ~fixture:"negative_type.ml"
        ~code:"VERO_SYMBOLIC_DECLARATION";
      frontend_case ~name:"nullary-trigger-rejected"
        ~module_name:"Negative_trigger" ~fixture:"negative_trigger.ml"
        ~code:"VERO_QUANTIFIER_TRIGGER";
      frontend_case ~name:"callback-symbolic-type-rejected"
        ~module_name:"Symbolic_callback_type" ~fixture:"symbolic_callback_type.ml"
        ~code:"VERO_INVALID_SYMBOLIC";
      frontend_case ~name:"reference-symbolic-type-rejected"
        ~module_name:"Symbolic_reference_type"
        ~fixture:"symbolic_reference_type.ml"
        ~code:"VERO_INVALID_SYMBOLIC";
      frontend_case ~name:"array-symbolic-type-rejected"
        ~module_name:"Symbolic_array_type" ~fixture:"symbolic_array_type.ml"
        ~code:"VERO_INVALID_SYMBOLIC";
      frontend_case ~name:"float-symbolic-type-rejected"
        ~module_name:"Symbolic_float_type" ~fixture:"symbolic_float_type.ml"
        ~code:"VERO_INVALID_SYMBOLIC";
      frontend_case ~name:"mutable-cycle-symbolic-type-rejected"
        ~module_name:"Symbolic_mutable_cycle"
        ~fixture:"symbolic_mutable_cycle.ml" ~code:"VERO_INVALID_RECURSIVE_RANK";
      frontend_case ~name:"nonapplication-trigger-rejected"
        ~module_name:"Trigger_nonapplication"
        ~fixture:"trigger_nonapplication.ml" ~code:"VERO_QUANTIFIER_TRIGGER";
      frontend_case ~name:"incomplete-trigger-rejected"
        ~module_name:"Trigger_incomplete" ~fixture:"trigger_incomplete.ml"
        ~code:"VERO_MALFORMED_GHOST_CALL";
      frontend_case ~name:"duplicate-trigger-rejected"
        ~module_name:"Trigger_duplicate" ~fixture:"trigger_duplicate.ml"
        ~code:"VERO_QUANTIFIER_TRIGGER";
      frontend_case ~name:"misplaced-trigger-rejected"
        ~module_name:"Trigger_misplaced" ~fixture:"trigger_misplaced.ml"
        ~code:"VERO_QUANTIFIER_TRIGGER";
      frontend_case ~name:"unit-result-trigger-rejected"
        ~module_name:"Trigger_unit_result" ~fixture:"trigger_unit_result.ml"
        ~code:"VERO_QUANTIFIER_TRIGGER";
      foreign_symbolic_case;
    ]
