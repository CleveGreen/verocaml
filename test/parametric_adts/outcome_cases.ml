open Outcome_test_support

let ( let* ) = Result.bind

let suite_path = "test/parametric_adts/outcome_cases.ml"

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
    ~libraries:[ "verocaml.ghost"; "verocaml.vstd" ]

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

let verify_prepared ~environment ~threads ~unit_name cmt =
  let* () =
    match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
    | Ok _ -> Ok ()
    | Error message -> mismatch "prepared CMT declaration: %s" message
  in
  let* implementation = load_implementation cmt in
  let* providers =
    Fixture.retained_providers ~environment
      ~libraries:[ "verocaml.vstd" ]
  in
  let imported_units =
    implementation.Cmt_input.imports |> Array.to_list
    |> List.map (fun (import : Cmt_input.import) -> import.unit_name)
  in
  let dependencies =
    List.filter
      (fun (provider : Cmt_input.implementation) ->
        List.mem provider.unit_name imported_units)
      providers
  in
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
      ~dependencies
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
  let project_root = Filename.concat source_workspace "project" in
  let* cmt = discover_cmt project_root module_name in
  let* prepared =
    verify_prepared ~environment ~threads:2 ~unit_name:module_name cmt
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

let counterexample_case ~name ~module_name ~fixture ~function_name =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name Outcome.Postcondition
      |> Expectation.require_unit module_name Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let proof_matrix_case =
  let module_name = "Proof_matrix" in
  let expectation =
    mode_expectation module_name
    |> Expectation.require_named_fact "function:lemma_index_oob"
         (Outcome.Function_exists "lemma_index_oob")
    |> Expectation.require_named_fact "obligation-kind:lemma_index_oob"
         (Outcome.Obligation_kind_exists
            {
              function_name = "lemma_index_oob";
              kind = Outcome.Entry_measure_nonnegative;
            })
    |> Expectation.require_named_fact "obligation-kind:lemma_index_oob"
         (Outcome.Obligation_kind_exists
            {
              function_name = "lemma_index_oob";
              kind =
                Outcome.Recursive_call_measure_nonnegative
                  { callee = "lemma_index_oob" };
            })
    |> Expectation.require_named_fact "obligation-kind:lemma_index_oob"
         (Outcome.Obligation_kind_exists
            {
              function_name = "lemma_index_oob";
              kind =
                Outcome.Recursive_call_strict_descent
                  { callee = "lemma_index_oob" };
            })
    |> Expectation.require_named_fact "obligation-kind:lemma_index_oob"
         (Outcome.Obligation_kind_exists
            { function_name = "lemma_index_oob"; kind = Outcome.Local_assertion })
  in
  Suite.case ~name:"proof-matrix-kinds-and-parity" ~expectation
    (parity_runner module_name "proof_matrix.ml")

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
      |> Expectation.require_named_fact "function:copy"
           (Outcome.Function_exists "copy")
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

let unauthenticated_descent ~environment ~workspace =
  let source = "unauthenticated_descent.ml" in
  write_file (Filename.concat workspace source)
    (fixture_source "unauthenticated_descent.ml");
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

let unauthenticated_descent_case =
  Suite.case ~name:"reject-unauthenticated-descent"
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact (Outcome.Stable_code "VERO_DEPENDENCY")
      |> Expectation.require_process_fact (Outcome.Forwarded "VEROCAML_PPX")
      |> Expectation.require_process_fact (Outcome.Forwarded "VEROCAML_GHOST_DIR"))
    unauthenticated_descent

let abstract_false_case =
  Suite.case ~name:"abstract-logical-equality-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"false_identity"
           Outcome.Assertion
      |> Expectation.require_unit "Abstract_logical_equality_false"
           Outcome.Unit_counterexample)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (input "Abstract_logical_equality_false"
           "abstract_logical_equality_false.ml"))

let nested_external_type_case =
  let module_name = "Nested_external_types" in
  let project =
    Fixture.dune_project
      {
        files =
          [
            {
              path = "dune-project";
              contents = "(lang dune 3.17)\n(name nested_external_types)\n";
            };
            {
              path = "dune";
              contents =
                "(library\n (name nested_external_types)\n (wrapped false)\n \
                 (modules Foreign_external_types Nested_external_types)\n \
                 (libraries verocaml.ghost)\n (flags (:standard -ppx \
                 \"verocaml-ppx --keep-ghost\")))\n";
            };
            {
              path = "foreign_external_types.ml";
              contents = fixture_source "foreign_external_types.ml";
            };
            {
              path = "nested_external_types.ml";
              contents = fixture_source "nested_external_types.ml";
            };
          ];
        libraries = [ "verocaml.ghost" ];
        targets = [ "@all" ];
        selected_units = [ module_name ];
      }
  in
  Suite.case ~name:"generic-external-types-compose-when-deeply-nested"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified
      |> Expectation.require_named_fact "function:make_nested_option"
           (Outcome.Function_exists "make_nested_option")
      |> Expectation.require_named_fact "function:make_deep_value"
           (Outcome.Function_exists "make_deep_value")
      |> Expectation.require_named_fact "function:lemma_observes_box_reflexive"
           (Outcome.Function_exists "lemma_observes_box_reflexive"))
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace project)

let optional_carrier_project ~name ~consumer_module ~consumer_fixture =
  Fixture.dune_project
    {
      files =
        [
          {
            path = "dune-project";
            contents = Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name %s)\n (wrapped false)\n (modules \
                 Optional_carrier_shapes %s)\n (libraries verocaml.ghost)\n \
                 (flags (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n"
                name consumer_module;
          };
          {
            path = "optional_carrier_shapes.ml";
            contents = fixture_source "optional_carrier_shapes.ml";
          };
          {
            path = String.uncapitalize_ascii consumer_module ^ ".ml";
            contents = fixture_source consumer_fixture;
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ consumer_module ];
    }

let optional_carrier_identity_case =
  let module_name = "Optional_carrier_identity" in
  let project =
    optional_carrier_project ~name:"optional_carrier_identity"
      ~consumer_module:module_name
      ~consumer_fixture:"optional_carrier_identity.ml"
  in
  Suite.case ~name:"optional-carrier-uses-exact-compiler-domain-identity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified
      |> Expectation.require_named_fact "function:omitted"
           (Outcome.Function_exists "omitted")
      |> Expectation.require_named_fact "function:provided"
           (Outcome.Function_exists "provided")
      |> Expectation.require_named_fact "function:forwarded"
           (Outcome.Function_exists "forwarded")
      |> Expectation.require_named_fact "function:nested_forwarded"
           (Outcome.Function_exists "nested_forwarded"))
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace project)

let optional_carrier_forgery_case =
  let module_name = "Optional_carrier_forgery" in
  let project =
    optional_carrier_project ~name:"optional_carrier_forgery"
      ~consumer_module:module_name
      ~consumer_fixture:"optional_carrier_forgery.ml"
  in
  Suite.case ~name:"lookalike-descriptor-cannot-forge-optional-carrier"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TYPE"
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace project)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_parity_case ~name:"type-changing-option-map-parity"
        ~module_name:"Type_changing_option_map"
        ~fixture:"type_changing_option_map.ml" [ "map_option" ];
      verified_parity_case ~name:"generic-structures-parity"
        ~module_name:"Structures" ~fixture:"structures.ml"
        [
          "copy_option";
          "copy_result";
          "copy_box";
          "copy_pair";
          "append";
          "tree_size";
          "tree_height";
        ];
      verified_parity_case ~name:"closed-equality-parity"
        ~module_name:"Equality" ~fixture:"equality.ml"
        [ "local_matrix"; "stdlib_matrix"; "scalar_matrix" ];
      proof_matrix_case;
      verified_parity_case ~name:"descriptor-laws-verify"
        ~module_name:"Descriptor_laws" ~fixture:"descriptor_laws.ml"
        [ "option_laws"; "constructor_laws"; "record_laws" ];
      counterexample_case ~name:"wrong-constructor-postcondition"
        ~module_name:"Wrong_constructor_result"
        ~fixture:"wrong_constructor_result.ml" ~function_name:"wrong";
      counterexample_case ~name:"swapped-record-postcondition"
        ~module_name:"Swapped_record" ~fixture:"swapped_record.ml"
        ~function_name:"swapped";
      rejection_case ~name:"reject-open-equality"
        ~module_name:"Open_equality" ~fixture:"open_equality.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-recursive-equality"
        ~module_name:"Recursive_equality" ~fixture:"recursive_equality.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-unit-payload-equality"
        ~module_name:"Unit_payload_equality" ~fixture:"unit_payload_equality.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-function-payload-equality"
        ~module_name:"Function_payload_equality"
        ~fixture:"function_payload_equality.ml"
        ~code:"VERO_UNSUPPORTED_AGGREGATE";
      rejection_case ~name:"reject-ref-payload-equality"
        ~module_name:"Ref_payload_equality" ~fixture:"ref_payload_equality.ml"
        ~code:"VERO_UNSUPPORTED_TYPE";
      rejection_case ~name:"reject-array-payload-equality"
        ~module_name:"Array_payload_equality" ~fixture:"array_payload_equality.ml"
        ~code:"VERO_UNSUPPORTED_TYPE";
      unauthenticated_descent_case;
      rejection_case ~name:"reject-changed-recursive-arguments"
        ~module_name:"Changed_recursive_arguments"
        ~fixture:"changed_recursive_arguments.ml"
        ~code:"VERO_UNSUPPORTED_GENERIC_USE";
      rejection_case ~name:"reject-generic-mutual-recursion"
        ~module_name:"Generic_mutual" ~fixture:"generic_mutual.ml"
        ~code:"VERO_UNSUPPORTED_MUTUAL_RECURSION";
      rejection_case ~name:"reject-mutable-generic"
        ~module_name:"Mutable_generic" ~fixture:"mutable_generic.ml"
        ~code:"VERO_UNSUPPORTED_MUTATION";
      alpha_case;
      verified_parity_case ~name:"abstract-logical-equality-verifies"
        ~module_name:"Abstract_logical_equality"
        ~fixture:"abstract_logical_equality.ml"
        [ "tree_reflexive"; "tree_reconstruction"; "record_reconstruction" ];
      abstract_false_case;
      nested_external_type_case;
      optional_carrier_identity_case;
      optional_carrier_forgery_case;
    ]
