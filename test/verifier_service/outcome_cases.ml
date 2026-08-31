open Outcome_test_support

let suite_path = "test/verifier_service/outcome_cases.ml"
let ( let* ) = Result.bind

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let test_source relative =
  let adjacent = Filename.concat (executable_directory ()) relative in
  let source_relative = Filename.concat "test/verifier_service" relative in
  read_file (if Sys.file_exists adjacent then adjacent else source_relative)

let single_source module_name relative =
  Fixture.single_source ~module_name ~source:(test_source relative)
    ~libraries:[ "verocaml.ghost" ]

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let semantic_case ~name ~module_name ~relative expectation =
  Suite.case ~name ~expectation
    (run_fixture (single_source module_name relative))

let verified_case =
  semantic_case ~name:"service-projects-verified-result"
    ~module_name:"Local_positive"
    ~relative:"../private_receipt/fixtures/local_positive.ml"
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Local_positive" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:run"
         (Outcome.Function_exists "run"))

let diagnostic_case =
  semantic_case ~name:"service-projects-counterexample-kind"
    ~module_name:"False_postcondition"
    ~relative:"../release_verification/fixtures/false_postcondition.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "False_postcondition"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"false_postcondition"
         Outcome.Postcondition)

let trusted_case =
  semantic_case ~name:"trusted-body-consumer-verifies" ~module_name:"Positive"
    ~relative:"../trusted_external_bodies/fixtures/positive.ml"
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Positive" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:caller"
         (Outcome.Function_exists "caller")
    |> Expectation.require_named_fact "obligation-kind:caller"
         (Outcome.Obligation_kind_exists
            {
              function_name = "caller";
              kind = Outcome.Call_precondition { callee = "keep_only_first" };
            }))

let model_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name verifier_service_model)\n";
          };
          {
            path = "dune";
            contents =
              "(library\n (name verifier_service_model)\n (wrapped false)\n \
               (modules Model_dependency Model_consumer)\n (libraries \
               verocaml.ghost)\n (flags (:standard -ppx \"verocaml-ppx \
               --keep-ghost\")))\n";
          };
          {
            path = "model_dependency.ml";
            contents =
              test_source "../verified_interfaces/fixtures/model_dependency.ml";
          };
          {
            path = "model_consumer.ml";
            contents =
              test_source "../verified_interfaces/fixtures/model_consumer.ml";
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Model_dependency"; "Model_consumer" ];
    }

let dependency_case =
  Suite.case ~name:"service-projects-dependency-units"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Model_dependency" Outcome.Unit_verified
      |> Expectation.require_unit "Model_consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:observe"
           (Outcome.Function_exists "observe"))
    (run_fixture model_project)

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt root basename =
  match
    files_below root
    |> List.filter (fun path -> String.equal (Filename.basename path) basename)
  with
  | [ path ] -> Ok path
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("missing prepared service artifact " ^ basename))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("ambiguous prepared service artifact " ^ basename))

let load_implementation path =
  match Cmt_input.load path with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message))

let configuration ~threads ~rlimit =
  match
    Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit
  with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let verify configuration consumer =
  Verifier_service.request ~configuration ~consumer ~dependencies:[]
  |> Verifier_service.verify
  |> Result.map_error (fun error ->
         Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let with_mode mode outcome =
  Outcome.merge
    [
      outcome;
      Outcome.observation ~status:Outcome.Verified
        ~named_facts:[ ("mode", Outcome.Function_exists mode) ] ()
      |> Outcome.project;
    ]
  |> Outcome.with_unit "Local_positive" Outcome.Unit_verified

let parity_case_run ~environment ~workspace =
  let compiled = Filename.concat workspace "compiled" in
  let input =
    single_source "Local_positive"
      "../private_receipt/fixtures/local_positive.ml"
  in
  let* _ = Fixture.run ~environment ~workspace:compiled input in
  let* cmt =
    discover_cmt (Filename.concat compiled "project/_build") "local_positive.cmt"
  in
  let* consumer = load_implementation cmt in
  let* serial_configuration = configuration ~threads:1 ~rlimit:None in
  let* explicit_configuration =
    configuration ~threads:1 ~rlimit:(Some 3_000_000)
  in
  let* higher_configuration = configuration ~threads:2 ~rlimit:None in
  let* serial = verify serial_configuration consumer in
  let* explicit = verify explicit_configuration consumer in
  let* higher = verify higher_configuration consumer in
  let serial = with_mode "serial-default" (Outcome.of_verifier_result serial) in
  let explicit =
    with_mode "serial-explicit-rlimit" (Outcome.of_verifier_result explicit)
  in
  let higher =
    with_mode "higher-thread-count" (Outcome.of_verifier_result higher)
  in
  let* () =
    Outcome.semantic_parity ~except:[ "mode" ] serial explicit
    |> Result.map_error (fun message ->
           Failure.make Failure.Expectation_mismatch message)
  in
  let* () =
    Outcome.semantic_parity ~except:[ "mode" ] serial higher
    |> Result.map_error (fun message ->
           Failure.make Failure.Expectation_mismatch message)
  in
  Ok serial

let parity_case =
  Suite.case ~name:"configuration-semantic-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Local_positive" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:run"
           (Outcome.Function_exists "run")
      |> Expectation.require_named_fact "mode"
           (Outcome.Function_exists "serial-default"))
    parity_case_run

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ verified_case; diagnostic_case; trusted_case; dependency_case; parity_case ]
