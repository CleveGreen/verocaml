open Outcome_test_support

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

let fixture name =
  Filename.concat (executable_directory ()) (Filename.concat "fixtures" name)
  |> read_file

let rejected ~name ~module_name ~fixture_name ~code =
  let input =
    Fixture.single_source ~module_name ~source:(fixture fixture_name)
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace -> Fixture.run ~environment ~workspace input)

let contains source fragment =
  let source_length = String.length source
  and fragment_length = String.length fragment in
  let rec search offset =
    offset + fragment_length <= source_length
    &&
    (String.equal (String.sub source offset fragment_length) fragment
    || search (offset + 1))
  in
  fragment_length = 0 || search 0

let diagnostic_classification_case =
  Suite.case ~name:"broadcast-diagnostics-separate-source-artifact-and-internal"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let span = Diagnostic.file_span "provider.ml" in
      let artifact_failures =
        [
          Diagnostic.Missing_provider_artifact;
          Diagnostic.Malformed_provider_artifact;
          Diagnostic.Stale_provider_artifact;
          Diagnostic.Mismatched_provider_artifact;
          Diagnostic.Conflicting_provider_artifact;
        ]
      in
      let artifacts =
        List.map
          (fun failure ->
            Diagnostic.make
              (Diagnostic.Invalid_broadcast_dependency
                 { provider = "Example_provider"; failure })
              span)
          artifact_failures
      in
      let source =
        Diagnostic.make
          (Diagnostic.Invalid_broadcast "private compiler identity detail")
          span
      in
      let forbidden =
        [
          "private compiler identity detail";
          "compiler_uid";
          "receipt";
          "canonical";
          "carrier";
          "encoding";
        ]
      in
      let safe diagnostic =
        List.for_all (fun fragment -> not (contains diagnostic.Diagnostic.message fragment))
          forbidden
        && List.exists
             (function
               | Diagnostic.Hint message ->
                   contains message "Rebuild the provider and consumer"
               | Diagnostic.Note _ -> false)
             diagnostic.submessages
      in
      let internal =
        match
          Interface_specification_environment_private.internal_error
            "private impossible invariant detail"
        with
        | Ok _ -> false
        | Error error ->
            Interface_specification_environment_private.error_is_internal error
            && not
                 (contains
                    (Interface_specification_environment_private.error_to_string
                       error)
                    "private impossible invariant detail")
      in
      if
        List.for_all
          (fun diagnostic ->
            diagnostic.Diagnostic.code = "VERO_DEPENDENCY"
            && Diagnostic.failure_class diagnostic.classification
               = Diagnostic.Artifact_failure
            && safe diagnostic)
          artifacts
        && source.code = "VERO_INVALID_BROADCAST"
        && Diagnostic.failure_class source.classification
           = Diagnostic.Source_failure
        && List.for_all
             (fun fragment -> not (contains source.message fragment))
             forbidden
        && internal
      then Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      else
        Error
          (Failure.make Failure.Expectation_mismatch
             "broadcast diagnostic failure classes or redaction contract diverged"))

let () =
  Suite.run_cli ~suite_path:"test/user_diagnostics/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      diagnostic_classification_case;
      rejected ~name:"proof-role-is-required"
        ~module_name:"Missing_proof_annotation"
        ~fixture_name:"missing_proof_annotation.ml" ~code:"VERO_ERASED_CALL";
      rejected ~name:"logical-role-is-required"
        ~module_name:"Missing_spec_annotation"
        ~fixture_name:"missing_spec_annotation.ml" ~code:"VERO_EXEC_IN_SPEC";
      rejected ~name:"unsupported-source-type"
        ~module_name:"Unsupported_type" ~fixture_name:"unsupported_type.ml"
        ~code:"VERO_UNSUPPORTED_TYPE";
      rejected ~name:"recursive-rank-is-required"
        ~module_name:"Missing_decreases" ~fixture_name:"missing_decreases.ml"
        ~code:"VERO_INVALID_RECURSIVE_RANK";
      rejected ~name:"refutable-parameter-is-rejected"
        ~module_name:"Refutable_parameter" ~fixture_name:"refutable_parameter.ml"
        ~code:"VERO_REFUTABLE_PARAMETER";
      rejected ~name:"forall-trigger-is-required"
        ~module_name:"Missing_forall_trigger"
        ~fixture_name:"missing_forall_trigger.ml" ~code:"VERO_QUANTIFIER_TRIGGER";
      rejected ~name:"callback-contract-is-required"
        ~module_name:"Callback_missing_contract"
        ~fixture_name:"callback_missing_contract.ml" ~code:"VERO_CALLBACK_CONTRACT";
    ]
