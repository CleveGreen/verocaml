open Outcome_test_support

let suite_path = "test/logical_adt_schema/outcome_cases.ml"

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
        (Filename.concat "test/logical_adt_schema/fixtures" name);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> read_file path
  | None -> failwith ("fixture source is unavailable: " ^ name)

let input module_name fixture =
  Fixture.single_source ~module_name ~source:(fixture_source fixture)
    ~libraries:[ "verocaml.ghost" ]

let require_function function_name expectation =
  Expectation.require_named_fact ("function:" ^ function_name)
    (Outcome.Function_exists function_name) expectation

let require_obligation function_name kind expectation =
  Expectation.require_named_fact ("obligation-kind:" ^ function_name)
    (Outcome.Obligation_kind_exists { function_name; kind }) expectation

let verified_case ~name ~module_name ~fixture ~function_name =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified
      |> require_function function_name
      |> require_obligation function_name Outcome.Assertion)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input module_name fixture))

let false_identity_case =
  let module_name = "False_identity" in
  Suite.case ~name:"false-identity-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"false_identity"
           Outcome.Assertion
      |> Expectation.require_unit module_name Outcome.Unit_counterexample
      |> require_function "false_identity"
      |> require_obligation "false_identity" Outcome.Assertion)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (input module_name "false_identity.ml"))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_case ~name:"abstract-payload-reflexivity"
        ~module_name:"Abstract_payload" ~fixture:"abstract_payload.ml"
        ~function_name:"reflexive";
      verified_case ~name:"nullary-constructor-discrimination"
        ~module_name:"Nullary_discrimination"
        ~fixture:"nullary_discrimination.ml" ~function_name:"prove";
      verified_case ~name:"recursive-list-reconstruction"
        ~module_name:"Recursive_list" ~fixture:"recursive_list.ml"
        ~function_name:"reconstruction";
      false_identity_case;
    ]
