open Outcome_test_support

let suite_path = "test/retained_exec_results/outcome_cases.ml"
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
  else read_file (Filename.concat "test/retained_exec_results/fixtures" name)

let keep_lines predicate source =
  source |> String.split_on_char '\n' |> List.filter predicate
  |> String.concat "\n"

let without_generic_proof_and_spec source =
  keep_lines
    (fun line ->
      not
        (String.starts_with ~prefix:"let proof_box " line
        || String.starts_with ~prefix:"let spec_box " line
        || String.starts_with ~prefix:"val proof_box " line
        || String.starts_with ~prefix:"val spec_box " line))
    source

let selected_lines prefixes source =
  keep_lines
    (fun line ->
      List.exists (fun prefix -> String.starts_with ~prefix line) prefixes)
    source

let file path contents = { Fixture.path; contents }

let project_input ~name ~modules ~files ~selected_units =
  Fixture.dune_project
    {
      files =
        [
          file "dune-project"
            (Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name);
          file "dune"
            (Printf.sprintf
               "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                (libraries verocaml.ghost)\n (flags (:standard -ppx \
                \"verocaml-ppx --keep-ghost\")))\n"
               name (String.concat " " modules));
        ]
        @ files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units;
    }

let provider_files () =
  [
    file "provider.mli"
      (fixture_source "provider.mli" |> without_generic_proof_and_spec);
    file "provider.ml"
      (fixture_source "provider.ml" |> without_generic_proof_and_spec);
  ]

let positive_project selected_units extra_files =
  project_input ~name:"retained_exec_positive"
    ~modules:("Provider" :: selected_units)
    ~files:(provider_files () @ extra_files)
    ~selected_units:("Provider" :: selected_units)

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

let load_implementation cmt =
  let cmi = Filename.remove_extension cmt ^ ".cmi" in
  match Cmt_input.load_with_interface ~cmt ~cmi () with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      Error
        (Failure.make Failure.Selected_cmt_load
           (Printf.sprintf "%s: %s" diagnostic.Diagnostic.code diagnostic.message))

let disposition outcome =
  match Outcome.status outcome with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let configuration threads =
  match Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))

let verify ~configuration ~unit_name ~dependencies implementation =
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer:implementation
         ~dependencies)
  with
  | Ok result ->
      let outcome = Outcome.of_verifier_result result in
      Ok (Outcome.with_unit unit_name (disposition outcome) outcome)
  | Error error ->
      Error
        (Failure.make Failure.Verifier_outcome
           (Verifier_service.error_message error))

let with_modes modes outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (List.map
         (fun (key, value) -> (key, Outcome.Function_exists value))
         modes
      @ Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let positive_parity ~environment ~workspace =
  let input =
    positive_project [ "Consumer" ]
      [ file "consumer.ml" (fixture_source "consumer.ml") ]
  in
  let source_workspace = Filename.concat workspace "source" in
  let* source = Fixture.run ~environment ~workspace:source_workspace input in
  let project_root = Filename.concat (absolute source_workspace) "project" in
  let* provider_cmt = discover_cmt project_root "Provider" in
  let* consumer_cmt = discover_cmt project_root "Consumer" in
  let* provider = load_implementation provider_cmt in
  let* consumer = load_implementation consumer_cmt in
  let* configuration = configuration 2 in
  let* provider_outcome =
    verify ~configuration ~unit_name:"Provider" ~dependencies:[] provider
  in
  let* consumer_outcome =
    verify ~configuration ~unit_name:"Consumer" ~dependencies:[ provider ]
      consumer
  in
  let source =
    with_modes
      [ ("input-mode", "dune-project"); ("execution-mode", "threads-1") ]
      source
  in
  let prepared =
    Outcome.merge [ provider_outcome; consumer_outcome ]
    |> with_modes
         [ ("input-mode", "prepared-cmt"); ("execution-mode", "threads-2") ]
  in
  let* () =
    match
      Outcome.semantic_parity ~except:[ "input-mode"; "execution-mode" ]
        source prepared
    with
    | Ok () -> Ok ()
    | Error message -> mismatch "%s" message
  in
  Ok (Outcome.merge [ source; prepared ])

let positive_case =
  Suite.case ~name:"aggregate-result-project-parity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:direct"
           (Outcome.Function_exists "direct")
      |> Expectation.require_named_fact "function:postcondition"
           (Outcome.Function_exists "postcondition")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "dune-project")
      |> Expectation.require_named_fact "input-mode"
           (Outcome.Function_exists "prepared-cmt")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-1")
      |> Expectation.require_named_fact "execution-mode"
           (Outcome.Function_exists "threads-2"))
    positive_parity

let counterexample_matrix =
  let run ~environment ~workspace =
    Fixture.run ~environment ~workspace
      (positive_project
         [ "Uncontracted_field_failure"; "Two_calls_failure" ]
         [
           file "uncontracted_field_failure.ml"
             (fixture_source "uncontracted_field_failure.ml");
           file "two_calls_failure.ml"
             (fixture_source "two_calls_failure.ml");
         ])
  in
  Suite.case ~name:"uncontracted-result-counterexamples"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Uncontracted_field_failure"
           Outcome.Unit_counterexample
      |> Expectation.require_unit "Two_calls_failure"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"body_fact"
           Outcome.Postcondition
      |> Expectation.require_semantic ~function_name:"two_calls"
           Outcome.Postcondition)
    run

let run_frontend_boundary ~unit_name input ~environment ~workspace =
  match Fixture.run ~environment ~workspace input with
  | Ok outcome -> Ok outcome
  | Error failure when Failure.category failure = Failure.Verifier_outcome ->
      Outcome.observation ~status:Outcome.Frontend_rejected
        ~units:[ (unit_name, Outcome.Unit_frontend_rejected) ] ()
      |> Outcome.project |> Result.ok
  | Error failure -> Error failure

let rejection_case ~name ~unit_name input =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_unit unit_name Outcome.Unit_frontend_rejected)
    (run_frontend_boundary ~unit_name input)

let provider_rejection_project ~name ~module_name ~path ~contents =
  project_input ~name ~modules:[ "Provider"; module_name ]
    ~files:(provider_files () @ [ file path contents ])
    ~selected_units:[ "Provider"; module_name ]

let provider_consumer_rejection ~name ~module_name ~fixture =
  let project_name =
    "reject_" ^ String.map (function '-' -> '_' | character -> character) name
  in
  rejection_case ~name ~unit_name:module_name
    (provider_rejection_project ~name:project_name ~module_name
       ~path:fixture ~contents:(fixture_source fixture))

let monomorphic_consumer fixture function_name =
  selected_lines [ "[@@@verocaml.verify]"; "let " ^ function_name ^ " " ]
    (fixture_source fixture)

let generic_rejection ~role ~consumer_fixture =
  let provider_source = fixture_source "provider.ml" in
  let provider_interface = fixture_source "provider.mli" in
  let function_name = role ^ "_box" in
  let provider_name = "Generic_" ^ role ^ "_provider" in
  let consumer_name = "Generic_" ^ role ^ "_result_consumer" in
  let input =
    project_input ~name:("reject_generic_" ^ role)
      ~modules:[ provider_name; consumer_name ]
      ~files:
        [
          file (String.uncapitalize_ascii provider_name ^ ".mli")
            (selected_lines
               [ "type 'a box "; "val " ^ function_name ^ " " ]
               provider_interface);
          file (String.uncapitalize_ascii provider_name ^ ".ml")
            (selected_lines
               [
                 "[@@@verocaml.verify]";
                 "type 'a box ";
                 "let " ^ function_name ^ " ";
               ]
               provider_source);
          file (String.uncapitalize_ascii consumer_name ^ ".ml")
            (monomorphic_consumer consumer_fixture "use_generic");
        ]
      ~selected_units:[ provider_name; consumer_name ]
  in
  rejection_case ~name:("reject-generic-" ^ role ^ "-result")
    ~unit_name:consumer_name input

let mutable_rejection ~shared =
  let source = fixture_source "mutable_provider.ml" in
  let interface = fixture_source "mutable_provider.mli" in
  let provider_name, consumer_name, type_prefix, value_prefix, interface_prefix,
      function_name =
    if shared then
      ( "Shared_provider",
        "Shared_result_consumer",
        "type shared_result",
        "let make_shared ",
        "val make_shared ",
        "use_shared" )
    else
      ( "Mutable_provider",
        "Mutable_result_consumer",
        "type result",
        "let make ",
        "val make ",
        "use_mutable" )
  in
  let input =
    project_input ~name:("reject_" ^ String.uncapitalize_ascii consumer_name)
      ~modules:[ provider_name; consumer_name ]
      ~files:
        [
          file (String.uncapitalize_ascii provider_name ^ ".mli")
            (selected_lines [ type_prefix; interface_prefix ] interface);
          file (String.uncapitalize_ascii provider_name ^ ".ml")
            (selected_lines
               [ "[@@@verocaml.verify]"; type_prefix; value_prefix ] source);
          file (String.uncapitalize_ascii consumer_name ^ ".ml")
            (monomorphic_consumer "mutable_result_consumer.ml" function_name);
        ]
      ~selected_units:[ provider_name; consumer_name ]
  in
  rejection_case
    ~name:(if shared then "reject-shared-result" else "reject-mutable-result")
    ~unit_name:consumer_name input

let hidden_rejection =
  rejection_case ~name:"reject-hidden-result"
    ~unit_name:"Hidden_result_consumer"
    (project_input ~name:"reject_hidden_result"
       ~modules:[ "Hidden_provider"; "Hidden_result_consumer" ]
       ~files:
         [
           file "hidden_provider.mli" (fixture_source "hidden_provider.mli");
           file "hidden_provider.ml" (fixture_source "hidden_provider.ml");
           file "hidden_result_consumer.ml"
             (fixture_source "hidden_result_consumer.ml");
         ]
       ~selected_units:[ "Hidden_provider"; "Hidden_result_consumer" ])

let foreign_rejection =
  let input =
    project_input ~name:"reject_foreign_result"
      ~modules:[ "Hidden_provider"; "Foreign_provider"; "Foreign_result_consumer" ]
      ~files:
        [
          file "hidden_provider.mli" (fixture_source "hidden_provider.mli");
          file "hidden_provider.ml" (fixture_source "hidden_provider.ml");
          file "foreign_provider.mli"
            (selected_lines [ "val pass_foreign " ]
               (fixture_source "mutable_provider.mli"));
          file "foreign_provider.ml"
            (selected_lines [ "[@@@verocaml.verify]"; "let pass_foreign " ]
               (fixture_source "mutable_provider.ml"));
          file "foreign_result_consumer.ml"
            (monomorphic_consumer "mutable_result_consumer.ml" "use_foreign");
        ]
      ~selected_units:
        [ "Hidden_provider"; "Foreign_provider"; "Foreign_result_consumer" ]
  in
  rejection_case ~name:"reject-foreign-result"
    ~unit_name:"Foreign_result_consumer" input

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_case;
      counterexample_matrix;
      provider_consumer_rejection ~name:"reject-finite-result"
        ~module_name:"Finite_result_consumer" ~fixture:"finite_result_consumer.ml";
      provider_consumer_rejection ~name:"reject-partial-result"
        ~module_name:"Partial_consumer" ~fixture:"partial_consumer.ml";
      provider_consumer_rejection ~name:"reject-callback-result"
        ~module_name:"Callback_consumer" ~fixture:"callback_consumer.ml";
      rejection_case ~name:"reject-proof-result"
        ~unit_name:"Proof_result_consumer"
        (provider_rejection_project ~name:"reject_proof_result"
           ~module_name:"Proof_result_consumer" ~path:"proof_result_consumer.ml"
           ~contents:
             (monomorphic_consumer "proof_result_consumer.ml" "use_monomorphic"));
      rejection_case ~name:"reject-spec-result"
        ~unit_name:"Spec_result_consumer"
        (provider_rejection_project ~name:"reject_spec_result"
           ~module_name:"Spec_result_consumer" ~path:"spec_result_consumer.ml"
           ~contents:
             (monomorphic_consumer "spec_result_consumer.ml" "use_monomorphic"));
      generic_rejection ~role:"proof"
        ~consumer_fixture:"proof_result_consumer.ml";
      generic_rejection ~role:"spec"
        ~consumer_fixture:"spec_result_consumer.ml";
      mutable_rejection ~shared:false;
      mutable_rejection ~shared:true;
      hidden_rejection;
      foreign_rejection;
    ]
