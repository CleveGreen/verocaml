open Outcome_test_support

let suite_path = "test/sequence_specifications/outcome_cases.ml"
let ( let* ) = Result.bind

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let read_source ~local ~repository =
  if Sys.file_exists local then read_file local
  else if Sys.file_exists repository then read_file repository
  else failwith ("missing fixture source: " ^ repository)

let sequence_library () =
  read_source ~local:"../../library/seq.ml" ~repository:"library/seq.ml"

let client name =
  read_source ~local:(Filename.concat "fixtures" name)
    ~repository:(Filename.concat "test/sequence_specifications/fixtures" name)

let sequence_project ~name ~module_name ~client_source =
  let source = sequence_library () ^ "\n" ^ client_source in
  let source_path = String.uncapitalize_ascii module_name ^ ".ml" in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                 (libraries verocaml.ghost)\n (flags (:standard -ppx \
                 \"verocaml-ppx --keep-ghost\")))\n"
                name module_name;
          };
          { path = source_path; contents = source };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ module_name ];
    }

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let repeated_fixture input ~environment ~workspace =
  let* first =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "first") input
  in
  let* second =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "second") input
  in
  match Outcome.semantic_parity ~except:[] first second with
  | Ok () -> Ok (Outcome.merge [ first; second ])
  | Error message -> mismatch "repeated Dune fixture: %s" message

let positive_functions =
  [
    "empty_int_observations";
    "empty_bool_observations";
    "initialized_integers";
    "initialized_booleans";
    "polymorphic_singleton";
    "mathematical_push_has_no_capacity_guard";
    "mathematical_append_has_no_capacity_guard";
    "pushed_integer_chain";
    "branching_push";
    "updated_integers";
    "updated_booleans";
    "polymorphic_update";
    "direct_subrange";
    "take_drop_skip";
    "appended_integers";
    "polymorphic_append";
    "contains_singleton_exact";
    "explicit_update_extensionality";
    "explicit_slice_extensionality";
    "explicit_append_extensionality";
    "composed_pipeline";
  ]

let sequence_laws_expectation =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Seq_positive" Outcome.Unit_verified)
    positive_functions

let sequence_laws_case =
  let input =
    sequence_project ~name:"sequence_laws_fixture" ~module_name:"Seq_positive"
      ~client_source:(client "positive_client.ml")
  in
  Suite.case ~name:"sequence-laws" ~expectation:sequence_laws_expectation
    (repeated_fixture input)

let invalid_functions =
  [
    "invalid_init_length_is_not_constrained";
    "invalid_empty_get_is_not_constrained";
    "invalid_upper_get_is_not_constrained";
    "invalid_update_is_not_constrained";
    "invalid_subrange_is_not_constrained";
  ]

let invalid_domains_expectation =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_semantic ~function_name Outcome.Local_assertion
        expectation)
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Seq_negative" Outcome.Unit_counterexample)
    invalid_functions

let invalid_domains_case =
  let input =
    sequence_project ~name:"sequence_invalid_domains_fixture"
      ~module_name:"Seq_negative"
      ~client_source:(client "negative_invalid_client.ml")
  in
  Suite.case ~name:"invalid-domains-are-counterexamples"
    ~expectation:invalid_domains_expectation (repeated_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ sequence_laws_case; invalid_domains_case ]
