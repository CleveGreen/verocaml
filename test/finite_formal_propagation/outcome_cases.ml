open Outcome_test_support

let suite_path = "test/finite_formal_propagation/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/finite_formal_propagation" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name path =
  path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let project paths selected_units =
  let modules = paths |> List.map module_name |> List.sort_uniq String.compare in
  let files =
    {
      Fixture.path = "dune-project";
      contents = "(lang dune 3.17)\n(name finite_formal_outcomes)\n";
    }
    :: {
         path = "dune";
         contents =
           Printf.sprintf
             "(library\n (name finite_formal_outcomes)\n (wrapped false)\n \
              (modules %s)\n (libraries verocaml.ghost verocaml.vstd)\n (flags (:standard \
              -ppx \"verocaml-ppx --keep-ghost\")))\n"
             (String.concat " " modules);
       }
       :: List.map
            (fun path ->
              { Fixture.path = Filename.basename path; contents = read_file path })
            paths
  in
  Fixture.dune_project
    {
      files;
      libraries = [ "verocaml.ghost"; "verocaml.vstd" ];
      targets = [ "@all" ];
      selected_units;
    }

let run paths units ~environment ~workspace =
  Fixture.run ~environment ~workspace (project paths units)

let require_units disposition units expectation =
  List.fold_left
    (fun expectation unit_name ->
      Expectation.require_unit unit_name disposition expectation)
    expectation units

let require_functions functions expectation =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    expectation functions

let verified units functions =
  Expectation.empty |> Expectation.status Outcome.Verified
  |> require_units Outcome.Unit_verified units
  |> require_functions functions

let rejected units codes =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> require_units Outcome.Unit_frontend_rejected units
  in
  List.fold_left
    (fun expectation code ->
      Expectation.require_frontend_code code expectation)
    expectation codes

let case name paths units expectation =
  Suite.case ~name ~expectation (run paths units)

let accepted_flows =
  let names =
    [
      "positive";
      "driver_dispatch_consumer";
      "same_cmt_modes";
      "generic_recursive_proof";
      "push_preservation";
      "generic_unused";
      "generic_open_proof";
      "generic_exec";
      "generic_result";
      "generic_partial";
      "generic_labelled";
      "same_cmt_signature_positive";
    ]
  in
  let units = List.map String.capitalize_ascii names in
  case "accepted-finite-formal-flows"
    (List.map (fun name -> "fixtures/" ^ name ^ ".ml") names)
    units
    (verified units [ "caller"; "lemma_equal_refl"; "lemma_push_front_wf"; "run" ])

let generic_policy_rejections =
  let names =
    [ "generic_type_changing"; "generic_higher_order"; "generic_foreign_actual" ]
  in
  let units = List.map String.capitalize_ascii names in
  case "generic-policy-rejections"
    (List.map (fun name -> "fixtures/" ^ name ^ ".ml") names)
    units
    (rejected units
       [
         "VERO_UNSUPPORTED_GENERIC_USE";
         "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION";
         "VERO_UNSUPPORTED_TYPE";
       ])

let public_interface_success =
  case "public-interface-success"
    [
      "fixtures/generic_explicit_interface.mli";
      "fixtures/generic_explicit_interface.ml";
    ]
    [ "Generic_explicit_interface" ]
    (verified [ "Generic_explicit_interface" ]
       [ "seq_reflexive"; "int_seq_reflexive"; "bool_seq_reflexive" ])

let public_interface_rejections =
  case "public-interface-rejections"
    [
      "fixtures/generic_explicit_alias.mli";
      "fixtures/generic_explicit_alias.ml";
      "fixtures/generic_explicit_nested.mli";
      "fixtures/generic_explicit_nested.ml";
    ]
    [ "Generic_explicit_alias"; "Generic_explicit_nested" ]
    (rejected
       [ "Generic_explicit_alias"; "Generic_explicit_nested" ]
       [ "VERO_UNSUPPORTED_TYPE"; "VERO_UNSUPPORTED_GENERIC_USE" ])

let same_cmt_signature_rejections =
  case "same-cmt-signature-rejections"
    [
      "fixtures/same_cmt_signature_add.ml";
      "fixtures/same_cmt_signature_remove.ml";
    ]
    [ "Same_cmt_signature_add"; "Same_cmt_signature_remove" ]
    (rejected
       [ "Same_cmt_signature_add"; "Same_cmt_signature_remove" ]
       [ "VERO_UNSUPPORTED_STRUCTURE_ITEM" ])

let retained_provider_consumer =
  case "retained-provider-consumer"
    [
      "fixtures/retained_simple_bindings_provider.mli";
      "fixtures/retained_simple_bindings_provider.ml";
      "fixtures/retained_simple_bindings_consumer.ml";
    ]
    [ "Retained_simple_bindings_provider"; "Retained_simple_bindings_consumer" ]
    (verified
       [ "Retained_simple_bindings_provider"; "Retained_simple_bindings_consumer" ]
       [ "seq_reflexive"; "checked"; "run" ])

let transferred_precondition_failure =
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_semantic ~function_name:"caller"
         (Outcome.Call_precondition { callee = "checked" })
    |> Expectation.require_unit "False_precondition_after_transfer"
         Outcome.Unit_counterexample
    |> require_functions [ "caller"; "checked" ]
  in
  case "transferred-precondition-failure"
    [ "fixtures/false_precondition_after_transfer.ml" ]
    [ "False_precondition_after_transfer" ] expectation

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      accepted_flows;
      generic_policy_rejections;
      public_interface_success;
      public_interface_rejections;
      same_cmt_signature_rejections;
      retained_provider_consumer;
      transferred_precondition_failure;
    ]
