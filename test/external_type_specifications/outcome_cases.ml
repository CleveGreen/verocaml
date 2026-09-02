open Outcome_test_support

let suite_path = "test/external_type_specifications/outcome_cases.ml"

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

let fixture path =
  Filename.concat (executable_directory ()) path |> read_file

let project ~name ~modules ~files ~selected_units =
  let modules = String.concat " " modules in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
          };
          {
            Fixture.path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
                 (libraries verocaml.ghost)\n (flags (:standard -w -A -alert \
                 -all -ppx \"verocaml-ppx --keep-ghost\")))\n"
                name modules;
          };
        ]
        @ files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units;
    }

let source path contents = { Fixture.path = path; contents }

let verified_case ~name ~unit_name input functions =
  let expectation =
    List.fold_left
      (fun expectation function_name ->
        Expectation.require_named_fact ("function:" ^ function_name)
          (Outcome.Function_exists function_name) expectation)
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit unit_name Outcome.Unit_verified)
      functions
  in
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace input)

let rejection_case ~name ~unit_name ~code input =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit unit_name Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace -> Fixture.run ~environment ~workspace input)

let generic_mechanism_case =
  let input =
    project ~name:"generic_external_mechanism"
      ~modules:[ "External_types"; "Generic_mechanism" ]
      ~files:
        [
          source "external_types.ml" (fixture "fixtures/external_types.ml");
          source "generic_mechanism.ml" (fixture "fixtures/generic_mechanism.ml");
        ]
      ~selected_units:[ "Generic_mechanism" ]
  in
  verified_case ~name:"one-generic-mechanism-covers-stdlib-and-imported-types"
    ~unit_name:"Generic_mechanism" input
    [
      "construct_option";
      "construct_list";
      "construct_result";
      "construct_imported_variant";
      "construct_imported_outcome";
      "construct_imported_record";
    ]

let deeply_nested_case =
  let input =
    project ~name:"deep_external_composition"
      ~modules:[ "External_types"; "Deep_external_composition" ]
      ~files:
        [
          source "external_types.ml" (fixture "fixtures/external_types.ml");
          source "deep_external_composition.ml"
            (fixture "fixtures/deep_external_composition.ml");
        ]
      ~selected_units:[ "Deep_external_composition" ]
  in
  verified_case ~name:"deep-option-result-custom-composition"
    ~unit_name:"Deep_external_composition" input
    [ "lemma_deep_symbolic_reflexive" ]

let overlap_case =
  let external_types = "type 'a box = Empty | Box of 'a\n" in
  let first_catalog =
    {|type 'a box_specification = 'a External_types.box
[@@verocaml.external_type_specification]

let ready () = ()
|}
  in
  let second_catalog =
    {|type 'a competing_box_specification = 'a External_types.box
[@@verocaml.external_type_specification]

let ready () = ()
|}
  in
  let consumer =
    {|let catalogs_ready () = Catalog_box.ready (), Catalog_box_duplicate.ready ()

let identity (value : int External_types.box) = value
|}
  in
  let input =
    project ~name:"external_catalog_overlap"
      ~modules:
        [
          "External_types";
          "Catalog_box";
          "Catalog_box_duplicate";
          "Catalog_overlap_consumer";
        ]
      ~files:
        [
          source "external_types.ml" external_types;
          source "catalog_box.ml" first_catalog;
          source "catalog_box_duplicate.ml" second_catalog;
          source "catalog_overlap_consumer.ml" consumer;
        ]
      ~selected_units:
        [
          "External_types";
          "Catalog_box";
          "Catalog_box_duplicate";
          "Catalog_overlap_consumer";
        ]
  in
  rejection_case ~name:"overlapping-imported-catalogs-are-rejected"
    ~unit_name:"Catalog_overlap_consumer" ~code:"VERO_INVALID_PROGRAM" input

let forged_marker_case =
  let input =
    Fixture.dune_project
      {
        files =
          [
            {
              Fixture.path = "dune-project";
              contents = "(lang dune 3.17)\n(name forged_external_marker)\n";
            };
            {
              Fixture.path = "dune";
              contents =
                "(library\n (name forged_external_marker)\n (wrapped false)\n \
                 (modules Forged_marker))\n";
            };
            source "forged_marker.ml" (fixture "fixtures/forged_marker.ml");
          ];
        libraries = [];
        targets = [ "@all" ];
        selected_units = [ "Forged_marker" ];
      }
  in
  rejection_case ~name:"source-forged-marker-has-no-authority"
    ~unit_name:"Forged_marker" ~code:"VERO_MALFORMED_GHOST_CALL" input

let unregistered_name_case =
  let input =
    Fixture.single_source ~module_name:"Without_specification"
      ~source:(fixture "fixtures/without_specification.ml")
      ~libraries:[ "verocaml.ghost" ]
  in
  rejection_case ~name:"type-name-without-registration-is-not-special"
    ~unit_name:"Without_specification" ~code:"VERO_UNSUPPORTED_TYPE" input

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      generic_mechanism_case;
      deeply_nested_case;
      overlap_case;
      forged_marker_case;
      unregistered_name_case;
    ]
