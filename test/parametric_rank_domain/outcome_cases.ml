open Outcome_test_support

let suite_path = "test/parametric_rank_domain/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/parametric_rank_domain" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let project paths selected_units =
  let modules =
    paths
    |> List.map (fun path ->
           path |> Filename.basename |> Filename.remove_extension
           |> String.capitalize_ascii)
    |> List.sort_uniq String.compare
  in
  let files =
    {
      Fixture.path = "dune-project";
      contents = "(lang dune 3.17)\n(name parametric_rank_outcomes)\n";
    }
    :: {
         path = "dune";
         contents =
           Printf.sprintf
             "(library\n (name parametric_rank_outcomes)\n (wrapped false)\n \
              (modules %s)\n (libraries verocaml.ghost)\n (flags (:standard \
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
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units;
    }

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

let case name paths units expectation =
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (project paths units))

let fixtures names = List.map (fun name -> "fixtures/" ^ name ^ ".ml") names
let units names = List.map String.capitalize_ascii names

let positive_rank_domains =
  let names =
    [ "positive_structural_decreases"; "positive_finite_formal"; "positive_recursive_child" ]
  in
  let selected_units = units names in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified selected_units
    |> require_functions [ "finite_refl"; "child"; "use_int"; "use_bool" ]
    |> Expectation.require_named_fact "obligation-kind:finite_refl"
         (Outcome.Obligation_kind_exists
            {
              function_name = "finite_refl";
              kind =
                Outcome.Recursive_call_strict_descent
                  { callee = "finite_refl" };
            })
  in
  case "positive-rank-domains" (fixtures names) selected_units expectation

let retained_project =
  let names = [ "positive_retained_provider"; "positive_retained_consumer" ] in
  let selected_units = units names in
  case "retained-provider-consumer" (fixtures names) selected_units
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified selected_units
    |> require_functions [ "finite_refl"; "use_int"; "run" ])

let negative_case name code =
  let selected_units = units [ name ] in
  case (name ^ "-rejection") (fixtures [ name ]) selected_units
    (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> Expectation.require_frontend_code code
    |> require_units Outcome.Unit_frontend_rejected selected_units)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      positive_rank_domains;
      retained_project;
      negative_case "negative_abstract_foreign" "VERO_UNSUPPORTED_STRUCTURE_ITEM";
      negative_case "negative_cyclic_gadt_private" "VERO_UNSUPPORTED_AGGREGATE";
      negative_case "negative_function_reference_array" "VERO_UNSUPPORTED_TYPE";
      negative_case "negative_mutable" "VERO_UNSUPPORTED_AGGREGATE";
      negative_case "negative_nonuniform_recursion" "VERO_UNSUPPORTED_AGGREGATE";
      negative_case "negative_stale_forged_rebound" "VERO_UNSUPPORTED_TYPE";
      negative_case "negative_unknown_payload" "VERO_UNSUPPORTED_TYPE";
    ]
