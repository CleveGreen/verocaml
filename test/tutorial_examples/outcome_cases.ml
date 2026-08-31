open Outcome_test_support

let suite_path = "test/tutorial_examples/outcome_cases.ml"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let read_source ~local ~repository =
  if Sys.file_exists local then read_file local
  else if Sys.file_exists repository then read_file repository
  else failwith ("missing fixture source: " ^ repository)

let tutorial_source name =
  read_source ~local:(Filename.concat "../../examples/tutorial" name)
    ~repository:(Filename.concat "examples/tutorial" name)

let failed_proof_source () =
  read_source ~local:"fixtures/failed-proof.ml"
    ~repository:"test/tutorial_examples/fixtures/failed-proof.ml"

let tutorial_modules =
  [
    ("Tutorial_01_contracts", "tutorial_01_contracts.ml", "01-contracts.ml");
    ( "Tutorial_02_recursive_specification",
      "tutorial_02_recursive_specification.ml",
      "02-recursive-specification.ml" );
    ( "Tutorial_03_scalar_induction",
      "tutorial_03_scalar_induction.ml",
      "03-scalar-induction.ml" );
    ( "Tutorial_04_list_induction",
      "tutorial_04_list_induction.ml",
      "04-list-induction.ml" );
    ( "Tutorial_05_tree_induction",
      "tutorial_05_tree_induction.ml",
      "05-tree-induction.ml" );
    ( "Tutorial_06_finite_formals",
      "tutorial_06_finite_formals.ml",
      "06-finite-formals.ml" );
    ( "Tutorial_07_owned_recursive_stack",
      "tutorial_07_owned_recursive_stack.ml",
      "07-owned-recursive-stack.ml" );
    ( "Tutorial_08_abstract_nested_mutation",
      "tutorial_08_abstract_nested_mutation.ml",
      "08-abstract-nested-mutation.ml" );
  ]

let project ~name modules =
  let module_names = modules |> List.map (fun (unit_name, _, _) -> unit_name) in
  let files =
    [
      {
        Fixture.path = "dune-project";
        contents =
          Printf.sprintf "(lang dune 3.17)\n(name %s)\n" name;
      };
      {
        path = "dune";
        contents =
          Printf.sprintf
            "(library\n (name %s)\n (wrapped false)\n (modules %s)\n \
             (libraries verocaml.ghost)\n (flags (:standard -ppx \
             \"verocaml-ppx --keep-ghost\")))\n"
            name (String.concat " " module_names);
      };
    ]
    @ List.map
        (fun (_, path, source) -> { Fixture.path; contents = source })
        modules
  in
  Fixture.dune_project
    {
      files;
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = module_names;
    }

let public_project () =
  tutorial_modules
  |> List.map (fun (unit_name, path, source_name) ->
         (unit_name, path, tutorial_source source_name))
  |> project ~name:"tutorial_examples_fixture"

let replace_once ~needle ~replacement source =
  let needle_length = String.length needle in
  let source_length = String.length source in
  let rec locate index =
    if index + needle_length > source_length then None
    else if String.sub source index needle_length = needle then Some index
    else locate (index + 1)
  in
  match locate 0 with
  | None -> failwith "tutorial mutation target is absent"
  | Some index ->
      String.sub source 0 index ^ replacement
      ^ String.sub source (index + needle_length)
          (source_length - index - needle_length)

let run input ~environment ~workspace = Fixture.run ~environment ~workspace input

let public_tutorial_expectation =
  let units = tutorial_modules |> List.map (fun (unit_name, _, _) -> unit_name) in
  let functions =
    [
      "checked_increment";
      "unfold_one_step";
      "use_theorem";
      "equal_sum";
      "nonnegative_sum";
      "demonstrate_receipts";
      "push_drop_drain";
      "singleton_then_zero";
    ]
  in
  let expectation =
    List.fold_left
      (fun expectation unit_name ->
        Expectation.require_unit unit_name Outcome.Unit_verified expectation)
      (Expectation.empty |> Expectation.status Outcome.Verified)
      units
  in
  let expectation =
    List.fold_left
      (fun expectation function_name ->
        Expectation.require_named_fact ("function:" ^ function_name)
          (Outcome.Function_exists function_name) expectation)
      expectation functions
  in
  Expectation.require_named_fact "obligation-kind:equal_sum"
    (Outcome.Obligation_kind_exists
       {
         function_name = "equal_sum";
         kind = Outcome.Recursive_call_strict_descent { callee = "equal_sum" };
       })
    expectation

let public_tutorial_case =
  Suite.case ~name:"public-tutorial-project"
    ~expectation:public_tutorial_expectation
    (fun ~environment ~workspace ->
      run (public_project ()) ~environment ~workspace)

let list_call_removed_case =
  let source =
    tutorial_source "04-list-induction.ml"
    |> replace_once
         ~needle:"          equal_sum left_tail right_tail;\n"
         ~replacement:""
  in
  let input =
    project ~name:"list_call_removed_fixture"
      [ ("List_call_removed", "list_call_removed.ml", source) ]
  in
  Suite.case ~name:"list-call-summary-required"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Inconclusive
      |> Expectation.require_unit "List_call_removed" Outcome.Unit_inconclusive
      |> Expectation.require_semantic ~function_name:"equal_sum"
           Outcome.Postcondition)
    (run input)

let documented_failed_proof_case =
  let input =
    project ~name:"documented_failed_proof_fixture"
      [
        ( "Documented_failed_proof",
          "documented_failed_proof.ml",
          failed_proof_source () );
      ]
  in
  Suite.case ~name:"documented-failed-proof"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Documented_failed_proof"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"wrong_successor"
           Outcome.Postcondition)
    (run input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ public_tutorial_case; list_call_removed_case; documented_failed_proof_case ]
