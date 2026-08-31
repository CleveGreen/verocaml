open Outcome_test_support

let suite_path = "test/finite_result_promotion/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/finite_result_promotion" path
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
      contents = "(lang dune 3.17)\n(name finite_result_outcomes)\n";
    }
    :: {
         path = "dune";
         contents =
           Printf.sprintf
             "(library\n (name finite_result_outcomes)\n (wrapped false)\n \
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

let paths names = List.map (fun name -> "fixtures/" ^ name ^ ".ml") names
let units names = List.map String.capitalize_ascii names

let promotion_successes =
  let names =
    [
      "push_front_inductive_eq";
      "push_front_explicit_let";
      "push_front_summary_demand";
      "caller_transfer";
      "alias_and_two_calls";
      "two_finite_exits";
      "branch_all_paths";
      "direct_nested_construction_cycle";
      "direct_record_nested_cycle";
      "direct_structural_self_composition";
      "direct_self_branch_partial";
      "direct_self_double_use";
      "direct_self_dropped_result";
      "direct_self_projection_only";
      "result_copy";
      "result_rebind";
    ]
  in
  let selected_units = units names in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> require_units Outcome.Unit_verified selected_units
    |> require_functions [ "build"; "consume"; "node_eq_trans"; "push_front" ]
    |> Expectation.require_named_fact "obligation-kind:build"
         (Outcome.Obligation_kind_exists
            {
              function_name = "build";
              kind = Outcome.Recursive_call_strict_descent { callee = "build" };
            })
  in
  case "promotion-successes" (paths names) selected_units expectation

let failed_obligation =
  let names = [ "failed_obligation_no_publication" ] in
  case "failed-obligation-inconclusive" (paths names) (units names)
    (Expectation.empty |> Expectation.status Outcome.Inconclusive
    |> Expectation.require_semantic ~function_name:"make_n_nodes"
         Outcome.Postcondition
    |> Expectation.require_unit "Failed_obligation_no_publication"
         Outcome.Unit_inconclusive
    |> require_functions [ "make_n_nodes"; "consume" ])

let nondecreasing_actual =
  let names = [ "direct_self_nondecreasing_actual" ] in
  case "nondecreasing-actual-counterexample" (paths names) (units names)
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_semantic ~function_name:"build"
         (Outcome.Recursive_call_strict_descent { callee = "build" })
    |> Expectation.require_unit "Direct_self_nondecreasing_actual"
         Outcome.Unit_counterexample
    |> require_functions [ "build"; "consume" ])

let frontend_rejections =
  let names =
    [
      "external_result_route";
      "partial_result_route";
      "higher_order_result_route";
      "direct_result_cycle";
      "indirect_result_cycle";
      "mutual_nested_construction_barrier";
    ]
  in
  let selected_units = units names in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Frontend_rejected
    |> require_units Outcome.Unit_frontend_rejected selected_units
    |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_TOP_LEVEL_BINDING"
    |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTUAL_RECURSION"
  in
  case "frontend-rejections" (paths names) selected_units expectation

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      promotion_successes;
      failed_obligation;
      nondecreasing_actual;
      frontend_rejections;
    ]
