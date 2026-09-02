open Outcome_test_support

let suite_path = "test/finite_induction/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/finite_induction" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let module_name name = String.capitalize_ascii name

let project names =
  let units = List.map module_name names in
  let files =
    {
      Fixture.path = "dune-project";
      contents = "(lang dune 3.17)\n(name finite_induction_outcomes)\n";
    }
    :: {
         path = "dune";
         contents =
           Printf.sprintf
             "(library\n (name finite_induction_outcomes)\n (wrapped false)\n \
              (modules %s)\n (libraries verocaml.ghost verocaml.vstd)\n (flags (:standard \
              -ppx \"verocaml-ppx --keep-ghost\")))\n"
             (String.concat " " units);
       }
       :: List.map
            (fun name ->
              let path = "fixtures/" ^ name ^ ".ml" in
              { Fixture.path = name ^ ".ml"; contents = read_file path })
            names
  in
  ( Fixture.dune_project
      {
        files;
        libraries = [ "verocaml.ghost"; "verocaml.vstd" ];
        targets = [ "@all" ];
        selected_units = units;
      },
    units )

let case name names expectation =
  let input, _units = project names in
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace input)

let accepted_induction =
  let units =
    [ "Finite_induction_positive"; "Ordinary_direct_return"; "Ordinary_push_front" ]
  in
  let expectation =
    List.fold_left
      (fun expectation unit_name ->
        Expectation.require_unit unit_name Outcome.Unit_verified expectation)
      (Expectation.empty |> Expectation.status Outcome.Verified) units
    |> Expectation.require_named_fact "function:node_eq_refl"
         (Outcome.Function_exists "node_eq_refl")
    |> Expectation.require_named_fact "function:make_n_nodes"
         (Outcome.Function_exists "make_n_nodes")
    |> Expectation.require_named_fact "function:push_front"
         (Outcome.Function_exists "push_front")
    |> Expectation.require_named_fact "obligation-kind:make_n_nodes"
         (Outcome.Obligation_kind_exists
            {
              function_name = "make_n_nodes";
              kind =
                Outcome.Recursive_call_strict_descent
                  { callee = "make_n_nodes" };
            })
  in
  case "accepted-finite-induction"
    [ "finite_induction_positive"; "ordinary_direct_return"; "ordinary_push_front" ]
    expectation

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ accepted_induction ]
