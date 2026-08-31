open Outcome_test_support

let suite_path = "test/outcome_framework/framework_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let success_case =
  let input =
    Fixture.single_source ~module_name:"Successful"
      ~source:"[@@@verocaml.verify]\n\nlet identity (value : int) = value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"verification-success"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Successful" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity"))
    (run_fixture input)

let frontend_rejection_case =
  let input =
    Fixture.single_source ~module_name:"Frontend_rejection"
      ~source:
        "[@@@verocaml.verify]\n\nlet reject (value : int) =\n  try value with _ -> \
         value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"frontend-rejection-code"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXCEPTION"
      |> Expectation.require_unit "Frontend_rejection"
           Outcome.Unit_frontend_rejected)
    (run_fixture input)

let semantic_failure_case =
  let input =
    Fixture.single_source ~module_name:"Semantic_failure"
      ~source:
        "[@@@verocaml.verify]\n\nlet reject (value : int) =\n  [%verocaml.assert \
         false];\n  value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"semantic-failure-kind"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"reject" Outcome.Assertion
      |> Expectation.require_unit "Semantic_failure"
           Outcome.Unit_counterexample)
    (run_fixture input)

let project_case =
  let files =
    [
      {
        Fixture.path = "dune-project";
        contents = "(lang dune 3.17)\n(name grouped_fixture)\n";
      };
      {
        path = "dune";
        contents =
          "(library\n (name grouped_fixture)\n (wrapped false)\n (modules Helper \
           Consumer)\n (libraries verocaml.ghost unix)\n (flags (:standard -ppx \
           \"verocaml-ppx --keep-ghost\")))\n";
      };
      { path = "helper.mli"; contents = "val identity : int -> int\n" };
      {
        path = "helper.ml";
        contents =
          "[@@@verocaml.verify]\n\nlet identity (value : int) = value\n";
      };
      {
        path = "consumer.ml";
        contents =
          "[@@@verocaml.verify]\n\nlet use (value : int) = value\n";
      };
    ]
  in
  let input =
    Fixture.dune_project
      {
        files;
        libraries = [ "verocaml.ghost"; "unix" ];
        targets = [ "@all" ];
        selected_units = [ "Helper"; "Consumer" ];
      }
  in
  Suite.case ~name:"project-library-structured-facts"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Helper" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity")
      |> Expectation.require_named_fact "function:use"
           (Outcome.Function_exists "use"))
    (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ success_case; frontend_rejection_case; semantic_failure_case; project_case ]
