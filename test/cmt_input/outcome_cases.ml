open Outcome_test_support

let suite_path = "test/cmt_input/outcome_cases.ml"

let prepared artifact =
  match Fixture.prepared_cmt ~declared_dependencies:[ artifact ] artifact with
  | Ok input -> input
  | Error message -> invalid_arg message

let run artifact ~environment ~workspace =
  Fixture.run ~environment ~workspace (prepared artifact)

let implementation_case =
  Suite.case ~name:"prepared-implementation-classifies-mutation"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_MUTATION"
      |> Expectation.require_unit "Implementation"
           Outcome.Unit_frontend_rejected)
    (run Prepared_artifacts.implementation)

let interface_case =
  Suite.case ~name:"prepared-interface-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_INTERFACE"
      |> Expectation.require_unit "prepared-interface.cmti"
           Outcome.Unit_frontend_rejected)
    (run Prepared_artifacts.interface)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ implementation_case; interface_case ]
