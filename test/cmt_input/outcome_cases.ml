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

let no_interface_load_case =
  let name = "ordinary-no-interface-loads" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace:_ ->
      match Cmt_input.load Prepared_artifacts.implementation with
      | Ok implementation
        when implementation.Cmt_input.embedded_interface
             && not implementation.explicit_interface ->
          Ok
            (Outcome.observation ~status:Outcome.Verified
               ~units:[ (name, Outcome.Unit_verified) ]
               ~named_facts:
                 [ ("function:" ^ name, Outcome.Function_exists name) ]
               ()
            |> Outcome.project)
      | Ok _ ->
          Error
            (Failure.make Failure.Runner_internal
               "ordinary no-.mli CMT was not classified by its embedded interface")
      | Error diagnostic ->
          Error
            (Failure.make Failure.Runner_internal
               ("ordinary no-.mli CMT load failed: " ^ diagnostic.code)))

let stable_snapshot_case =
  let name = "stable-snapshot-decodes-receipted-bytes" in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ name)
           (Outcome.Function_exists name))
    (fun ~environment:_ ~workspace:_ ->
      if Cmt_input.For_testing.stable_snapshot_uses_receipted_bytes () then
        Ok
          (Outcome.observation ~status:Outcome.Verified
             ~units:[ (name, Outcome.Unit_verified) ]
             ~named_facts:
               [ ("function:" ^ name, Outcome.Function_exists name) ]
             ()
          |> Outcome.project)
      else
        Error
          (Failure.make Failure.Runner_internal
             "stable snapshot decoder observed substituted path bytes"))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ implementation_case; interface_case; no_interface_load_case;
      stable_snapshot_case ]
