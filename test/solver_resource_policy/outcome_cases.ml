open Outcome_test_support

let () = ignore Solver_resource_policy_outcome_prerequisites.ready

let suite_path = "test/solver_resource_policy/outcome_cases.ml"

let passed = Outcome.observation ~status:Outcome.Verified () |> Outcome.project

let check condition message =
  if condition then Ok ()
  else Error (Failure.make Failure.Expectation_mismatch message)

let ( let* ) = Result.bind

let semantic_case name run =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let* () = run () in
      Ok passed)

let default_policy_is_finite =
  semantic_case "default-policy-is-finite" (fun () ->
      match Solver_policy_private.create_default ~timeout_ms:10_000 with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Solver_policy_private.error_to_string error))
      | Ok policy ->
          let* () =
            check
              (Solver_policy_private.timeout_ms policy = 10_000)
              "default policy did not preserve its positive timeout"
          in
          check
            (Solver_policy_private.rlimit policy
             = Solver_policy_private.default_rlimit
            && Solver_policy_private.default_rlimit > 0)
            "default policy does not own one finite positive rlimit")

let nonpositive_values_are_rejected =
  semantic_case "nonpositive-policy-values-are-rejected" (fun () ->
      [ (0, 1); (-1, 1); (1, 0); (1, -1) ]
      |> List.fold_left
           (fun result (timeout_ms, rlimit) ->
             let* () = result in
             match Solver_policy_private.create ~timeout_ms ~rlimit with
             | Error _ -> Ok ()
             | Ok _ ->
                 Error
                   (Failure.make Failure.Expectation_mismatch
                      "nonpositive solver policy value was accepted"))
           (Ok ()))

let explicit_budget_is_preserved =
  semantic_case "explicit-positive-budget-is-preserved" (fun () ->
      match
        Solver_backend.config_with_rlimit ~timeout_ms:10_000 ~rlimit:17
      with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Solver_backend.error_to_string error))
      | Ok configuration ->
          check
            (Solver_backend.timeout_ms configuration = 10_000
            && Solver_backend.rlimit configuration = 17)
            "backend configuration changed an explicit positive budget")

let legacy_entry_uses_the_default =
  semantic_case "legacy-entry-uses-positive-default" (fun () ->
      match Solver_backend.config ~timeout_ms:10_000 with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Solver_backend.error_to_string error))
      | Ok configuration ->
          check
            (Solver_backend.rlimit configuration
             = Solver_policy_private.default_rlimit
            && Solver_backend.rlimit configuration > 0)
            "legacy backend entry bypassed the finite positive default")

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      default_policy_is_finite;
      nonpositive_values_are_rejected;
      explicit_budget_is_preserved;
      legacy_entry_uses_the_default;
    ]
