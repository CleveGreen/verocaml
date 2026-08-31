open Outcome_test_support

let run ~environment:_ ~workspace:_ =
  Delator_trace.event "failure-probe-trace";
  Outcome.observation ~status:Outcome.Verified () |> Outcome.project |> Result.ok

let () =
  Suite.run_cli ~suite_path:"test/outcome_framework/failure_probe_a.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      Suite.case ~name:"intentional-mismatch"
        ~expectation:(Expectation.status Outcome.Counterexample Expectation.empty)
        run;
    ]
