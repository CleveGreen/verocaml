open Outcome_test_support

let verifier_failure ~environment:_ ~workspace:_ =
  Delator_trace.event "verifier-failure-trace";
  Error
    (Failure.make Failure.Verifier_outcome
       "intentional verifier-outcome category probe")

let internal_failure ~environment:_ ~workspace:_ =
  Delator_trace.event "runner-internal-trace";
  failwith "intentional runner-internal category probe"

let () =
  Suite.run_cli ~suite_path:"test/outcome_framework/failure_probe_b.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      Suite.case ~name:"intentional-verifier-outcome"
        ~expectation:Expectation.empty verifier_failure;
      Suite.case ~name:"intentional-runner-internal"
        ~expectation:Expectation.empty internal_failure;
    ]
