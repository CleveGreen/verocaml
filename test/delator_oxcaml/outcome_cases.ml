open Outcome_test_support

let portable_trace ~environment:_ ~workspace:_ =
  let result, trace = Delator_trace.capture Oxcaml_backend.run in
  if result <> 42 then
    Error
      (Failure.make Failure.Expectation_mismatch
         "portable instrumented execution returned the wrong result")
  else if String.equal trace "" then
    Error
      (Failure.make Failure.Expectation_mismatch
         "portable structured tracing emitted no events")
  else
    Ok
      (Outcome.observation ~status:Outcome.Verified
         ~named_facts:
           [
             ( "portable-delator-events",
               Outcome.Function_exists "portable_instrumented" );
           ]
         ()
      |> Outcome.project)

let portable_trace_case =
  Suite.case ~name:"portable-structured-events"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_named_fact "portable-delator-events"
           (Outcome.Function_exists "portable_instrumented"))
    portable_trace

let () =
  Suite.run_cli ~suite_path:"test/delator_oxcaml/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ portable_trace_case ]
