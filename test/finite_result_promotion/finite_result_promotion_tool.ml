let () = ignore Finite_result_promotion_prerequisites.ready

let fail message = prerr_endline message; exit 3

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message)

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let run filename =
  match Verification_driver_private.run ~timeout_ms:5_000
          ~allow_imported_opens:false (load filename) with
  | Error (Verification_driver_private.Pipeline_error
      (Verification_pipeline.Engine_error error)) ->
      fail (Symbolic_executor_private.error_to_string error)
  | Error _ -> fail "verification failed before reporting"
  | Ok report ->
      let counters = Verification_driver_private.counters report in
      Printf.printf
        "status=%s functions=%d obligations=%d promotion/path=%d/%d lifecycle=%d/%d/%d consumption=%d/%d\n"
        (status (Verification_driver_private.status report))
        (Verification_driver_private.functions report)
        (Verification_driver_private.obligations report)
        counters.Verification_session.finite_result_promotions
        counters.finite_result_path_records counters.finite_result_manifests
        counters.finite_result_completions counters.finite_result_finalizations
        counters.finite_result_consumption_attempts
        counters.finite_result_consumptions

let () =
  if Array.length Sys.argv < 2 then
    fail "usage: finite_result_promotion_tool [matrix | FILE]"
  else if String.equal Sys.argv.(1) "matrix" then
    List.iter print_endline
      (Verification_session.For_testing.finite_result_call_instance_matrix ())
  else
    run Sys.argv.(Array.length Sys.argv - 1)
