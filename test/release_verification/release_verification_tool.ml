let fail format = Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let controlled_inconclusive () =
  let symbol : Vir.symbol =
    {
      symbol_id = 0;
      source_name = "x";
      sort = Integer;
      role = Input;
      span = Diagnostic.file_span "controlled.ml";
    }
  in
  let obligation : Vir.obligation =
    {
      obligation_index = 0;
      function_ref = { function_index = 0; function_name = "controlled" };
      kind = Assertion { assertion_ordinal = 0 };
      span = Diagnostic.file_span "controlled.ml";
      assumptions = Vir.integer_range (Integer_symbol symbol);
      required_preceding_safety = [];
      path_condition = [];
      goal = Boolean_constant true;
      projection_symbols = [ symbol ];
    }
  in
  let config =
    match Solver_backend.config ~timeout_ms:17 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  match
    Solver_backend.For_testing.solve_after_translation Unknown config obligation
  with
  | Ok
      (Solver_backend.Inconclusive
        {
          configured_timeout_ms = 17;
          configured_rlimit;
          reason = Backend_unknown "controlled unknown";
        })
    when configured_rlimit = Solver_backend.rlimit config ->
      print_endline "controlled-inconclusive: result=unknown exit=3"
  | Ok _ -> fail "controlled solver did not report unknown"
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let () =
  match Array.to_list Sys.argv with
  | [ _; "controlled-inconclusive" ] -> controlled_inconclusive ()
  | _ -> fail "usage: release_verification_tool controlled-inconclusive"
