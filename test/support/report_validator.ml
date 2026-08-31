let () =
  match Array.to_list Sys.argv with
  | [ _; report; archive ] -> (
      match Outcome_test_support.Suite.validate_report ~report ~archive with
      | Ok () -> ()
      | Error message ->
          prerr_endline message;
          exit 1)
  | _ ->
      prerr_endline "usage: report_validator REPORT ARCHIVE";
      exit 2
