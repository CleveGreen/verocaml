open Outcome_test_support

let fail format = Printf.ksprintf failwith format

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text pattern =
  let rec loop index =
    if index + String.length pattern > String.length text then false
    else if String.sub text index (String.length pattern) = pattern then true
    else loop (index + 1)
  in
  loop 0

let check suite case_name report archive =
  if not (Sys.file_exists report) then fail "%s report was not published" suite;
  if not (Sys.file_exists archive) then fail "%s archive was not published" suite;
  let validation_message =
    match Suite.validate_report ~report ~archive with
    | Error message -> message
    | Ok () -> fail "%s validator accepted intentional failure" suite
  in
  let report_text = read report in
  let archive_text = read archive in
  if not (contains report_text ("identity=" ^ suite ^ "::" ^ case_name)) then
    fail "%s report lacks canonical identity" suite;
  if
    not
      (let executable = Filename.basename suite |> Filename.remove_extension in
       contains report_text
        ("rerun=dune exec test/outcome_framework/" ^ executable
        ^ ".exe -- --case " ^ suite ^ "::" ^ case_name))
  then fail "%s report lacks exact rerun" suite;
  if not (contains archive_text "entry=") || not (contains archive_text "/failure.txt") then
    fail "%s archive lacks encoded identity/run entry" suite;
  if not (contains archive_text "/delator-trace.log") then
    fail "%s archive lacks its failure-only Delator trace" suite;
  if not (contains validation_message "Delator trace (diagnostic only; not an oracle)")
  then fail "%s validator did not display its Delator trace" suite

let check_quiet_success report archive =
  (match Suite.validate_report ~report ~archive with
  | Ok () -> ()
  | Error message -> fail "successful framework report failed: %s" message);
  let report_text = read report in
  let archive_text = read archive in
  if contains report_text "failure-probe-trace" then
    fail "successful report contains diagnostic Delator output";
  if contains archive_text "/delator-trace.log" then
    fail "successful archive contains diagnostic Delator output"

let () =
  match Array.to_list Sys.argv with
  | [ _; success_report; success_archive; report_a; archive_a; report_b; archive_b ] ->
      check_quiet_success success_report success_archive;
      check "test/outcome_framework/failure_probe_a.ml" "intentional-mismatch"
        report_a archive_a;
      check "test/outcome_framework/failure_probe_b.ml"
        "intentional-verifier-outcome" report_b archive_b;
      let report_b_text = read report_b in
      if not (contains report_b_text "category=verifier-outcome") then
        fail "verifier-outcome category probe was not recorded";
      if not (contains report_b_text "category=runner-internal") then
        fail "runner-internal category probe was not recorded";
      let report_a_text = read report_a in
      let archive_a_text = read archive_a in
      if contains report_a_text "failure-probe-trace" then
        fail "Delator trace leaked into the semantic result report";
      if not (contains archive_a_text "failure-probe-trace") then
        fail "failure archive omitted the trace-level Delator marker";
      if String.equal report_a report_b || String.equal archive_a archive_b then
        fail "suite-qualified artifacts collide";
      print_endline "forced-sandbox-persistence: pass"
  | _ ->
      fail
        "usage: persistence_check SUCCESS_REPORT SUCCESS_ARCHIVE REPORT_A ARCHIVE_A REPORT_B ARCHIVE_B"
