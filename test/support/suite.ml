type case = {
  name : string;
  expectation : Expectation.t;
  run :
    environment:Project_environment.t ->
    workspace:string ->
    (Outcome.t, Failure.t) result;
}

type case_result =
  | Passed of Outcome.t
  | Failed of Failure.t * Outcome.t option

let case ~name ~expectation run = { name; expectation; run }

let canonical_identity ~suite_path ~case_name = suite_path ^ "::" ^ case_name

let valid_case_name name =
  let length = String.length name in
  length > 0
  &&
  (match name.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
         | _ -> false)
       name
  && not (String.contains name '/')
  && not (String.contains name ':')
  && name <> "."
  && name <> ".."

let valid_suite_path path =
  (not (String.equal path ""))
  && Filename.is_relative path
  && not (String.contains path ':')
  && not (String.contains path '\\')
  && (String.split_on_char '/' path
     |> List.for_all (fun component ->
            component <> "" && component <> "." && component <> ".."))

let validate_cases ~suite_path cases =
  if not (valid_suite_path suite_path) then
    Error
      (Failure.make Failure.Runner_malformed_identity
         ("invalid repository-relative suite path: " ^ suite_path))
  else
    match List.find_opt (fun case -> not (valid_case_name case.name)) cases with
    | Some case ->
        Error
          (Failure.make Failure.Runner_malformed_identity
             ("invalid case name: " ^ case.name))
    | None ->
        let names = List.map (fun case -> case.name) cases in
        if List.length names <> List.length (List.sort_uniq String.compare names) then
          Error
            (Failure.make Failure.Runner_duplicate_identity
               "duplicate canonical case identity")
        else Ok ()

let select ~suite_path cases identity =
  Result.bind (validate_cases ~suite_path cases) (fun () ->
      let prefix = suite_path ^ "::" in
      if
        (not (String.starts_with ~prefix identity))
        || String.length identity = String.length prefix
        || String.contains (String.sub identity (String.length prefix)
                              (String.length identity - String.length prefix)) ':'
      then
        Error
          (Failure.make Failure.Runner_malformed_identity
             ("selector is not a full canonical identity: " ^ identity))
      else
        let name =
          String.sub identity (String.length prefix)
            (String.length identity - String.length prefix)
        in
        if not (valid_case_name name) then
          Error
            (Failure.make Failure.Runner_malformed_identity
               ("selector contains a malformed case name: " ^ identity))
        else
          match List.find_opt (fun case -> case.name = name) cases with
          | Some case -> Ok case
          | None ->
              Error
                (Failure.make Failure.Runner_unknown_identity
                   ("unknown canonical case identity: " ^ identity)))

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path

let encode value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_') as character ->
          Buffer.add_char buffer character
      | character -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code character)))
    value;
  Buffer.contents buffer

let run_id () =
  Printf.sprintf "%d-%08x" (Unix.getpid ()) (Random.bits ())

let result_of_case ~environment ~workspace case =
  Delator_trace.capture (fun () ->
      try
        match case.run ~environment ~workspace with
        | Error failure -> Failed (failure, None)
        | Ok actual -> (
            match Expectation.check case.expectation actual with
            | Ok () -> Passed actual
            | Error message ->
                Failed
                  (Failure.make Failure.Expectation_mismatch message, Some actual))
      with exn ->
        Failed
          ( Failure.make Failure.Runner_internal
              (Printexc.to_string exn),
            None ))

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let rec archive_files ~root relative =
  let path = if relative = "" then root else Filename.concat root relative in
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then
    if Filename.basename path = "_build" then []
    else
      Sys.readdir path |> Array.to_list
      |> List.concat_map (fun name ->
             archive_files ~root
               (if relative = "" then name else Filename.concat relative name))
  else if (Unix.stat path).st_size > 1_000_000 then []
  else [ (relative, read_file path) ]

let report_case channel ~suite_path ~run_id case result archive_location =
  let identity = canonical_identity ~suite_path ~case_name:case.name in
  Printf.fprintf channel "case=%s\n" (encode identity);
  Printf.fprintf channel "identity=%s\n" identity;
  Printf.fprintf channel "expected=%s\n" (Expectation.describe case.expectation);
  Printf.fprintf channel "archive=%s\n" archive_location;
  let executable = Filename.remove_extension suite_path ^ ".exe" in
  Printf.fprintf channel
    "rerun=dune exec %s -- --case %s\n"
    executable identity;
  (match result with
  | Passed actual ->
      Printf.fprintf channel "result=pass\nactual=%s\n" (Outcome.to_string actual)
  | Failed (failure, actual) ->
      Printf.fprintf channel "result=fail\ncategory=%s\nmessage=%s\nactual=%s\n"
        (Failure.category_name (Failure.category failure))
        (String.escaped (Failure.message failure))
        (Option.fold ~none:"unavailable" ~some:Outcome.to_string actual));
  Printf.fprintf channel "run=%s\nend-case\n" run_id

let execute ~suite_path ~environment ~workspace_root cases =
  List.map
    (fun case ->
      let workspace =
        Filename.concat workspace_root
          (encode (canonical_identity ~suite_path ~case_name:case.name))
      in
      remove_tree workspace;
      mkdir_p workspace;
      let result, trace = result_of_case ~environment ~workspace case in
      (case, workspace, result, trace))
    cases

let write_results ~suite_path ~report ~archive ~environment cases =
  mkdir_p (Filename.dirname report);
  mkdir_p (Filename.dirname archive);
  let run_id = run_id () in
  let workspace_root =
    Filename.concat (Filename.dirname report) (".outcome-work-" ^ run_id)
  in
  mkdir_p workspace_root;
  let results = execute ~suite_path ~environment ~workspace_root cases in
  let report_channel = open_out_bin report in
  let archive_channel = open_out_bin archive in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr report_channel;
      close_out_noerr archive_channel;
      remove_tree workspace_root)
    (fun () ->
      Printf.fprintf report_channel "outcome-report-version=1\nsuite=%s\n" suite_path;
      Printf.fprintf archive_channel "outcome-failure-archive-version=1\nsuite=%s\nrun=%s\n"
        suite_path run_id;
      List.iter
        (fun (case, workspace, result, trace) ->
          report_case report_channel ~suite_path ~run_id case result archive;
          match result with
          | Passed _ -> ()
          | Failed (failure, _) ->
              let identity = canonical_identity ~suite_path ~case_name:case.name in
              let entry_root = encode identity ^ "/" ^ run_id in
              Printf.fprintf archive_channel "entry=%s/failure.txt\ncontent=%S\n"
                entry_root (Failure.to_string failure);
              if trace <> "" then
                Printf.fprintf archive_channel
                  "entry=%s/delator-trace.log\ncontent=%S\n"
                  entry_root trace;
              archive_files ~root:workspace ""
              |> List.iter (fun (relative, contents) ->
                     Printf.fprintf archive_channel "entry=%s/%s\ncontent=%S\n"
                       entry_root (encode relative) contents))
        results);
  results

let contains line text =
  String.split_on_char '\n' text |> List.exists (String.equal line)

let failure_traces archive =
  let trace_entry line =
    String.starts_with ~prefix:"entry=" line
    && String.ends_with ~suffix:"/delator-trace.log" line
  in
  let rec collect traces = function
    | entry :: content :: rest when trace_entry entry ->
        let trace =
          try Scanf.sscanf content "content=%S" Fun.id
          with Scanf.Scan_failure _ | End_of_file ->
            "[malformed Delator trace entry]"
        in
        collect (trace :: traces) rest
    | _ :: rest -> collect traces rest
    | [] -> List.rev traces
  in
  read_file archive |> String.split_on_char '\n' |> collect []

let validate_report ~report ~archive =
  if not (Sys.file_exists report) then Error "result report is missing"
  else if not (Sys.file_exists archive) then Error "failure archive is missing"
  else
    let contents = read_file report in
    if not (contains "outcome-report-version=1" contents) then
      Error "unsupported result report"
    else if contains "result=fail" contents then
      let traces = failure_traces archive in
      let rendered_traces =
        match traces with
        | [] -> ""
        | _ ->
            "\nDelator trace (diagnostic only; not an oracle):\n"
            ^ String.concat "\n" traces
      in
      Error
        ("outcome suite failed; inspect " ^ report ^ " and " ^ archive
       ^ rendered_traces)
    else Ok ()

let fail_and_exit failure =
  prerr_endline (Failure.to_string failure);
  exit 2

let sorted_cases cases = List.sort (fun left right -> String.compare left.name right.name) cases

let run_cli ~suite_path ~manifest ~expected_environment cases =
  match validate_cases ~suite_path cases with
  | Error failure -> fail_and_exit failure
  | Ok () ->
      let arguments = Array.to_list Sys.argv |> List.tl in
      (match arguments with
      | [ "--list" ] ->
          sorted_cases cases
          |> List.iter (fun case ->
                 print_endline (canonical_identity ~suite_path ~case_name:case.name))
      | [ "--case"; identity ] -> (
          match select ~suite_path cases identity with
          | Error failure -> fail_and_exit failure
          | Ok case -> (
              match Project_environment.validate ~expected:expected_environment manifest with
              | Error failure -> fail_and_exit failure
              | Ok environment ->
                  let run_id = run_id () in
                  let root =
                    Filename.concat "_build/outcome-framework/manual"
                      (encode identity ^ "-" ^ run_id)
                  in
                  let _, workspace, result, trace =
                    execute ~suite_path ~environment ~workspace_root:root [ case ] |> List.hd
                  in
                  (match result with
                  | Passed _ ->
                      remove_tree root;
                      print_endline (identity ^ ": pass")
                  | Failed (failure, _) ->
                      Printf.eprintf "%s: %s\nworkspace=%s\n" identity
                        (Failure.to_string failure) workspace;
                      if trace <> "" then
                        Printf.eprintf
                          "Delator trace (diagnostic only; not an oracle):\n%s"
                          trace;
                      exit 1)))
      | [ "--produce"; report; archive ] -> (
          match Project_environment.validate ~expected:expected_environment manifest with
          | Error failure ->
              let channel = open_out_bin report in
              Printf.fprintf channel
                "outcome-report-version=1\nsuite=%s\nresult=fail\ncategory=%s\nmessage=%s\n"
                suite_path (Failure.category_name (Failure.category failure))
                (String.escaped (Failure.message failure));
              close_out channel;
              let channel = open_out_bin archive in
              Printf.fprintf channel
                "outcome-failure-archive-version=1\nentry=environment/failure.txt\ncontent=%S\n"
                (Failure.to_string failure);
              close_out channel
          | Ok environment ->
              ignore (write_results ~suite_path ~report ~archive ~environment cases))
      | [] -> (
          match Project_environment.validate ~expected:expected_environment manifest with
          | Error failure -> fail_and_exit failure
          | Ok environment ->
              let root = Filename.concat "_build/outcome-framework/manual" (run_id ()) in
              let results = execute ~suite_path ~environment ~workspace_root:root cases in
              let failures =
                List.filter_map
                  (fun (case, _, result, trace) ->
                    match result with
                    | Passed _ -> None
                    | Failed (failure, _) -> Some (case.name, failure, trace))
                  results
              in
              if failures = [] then (
                remove_tree root;
                Printf.printf "%d cases passed\n" (List.length results))
              else (
                List.iter
                  (fun (name, failure, trace) ->
                    Printf.eprintf "%s: %s\n" name (Failure.to_string failure);
                    if trace <> "" then
                      Printf.eprintf
                        "Delator trace (diagnostic only; not an oracle):\n%s"
                        trace)
                  failures;
                exit 1))
      | _ ->
          fail_and_exit
            (Failure.make Failure.Runner_selector_protocol
               "expected no arguments, --list, --case <identity>, or --produce <report> <archive>"))
