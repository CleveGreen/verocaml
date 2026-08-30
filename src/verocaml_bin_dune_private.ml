type artifact = {
  source : string;
  cmt : string;
}

type project = {
  root : string;
  requested_directory : string;
  artifacts : artifact list;
}

type sexp = Atom of string | List of sexp list

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let existing_directory path =
  try Sys.file_exists path && Sys.is_directory path with Sys_error _ -> false

let existing_file path =
  try Sys.file_exists path && not (Sys.is_directory path) with Sys_error _ -> false

let rec project_root ~requested (directory [@delator.field Fun.id]) =
  [%log.trace "inspect Dune root candidate" ~directory];
  if existing_file (Filename.concat directory "dune-project") then Ok directory
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then
      Error
        (Printf.sprintf
           "could not find dune-project at or above %S"
           requested)
    else project_root ~requested parent
[@@delator.instrument]

let dune_executable () =
  match Sys.getenv_opt "VEROCAML_DUNE" with
  | Some path when not (String.equal path "") -> path
  | Some _ | None -> "dune"

let environment () = Unix.environment ()

let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let remove_file filename =
  try Unix.unlink filename
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let process_status = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let run (arguments [@delator.field (String.concat " ")]) =
  let executable = dune_executable () in
  let stdout_file = Filename.temp_file "verocaml-dune-" ".stdout" in
  let stderr_file = Filename.temp_file "verocaml-dune-" ".stderr" in
  Fun.protect
    ~finally:(fun () ->
      remove_file stdout_file;
      remove_file stderr_file)
    (fun () ->
      let stdout_channel = open_out_bin stdout_file in
      let stderr_channel = open_out_bin stderr_file in
      let argv = Array.of_list (executable :: arguments) in
      [%log.debug "invoke Dune"
        ~executable
        ~arguments:(Delator.Field.string (String.concat " " arguments))];
      let process =
        try
          Ok
            (Unix.create_process_env executable argv (environment ()) Unix.stdin
               (Unix.descr_of_out_channel stdout_channel)
               (Unix.descr_of_out_channel stderr_channel))
        with Unix.Unix_error (error, function_name, argument) ->
          Error
            (Printf.sprintf "%s(%s): %s" function_name argument
               (Unix.error_message error))
      in
      close_out_noerr stdout_channel;
      close_out_noerr stderr_channel;
      match process with
      | Error _ as error -> error
      | Ok process ->
          let _, status = Unix.waitpid [] process in
          let stdout = read_file stdout_file in
          let stderr = read_file stderr_file in
          [%log.debug "Dune process completed"
            ~status:(Delator.Field.string (process_status status))
            ~stdout_bytes:(Delator.Field.int (String.length stdout))
            ~stderr_bytes:(Delator.Field.int (String.length stderr))];
          (match status with
          | Unix.WEXITED 0 -> Ok (stdout, stderr)
          | _ ->
              let detail = String.trim stderr in
              let detail =
                if String.equal detail "" then String.trim stdout else detail
              in
              Error
                (Printf.sprintf "dune %s failed with %s%s"
                   (String.concat " " arguments)
                   (process_status status)
                   (if String.equal detail "" then "" else ":\n" ^ detail))))
[@@delator.instrument]

let parse_csexp input =
  let length = String.length input in
  let rec expression index =
    if index >= length then Error "unexpected end of canonical S-expression"
    else
      match String.unsafe_get input index with
      | '(' -> list (index + 1) []
      | ')' -> Error "unexpected ')' in canonical S-expression"
      | character when character >= '0' && character <= '9' ->
          atom_length index 0
      | character ->
          Error
            (Printf.sprintf
               "unexpected byte %C in canonical S-expression"
               character)
  and list index reversed =
    if index >= length then Error "unterminated canonical S-expression list"
    else if Char.equal (String.unsafe_get input index) ')' then
      Ok (List (List.rev reversed), index + 1)
    else
      let* item, next = expression index in
      list next (item :: reversed)
  and atom_length index value =
    if index >= length then Error "unterminated canonical S-expression atom"
    else
      match String.unsafe_get input index with
      | ':' ->
          let start = index + 1 in
          if value > length - start then
            Error "truncated canonical S-expression atom"
          else Ok (Atom (String.sub input start value), start + value)
      | character when character >= '0' && character <= '9' ->
          let digit = Char.code character - Char.code '0' in
          if value > (max_int - digit) / 10 then
            Error "canonical S-expression atom length overflow"
          else atom_length (index + 1) ((value * 10) + digit)
      | _ -> Error "invalid canonical S-expression atom length"
  in
  let* value, next = expression 0 in
  if next = length then Ok value
  else Error "trailing data after canonical S-expression"
[@@delator.instrument]

let field name fields =
  List.find_map
    (function
      | List [ Atom candidate; value ] when String.equal candidate name ->
          Some value
      | Atom _ | List _ -> None)
    fields

let atom_field name fields =
  match field name fields with Some (Atom value) -> Some value | _ -> None

let optional_atom_field name fields =
  match field name fields with
  | Some (List [ Atom value ]) -> Some value
  | Some (List []) | Some (Atom _) | Some (List _) | None -> None

let rec described_artifacts sexp =
  match sexp with
  | Atom _ -> []
  | List fields ->
      let here =
        match
          (optional_atom_field "impl" fields, optional_atom_field "cmt" fields)
        with
        | Some source, Some cmt when not (String.equal source "")
                                     && not (String.equal cmt "") ->
            [ { source; cmt } ]
        | Some _, Some _ | None, _ | _, None -> []
      in
      here @ List.concat_map described_artifacts fields

let relative_to ~root path =
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  if String.equal path root then ""
  else if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let source_path ~root ~build_context source =
  let build_prefix =
    if String.ends_with ~suffix:"/" build_context then build_context
    else build_context ^ "/"
  in
  let relative =
    if String.starts_with ~prefix:build_prefix source then
      String.sub source (String.length build_prefix)
        (String.length source - String.length build_prefix)
    else source
  in
  if Filename.is_relative relative then Filename.concat root relative else relative

let within_directory ~directory path =
  String.equal path directory
  || String.starts_with ~prefix:(directory ^ "/") path

let deduplicate artifacts =
  List.sort
    (fun left right -> String.compare left.cmt right.cmt)
    artifacts
  |> List.fold_left
       (fun unique artifact ->
         match unique with
         | previous :: _ when String.equal previous.cmt artifact.cmt -> unique
         | _ -> artifact :: unique)
       []
  |> List.rev

let build_and_describe (requested [@delator.field Fun.id]) =
  let* requested_directory =
    try
      let directory = Unix.realpath requested in
      if existing_directory directory then Ok directory
      else Error (Printf.sprintf "%S is not a directory" requested)
    with Unix.Unix_error (error, function_name, argument) ->
      Error
        (Printf.sprintf "%s(%s): %s" function_name argument
           (Unix.error_message error))
  in
  let* root =
    project_root ~requested:requested_directory requested_directory
  in
  let* description, describe_stderr =
    run
      [
        "describe";
        "workspace";
        "--no-print-directory";
        "--root";
        root;
        "--format";
        "csexp";
        "--lang";
        "0.1";
      ]
  in
  if not (String.equal describe_stderr "") then
    output_string Stdlib.stderr describe_stderr;
  let* workspace = parse_csexp description in
  let* build_context =
    match workspace with
    | List fields -> (
        match (atom_field "root" fields, atom_field "build_context" fields) with
        | Some described_root, Some build_context
          when String.equal described_root root -> Ok build_context
        | Some described_root, Some _ ->
            Error
              (Printf.sprintf
                 "Dune described workspace root %S while %S was requested"
                 described_root root)
        | None, _ | _, None ->
            Error
              "Dune workspace description 0.1 omitted root or build_context")
    | Atom _ -> Error "Dune workspace description 0.1 is not a record"
  in
  let requested_relative = relative_to ~root requested_directory in
  let artifacts =
    described_artifacts workspace
    |> List.filter_map (fun artifact ->
           let source_relative =
             let source = relative_to ~root artifact.source in
             let prefix = build_context ^ "/" in
             if String.starts_with ~prefix source then
               String.sub source (String.length prefix)
                 (String.length source - String.length prefix)
             else source
           in
           if
             String.equal requested_relative ""
             || String.equal requested_relative "."
             || within_directory ~directory:requested_relative source_relative
           then
             Some
               {
                 source = source_path ~root ~build_context artifact.source;
                 cmt =
                   (if Filename.is_relative artifact.cmt then
                      Filename.concat root artifact.cmt
                    else artifact.cmt);
               }
           else None)
    |> deduplicate
  in
  let build_targets =
    List.map (fun artifact -> relative_to ~root artifact.cmt) artifacts
  in
  let* () =
    match build_targets with
    | [] -> Ok ()
    | _ ->
        [%log.info "build Dune verification artifacts"
          ~root
          ~artifact_count:(Delator.Field.int (List.length build_targets))];
        let* _, build_stderr =
          run
            ([ "build"; "--no-print-directory"; "--root"; root ]
            @ build_targets)
        in
        if not (String.equal build_stderr "") then
          output_string Stdlib.stderr build_stderr;
        Ok ()
  in
  [%log.info "described Dune verification artifacts"
    ~root
    ~requested_directory
    ~artifact_count:(Delator.Field.int (List.length artifacts))];
  Ok { root; requested_directory; artifacts }
[@@delator.instrument]
