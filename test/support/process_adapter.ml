type command = {
  program : string;
  arguments : string list;
  forwarded : (string * string) list;
  cleanup_paths : string list;
  adjacency : (string * string * string) list;
}

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let codes_in_brackets text =
  let length = String.length text in
  let allowed = function
    | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let rec scan index codes =
    if index + 5 > length then List.sort_uniq String.compare codes
    else if index > 0 && text.[index - 1] = '[' && String.sub text index 5 = "VERO_" then
      let finish = ref (index + 5) in
      while !finish < length && allowed text.[!finish] do
        incr finish
      done;
      if !finish = index + 5 || !finish >= length || text.[!finish] <> ']' then
        scan (index + 5) codes
      else
        scan !finish (String.sub text index (!finish - index) :: codes)
    else scan (index + 1) codes
  in
  scan 0 []

let stable_codes text =
  text |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if String.starts_with ~prefix:"verocaml:" line
            || String.starts_with ~prefix:"Error: [VERO_" line
         then Some (codes_in_brackets line)
         else None)
  |> List.concat |> List.sort_uniq String.compare

let index_of ~from pattern text =
  let pattern_length = String.length pattern in
  let rec search index =
    if index + pattern_length > String.length text then None
    else if String.sub text index pattern_length = pattern then Some index
    else search (index + 1)
  in
  search from

let run ~cwd command =
  let nonce = Printf.sprintf "%d-%06x" (Unix.getpid ()) (Random.bits ()) in
  let stdout_path = Filename.concat cwd (".process-stdout-" ^ nonce) in
  let stderr_path = Filename.concat cwd (".process-stderr-" ^ nonce) in
  let stdout_fd = Unix.openfile stdout_path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 in
  let stderr_fd = Unix.openfile stderr_path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 in
  let environment =
    let forwarded name =
      List.exists (fun (candidate, _) -> String.equal candidate name) command.forwarded
    in
    let base_environment =
      [
        ("PATH", "/usr/bin:/bin");
        ("DELATOR_LOG", "trace");
        ("DELATOR_FORMAT", "flat");
        ("DELATOR_COLOR", "never");
      ]
      |> List.filter_map (fun (name, value) ->
             if forwarded name then None else Some (name ^ "=" ^ value))
    in
    command.forwarded
    |> List.fold_left
         (fun entries (name, value) -> (name ^ "=" ^ value) :: entries)
         base_environment
    |> Array.of_list
  in
  let shell = "/bin/sh" in
  let argv =
    Array.of_list
      ([ shell; "-c"; "cd \"$1\" && shift && exec \"$@\""; "outcome-process";
         cwd; command.program ]
      @ command.arguments)
  in
  let result =
    Fun.protect
      ~finally:(fun () ->
        Unix.close stdout_fd;
        Unix.close stderr_fd)
      (fun () ->
        try
          let pid =
            Unix.create_process_env shell argv environment Unix.stdin
              stdout_fd stderr_fd
          in
          Ok (snd (Unix.waitpid [] pid))
        with Unix.Unix_error (error, operation, _) ->
          Error
            (Failure.make Failure.Process_protocol
               (Printf.sprintf "%s: %s" operation (Unix.error_message error))))
  in
  match result with
  | Error _ as error -> error
  | Ok process_status ->
      let stdout = read_file stdout_path in
      let stderr = read_file stderr_path in
      Sys.remove stdout_path;
      Sys.remove stderr_path;
      let combined = stdout ^ "\n" ^ stderr in
      let exit_class =
        match process_status with
        | Unix.WEXITED code -> Outcome.Exited code
        | WSIGNALED _ -> Signaled
        | WSTOPPED _ -> Stopped
      in
      if Delator.Runtime.is_enabled ~level:Delator.Trace ~target:"Outcome_test_process"
      then
        Delator.Runtime.event ~target:"Outcome_test_process" ~level:Delator.Trace
          ~msg:"subprocess completed"
          ~fields:
            [
              ("program", Delator.Field.string command.program);
              ( "exit",
                Delator.Field.string
                  (match exit_class with
                  | Outcome.Exited code -> "exited " ^ string_of_int code
                  | Signaled -> "signaled"
                  | Stopped -> "stopped") );
              ("stdout", Delator.Field.string stdout);
              ("stderr", Delator.Field.string stderr);
            ];
      let code_facts = stable_codes combined |> List.map (fun code -> Outcome.Stable_code code) in
      let forwarding_facts =
        command.forwarded |> List.map (fun (name, _) -> Outcome.Forwarded name)
      in
      let cleanup_facts, cleanup_failure =
        command.cleanup_paths
        |> List.fold_left
             (fun (facts, failure) path ->
               if Sys.file_exists (Filename.concat cwd path) then
                 (facts, Some ("cleanup path remains: " ^ path))
               else (Outcome.Cleaned path :: facts, failure))
             ([], None)
      in
      let adjacency_facts, adjacency_failure =
        command.adjacency
        |> List.fold_left
             (fun (facts, failure) (name, before, after) ->
               match (index_of ~from:0 before combined, index_of ~from:0 after combined) with
               | Some left, Some right when left + String.length before <= right ->
                   (Outcome.Adjacent name :: facts, failure)
               | _ -> (facts, Some ("adjacent-output fact failed: " ^ name)))
             ([], None)
      in
      let failure =
        match cleanup_failure with Some _ as failure -> failure | None -> adjacency_failure
      in
      (match failure with
      | Some message -> Error (Failure.make Failure.Process_protocol message)
      | None ->
          Outcome.observation ~status:Outcome.Verified
            ~process_facts:
              (Outcome.Exit_class exit_class :: code_facts @ forwarding_facts
             @ cleanup_facts @ adjacency_facts)
            ()
          |> Outcome.project |> Result.ok)
