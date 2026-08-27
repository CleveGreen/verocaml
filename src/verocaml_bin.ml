open Verocaml_bin_render

type options = {
  input : string;
  dependencies : string list;
  timeout_ms : int;
  rlimit : int option;
  threads : int;
  dump_sst : string option;
  dump_vir : string option;
}
type parsed = (options, string) result
let default_timeout_ms = 5000
external production_default_threads : int -> int
  = "verocaml_default_threads"
let render_message = Verocaml_bin_render.message
let parse_positive_int flag value =
  match int_of_string_opt value with
  | Some timeout when timeout > 0 -> Ok timeout
  | _ -> Error (render_message (Positive_integer flag))
let parse_positive_decimal_int flag value =
  if
    String.length value > 0
    && String.for_all
         (fun character -> character >= '0' && character <= '9')
         value
  then parse_positive_int flag value
  else Error (render_message (Positive_integer flag))
let resolve_threads = function
  | None -> Ok (production_default_threads (Multicore.max_domains ()))
  | Some threads -> Ok threads
let parse argv : parsed =
  match Array.to_list argv with
  | [ _ ] -> Error Verocaml_bin_render.usage
  | _ :: "verify" :: input :: arguments ->
      let rec loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
          threads threads_seen dump_sst dump_vir solver_seen = function
        | [] ->
            (match resolve_threads threads with
            | Error _ as error -> error
            | Ok threads ->
                Ok
                  {
                    input;
                    dependencies = List.rev dependencies;
                    timeout_ms;
                    rlimit;
                    threads;
                    dump_sst;
                    dump_vir;
                  })
        | "--dependency" :: value :: rest ->
            if String.starts_with ~prefix:"--" value then
              Error (render_message (Option_requires ("--dependency", "FILE.cmt")))
            else
              loop (value :: dependencies) timeout_ms timeout_seen rlimit
                rlimit_seen threads threads_seen dump_sst dump_vir solver_seen
                rest
        | "--solver" :: value :: rest ->
            if solver_seen then Error (render_message (Option_once "--solver"))
            else if not (String.equal value "z3") then
              Error (render_message (Unsupported_solver value))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen dump_sst dump_vir true rest
        | "--timeout-ms" :: value :: rest ->
            if timeout_seen then
              Error (render_message (Option_once "--timeout-ms"))
            else (
              match parse_positive_int "--timeout-ms" value with
              | Error _ as error -> error
              | Ok timeout_ms ->
                  loop dependencies timeout_ms true rlimit rlimit_seen threads
                    threads_seen dump_sst dump_vir solver_seen rest)
        | "--rlimit" :: value :: rest ->
            if rlimit_seen then
              Error (render_message (Option_once "--rlimit"))
            else (
              match parse_positive_decimal_int "--rlimit" value with
              | Error _ as error -> error
              | Ok rlimit ->
                  loop dependencies timeout_ms timeout_seen (Some rlimit) true
                    threads threads_seen dump_sst dump_vir solver_seen rest)
        | "--threads" :: value :: rest ->
            if threads_seen then
              Error (render_message (Option_once "--threads"))
            else (
              match parse_positive_decimal_int "--threads" value with
              | Error _ as error -> error
              | Ok threads ->
                  loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                    (Some threads) true dump_sst dump_vir solver_seen rest)
        | "--dump-sst" :: value :: rest ->
            if Option.is_some dump_sst then
              Error (render_message (Option_once "--dump-sst"))
            else if String.starts_with ~prefix:"--" value then
              Error (render_message (Option_requires ("--dump-sst", "FILE")))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen (Some value) dump_vir solver_seen rest
        | "--dump-vir" :: value :: rest ->
            if Option.is_some dump_vir then
              Error (render_message (Option_once "--dump-vir"))
            else if String.starts_with ~prefix:"--" value then
              Error (render_message (Option_requires ("--dump-vir", "FILE")))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen dump_sst (Some value) solver_seen rest
        | flag :: _ when String.starts_with ~prefix:"--" flag ->
            Error (render_message (Unknown_option flag))
        | argument :: _ ->
            Error (render_message (Unexpected_argument argument))
      in
      loop [] default_timeout_ms false None false None false None None false
        arguments
  | _ -> Error Verocaml_bin_render.usage
let render_frontend_error diagnostic =
  prerr_endline (Verocaml_bin_render.frontend_error diagnostic)
let write_dump filename contents =
  try
    let channel = open_out_bin filename in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel contents);
    Ok ()
  with Sys_error message -> Error message
let write_optional_dump label filename contents =
  match filename with
  | None -> Ok ()
  | Some filename -> (
      match write_dump filename contents with
      | Ok () -> Ok ()
      | Error message ->
          Error
            (render_message
               (Dump_write_failure { label; filename; detail = message })))
let find_substring_from ~start ~substring string =
  let substring_length = String.length substring in
  let last_start = String.length string - substring_length in
  let rec find index =
    if index > last_start then None
    else if String.sub string index substring_length = substring then Some index
    else find (index + 1)
  in
  if substring_length = 0 then Some start else find start
let replace_substring_at ~start ~length ~replacement string =
  String.sub string 0 start ^ replacement
  ^ String.sub string (start + length)
      (String.length string - start - length)
let render_source_semantic_sst ~private_cmt semantic_sst =
  let authority_prefix = "authority=retained " in
  let private_field = " cmt=" ^ private_cmt ^ " interface=" in
  let stable_field =
    " cmt=<private-source-compilation-cmt> interface="
  in
  let private_field_length = String.length private_field in
  let rec render matched rendered = function
    | [] -> (
        match matched with
        | 1 -> Ok (String.concat "\n" (List.rev rendered))
        | count ->
            Error (render_message (Source_cmt_identity_count count)))
    | line :: rest ->
        if String.starts_with ~prefix:authority_prefix line then
          match
            find_substring_from ~start:0 ~substring:private_field line
          with
          | None -> render matched (line :: rendered) rest
          | Some start ->
              let line =
                replace_substring_at ~start ~length:private_field_length
                  ~replacement:stable_field line
              in
              render (matched + 1) (line :: rendered) rest
        else render matched (line :: rendered) rest
  in
  render 0 [] (String.split_on_char '\n' semantic_sst)
let fail_internal message =
  prerr_endline (Verocaml_bin_render.internal_error message);
  3
let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
let normalize_absolute_path path =
  let components =
    absolute_path path |> String.split_on_char '/'
    |> List.fold_left
         (fun normalized -> function
           | "" | "." -> normalized
           | ".." -> (
               match normalized with [] -> [] | _ :: rest -> rest)
           | component -> component :: normalized)
         []
    |> List.rev
  in
  "/" ^ String.concat "/" components
let rec canonicalize_existing_prefix path =
  try Ok (Unix.realpath path)
  with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      let parent = Filename.dirname path in
      if String.equal parent path then Ok path
      else
        Result.map
          (fun canonical_parent ->
            normalize_absolute_path
              (Filename.concat canonical_parent (Filename.basename path)))
          (canonicalize_existing_prefix parent)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (render_message
           (Unix_failure
              {
                function_name;
                argument;
                detail = Unix.error_message error;
              }))
type path_identity = {
  label : string;
  original : string;
  lexical : string;
  canonical : string;
  inode : (int * int) option;
}
let path_identity label original =
  let uncollapsed_absolute = absolute_path original in
  let lexical = normalize_absolute_path uncollapsed_absolute in
  match canonicalize_existing_prefix uncollapsed_absolute with
  | Error message ->
      Error
        (render_message
           (Path_inspection_failure { label; path = original; detail = message }))
  | Ok canonical ->
      let inode =
        try
          let stats = Unix.stat uncollapsed_absolute in
          Some (stats.st_dev, stats.st_ino)
        with
        | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
        | Unix.Unix_error (error, function_name, argument) ->
            raise
              (Sys_error
                 (render_message
                    (Unix_failure
                       {
                         function_name;
                         argument;
                         detail = Unix.error_message error;
                       })))
      in
      Ok { label; original; lexical; canonical; inode }
let same_path left right =
  String.equal left.lexical right.lexical
  || String.equal left.canonical right.canonical
  ||
  match (left.inode, right.inode) with
  | Some left_inode, Some right_inode -> left_inode = right_inode
  | None, _ | _, None -> false
let validate_distinct_paths options =
  let requested =
    ("input", options.input)
    :: (List.mapi
          (fun index path ->
            (Printf.sprintf "dependency[%d]" index, path))
          options.dependencies
       @ List.filter_map
           (fun (label, path) -> Option.map (fun path -> (label, path)) path)
           [ ("SST dump", options.dump_sst); ("VIR dump", options.dump_vir) ])
  in
  let rec inspect inspected = function
    | [] -> Ok ()
    | (label, path) :: rest -> (
        match
          try path_identity label path
          with Sys_error message ->
            Error
              (render_message
                 (Path_inspection_failure { label; path; detail = message }))
        with
        | Error _ as error -> error
        | Ok identity -> (
            match List.find_opt (same_path identity) inspected with
            | Some other ->
                Error
                  (render_message
                     (Same_path
                        {
                          left_label = other.label;
                          left_path = other.original;
                          right_label = identity.label;
                          right_path = identity.original;
                        }))
            | None -> inspect (identity :: inspected) rest))
  in
  inspect [] requested
let executable_directory () =
  absolute_path Sys.executable_name |> Filename.dirname
let first_existing_file paths =
  List.find_opt
    (fun path ->
      try Sys.file_exists path && not (Sys.is_directory path)
      with Sys_error _ -> false)
    paths
let first_existing_directory paths =
  List.find_opt
    (fun path ->
      try Sys.file_exists path && Sys.is_directory path
      with Sys_error _ -> false)
    paths
let compiler_prefix () =
  Config.standard_library |> Filename.dirname |> Filename.dirname
let base_ocaml_version () =
  match String.index_opt Sys.ocaml_version '+' with
  | None -> Sys.ocaml_version
  | Some index -> String.sub Sys.ocaml_version 0 index
type source_toolchain = {
  ocamlc : string;
  ppx : string;
  ghost_directory : string;
}
let source_toolchain () =
  let executable_directory = executable_directory () in
  let install_prefix = Filename.dirname executable_directory in
  let getenv_or name fallback =
    match Sys.getenv_opt name with
    | Some value -> value
    | None -> fallback
  in
  let default_ocamlc =
    Filename.concat (Filename.concat (compiler_prefix ()) "bin") "ocamlc"
  in
  let default_ppx_candidates =
    [
      Filename.concat executable_directory "verocaml-ppx";
      Filename.concat
        (Filename.concat executable_directory "..")
        "ppx/vero_ppx.exe";
    ]
  in
  let installed_ghost_root =
    Filename.concat
      (Filename.concat
         (Filename.concat
            (Filename.concat install_prefix "lib")
            "ocaml")
         (base_ocaml_version ()))
      "site-lib/verocaml/ghost"
  in
  let default_ghost_candidates =
    [
      installed_ghost_root;
      Filename.concat
        (Filename.concat (Filename.concat install_prefix "lib") "verocaml")
        "ghost";
      Filename.concat
        (Filename.concat executable_directory "..")
        "runtime/.vero_ghost.objs/byte";
    ]
  in
  let ocamlc = getenv_or "VEROCAML_OCAMLC" default_ocamlc in
  let ppx =
    match Sys.getenv_opt "VEROCAML_PPX" with
    | Some path -> first_existing_file [ path ]
    | None -> first_existing_file default_ppx_candidates
  in
  let ghost_directory =
    match Sys.getenv_opt "VEROCAML_GHOST_DIR" with
    | Some path -> first_existing_directory [ path ]
    | None -> first_existing_directory default_ghost_candidates
  in
  match (first_existing_file [ ocamlc ], ppx, ghost_directory) with
  | Some ocamlc, Some ppx, Some ghost_directory ->
      Ok { ocamlc; ppx; ghost_directory }
  | None, _, _ ->
      Error (render_message (Compiler_unavailable ocamlc))
  | _, None, _ -> Error (render_message Ppx_unavailable)
  | _, _, None -> Error (render_message Ghost_unavailable)
let environment_with_compiler_color () =
  let environment = Unix.environment () in
  if
    Array.exists
      (fun entry -> String.starts_with ~prefix:"OCAML_COLOR=" entry)
      environment
  then environment
  else Array.append environment [| "OCAML_COLOR=always" |]
let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)
let rec remove_path path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_path (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path
let with_temporary_directory action =
  match
    try Ok (Filename.temp_dir ~perms:0o700 "verocaml-source-" "")
    with
    | Sys_error message -> Error message
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (render_message
             (Unix_failure
                {
                  function_name;
                  argument;
                  detail = Unix.error_message error;
                }))
  with
  | Error _ as error -> error
  | Ok directory ->
      Ok
        (Fun.protect
           ~finally:(fun () ->
             try remove_path directory
             with Unix.Unix_error _ | Sys_error _ -> ())
           (fun () -> action directory))
let print_compiler_output stdout stderr =
  if not (String.equal stdout "") then (
    output_string Stdlib.stdout stdout;
    flush Stdlib.stdout);
  if not (String.equal stderr "") then (
    output_string Stdlib.stderr stderr;
    flush Stdlib.stderr)
let compile_source source on_success =
  match source_toolchain () with
  | Error message -> fail_internal message
  | Ok toolchain -> (
      match
        with_temporary_directory (fun directory ->
          let output_base = Filename.concat directory "source" in
          let output_object = output_base ^ ".cmo" in
          let output_cmt = output_base ^ ".cmt" in
          let compiler_stdout = Filename.concat directory "compiler.stdout" in
          let compiler_stderr = Filename.concat directory "compiler.stderr" in
          let stdout_channel = open_out_bin compiler_stdout in
          let stderr_channel = open_out_bin compiler_stderr in
          let source_directory =
            match Filename.dirname source with
            | "" -> "."
            | directory -> directory
          in
          let ppx_command =
            Printf.sprintf "%s --keep-ghost" (Filename.quote toolchain.ppx)
          in
          let arguments =
            [|
              toolchain.ocamlc;
              "-bin-annot";
              "-I";
              toolchain.ghost_directory;
              "-I";
              source_directory;
              "-ppx";
              ppx_command;
              "-stop-after";
              "typing";
              "-c";
              "-o";
              output_object;
              "-impl";
              source;
            |]
          in
          let process_result =
            try
              let process =
                Unix.create_process_env toolchain.ocamlc arguments
                  (environment_with_compiler_color ())
                  Unix.stdin
                  (Unix.descr_of_out_channel stdout_channel)
                  (Unix.descr_of_out_channel stderr_channel)
              in
              close_out stdout_channel;
              close_out stderr_channel;
              let _, status = Unix.waitpid [] process in
              Ok status
            with
            | Unix.Unix_error (error, function_name, argument) ->
                close_out_noerr stdout_channel;
                close_out_noerr stderr_channel;
                Error
                  (render_message
                     (Unix_failure
                        {
                          function_name;
                          argument;
                          detail = Unix.error_message error;
                        }))
          in
          let stdout = read_file compiler_stdout in
          let stderr = read_file compiler_stderr in
          match process_result with
          | Error message ->
              print_compiler_output stdout stderr;
              fail_internal
                (render_message (Compiler_invocation_failure message))
          | Ok (Unix.WEXITED 0) ->
              print_compiler_output stdout stderr;
              if Sys.file_exists output_cmt then on_success output_cmt
              else fail_internal (render_message Compiler_missing_cmt)
          | Ok status ->
              prerr_endline
                (Verocaml_bin_render.source_compile_error ~source
                   ~process_status:status);
              print_compiler_output stdout stderr;
              2)
      with
      | Ok result -> result
      | Error message ->
          fail_internal
            (render_message (Temporary_storage_failure message)))
(* This classification is evaluated before command parsing and, critically,
   before any input path is opened. *)
let startup_classification = Int_bounds.check_target ()
let verification_exit_code result =
  match Verifier_service.status result with
  | Verified -> 0
  | Counterexample -> 1
  | Inconclusive | Incomplete_source -> 3
let render_service_error error =
  match Verifier_service.error_diagnostic error with
  | Some diagnostic -> render_frontend_error diagnostic
  | None ->
      prerr_endline
        (Verocaml_bin_render.dependency_error
           ~unit_name:(Verifier_service.error_unit_name error)
           ~message:(Verifier_service.error_message error))
let load_inputs options cmt_input =
  let consumer_error diagnostic =
    if options.dependencies = [] then render_frontend_error diagnostic
    else
      prerr_endline
        (Verocaml_bin_render.dependency_error ~unit_name:None
           ~message:(render_message (Consumer_cmt_rejected (cmt_input, diagnostic))))
  in
  let rec load_dependencies loaded = function
    | [] -> Ok (List.rev loaded)
    | filename :: rest -> (
        match Cmt_input.load filename with
        | Ok dependency -> load_dependencies (dependency :: loaded) rest
        | Error diagnostic ->
            prerr_endline
              (Verocaml_bin_render.dependency_error ~unit_name:None
                 ~message:
                   (render_message
                      (Dependency_cmt_rejected (filename, diagnostic))));
            Error ())
  in
  match Cmt_input.load cmt_input with
  | Error diagnostic ->
      consumer_error diagnostic;
      Error ()
  | Ok consumer ->
      Result.map
        (fun dependencies -> (consumer, dependencies))
        (load_dependencies [] options.dependencies)
let emit_result ?source_compilation_cmt options result =
  let exit_code = verification_exit_code result in
  let rendered_sst =
    match options.dump_sst with
    | None -> Ok ""
    | Some _ ->
        let semantic_sst = Verifier_service.semantic_sst result in
        (match source_compilation_cmt with
        | Some private_cmt when exit_code = 0 ->
            render_source_semantic_sst ~private_cmt semantic_sst
        | Some _ | None -> Ok semantic_sst)
  in
  match rendered_sst with
  | Error message -> fail_internal message
  | Ok rendered_sst -> (
      match write_optional_dump "SST" options.dump_sst rendered_sst with
      | Error message -> fail_internal message
      | Ok () -> (
          let write_vir =
            match options.dump_vir with
            | None -> Ok ()
            | Some _ ->
                write_optional_dump "VIR" options.dump_vir
                  (Verifier_service.vir result |> Vir.to_string)
          in
          match write_vir with
          | Error message -> fail_internal message
          | Ok () ->
              Verocaml_bin_render.verification_stderr_lines result
              |> List.iter prerr_endline;
              Verocaml_bin_render.verification_stdout_lines
                ~display_file:options.input result
              |> List.iter print_endline;
              exit_code))
let verify_decoded ?source_compilation_cmt options configuration cmt_input =
  match load_inputs options cmt_input with
  | Error () -> 2
  | Ok (consumer, dependencies) -> (
      let request =
        Verifier_service.request ~configuration ~consumer ~dependencies
      in
      match Verifier_service.verify request with
      | Error error ->
          render_service_error error;
          2
      | Ok result -> emit_result ?source_compilation_cmt options result)
let verify_cmt options configuration cmt_input =
  verify_decoded options configuration cmt_input
let verify_source options configuration private_cmt =
  verify_decoded ~source_compilation_cmt:private_cmt options configuration
    private_cmt
let verify options configuration =
  match validate_distinct_paths options with
  | Error message ->
      prerr_endline (Verocaml_bin_render.cli_error message);
      2
  | Ok () -> (
      match startup_classification with
      | Error diagnostic ->
          render_frontend_error diagnostic;
          2
      | Ok () -> (
          match Filename.extension options.input with
          | ".ml" ->
              compile_source options.input (verify_source options configuration)
          | _ -> verify_cmt options configuration options.input))
let main_force argv =
  match parse argv with
  | Error message ->
      prerr_endline (Verocaml_bin_render.cli_error message);
      2
  | Ok options -> (
      match
        Verifier_service.configuration ~threads:options.threads
          ~timeout_ms:options.timeout_ms ~rlimit:options.rlimit
      with
      | Error configuration_error ->
          prerr_endline
            (Verocaml_bin_render.cli_error
               (Verifier_service.configuration_error_message
                  configuration_error));
          2
      | Ok configuration -> verify options configuration)
let main argv =
  match Array.to_list argv with
  | _ :: "verify-project" :: _ ->
      Verocaml_bin_project_private.main ~startup_classification
        ~default_threads:(fun () -> production_default_threads (Multicore.max_domains ())) argv
  | _ -> main_force argv
