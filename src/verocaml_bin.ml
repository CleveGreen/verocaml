open Verocaml_bin_render

type options = {
  input : string;
  dependencies : string list;
  inventory : string option;
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
      let rec extract_inventory inventory reversed = function
        | [] -> Ok (inventory, List.rev reversed)
        | "--inventory" :: filename :: rest when Option.is_none inventory ->
            extract_inventory (Some filename) reversed rest
        | "--inventory" :: _ when Option.is_some inventory ->
            Error (render_message (Option_once "--inventory"))
        | "--inventory" :: [] ->
            Error (render_message (Option_requires ("--inventory", "FILE")))
        | argument :: rest ->
            extract_inventory inventory (argument :: reversed) rest
      in
      let rec loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
          threads threads_seen dump_sst dump_vir solver_seen inventory = function
        | [] ->
            (match resolve_threads threads with
            | Error _ as error -> error
            | Ok threads ->
                Ok
                  {
                    input;
                    dependencies = List.rev dependencies;
                    inventory;
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
                inventory rest
        | "--solver" :: value :: rest ->
            if solver_seen then Error (render_message (Option_once "--solver"))
            else if not (String.equal value "z3") then
              Error (render_message (Unsupported_solver value))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen dump_sst dump_vir true inventory rest
        | "--timeout-ms" :: value :: rest ->
            if timeout_seen then
              Error (render_message (Option_once "--timeout-ms"))
            else (
              match parse_positive_int "--timeout-ms" value with
              | Error _ as error -> error
              | Ok timeout_ms ->
                  loop dependencies timeout_ms true rlimit rlimit_seen threads
                    threads_seen dump_sst dump_vir solver_seen inventory rest)
        | "--rlimit" :: value :: rest ->
            if rlimit_seen then
              Error (render_message (Option_once "--rlimit"))
            else (
              match parse_positive_decimal_int "--rlimit" value with
              | Error _ as error -> error
              | Ok rlimit ->
                  loop dependencies timeout_ms timeout_seen (Some rlimit) true
                    threads threads_seen dump_sst dump_vir solver_seen inventory rest)
        | "--threads" :: value :: rest ->
            if threads_seen then
              Error (render_message (Option_once "--threads"))
            else (
              match parse_positive_decimal_int "--threads" value with
              | Error _ as error -> error
              | Ok threads ->
                  loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                    (Some threads) true dump_sst dump_vir solver_seen inventory rest)
        | "--dump-sst" :: value :: rest ->
            if Option.is_some dump_sst then
              Error (render_message (Option_once "--dump-sst"))
            else if String.starts_with ~prefix:"--" value then
              Error (render_message (Option_requires ("--dump-sst", "FILE")))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen (Some value) dump_vir solver_seen inventory rest
        | "--dump-vir" :: value :: rest ->
            if Option.is_some dump_vir then
              Error (render_message (Option_once "--dump-vir"))
            else if String.starts_with ~prefix:"--" value then
              Error (render_message (Option_requires ("--dump-vir", "FILE")))
            else
              loop dependencies timeout_ms timeout_seen rlimit rlimit_seen
                threads threads_seen dump_sst (Some value) solver_seen inventory rest
        | flag :: _ when String.starts_with ~prefix:"--" flag ->
            Error (render_message (Unknown_option flag))
        | argument :: _ ->
            Error (render_message (Unexpected_argument argument))
      in
      (match extract_inventory None [] arguments with
      | Error _ as error -> error
      | Ok (inventory, arguments) ->
          loop [] default_timeout_ms false None false None false None None false
            inventory arguments)
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
  let environment =
    Unix.environment ()
    |> Array.to_list
    |> List.filter (fun entry ->
           not (String.starts_with ~prefix:"BUILD_PATH_PREFIX_MAP=" entry))
    |> Array.of_list
  in
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
let compile_source ~dependency_directories
    (source [@delator.field Fun.id]) (on_success [@delator.skip]) =
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
            [
              toolchain.ocamlc;
              "-bin-annot";
              "-I";
              toolchain.ghost_directory;
              "-I";
              source_directory;
            ]
            @ List.concat_map
                (fun directory -> [ "-I"; directory ])
                dependency_directories
            @ [
              "-ppx";
              ppx_command;
              "-stop-after";
              "typing";
              "-c";
              "-o";
              output_object;
              "-impl";
              source;
            ]
            |> Array.of_list
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
[@@delator.instrument] [@@delator.level debug]
(* This classification is evaluated before command parsing and, critically,
   before any input path is opened. *)
let startup_classification = Int_bounds.check_target ()
let verification_exit_code result =
  match Verifier_service.status result with
  | Verified -> 0
  | Counterexample -> 1
  | Inconclusive | Incomplete_source -> 3
let render_service_error error =
  match Verifier_service.error_classification error with
  | Verifier_service.Source_error ->
      Option.iter render_frontend_error
        (Verifier_service.error_diagnostic error);
      2
  | Dependency_error ->
      (match Verifier_service.error_diagnostic error with
      | Some diagnostic -> render_frontend_error diagnostic
      | None ->
          prerr_endline
            (Verocaml_bin_render.dependency_error
               ~unit_name:(Verifier_service.error_unit_name error)
               ~message:(Verifier_service.error_message error)));
      2
  | Internal_error -> fail_internal (Verifier_service.error_message error)
let canonical_import_unit = function
  | "CamlinternalFormatBasics" | "Stdlib" | "Stdlib__Domain"
  | "Stdlib__Effect" | "Vero_ghost" ->
      true
  | _ -> false
let custom_import (owner : Cmt_input.implementation)
    (import : Cmt_input.import) =
  not (String.equal owner.unit_name import.unit_name)
  && not (canonical_import_unit import.unit_name)
let adjacent_cmt_files directories =
  let files directory =
    try
      Sys.readdir directory |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun entry ->
             if Filename.check_suffix entry ".cmt" then
               let filename = Filename.concat directory entry in
               if Sys.is_directory filename then None else Some filename
             else None)
    with Sys_error _ -> []
  in
  List.sort_uniq String.compare directories
  |> List.concat_map files |> List.sort_uniq String.compare
let automatic_candidates (consumer : Cmt_input.implementation) =
  adjacent_cmt_files
    (consumer.load_path_visible @ consumer.load_path_hidden)
  |> List.filter_map (fun filename ->
         match Cmt_input.load filename with
         | Ok candidate ->
             [%log.trace "decoded ambient CMT candidate without granting authority"
               ~stage:(Delator.Field.string "automatic-cmt-discovery")
               ~route:(Delator.Field.string "compiler-load-path")
               ~unit_name:(Delator.Field.string candidate.unit_name)
               ~has_interface_identity:
                 (Delator.Field.bool (Option.is_some candidate.interface_digest))
               ~decision:(Delator.Field.string "candidate-only")];
             Some candidate
         | Error _ ->
             [%log.trace "ignored undecodable ambient CMT candidate"
               ~stage:(Delator.Field.string "automatic-cmt-discovery")
               ~route:(Delator.Field.string "compiler-load-path")
               ~decision:(Delator.Field.string "ignored")
               ~reason_class:(Delator.Field.string "artifact-decode")];
             None)
[@@delator.instrument] [@@delator.level debug]
let exact_identity (candidate : Cmt_input.implementation) =
  Option.map
    (fun crc ->
      ( candidate.unit_name,
        crc,
        candidate.retained_authority_receipt,
        candidate.retained_authority_index ))
    candidate.interface_digest
let deduplicate_explicit candidates =
  let rec loop identities loaded = function
    | [] -> Ok (List.rev loaded)
    | candidate :: rest -> (
        match exact_identity candidate with
        | None -> loop identities (candidate :: loaded) rest
        | Some identity -> (
            match List.assoc_opt identity identities with
            | None ->
                loop
                  ((identity, candidate) :: identities)
                  (candidate :: loaded) rest
            | Some existing
              when String.equal existing.raw_artifact_digest
                     candidate.raw_artifact_digest ->
                loop identities loaded rest
            | Some existing -> Error (identity, [ existing; candidate ])))
  in
  loop [] [] candidates
let exact_matches owner import candidates =
  List.filter
    (fun (candidate : Cmt_input.implementation) ->
      Cmt_input.exact_import ~owner
        ~dependency:candidate import)
    candidates
let distinct_artifacts candidates =
  List.sort
    (fun (left : Cmt_input.implementation) right ->
      String.compare left.filename right.filename)
    candidates
  |> List.fold_left
       (fun distinct candidate ->
         if
           List.exists
             (fun existing ->
               String.equal existing.Cmt_input.raw_artifact_digest
                 candidate.Cmt_input.raw_artifact_digest)
             distinct
         then distinct
         else candidate :: distinct)
       []
  |> List.rev
let report_ambiguity unit_name crc candidates =
  let artifacts =
    candidates
    |> List.map (fun (candidate : Cmt_input.implementation) ->
           Printf.sprintf "%S (%s)" candidate.filename
             candidate.raw_artifact_digest)
    |> String.concat ", "
  in
  prerr_endline
    (Verocaml_bin_render.dependency_error ~unit_name:(Some unit_name)
       ~message:
         (Printf.sprintf
            "ambiguous exact interface CRC %s is provided by differing CMT artifacts: %s"
            crc artifacts))
let discover_dependencies (consumer : Cmt_input.implementation) explicit
    automatic =
  let explicit_units =
    List.map (fun (candidate : Cmt_input.implementation) -> candidate.unit_name)
      explicit
    |> List.sort_uniq String.compare
  in
  let rec visit visited discovered = function
    | [] -> Ok (List.rev discovered)
    | owner :: rest ->
        let identity =
          (owner.Cmt_input.unit_name, owner.Cmt_input.raw_artifact_digest)
        in
        if List.mem identity visited then visit visited discovered rest
        else
          let imports =
            Array.to_list owner.imports |> List.filter (custom_import owner)
            |> List.sort (fun (left : Cmt_input.import) right ->
                   match String.compare left.Cmt_input.unit_name right.unit_name with
                   | 0 -> Option.compare String.compare left.crc right.crc
                   | comparison -> comparison)
          in
          let rec select selected = function
            | [] -> Ok selected
            | (import : Cmt_input.import) :: imports -> (
                if String.equal import.unit_name consumer.unit_name then
                  select selected imports
                else
                match import.crc with
                | None -> select selected imports
                | Some crc ->
                    let explicit_matches =
                      exact_matches owner import explicit
                    in
                    let automatic_matches =
                      exact_matches owner import automatic
                    in
                    let artifacts =
                      distinct_artifacts (explicit_matches @ automatic_matches)
                    in
                    (match artifacts with
                    | _ :: _ :: _ ->
                        report_ambiguity import.unit_name crc artifacts;
                        Error ()
                    | [] -> select selected imports
                    | [ candidate ] ->
                        let selected_candidate =
                          match explicit_matches with
                          | [ explicit ] -> Some (false, explicit)
                          | [] when List.mem import.unit_name explicit_units ->
                              None
                          | [] -> Some (true, candidate)
                          | _ -> assert false
                        in
                        select
                          (Option.fold ~none:selected
                             ~some:(fun candidate -> candidate :: selected)
                             selected_candidate)
                          imports))
          in
          (match select [] imports with
          | Error () -> Error ()
          | Ok selected ->
              let discovered =
                List.fold_left
                  (fun discovered (automatic, candidate) ->
                    if
                      automatic
                      && not
                           (List.exists
                              (fun existing ->
                                String.equal existing.Cmt_input.unit_name
                                  candidate.Cmt_input.unit_name
                                && String.equal existing.raw_artifact_digest
                                     candidate.raw_artifact_digest)
                              discovered)
                    then candidate :: discovered
                    else discovered)
                  discovered selected
              in
              visit (identity :: visited) discovered
                (List.rev_map snd selected @ rest))
  in
  visit [] [] (consumer :: explicit)
[@@delator.instrument] [@@delator.level debug]
let load_inputs ~discover options cmt_input =
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
            (match diagnostic.Diagnostic.classification with
            | Invalid_symbolic_dependency _
            | Invalid_broadcast_dependency _ ->
                render_frontend_error diagnostic
            | _ ->
                prerr_endline
                  (Verocaml_bin_render.dependency_error ~unit_name:None
                     ~message:
                       (render_message
                          (Dependency_cmt_rejected (filename, diagnostic)))));
            Error ())
  in
  match Cmt_input.load cmt_input with
  | Error diagnostic ->
      consumer_error diagnostic;
      Error ()
  | Ok consumer -> (
      match load_dependencies [] options.dependencies with
      | Error () -> Error ()
      | Ok explicit -> (
          match deduplicate_explicit explicit with
          | Error ((unit_name, crc, _, _), candidates) ->
              report_ambiguity unit_name crc (distinct_artifacts candidates);
              Error ()
          | Ok explicit ->
              let automatic =
                if discover then automatic_candidates consumer else []
              in
              (match discover_dependencies consumer explicit automatic with
              | Error () -> Error ()
              | Ok reachable_dependencies ->
                  [%log.debug "selected automatic dependencies by exact compiler imports"
                    ~stage:(Delator.Field.string "automatic-cmt-authority")
                    ~route:(Delator.Field.string "compiler-import-graph")
                    ~ambient_candidate_count:
                      (Delator.Field.int (List.length automatic))
                    ~explicit_dependency_count:
                      (Delator.Field.int (List.length explicit))
                    ~exact_reachable_count:
                      (Delator.Field.int (List.length reachable_dependencies))
                    ~decision:(Delator.Field.string "exact-imports-only")];
                  Ok (consumer, explicit @ reachable_dependencies))))
[@@delator.instrument] [@@delator.level debug]
let source_dependency_directories dependencies =
  let[@log_value.debug] complete_receipt
      (implementation : Cmt_input.implementation) =
    Cmt_input.broadcast_complete_receipt implementation
  in
  let rec load loaded directories = function
    | [] -> Ok (List.rev directories)
    | filename :: rest -> (
        match Cmt_input.load filename with
        | Error diagnostic ->
            render_frontend_error diagnostic;
            Error ()
        | Ok dependency ->
            let duplicates =
              List.filter
                (fun existing ->
                  String.equal existing.Cmt_input.unit_name dependency.unit_name)
                loaded
            in
            if duplicates <> [] then (
              let[@log_value.debug] _differing =
                List.exists
                  (fun existing ->
                    not
                      (String.equal
                         ((complete_receipt [@log_value.debug]) existing)
                         ((complete_receipt [@log_value.debug]) dependency)))
                  duplicates
              in
              [%log.debug "rejected duplicate source dependency candidate"
                ~provider:(Delator.Field.string dependency.unit_name)
                ~stage:(Delator.Field.string "source-dependency-preflight")
                ~route:(Delator.Field.string "adjacent-cmi")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:
                  (Delator.Field.string
                     (if (_differing [@log_value.debug]) then
                        "conflicting-complete-receipts"
                      else "duplicate-candidate"))];
              prerr_endline
                (Verocaml_bin_render.dependency_error
                   ~unit_name:(Some dependency.unit_name)
                   ~message:"duplicate dependency candidate is ambiguous");
              Error ())
            else
              match dependency.interface_filename with
              | None ->
                  prerr_endline
                    (Verocaml_bin_render.dependency_error
                       ~unit_name:(Some dependency.unit_name)
                       ~message:
                         "dependency has no authenticated adjacent interface");
                  Error ()
              | Some cmi ->
                  let directory = Filename.dirname cmi in
                  let directories =
                    if List.mem directory directories then directories
                    else directory :: directories
                  in
                  load (dependency :: loaded) directories rest)
  in
  load [] [] dependencies

let explicit_inventory_dependencies filename =
  match Verocaml_bin_inventory_private.read filename with
  | Error message ->
      prerr_endline (Verocaml_bin_render.cli_error message);
      Error ()
  | Ok entries ->
      if
        List.exists
          (fun (entry : Verocaml_bin_inventory_private.entry) ->
            entry.role = Root)
          entries
      then (
        prerr_endline
          (Verocaml_bin_render.cli_error
             "source verification inventory may contain dependency entries only");
        Error ())
      else
        let artifact_directories =
          entries
          |> List.concat_map
               (fun (entry : Verocaml_bin_inventory_private.entry) ->
                 List.map Filename.dirname
                   (entry.cmt :: entry.cmi :: Option.to_list entry.cmti
                  @ Option.to_list entry.vri))
          |> List.sort_uniq String.compare
        in
        let rec load implementations = function
          | [] -> Ok (List.rev implementations, artifact_directories)
          | (entry : Verocaml_bin_inventory_private.entry) :: rest -> (
              match
                Cmt_input.load_with_interface ~cmt:entry.cmt ~cmi:entry.cmi
                  ?cmti:entry.cmti ?vri:entry.vri ~artifact_directories ()
              with
              | Ok implementation -> load (implementation :: implementations) rest
              | Error diagnostic ->
                  render_frontend_error diagnostic;
                  Error ())
        in
        load [] entries
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
let verify_decoded ?source_compilation_cmt ?dependencies options configuration
    cmt_input =
  let loaded =
    match dependencies with
    | None ->
        load_inputs ~discover:(Option.is_none source_compilation_cmt) options
          cmt_input
    | Some dependencies -> (
        match Cmt_input.load cmt_input with
        | Ok consumer -> Ok (consumer, dependencies)
        | Error diagnostic ->
            render_frontend_error diagnostic;
            Error ())
  in
  match loaded with
  | Error () -> 2
  | Ok (consumer, dependencies) -> (
      let request =
        Verifier_service.request ~configuration ~consumer ~dependencies
      in
      match Verifier_service.verify request with
      | Error error -> render_service_error error
      | Ok result -> emit_result ?source_compilation_cmt options result)
[@@delator.instrument] [@@delator.level info]
let verify_cmt options configuration cmt_input =
  verify_decoded options configuration cmt_input
let verify_source ?dependencies options configuration private_cmt =
  verify_decoded ~source_compilation_cmt:private_cmt ?dependencies options
    configuration private_cmt

let marked_for_verification (implementation : Cmt_input.implementation) =
  implementation.verification_scope_markers = [ "marked-v1" ]

let load_dune_artifacts artifacts =
  let artifact_directories =
    artifacts
    |> List.concat_map (fun (artifact : Verocaml_bin_dune_private.artifact) ->
           Filename.dirname artifact.cmt
           :: Option.to_list (Option.map Filename.dirname artifact.cmi)
           @ Option.to_list (Option.map Filename.dirname artifact.cmti)
           @ Option.to_list (Option.map Filename.dirname artifact.vri))
    |> List.sort_uniq String.compare
  in
  let rec load loaded = function
    | [] -> Ok (List.rev loaded)
    | (artifact : Verocaml_bin_dune_private.artifact) :: rest -> (
        match artifact.cmi with
        | None ->
            [%log.debug "skipped incomplete Dune implementation family"
              ~stage:(Delator.Field.string "dune-artifact-load")
              ~route:(Delator.Field.string "directory-verification")
              ~decision:(Delator.Field.string "skipped")
              ~reason_class:(Delator.Field.string "missing-cmi")];
            load loaded rest
        | Some cmi -> (
            match
              Cmt_input.load_with_interface ~cmt:artifact.cmt ~cmi
                ?cmti:artifact.cmti ?vri:artifact.vri ~artifact_directories
                ~implicit_authority_discovery:false ()
            with
            | Ok implementation ->
                [%log.debug "loaded Dune verification authority family"
                  ~unit_name:(Delator.Field.string implementation.unit_name)
                  ~stage:(Delator.Field.string "dune-artifact-load")
                  ~route:(Delator.Field.string "directory-verification")
                  ~requested:(Delator.Field.bool artifact.requested)
                  ~retained:
                    (Delator.Field.bool
                       (Cmt_input.retained_ppx_artifact implementation))
                  ~marked:
                    (Delator.Field.bool
                       (marked_for_verification implementation))];
                load ((artifact, implementation) :: loaded) rest
            | Error diagnostic ->
                [%log.warn "rejected Dune verification authority family"
                  ~stage:(Delator.Field.string "dune-artifact-load")
                  ~route:(Delator.Field.string "directory-verification")
                  ~requested:(Delator.Field.bool artifact.requested)
                  ~decision:(Delator.Field.string "rejected")
                  ~reason_class:(Delator.Field.string "artifact-authentication")];
                Error diagnostic))
  in
  load [] artifacts

let imported_by implementations (implementation : Cmt_input.implementation) =
  List.exists
    (fun owner ->
      owner != implementation
      && Array.exists
           (fun (imported : Cmt_input.import) ->
             String.equal imported.unit_name implementation.unit_name)
           owner.Cmt_input.imports)
    implementations

let dependency_closure
    (implementations : Cmt_input.implementation list)
    (root : Cmt_input.implementation) =
  let matches_import (imported : Cmt_input.import)
      ~(owner : Cmt_input.implementation)
      (implementation : Cmt_input.implementation) =
    Cmt_input.exact_import ~owner
      ~dependency:implementation imported
  in
  let select_dependency owner imported =
    let candidates =
      List.filter (matches_import imported ~owner) implementations
    in
    let retained =
      List.filter
        (fun implementation -> Option.is_some implementation.Cmt_input.retained_authority)
        candidates
    in
    match (retained, candidates) with
    | [ dependency ], _ ->
        [%log.debug "selected retained Dune dependency authority"
          ~stage:(Delator.Field.string "dune-dependency-closure")
          ~route:(Delator.Field.string "directory-verification")
          ~decision:(Delator.Field.string "selected")];
        Ok (Some dependency)
    | [], [] ->
        [%log.trace "ignored compiler import without declared Dune authority"
          ~stage:(Delator.Field.string "dune-dependency-closure")
          ~route:(Delator.Field.string "directory-verification")
          ~decision:(Delator.Field.string "ignored")];
        Ok None
    | [], [ dependency ] ->
        [%log.trace "selected ordinary Dune dependency implementation"
          ~stage:(Delator.Field.string "dune-dependency-closure")
          ~route:(Delator.Field.string "directory-verification")
          ~decision:(Delator.Field.string "selected")];
        Ok (Some dependency)
    | _ ->
        let distinct =
          (match retained with [] -> candidates | _ -> retained)
          |> List.map (fun implementation -> implementation.Cmt_input.raw_artifact_digest)
          |> List.sort_uniq String.compare
        in
        if List.length distinct = 1 then
          let selected =
            List.find_opt
               (fun implementation ->
                 String.equal implementation.Cmt_input.raw_artifact_digest
                   (List.hd distinct))
               (match retained with [] -> candidates | _ -> retained)
          in
          [%log.debug "coalesced equivalent Dune dependency authorities"
            ~stage:(Delator.Field.string "dune-dependency-closure")
            ~route:(Delator.Field.string "directory-verification")
            ~decision:(Delator.Field.string "coalesced")];
          Ok selected
        else (
          [%log.warn "rejected ambiguous Dune dependency authority"
            ~stage:(Delator.Field.string "dune-dependency-closure")
            ~route:(Delator.Field.string "directory-verification")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "ambiguous-exact-import")];
          Error
            (Printf.sprintf
               "Dune dependency %s has multiple retained families for one exact compiler import"
               imported.unit_name))
  in
  let rec visit visiting (visited : Cmt_input.implementation list)
      (implementation : Cmt_input.implementation) =
    if List.mem implementation.Cmt_input.unit_name visiting then (
      [%log.warn "rejected Dune verification dependency cycle"
        ~stage:(Delator.Field.string "dune-dependency-closure")
        ~route:(Delator.Field.string "directory-verification")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "cycle")];
      Error "Dune verification dependency cycle"
    )
    else if
      List.exists
        (fun candidate ->
          String.equal candidate.Cmt_input.raw_artifact_digest
            implementation.Cmt_input.raw_artifact_digest)
        visited
    then Ok visited
    else
      Array.fold_left
        (fun result (imported : Cmt_input.import) ->
          Result.bind result (fun visited ->
              if String.equal imported.unit_name implementation.unit_name then
                Ok visited
              else
                Result.bind (select_dependency implementation imported) (function
                  | None -> Ok visited
                  | Some dependency ->
                      visit (implementation.unit_name :: visiting) visited
                        dependency)))
        (Ok visited) implementation.imports
      |> Result.map (fun visited -> implementation :: visited)
  in
  let specification_providers =
    implementations
    |> List.filter (fun implementation ->
           implementation != root
           && Cmt_input.retained_ppx_artifact implementation
           && Cmt_input.advertises_external_type_specification implementation)
  in
  [%log.debug "selected implicit external type specification providers"
    ~consumer:(Delator.Field.string root.unit_name)
    ~stage:(Delator.Field.string "dune-dependency-closure")
    ~route:(Delator.Field.string "external-type-specification-advertisement")
    ~available_implementation_count:
      (Delator.Field.int (List.length implementations))
    ~provider_count:(Delator.Field.int (List.length specification_providers))
    ~decision:(Delator.Field.string "seeded")];
  let rec seed visited = function
    | [] -> Ok visited
    | provider :: rest ->
        Result.bind (visit [] visited provider) (fun visited ->
            seed visited rest)
  in
  Result.bind (visit [] [] root) (fun dependencies ->
      Result.map
        (fun dependencies ->
          dependencies
          |> List.filter (fun implementation ->
                 not
                   (String.equal implementation.Cmt_input.raw_artifact_digest
                      root.Cmt_input.raw_artifact_digest))
          |> List.sort (fun left right ->
                 String.compare left.Cmt_input.unit_name
                   right.Cmt_input.unit_name))
        (seed dependencies specification_providers))

let combined_exit_code left right =
  match (left, right) with
  | 2, _ | _, 2 -> 2
  | 3, _ | _, 3 -> 3
  | 1, _ | _, 1 -> 1
  | _ -> 0

let verify_dune_project options configuration =
  if options.dependencies <> [] then (
    [%log.warn "rejected manual dependency on Dune directory route"
      ~stage:(Delator.Field.string "dune-route-options")
      ~route:(Delator.Field.string "directory-verification")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "manual-dependency")];
    prerr_endline
      (Verocaml_bin_render.cli_error
         "Dune directory verification discovers dependencies automatically; --dependency is not accepted");
    2)
  else if Option.is_some options.dump_sst || Option.is_some options.dump_vir then (
    [%log.warn "rejected single-file dump on Dune directory route"
      ~stage:(Delator.Field.string "dune-route-options")
      ~route:(Delator.Field.string "directory-verification")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "single-file-dump")];
    prerr_endline
      (Verocaml_bin_render.cli_error
         "Dune directory verification does not accept single-file SST or VIR dump paths");
    2)
  else
    match Verocaml_bin_dune_private.build_and_describe options.input with
    | Error (Verocaml_bin_dune_private.Cli_error message) ->
        prerr_endline (Verocaml_bin_render.cli_error message);
        2
    | Error
        (Verocaml_bin_dune_private.Dependency_error
          { provider; reason_class = _reason_class }) ->
        let[@log_value.warn] reason_class = _reason_class in
        [%log.warn "rejected declared Dune dependency artifact inventory"
          ~stage:(Delator.Field.string "dune-artifact-discovery")
          ~route:(Delator.Field.string "directory-verification")
          ~provider_class:
            (Delator.Field.string
               (if Option.is_some provider then "identified" else "unidentified"))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:
            (Delator.Field.string (reason_class [@log_value.warn]))];
        prerr_endline
          (Verocaml_bin_render.dependency_error ~unit_name:provider
             ~message:
               "declared specification-library artifacts are missing, malformed, or conflicting; rebuild or reinstall the provider and consumer");
        2
    | Ok project -> (
        match load_dune_artifacts project.artifacts with
        | Error diagnostic ->
            if String.equal diagnostic.Diagnostic.code "VERO_DEPENDENCY" then
              let unit_name =
                match diagnostic.classification with
                | Diagnostic.Invalid_broadcast_dependency { provider; _ }
                | Invalid_symbolic_dependency { provider; _ } ->
                    Some provider
                | Invalid_imported_specification _ -> None
                | _ -> None
              in
              prerr_endline
                (Verocaml_bin_render.dependency_error ~unit_name
                   ~message:
                     "declared specification-library artifacts are missing, stale, or conflicting; rebuild or reinstall the provider and consumer")
            else render_frontend_error diagnostic;
            2
        | Ok loaded ->
            let marked =
              loaded
              |> List.filter (fun (artifact, implementation) ->
                     artifact.Verocaml_bin_dune_private.requested
                     && marked_for_verification implementation)
            in
            let implementations = List.map snd loaded in
            let marked_implementations = List.map snd marked in
            let roots =
              marked
              |> List.filter (fun (_, implementation) ->
                     not (imported_by marked_implementations implementation))
            in
            [%log.info "selected Dune verification authority roots"
              ~requested_directory:
                (Delator.Field.string project.requested_directory)
              ~stage:(Delator.Field.string "dune-root-selection")
              ~route:(Delator.Field.string "directory-verification")
              ~marked_count:(Delator.Field.int (List.length marked))
              ~root_count:(Delator.Field.int (List.length roots))
              ~decision:(Delator.Field.string "selected")];
            if roots = [] then (
              [%log.warn "Dune directory contained no requested verification root"
                ~stage:(Delator.Field.string "dune-root-selection")
                ~route:(Delator.Field.string "directory-verification")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "no-marked-root")];
              prerr_endline
                (Verocaml_bin_render.cli_error
                   (Printf.sprintf
                      "Dune built %S but found no [@@@verocaml.verify] modules"
                      project.requested_directory));
              2)
            else
              let exit_code =
                List.fold_left
                  (fun exit_code
                       ((artifact : Verocaml_bin_dune_private.artifact), consumer) ->
                    match dependency_closure implementations consumer with
                    | Error message ->
                        [%log.warn "rejected Dune verification dependency closure"
                          ~stage:(Delator.Field.string "dune-dependency-closure")
                          ~route:(Delator.Field.string "directory-verification")
                          ~decision:(Delator.Field.string "rejected")
                          ~reason_class:(Delator.Field.string "closure-error")];
                        prerr_endline (Verocaml_bin_render.cli_error message);
                        combined_exit_code exit_code 2
                    | Ok dependencies -> (
                        match
                          Verifier_service.verify
                            (Verifier_service.request ~configuration ~consumer
                               ~dependencies)
                        with
                        | Error error ->
                            let rendered = render_service_error error in
                            (match Verifier_service.error_classification error with
                            | Verifier_service.Dependency_error ->
                                [%log.warn
                                  "Dune verification dependency lacked declared retained transport"
                                  ~stage:
                                    (Delator.Field.string
                                       "dune-dependency-closure")
                                  ~route:
                                    (Delator.Field.string
                                       "directory-verification")
                                  ~decision:
                                    (Delator.Field.string "rejected")
                                  ~reason_class:
                                    (Delator.Field.string
                                       "missing-declared-transport")];
                                ()
                            | Source_error | Internal_error -> ());
                            combined_exit_code exit_code rendered
                        | Ok result ->
                            combined_exit_code exit_code
                              (emit_result { options with input = artifact.cmt }
                                 result)))
                  0 roots
              in
              print_endline
                (Printf.sprintf "verocaml: project directory=%s roots=%d result=%s"
                   project.requested_directory (List.length roots)
                   (match exit_code with
                   | 0 -> "verified"
                   | 1 -> "counterexample"
                   | 2 -> "rejected"
                   | _ -> "inconclusive"));
              exit_code)
[@@delator.instrument] [@@delator.level info]

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
          if
            try Sys.file_exists options.input && Sys.is_directory options.input
            with Sys_error _ -> false
          then
            if Option.is_some options.inventory then (
              prerr_endline
                (Verocaml_bin_render.cli_error
                   "Dune directory verification already owns its generated inventory; --inventory is not accepted");
              2)
            else verify_dune_project options configuration
          else
            match Filename.extension options.input with
            | ".ml" ->
                (match options.inventory with
                | Some _ when options.dependencies <> [] ->
                    prerr_endline
                      (Verocaml_bin_render.cli_error
                         "source verification accepts either --inventory or --dependency, not both");
                    2
                | Some filename -> (
                    match explicit_inventory_dependencies filename with
                    | Error () -> 2
                    | Ok (dependencies, dependency_directories) ->
                        compile_source ~dependency_directories options.input
                          (verify_source ~dependencies options configuration))
                | None -> (
                    match source_dependency_directories options.dependencies with
                    | Error () -> 2
                    | Ok dependency_directories ->
                        compile_source ~dependency_directories options.input
                          (verify_source options configuration)))
            | _ ->
                if Option.is_some options.inventory then (
                  prerr_endline
                    (Verocaml_bin_render.cli_error
                       "use verify-project --inventory for retained CMT project verification");
                  2)
                else verify_cmt options configuration options.input))
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
[@@delator.instrument] [@@delator.level info]

let configure_observability () =
  Delator.init ();
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> Delator.set_default_level Delator.Warn
  | Some _ -> ()

let main_configured argv =
  let[@log_value.info] command =
    if Array.length argv > 1 then argv.(1) else "<missing>"
  in
  [%log.info "dispatching command"
    ~command:(Delator.Field.string (command [@log_value.info]))
    ~argument_count:(Delator.Field.int (Array.length argv - 1))];
  match Array.to_list argv with
  | _ :: "verify-project" :: _ ->
      Verocaml_bin_project_private.main ~startup_classification
        ~default_threads:(fun () ->
          production_default_threads (Multicore.max_domains ()))
        argv
  | _ -> main_force argv
[@@delator.instrument] [@@delator.level info]

let main argv =
  configure_observability ();
  main_configured argv
