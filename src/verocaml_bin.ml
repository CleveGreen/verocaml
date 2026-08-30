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
let existing_file path =
  try
    if Sys.file_exists path && not (Sys.is_directory path) then Some path
    else None
  with Sys_error _ -> None
let existing_directory path =
  try
    if Sys.file_exists path && Sys.is_directory path then Some path else None
  with Sys_error _ -> None
let compiler_prefix () =
  Config.standard_library |> Filename.dirname |> Filename.dirname
type source_toolchain = {
  ocamlc : string;
  ppx : string;
  ghost_directory : string;
}
let source_toolchain () =
  let executable_directory = executable_directory () in
  let install_prefix = Filename.dirname executable_directory in
  let development_layout =
    Filename.check_suffix Sys.executable_name ".exe"
  in
  let getenv_or name fallback =
    match Sys.getenv_opt name with
    | Some value -> value
    | None -> fallback
  in
  let default_ocamlc =
    Filename.concat (Filename.concat (compiler_prefix ()) "bin") "ocamlc"
  in
  let default_ppx =
    if development_layout then
      Filename.concat
        (Filename.concat executable_directory "..")
        "ppx/vero_ppx.exe"
    else Filename.concat executable_directory "verocaml-ppx"
  in
  let default_ghost_directory =
    if development_layout then
      Filename.concat
        (Filename.concat executable_directory "..")
        "runtime/.vero_ghost.objs/byte"
    else
      Filename.concat
        (Filename.concat (Filename.concat install_prefix "lib") "verocaml")
        "ghost"
  in
  let ocamlc = getenv_or "VEROCAML_OCAMLC" default_ocamlc in
  let ppx =
    match Sys.getenv_opt "VEROCAML_PPX" with
    | Some path -> existing_file path
    | None -> existing_file default_ppx
  in
  let ghost_directory =
    match Sys.getenv_opt "VEROCAML_GHOST_DIR" with
    | Some path -> existing_directory path
    | None -> existing_directory default_ghost_directory
  in
  match (existing_file ocamlc, ppx, ghost_directory) with
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
let compile_source
    (source [@delator.field Fun.id])
    (on_success [@delator.skip]) =
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
  match Verifier_service.error_diagnostic error with
  | Some diagnostic -> render_frontend_error diagnostic
  | None when Verifier_service.error_is_internal error ->
      prerr_endline
        (Verocaml_bin_render.internal_error
           "VeroCaml could not prepare this verification request. This is a verifier bug, not a failed proof. Re-run with DELATOR_LOG=debug to capture diagnostic details.")
  | None ->
      prerr_endline
        (Verocaml_bin_render.dependency_error
           ~unit_name:(Verifier_service.error_unit_name error)
           ~message:(Verifier_service.error_message error))
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
         match Cmt_input.load filename with Ok candidate -> Some candidate | Error _ -> None)
[@@delator.instrument] [@@delator.level trace]
let declares_external_type_specifications
    (candidate : Cmt_input.implementation) =
  let found = ref false in
  let relevant attribute =
    String.equal attribute.Parsetree.attr_name.txt
      "verocaml.external_type_specification"
    || String.equal attribute.attr_name.txt
         "verocaml.internal.external_type_specification.v1"
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      type_declaration =
        (fun self declaration ->
          if List.exists relevant declaration.Typedtree.typ_attributes then
            found := true;
          default.type_declaration self declaration);
    }
  in
  iterator.structure iterator candidate.structure;
  !found
let compiler_argument argument (candidate : Cmt_input.implementation) =
  Array.exists (String.equal argument) candidate.compiler_arguments
let dune_wrapper_support (candidate : Cmt_input.implementation) =
  (not (Cmt_input.retained_preprocessing candidate))
  && Filename.check_suffix candidate.source_file ".ml-gen"
  && compiler_argument "-no-alias-deps" candidate
  && compiler_argument "-nopervasives" candidate
  && compiler_argument "-nostdlib" candidate
let dune_wrapper_member wrapper (candidate : Cmt_input.implementation) =
  String.equal (Filename.dirname wrapper.Cmt_input.filename)
    (Filename.dirname candidate.filename)
  && Array.exists
       (fun (import : Cmt_input.import) ->
         String.equal import.unit_name candidate.unit_name
         && Option.is_none import.crc)
       wrapper.imports
let exact_identity (candidate : Cmt_input.implementation) =
  Option.map (fun crc -> (candidate.unit_name, crc)) candidate.interface_digest
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
let exact_matches unit_name crc candidates =
  List.filter
    (fun (candidate : Cmt_input.implementation) ->
      String.equal candidate.unit_name unit_name
      && candidate.interface_digest = Some crc)
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
                      exact_matches import.unit_name crc explicit
                    in
                    let automatic_matches =
                      exact_matches import.unit_name crc automatic
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
  | Ok consumer -> (
      match load_dependencies [] options.dependencies with
      | Error () -> Error ()
      | Ok explicit -> (
          match deduplicate_explicit explicit with
          | Error ((unit_name, crc), candidates) ->
              report_ambiguity unit_name crc (distinct_artifacts candidates);
              Error ()
          | Ok explicit ->
              let automatic =
                if discover then automatic_candidates consumer else []
              in
              (* A hidden Dune member has no CRC-bearing import edge from its
                 generated wrapper.  Discover the exact CRC graph first, then
                 admit external-type catalogs only for wrappers in that graph.
                 Merely placing a wrapper and member on an ambient [-I] path
                 must not grant specification authority. *)
              (match discover_dependencies consumer explicit automatic with
              | Error () -> Error ()
              | Ok reachable_dependencies ->
                  let reachable_wrappers =
                    List.filter dune_wrapper_support
                      (explicit @ reachable_dependencies)
                  in
                  let implicit_catalogs =
                    List.filter
                      (fun (candidate : Cmt_input.implementation) ->
                        Cmt_input.retained_preprocessing candidate
                        && declares_external_type_specifications candidate
                        && List.exists
                             (fun wrapper ->
                               dune_wrapper_member wrapper candidate)
                             reachable_wrappers)
                      automatic
                  in
                  (match deduplicate_explicit (explicit @ implicit_catalogs) with
                  | Error ((unit_name, crc), candidates) ->
                      report_ambiguity unit_name crc
                        (distinct_artifacts candidates);
                      Error ()
                  | Ok selected ->
                      Result.map
                        (fun discovered -> (consumer, selected @ discovered))
                        (discover_dependencies consumer selected automatic)))))
[@@delator.instrument] [@@delator.level debug]
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
[@@delator.instrument] [@@delator.level info]
let verify_decoded ?source_compilation_cmt options configuration cmt_input =
  match
    load_inputs ~discover:(Option.is_none source_compilation_cmt) options
      cmt_input
  with
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
[@@delator.instrument] [@@delator.level info]
let verify_cmt options configuration cmt_input =
  verify_decoded options configuration cmt_input
let verify_source options configuration private_cmt =
  verify_decoded ~source_compilation_cmt:private_cmt options configuration
    private_cmt

let marked_for_verification (implementation : Cmt_input.implementation) =
  implementation.verification_scope_markers = [ "marked-v1" ]

let imported_by implementations (implementation : Cmt_input.implementation) =
  List.exists
    (fun owner ->
      owner != implementation
      && Array.exists
           (fun (import : Cmt_input.import) ->
             String.equal import.unit_name implementation.unit_name)
           owner.Cmt_input.imports)
    implementations

let load_dune_artifacts artifacts =
  let rec load loaded = function
    | [] -> Ok (List.rev loaded)
    | (artifact : Verocaml_bin_dune_private.artifact) :: rest -> (
        match Cmt_input.load artifact.cmt with
        | Ok implementation ->
            [%log.debug "loaded Dune verification artifact"
              ~cmt:(Delator.Field.string artifact.cmt)
              ~source:(Delator.Field.string artifact.source)
              ~unit_name:(Delator.Field.string implementation.unit_name)
              ~retained:
                (Delator.Field.bool
                   (Cmt_input.retained_preprocessing implementation))
              ~verification_scope_marker_count:
                (Delator.Field.int
                   (List.length implementation.verification_scope_markers))
              ~marked_for_verification:
                (Delator.Field.bool
                   (marked_for_verification implementation))
              ~import_count:
                (Delator.Field.int (Array.length implementation.imports))];
            load ((artifact, implementation) :: loaded) rest
        | Error diagnostic -> Error (artifact.cmt, diagnostic))
  in
  load [] artifacts

let combined_exit_code left right =
  match (left, right) with
  | 2, _ | _, 2 -> 2
  | 3, _ | _, 3 -> 3
  | 1, _ | _, 1 -> 1
  | _ -> 0

let verify_dune_project options configuration =
  match Verocaml_bin_dune_private.build_and_describe options.input with
  | Error message ->
      prerr_endline (Verocaml_bin_render.cli_error message);
      2
  | Ok project -> (
      if options.dependencies <> [] then (
        prerr_endline
          (Verocaml_bin_render.cli_error
             "Dune directory verification discovers dependencies automatically; --dependency is not accepted");
        2)
      else if Option.is_some options.dump_sst || Option.is_some options.dump_vir
      then (
        prerr_endline
          (Verocaml_bin_render.cli_error
             "Dune directory verification does not accept single-file SST or VIR dump paths");
        2)
      else
      match load_dune_artifacts project.artifacts with
      | Error (_, diagnostic) ->
          render_frontend_error diagnostic;
          2
      | Ok loaded ->
          let marked =
            List.filter
              (fun (_, implementation) ->
                marked_for_verification implementation)
              loaded
          in
          let marked_implementations = List.map snd marked in
          let roots =
            List.filter
              (fun (_, implementation) ->
                let imported =
                  imported_by marked_implementations implementation
                in
                [%log.trace "classify marked Dune verification artifact"
                  ~unit_name:
                    (Delator.Field.string implementation.unit_name)
                  ~imported_by_marked_module:(Delator.Field.bool imported)
                  ~selected_as_root:(Delator.Field.bool (not imported))];
                not imported)
              marked
          in
          [%log.info "selected Dune verification roots"
            ~root:(Delator.Field.string project.root)
            ~requested_directory:
              (Delator.Field.string project.requested_directory)
            ~marked_count:(Delator.Field.int (List.length marked))
            ~root_count:(Delator.Field.int (List.length roots))];
          (match roots with
          | [] ->
          prerr_endline
            (Verocaml_bin_render.cli_error
               (Printf.sprintf
                  "Dune built %S but found no [@@@verocaml.verify] modules"
                  project.requested_directory));
          2
          | _ ->
              let exit_code =
                List.fold_left
                  (fun exit_code
                       ((artifact : Verocaml_bin_dune_private.artifact),
                        _implementation) ->
                    let module_options = { options with input = artifact.cmt } in
                    combined_exit_code exit_code
                      (verify_cmt module_options configuration artifact.cmt))
                  0 roots
              in
              print_endline
                (Printf.sprintf
                   "verocaml: project directory=%s roots=%d result=%s"
                   project.requested_directory (List.length roots)
                   (match exit_code with
                   | 0 -> "verified"
                   | 1 -> "counterexample"
                   | 2 -> "rejected"
                   | _ -> "inconclusive"));
              exit_code))
[@@delator.instrument]

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
          if Option.is_some (existing_directory options.input) then
            verify_dune_project options configuration
          else
            match Filename.extension options.input with
            | ".ml" ->
              compile_source options.input (verify_source options configuration)
            | _ -> verify_cmt options configuration options.input))
[@@delator.instrument] [@@delator.level info]
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

let main argv =
  configure_observability ();
  let command =
    if Array.length argv > 1 then argv.(1) else "<missing>"
  in
  Delator.in_span ~level:Delator.Info ~target:__MODULE__ ~name:"command"
    ~fields:(fun () ->
      [
        ("command", Delator.Field.string command);
        ("argument_count", Delator.Field.int (Array.length argv - 1));
      ])
    (fun () ->
      match Array.to_list argv with
      | _ :: "verify-project" :: _ ->
          Verocaml_bin_project_private.main ~startup_classification
            ~default_threads:(fun () ->
              production_default_threads (Multicore.max_domains ()))
            argv
      | _ -> main_force argv)
