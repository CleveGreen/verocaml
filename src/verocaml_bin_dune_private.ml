type artifact = {
  source : string;
  cmt : string;
  cmi : string option;
  cmti : string option;
  vri : string option;
  requested : bool;
}

type project = {
  root : string;
  requested_directory : string;
  artifacts : artifact list;
}

type error =
  | Cli_error of string
  | Dependency_error of {
      provider : string option;
      reason_class : string;
    }

type sexp = Atom of string | List of sexp list

type component = {
  name : string option;
  uid : string option;
  requires : string list;
  directories : string list;
  artifacts : artifact list;
}

let manifest_suffix = ".verocaml-retained-interface"
let manifest_magic = "verocaml-retained-interface-manifest-v1"

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let cli_result result = Result.map_error (fun message -> Cli_error message) result

let manifest_provider filename =
  let basename = Filename.basename filename in
  if Filename.check_suffix basename manifest_suffix then
    Some
      (String.sub basename 0
         (String.length basename - String.length manifest_suffix))
  else None

let dependency_error ?provider reason_class =
  Error (Dependency_error { provider; reason_class })

let dependency_result reason_class result =
  Result.map_error
    (fun _ -> Dependency_error { provider = None; reason_class })
    result

let existing_directory path =
  try Sys.file_exists path && Sys.is_directory path with Sys_error _ -> false

let existing_file path =
  try Sys.file_exists path && not (Sys.is_directory path) with Sys_error _ -> false

let absolute_from_root ~root path =
  if Filename.is_relative path then Filename.concat root path else path

let artifact_of_cmt ~source ~requested cmt =
  let base = Filename.remove_extension cmt in
  let optional extension =
    let filename = base ^ extension in
    if existing_file filename then Some filename else None
  in
  {
    source;
    cmt;
    cmi = optional ".cmi";
    cmti = optional ".cmti";
    vri = None;
    requested;
  }

let rec project_root ~requested directory =
  [%log.trace "inspect Dune root candidate"
    ~directory:(Delator.Field.string directory)];
  if existing_file (Filename.concat directory "dune-project") then Ok directory
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then (
      [%log.warn "rejected Dune directory without project root"
        ~stage:(Delator.Field.string "project-root")
        ~route:(Delator.Field.string "directory-verification")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "missing-dune-project")];
      Error (Printf.sprintf "could not find dune-project at or above %S" requested))
    else project_root ~requested parent
[@@delator.instrument] [@@delator.level debug]

let executable_on_path executable =
  let runnable path =
    try
      Unix.access path [ Unix.X_OK ];
      Some path
    with Unix.Unix_error _ -> None
  in
  if String.contains executable Filename.dir_sep.[0] then
    Option.value (runnable executable) ~default:executable
  else
    Sys.getenv_opt "PATH" |> Option.value ~default:""
    |> String.split_on_char ':'
    |> List.find_map (fun directory ->
           let directory = if String.equal directory "" then "." else directory in
           runnable (Filename.concat directory executable))
    |> Option.value ~default:executable

let dune_executable () =
  match Sys.getenv_opt "VEROCAML_DUNE" with
  | Some path when not (String.equal path "") -> executable_on_path path
  | Some _ | None -> executable_on_path "dune"

let dune_profile () =
  match Sys.getenv_opt "VEROCAML_DUNE_PROFILE" with
  | Some profile when profile <> "" -> profile
  | Some _ | None -> "release"

let environment () =
  let variable = "VEROCAML_INTERNAL_PPX_MODE" in
  let prefix = variable ^ "=" in
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry -> not (String.starts_with ~prefix entry))
  |> fun entries -> Array.of_list ((prefix ^ "retained") :: entries)

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

let run arguments =
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
      let[@log_value.debug] shown_arguments =
        arguments
        |> List.filteri (fun index _ -> index < 16)
        |> List.map Delator.Field.string
      in
      let[@log_value.debug] dropped_arguments =
        Int.max 0 (List.length arguments - 16)
      in
      [%log.debug "invoke Dune"
        ~stage:(Delator.Field.string "dune-command")
        ~route:(Delator.Field.string "directory-verification")
        ~executable:(Delator.Field.string executable)
        ~arguments:
          (Delator.Field.seq ~dropped:(dropped_arguments [@log_value.debug])
             (shown_arguments [@log_value.debug]))
        ~decision:(Delator.Field.string "invoke")];
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
      | Error message ->
          [%log.warn "failed to invoke Dune"
            ~stage:(Delator.Field.string "dune-command")
            ~route:(Delator.Field.string "directory-verification")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "process-create")];
          Error message
      | Ok process ->
          let _, status = Unix.waitpid [] process in
          let stdout = read_file stdout_file in
          let stderr = read_file stderr_file in
          (match status with
          | Unix.WEXITED 0 ->
              [%log.debug "Dune command completed"
                ~stage:(Delator.Field.string "dune-command")
                ~route:(Delator.Field.string "directory-verification")
                ~decision:(Delator.Field.string "accepted")
                ~stdout_bytes:(Delator.Field.int (String.length stdout))
                ~stderr_bytes:(Delator.Field.int (String.length stderr))];
              Ok (stdout, stderr)
          | _ ->
              let detail = String.trim stderr in
              let detail =
                if String.equal detail "" then String.trim stdout else detail
              in
              let[@log_value.warn] bounded_output_excerpt text =
                let edge = 8192 in
                let length = String.length text in
                if length <= edge * 2 then text
                else
                  String.concat ""
                    [
                      String.sub text 0 edge;
                      Printf.sprintf "\n... <%d bytes omitted> ...\n"
                        (length - (edge * 2));
                      String.sub text (length - edge) edge;
                    ]
              in
              let[@log_value.warn] stdout_excerpt =
                (bounded_output_excerpt [@log_value.warn]) stdout
              in
              let[@log_value.warn] stderr_excerpt =
                (bounded_output_excerpt [@log_value.warn]) stderr
              in
              [%log.trace "captured failed Dune command output"
                ~stage:(Delator.Field.string "dune-command")
                ~route:(Delator.Field.string "directory-verification")
                ~status:(Delator.Field.string (process_status status))
                ~stdout:(Delator.Field.string stdout)
                ~stderr:(Delator.Field.string stderr)];
              [%log.warn "Dune command failed"
                ~stage:(Delator.Field.string "dune-command")
                ~route:(Delator.Field.string "directory-verification")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "nonzero-exit")
                ~status:(Delator.Field.string (process_status status))
                ~stdout_bytes:(Delator.Field.int (String.length stdout))
                ~stderr_bytes:(Delator.Field.int (String.length stderr))
                ~stdout_excerpt:
                  (Delator.Field.string
                     (stdout_excerpt [@log_value.warn]))
                ~stderr_excerpt:
                  (Delator.Field.string
                     (stderr_excerpt [@log_value.warn]))];
              Error
                (Printf.sprintf "dune %s failed with %s%s"
                   (String.concat " " arguments)
                   (process_status status)
                   (if String.equal detail "" then "" else ":\n" ^ detail))))
[@@delator.instrument] [@@delator.level debug]

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
            (Printf.sprintf "unexpected byte %C in canonical S-expression"
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
[@@delator.instrument] [@@delator.level trace]

let parse_sexps input =
  let length = String.length input in
  let rec whitespace index =
    if index >= length then index
    else
      match String.unsafe_get input index with
      | ' ' | '\t' | '\r' | '\n' -> whitespace (index + 1)
      | ';' -> comment (index + 1)
      | _ -> index
  and comment index =
    if index >= length then index
    else if Char.equal (String.unsafe_get input index) '\n' then
      whitespace (index + 1)
    else comment (index + 1)
  and expression index =
    let index = whitespace index in
    if index >= length then Error "unexpected end of S-expression"
    else
      match String.unsafe_get input index with
      | '(' -> list (index + 1) []
      | ')' -> Error "unexpected ')' in S-expression"
      | '"' -> quoted (index + 1) (Buffer.create 32)
      | _ -> atom index index
  and list index reversed =
    let index = whitespace index in
    if index >= length then Error "unterminated S-expression list"
    else if Char.equal (String.unsafe_get input index) ')' then
      Ok (List (List.rev reversed), index + 1)
    else
      let* item, next = expression index in
      list next (item :: reversed)
  and quoted index buffer =
    if index >= length then Error "unterminated quoted S-expression atom"
    else
      match String.unsafe_get input index with
      | '"' -> Ok (Atom (Buffer.contents buffer), index + 1)
      | '\\' when index + 1 >= length ->
          Error "unterminated quoted S-expression escape"
      | '\\' ->
          let escaped = String.unsafe_get input (index + 1) in
          (match escaped with
          | '\\' | '"' as decoded ->
              Buffer.add_char buffer decoded;
              quoted (index + 2) buffer
          | 'n' ->
              Buffer.add_char buffer '\n';
              quoted (index + 2) buffer
          | 'r' ->
              Buffer.add_char buffer '\r';
              quoted (index + 2) buffer
          | 't' ->
              Buffer.add_char buffer '\t';
              quoted (index + 2) buffer
          | _ -> Error "unsupported quoted S-expression escape")
      | character ->
          Buffer.add_char buffer character;
          quoted (index + 1) buffer
  and atom start index =
    if index >= length then
      Ok (Atom (String.sub input start (index - start)), index)
    else
      match String.unsafe_get input index with
      | ' ' | '\t' | '\r' | '\n' | '(' | ')' | ';' ->
          if index = start then Error "empty S-expression atom"
          else Ok (Atom (String.sub input start (index - start)), index)
      | _ -> atom start (index + 1)
  in
  let rec expressions reversed index =
    let index = whitespace index in
    if index >= length then Ok (List.rev reversed)
    else
      let* value, next = expression index in
      expressions (value :: reversed) next
  in
  expressions [] 0
[@@delator.instrument] [@@delator.level trace]

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

let atom_list_field name fields =
  match field name fields with
  | Some (List values) ->
      List.filter_map (function Atom value -> Some value | List _ -> None) values
  | Some (Atom _) | None -> []

let rec described_artifacts sexp =
  match sexp with
  | Atom _ -> []
  | List fields ->
      let here =
        match
          (optional_atom_field "impl" fields, optional_atom_field "cmt" fields)
        with
        | Some source, Some cmt
          when not (String.equal source "") && not (String.equal cmt "") ->
            [
              {
                source;
                cmt;
                cmi = None;
                cmti = None;
                vri = None;
                requested = false;
              };
            ]
        | Some _, Some _ | None, _ | _, None -> []
      in
      here @ List.concat_map described_artifacts fields

let component ~root fields =
  let artifacts = described_artifacts (List fields) in
  let directories =
    Option.to_list (atom_field "source_dir" fields)
    @ atom_list_field "include_dirs" fields
    @ List.map (fun artifact -> Filename.dirname artifact.source) artifacts
    |> List.map (absolute_from_root ~root)
    |> List.sort_uniq String.compare
  in
  {
    name = atom_field "name" fields;
    uid = atom_field "uid" fields;
    requires = atom_list_field "requires" fields;
    directories;
    artifacts;
  }

let workspace_components ~root = function
  | Atom _ -> []
  | List fields ->
      List.filter_map
        (function
          | List [ Atom "library"; List component_fields ]
          | List [ Atom "executables"; List component_fields ] ->
              Some (component ~root component_fields)
          | Atom _ | List _ -> None)
        fields

let relative_to ~root path =
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  if String.equal path root then ""
  else if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let source_path ~root ~build_context source =
  let source = absolute_from_root ~root source in
  let build_context = absolute_from_root ~root build_context in
  let build_prefix =
    if String.ends_with ~suffix:"/" build_context then build_context
    else build_context ^ "/"
  in
  let relative =
    if String.starts_with ~prefix:build_prefix source then
      String.sub source (String.length build_prefix)
        (String.length source - String.length build_prefix)
    else relative_to ~root source
  in
  absolute_from_root ~root relative

let within_directory ~directory path =
  String.equal path directory
  || String.starts_with ~prefix:(directory ^ "/") path

let build_target ~root ~requested_directory =
  if not (within_directory ~directory:root requested_directory) then (
    [%log.warn "rejected requested directory outside its Dune project root"
      ~stage:(Delator.Field.string "dune-build-target")
      ~route:(Delator.Field.string "directory-verification")
      ~project_root:(Delator.Field.string root)
      ~requested_directory:(Delator.Field.string requested_directory)
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "outside-project-root")];
    Error
      (Printf.sprintf "requested directory %S is outside Dune project root %S"
         requested_directory root))
  else
    let relative = relative_to ~root requested_directory in
    let target =
      if String.equal relative "" then "@all" else "@" ^ relative ^ "/all"
    in
    [%log.debug "selected Dune build target for requested verification scope"
      ~stage:(Delator.Field.string "dune-build-target")
      ~route:(Delator.Field.string "directory-verification")
      ~project_root:(Delator.Field.string root)
      ~requested_directory:(Delator.Field.string requested_directory)
      ~relative_directory:(Delator.Field.string relative)
      ~build_target:(Delator.Field.string target)
      ~decision:(Delator.Field.string "accepted")];
    Ok target
[@@delator.instrument] [@@delator.level debug]

let verification_build_directory root =
  Filename.concat root (Filename.concat "_build" "verocaml-retained-v1")

let ensure_build_parent build_directory =
  let parent = Filename.dirname build_directory in
  if existing_directory parent then Ok ()
  else if Sys.file_exists parent then (
    [%log.warn "rejected non-directory verification build parent"
      ~stage:(Delator.Field.string "dune-build-directory")
      ~route:(Delator.Field.string "directory-verification")
      ~build_parent:(Delator.Field.string parent)
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "non-directory")];
    Error (Printf.sprintf "Dune build parent %S is not a directory" parent))
  else
    try
      Unix.mkdir parent 0o755;
      [%log.debug "created verification build parent"
        ~stage:(Delator.Field.string "dune-build-directory")
        ~route:(Delator.Field.string "directory-verification")
        ~build_parent:(Delator.Field.string parent)
        ~decision:(Delator.Field.string "created")];
      Ok ()
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when existing_directory parent ->
        [%log.debug "accepted concurrently created verification build parent"
          ~stage:(Delator.Field.string "dune-build-directory")
          ~route:(Delator.Field.string "directory-verification")
          ~build_parent:(Delator.Field.string parent)
          ~decision:(Delator.Field.string "accepted")];
        Ok ()
    | Unix.Unix_error (error, function_name, argument) ->
        [%log.warn "failed to create verification build parent"
          ~stage:(Delator.Field.string "dune-build-directory")
          ~route:(Delator.Field.string "directory-verification")
          ~build_parent:(Delator.Field.string parent)
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "mkdir")];
        Error
          (Printf.sprintf "%s(%s): %s" function_name argument
             (Unix.error_message error))
[@@delator.instrument] [@@delator.level debug]

let deduplicate_artifacts artifacts =
  List.sort (fun left right -> String.compare left.cmt right.cmt) artifacts
  |> List.fold_left
       (fun unique artifact ->
         match unique with
         | previous :: _ when String.equal previous.cmt artifact.cmt -> unique
         | _ -> artifact :: unique)
       []
  |> List.rev

let requested_components ~root ~requested_directory ~build_context components =
  List.map
    (fun component ->
      let artifacts =
        component.artifacts
        |> List.map (fun artifact ->
               let source = source_path ~root ~build_context artifact.source in
               let cmt = absolute_from_root ~root artifact.cmt in
               let requested = within_directory ~directory:requested_directory source in
               artifact_of_cmt ~source ~requested cmt)
        |> deduplicate_artifacts
      in
      ({ component with artifacts }, List.exists (fun artifact -> artifact.requested) artifacts))
    components

let reachable_components components =
  let roots =
    List.filter_map (fun (component, requested) -> if requested then Some component else None)
      components
  and by_uid =
    List.filter_map
      (fun (component, _) -> Option.map (fun uid -> (uid, component)) component.uid)
      components
  in
  let rec visit visited selected = function
    | [] -> Ok (List.rev selected)
    | component :: rest ->
        let identity =
          Option.value component.uid
            ~default:
              (String.concat "|"
                 (component.requires @ component.directories
                @ List.map (fun artifact -> artifact.cmt) component.artifacts))
        in
        if List.mem identity visited then visit visited selected rest
        else
          let rec dependencies reversed = function
            | [] -> Ok (List.rev reversed)
            | uid :: uids -> (
                match List.assoc_opt uid by_uid with
                | Some dependency -> dependencies (dependency :: reversed) uids
                | None ->
                    [%log.warn "Dune description omitted a required library"
                      ~stage:(Delator.Field.string "workspace-dependency-graph")
                      ~route:(Delator.Field.string "directory-verification")
                      ~decision:(Delator.Field.string "rejected")
                      ~reason_class:(Delator.Field.string "missing-library-uid")];
                    Error
                      "Dune workspace description 0.1 omitted a required library record")
          in
          let* dependencies = dependencies [] component.requires in
          visit (identity :: visited) (component :: selected)
            (dependencies @ rest)
  in
  visit [] [] roots
[@@delator.instrument] [@@delator.level debug]

let safe_manifest_path value =
  value <> "" && Filename.is_relative value
  && value |> String.split_on_char '/'
     |> List.for_all (fun component ->
            component <> "" && component <> "." && component <> "..")

let cmi_self_crc interface =
  Array.to_list interface.Cmi_format.cmi_crcs
  |> List.filter (fun imported ->
         Compilation_unit.Name.equal (Import_info.name imported)
           interface.Cmi_format.cmi_name)
  |> function
  | [ imported ] -> Option.map Digest.to_hex (Import_info.crc imported)
  | [] | _ :: _ :: _ -> None

let exact_imported_units components requested_artifacts =
  let directories =
    components |> List.concat_map (fun component -> component.directories)
    |> List.sort_uniq String.compare
  in
  let imports filename =
    try
      let interface = Cmi_format.read_cmi_lazy filename in
      Ok
        (Array.to_list interface.Cmi_format.cmi_crcs
        |> List.filter_map (fun imported ->
               if
                 Compilation_unit.Name.equal (Import_info.name imported)
                   interface.Cmi_format.cmi_name
               then None
               else
                 Option.map
                   (fun crc ->
                     ( Compilation_unit.Name.to_string (Import_info.name imported),
                       Digest.to_hex crc ))
                   (Import_info.crc imported)))
    with Cmi_format.Error _ | End_of_file | Sys_error _ -> Ok []
  in
  let exact_cmi unit_name expected_crc =
    let basenames =
      [ unit_name ^ ".cmi"; String.uncapitalize_ascii unit_name ^ ".cmi" ]
      |> List.sort_uniq String.compare
    in
    let matches =
      directories
      |> List.concat_map (fun directory ->
             List.map (Filename.concat directory) basenames)
      |> List.sort_uniq String.compare |> List.filter existing_file
      |> List.filter_map (fun filename ->
             try
               let interface = Cmi_format.read_cmi_lazy filename in
               if
                 Compilation_unit.Name.equal interface.Cmi_format.cmi_name
                   (Compilation_unit.Name.of_string unit_name)
                 && cmi_self_crc interface = Some expected_crc
               then Some (filename, Digest.file filename |> Digest.to_hex)
               else None
             with Cmi_format.Error _ | End_of_file | Sys_error _ -> None)
    in
    match matches with
    | [] ->
        [%log.trace "exact Dune import has no readable CMI in reachable directories"
          ~unit_name:(Delator.Field.string unit_name)
          ~stage:(Delator.Field.string "retained-manifest-reachability")
          ~route:(Delator.Field.string "compiler-cmi-import")
          ~decision:(Delator.Field.string "unavailable")
          ~reason_class:(Delator.Field.string "missing-exact-cmi")];
        Ok None
    | (filename, digest) :: rest ->
        if List.for_all (fun (_, candidate) -> String.equal candidate digest) rest
        then (
          [%log.trace "resolved exact Dune import CMI for manifest reachability"
            ~unit_name:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "retained-manifest-reachability")
            ~route:(Delator.Field.string "compiler-cmi-import")
            ~candidate_count:(Delator.Field.int (List.length matches))
            ~decision:(Delator.Field.string "accepted")];
          Ok (Some filename))
        else (
          [%log.warn "rejected conflicting exact Dune import CMIs"
            ~unit_name:(Delator.Field.string unit_name)
            ~stage:(Delator.Field.string "retained-manifest-reachability")
            ~route:(Delator.Field.string "compiler-cmi-import")
            ~candidate_count:(Delator.Field.int (List.length matches))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "conflicting-exact-import")];
          Error
            (Printf.sprintf
               "Dune dependency %s has conflicting CMIs for one exact compiler import"
               unit_name))
  in
  let rec visit visited units = function
    | [] -> Ok (List.sort_uniq String.compare units)
    | (unit_name, expected_crc) :: rest ->
        if List.mem (unit_name, expected_crc) visited then
          visit visited units rest
        else
          let* imported_cmi = exact_cmi unit_name expected_crc in
          let* nested =
            match imported_cmi with None -> Ok [] | Some filename -> imports filename
          in
          visit ((unit_name, expected_crc) :: visited) (unit_name :: units)
            (nested @ rest)
  in
  let seed_cmis =
    requested_artifacts |> List.filter_map (fun artifact -> artifact.cmi)
  in
  let rec seed imports_to_visit = function
    | [] -> Ok imports_to_visit
    | filename :: rest ->
        let* imported = imports filename in
        seed (imported @ imports_to_visit) rest
  in
  let* imports_to_visit = seed [] seed_cmis in
  visit [] [] imports_to_visit
[@@delator.instrument] [@@delator.level debug]

let manifest_filenames components requested_artifacts component_artifacts =
  let* imported_units = exact_imported_units components requested_artifacts in
  let directories =
    components |> List.concat_map (fun component -> component.directories)
    |> List.sort_uniq String.compare
  and artifact_units =
    component_artifacts
    |> List.map (fun artifact ->
           Filename.basename artifact.cmt |> Filename.remove_extension)
  in
  let manifest_basenames =
    artifact_units @ List.map String.uncapitalize_ascii imported_units
    |> List.sort_uniq String.compare
    |> List.map (fun stem -> stem ^ manifest_suffix)
  in
  let candidates =
    directories
    |> List.concat_map (fun directory ->
           List.map (Filename.concat directory) manifest_basenames)
    |> List.sort_uniq String.compare
  in
  let selected = List.filter existing_file candidates in
  let unmanifested_vris =
    candidates
    |> List.filter_map (fun manifest ->
           let stem = Filename.remove_extension manifest in
           let vri = stem ^ ".vri" in
           if existing_file vri && not (existing_file manifest) then Some vri
           else None)
  in
  [%log.debug "selected retained manifests for exact reachable Dune artifacts"
    ~stage:(Delator.Field.string "retained-manifest-discovery")
    ~route:(Delator.Field.string "dune-dependency-graph")
    ~reachable_component_count:(Delator.Field.int (List.length components))
    ~exact_imported_unit_count:
      (Delator.Field.int (List.length imported_units))
    ~exact_candidate_count:(Delator.Field.int (List.length candidates))
    ~selected_manifest_count:(Delator.Field.int (List.length selected))
    ~decision:(Delator.Field.string "exact-artifacts-only")];
  match unmanifested_vris with
  | [] -> Ok selected
  | _ :: _ ->
      [%log.warn "rejected retained authority without a declared manifest"
        ~stage:(Delator.Field.string "retained-manifest-discovery")
        ~route:(Delator.Field.string "dune-dependency-graph")
        ~undeclared_vri_count:
          (Delator.Field.int (List.length unmanifested_vris))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "missing-declared-manifest")];
      Error "retained authority exists without its declared manifest"
[@@delator.instrument] [@@delator.level debug]

let manifest_artifact filename =
  try
    let lines = read_file filename |> String.split_on_char '\n' in
    match lines with
    | [ magic; cmt; cmi; cmti; vri; "" ] when String.equal magic manifest_magic ->
        let paths = [ cmt; cmi; cmti; vri ] in
        if not (List.for_all safe_manifest_path paths) then (
          [%log.warn "rejected retained-interface manifest path"
            ~stage:(Delator.Field.string "retained-manifest-load")
            ~route:(Delator.Field.string "dune-dependency-graph")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "escaping-or-absolute-path")];
          dependency_error ?provider:(manifest_provider filename)
            "unsafe-declared-path")
        else
          let directory = Filename.dirname filename in
          let resolve path = Filename.concat directory path in
          let cmt, cmi, cmti, vri =
            (resolve cmt, resolve cmi, resolve cmti, resolve vri)
          in
          let missing =
            List.find_opt (fun path -> not (existing_file path))
              [ cmt; cmi; cmti; vri ]
          in
          (match missing with
          | Some _ ->
              [%log.warn "rejected incomplete retained-interface manifest"
                ~stage:(Delator.Field.string "retained-manifest-load")
                ~route:(Delator.Field.string "dune-dependency-graph")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "missing-declared-artifact")];
              dependency_error ?provider:(manifest_provider filename)
                "missing-declared-artifact"
          | None ->
              [%log.debug "selected declared retained-interface manifest"
                ~stage:(Delator.Field.string "retained-manifest-load")
                ~route:(Delator.Field.string "dune-dependency-graph")
                ~decision:(Delator.Field.string "selected")
                ~validation_boundary:(Delator.Field.string "artifact-load")];
              Ok
                {
                  source = filename;
                  cmt;
                  cmi = Some cmi;
                  cmti = Some cmti;
                  vri = Some vri;
                  requested = false;
                })
    | _ ->
        [%log.warn "rejected malformed retained-interface manifest"
          ~stage:(Delator.Field.string "retained-manifest-load")
          ~route:(Delator.Field.string "dune-dependency-graph")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "format-or-version")];
        dependency_error ?provider:(manifest_provider filename)
          "malformed-or-unsupported-manifest"
  with Sys_error message | Unix.Unix_error (_, _, message) ->
    [%log.warn "failed to read retained-interface manifest"
      ~stage:(Delator.Field.string "retained-manifest-load")
      ~route:(Delator.Field.string "dune-dependency-graph")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "manifest-io")];
    ignore message;
    dependency_error ?provider:(manifest_provider filename) "manifest-io"
[@@delator.instrument] [@@delator.level debug]

let load_manifests filenames =
  let rec load artifacts = function
    | [] -> Ok (List.rev artifacts)
    | filename :: rest ->
        let* artifact = manifest_artifact filename in
        (match
           List.find_opt (fun existing -> String.equal existing.cmt artifact.cmt)
             artifacts
         with
        | None -> load (artifact :: artifacts) rest
        | Some existing
          when existing.cmi = artifact.cmi && existing.cmti = artifact.cmti
               && existing.vri = artifact.vri ->
            [%log.debug "coalesced duplicate declared retained-interface manifest"
              ~stage:(Delator.Field.string "retained-manifest-load")
              ~route:(Delator.Field.string "dune-dependency-graph")
              ~decision:(Delator.Field.string "coalesced")];
            load artifacts rest
        | Some _ ->
            [%log.warn "rejected conflicting retained-interface manifests"
              ~stage:(Delator.Field.string "retained-manifest-load")
              ~route:(Delator.Field.string "dune-dependency-graph")
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "conflicting-family")];
            dependency_error ?provider:(manifest_provider artifact.source)
              "conflicting-declared-artifacts")
  in
  load [] filenames
[@@delator.instrument] [@@delator.level debug]

let merge_manifest_artifacts requested manifests =
  let requested =
    List.map
      (fun artifact ->
        match
          List.find_opt (fun manifest -> String.equal manifest.cmt artifact.cmt)
            manifests
        with
        | None -> artifact
        | Some manifest ->
            {
              artifact with
              cmi = manifest.cmi;
              cmti = manifest.cmti;
              vri = manifest.vri;
            })
      requested
  in
  let additional =
    List.filter
      (fun manifest ->
        not
          (List.exists
             (fun artifact -> String.equal artifact.cmt manifest.cmt)
             requested))
      manifests
  in
  deduplicate_artifacts (requested @ additional)

let rec dune_package_ancestor directory =
  let candidate = Filename.concat directory "dune-package" in
  if existing_file candidate then Some candidate
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None else dune_package_ancestor parent

let rec sexp_atoms = function
  | Atom value -> [ value ]
  | List values -> List.concat_map sexp_atoms values

let rec sexp_obj_names = function
  | Atom _ -> []
  | List [ Atom "obj_name"; Atom value ] -> [ value ]
  | List values -> List.concat_map sexp_obj_names values

let package_library_fields name sexps =
  List.filter_map
    (function
      | List (Atom "library" :: fields)
        when atom_field "name" fields = Some name ->
          Some fields
      | Atom _ | List _ -> None)
    sexps

let package_lib_section sexps =
  List.find_map
    (function
      | List (Atom "sections" :: sections) -> atom_field "lib" sections
      | Atom _ | List _ -> None)
    sexps

let package_lib_files sexps =
  List.find_map
    (function
      | List (Atom "files" :: sections) ->
          List.find_map
            (function
              | List (Atom "lib" :: files) -> Some (List.concat_map sexp_atoms files)
              | Atom _ | List _ -> None)
            sections
      | Atom _ | List _ -> None)
    sexps

let package_lib_root filename section =
  let package_root = Filename.dirname filename in
  let candidate =
    if String.equal section "." then Some package_root
    else if Filename.is_relative section && safe_manifest_path section then
      Some (Filename.concat package_root section)
    else if not (Filename.is_relative section) then Some section
    else None
  in
  Option.bind candidate (fun directory ->
      try
        let directory = Unix.realpath directory in
        if existing_directory directory then Some directory else None
      with Unix.Unix_error _ -> None)

let package_component_artifacts component filename =
  let reject reason message =
    ignore reason;
    [%log.warn "rejected installed Dune component artifact inventory"
      ~stage:(Delator.Field.string "component-artifact-inventory")
      ~route:(Delator.Field.string "dune-package")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string reason)];
    Error message
  in
  match component.name with
  | None ->
      reject "missing-component-name"
        "Dune described an installed component without a library name"
  | Some name -> (
      let parsed =
        try parse_sexps (read_file filename)
        with Sys_error message -> Error message
      in
      match parsed with
      | Error message -> reject "metadata-parse" message
      | Ok sexps -> (
          let section = package_lib_section sexps in
          let lib_root =
            Option.bind section (package_lib_root filename)
          in
          match
            ( package_library_fields name sexps,
              section,
              lib_root,
              package_lib_files sexps )
          with
          | [ fields ], Some _, Some lib_root, Some files ->
              let declared_files =
                files
                |> List.filter safe_manifest_path
                |> List.map (Filename.concat lib_root)
                |> List.filter (fun path ->
                       List.exists
                         (fun directory -> within_directory ~directory path)
                         component.directories)
                |> List.sort_uniq String.compare
              and object_names =
                sexp_obj_names (List fields) |> List.sort_uniq String.compare
              in
              let declared object_name extension =
                let candidates =
                  declared_files
                  |> List.filter (fun path ->
                         Filename.check_suffix path extension
                         && String.equal
                              (Filename.basename path
                              |> Filename.remove_extension)
                              object_name)
                in
                match candidates with
                | [] -> Ok None
                | [ path ] when existing_file path -> Ok (Some path)
                | [ _ ] ->
                    reject "missing-declared-file"
                      (Printf.sprintf
                         "installed Dune component %s declares a missing %s artifact"
                         name extension)
                | _ :: _ :: _ ->
                    reject "colliding-declared-files"
                      (Printf.sprintf
                         "installed Dune component %s declares multiple %s artifacts for object %s"
                         name extension object_name)
              in
              let rec artifacts reversed = function
                | [] -> Ok (List.rev reversed)
                | object_name :: rest ->
                    let* cmt = declared object_name ".cmt" in
                    let* cmi = declared object_name ".cmi" in
                    let* cmti = declared object_name ".cmti" in
                    (match cmt with
                    | None -> artifacts reversed rest
                    | Some cmt ->
                        [%log.trace
                          "correlated installed Dune object with declared compiler artifacts"
                          ~stage:
                            (Delator.Field.string "component-artifact-inventory")
                          ~route:(Delator.Field.string "dune-package")
                          ~has_cmi:(Delator.Field.bool (Option.is_some cmi))
                          ~has_cmti:(Delator.Field.bool (Option.is_some cmti))
                          ~decision:(Delator.Field.string "inventoried")];
                        artifacts
                          ({
                             source = cmt;
                             cmt;
                             cmi;
                             cmti;
                             vri = None;
                             requested = false;
                           }
                          :: reversed)
                          rest)
              in
              let* artifacts = artifacts [] object_names in
              [%log.debug "decoded installed Dune component artifact inventory"
                ~stage:(Delator.Field.string "component-artifact-inventory")
                ~route:(Delator.Field.string "dune-package")
                ~object_count:(Delator.Field.int (List.length object_names))
                ~declared_file_count:
                  (Delator.Field.int (List.length declared_files))
                ~artifact_count:(Delator.Field.int (List.length artifacts))
                ~decision:(Delator.Field.string "accepted")];
              Ok artifacts
          | [], _, _, _ ->
              reject "missing-library-record"
                (Printf.sprintf
                   "installed Dune package has no exact library record for %s" name)
          | _ :: _ :: _, _, _, _ ->
              reject "duplicate-library-record"
                (Printf.sprintf
                   "installed Dune package has duplicate library records for %s"
                   name)
          | _, None, _, _ | _, _, _, None ->
              reject "missing-file-inventory"
                "installed Dune package omits its lib section or file inventory"
          | _, Some _, None, Some _ ->
              reject "unsafe-lib-section"
                "installed Dune package has an unsafe lib section"))
[@@delator.instrument] [@@delator.level debug]

let described_or_package_artifacts component =
  match component.artifacts with
  | _ :: _ as artifacts -> Ok artifacts
  | [] ->
      let packages =
        component.directories
        |> List.filter_map dune_package_ancestor
        |> List.sort_uniq String.compare
      in
      (match packages with
      | [] ->
          [%log.trace "reachable Dune component has no compiler artifact inventory"
            ~stage:(Delator.Field.string "component-artifact-inventory")
            ~route:(Delator.Field.string "dune-dependency-graph")
            ~decision:(Delator.Field.string "unavailable")
            ~reason_class:(Delator.Field.string "no-dune-package")];
          Ok []
      | [ filename ] -> package_component_artifacts component filename
      | _ :: _ :: _ ->
          [%log.warn "rejected ambiguous Dune component metadata roots"
            ~stage:(Delator.Field.string "component-artifact-inventory")
            ~route:(Delator.Field.string "dune-package")
            ~candidate_count:(Delator.Field.int (List.length packages))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "ambiguous-package-metadata")];
          Error "reachable Dune component resolves to multiple package inventories")
[@@delator.instrument] [@@delator.level debug]

let reachable_component_artifacts components =
  let rec collect reversed = function
    | [] -> Ok (deduplicate_artifacts (List.concat (List.rev reversed)))
    | component :: rest ->
        let* artifacts = described_or_package_artifacts component in
        collect (artifacts :: reversed) rest
  in
  let* artifacts = collect [] components in
  List.iter
    (fun artifact ->
      ignore artifact;
      [%log.trace "retained exact Dune component implementation artifact"
        ~stage:(Delator.Field.string "component-artifact-inventory")
        ~route:(Delator.Field.string "dune-dependency-graph")
        ~requested:(Delator.Field.bool artifact.requested)
        ~has_cmi:(Delator.Field.bool (Option.is_some artifact.cmi))
        ~decision:(Delator.Field.string "inventoried")])
    artifacts;
  [%log.debug "retained reachable Dune component artifact inventory"
    ~stage:(Delator.Field.string "component-artifact-inventory")
    ~route:(Delator.Field.string "dune-dependency-graph")
    ~reachable_component_count:(Delator.Field.int (List.length components))
    ~artifact_count:(Delator.Field.int (List.length artifacts))
    ~requested_artifact_count:
      (Delator.Field.int
         (List.fold_left
            (fun count artifact ->
              if artifact.requested then count + 1 else count)
            0 artifacts))
    ~decision:(Delator.Field.string "exact-component-inventory-only")];
  Ok artifacts
[@@delator.instrument] [@@delator.level debug]

let workspace_shape ~root ~requested_directory description =
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
            Error "Dune workspace description 0.1 omitted root or build_context")
    | Atom _ -> Error "Dune workspace description 0.1 is not a record"
  in
  let components = workspace_components ~root workspace in
  let components =
    requested_components ~root ~requested_directory ~build_context components
  in
  let* reachable = reachable_components components in
  let requested_artifacts =
    components
    |> List.concat_map (fun (component, _) -> component.artifacts)
    |> List.filter (fun artifact -> artifact.requested)
    |> deduplicate_artifacts
  in
  let[@log_value.debug] manifest_directories =
    reachable |> List.concat_map (fun component -> component.directories)
    |> List.sort_uniq String.compare
  in
  [%log.debug "parsed declared Dune dependency graph"
    ~stage:(Delator.Field.string "workspace-description")
    ~route:(Delator.Field.string "directory-verification")
    ~component_count:(Delator.Field.int (List.length components))
    ~reachable_component_count:(Delator.Field.int (List.length reachable))
    ~requested_artifact_count:(Delator.Field.int (List.length requested_artifacts))
    ~dependency_directory_count:
      (Delator.Field.int
         (List.length (manifest_directories [@log_value.debug])))
    ~decision:(Delator.Field.string "accepted")];
  Ok (requested_artifacts, reachable)
[@@delator.instrument] [@@delator.level debug]

let local_component_build_targets ~root components =
  components
  |> List.concat_map (fun component -> component.artifacts)
  |> List.filter_map (fun artifact ->
         let directory = Filename.dirname artifact.source in
         if within_directory ~directory:root directory then
           let relative = relative_to ~root directory in
           Some
             (if String.equal relative "" then "@all"
              else "@" ^ relative ^ "/all")
         else None)
  |> List.sort_uniq String.compare

let dune_build_arguments ~root ~build_directory targets =
  [
    "build";
    "--no-print-directory";
    "--root";
    root;
    "--build-dir";
    build_directory;
    "--cache";
    "disabled";
    "--profile";
    dune_profile ();
  ]
  @ targets

let dune_describe_arguments ~root ~requested_directory ~build_directory =
  let relative = relative_to ~root requested_directory in
  let scope = if String.equal relative "" then [] else [ relative ] in
  [%log.debug "selected Dune workspace description scope"
    ~stage:(Delator.Field.string "workspace-description")
    ~route:(Delator.Field.string "directory-verification")
    ~project_root:(Delator.Field.string root)
    ~requested_directory:(Delator.Field.string requested_directory)
    ~described_directory:(Delator.Field.string relative)
    ~decision:(Delator.Field.string "scoped")];
  [
    "describe";
    "workspace";
    "--no-print-directory";
    "--root";
    root;
    "--build-dir";
    build_directory;
    "--profile";
    dune_profile ();
    "--format";
    "csexp";
    "--lang";
    "0.1";
    "--with-deps";
  ]
  @ scope
[@@delator.instrument] [@@delator.level debug]

let build_and_describe requested =
  let* requested_directory =
    (try
       let directory = Unix.realpath requested in
       if existing_directory directory then Ok directory
       else Error (Printf.sprintf "%S is not a directory" requested)
     with Unix.Unix_error (error, function_name, argument) ->
       Error
         (Printf.sprintf "%s(%s): %s" function_name argument
            (Unix.error_message error)))
    |> cli_result
  in
  let* root =
    project_root ~requested:requested_directory requested_directory |> cli_result
  in
  let* target = build_target ~root ~requested_directory |> cli_result in
  let build_directory = verification_build_directory root in
  let* () = ensure_build_parent build_directory |> cli_result in
  let[@log_value.info] build_scope =
    if String.equal root requested_directory then "project-root"
    else "requested-subtree"
  in
  [%log.info "build declared Dune verification targets"
    ~stage:(Delator.Field.string "dune-build")
    ~route:(Delator.Field.string "directory-verification")
    ~project_root:(Delator.Field.string root)
    ~requested_directory:(Delator.Field.string requested_directory)
    ~build_target:(Delator.Field.string target)
    ~build_directory:(Delator.Field.string build_directory)
    ~ppx_mode:(Delator.Field.string "retained")
    ~dune_cache:(Delator.Field.string "disabled")
    ~build_scope:
      (Delator.Field.string (build_scope [@log_value.info]))
    ~decision:(Delator.Field.string "invoke")];
  let* _, build_stderr =
    run (dune_build_arguments ~root ~build_directory [ target ])
    |> cli_result
  in
  if not (String.equal build_stderr "") then output_string Stdlib.stderr build_stderr;
  let* description, describe_stderr =
    run (dune_describe_arguments ~root ~requested_directory ~build_directory)
    |> cli_result
  in
  if not (String.equal describe_stderr "") then
    output_string Stdlib.stderr describe_stderr;
  let* requested_artifacts, reachable_components =
    workspace_shape ~root ~requested_directory description |> cli_result
  in
  let dependency_targets =
    local_component_build_targets ~root reachable_components
    |> List.filter (fun candidate -> not (String.equal candidate target))
  in
  let* () =
    match dependency_targets with
    | [] -> Ok ()
    | _ :: _ ->
        [%log.info "build retained artifacts for reachable local Dune dependencies"
          ~stage:(Delator.Field.string "dune-dependency-build")
          ~route:(Delator.Field.string "directory-verification")
          ~dependency_target_count:
            (Delator.Field.int (List.length dependency_targets))
          ~decision:(Delator.Field.string "invoke")];
        let* _, dependency_stderr =
          run (dune_build_arguments ~root ~build_directory dependency_targets)
          |> cli_result
        in
        if not (String.equal dependency_stderr "") then
          output_string Stdlib.stderr dependency_stderr;
        Ok ()
  in
  let* component_artifacts =
    reachable_component_artifacts reachable_components
    |> dependency_result "component-artifact-inventory"
  in
  let* () =
    (if requested_artifacts = [] then (
      [%log.warn "Dune produced no requested implementation artifacts"
        ~stage:(Delator.Field.string "workspace-description")
        ~route:(Delator.Field.string "directory-verification")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "no-cmt")];
      Error
        (Printf.sprintf
           "Dune successfully built %S but described no implementation CMT artifacts; ensure the directory contains an OCaml library or executable with binary annotations enabled"
           requested_directory))
     else Ok ())
    |> cli_result
  in
  let* manifest_filenames =
    manifest_filenames reachable_components requested_artifacts component_artifacts
    |> dependency_result "declared-manifest-discovery"
  in
  let* manifest_artifacts = load_manifests manifest_filenames in
  let artifacts =
    merge_manifest_artifacts component_artifacts manifest_artifacts
  in
  [%log.info "completed declared Dune artifact discovery"
    ~stage:(Delator.Field.string "retained-manifest-discovery")
    ~route:(Delator.Field.string "dune-dependency-graph")
    ~requested_artifact_count:(Delator.Field.int (List.length requested_artifacts))
    ~component_artifact_count:
      (Delator.Field.int (List.length component_artifacts))
    ~manifest_count:(Delator.Field.int (List.length manifest_filenames))
    ~artifact_count:(Delator.Field.int (List.length artifacts))
    ~decision:(Delator.Field.string "accepted")];
  Ok { root; requested_directory; artifacts }
[@@delator.instrument] [@@delator.level info]
