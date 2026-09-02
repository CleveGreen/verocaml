type file = { path : string; contents : string }

let ( let* ) = Result.bind

type dune_project = {
  files : file list;
  libraries : string list;
  targets : string list;
  selected_units : string list;
}

type input =
  | Dune_project of dune_project
  | Single_source of {
      module_name : string;
      source : string;
      libraries : string list;
    }
  | Prepared_cmt of { artifact : string; declared_dependency : string }

let dune_project project = Dune_project project
let single_source ~module_name ~source ~libraries =
  Single_source { module_name; source; libraries }

let prepared_cmt ~declared_dependencies artifact =
  let canonical path =
    try Unix.realpath path with Unix.Unix_error _ -> path
  in
  let selected = canonical artifact in
  match
    List.find_opt
      (fun dependency -> String.equal selected (canonical dependency))
      declared_dependencies
  with
  | None -> Error "prepared CMT is not a declared dependency"
  | Some declared_dependency -> Ok (Prepared_cmt { artifact; declared_dependency })

let valid_module_name name =
  String.length name > 0
  &&
  match name.[0] with
  | 'A' .. 'Z' ->
      String.for_all
        (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
        name
  | _ -> false

let source_name module_name = String.uncapitalize_ascii module_name ^ ".ml"

let lower_single_source ~module_name ~source ~libraries =
  let library_list = String.concat " " libraries in
  {
    files =
      [
        { path = "dune-project"; contents = "(lang dune 3.17)\n(name outcome_fixture)\n" };
        {
          path = "dune";
          contents =
            Printf.sprintf
              "(library\n (name outcome_fixture)\n (wrapped false)\n (modules %s)\n \
               (libraries %s)\n (flags (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n"
              module_name library_list;
        };
        { path = source_name module_name; contents = source };
      ];
    libraries;
    targets = [ "@all" ];
    selected_units = [ module_name ];
  }

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let path_components path =
  String.split_on_char '/' path |> List.filter (fun part -> part <> "")

let safe_relative path =
  Filename.is_relative path
  && path <> ""
  && not (String.contains path '\000')
  && path_components path
     |> List.for_all (fun part -> part <> "." && part <> "..")

let write_file root file =
  if not (safe_relative file.path) then
    Error
      (Failure.make Failure.Project_materialization
         ("unsafe project path: " ^ file.path))
  else
    let destination = Filename.concat root file.path in
    mkdir_p (Filename.dirname destination);
    let channel = open_out_bin destination in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel file.contents);
    Ok ()

let materialize root project =
  if project.targets = [] then
    Error (Failure.make Failure.Project_materialization "no Dune build target declared")
  else if project.selected_units = [] then
    Error (Failure.make Failure.Project_materialization "no implementation unit selected")
  else if not (List.for_all valid_module_name project.selected_units) then
    Error (Failure.make Failure.Project_materialization "invalid selected unit name")
  else
    let paths = List.map (fun file -> file.path) project.files in
    if List.length paths <> List.length (List.sort_uniq String.compare paths) then
      Error (Failure.make Failure.Project_materialization "duplicate virtual project path")
    else if not (List.mem "dune-project" paths) then
      Error (Failure.make Failure.Project_materialization "virtual tree lacks dune-project")
    else if not (List.exists (fun path -> Filename.basename path = "dune") paths) then
      Error (Failure.make Failure.Project_materialization "virtual tree lacks dune file")
    else
      let dune_text =
        project.files
        |> List.filter (fun file -> Filename.basename file.path = "dune")
        |> List.map (fun file -> file.contents)
        |> String.concat "\n"
      in
      let undeclared =
        List.find_opt
          (fun library ->
            not
              (String.contains dune_text '('
              && String.split_on_char ' ' dune_text
                 |> List.exists (fun token ->
                        String.trim token
                        |> String.starts_with ~prefix:library)))
          project.libraries
      in
      (match undeclared with
      | Some library ->
          Error
            (Failure.make Failure.Project_materialization
               ("declared library is absent from ordinary Dune libraries: " ^ library))
      | None ->
          mkdir_p root;
          List.fold_left
            (fun result file -> Result.bind result (fun () -> write_file root file))
            (Ok ()) project.files)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let run_process ~environment ~root targets =
  let log = Filename.concat root "dune-build.log" in
  let fd = Unix.openfile log [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  let ocaml_path = Project_environment.ocaml_path environment in
  let path = Project_environment.tool_path environment in
  let variables =
    [|
      "PATH=" ^ path;
      "OCAMLPATH=" ^ ocaml_path;
      "DUNE_CACHE=disabled";
      "DUNE_CONFIG__DISPLAY=quiet";
      "DELATOR_LOG=trace";
      "DELATOR_FORMAT=flat";
      "DELATOR_COLOR=never";
      "HOME=" ^ root;
      "TMPDIR=" ^ root;
    |]
  in
  let arguments =
    [ Project_environment.dune_path environment; "build"; "--root"; root;
      "--build-dir"; Filename.concat root "_build"; "--profile"; "release" ]
    @ targets
    |> Array.of_list
  in
  let status =
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        let pid =
          Unix.create_process_env arguments.(0) arguments variables Unix.stdin fd fd
        in
        snd (Unix.waitpid [] pid))
  in
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | WEXITED code ->
      Error
        (Failure.make Failure.Dune_build
           (Printf.sprintf "nested Dune exited %d: %s" code (read_file log)))
  | WSIGNALED signal ->
      Error
        (Failure.make Failure.Dune_build
           (Printf.sprintf "nested Dune was signaled %d" signal))
  | WSTOPPED signal ->
      Error
        (Failure.make Failure.Dune_build
           (Printf.sprintf "nested Dune stopped %d" signal))

let rec files_below root =
  try
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name ->
           let path = Filename.concat root name in
           try if Sys.is_directory path then files_below path else [ path ]
           with Sys_error _ -> [])
  with Sys_error _ -> []

let discover_cmt root unit_name =
  let expected = String.uncapitalize_ascii unit_name ^ ".cmt" in
  let rec installed_copy = function
    | "_build" :: "install" :: _ -> true
    | _ :: rest -> installed_copy rest
    | [] -> false
  in
  let matches =
    files_below (Filename.concat root "_build")
    |> List.filter (fun path ->
           Filename.basename path = expected
           && not (installed_copy (String.split_on_char '/' path)))
  in
  match matches with
  | [ path ] -> Ok path
  | [] ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("no selected CMT for unit " ^ unit_name))
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("ambiguous selected CMT for unit " ^ unit_name))

let load_cmt ?(artifact_directories = []) path =
  let path =
    try Unix.realpath path
    with Unix.Unix_error _ ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  let cmi = Filename.remove_extension path ^ ".cmi" in
  let loaded =
    if Sys.file_exists cmi then
      let rec before_build current =
        if Filename.basename current = "_build" then Filename.dirname current
        else
          let parent = Filename.dirname current in
          if String.equal parent current then current else before_build parent
      in
      let root = before_build (Filename.dirname path) in
      let default_build = Filename.concat root "_build/default" in
      let information = Cmt_format.read_cmt path in
      let unit_name =
        Compilation_unit.name_as_string information.Cmt_format.cmt_modname
      in
      let authority =
        let expected = String.uncapitalize_ascii unit_name ^ ".vri" in
        files_below default_build
        |> List.filter (fun candidate ->
               Filename.basename candidate = expected
               && String.starts_with ~prefix:(default_build ^ "/") candidate)
        |> function [ filename ] -> Some filename | [] | _ :: _ :: _ -> None
      in
      let local_artifact_directories =
        files_below default_build
        |> List.filter (fun filename ->
               String.starts_with ~prefix:(default_build ^ "/") filename
               && List.mem (Filename.extension filename) [ ".cmi"; ".cmti"; ".vri" ])
        |> List.map Filename.dirname |> List.sort_uniq String.compare
      in
      let artifact_directories =
        List.sort_uniq String.compare
          (artifact_directories @ local_artifact_directories)
      in
      Cmt_input.load_with_interface ~cmt:path ~cmi ?vri:authority ~artifact_directories
        ()
    else Cmt_input.load path
  in
  match loaded with
  | Ok implementation -> Ok (`Implementation implementation)
  | Error diagnostic -> (
      match diagnostic.Diagnostic.classification with
      | Unsupported_target _
      | Unsupported_input _
      | Unsupported_construct _
      | Invalid_recursive_rank _
      | Invalid_broadcast _
      | Invalid_broadcast_dependency _
      | Invalid_symbolic_declaration _
      | Invalid_symbolic_application _
      | Invalid_symbolic_authentication _
      | Invalid_symbolic_dependency _
      | Executable_function_in_specification _
      | Unannotated_erased_call _
      | Invalid_verification_call _
      | Invalid_imported_specification _
      | Invalid_semantic_program _ ->
          Ok (`Frontend diagnostic.code)
      | Malformed_input | Incompatible_magic | Input_io_error ->
          Error
            (Failure.make Failure.Selected_cmt_load
               (Printf.sprintf "%s: %s" diagnostic.code diagnostic.message)))

let disposition projection =
  match Outcome.status projection with
  | Outcome.Verified -> Outcome.Unit_verified
  | Counterexample -> Unit_counterexample
  | Inconclusive -> Unit_inconclusive
  | Incomplete_source -> Unit_incomplete_source
  | Frontend_rejected -> Unit_frontend_rejected

let verify_loaded ~providers loaded =
  let configuration =
    Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
  in
  match configuration with
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Verifier_service.configuration_error_message error))
  | Ok configuration ->
      let implementations =
        let selected =
          loaded
          |> List.filter_map (function
             | unit_name, `Implementation implementation ->
                 Some (unit_name, implementation)
             | _, `Frontend _ -> None)
        in
        selected
        @ List.map
            (fun (implementation : Cmt_input.implementation) ->
              (implementation.unit_name, implementation))
            providers
      in
      loaded
      |> List.map (function
           | unit_name, `Frontend code ->
               Outcome.frontend_rejection ~code
               |> Outcome.with_unit unit_name Outcome.Unit_frontend_rejected
               |> Result.ok
           | unit_name, `Implementation consumer ->
               let imported_units =
                 consumer.Cmt_input.imports
                 |> Array.to_list
                 |> List.map (fun (import : Cmt_input.import) -> import.unit_name)
               in
               let dependencies =
                 implementations
                 |> List.filter_map (fun (other_name, implementation) ->
                        if other_name = unit_name || not (List.mem other_name imported_units)
                        then None
                        else Some implementation)
               in
               let request =
                 Verifier_service.request ~configuration ~consumer ~dependencies
               in
               (match Verifier_service.verify request with
               | Ok result ->
                   let projection = Outcome.of_verifier_result result in
                   Outcome.with_unit unit_name (disposition projection) projection
                   |> Result.ok
               | Error error -> (
                   match Verifier_service.error_diagnostic error with
                   | Some diagnostic ->
                       Outcome.frontend_rejection ~code:diagnostic.code
                       |> Outcome.with_unit unit_name Outcome.Unit_frontend_rejected
                       |> Result.ok
                   | None ->
                       Error
                         (Failure.make Failure.Verifier_outcome
                            (Verifier_service.error_message error)))))
      |> List.fold_left
           (fun result item ->
             Result.bind result (fun projections ->
                 Result.map (fun projection -> projection :: projections) item))
           (Ok [])
      |> Result.map Outcome.merge

let library_provider_directories environment libraries =
  libraries
  |> List.map (fun library ->
         Filename.concat (Project_environment.package_root environment)
           (String.concat "/" (String.split_on_char '.' library)))
  |> List.filter Sys.file_exists
  |> List.sort_uniq String.compare

let retained_providers ~environment ~libraries =
  let provider_directories =
    library_provider_directories environment libraries
  in
  provider_directories
  |> List.concat_map (fun directory ->
         files_below directory
         |> List.filter (fun path -> Filename.extension path = ".cmt"))
  |> List.sort_uniq String.compare
  |> List.fold_left
       (fun result path ->
         let* providers = result in
         match load_cmt ~artifact_directories:provider_directories path with
         | Ok (`Implementation implementation)
           when Cmt_input.retained_ppx_artifact implementation ->
             Ok (implementation :: providers)
         | Ok _ -> Ok providers
         | Error failure -> Error failure)
       (Ok [])
  |> Result.map List.rev

let run_project ~environment ~workspace project =
  let workspace =
    if Filename.is_relative workspace then Filename.concat (Sys.getcwd ()) workspace
    else workspace
  in
  let root = Filename.concat workspace "project" in
  let ( let* ) result next = Result.bind result next in
  let* () = materialize root project in
  let* () = run_process ~environment ~root project.targets in
  let provider_directories =
    library_provider_directories environment project.libraries
  in
  let* selected =
    project.selected_units
    |> List.map (fun unit_name ->
           Result.map (fun path -> (unit_name, path)) (discover_cmt root unit_name))
    |> List.fold_left
         (fun result item ->
           let* items = result in
           let* item = item in
           Ok (item :: items))
         (Ok [])
  in
  let* loaded =
    selected
    |> List.map (fun (unit_name, path) ->
           Result.map
             (fun loaded -> (unit_name, loaded))
             (load_cmt ~artifact_directories:provider_directories path))
    |> List.fold_left
         (fun result item ->
           let* items = result in
           let* item = item in
           Ok (item :: items))
         (Ok [])
  in
  let* providers = retained_providers ~environment ~libraries:project.libraries in
  let providers =
    providers
    |> List.filter (fun implementation ->
           not
             (List.exists
                (fun (_, loaded) ->
                  match loaded with
                  | `Implementation selected ->
                      String.equal selected.Cmt_input.unit_name
                        implementation.Cmt_input.unit_name
                  | `Frontend _ -> false)
                loaded))
  in
  verify_loaded ~providers loaded

let run ~environment ~workspace = function
  | Single_source { module_name; source; libraries } ->
      if not (valid_module_name module_name) then
        Error (Failure.make Failure.Project_materialization "invalid module name")
      else
        lower_single_source ~module_name ~source ~libraries
        |> run_project ~environment ~workspace
  | Dune_project project -> run_project ~environment ~workspace project
  | Prepared_cmt { artifact; declared_dependency } ->
      let canonical path =
        try Unix.realpath path with Unix.Unix_error _ -> path
      in
      if canonical artifact <> canonical declared_dependency then
        Error
          (Failure.make Failure.Selected_cmt_load
             "prepared CMT no longer matches its declared dependency")
      else
        Result.bind (load_cmt artifact) (fun loaded ->
            let unit_name =
              match loaded with
              | `Implementation implementation -> implementation.Cmt_input.unit_name
              | `Frontend _ -> Filename.basename artifact
            in
            verify_loaded ~providers:[] [ (unit_name, loaded) ])
