let fail message =
  prerr_endline ("integration-manifest-writer: " ^ message);
  exit 2

let rec parse options libraries = function
  | [] -> (options, List.rev libraries)
  | "--library" :: name :: artifact :: rest ->
      parse options ((name, artifact) :: libraries) rest
  | flag :: value :: rest when String.starts_with ~prefix:"--" flag ->
      parse ((flag, value) :: options) libraries rest
  | flag :: _ -> fail ("invalid argument " ^ flag)

let required key options =
  match List.assoc_opt key options with
  | Some value -> value
  | None -> fail ("missing " ^ key)

let find_program program =
  if not (Filename.is_relative program) then Unix.realpath program
  else
    let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
    match
      String.split_on_char ':' path
      |> List.find_map (fun directory ->
             let candidate = Filename.concat directory program in
             if Sys.file_exists candidate then Some candidate else None)
    with
    | Some path -> Unix.realpath path
    | None -> fail ("cannot resolve program " ^ program)

let resolve_program = find_program

let find_substring pattern value =
  let rec loop index =
    if index + String.length pattern > String.length value then None
    else if String.sub value index (String.length pattern) = pattern then Some index
    else loop (index + 1)
  in
  loop 0

let outer_build_root context =
  let cwd = Unix.realpath (Sys.getcwd ()) in
  match find_substring "/_build/.sandbox/" cwd with
  | Some index ->
      String.sub cwd 0 index |> fun root -> Filename.concat root ("_build/" ^ context)
  | None -> (
      let marker = "/_build/" ^ context in
      match find_substring marker cwd with
      | Some index -> String.sub cwd 0 (index + String.length marker)
      | None -> fail "manifest writer is not running in an outer Dune build context")

let () =
  let options, libraries =
    Array.to_list Sys.argv |> List.tl |> parse [] []
  in
  if libraries = [] then fail "at least one library is required";
  let manifest = required "--manifest" options in
  let generated_module = required "--module" options in
  let source_commit = required "--source-commit" options in
  let context = required "--context" options in
  let build_root = outer_build_root context in
  let repository_root = Filename.dirname (Filename.dirname build_root) in
  let install_root = Filename.concat repository_root ("_build/install/" ^ context) in
  let package_root = Filename.concat install_root "lib" in
  let binary_root = Filename.concat install_root "bin" in
  let dune_path = required "--dune-path" options |> resolve_program in
  let compiler_path = required "--compiler-path" options |> resolve_program in
  let tool_directories =
    [ "as"; "cc"; "gcc"; "ld"; "ar"; "ranlib"; "sh"; "ln"; "mkdir"; "rm" ]
    |> List.filter_map (fun program ->
           try Some (find_program program |> Filename.dirname) with _ -> None)
    |> List.sort_uniq String.compare
  in
  let tool_path =
    binary_root :: Filename.dirname dune_path :: Filename.dirname compiler_path
    :: tool_directories
    |> List.sort_uniq String.compare |> String.concat ":"
  in
  let digest =
    let libraries =
      List.map
        (fun (name, artifact) ->
          let artifact =
            if Filename.is_relative artifact then Filename.concat install_root artifact
            else artifact
          in
          (name, artifact))
        libraries
    in
    Outcome_test_support.Project_environment.write ~path:manifest ~source_commit
      ~context ~package_root ~binary_root ~dune_path ~tool_path ~libraries
  in
  let channel = open_out_bin generated_module in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      Printf.fprintf channel
        "let manifest = Filename.concat (Filename.dirname Sys.executable_name) %S\n"
        "integration-library-environment.manifest";
      Printf.fprintf channel
        "let expected : Outcome_test_support.Project_environment.expected =\n  \
         { source_commit = %S; context = %S; package_root = %S; binary_root = %S;\n    \
         dune_path = %S; tool_path = %S; library_set_digest = %S }\n"
        source_commit context package_root binary_root dune_path tool_path digest)
