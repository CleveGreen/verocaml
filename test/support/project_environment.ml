type expected = {
  source_commit : string;
  context : string;
  package_root : string;
  binary_root : string;
  dune_path : string;
  tool_path : string;
  library_set_digest : string;
}

type library = { name : string; artifact : string; digest : string }

type t = {
  source_commit : string;
  context : string;
  package_root : string;
  binary_root : string;
  dune_path : string;
  tool_path : string;
  library_set_digest : string;
  libraries : library list;
}

let source_commit environment = environment.source_commit
let context environment = environment.context
let package_root environment = environment.package_root
let binary_root environment = environment.binary_root
let dune_path environment = environment.dune_path
let tool_path environment = environment.tool_path
let library_set_digest environment = environment.library_set_digest
let library_names environment = List.map (fun library -> library.name) environment.libraries

let rec parent count path =
  if count <= 0 then path else parent (count - 1) (Filename.dirname path)

let library_root library =
  let depth = List.length (String.split_on_char '.' library.name) in
  parent depth (Filename.dirname library.artifact)

let ocaml_path environment =
  environment.package_root
  :: List.map library_root environment.libraries
  |> List.sort_uniq String.compare |> String.concat ":"

let split_once separator value =
  match String.index_opt value separator with
  | None -> (value, "")
  | Some index ->
      ( String.sub value 0 index,
        String.sub value (index + 1) (String.length value - index - 1) )

let read_lines path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop lines =
        match input_line channel with
        | line -> loop (line :: lines)
        | exception End_of_file -> List.rev lines
      in
      loop [])

let library_line library =
  String.concat "|" [ library.name; library.artifact; library.digest ]

let set_digest libraries =
  libraries
  |> List.sort (fun left right -> String.compare left.name right.name)
  |> List.map library_line |> String.concat "\n" |> Digest.string |> Digest.to_hex

let parse_library line =
  match String.split_on_char '|' line with
  | [ name; artifact; digest ] when name <> "" && artifact <> "" && digest <> "" ->
      Ok { name; artifact; digest }
  | _ -> Error "malformed library row"

let canonical_digest digest =
  String.length digest = 32
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) digest

let canonical_absolute_path path =
  (not (Filename.is_relative path))
  && Sys.file_exists path
  &&
  match String.split_on_char '/' path with
  | "" :: components ->
      components <> []
      && List.for_all
           (fun component -> component <> "" && component <> "." && component <> "..")
           components
  | _ -> false

let parse_manifest lines =
  let scalar_keys =
    [
      "version";
      "source_commit";
      "context";
      "package_root";
      "binary_root";
      "dune_path";
      "tool_path";
      "library_set_digest";
    ]
  in
  let rec scalars fields keys lines =
    match (keys, lines) with
    | [], lines -> (List.rev fields, lines)
    | key :: keys, line :: lines ->
        let actual_key, value = split_once '=' line in
        if actual_key <> key || value = "" then
          raise (Failure ("noncanonical or missing manifest field " ^ key));
        scalars ((key, value) :: fields) keys lines
    | _ -> raise (Failure "truncated manifest")
  in
  let fields, library_lines = scalars [] scalar_keys lines in
  if library_lines = [] then raise (Failure "manifest has no library rows");
  let libraries =
    library_lines
    |> List.map (fun line ->
           let key, value = split_once '=' line in
           if key <> "library" || value = "" then
             raise (Failure "noncanonical manifest field after library-set digest");
           match parse_library value with
           | Ok library -> library
           | Error message -> raise (Failure message))
  in
  (fields, libraries)

let validate_canonical_projection ~package_root ~binary_root ~dune_path ~tool_path
    libraries =
  let paths =
    [
      ("package_root", package_root);
      ("binary_root", binary_root);
      ("dune_path", dune_path);
    ]
  in
  (match List.find_opt (fun (_, path) -> not (canonical_absolute_path path)) paths with
  | Some (label, _) -> raise (Failure ("noncanonical manifest path " ^ label))
  | None -> ());
  let tool_directories = String.split_on_char ':' tool_path in
  if
    tool_directories = []
    || List.exists (fun path -> not (canonical_absolute_path path)) tool_directories
    || tool_directories <> List.sort_uniq String.compare tool_directories
  then raise (Failure "noncanonical manifest tool_path");
  if
    List.exists
      (fun library ->
        not (canonical_absolute_path library.artifact)
        || not (canonical_digest library.digest))
      libraries
  then raise (Failure "noncanonical library row");
  let canonical_libraries =
    List.sort (fun left right -> String.compare left.name right.name) libraries
  in
  if libraries <> canonical_libraries then raise (Failure "noncanonical library row order");
  let names = List.map (fun library -> library.name) libraries in
  let artifacts = List.map (fun library -> library.artifact) libraries in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    raise (Failure "duplicate library name");
  if List.length artifacts <> List.length (List.sort_uniq String.compare artifacts) then
    raise (Failure "duplicate library artifact")

let validate ~(expected : expected) path =
  if not (Sys.file_exists path) then
    Error
      (Failure.make Failure.Project_environment_missing
         ("integration library-environment manifest is absent: " ^ path))
  else
    let fail message =
      Error
        (Failure.make Failure.Project_environment_provenance_mismatch message)
    in
    try
      let fields, libraries = read_lines path |> parse_manifest in
      let required key =
        match List.assoc_opt key fields with
        | Some value when value <> "" -> value
        | _ -> raise (Failure ("missing manifest field " ^ key))
      in
      let version = required "version" in
      let source_commit = required "source_commit" in
      let context = required "context" in
      let package_root = required "package_root" in
      let binary_root = required "binary_root" in
      let dune_path = required "dune_path" in
      let tool_path = required "tool_path" in
      let library_set_digest = required "library_set_digest" in
      validate_canonical_projection ~package_root ~binary_root ~dune_path ~tool_path
        libraries;
      if version <> "1" then fail "unsupported manifest version"
      else if source_commit <> expected.source_commit then
        fail "source/integration commit does not match the running build"
      else if context <> expected.context then
        fail "outer Dune context does not match the running build"
      else if package_root <> expected.package_root then
        fail "package-root projection does not match the running build"
      else if binary_root <> expected.binary_root then
        fail "binary-root projection does not match the running build"
      else if dune_path <> expected.dune_path then
        fail "Dune path does not match the running build"
      else if tool_path <> expected.tool_path then
        fail "tool-path projection does not match the running build"
      else if library_set_digest <> expected.library_set_digest then
        fail "library-set identity/digest does not match the running build"
      else if set_digest libraries <> library_set_digest then
        fail "library-set rows do not match their canonical digest"
      else
        let mismatched =
          List.find_opt
            (fun library ->
              (not (Sys.file_exists library.artifact))
              || Digest.file library.artifact |> Digest.to_hex <> library.digest)
            libraries
        in
        (match mismatched with
        | Some library ->
            fail ("library artifact provenance differs: " ^ library.name)
        | None ->
            Ok
              {
                source_commit;
                context;
                package_root;
                binary_root;
                dune_path;
                tool_path;
                library_set_digest;
                libraries;
              })
    with
    | Sys_error message -> fail message
    | Failure message -> fail message
    | End_of_file -> fail "truncated manifest"

let write ~path ~source_commit ~context ~package_root ~binary_root ~dune_path
    ~tool_path ~libraries =
  let libraries =
    libraries
    |> List.map (fun (name, artifact) ->
           { name; artifact; digest = Digest.file artifact |> Digest.to_hex })
    |> List.sort (fun left right -> String.compare left.name right.name)
  in
  let library_set_digest = set_digest libraries in
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      Printf.fprintf channel "version=1\nsource_commit=%s\ncontext=%s\n" source_commit
        context;
      Printf.fprintf channel "package_root=%s\nbinary_root=%s\n" package_root binary_root;
      Printf.fprintf channel "dune_path=%s\ntool_path=%s\n" dune_path tool_path;
      Printf.fprintf channel "library_set_digest=%s\n" library_set_digest;
      List.iter
        (fun library -> Printf.fprintf channel "library=%s\n" (library_line library))
        libraries);
  library_set_digest
