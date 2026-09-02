let manifest_magic = "verocaml-retained-interface-manifest-v1"

let usage () =
  [%log.warn "rejected retained-interface command"
    ~stage:(Delator.Field.string "command-line")
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string "usage")];
  prerr_endline
    "usage: verocaml-retained-interface emit CMT CMI CMTI OUTPUT.vri [--ordinary-cmi FILE.cmi]... [--artifact-directory DIR]... | verocaml-retained-interface manifest OUTPUT CMT CMI CMTI VRI | verocaml-retained-interface dune-stanza PACKAGE DESTINATION RETAINED_LIBRARY UNIT [--transport-unit COMPILER_UNIT]... [--dependency-transport-unit COMPILER_UNIT]... [DEPENDENCY_DIRECTORY DEPENDENCY_LIBRARY DEPENDENCY_UNIT]... | verocaml-retained-interface erase-cmi INPUT.cmi OUTPUT.cmi";
  2

let identifier value =
  value <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
         | _ -> false)
       value

let package_name value =
  value <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
         | _ -> false)
       value

let safe_relative_path value =
  (String.equal value "."
  || (value <> "" && Filename.is_relative value
  && value |> String.split_on_char '/'
     |> List.for_all (fun component ->
            component <> "" && component <> "." && component <> ".."
            && String.for_all
                 (function
                   | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' ->
                       true
                   | _ -> false)
                 component)))

let dune_relative_path value =
  String.equal value "."
  || (value <> "" && Filename.is_relative value
  && value |> String.split_on_char '/'
     |> List.for_all (fun component ->
            component <> "" && component <> "."
            &&
            (String.equal component ".."
            || String.for_all
                 (function
                   | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' ->
                       true
                   | _ -> false)
                 component)))

let path_in directory filename =
  if String.equal directory "." then filename
  else Filename.concat directory filename

let write_file filename contents =
  let temporary =
    Filename.temp_file ~temp_dir:(Filename.dirname filename)
      (Filename.basename filename ^ ".tmp-") ""
  in
  try
    let channel = open_out_bin temporary in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        output_string channel contents;
        flush channel;
        Unix.fsync (Unix.descr_of_out_channel channel));
    Unix.rename temporary filename;
    Ok ()
  with Sys_error message | Unix.Unix_error (_, _, message) ->
    (try if Sys.file_exists temporary then Sys.remove temporary
     with Sys_error _ -> ());
    Error message

let manifest output paths =
  if List.length paths <> 4 || not (List.for_all safe_relative_path paths) then (
    [%log.warn "rejected retained-interface manifest"
      ~stage:(Delator.Field.string "manifest-write")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "unsafe-path")];
    usage ())
  else
    let contents = String.concat "\n" (manifest_magic :: paths) ^ "\n" in
    match write_file output contents with
    | Ok () ->
        [%log.info "wrote declared retained-interface manifest"
          ~stage:(Delator.Field.string "manifest-write")
          ~route:(Delator.Field.string "dune-rule")
          ~decision:(Delator.Field.string "accepted")];
        0
    | Error _message ->
        [%log.warn "failed to write retained-interface manifest"
          ~stage:(Delator.Field.string "manifest-write")
          ~route:(Delator.Field.string "dune-rule")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "io-error")];
        prerr_endline
          "verocaml-retained-interface: could not write the declared manifest";
        2
[@@delator.instrument] [@@delator.level info]

let dependency_stanza dependencies =
  dependencies
  |> List.mapi (fun index (directory, library, unit_name) ->
         let stem = String.uncapitalize_ascii unit_name in
         let object_path =
           path_in directory ("." ^ library ^ ".objs/byte/" ^ stem)
         in
         Printf.sprintf
           "  (:dependency_%d_cmt %s)\n  (:dependency_%d_cmi %s)\n  (:dependency_%d_cmti %s)\n  (:dependency_%d_vri %s)\n  (:dependency_%d_manifest %s)"
           index (object_path ^ ".cmt") index (object_path ^ ".cmi") index
           (object_path ^ ".cmti") index
           (path_in directory (stem ^ ".vri")) index
           (path_in directory (stem ^ ".verocaml-retained-interface")))
  |> String.concat "\n"

let dependency_directories dependencies =
  dependencies
  |> List.map (fun (directory, library, _) -> (directory, library))
  |> List.sort_uniq compare
  |> List.concat_map (fun (directory, library) ->
         [
           " --artifact-directory " ^ directory;
           " --artifact-directory "
           ^ path_in directory ("." ^ library ^ ".objs/byte");
         ])
  |> String.concat ""

let dependency_transport_stanza dependencies units =
  let rec stanzas reversed index = function
    | [] -> Ok (List.rev reversed |> String.concat "\n")
    | unit_name :: rest ->
        let owners =
          List.filter
            (fun (_, library, _) ->
              String.equal (String.capitalize_ascii library) unit_name)
            dependencies
          |> List.map (fun (directory, library, _) -> (directory, library))
          |> List.sort_uniq compare
        in
        (match owners with
        | [ (directory, library) ] ->
            [%log.trace "resolved dependency transport to one declared Dune library"
              ~stage:(Delator.Field.string "dune-stanza-generation")
              ~transport_unit:(Delator.Field.string unit_name)
              ~dependency_directory:(Delator.Field.string directory)
              ~dependency_library:(Delator.Field.string library)
              ~decision:(Delator.Field.string "accepted")];
            let path =
              path_in directory
                (Printf.sprintf ".%s.objs/byte/%s.cmi" library
                   (String.uncapitalize_ascii unit_name))
            in
            stanzas
              (Printf.sprintf "  (:dependency_transport_%d_cmi %s)" index path
              :: reversed)
              (index + 1) rest
        | [] | _ :: _ :: _ ->
            Error
              "dependency transport unit does not identify exactly one declared Dune library")
  in
  stanzas [] 0 units

let transport_stanza retained_library units =
  units
  |> List.mapi (fun index unit_name ->
         let stem = String.uncapitalize_ascii unit_name in
         Printf.sprintf "  (:transport_%d_cmi .%s.objs/byte/%s.cmi)" index
           retained_library stem)
  |> String.concat "\n"

let dune_stanza package destination retained_library unit_name arguments =
  let rec options transports dependency_transports = function
    | "--transport-unit" :: transport_unit :: rest
      when identifier transport_unit ->
        options (transport_unit :: transports) dependency_transports rest
    | "--dependency-transport-unit" :: transport_unit :: rest
      when identifier transport_unit ->
        options transports (transport_unit :: dependency_transports) rest
    | arguments ->
        ( List.rev transports,
          List.rev dependency_transports,
          arguments )
  in
  let rec triples reversed = function
    | [] -> Some (List.rev reversed)
    | dependency_directory :: dependency_library :: dependency_unit :: rest
      when dune_relative_path dependency_directory
           && identifier dependency_library && identifier dependency_unit ->
        triples
          ((dependency_directory, dependency_library, dependency_unit) :: reversed)
          rest
    | _ -> None
  in
  let transport_units, dependency_transport_units, arguments =
    options [] [] arguments
  in
  let transport_units = List.sort_uniq String.compare transport_units in
  let dependency_transport_units =
    List.sort_uniq String.compare dependency_transport_units
  in
  match triples [] arguments with
  | None -> usage ()
  | Some dependencies
    when package_name package && safe_relative_path destination
         && identifier retained_library && identifier unit_name ->
      let dependency_transport_stanza =
        dependency_transport_stanza dependencies dependency_transport_units
      in
      (match dependency_transport_stanza with
      | Error reason ->
          [%log.warn "rejected Dune dependency transport declaration"
            ~stage:(Delator.Field.string "dune-stanza")
            ~route:(Delator.Field.string "explicit-library-opt-in")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "ambiguous-dependency-transport")];
          prerr_endline ("verocaml-retained-interface: " ^ reason);
          2
      | Ok dependency_transport_stanza ->
      let stem = String.uncapitalize_ascii unit_name in
      let retained_object_path =
        "." ^ retained_library ^ ".objs/byte/" ^ stem
      and build_manifest = stem ^ ".verocaml-retained-interface"
      and install_manifest = stem ^ ".verocaml-retained-interface.install" in
      let dependency_stanza = dependency_stanza dependencies in
      let transport_stanza = transport_stanza retained_library transport_units in
      let dependency_stanza =
        [ transport_stanza; dependency_transport_stanza; dependency_stanza ]
        |> List.filter (fun stanza -> not (String.equal stanza ""))
        |> String.concat "\n"
      in
      let dependency_stanza =
        if String.equal dependency_stanza "" then ""
        else "\n" ^ dependency_stanza
      in
      Printf.printf
        {|(rule
 (targets %s.vri %s %s)
 (deps
  (sandbox always)
  (:emitter %%{bin:verocaml-retained-interface})
  (:cmt %s.cmt)
  (:cmi %s.cmi)
  (:cmti %s.cmti)%s)
 (action
  (progn
   (run %%{emitter} emit %%{cmt} %%{cmi} %%{cmti} %s.vri
    --artifact-directory .%s.objs/byte%s)
   (run %%{emitter} manifest %s %s.cmt %s.cmi %s.cmti %s.vri)
   (run %%{emitter} manifest %s %s.cmt %s.cmi %s.cmti %s.vri))))

(alias
 (name verocaml-retained-interfaces)
 (deps %s.vri %s))

(alias
 (name all)
 (deps %s.vri %s))

(install
 (package %s)
 (section lib)
 (files
  (%s.vri as %s/%s.vri)
  (%s as %s/%s)))
|}
        stem build_manifest install_manifest retained_object_path
        retained_object_path retained_object_path dependency_stanza stem
        retained_library (dependency_directories dependencies) build_manifest
        retained_object_path retained_object_path retained_object_path stem
        install_manifest stem stem stem stem stem build_manifest stem build_manifest
        package stem destination stem install_manifest destination build_manifest;
      [%log.info "generated reusable Dune retained-interface stanza"
        ~stage:(Delator.Field.string "dune-stanza")
        ~route:(Delator.Field.string "explicit-library-opt-in")
        ~transport_unit_count:(Delator.Field.int (List.length transport_units))
        ~dependency_transport_unit_count:
          (Delator.Field.int (List.length dependency_transport_units))
        ~dependency_count:(Delator.Field.int (List.length dependencies))
        ~decision:(Delator.Field.string "accepted")];
      0)
  | Some _ ->
      [%log.warn "rejected Dune retained-interface stanza request"
        ~stage:(Delator.Field.string "dune-stanza")
        ~route:(Delator.Field.string "explicit-library-opt-in")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "invalid-identifier-or-destination")];
      usage ()
[@@delator.instrument] [@@delator.level info]

let emit cmt cmi cmti output arguments =
  let rec options ordinary_cmis artifact_directories = function
    | [] -> Ok (List.rev ordinary_cmis, List.rev artifact_directories)
    | "--ordinary-cmi" :: filename :: rest ->
        options (filename :: ordinary_cmis) artifact_directories rest
    | "--artifact-directory" :: directory :: rest ->
        options ordinary_cmis (directory :: artifact_directories) rest
    | _ -> Error ()
  in
  match options [] [] arguments with
  | Error () -> usage ()
  | Ok (ordinary_cmis, artifact_directories) ->
      (match
         Cmt_input.emit_retained_interface_authority ~ordinary_cmis
           ~artifact_directories ~cmt ~cmi ~cmti ~output ()
       with
      | Ok () when Sys.file_exists output ->
          [%log.info "completed declared retained-interface emission"
            ~stage:(Delator.Field.string "post-typing-emitter")
            ~route:(Delator.Field.string "dune-rule")
            ~decision:(Delator.Field.string "accepted")];
          0
      | Ok () ->
          [%log.warn "retained-interface emission produced no declared target"
            ~stage:(Delator.Field.string "post-typing-emitter")
            ~route:(Delator.Field.string "dune-rule")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "no-retained-authority")];
          prerr_endline
            "verocaml-retained-interface: opted-in unit produced no retained interface; ensure its library uses verocaml.ppx -- --verocaml-retained";
          2
      | Error diagnostic ->
          [%log.warn "rejected declared retained-interface emission"
            ~stage:(Delator.Field.string "post-typing-emitter")
            ~route:(Delator.Field.string "dune-rule")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "invalid-artifact-family")];
          prerr_endline (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message);
          2)
[@@delator.instrument] [@@delator.level info]

let erase input output =
  match Cmt_input.erase_retained_interface_authority ~input ~output with
  | Ok () ->
      [%log.info "completed retained-interface CMI erasure"
        ~stage:(Delator.Field.string "ordinary-interface-erasure")
        ~decision:(Delator.Field.string "accepted")];
      0
  | Error diagnostic ->
      [%log.warn "rejected retained-interface CMI erasure"
        ~stage:(Delator.Field.string "ordinary-interface-erasure")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "invalid-artifact")];
      prerr_endline (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message);
      2
[@@delator.instrument] [@@delator.level info]

let main argv =
  match Array.to_list argv with
  | [ _; "erase-cmi"; input; output ] -> erase input output
  | _ :: "emit" :: cmt :: cmi :: cmti :: output :: arguments ->
      emit cmt cmi cmti output arguments
  | [ _; "manifest"; output; cmt; cmi; cmti; vri ] ->
      manifest output [ cmt; cmi; cmti; vri ]
  | _ :: "dune-stanza" :: package :: destination :: retained_library
    :: unit_name :: dependencies ->
      dune_stanza package destination retained_library unit_name dependencies
  | [ _; cmt; cmi; cmti ] | [ _; cmt; cmi; cmti; "" ] ->
      emit cmt cmi cmti (Filename.remove_extension cmi ^ ".vri") []
  | [ _; cmt; cmi; cmti; output ] -> emit cmt cmi cmti output []
  | _ -> usage ()
[@@delator.instrument] [@@delator.level info]

let () = exit (main Sys.argv)
