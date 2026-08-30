type import = {
  unit_name : string;
  crc : string option;
}

type implementation = {
  metadata : Cmt_format.cmt_infos;
  embedded_interface_metadata : Cmi_format.cmi_infos_lazy option;
  raw_artifact_digest : string;
  filename : string;
  source_file : string;
  unit_name : string;
  interface_digest : string option;
  structure : Typedtree.structure;
  imports : import array;
  compiler_arguments : string array;
  source_digest : string option;
  build_directory : string;
  load_path_visible : string list;
  load_path_hidden : string list;
  embedded_interface : bool;
  explicit_interface : bool;
  interface_unit_name : string option;
  interface_implementation_unit_name : string option;
  interface_parameter_count : int;
  interface_imports : import array;
  implementation_family_markers : string list;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
  interface_family_markers : string list;
  interface_mode_signatures : (string * string option) list;
  interface_finite_signatures : (string * string option) list;
  declaration_dependency_count : int;
  has_implementation_shape : bool;
  identifier_occurrence_count : int;
}

let argument_after flag arguments =
  let rec loop = function
    | [] | [ _ ] -> []
    | candidate :: value :: rest ->
        if String.equal candidate flag then value :: loop rest
        else loop (value :: rest)
  in
  loop (Array.to_list arguments)

let retained_ppx_command command =
  match String.split_on_char ' ' command |> List.filter (( <> ) "") with
  | executable :: arguments ->
      let executable =
        String.trim executable
        |> String.map (fun character ->
               if Char.equal character '\'' || Char.equal character '"' then
                 ' '
               else character)
        |> String.trim
      in
      let basename = Filename.basename executable in
      (String.equal basename "vero_ppx.exe"
      || String.equal basename "verocaml-ppx")
      && List.exists (String.equal "--keep-ghost") arguments
  | [] -> false

let retained_preprocessing implementation =
  let ppx = argument_after "-ppx" implementation.compiler_arguments in
  (List.length ppx = 1 && retained_ppx_command (List.hd ppx))
  ||
  (implementation.implementation_metadata_valid
  && implementation.implementation_family_markers = [ "retained-v1" ]
  && Filename.check_suffix implementation.source_file ".pp.ml"
  && Array.exists (String.equal "-impl") implementation.compiler_arguments)

let diagnostic_for_location classification ~source_file location =
  Diagnostic.make classification
    (Diagnostic.span_of_location ~fallback_file:source_file location)

let classify_annots ~source_file = function
  | Cmt_format.Implementation structure -> Ok structure
  | Interface signature ->
      Error
        (diagnostic_for_location (Unsupported_input Interface) ~source_file
           signature.Typedtree.sig_sloc)
  | Packed _ ->
      Error
        (Diagnostic.make (Unsupported_input Packed)
           (Diagnostic.file_span source_file))
  | Partial_implementation parts ->
      let location =
        if Array.length parts = 0 then Location.none
        else
          match parts.(0) with
          | Partial_structure structure -> (
              match structure.Typedtree.str_items with
              | item :: _ -> item.Typedtree.str_loc
              | [] -> Location.none)
          | Partial_structure_item item -> item.Typedtree.str_loc
          | Partial_expression expression -> expression.Typedtree.exp_loc
          | Partial_pattern (_, pattern) -> pattern.Typedtree.pat_loc
          | Partial_class_expr class_expression ->
              class_expression.Typedtree.cl_loc
          | Partial_signature signature -> signature.Typedtree.sig_sloc
          | Partial_signature_item item -> item.Typedtree.sig_loc
          | Partial_module_type module_type -> module_type.Typedtree.mty_loc
      in
      Error
        (diagnostic_for_location
           (Unsupported_input Partial_implementation)
           ~source_file location)
  | Partial_interface parts ->
      let location =
        if Array.length parts = 0 then Location.none
        else
          match parts.(0) with
          | Partial_structure structure -> (
              match structure.Typedtree.str_items with
              | item :: _ -> item.Typedtree.str_loc
              | [] -> Location.none)
          | Partial_structure_item item -> item.Typedtree.str_loc
          | Partial_expression expression -> expression.Typedtree.exp_loc
          | Partial_pattern (_, pattern) -> pattern.Typedtree.pat_loc
          | Partial_class_expr class_expression ->
              class_expression.Typedtree.cl_loc
          | Partial_signature signature -> signature.Typedtree.sig_sloc
          | Partial_signature_item item -> item.Typedtree.sig_loc
          | Partial_module_type module_type -> module_type.Typedtree.mty_loc
      in
      Error
        (diagnostic_for_location (Unsupported_input Partial_interface)
           ~source_file location)

type magic_classification =
  | Readable_abi
  | Malformed
  | Incompatible

let has_prefix string prefix =
  String.length string >= String.length prefix
  && String.sub string 0 (String.length prefix) = prefix

let classify_magic filename =
  let magic_length = String.length Config.cmt_magic_number in
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let bytes = really_input_string channel magic_length in
      if
        String.equal bytes Config.cmt_magic_number
        || String.equal bytes Config.cmi_magic_number
      then Readable_abi
      else if has_prefix bytes "Caml1999" then Incompatible
      else Malformed)

let artifact_digest filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length |> Digest.string |> Digest.to_hex)

let source_file filename info =
  match info.Cmt_format.cmt_sourcefile with
  | Some source_file -> source_file
  | None -> filename

let imports_of_array imports =
  Array.map
    (fun import ->
      {
        unit_name =
          Compilation_unit.Name.to_string (Import_info.name import);
        crc = Option.map Digest.to_hex (Import_info.crc import);
      })
    imports

let imports info = imports_of_array info.Cmt_format.cmt_imports

let family_markers attributes =
  let prefix = "verocaml.internal.artifact_family." in
  List.filter_map
    (fun attribute ->
      if
        attribute.Parsetree.attr_loc.Location.loc_ghost
        && String.starts_with ~prefix attribute.attr_name.txt
      then
        Some
          (String.sub attribute.attr_name.txt (String.length prefix)
             (String.length attribute.attr_name.txt - String.length prefix))
      else None)
    attributes

type implementation_metadata = {
  implementation_family_markers : string list;
  verification_scope_markers : string list;
  implementation_metadata_valid : bool;
}

let implementation_metadata structure =
  let family_prefix = "verocaml.internal.artifact_family." in
  let scope_prefix = "verocaml.internal.verification_scope." in
  let families = ref [] and scopes = ref [] and valid = ref true in
  let collect prefix target attribute =
    if String.starts_with ~prefix attribute.Parsetree.attr_name.txt then
      if
        attribute.attr_loc.Location.loc_ghost
        && attribute.attr_name.loc.loc_ghost
        && attribute.attr_payload = Parsetree.PStr []
      then
        target :=
          String.sub attribute.attr_name.txt (String.length prefix)
            (String.length attribute.attr_name.txt - String.length prefix)
          :: !target
      else valid := false
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      attribute =
        (fun self attribute ->
          collect family_prefix families attribute;
          collect scope_prefix scopes attribute;
          default.attribute self attribute);
      value_binding =
        (fun self binding ->
          List.iter (collect family_prefix families) binding.vb_attributes;
          default.value_binding self binding);
      type_declaration =
        (fun self declaration ->
          List.iter (collect family_prefix families) declaration.typ_attributes;
          default.type_declaration self declaration);
    }
  in
  iterator.structure iterator structure;
  {
    implementation_family_markers =
      List.sort_uniq String.compare !families;
    verification_scope_markers = List.rev !scopes;
    implementation_metadata_valid = !valid;
  }

let interface_family_markers interface =
  Subst.Lazy.force_signature interface.Cmi_format.cmi_sign
  |> List.concat_map (function
       | Types.Sig_value (_, description, _) ->
           family_markers description.Types.val_attributes
       | Types.Sig_type (_, declaration, _, _) ->
           family_markers declaration.Types.type_attributes
       | Types.Sig_module (_, _, declaration, _, _) ->
           family_markers declaration.Types.md_attributes
       | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
       | Types.Sig_class_type _ ->
           [])
  |> List.sort_uniq String.compare

let string_payload_attribute name attributes =
  List.find_map
    (fun attribute ->
      if
        attribute.Parsetree.attr_loc.Location.loc_ghost
        && attribute.attr_name.loc.loc_ghost
        && String.equal attribute.attr_name.txt name
      then
        match attribute.attr_payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc = Pexp_constant (Pconst_string (value, _, _));
                        _;
                      },
                      [] );
                _;
              };
            ] ->
            Some value
        | _ -> None
      else None)
    attributes

let mode_signature =
  string_payload_attribute "verocaml.internal.mode_signature"

let finite_signature attributes =
  let matching =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt
          "verocaml.internal.finite_signature")
      attributes
  in
  match matching with
  | [] -> None
  | _ :: _ :: _ -> raise (Failure "duplicate finite signature metadata")
  | [ attribute ] ->
      if
        not
          (attribute.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost)
      then raise (Failure "non-ghost finite signature metadata")
      else
        let payload =
          match attribute.attr_payload with
          | PStr
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp_desc =
                            Pexp_constant (Pconst_string (value, _, _));
                          _;
                        },
                        [] );
                  _;
                };
              ] ->
              value
          | _ -> raise (Failure "malformed finite signature metadata")
        in
        if not (String.starts_with ~prefix:"v1|" payload) then
          raise (Failure "unknown finite signature metadata version")
        else
          let entries =
            String.sub payload 3 (String.length payload - 3)
            |> fun body ->
            if body = "" then [] else String.split_on_char ';' body
          in
          List.iteri
            (fun expected entry ->
              match String.split_on_char '=' entry with
              | [ key; ("finite" | "default") ] -> (
                  match String.index_opt key ':' with
                  | Some separator ->
                      let ordinal =
                        String.sub key 0 separator |> int_of_string_opt
                      in
                      let label =
                        String.sub key (separator + 1)
                          (String.length key - separator - 1)
                      in
                      if ordinal <> Some expected || label = "" then
                        raise (Failure "malformed finite signature vector")
                  | None ->
                      raise (Failure "malformed finite signature vector"))
              | _ -> raise (Failure "malformed finite signature vector"))
            entries;
          Some payload

let interface_signatures attribute interface =
  let root = Subst.Lazy.force_signature interface.Cmi_format.cmi_sign in
  let key kind prefix ident =
    String.concat "." (List.rev (Ident.name ident :: prefix)) |> ( ^ ) kind
  in
  let rec signature prefix items =
    let module_types =
      List.filter_map
        (function
          | Types.Sig_modtype (ident, declaration, _) ->
              Option.map (fun typ -> (ident, typ)) declaration.Types.mtd_type
          | _ -> None)
        items
    in
    let rec module_signature seen = function
      | Types.Mty_signature nested -> Some nested
      | Mty_strengthen (nested, _, _) -> module_signature seen nested
      | Mty_ident (Path.Pident ident) -> (
          if List.exists (Ident.same ident) seen then None
          else
            match
              List.find_opt
                (fun (candidate, _) -> Ident.same candidate ident)
                module_types
            with
            | Some (_, nested) -> module_signature (ident :: seen) nested
            | None -> None)
      | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None
    in
    List.concat_map
      (function
        | Types.Sig_value (ident, description, _) ->
            [
              ( key "value:" prefix ident,
                attribute description.Types.val_attributes );
            ]
        | Types.Sig_type (ident, declaration, _, _) ->
            [
              ( key "type:" prefix ident,
                attribute declaration.Types.type_attributes );
            ]
        | Types.Sig_module (ident, _, declaration, _, _) -> (
            match module_signature [] declaration.Types.md_type with
            | Some nested -> signature (Ident.name ident :: prefix) nested
            | None -> [])
        | Types.Sig_typext _ | Types.Sig_modtype _ | Types.Sig_class _
        | Types.Sig_class_type _ ->
            [])
      items
  in
  signature [] root
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let interface_mode_signatures = interface_signatures mode_signature
let interface_finite_signatures interface =
  let signatures = interface_signatures finite_signature interface in
  let rec duplicates = function
    | (left, _) :: ((right, _) :: _ as rest) ->
        if String.equal left right then
          raise (Failure "duplicate finite signature path")
        else duplicates rest
    | [] | [ _ ] -> ()
  in
  duplicates signatures;
  signatures

let embedded_interface_metadata = function
  | None -> (false, None, None, 0, [||], [], [], [])
  | Some interface ->
      let implementation_unit_name =
        match interface.Cmi_format.cmi_kind with
        | Cmi_format.Normal { cmi_impl; _ } ->
            Some (Compilation_unit.name_as_string cmi_impl)
        | Cmi_format.Parameter -> None
      in
      ( true,
        Some
          (Compilation_unit.Name.to_string interface.Cmi_format.cmi_name),
        implementation_unit_name,
        List.length interface.Cmi_format.cmi_params,
        imports_of_array interface.Cmi_format.cmi_crcs,
        interface_family_markers interface,
        interface_mode_signatures interface,
        interface_finite_signatures interface )

let adjacent_interface filename =
  let basename =
    try Filename.chop_extension filename with Invalid_argument _ -> filename
  in
  let filename = basename ^ ".cmi" in
  if Sys.file_exists filename then Some (Cmi_format.read_cmi_lazy filename)
  else None

let implementation_interface_digest info =
  match info.Cmt_format.cmt_interface_digest with
  | Some digest -> Some digest
  | None ->
      let self = Compilation_unit.name info.Cmt_format.cmt_modname in
      let matching =
        Array.to_list info.Cmt_format.cmt_imports
        |> List.filter (fun imported ->
               Compilation_unit.Name.equal (Import_info.name imported) self)
      in
      (match matching with [ imported ] -> Import_info.crc imported | _ -> None)

let interface_digest_matches info interface =
  match implementation_interface_digest info with
  | None -> false
  | Some expected ->
      let matching =
        Array.to_list interface.Cmi_format.cmi_crcs
        |> List.filter (fun imported ->
               Compilation_unit.Name.equal
                 (Import_info.name imported)
                 interface.Cmi_format.cmi_name)
      in
      (match matching with
      | [ imported ] -> Import_info.crc imported = Some expected
      | _ -> false)

let explicit_interface_identity_matches info interface =
  let implementation_name =
    Compilation_unit.name_as_string info.Cmt_format.cmt_modname
  in
  let interface_name =
    Compilation_unit.Name.to_string interface.Cmi_format.cmi_name
  in
  let interface_implementation_name =
    match interface.Cmi_format.cmi_kind with
    | Cmi_format.Normal { cmi_impl; _ } ->
        Some (Compilation_unit.name_as_string cmi_impl)
    | Cmi_format.Parameter -> None
  in
  String.equal implementation_name interface_name
  && interface_implementation_name = Some implementation_name

let explicit_interface_unsupported interface =
  if interface.Cmi_format.cmi_params <> [] then
    Some (Diagnostic.Unsupported_input Explicit_cmi_parameters)
  else
    match interface.Cmi_format.cmi_kind with
    | Cmi_format.Normal { cmi_arg_for = Some _; _ } ->
        Some (Unsupported_input Explicit_cmi_argument_for)
    | Normal { cmi_arg_for = None; _ } | Parameter -> None

exception Unsupported_interface_metadata of Diagnostic.classification

type lexical_path = {
  absolute : bool;
  components : string list;
}

let lexical_path path =
  let absolute = not (Filename.is_relative path) in
  let separator = Filename.dir_sep.[0] in
  let components =
    String.split_on_char separator path
    |> List.fold_left
         (fun components -> function
           | "" | "." -> components
           | ".." -> (
               match components with
               | component :: rest when not (String.equal component "..") ->
                   rest
               | _ when absolute -> components
               | _ -> ".." :: components)
           | component -> component :: components)
         []
    |> List.rev
  in
  { absolute; components }

let lexical_path_string path =
  let body = String.concat Filename.dir_sep path.components in
  if path.absolute then Filename.dir_sep ^ body
  else if String.equal body "" then "."
  else body

let append_lexical base relative =
  lexical_path
    (Filename.concat (lexical_path_string base) (lexical_path_string relative))

let path_from_build_directory build_directory path =
  let path = lexical_path path in
  if path.absolute then path else append_lexical build_directory path

let rec drop_common left right =
  match (left, right) with
  | left :: lefts, right :: rights when String.equal left right ->
      drop_common lefts rights
  | _ -> (left, right)

let path_relative_to ~from target =
  if from.absolute <> target.absolute then
    raise
      (Failure
         (Printf.sprintf "relocated load-path roots differ: from=%s target=%s"
            (lexical_path_string from) (lexical_path_string target)))
  else
    let remaining_from, remaining_target =
      drop_common from.components target.components
    in
    {
      absolute = false;
      components =
        List.init (List.length remaining_from) (Fun.const "..")
        @ remaining_target;
    }

let path_has_prefix ~prefix path =
  prefix.absolute = path.absolute
  &&
  let rec loop prefix path =
    match (prefix, path) with
    | [], _ -> true
    | prefix :: prefixes, path :: paths when String.equal prefix path ->
        loop prefixes paths
    | _ -> false
  in
  loop prefix.components path.components

let relocated_paths ~filename ~build_directory ~compiler_arguments load_path =
  let absolute_filename =
    if Filename.is_relative filename then Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  let artifact_directory = lexical_path (Filename.dirname absolute_filename) in
  let build_directory = lexical_path build_directory in
  let output_directories =
    argument_after "-o" compiler_arguments
    |> List.map Filename.dirname |> List.sort_uniq String.compare
  in
  let output_directory =
    match output_directories with
    | [] -> build_directory
    | [ output ] -> path_from_build_directory build_directory output
    | _ -> raise (Failure "ambiguous compiler output directories")
  in
  let relocate original =
    if original = output_directory then artifact_directory
    else if path_has_prefix ~prefix:build_directory original then
      append_lexical artifact_directory
        (path_relative_to ~from:output_directory original)
    else original
  in
  let resolve path =
    path_from_build_directory build_directory path |> relocate
    |> lexical_path_string
  in
  ( relocate build_directory |> lexical_path_string,
    List.map resolve load_path.Load_path.visible,
    List.map resolve load_path.Load_path.hidden )

let load_internal ?int_size ?interface_info
    (filename [@delator.field Fun.id]) =
  match Int_bounds.check_target ?int_size () with
  | Error _ as error -> error
  | Ok () -> (
      let input_error classification =
        Error
          (Diagnostic.make classification (Diagnostic.file_span filename))
      in
      match classify_magic filename with
      | Incompatible -> input_error Incompatible_magic
      | Malformed -> input_error Malformed_input
      | Readable_abi -> (
          try
            (* The accepted loader threat model requires the CMT path to remain
               stable for the duration of this load.  Capture the digest before
               decoding so the raw bytes are part of the exact decoded
               artifact's narrow callback identity. *)
            let raw_artifact_digest = artifact_digest filename in
            let embedded_interface_info, info =
              match Cmt_format.read filename with
              | embedded_interface, Some info -> (embedded_interface, info)
              | _, None -> raise (Failure "missing CMT metadata")
            in
            if not (String.equal raw_artifact_digest (artifact_digest filename))
            then raise (Failure "CMT changed while it was being loaded");
            let embedded_interface =
              Option.is_some embedded_interface_info
            in
            (* The compiler embeds the implementation interface only when no
               separately compiled interface constrained the unit.  A CMTI is
               optional, so its presence cannot authenticate this boundary. *)
            let explicit_interface = not embedded_interface in
            let interface_info =
              match interface_info with
              | Some explicit -> Some explicit
              | None -> (
                  match
                    (adjacent_interface filename, embedded_interface_info)
                  with
                  | Some adjacent, _ -> Some adjacent
                  | None, Some embedded -> Some embedded
                  | None, None ->
                      raise
                        (Failure
                           "separate implementation interface has no adjacent CMI"))
            in
            (match interface_info with
            | Some interface -> (
                match explicit_interface_unsupported interface with
                | Some classification ->
                    raise (Unsupported_interface_metadata classification)
                | None ->
                    if not (explicit_interface_identity_matches info interface)
                    then raise (Failure "explicit CMT/CMI identity mismatch"))
            | None -> raise (Failure "missing implementation interface"));
            if
              match interface_info with
              | Some interface -> not (interface_digest_matches info interface)
              | None -> false
            then raise (Failure "CMT/CMI interface digest mismatch");
            let source_file = source_file filename info in
            match classify_annots ~source_file info.Cmt_format.cmt_annots with
            | Error _ as error -> error
            | Ok structure ->
                let implementation_metadata = implementation_metadata structure in
                let ( _interface_present,
                      interface_unit_name,
                      interface_implementation_unit_name,
                      interface_parameter_count,
                      interface_imports,
                      interface_family_markers,
                      interface_mode_signatures,
                      interface_finite_signatures ) =
                  embedded_interface_metadata interface_info
                in
                let build_directory, load_path_visible, load_path_hidden =
                  relocated_paths ~filename
                    ~build_directory:info.Cmt_format.cmt_builddir
                    ~compiler_arguments:info.Cmt_format.cmt_args
                    info.Cmt_format.cmt_loadpath
                in
                let implementation =
                  {
                    metadata = info;
                    embedded_interface_metadata = interface_info;
                    raw_artifact_digest;
                    filename;
                    source_file;
                    unit_name =
                      Compilation_unit.name_as_string info.Cmt_format.cmt_modname;
                    interface_digest =
                      Option.map Digest.to_hex
                        (implementation_interface_digest info);
                    structure;
                    imports = imports info;
                    compiler_arguments =
                      Array.copy info.Cmt_format.cmt_args;
                    source_digest = info.Cmt_format.cmt_source_digest;
                    build_directory;
                    load_path_visible;
                    load_path_hidden;
                    embedded_interface;
                    explicit_interface;
                    interface_unit_name;
                    interface_implementation_unit_name;
                    interface_parameter_count;
                    interface_imports;
                    implementation_family_markers =
                      implementation_metadata.implementation_family_markers;
                    verification_scope_markers =
                      implementation_metadata.verification_scope_markers;
                    implementation_metadata_valid =
                      implementation_metadata.implementation_metadata_valid;
                    interface_family_markers;
                    interface_mode_signatures;
                    interface_finite_signatures;
                    declaration_dependency_count =
                      List.length info.Cmt_format.cmt_declaration_dependencies;
                    has_implementation_shape =
                      Option.is_some info.Cmt_format.cmt_impl_shape;
                    identifier_occurrence_count =
                      Array.length info.Cmt_format.cmt_ident_occurrences;
                  }
                in
                [%log.debug "loaded compiler artifact"
                  ~filename
                  ~unit_name:(Delator.Field.string implementation.unit_name)
                  ~imports:
                    (Delator.Field.int (Array.length implementation.imports))
                  ~visible_load_paths:
                    (Delator.Field.int
                       (List.length implementation.load_path_visible))
                  ~hidden_load_paths:
                    (Delator.Field.int
                       (List.length implementation.load_path_hidden))
                  ~retained:
                    (Delator.Field.bool
                       (retained_preprocessing implementation))];
                Ok implementation
          with
          | Cmt_format.Error _
          | Cmi_format.Error _
          | End_of_file
          | Failure _
          | Invalid_argument _ ->
              input_error Malformed_input
          | Unsupported_interface_metadata classification ->
              input_error classification
          | Sys_error _ -> input_error Input_io_error)
      | exception End_of_file -> input_error Malformed_input
      | exception Sys_error _ -> input_error Input_io_error)
[@@delator.instrument] [@@delator.level debug]

let load ?int_size (filename [@delator.field Fun.id]) =
  load_internal ?int_size filename
[@@delator.instrument] [@@delator.level trace]

let load_with_interface ?int_size
    ~cmt:(cmt [@delator.field Fun.id])
    ~cmi:(cmi [@delator.field Fun.id]) () =
  match Int_bounds.check_target ?int_size () with
  | Error _ as error -> error
  | Ok () ->
      let input_error classification =
        Error (Diagnostic.make classification (Diagnostic.file_span cmi))
      in
      (match classify_magic cmi with
      | Incompatible -> input_error Incompatible_magic
      | Malformed -> input_error Malformed_input
      | Readable_abi -> (
          try
            let interface_info = Cmi_format.read_cmi_lazy cmi in
            match explicit_interface_unsupported interface_info with
            | Some classification -> input_error classification
            | None ->
                load_internal ?int_size ~interface_info cmt
          with
          | Cmi_format.Error _ | End_of_file | Failure _ | Invalid_argument _ ->
              input_error Malformed_input)
      | exception End_of_file -> input_error Malformed_input
      | exception Sys_error _ -> input_error Input_io_error)
[@@delator.instrument] [@@delator.level debug]
