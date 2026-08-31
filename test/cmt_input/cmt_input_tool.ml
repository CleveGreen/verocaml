let fail format = Printf.ksprintf failwith format

let require condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format

let require_equal_string expected actual label =
  if not (String.equal expected actual) then
    fail "%s: expected %S, found %S" label expected actual

let require_equal_z expected actual label =
  if not (Z.equal expected actual) then
    fail "%s: expected %s, found %s" label (Z.to_string expected)
      (Z.to_string actual)

let diagnostic_of_error = function
  | Error diagnostic -> diagnostic
  | Ok _ -> fail "expected a classified diagnostic"

let require_classification expected diagnostic =
  if diagnostic.Diagnostic.classification <> expected then
    fail "unexpected diagnostic classification for %s" diagnostic.code

let test_bounds () =
  require_equal_z (Z.of_string "-4611686018427387904") Int_bounds.minimum
    "hardcoded minimum";
  require_equal_z (Z.of_string "4611686018427387903") Int_bounds.maximum
    "hardcoded maximum";
  require_equal_z (Z.of_int Stdlib.min_int) Int_bounds.minimum
    "runtime minimum";
  require_equal_z (Z.of_int Stdlib.max_int) Int_bounds.maximum
    "runtime maximum";
  require (Int_bounds.expected_int_size = 63) "expected width is not 63";
  require (Int_bounds.check_target () = Ok ())
    "pinned runtime did not pass target check"

let test_unsupported_target_precedes_io () =
  let diagnostic =
    diagnostic_of_error
      (Cmt_input.load ~int_size:31 "/this/path/must/not/be-read.cmt")
  in
  require_classification
    (Diagnostic.Unsupported_target
       { expected_int_size = 63; actual_int_size = 31 })
    diagnostic;
  require_equal_string "VERO_UNSUPPORTED_TARGET" diagnostic.code
    "unsupported-target code"

let test_classifier_seams () =
  let partial_implementation =
    diagnostic_of_error
      (Cmt_input.classify_annots ~source_file:"partial.ml"
         (Cmt_format.Partial_implementation [||]))
  in
  require_classification
    (Diagnostic.Unsupported_input Partial_implementation)
    partial_implementation;
  let partial_interface =
    diagnostic_of_error
      (Cmt_input.classify_annots ~source_file:"partial.mli"
         (Cmt_format.Partial_interface [||]))
  in
  require_classification
    (Diagnostic.Unsupported_input Partial_interface)
    partial_interface;
  let packed =
    diagnostic_of_error
      (Cmt_input.classify_annots ~source_file:"seam-pack.ml"
         (Cmt_format.Packed ([], [])))
  in
  require_classification (Diagnostic.Unsupported_input Packed) packed

let with_temp_file prefix contents function_ =
  let filename = Filename.temp_file prefix ".cmt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      let channel = open_out_bin filename in
      output_string channel contents;
      close_out channel;
      function_ filename)

let test_malformed () =
  with_temp_file "verocaml-malformed" "not a cmt" (fun filename ->
      let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
      require_classification Diagnostic.Malformed_input diagnostic;
      require_equal_string "VERO_MALFORMED_INPUT" diagnostic.code
        "malformed code");
  with_temp_file "verocaml-truncated" Config.cmt_magic_number (fun filename ->
      let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
      require_classification Diagnostic.Malformed_input diagnostic;
      require_equal_string "VERO_MALFORMED_INPUT" diagnostic.code
        "truncated CMT code")

let test_incompatible_magic () =
  let magic = Bytes.of_string Config.cmt_magic_number in
  let last = Bytes.length magic - 1 in
  Bytes.set magic last
    (if Char.equal (Bytes.get magic last) '0' then '1' else '0');
  with_temp_file "verocaml-incompatible" (Bytes.to_string magic) (fun filename ->
      let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
    require_classification Diagnostic.Incompatible_magic diagnostic;
    require_equal_string "VERO_INCOMPATIBLE_CMT" diagnostic.code
      "incompatible code")

let require_positive value label =
  require (value > 0) "%s was not observed in the pinned Typedtree" label

let path_suffix suffix path =
  let path_length = String.length path and suffix_length = String.length suffix in
  path_length >= suffix_length
  && String.sub path (path_length - suffix_length) suffix_length = suffix

let require_resolved_path surface suffix =
  require
    (List.exists (path_suffix suffix) surface.Typedtree_adapter.resolved_call_paths)
    "resolved call path ending in %S was not observed" suffix

let test_implementation filename =
  match Cmt_input.load filename with
  | Error diagnostic ->
      fail "implementation rejected as %s: %s" diagnostic.code diagnostic.message
  | Ok implementation ->
      require
        (path_suffix "implementation.ml" implementation.source_file)
        "implementation source did not identify implementation.ml";
      require_equal_string "Implementation" implementation.unit_name
        "implementation unit";
      require (Option.is_some implementation.interface_digest)
        "implementation interface digest missing";
      require implementation.embedded_interface
        "implementation embedded interface missing";
      require
        (implementation.interface_unit_name = Some "Implementation")
        "embedded interface unit mismatch";
      require
        (implementation.interface_implementation_unit_name
        = Some "Implementation")
        "embedded implementation unit mismatch";
      require (implementation.interface_parameter_count = 0)
        "unexpected interface parameters";
      require (Option.is_some implementation.source_digest)
        "source digest missing";
      require implementation.has_implementation_shape
        "implementation shape missing";
      require
        (Array.exists
           (fun argument -> String.equal argument "-bin-annot")
           implementation.compiler_arguments)
        "compiler arguments did not retain -bin-annot";
      require
        (Array.to_list implementation.imports
        |> List.map (fun (import : Cmt_input.import) ->
               (import.Cmt_input.unit_name, import.crc))
        |> List.sort compare
        =
        (Array.to_list implementation.interface_imports
        |> List.map (fun (import : Cmt_input.import) ->
               (import.Cmt_input.unit_name, import.crc))
        |> List.sort compare))
        "CMT and embedded interface import metadata differ";
      let surface = Typedtree_adapter.probe implementation.structure in
      require_positive surface.structure_items "structure items";
      require_positive surface.value_bindings "value bindings";
      require_positive surface.expression_metadata "expression metadata";
      require_positive surface.pattern_metadata "pattern metadata";
      require_positive surface.pattern_modes "pattern modes";
      require_positive surface.parameter_modes "parameter modes";
      require_positive surface.identifier_unique_uses "identifier unique uses";
      require_positive surface.resolved_pattern_barriers "pattern barriers";
      require_positive surface.resolved_field_barriers "field barriers";
      require_positive surface.field_reads "field reads";
      require_positive surface.field_writes "field writes";
      require_positive surface.mutable_local_bindings "mutable local bindings";
      require_positive surface.mutable_local_reads "mutable local reads";
      require_positive surface.mutable_local_writes "mutable local writes";
      require_positive surface.resolved_calls "resolved calls";
      require_resolved_path surface "Stdlib.ref";
      require_resolved_path surface "Stdlib.!";
      require_resolved_path surface "Stdlib.:="

let test_interface filename =
  let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
  require_classification (Diagnostic.Unsupported_input Interface) diagnostic;
  require_equal_string "VERO_UNSUPPORTED_INTERFACE" diagnostic.code
    "interface code"

let test_pack filename =
  let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
  require_classification (Diagnostic.Unsupported_input Packed) diagnostic;
  require_equal_string "VERO_UNSUPPORTED_PACK" diagnostic.code "pack code"

let make_import unit_name crc =
  let name = Compilation_unit.Name.of_string unit_name in
  let crc_with_unit =
    Option.map
      (fun crc -> (Compilation_unit.of_string unit_name, crc))
      crc
  in
  Import_info.create name ~crc_with_unit

let forge_interface_identity input output unit_name =
  let interface = Cmi_format.read_cmi_lazy input in
  let original_name = interface.Cmi_format.cmi_name in
  let cmi_crcs =
    Array.map
      (fun imported ->
        if Compilation_unit.Name.equal (Import_info.name imported) original_name
        then make_import unit_name (Import_info.crc imported)
        else imported)
      interface.Cmi_format.cmi_crcs
  in
  let cmi_kind =
    match interface.Cmi_format.cmi_kind with
    | Cmi_format.Normal metadata ->
        Cmi_format.Normal
          {
            metadata with
            cmi_impl = Compilation_unit.of_string unit_name;
          }
    | Parameter -> fail "%s is a parameter CMI" input
  in
  let interface =
    {
      interface with
      Cmi_format.cmi_name = Compilation_unit.Name.of_string unit_name;
      cmi_kind;
      cmi_crcs;
    }
  in
  let channel = open_out_bin output in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> ignore (Cmi_format.output_cmi output channel interface))

let test_rejected_implementation filename =
  let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
  require_classification Diagnostic.Malformed_input diagnostic;
  print_endline "ordinary CMT/CMI identity rejected"

let canonical_path path =
  (if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path)
  |> Unix.realpath

let test_relocated_load_path filename expected =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic ->
        fail "relocated implementation rejected as %s: %s" diagnostic.code
          diagnostic.message
  in
  let expected = List.map canonical_path expected in
  let label paths path =
    let basename = Filename.basename path in
    List.find_opt
      (fun expected -> String.equal (Filename.basename expected) basename)
      paths
  in
  let raw_order =
    implementation.metadata.Cmt_format.cmt_loadpath.visible
    |> List.filter_map (label expected)
    |> List.map Filename.basename
  in
  let relocated_order =
    implementation.load_path_visible
    |> List.filter_map (fun path ->
           let path = canonical_path path in
           Option.map Filename.basename
             (List.find_opt (String.equal path) expected))
  in
  require (raw_order = relocated_order)
    "visible load-path precedence changed: raw=%s relocated=%s"
    (String.concat "," raw_order) (String.concat "," relocated_order);
  require (List.length relocated_order = List.length expected)
    "not every expected relocated load path was retained";
  print_endline
    ("relocated load-path order=" ^ String.concat "," relocated_order)

let () =
  match Array.to_list Sys.argv with
  | [ _; implementation; interface; pack ] ->
      test_bounds ();
      test_unsupported_target_precedes_io ();
      test_classifier_seams ();
      test_malformed ();
      test_incompatible_magic ();
      test_implementation implementation;
      test_interface interface;
      test_pack pack;
      print_endline "CMT input checks passed"
  | [ _; "forge-interface-identity"; input; output; unit_name ] ->
      forge_interface_identity input output unit_name
  | [ _; "rejected-implementation"; filename ] ->
      test_rejected_implementation filename
  | _ :: "relocated-load-path" :: filename :: expected ->
      test_relocated_load_path filename expected
  | _ ->
      fail
        "usage: %s IMPLEMENTATION.cmt INTERFACE.cmti PACK.cmt | \
         forge-interface-identity INPUT OUTPUT UNIT | \
         rejected-implementation CMT | relocated-load-path CMT PATH..."
        Sys.argv.(0)
