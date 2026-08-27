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
    "unsupported-target code";
  require_equal_string "<startup>" diagnostic.span.file
    "unsupported-target span"

let test_classifier_seams () =
  let partial_implementation =
    diagnostic_of_error
      (Cmt_input.classify_annots ~source_file:"partial.ml"
         (Cmt_format.Partial_implementation [||]))
  in
  require_classification
    (Diagnostic.Unsupported_input Partial_implementation)
    partial_implementation;
  require_equal_string "partial.ml" partial_implementation.span.file
    "partial implementation span";
  let partial_interface =
    diagnostic_of_error
      (Cmt_input.classify_annots ~source_file:"partial.mli"
         (Cmt_format.Partial_interface [||]))
  in
  require_classification
    (Diagnostic.Unsupported_input Partial_interface)
    partial_interface;
  require_equal_string "partial.mli" partial_interface.span.file
    "partial interface span";
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
        "malformed code";
      require_equal_string filename diagnostic.span.file "malformed span");
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
        "incompatible code";
      require_equal_string filename diagnostic.span.file "incompatible span")

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
      require_equal_string "implementation.ml" implementation.source_file
        "implementation source";
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
    "interface code";
  require_equal_string "interface.mli" diagnostic.span.file "interface span";
  require (diagnostic.span.start_pos.line >= 1)
    "interface diagnostic has no source line"

let test_pack filename =
  let diagnostic = diagnostic_of_error (Cmt_input.load filename) in
  require_classification (Diagnostic.Unsupported_input Packed) diagnostic;
  require_equal_string "VERO_UNSUPPORTED_PACK" diagnostic.code "pack code"

let () =
  if Array.length Sys.argv <> 4 then
    fail "usage: %s IMPLEMENTATION.cmt INTERFACE.cmti PACK.cmt" Sys.argv.(0);
  test_bounds ();
  test_unsupported_target_precedes_io ();
  test_classifier_seams ();
  test_malformed ();
  test_incompatible_magic ();
  test_implementation Sys.argv.(1);
  test_interface Sys.argv.(2);
  test_pack Sys.argv.(3);
  print_endline "CMT input checks passed"
