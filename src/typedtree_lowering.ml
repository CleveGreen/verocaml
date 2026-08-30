let lower ?allow_imported_opens implementation =
  [%log.debug "begin typedtree lowering"
    ~unit_name:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~source_file:(Delator.Field.string implementation.source_file)];
  let authenticated_source_text =
    match implementation.Cmt_input.source_digest with
    | None -> None
    | Some expected_digest ->
        let candidates =
          [
            implementation.source_file;
            Filename.concat implementation.build_directory
              implementation.source_file;
            Filename.concat
              (Filename.dirname implementation.filename)
              (Filename.basename implementation.source_file);
          ]
        in
        let rec read = function
          | [] -> None
          | filename :: rest -> (
              try
                let channel = open_in_bin filename in
                let length = in_channel_length channel in
                let text = really_input_string channel length in
                close_in channel;
                if String.equal (Digest.string text) expected_digest then
                  Some text
                else read rest
              with Sys_error _ -> read rest)
        in
        read candidates
  in
  let proof_capture_artifact =
    Typedtree_adapter_private.Public.proof_capture_artifact implementation
  in
  let compilation_identity =
    Callback_certificate_private.cmt_compilation_identity implementation
  in
  match
    Typedtree_adapter_private.Public.lower_with_capture_artifact
      ?allow_imported_opens
      ~proof_capture_artifact
      ~compilation_identity
      ?authenticated_source_text
      ~source_file:implementation.Cmt_input.source_file
      ~imports:implementation.imports implementation.structure
  with
  | Error _ as error -> error
  | Ok program -> (
      match
        Parametric_rank_domain_private.seal_local_schemas ~implementation
          ~program
      with
      | Error error ->
          Error
            (Diagnostic.make
               (Diagnostic.Invalid_recursive_rank error.detail)
               error.Parametric_rank_domain_private.span)
      | Ok () -> (
          match Instance_mode.seal implementation program with
          | Ok () -> (
              match Finite_formal_requirement.seal implementation program with
              | Ok () -> Ok program
              | Error message ->
                  [%log.debug "finite-formal sealing failed" ~message];
                  Error
                    (Diagnostic.make
                       (Diagnostic.Unsupported_construct
                          Diagnostic.Malformed_ghost_call)
                       (Diagnostic.file_span implementation.source_file)))
          | Error error ->
              Error (Instance_mode.to_diagnostic error)))
[@@delator.instrument]

let lower_file ?int_size ?allow_imported_opens filename =
  match Cmt_input.load ?int_size filename with
  | Error _ as error -> error
  | Ok implementation -> lower ?allow_imported_opens implementation
[@@delator.instrument]
