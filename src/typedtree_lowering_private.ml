let lowering_entries = ref 0

type lowered = {
  program : Sst.program;
  registration : Imported_callable.registration;
  external_registration :
    External_target_specification_private.registration option;
}

let malformed implementation message =
  Printf.eprintf "imported-call seal: %s\n" message;
  Error
    (Diagnostic.make
       (Diagnostic.Unsupported_construct Diagnostic.Malformed_ghost_call)
       (Diagnostic.file_span implementation.Cmt_input.source_file))

let lower ?(allow_public_parametric_signatures = false) ?external_specifications ~imported
    (implementation : Cmt_input.implementation) =
  incr lowering_entries;
  let proof_capture_artifact =
    Typedtree_adapter_private.Public.proof_capture_artifact implementation
  in
  let compilation_identity =
    Callback_certificate_private.cmt_compilation_identity implementation
  in
  let imported_types = Imported_callable.types imported in
  let map_type_id type_id =
    match
      List.find_opt
        (fun (snapshot : Imported_callable.type_snapshot) ->
          snapshot.source_type_id = type_id)
        imported_types
    with
    | Some snapshot -> snapshot.definition.Sst.type_id
    | None -> type_id
  in
  let map_constructor (constructor : Sst.constructor_id) =
    {
      constructor with
      Sst.constructor_type = map_type_id constructor.constructor_type;
    }
  in
  let map_field_owner = function
    | Sst.Record_owner type_id -> Sst.Record_owner (map_type_id type_id)
    | Sst.Constructor_owner constructor ->
        Sst.Constructor_owner (map_constructor constructor)
  in
  let map_field (field : Sst.field_id) =
    { field with Sst.field_owner = map_field_owner field.field_owner }
  in
  let requirements =
    Imported_callable.callables imported
    |> List.concat_map
         (fun (callable : Imported_callable.callable_snapshot) ->
           callable.finite_requirements)
    |> List.sort_uniq
         (fun (left : Imported_callable.formal_requirement)
              (right : Imported_callable.formal_requirement) ->
           String.compare left.rank_domain_digest right.rank_domain_digest)
  in
  let imported_environment :
      Typedtree_adapter_private.Public.imported_environment =
    {
      allow_public_parametric_signatures;
      imported_callables =
        List.map
          (fun (callable : Imported_callable.callable_snapshot) ->
            {
              Typedtree_adapter_private.Public.imported_path = callable.path;
              imported_uid = callable.binding_uid;
              imported_definition = callable.definition;
              imported_signature = callable.signature;
            })
          (Imported_callable.callables imported);
      imported_types =
        List.map
          (fun (typ : Imported_callable.type_snapshot) ->
            {
              Typedtree_adapter_private.Public.imported_type_path = typ.path;
              imported_type_definition = typ.definition;
              imported_parametric_descriptor = typ.parametric_descriptor;
            })
          imported_types;
      imported_rank_domains =
        List.map
          (fun (requirement : Imported_callable.formal_requirement) ->
            {
              Typedtree_adapter_private.Public.imported_rank_component =
                requirement.rank_component
                |> List.map
                     (fun
                       (identity : Typedtree_adapter.rank_type_identity) ->
                       ( map_type_id identity.rank_type_id,
                         identity.rank_path,
                         identity.rank_uid,
                         identity.rank_span ));
              imported_rank_positive_children =
                requirement.rank_positive_children
                |> List.map
                     (fun
                       (child : Typedtree_adapter.rank_positive_child) ->
                       ( map_constructor child.rank_constructor,
                         child.rank_constructor_uid,
                         map_field child.rank_field,
                         child.rank_field_uid,
                         child.rank_child_path,
                         map_type_id child.rank_child_type,
                         child.rank_expansion_trace ));
              imported_rank_ground_witnesses =
                requirement.rank_ground_witnesses
                |> List.map
                     (fun
                       (witness : Typedtree_adapter.rank_ground_witness) ->
                       ( map_constructor witness.rank_ground_constructor,
                         witness.rank_ground_constructor_uid ));
              imported_rank_actual_evidence =
                requirement.profile_actual_snapshot;
            })
          requirements;
      external_target_specifications = external_specifications;
    }
  in
  match
    Typedtree_adapter_private.Public.lower_with_imports
      ~allow_imported_opens:true ~imported:imported_environment
      ~proof_capture_artifact
      ~compilation_identity
      ~load_path_visible:implementation.load_path_visible
      ~load_path_hidden:implementation.load_path_hidden
      ?authenticated_source_text:
        (match implementation.source_digest with
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
            read candidates)
      ~explicit_interface:implementation.explicit_interface
      ~interface_value_paths:
        (List.map fst implementation.interface_mode_signatures)
      ~source_file:implementation.source_file ~imports:implementation.imports
      implementation.structure
  with
  | Error _ as error -> error
  | Ok program -> (
      match
        Parametric_rank_domain_private.seal_local_schemas ~implementation
          ~program
      with
      | Error error -> malformed implementation error.detail
      | Ok () -> (
          match Instance_mode.seal implementation program with
          | Error error -> malformed implementation error.Instance_mode.message
          | Ok () -> (
              match Finite_formal_requirement.seal implementation program with
              | Error message -> malformed implementation message
              | Ok () ->
                  (match
                     Imported_callable.seal_calls imported ~implementation
                       ~program
                   with
                  | Error message -> malformed implementation message
                  | Ok registration -> (
                      match external_specifications with
                      | None ->
                          Ok
                            {
                              program;
                              registration;
                              external_registration = None;
                            }
                      | Some environment ->
                          (match
                             External_target_specification_private.register
                               environment ~consumer:implementation ~program
                           with
                          | Error message ->
                              Imported_callable.invalidate_registration
                                registration;
                              malformed implementation message
                          | Ok external_registration ->
                              Ok
                                {
                                  program;
                                  registration;
                                  external_registration;
                                }))))))

let program lowered = lowered.program
let registration lowered = lowered.registration
let external_registration lowered = lowered.external_registration

module For_testing = struct
  let reset_lowering_entries () = lowering_entries := 0
  let lowering_entries () = !lowering_entries
end
