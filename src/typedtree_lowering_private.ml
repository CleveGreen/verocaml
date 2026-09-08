let lowering_entries = ref 0

type lowered = {
  program : Sst.program;
  registration : Imported_callable.registration;
  external_registration :
    External_target_specification_private.registration option;
}

let malformed implementation message =
  let _ = message in
  [%log.debug "rejected imported broadcast lowering boundary"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~stage:(Delator.Field.string "activation-lowering")
    ~route:(Delator.Field.string "typedtree")
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string "authenticated-lowering")];
  Error
    (Diagnostic.make
       (Diagnostic.Unsupported_construct Diagnostic.Malformed_ghost_call)
       (Diagnostic.file_span implementation.Cmt_input.source_file)
    |> Diagnostic.with_message message)

let imported_specification_error implementation message =
  let prefix = "[VERO_DEPENDENCY] " in
  let message =
    if String.starts_with ~prefix message then
      String.sub message (String.length prefix)
        (String.length message - String.length prefix)
    else message
  in
  Error
    (Diagnostic.make (Diagnostic.Invalid_imported_specification message)
       (Diagnostic.file_span implementation.Cmt_input.source_file))

let lower_internal ?(allow_public_parametric_signatures = false) ?external_specifications ~imported
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
  let imported_logical_values = Imported_callable.logical_constants imported in
  let imported_symbolic_values =
    imported_logical_values
    |> List.concat_map (function
         | Imported_callable.Imported_symbolic_logical_value
             { symbolic; routes; _ } ->
             List.map
               (fun (route : Imported_callable.logical_constant_route) ->
                 { symbolic with
                   path = route.logical_constant_path;
                   binding_uid = route.logical_constant_uid })
               routes
         | Imported_callable.Imported_defined_logical_value _ -> [])
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
              imported_broadcast_trigger_span =
                callable.broadcast_trigger_span;
            })
          (Imported_callable.callables imported @ imported_symbolic_values);
      imported_broadcast_declarations =
        List.map
          (fun (declaration : Imported_callable.broadcast_declaration_snapshot) ->
            [%log.debug "installed authenticated broadcast declaration snapshot"
              ~route:(Delator.Field.string "typedtree-lowering")
              ~stage:(Delator.Field.string "activation-environment")
              ~member_kind:(Delator.Field.string "declaration")
              ~trust_class:
                (Delator.Field.string
                   (match declaration.Imported_callable.trust with
                   | Retained_broadcast_private.Proved -> "proved"
                   | Retained_broadcast_private.Trusted -> "trusted"))
              ~correlation:
                (Delator.Field.string
                   (Retained_broadcast_private.correlation declaration.identity))
              ~decision:(Delator.Field.string "installed")];
            {
              Typedtree_adapter_private.Public.imported_broadcast_identity =
                declaration.identity;
              imported_broadcast_definition = declaration.definition;
              imported_broadcast_trigger_span = declaration.trigger_span;
              imported_broadcast_trust = declaration.trust;
            })
          (Imported_callable.broadcast_declarations imported);
      imported_broadcast_groups =
        List.map
          (fun (group : Imported_callable.broadcast_group_snapshot) ->
            [%log.debug "installed authenticated broadcast group snapshot"
              ~route:(Delator.Field.string "typedtree-lowering")
              ~stage:(Delator.Field.string "activation-environment")
              ~member_kind:(Delator.Field.string "group")
              ~set_cardinality:(Delator.Field.int (List.length group.members))
              ~correlation:
                (Delator.Field.string
                   (Retained_broadcast_private.correlation group.identity))
              ~decision:(Delator.Field.string "installed")];
            {
              Typedtree_adapter_private.Public.imported_broadcast_group_identity =
                group.identity;
              imported_broadcast_members = group.members;
            })
          (Imported_callable.broadcast_groups imported);
      imported_types =
        List.map
          (fun (typ : Imported_callable.type_snapshot) ->
            {
              Typedtree_adapter_private.Public.imported_type_path = typ.path;
              imported_type_uid = typ.binding_uid;
              imported_type_definition = typ.definition;
              imported_parametric_descriptor = typ.parametric_descriptor;
            })
          imported_types;
      imported_logical_constants =
        imported_logical_values
        |> List.concat_map (function
             | Imported_callable.Imported_symbolic_logical_value _ -> []
             | Imported_callable.Imported_defined_logical_value
                 { routes; definition; _ } ->
                 List.map
                   (fun (route : Imported_callable.logical_constant_route) ->
                   { Typedtree_adapter_private.Public.imported_constant_path =
                       route.logical_constant_path;
                     imported_constant_uid = route.logical_constant_uid;
                     imported_constant_definition = definition })
                   routes);
      imported_logical_sorts = Imported_callable.logical_sorts imported;
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
      ~interface_logical_values:implementation.interface_logical_values
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
          | Error error -> Error (Instance_mode.to_diagnostic error)
          | Ok () -> (
              match Finite_formal_requirement.seal implementation program with
              | Error message -> malformed implementation message
              | Ok () ->
                  (match
                     Imported_callable.seal_calls imported ~implementation
                       ~program
                   with
                  | Error message ->
                      imported_specification_error implementation message
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
                              imported_specification_error implementation
                                message
                          | Ok external_registration ->
                              Ok
                                {
                                  program;
                                  registration;
                                  external_registration;
                                }))))))
[@@delator.instrument] [@@delator.level debug]

let lower ?(allow_public_parametric_signatures = false) ?external_specifications
    ~imported:(imported [@delator.skip])
    ((implementation : Cmt_input.implementation) [@delator.skip]) =
  let result =
    lower_internal ~allow_public_parametric_signatures
      ?external_specifications ~imported implementation
  in
  [%log.debug "completed imported broadcast activation lowering"
    ~provider:(Delator.Field.string implementation.unit_name)
    ~stage:(Delator.Field.string "activation-lowering")
    ~route:(Delator.Field.string "typedtree")
    ~declaration_count:
      (Delator.Field.int
         (List.length (Imported_callable.broadcast_declarations imported)))
    ~group_count:
      (Delator.Field.int
         (List.length (Imported_callable.broadcast_groups imported)))
    ~decision:
      (Delator.Field.string
         (if Result.is_ok result then "accepted" else "rejected"))];
  result
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let program lowered = lowered.program
let registration lowered = lowered.registration
let external_registration lowered = lowered.external_registration

module For_testing = struct
  let reset_lowering_entries () = lowering_entries := 0
  let lowering_entries () = !lowering_entries
end
