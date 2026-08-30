type callable = {
  function_index : int;
  display_name : string;
  leaf_name : string;
  resolved_path : string;
  binding_uid : string;
  canonical_path_component : string;
}

type t = {
  validated : Sst_validation.validated_program;
  program : Sst.program;
  callables : callable list;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let rec compiler_path = function
  | Path.Pident ident -> Printf.sprintf "ident(%s)" (Ident.unique_name ident)
  | Path.Pdot (prefix, field) ->
      Printf.sprintf "dot(%s,%s)" (compiler_path prefix) field
  | Path.Papply (functor_, argument) ->
      Printf.sprintf "apply(%s,%s)" (compiler_path functor_)
        (compiler_path argument)
  | Path.Pextra_ty (path, extra) ->
      let extra =
        match extra with
        | Path.Pcstr_ty constructor -> "constructor:" ^ constructor
        | Path.Pext_ty -> "extension"
        | Path.Punboxed_ty -> "unboxed"
      in
      Printf.sprintf "extra(%s,%s)" (compiler_path path) extra

let create ~imports ~implementation ~validated =
  let program = Sst_validation.program validated in
  let local_callables =
    Typedtree_adapter_private.Public.issued_callable_instances
      ~structure:implementation.Cmt_input.structure ~program
    |> List.map (fun instance ->
           if
             not
               (Typedtree_adapter_private.Public.authenticate_callable_instance
                  ~structure:implementation.Cmt_input.structure ~program
                  instance)
           then invalid_arg "unauthenticated private callable instance";
           let definition =
             Typedtree_adapter_private.Public.callable_instance_definition
               instance
           in
           let paths =
             Typedtree_adapter_private.Public.callable_instance_source_paths
               instance
           in
           let path =
             match List.rev paths with
             | path :: _ -> path
             | [] -> invalid_arg "callable instance has no compiler path"
           in
           let resolved_path = compiler_path path in
           let binding_uid =
             Typedtree_adapter_private.Public.callable_instance_binding_uid
               instance
           in
           let profile =
             Typedtree_adapter_private.Public.callable_instance_profile_snapshot
               instance
           in
           let specialization_digest =
             Typedtree_adapter_private.Public
             .callable_instance_specialization_digest instance
           in
           let rank_snapshots =
             Typedtree_adapter_private.Public.callable_instance_rank_snapshots
               instance
           in
           {
             function_index = definition.function_id.function_index;
             display_name = definition.function_id.function_name;
             leaf_name =
               Typedtree_adapter_private.Public.callable_instance_leaf_name
                 instance;
             resolved_path;
             binding_uid;
             canonical_path_component =
               String.concat "\000"
                 ([ resolved_path; binding_uid; profile; specialization_digest ]
                 @ rank_snapshots);
           })
  in
  let imported_callables =
    match
      Option.map Imported_callable.registration_environment imports
    with
    | None -> []
    | Some environment ->
        Imported_callable.callables environment
        |> List.map (fun
             (snapshot : Imported_callable.callable_snapshot) ->
               let definition = snapshot.definition in
               let leaf_name =
                 match List.rev (String.split_on_char '.' snapshot.path) with
                 | leaf :: _ -> leaf
                 | [] -> snapshot.path
               in
               {
                 function_index = definition.function_id.function_index;
                 display_name = definition.function_id.function_name;
                 leaf_name;
                 resolved_path = snapshot.path;
                 binding_uid = snapshot.binding_uid;
                 canonical_path_component =
                   String.concat "\000"
                     [ snapshot.path; snapshot.binding_uid ];
               })
  in
  let definitions =
    Sst_validation.callable_descriptors validated
    |> List.map Sst_validation.callable_definition
  in
  let external_callables =
    definitions
    |> List.filter_map (fun (definition : Sst.function_definition) ->
           match definition.body with
           | Sst.External_specification
               (Sst.Imported_unverified_target link)
             when not
                    (List.exists
                       (fun identity ->
                         identity.function_index
                         = definition.function_id.function_index)
                       local_callables) ->
               Some
                 {
                   function_index = definition.function_id.function_index;
                   display_name = definition.function_id.function_name;
                   leaf_name =
                     (match
                        List.rev (String.split_on_char '.' link.canonical_path)
                      with
                     | leaf :: _ -> leaf
                     | [] -> link.canonical_path);
                   resolved_path = link.canonical_path;
                   binding_uid = link.value_uid;
                   canonical_path_component =
                     String.concat "\000"
                       [
                         link.target_unit;
                         link.target_interface_digest;
                         link.canonical_path;
                         link.value_uid;
                         link.callable_abi_digest;
                       ];
                 }
           | Sst.Checked_exec _ | Sst.Spec_definition _
           | Sst.Recursive_spec_definition _ | Sst.Proof_body _
           | Sst.External_specification _ | Sst.Trusted_external_spec_target _
           | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
               None)
  in
  let callables = imported_callables @ external_callables @ local_callables in
  if List.length callables <> List.length definitions then
    Error "private callable identity count does not match the validated program"
  else
    let rec authenticate = function
      | [], [] -> Ok ()
      | identity :: identities, definition :: definitions ->
          if
            identity.function_index <> definition.Sst.function_id.function_index
            || not
                 (String.equal identity.display_name
                    definition.function_id.function_name)
          then
            Error
              "private callable identity does not match the validated callable \
               index/name"
          else authenticate (identities, definitions)
      | [], _ :: _ | _ :: _, [] -> assert false
    in
    let* () = authenticate (callables, definitions) in
    Ok { validated; program; callables }

let find identities (definition : Sst.function_definition) =
  if identities.program != Sst_validation.program identities.validated then
    Error "private callable identity metadata lost its validated program"
  else
    match
      List.find_opt
        (fun identity ->
          identity.function_index = definition.function_id.function_index
          && String.equal identity.display_name
               definition.function_id.function_name)
        identities.callables
    with
    | Some identity -> Ok identity
    | None ->
        Error "validated callable has no compiler-authenticated value identity"

let resolved_path callable = callable.resolved_path
let binding_uid callable = callable.binding_uid
let leaf_name callable = callable.leaf_name
let canonical_path_component callable = callable.canonical_path_component
