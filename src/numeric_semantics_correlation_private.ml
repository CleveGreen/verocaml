type declaration_kind =
  | Completed_proof_declaration
  | Explicit_axiom_declaration
  | Ordinary_specification_declaration

type t = {
  role : Cmt_input.interface_numeric_role;
  definition : Sst.function_definition;
  callable_definition : Sst.function_definition option;
  kind : declaration_kind;
}

let ( let* ) = Result.bind

let classify (definition : Sst.function_definition) =
  match definition.mode, definition.body, definition.recursive with
  | Sst.Proof, Sst.Proof_body _, _ -> Ok Completed_proof_declaration
  | Proof, Trusted_external_body (Authenticated_external_body _), false ->
      Ok Explicit_axiom_declaration
  | Spec,
    (Spec_definition _ | Recursive_spec_definition _ | Symbolic_declaration _),
    _ ->
      Ok Ordinary_specification_declaration
  | _ ->
      Error
        "A numeric semantics declaration must be a specification, proof, or explicit proof axiom; an executable trusted body is not a semantic law."

let correlate_local ~implementation:(implementation [@delator.skip])
    ~validated:(validated [@delator.skip]) =
  let result =
    let roles =
      implementation.Cmt_input.interface_numeric_claims.numeric_roles
      |> List.filter (fun role ->
             String.equal role.Cmt_input.numeric_role_semantics_owner_unit
               implementation.Cmt_input.unit_name)
    in
    if roles = [] then Ok []
    else if not (Cmt_input.retained_authority_identity_is_exact implementation)
    then
      Error
        "Numeric semantics have no matching retained provider artifact; rebuild the provider."
    else
      let program = Sst_validation.program validated in
      let instances =
        Typedtree_adapter_private.Public.issued_callable_instances
          ~structure:implementation.structure ~program
      in
      let matching_instances ~path ~uid =
        List.filter
          (fun instance ->
            Cmt_input.interface_value_uid_correlates implementation ~path
              ~interface_uid:uid
              ~implementation_uid:
                (Typedtree_adapter_private.Public.callable_instance_binding_uid
                   instance))
          instances
      in
      List.fold_left
        (fun result role ->
          let* bindings = result in
          let matching =
            matching_instances ~path:role.Cmt_input.numeric_role_semantics_path
              ~uid:role.numeric_role_semantics_uid
          in
          let* instance =
            match matching with
            | [ instance ] -> Ok instance
            | [] ->
                Error
                  "A numeric semantics declaration has no matching retained source definition; rebuild its provider."
            | _ ->
                Error
                  "A numeric semantics declaration refers to multiple retained source instances; a unique instance is required."
          in
          let definition =
            Typedtree_adapter_private.Public.callable_instance_definition instance
          in
          let* kind = classify definition in
          let callable_definition =
            if
              not
                (String.equal role.numeric_role_callable_owner_unit
                   implementation.Cmt_input.unit_name)
            then None
            else
              match
                matching_instances ~path:role.numeric_role_callable_path
                  ~uid:role.numeric_role_callable_uid
              with
              | [ instance ] ->
                  Some
                    (Typedtree_adapter_private.Public
                     .callable_instance_definition instance)
              | [] | _ :: _ :: _ -> None
          in
          let[@log_value.trace] kind_name =
            match kind with
            | Completed_proof_declaration -> "proof-declaration"
            | Explicit_axiom_declaration -> "explicit-axiom-declaration"
            | Ordinary_specification_declaration ->
                "ordinary-specification-declaration"
          in
          [%log.trace "correlated pre-solver numeric source declarations"
            ~stage:(Delator.Field.string "numeric-source-correlation")
            ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
            ~interface_uid:(Delator.Field.string role.numeric_role_semantics_uid)
            ~declaration_kind:
              (Delator.Field.string (kind_name [@log_value.trace]))
            ~local_callable_correlated:
              (Delator.Field.bool (Option.is_some callable_definition))
            ~proof_success:(Delator.Field.bool false)
            ~native_authority:(Delator.Field.bool false)];
          Ok ({ role; definition; callable_definition; kind } :: bindings))
        (Ok []) roles
      |> Result.map List.rev
  in
  [%log.debug "completed pre-solver numeric source correlation"
    ~stage:(Delator.Field.string "numeric-source-correlation")
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~declaration_count:
      (Delator.Field.int
         (match result with Ok bindings -> List.length bindings | Error _ -> 0))
    ~decision:
      (Delator.Field.string
         (if Result.is_ok result then "source-identity" else "rejected"))];
  result
[@@delator.instrument] [@@delator.level debug]

let selects_explicit_axiom bindings definition =
  List.exists
    (fun binding ->
      binding.definition == definition
      && binding.kind = Explicit_axiom_declaration)
    bindings
