type t = {
  base_int_full_key : string;
  validated_base_int_digest : string;
  authenticated_base_int_artifact_binding_key : string;
}

let correlate ~provider:(provider [@delator.skip])
    ~logical_sort:(logical_sort [@delator.skip]) =
  let result =
    let artifact = Typedtree_adapter_issuance_private.proof_capture_artifact provider in
    if not (Typedtree_adapter_issuance_private.authenticate_logical_builtin_artifact
        artifact ~source_file:provider.Cmt_input.source_file) then
      Error "The mathematical-Int provider is not an authenticated retained artifact; rebuild the provider."
    else match provider.retained_authority with
    | None -> Error "The mathematical-Int provider has no retained interface authority; rebuild the provider."
    | Some authority ->
        if not (Cmt_input.retained_authority_identity_is_exact provider) then
          Error "The mathematical-Int provider's retained artifact identity does not match; rebuild the provider."
        else if not (String.equal logical_sort.Logical_sort_private.provider_origin provider.unit_name) then
          Error "The mathematical-Int receipt must be bound at its original provider, not a reexport."
        else
          let exact_membership sorts =
            match List.filter (Logical_sort_private.equal logical_sort) sorts with
            | [_] -> true | _ -> false in
          if not (exact_membership provider.interface_logical_sorts
              && exact_membership (Retained_interface_authority_private.logical_sorts authority)) then
            Error "The mathematical-Int receipt does not match the provider's exact compiler and retained declarations."
          else
            let base_int_full_key = Logical_sort_private.canonical_material logical_sort in
            let validated_base_int_digest = Logical_sort_private.digest logical_sort in
            let authenticated_base_int_artifact_binding_key =
              Numeric_receipt_private.encode
                ~schema:"verocaml.numeric-base-int-artifact-binding.v1"
                [Config.cmt_magic_number; Config.cmi_magic_number;
                 provider.unit_name; provider.raw_artifact_receipt;
                 Retained_interface_authority_private.encode authority;
                 base_int_full_key]
            in
            [%log.trace "bound existing mathematical-Int receipt to its provider artifact"
              ~provider:(Delator.Field.string provider.unit_name)
              ~type_path:(Delator.Field.string logical_sort.type_path)
              ~type_uid:(Delator.Field.string logical_sort.type_uid)
              ~base_digest:(Delator.Field.string validated_base_int_digest)
              ~numeric_law_authority:(Delator.Field.bool false)
              ~runtime_refinement_authority:(Delator.Field.bool false)];
            Ok {base_int_full_key; validated_base_int_digest;
                authenticated_base_int_artifact_binding_key}
  in
  [%log.debug "completed numeric base-Int artifact correlation"
    ~provider:(Delator.Field.string provider.Cmt_input.unit_name)
    ~accepted:(Delator.Field.bool (Result.is_ok result))
    ~reason:(Delator.Field.string (match result with Ok _ -> "exact-existing-base-receipt" | Error reason -> reason))];
  result
[@@delator.instrument] [@@delator.level debug]

let resolve ~providers:(providers [@delator.skip]) (reference [@delator.skip]) =
  let result =
    let candidates = List.filter (fun (provider : Cmt_input.implementation) ->
      String.equal provider.unit_name reference.Numeric_interface_claim_private.base_owner.owner_unit
      && match provider.retained_authority with
        | None -> false
        | Some authority ->
            String.equal reference.base_owner.owner_cmi_full_key
              (Numeric_interface_claim_private.owner_cmi_full_key ~unit_name:provider.unit_name
                ~self_crc:authority.cmi_self_crc ~content_receipt:authority.cmi_receipt ~imports:authority.cmi_imports)) providers in
    match candidates with
    | [provider] ->
        (match List.filter (fun sort ->
          String.equal sort.Logical_sort_private.type_uid reference.type_uid
          && String.equal sort.provider_origin reference.base_owner.owner_unit) provider.interface_logical_sorts with
        | [logical_sort] -> Result.map (fun binding -> provider, logical_sort, binding) (correlate ~provider ~logical_sort)
        | _ -> Error "The numeric base reference does not identify one annotated mathematical-Int declaration.")
    | _ -> Error "The original mathematical-Int provider is missing or has a different compiler interface. Rebuild its ordinary library dependency." in
  [%log.debug "selected exact mathematical base provider"
    ~type_uid:(Delator.Field.string reference.type_uid)
    ~owner:(Delator.Field.string reference.base_owner.owner_unit)
    ~decision:(Delator.Field.string (if Result.is_ok result then "resolved-exact-base" else "rejected"))
    ~reason:(Delator.Field.string (match result with Ok _ -> "compiler-uid-and-full-cmi" | Error reason -> reason))];
  result
[@@delator.instrument] [@@delator.level debug]
