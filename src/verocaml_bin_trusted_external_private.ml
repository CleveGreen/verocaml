let span (value : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" value.file value.start_pos.line
    value.start_pos.column value.end_pos.line value.end_pos.column

let function_ref (value : Verifier_service.function_ref) =
  Printf.sprintf "%s#%d" (Verifier_service.function_name value)
    (Verifier_service.function_index value)

let dependency_list = function
  | [] -> "none"
  | dependencies ->
      List.map (fun (unit_name, digest) -> unit_name ^ "@" ^ digest) dependencies
      |> String.concat ","

let provenance_line value =
  Printf.sprintf
    "verocaml: verified dependency unit=%s interface-digest=%s direct=%s transitive=%s trust=none"
    (Verifier_service.provenance_unit_name value)
    (Verifier_service.provenance_interface_digest value)
    (Verifier_service.provenance_direct_dependencies value |> dependency_list)
    (Verifier_service.provenance_transitive_dependencies value
    |> dependency_list)

let line = function
  | Verifier_service.Trusted_external_specification_use use ->
      Printf.sprintf
        "verocaml: trusted external specification trust=axiomatic target=%s wrapper=%s target-span=%s wrapper-span=%s witness-span=%s call=%s requires=%d ensures=%d result=unconstrained"
        (function_ref use.target) (function_ref use.wrapper)
        (span use.target_span) (span use.wrapper_span) (span use.witness_span)
        (span use.call_span) use.requires_count use.ensures_count
  | Trusted_external_target_specification_use use ->
      Printf.sprintf
        "verocaml: trusted external specification trust=imported-unverified-target target-unit=%s target-interface-digest=%s target-path=%s consumer-artifact=%s import-crc=%s value-uid=%s callable-abi=%s summary-digest=%s wrapper=%s target-span=%s wrapper-span=%s witness-span=%s call=%s requires=%d ensures=%d target-body=unverified result=constrained-only-by-ensures"
        use.target_unit use.target_interface_digest use.canonical_path
        use.consumer_artifact_digest use.import_crc use.value_uid
        use.callable_abi_digest use.summary_digest
        (function_ref use.wrapper) (span use.target_span)
        (span use.wrapper_span) (span use.witness_span) (span use.call_span)
        use.requires_count use.ensures_count
  | Trusted_external_body_use use ->
      let mode, call_form =
        if use.proof_call then (" mode=proof", " call-form=proof")
        else ("", "")
      in
      Printf.sprintf
        "verocaml: trusted external body trust=axiomatic%s%s function=%s declaration-span=%s witness-span=%s call=%s requires=%d ensures=%d body=unchecked result=constrained-only-by-ensures"
        mode call_form (function_ref use.function_) (span use.declaration_span)
        (span use.witness_span) (span use.call_span) use.requires_count
        use.ensures_count
  | Trusted_external_body_declaration declaration ->
      let mode = if declaration.proof_mode then " mode=proof" else "" in
      Printf.sprintf
        "verocaml: trusted external body declaration trust=axiomatic%s function=%s declaration-span=%s witness-span=%s requires=%d ensures=%d body=unchecked"
        mode (function_ref declaration.function_)
        (span declaration.declaration_span) (span declaration.witness_span)
        declaration.requires_count declaration.ensures_count
