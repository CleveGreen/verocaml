let executable_summary definition =
  match definition.Sst.body with
  | Sst.External_specification (Sst.Imported_unverified_target _) ->
      let executable_body =
        {
          Sst.expression_desc = Sst.Unit_constant;
          typ = Sst.Unit;
          span = definition.span;
        }
      in
      ( {
          definition with
          Sst.body =
            Sst.Checked_exec
              {
                body = { stage = Sst.Runtime; expression = executable_body };
                provenance =
                  Sst.Authenticated_typedtree
                    {
                      source_file = definition.span.file;
                      declaration_span = definition.span;
                    };
              };
        },
        executable_body )
  | Sst.Checked_exec _ | Sst.Spec_definition _
  | Sst.Recursive_spec_definition _ | Sst.Proof_body _
  | Sst.External_specification
      (Sst.Same_unit_target _ | Sst.Unresolved_target _)
  | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
  | Sst.Symbolic_declaration _ ->
      invalid_arg "executable verified external specification requires a validated wrapper"

let create link ~call_span ~requires_count ~ensures_count =
  match link with
  | Sst.Imported_unverified_target
      {
        wrapper;
        consumer_artifact_digest;
        target_unit;
        target_interface_digest;
        import_crc;
        canonical_path;
        value_uid;
        callable_abi_digest;
        summary_digest;
        target_span;
        declaration_span;
        witness_span;
      } ->
      Vir.Trusted_external_target_specification_use
        {
          consumer_artifact_digest;
          target_unit;
          target_interface_digest;
          import_crc;
          canonical_path;
          value_uid;
          callable_abi_digest;
          wrapper =
            {
              Vir.function_name = wrapper.function_name;
              function_index = wrapper.function_index;
            };
          target_span;
          wrapper_span = declaration_span;
          witness_span;
          call_span;
          summary_digest;
          requires_count;
          ensures_count;
        }
  | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
      invalid_arg "trusted external-target use requires an imported target"

let collect ~validated ~caller =
  Sst_validation.call_edge_descriptors validated
  |> List.filter_map (fun edge ->
         let caller_descriptor = Sst_validation.call_edge_caller edge
         and callee_descriptor = Sst_validation.call_edge_callee edge in
         let callee = Sst_validation.callable_definition callee_descriptor in
         if
           Sst_validation.callable_id caller_descriptor <> caller
           || Sst_validation.call_edge_region edge
              <> Sst_validation.Body_region
         then None
         else
           Option.map
             (fun link ->
               let contract = Sst_validation.callable_contract callee_descriptor in
               create link
                 ~call_span:(Sst_validation.call_edge_span edge)
                 ~requires_count:
                   (List.length (Sst_validation.contract_requires contract))
                 ~ensures_count:
                   (List.length (Sst_validation.contract_ensures contract)))
             (Sst_validation.external_target_identity validated callee))
