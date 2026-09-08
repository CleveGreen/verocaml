type t = {
  consumer_artifact_full_key : string;
  dependency_artifact_full_keys : string list;
  function_origin_full_key : string;
  obligation : Vir.obligation;
  full_key : string;
  checked_digest : string;
}

let equal a b = String.equal a.full_key b.full_key
let matches_consumer original (implementation : Cmt_input.implementation) =
  String.equal original.consumer_artifact_full_key
    (Numeric_receipt_private.encode ~schema:"verocaml.numeric-original-consumer.v1"
      [implementation.unit_name; implementation.raw_artifact_receipt;
       Option.fold ~none:"" ~some:Retained_interface_authority_private.encode implementation.retained_authority])

module For_pipeline = struct
  let capture ~implementation:(implementation [@delator.skip])
      ~imported:(imported [@delator.skip])
      ~program:(program [@delator.skip]) ~definition:(definition [@delator.skip])
      (execution [@delator.skip]) =
    let dependency_artifact_full_keys = Option.fold ~none:[]
      ~some:Imported_callable.artifact_inventory imported in
    let consumer_artifact_full_key = Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-original-consumer.v1"
      [implementation.Cmt_input.unit_name; implementation.raw_artifact_receipt;
       Option.fold ~none:"" ~some:Retained_interface_authority_private.encode implementation.retained_authority] in
    let instances = Typedtree_adapter_private.Public.issued_callable_instances
      ~structure:implementation.structure ~program in
    let origins = List.filter_map (fun instance ->
      if Typedtree_adapter_private.Public.callable_instance_definition instance == definition then
        Some (Numeric_receipt_private.encode ~schema:"compiler-callable-instance"
          [Typedtree_adapter_private.Public.callable_instance_binding_uid instance;
           Typedtree_adapter_private.Public.callable_instance_profile_snapshot instance;
           Typedtree_adapter_private.Public.callable_instance_specialization_digest instance;
           Numeric_receipt_private.list (Typedtree_adapter_private.Public.callable_instance_rank_snapshots instance)])
      else None) instances |> List.sort_uniq String.compare in
    let function_origin_full_key = Numeric_receipt_private.encode
      ~schema:"verocaml.original-function-origin.v1"
      [consumer_artifact_full_key; string_of_int definition.Sst.function_id.function_index;
       definition.function_id.function_name; Numeric_receipt_private.list origins;
       Numeric_receipt_private.list dependency_artifact_full_keys;
       Sst.to_string {program with functions = [definition]}] in
    let obligations = List.map (fun obligation ->
      let full_key = Numeric_receipt_private.encode ~schema:"verocaml.original-obligation-receipt.v1"
        [consumer_artifact_full_key; function_origin_full_key; Vir_identity_private.obligation obligation] in
      let checked_digest = Numeric_receipt_private.digest ~domain:"verocaml.original-obligation-receipt.v1" full_key in
      {consumer_artifact_full_key; dependency_artifact_full_keys; function_origin_full_key; obligation; full_key; checked_digest})
      execution.Vir.obligations in
    [%log.debug "captured original target-neutral obligations"
      ~provider:(Delator.Field.string implementation.unit_name)
      ~function_name:(Delator.Field.string definition.function_id.function_name)
      ~compiler_origin_count:(Delator.Field.int (List.length origins))
      ~obligation_count:(Delator.Field.int (List.length obligations))
      ~proof_authority:(Delator.Field.bool false)];
    obligations
  [@@delator.instrument] [@@delator.level debug]
end
