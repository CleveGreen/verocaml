type facts = {
  provider_artifact_full_key : string;
  provider_artifact_checked_digest : string;
  carrier_claim : Numeric_interface_claim_private.carrier;
  source_request : Numeric_source_claim_private.carrier;
  compiler_layout_abi : string;
  representation : Cmt_input.numeric_artifact_representation;
  binding_full_key : string;
  binding_checked_digest : string;
}

type t = {
  issuer : unit ref;
  implementation : Cmt_input.implementation;
  facts : facts;
}

let issuer = ref ()
let ( let* ) = Result.bind

let representation_name = function
  | Cmt_input.Artifact_immediate -> "immediate"
  | Artifact_boxed -> "boxed"

let provider_key (implementation : Cmt_input.implementation) owner_key =
  match implementation.retained_authority with
  | None -> Error "numeric provider has no retained artifact authority"
  | Some authority ->
      if not (Cmt_input.retained_authority_identity_is_exact implementation) then
        Error "numeric provider retained artifact identity is incomplete"
      else
        let imports =
          Array.to_list implementation.imports
          |> List.map (fun (imported : Cmt_input.import) ->
                 Numeric_receipt_private.encode
                   ~schema:"verocaml.numeric-provider-import.v1"
                   [ imported.unit_name; Option.value ~default:"" imported.crc ])
          |> List.sort String.compare
        in
        Ok
          (Numeric_receipt_private.encode
             ~schema:"verocaml.numeric-provider-artifact-full-key.v1"
             [ Config.cmt_magic_number; Config.cmi_magic_number;
               implementation.unit_name; owner_key;
               implementation.raw_artifact_receipt;
               Option.value ~default:"" implementation.interface_digest;
               Option.value ~default:"" implementation.source_digest;
               Numeric_receipt_private.list
                 (Array.to_list implementation.compiler_arguments);
               Numeric_receipt_private.list imports;
               Retained_interface_authority_private.encode authority ])

let derive (implementation : Cmt_input.implementation)
    (carrier_claim : Numeric_interface_claim_private.carrier) =
  let result =
    if not (String.equal carrier_claim.owner.owner_unit implementation.unit_name) then
      Error "numeric reexports must use the original provider's artifact binding"
    else
      let retained =
        Cmt_input.retained_numeric_claims implementation.interface_numeric_claims
      in
      if not (List.mem carrier_claim.transport_key retained.carrier_reconstruction_claims) then
        Error "numeric carrier does not match the compiler-reconstructed artifact"
      else
        match List.filter
          (fun candidate ->
            String.equal candidate.Cmt_input.numeric_carrier_uid carrier_claim.carrier_uid
            && String.equal candidate.numeric_carrier_path carrier_claim.carrier_path
            && String.equal candidate.numeric_carrier_owner_cmi_full_key
                 carrier_claim.owner.owner_cmi_full_key)
          implementation.interface_numeric_claims.numeric_carriers
        with
        | [ carrier ] ->
            let source_request = carrier.numeric_carrier_source in
            let representation = carrier.numeric_carrier_compiler_representation in
            let compiler_layout_abi = carrier.numeric_carrier_compiler_jkind_abi in
            let request_matches =
              match source_request.representation, representation with
              | Numeric_source_claim_private.Immediate, Cmt_input.Artifact_immediate
              | Boxed, Artifact_boxed -> true
              | _ -> false
            in
            if not request_matches then
              Error "numeric representation request disagrees with the compiler layout"
            else
              let* provider_artifact_full_key =
                provider_key implementation carrier_claim.owner.owner_cmi_full_key
              in
              let provider_artifact_checked_digest =
                Numeric_receipt_private.digest
                  ~domain:"verocaml.numeric-provider-artifact-full-key.v1"
                  provider_artifact_full_key
              in
              let binding_full_key =
                Numeric_receipt_private.encode
                  ~schema:"verocaml.numeric-artifact-correlation.v1"
                  [ provider_artifact_full_key; carrier_claim.claim_key;
                    compiler_layout_abi; representation_name representation ]
              in
              let binding_checked_digest =
                Numeric_receipt_private.digest
                  ~domain:"verocaml.numeric-artifact-correlation.v1" binding_full_key
              in
              Ok { provider_artifact_full_key; provider_artifact_checked_digest;
                   carrier_claim; source_request; compiler_layout_abi;
                   representation; binding_full_key; binding_checked_digest }
        | [] | _ :: _ :: _ -> Error "numeric carrier has no unique compiler declaration"
  in
  (match result with
  | Ok (facts [@log_value.trace]) ->
      [%log.trace "correlated numeric carrier with its compiler artifact"
        ~provider:(Delator.Field.string implementation.unit_name)
        ~representation:
          (Delator.Field.string (representation_name (facts [@log_value.trace]).representation))
        ~authority:(Delator.Field.string "artifact-origin-and-layout-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected numeric carrier artifact correlation"
        ~provider:(Delator.Field.string implementation.unit_name)
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let correlate implementation carrier_claim =
  let* facts = derive implementation carrier_claim in
  [%log.info "issued numeric artifact-origin binding"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~authority:(Delator.Field.string "artifact-origin-and-layout-only")
    ~decision:(Delator.Field.string "issued")];
  Ok { issuer; implementation; facts }
[@@delator.instrument] [@@delator.level info]

let facts binding =
  let result =
    if binding.issuer != issuer then Error "numeric artifact binding has no live issuer"
    else
      let* current = derive binding.implementation binding.facts.carrier_claim in
      if String.equal current.binding_full_key binding.facts.binding_full_key then
        Ok current
      else Error "numeric artifact binding no longer matches its provider"
  in
  (match result with
  | Ok _ ->
      [%log.trace "validated numeric artifact-origin binding"
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.warn]) ->
      [%log.warn "rejected numeric artifact-origin binding"
        ~reason_class:(Delator.Field.string (reason [@log_value.warn]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]
