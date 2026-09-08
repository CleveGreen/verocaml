type owner = {
  owner_unit : string;
  owner_cmi_full_key : string;
  owner_cmi_checked_digest : string;
  import_routes : string list;
}

type base_reference = { type_path : string; type_uid : string; base_owner : owner }

type carrier = {
  base_reference : base_reference option;
  source_claim : string;
  carrier_path : string;
  carrier_uid : string;
  owner : owner;
  constructor_abi : string;
  binder_abi : string;
  compiler_jkind_abi : string;
  compiler_representation : string;
  claim_key : string;
  transport_key : string;
}

type role = {
  source_claim : string;
  callable_path : string;
  callable_uid : string;
  callable_type_abi : string;
  callable_mode_abi : string;
  callable_owner : owner;
  semantics_path : string;
  semantics_uid : string;
  semantics_type_abi : string;
  semantics_mode_abi : string;
  semantics_owner : owner;
  carrier_uid : string;
  carrier_owner : owner;
  claim_key : string;
  transport_key : string;
}

let owner_schema = "verocaml.numeric-interface-owner-claim.v1"
let owner_identity_schema = "verocaml.numeric-interface-owner-identity.v1"
let carrier_schema = "verocaml.numeric-interface-carrier-transport.v4"
let carrier_identity_schema = "verocaml.numeric-interface-carrier-claim.v3"
let role_schema = "verocaml.numeric-interface-role-transport.v2"
let role_identity_schema = "verocaml.numeric-interface-role-claim.v1"
let owner_cmi_schema = "verocaml.numeric-owner-cmi-full-key.v1"
let owner_import_schema = "verocaml.numeric-owner-cmi-import.v1"
let carrier_source_schema = "verocaml.numeric-carrier-source-claim.v1"

let owner_cmi_full_key ~unit_name ~self_crc ~content_receipt ~imports =
  let imports = List.map (fun (unit_name, crc) -> Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-owner-cmi-import.v1" [unit_name; Option.value ~default:"" crc]) imports
    |> List.sort String.compare in
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-owner-cmi-full-key.v1"
    [Config.cmi_magic_number; unit_name; self_crc; content_receipt; Numeric_receipt_private.list imports]
let role_source_schema = "verocaml.numeric-role-source-claim.v1"

let ( let* ) value continuation =
  match value with Ok value -> continuation value | Error _ as error -> error

let nonempty values = List.for_all (fun value -> not (String.equal value "")) values

let validate_owner_cmi_full_key encoded =
  let* fields =
    Numeric_receipt_private.decode ~schema:owner_cmi_schema ~field_count:5
      encoded
  in
  match fields with
  | [ compiler_magic; unit_name; self_crc; content_receipt; imports ] ->
      let* imports = Numeric_receipt_private.decode_list imports in
      let rec validate_imports units = function
        | [] -> Ok (List.rev units)
        | encoded :: rest ->
            let* fields =
              Numeric_receipt_private.decode ~schema:owner_import_schema
                ~field_count:2 encoded
            in
            let* imported_unit =
              match fields with
              | [ imported_unit; _ ] when imported_unit <> "" ->
                  Ok imported_unit
              | [ _; _ ] -> Error "numeric owner CMI import has an empty unit"
              | _ -> assert false
            in
            validate_imports (imported_unit :: units) rest
      in
      let* import_units = validate_imports [] imports in
      if
        imports <> List.sort_uniq String.compare imports
        || List.length import_units
           <> List.length (List.sort_uniq String.compare import_units)
      then Error "numeric owner CMI imports are duplicate or noncanonical"
      else if nonempty [ compiler_magic; unit_name; self_crc; content_receipt ] then
        Ok unit_name
      else Error "numeric owner CMI full key is incomplete"
  | _ -> assert false

let owner_material value =
  Numeric_receipt_private.encode ~schema:owner_schema
    [ value.owner_unit; value.owner_cmi_full_key;
      value.owner_cmi_checked_digest;
      Numeric_receipt_private.list value.import_routes ]

let owner_identity_material value =
  Numeric_receipt_private.encode ~schema:owner_identity_schema
    [ value.owner_unit; value.owner_cmi_full_key;
      value.owner_cmi_checked_digest ]

let owner ~owner_unit ~owner_cmi_full_key ~owner_cmi_checked_digest
    ~import_routes =
  let result =
    let* full_key_unit = validate_owner_cmi_full_key owner_cmi_full_key in
    let expected_digest =
      Numeric_receipt_private.digest
        ~domain:"verocaml.numeric-owner-cmi-full-key.v1" owner_cmi_full_key
    in
    let import_routes = List.sort String.compare import_routes in
    let* () =
      let validate = function
        | [] -> Error "numeric interface claim has no claimed import route"
        | routes ->
            let rec each = function
              | [] -> Ok ()
              | route :: rest ->
                  let* route = Numeric_receipt_private.decode_list route in
                  if route = [] || List.hd (List.rev route) <> owner_cmi_full_key
                  then Error "numeric interface import route has the wrong owner"
                  else
                    let* () =
                      let rec owners = function
                        | [] -> Ok ()
                        | owner :: rest ->
                            let* _ = validate_owner_cmi_full_key owner in
                            owners rest
                      in
                      owners route
                    in
                    each rest
            in
            each routes
      in
      validate import_routes
    in
    if
      not (nonempty [ owner_unit; owner_cmi_full_key; owner_cmi_checked_digest ])
      || not (String.equal owner_unit full_key_unit)
      || not (String.equal owner_cmi_checked_digest expected_digest)
      || List.length import_routes
         <> List.length (List.sort_uniq String.compare import_routes)
    then Error "numeric interface owner claim is inconsistent"
    else
      Ok
        { owner_unit; owner_cmi_full_key; owner_cmi_checked_digest;
          import_routes }
  in
  (match result with
  | Ok _ ->
      [%log.trace "validated numeric interface owner claim"
        ~stage:(Delator.Field.string "numeric-interface-owner-claim")
        ~route_count:(Delator.Field.int (List.length import_routes))
        ~authority:(Delator.Field.string "compiler-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected numeric interface owner claim"
        ~stage:(Delator.Field.string "numeric-interface-owner-claim")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let decode_owner encoded =
  let* fields =
    Numeric_receipt_private.decode ~schema:owner_schema ~field_count:4 encoded
  in
  match fields with
  | [ owner_unit; owner_cmi_full_key; owner_cmi_checked_digest; routes ] ->
      let* import_routes = Numeric_receipt_private.decode_list routes in
      let* value =
        owner ~owner_unit ~owner_cmi_full_key ~owner_cmi_checked_digest
          ~import_routes
      in
      if String.equal encoded (owner_material value) then Ok value
      else Error "numeric interface owner claim is noncanonical"
  | _ -> assert false

let validate_carrier_source source_claim =
  let* fields =
    match Numeric_receipt_private.decode ~schema:carrier_source_schema ~field_count:3 source_claim with
    | Ok fields -> Ok fields
    | Error _ ->
        let* fields = Numeric_receipt_private.decode ~schema:"verocaml.numeric-carrier-source-claim.v2" ~field_count:4 source_claim in
        (match fields with
        | [profile; representation; compatibility; base] when base <> "" -> Ok [profile; representation; compatibility]
        | _ -> Error "retained numeric carrier has no base type reference")
  in
  match fields with
  | [ profile; representation; compatibility ] ->
      let* compatibility = Numeric_receipt_private.decode_list compatibility in
      if
        not (nonempty (profile :: compatibility))
        || not (List.mem representation [ "immediate"; "boxed" ])
        || compatibility <> List.sort_uniq String.compare compatibility
      then Error "retained numeric carrier source claim is malformed"
      else Ok ()
  | _ -> assert false

let validate_role_source source_claim =
  let* fields =
    Numeric_receipt_private.decode ~schema:role_source_schema ~field_count:7
      source_claim
  in
  match fields with
  | [ carrier; schema; identity; semantics; visibility; reveal; inline ] ->
      if
        not (nonempty [ carrier; schema; identity; semantics; visibility ])
        || not (List.mem visibility [ "visible"; "opaque" ])
        || not (List.mem reveal [ "true"; "false" ])
        || not (List.mem inline [ "true"; "false" ])
      then Error "retained numeric semantic-role source claim is malformed"
      else Ok ()
  | _ -> assert false

let base_reference ~type_path ~type_uid ~base_owner =
  if nonempty [type_path; type_uid] then Ok {type_path; type_uid; base_owner}
  else Error "numeric base reference is missing its compiler type identity"

let base_identity value = Numeric_receipt_private.encode ~schema:"verocaml.numeric-base-reference.v1"
  [value.type_path; value.type_uid; owner_identity_material value.base_owner]
let base_material value = Numeric_receipt_private.encode ~schema:"verocaml.numeric-base-reference-transport.v1"
  [value.type_path; value.type_uid; owner_material value.base_owner]
let option_material f value = Numeric_receipt_private.list (Option.to_list (Option.map f value))
let decode_base encoded =
  let* fields = Numeric_receipt_private.decode_list encoded in
  match fields with
  | [] -> Ok None
  | [material] ->
      let* fields = Numeric_receipt_private.decode ~schema:"verocaml.numeric-base-reference-transport.v1" ~field_count:3 material in
      (match fields with
      | [type_path; type_uid; owner] ->
          let* base_owner = decode_owner owner in
          Result.map Option.some (base_reference ~type_path ~type_uid ~base_owner)
      | _ -> assert false)
  | _ -> Error "numeric carrier contains multiple base references"

let carrier_with_base ~base_reference ~source_claim ~carrier_path ~carrier_uid ~owner ~constructor_abi
    ~binder_abi ~compiler_jkind_abi ~compiler_representation =
  let result =
    let* () = validate_carrier_source source_claim in
    let source_has_base = Result.is_ok (Numeric_receipt_private.decode
      ~schema:"verocaml.numeric-carrier-source-claim.v2" ~field_count:4 source_claim) in
    let* () = if source_has_base = Option.is_some base_reference then Ok ()
      else Error "numeric carrier source and compiler base reference disagree" in
    if
      not
        (nonempty
           [ carrier_path; carrier_uid; constructor_abi; binder_abi;
             compiler_jkind_abi; compiler_representation ])
    then Error "retained numeric carrier compiler claim is incomplete"
    else if
      not (List.mem compiler_representation [ "immediate"; "boxed" ])
    then Error "retained numeric carrier compiler representation is unsupported"
    else
      let claim_key =
        Numeric_receipt_private.encode ~schema:carrier_identity_schema
          [ source_claim; carrier_path; carrier_uid;
            owner_identity_material owner;
            constructor_abi; binder_abi; compiler_jkind_abi;
            compiler_representation; option_material base_identity base_reference ]
      in
      let transport_key =
        Numeric_receipt_private.encode ~schema:carrier_schema
          [ claim_key; owner_material owner; option_material base_material base_reference ]
      in
      Ok
        { base_reference; source_claim; carrier_path; carrier_uid; owner; constructor_abi;
          binder_abi; compiler_jkind_abi; compiler_representation; claim_key;
          transport_key }
  in
  (match result with
  | Ok _ ->
      [%log.trace "constructed retained numeric carrier claim"
        ~stage:(Delator.Field.string "numeric-interface-carrier-claim")
        ~claim_class:(Delator.Field.string "carrier")
        ~owner_route_count:
          (Delator.Field.int (List.length owner.import_routes))
        ~constructor_abi_present:
          (Delator.Field.bool (constructor_abi <> ""))
        ~binder_abi_present:(Delator.Field.bool (binder_abi <> ""))
        ~compiler_representation:
          (Delator.Field.string compiler_representation)
        ~authority:(Delator.Field.string "compiler-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected retained numeric carrier claim construction"
        ~stage:(Delator.Field.string "numeric-interface-carrier-claim")
        ~claim_class:(Delator.Field.string "carrier")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let carrier ~source_claim ~carrier_path ~carrier_uid ~owner ~constructor_abi
    ~binder_abi ~compiler_jkind_abi ~compiler_representation =
  carrier_with_base ~base_reference:None ~source_claim ~carrier_path ~carrier_uid ~owner
    ~constructor_abi ~binder_abi ~compiler_jkind_abi ~compiler_representation

let role ~source_claim ~callable_path ~callable_uid ~callable_type_abi
    ~callable_mode_abi ~callable_owner ~semantics_path ~semantics_uid
    ~semantics_type_abi ~semantics_mode_abi ~semantics_owner ~carrier_uid
    ~carrier_owner =
  let result =
    let* () = validate_role_source source_claim in
    if
      not
        (nonempty
           [ callable_path; callable_uid; callable_type_abi; callable_mode_abi;
             semantics_path; semantics_uid; semantics_type_abi;
             semantics_mode_abi; carrier_uid ])
    then Error "retained numeric semantic-role compiler claim is incomplete"
    else
      let claim_key =
        Numeric_receipt_private.encode ~schema:role_identity_schema
          [ source_claim; callable_path; callable_uid; callable_type_abi;
            callable_mode_abi; owner_identity_material callable_owner; semantics_path;
            semantics_uid; semantics_type_abi; semantics_mode_abi;
            owner_identity_material semantics_owner; carrier_uid;
            owner_identity_material carrier_owner ]
      in
      let transport_key =
        Numeric_receipt_private.encode ~schema:role_schema
          [ claim_key; owner_material callable_owner;
            owner_material semantics_owner; owner_material carrier_owner ]
      in
      Ok
        { source_claim; callable_path; callable_uid; callable_type_abi;
          callable_mode_abi; callable_owner; semantics_path; semantics_uid;
          semantics_type_abi; semantics_mode_abi; semantics_owner; carrier_uid;
          carrier_owner; claim_key; transport_key }
  in
  (match result with
  | Ok _ ->
      [%log.trace "constructed retained numeric semantic-role claim"
        ~stage:(Delator.Field.string "numeric-interface-role-claim")
        ~claim_class:(Delator.Field.string "semantic-role")
        ~owner_route_count:
          (Delator.Field.int
             (List.length callable_owner.import_routes
             + List.length semantics_owner.import_routes
             + List.length carrier_owner.import_routes))
        ~type_abi_count:(Delator.Field.int 2)
        ~mode_abi_count:(Delator.Field.int 2)
        ~cross_owner:
          (Delator.Field.bool
             (not
                (String.equal callable_owner.owner_unit
                   semantics_owner.owner_unit
                && String.equal callable_owner.owner_unit
                     carrier_owner.owner_unit)))
        ~authority:(Delator.Field.string "compiler-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected retained numeric semantic-role claim construction"
        ~stage:(Delator.Field.string "numeric-interface-role-claim")
        ~claim_class:(Delator.Field.string "semantic-role")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let decode_carrier encoded =
  let result =
    let* fields =
      Numeric_receipt_private.decode ~schema:carrier_schema ~field_count:3
        encoded
    in
    match fields with
    | [ claimed_identity; encoded_owner; encoded_base ] ->
        let* owner = decode_owner encoded_owner in
        let* base_reference = decode_base encoded_base in
        let* identity_fields =
          Numeric_receipt_private.decode ~schema:carrier_identity_schema
            ~field_count:9 claimed_identity
        in
        let source_claim, carrier_path, carrier_uid, claimed_owner,
            constructor_abi, binder_abi, compiler_jkind_abi,
            compiler_representation, claimed_base =
          match identity_fields with
          | [ source_claim; carrier_path; carrier_uid; claimed_owner;
              constructor_abi; binder_abi; compiler_jkind_abi;
              compiler_representation; claimed_base ] ->
              (source_claim, carrier_path, carrier_uid, claimed_owner,
               constructor_abi, binder_abi, compiler_jkind_abi,
               compiler_representation, claimed_base)
          | _ -> assert false
        in
        if not (String.equal claimed_owner (owner_identity_material owner))
          || not (String.equal claimed_base (option_material base_identity base_reference)) then
          Error "retained numeric carrier transport substitutes its owner"
        else
        let* value =
          carrier_with_base ~base_reference ~source_claim ~carrier_path ~carrier_uid ~owner
            ~constructor_abi ~binder_abi ~compiler_jkind_abi
            ~compiler_representation
        in
        if
          String.equal claimed_identity value.claim_key
          && String.equal encoded value.transport_key
        then Ok value
        else Error "retained numeric carrier claim is noncanonical"
    | _ -> assert false
  in
  (match result with
  | Ok _ ->
      [%log.trace "decoded retained numeric carrier claim"
        ~stage:(Delator.Field.string "numeric-interface-carrier-decode")
        ~claim_class:(Delator.Field.string "carrier")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected retained numeric carrier claim decode"
        ~stage:(Delator.Field.string "numeric-interface-carrier-decode")
        ~claim_class:(Delator.Field.string "carrier")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let decode_role encoded =
  let result =
    let* fields =
      Numeric_receipt_private.decode ~schema:role_schema ~field_count:4 encoded
    in
    match fields with
    | [ claimed_identity; encoded_callable_owner; encoded_semantics_owner;
        encoded_carrier_owner ] ->
        let* callable_owner = decode_owner encoded_callable_owner in
        let* semantics_owner = decode_owner encoded_semantics_owner in
        let* carrier_owner = decode_owner encoded_carrier_owner in
        let* identity_fields =
          Numeric_receipt_private.decode ~schema:role_identity_schema
            ~field_count:13 claimed_identity
        in
        let source_claim, callable_path, callable_uid, callable_type_abi,
            callable_mode_abi, claimed_callable_owner, semantics_path,
            semantics_uid, semantics_type_abi, semantics_mode_abi,
            claimed_semantics_owner, carrier_uid, claimed_carrier_owner =
          match identity_fields with
          | [ source_claim; callable_path; callable_uid; callable_type_abi;
              callable_mode_abi; claimed_callable_owner; semantics_path;
              semantics_uid; semantics_type_abi; semantics_mode_abi;
              claimed_semantics_owner; carrier_uid; claimed_carrier_owner ] ->
              (source_claim, callable_path, callable_uid, callable_type_abi,
               callable_mode_abi, claimed_callable_owner, semantics_path,
               semantics_uid, semantics_type_abi, semantics_mode_abi,
               claimed_semantics_owner, carrier_uid, claimed_carrier_owner)
          | _ -> assert false
        in
        if
          not
            (String.equal claimed_callable_owner
               (owner_identity_material callable_owner)
            && String.equal claimed_semantics_owner
                 (owner_identity_material semantics_owner)
            && String.equal claimed_carrier_owner
                 (owner_identity_material carrier_owner))
        then Error "retained numeric role transport substitutes an owner"
        else
        let* value =
          role ~source_claim ~callable_path ~callable_uid ~callable_type_abi
            ~callable_mode_abi ~callable_owner ~semantics_path ~semantics_uid
            ~semantics_type_abi ~semantics_mode_abi ~semantics_owner
            ~carrier_uid ~carrier_owner
        in
        if
          String.equal claimed_identity value.claim_key
          && String.equal encoded value.transport_key
        then Ok value
        else Error "retained numeric semantic-role claim is noncanonical"
    | _ -> assert false
  in
  (match result with
  | Ok _ ->
      [%log.trace "decoded retained numeric semantic-role claim"
        ~stage:(Delator.Field.string "numeric-interface-role-decode")
        ~claim_class:(Delator.Field.string "semantic-role")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected retained numeric semantic-role claim decode"
        ~stage:(Delator.Field.string "numeric-interface-role-decode")
        ~claim_class:(Delator.Field.string "semantic-role")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let compare_carrier (left : carrier) (right : carrier) =
  String.compare left.claim_key right.claim_key

let compare_role (left : role) (right : role) =
  String.compare left.claim_key right.claim_key
