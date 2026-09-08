type layout = {
  layout_class : string;
  layout_abi : string;
  width : int;
  signed : bool;
}

type logical_bv_width_permission = {
  schema : string;
  domain : string;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

type claim = {
  schema : string;
  compiler_abi : string;
  toolchain_abi : string;
  target_identity : string;
  layouts : layout list;
  supported_widths : int list;
  logical_bv_width_permission : logical_bv_width_permission;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

type instance_claim = {
  schema : string;
  profile_full_key : string;
  profile_checked_digest : string;
  target_identity : string;
  layout_class : string;
  layout_abi : string;
  width : int;
  signed : bool;
  logical_bv_width_permission_full_key : string;
  logical_bv_width_permission_checked_digest : string;
  issuer_claim : string;
  full_key : string;
  checked_digest : string;
}

let schema = "verocaml.build-target-profile-claim.v2"
let instance_schema = "verocaml.build-target-instance-claim.v2"
let logical_bv_width_permission_schema =
  "verocaml.logical-bv-width-permission.v1"
let profile_digest_domain = "verocaml.build-target-profile-claim.full-key.v2"
let instance_digest_domain = "verocaml.build-target-instance-claim.full-key.v2"
let logical_bv_width_permission_digest_domain =
  "verocaml.logical-bv-width-permission.full-key.v1"
let positive_logical_bv_width_domain = "positive-mathematical-widths"

let ( let* ) value continuation =
  match value with Ok value -> continuation value | Error _ as error -> error

let issue_logical_bv_width_permission ~issuer_claim =
  let result =
    let* () =
      Numeric_receipt_private.nonempty ~label:"logical BV permission issuer"
        issuer_claim
    in
    let domain = positive_logical_bv_width_domain in
    let full_key =
      Numeric_receipt_private.encode ~schema:logical_bv_width_permission_schema
        [ domain; issuer_claim ]
    in
    let checked_digest =
      Numeric_receipt_private.digest
        ~domain:logical_bv_width_permission_digest_domain full_key
    in
    Ok
      { schema = logical_bv_width_permission_schema; domain; issuer_claim;
        full_key; checked_digest }
  in
  (match result with
  | Ok _ ->
      [%log.trace "issued canonical logical BV width permission claim"
        ~stage:(Delator.Field.string "logical-bv-width-permission-claim")
        ~domain:(Delator.Field.string positive_logical_bv_width_domain)
        ~authority:(Delator.Field.string "claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected logical BV width permission claim"
        ~stage:(Delator.Field.string "logical-bv-width-permission-claim")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let logical_bv_width_permitted (permission : logical_bv_width_permission) width =
  String.equal permission.schema logical_bv_width_permission_schema
  && String.equal permission.domain positive_logical_bv_width_domain
  && Z.sign width > 0

let layout ~layout_class ~layout_abi ~width ~signed =
  (let* () = Numeric_receipt_private.nonempty ~label:"layout class" layout_class in
   let* () = Numeric_receipt_private.nonempty ~label:"layout ABI" layout_abi in
   let* () = Numeric_receipt_private.positive ~label:"layout width" width in
   Ok { layout_class; layout_abi; width; signed })
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-layout-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-layout-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let layout_material (value : layout) =
  Numeric_receipt_private.encode ~schema:"verocaml.carrier-layout-claim.v1"
    [ value.layout_class; value.layout_abi; string_of_int value.width;
      string_of_bool value.signed ]

let compare_layout (left : layout) (right : layout) =
  String.compare (layout_material left) (layout_material right)

let profile_material ~compiler_abi ~toolchain_abi ~target_identity ~layouts
    ~supported_widths
    ~(logical_bv_width_permission : logical_bv_width_permission)
    ~issuer_claim =
  Numeric_receipt_private.encode ~schema
    [ compiler_abi; toolchain_abi; target_identity;
      Numeric_receipt_private.list (List.map layout_material layouts);
      Numeric_receipt_private.list (List.map string_of_int supported_widths);
      logical_bv_width_permission.full_key;
      logical_bv_width_permission.checked_digest;
      issuer_claim ]

let issue_claim ~compiler_abi ~toolchain_abi ~target_identity ~layouts
    ~supported_widths
    ~(logical_bv_width_permission : logical_bv_width_permission)
    ~issuer_claim =
  let result =
    let required =
      [ compiler_abi; toolchain_abi; target_identity; issuer_claim ]
    in
    if List.exists (String.equal "") required then
      Error "target profile claim contains an empty identity"
    else if layouts = [] then Error "target profile claim has no layouts"
    else if supported_widths = [] then
      Error "target profile claim has no supported widths"
    else if List.exists (fun width -> width <= 0) supported_widths then
      Error "target profile claim contains a nonpositive width"
    else
      if
        not
          (String.equal logical_bv_width_permission.schema
             logical_bv_width_permission_schema)
        || not
             (String.equal logical_bv_width_permission.domain
                positive_logical_bv_width_domain)
        || not
             (String.equal logical_bv_width_permission.issuer_claim
                issuer_claim)
      then Error "logical BV width permission differs from the profile issuer"
      else
      let layouts = List.sort compare_layout layouts in
      let layout_keys = List.map layout_material layouts in
      let supported_widths = List.sort Int.compare supported_widths in
      let layout_widths =
        layouts |> List.map (fun (layout : layout) -> layout.width)
        |> List.sort_uniq Int.compare
      in
      if
        List.length layout_keys
        <> List.length (List.sort_uniq String.compare layout_keys)
      then Error "target profile claim contains a duplicate full layout"
      else if
        List.length supported_widths
        <> List.length (List.sort_uniq Int.compare supported_widths)
      then Error "target profile claim contains a duplicate supported width"
      else if layout_widths <> supported_widths then
        Error "target profile claim is incomplete for its supported width set"
      else
        let full_key =
          profile_material ~compiler_abi ~toolchain_abi ~target_identity
            ~layouts ~supported_widths ~logical_bv_width_permission
            ~issuer_claim
        in
        let checked_digest =
          Numeric_receipt_private.digest ~domain:profile_digest_domain full_key
        in
        Ok
          { schema; compiler_abi; toolchain_abi; target_identity; layouts;
            supported_widths; logical_bv_width_permission; issuer_claim;
            full_key; checked_digest }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-profile-claim-issuance")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-profile-claim-issuance")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let decode_layout encoded =
  (match
     Numeric_receipt_private.decode
       ~schema:"verocaml.carrier-layout-claim.v1" ~field_count:4 encoded
   with
  | Error reason -> Error reason
  | Ok [ layout_class; layout_abi; width; signed ] -> (
      match (int_of_string_opt width, bool_of_string_opt signed) with
      | Some width, Some signed -> layout ~layout_class ~layout_abi ~width ~signed
      | _ -> Error "invalid target layout claim scalar")
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-layout-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-layout-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_claim encoded =
  (match Numeric_receipt_private.decode ~schema ~field_count:8 encoded with
  | Error reason -> Error reason
  | Ok
      [ compiler_abi; toolchain_abi; target_identity; encoded_layouts;
        encoded_widths; permission_full_key; permission_checked_digest;
        issuer_claim ] ->
      let* layout_fields = Numeric_receipt_private.decode_list encoded_layouts in
      let rec layouts values = function
        | [] -> Ok (List.rev values)
        | field :: rest ->
            let* value = decode_layout field in
            layouts (value :: values) rest
      in
      let* layouts = layouts [] layout_fields in
      let* width_fields = Numeric_receipt_private.decode_list encoded_widths in
      let rec widths values = function
        | [] -> Ok (List.rev values)
        | field :: rest -> (
            match int_of_string_opt field with
            | Some value -> widths (value :: values) rest
            | None -> Error "invalid target profile claim width")
      in
      let* supported_widths = widths [] width_fields in
      let* logical_bv_width_permission =
        issue_logical_bv_width_permission ~issuer_claim
      in
      let* () =
        if
          String.equal permission_full_key
            logical_bv_width_permission.full_key
          && String.equal permission_checked_digest
               logical_bv_width_permission.checked_digest
        then Ok ()
        else Error "logical BV width permission substitution"
      in
      let* value =
        issue_claim ~compiler_abi ~toolchain_abi ~target_identity ~layouts
          ~supported_widths ~logical_bv_width_permission ~issuer_claim
      in
      if String.equal encoded value.full_key then Ok value
      else Error "target profile claim is not canonical"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-profile-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-profile-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let instance_material (profile : claim) (layout : layout) =
  Numeric_receipt_private.encode ~schema:instance_schema
    [ profile.full_key; profile.checked_digest; profile.target_identity;
      layout.layout_class; layout.layout_abi; string_of_int layout.width;
      string_of_bool layout.signed;
      profile.logical_bv_width_permission.full_key;
      profile.logical_bv_width_permission.checked_digest;
      profile.issuer_claim ]

let instantiate_claim profile ~layout =
  (match
     List.filter
       (fun candidate -> compare_layout candidate layout = 0)
       profile.layouts
   with
  | [ exact ] ->
      let full_key = instance_material profile exact in
      let checked_digest =
        Numeric_receipt_private.digest ~domain:instance_digest_domain full_key
      in
      Ok
        { schema = instance_schema; profile_full_key = profile.full_key;
          profile_checked_digest = profile.checked_digest;
          target_identity = profile.target_identity;
          layout_class = exact.layout_class; layout_abi = exact.layout_abi;
          width = exact.width; signed = exact.signed;
          logical_bv_width_permission_full_key =
            profile.logical_bv_width_permission.full_key;
          logical_bv_width_permission_checked_digest =
            profile.logical_bv_width_permission.checked_digest;
          issuer_claim = profile.issuer_claim; full_key; checked_digest }
  | [] -> Error "profile claim lacks the requested complete layout identity"
  | _ -> Error "profile claim has an ambiguous complete layout identity")
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-instance-claim-issuance")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-instance-claim-issuance")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_instance_claim ~profile encoded =
  (match
     Numeric_receipt_private.decode ~schema:instance_schema ~field_count:10
       encoded
   with
  | Error reason -> Error reason
  | Ok
      [ profile_full_key; profile_checked_digest; target_identity; layout_class;
        layout_abi; width; signed; permission_full_key;
        permission_checked_digest; issuer_claim ] -> (
      match (int_of_string_opt width, bool_of_string_opt signed) with
      | Some width, Some signed ->
          let* layout = layout ~layout_class ~layout_abi ~width ~signed in
          let* value = instantiate_claim profile ~layout in
          if
            String.equal profile_full_key value.profile_full_key
            && String.equal profile_checked_digest value.profile_checked_digest
            && String.equal target_identity value.target_identity
            && String.equal issuer_claim value.issuer_claim
            && String.equal permission_full_key
                 value.logical_bv_width_permission_full_key
            && String.equal permission_checked_digest
                 value.logical_bv_width_permission_checked_digest
            && String.equal encoded value.full_key
          then Ok value
          else Error "target instance claim differs from its profile claim"
      | _ -> Error "invalid target instance claim scalar")
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-instance-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "target-instance-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let compare_claim (left : claim) (right : claim) =
  String.compare left.full_key right.full_key

let equal_claim (left : claim) (right : claim) =
  compare_claim left right = 0

let compare_instance_claim (left : instance_claim) (right : instance_claim) =
  String.compare left.full_key right.full_key

let equal_instance_claim (left : instance_claim) (right : instance_claim) =
  compare_instance_claim left right = 0
