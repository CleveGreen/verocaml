type t = {
  width : int;
  profile_full_key : string;
  target_full_key : string;
  evidence_full_key : string;
  full_key : string;
  checked_digest : string;
}

let schema = "verocaml.authenticated-numeric-semantic-width.v1"
let digest_domain = "verocaml.authenticated-numeric-semantic-width.full-key.v1"

let issue ~(target : Build_target_profile_private.instance) ~width
    ~evidence_full_key =
  let result =
    let capability = Build_target_profile_private.capability () in
    match Build_target_profile_private.decode_instance capability target.full_key with
    | Error reason -> Error reason
    | Ok authenticated ->
        if
          not
            (Build_target_profile_private.equal_instance authenticated target)
        then Error "numeric semantic width target substitution"
        else if evidence_full_key = "" then
          Error "numeric semantic width has no exact statement evidence"
        else if width <= 0 then Error "numeric semantic width is not positive"
        else if
          not
            (Build_target_profile_private.instance_logical_bv_width_permitted
               target (Z.of_int width))
        then Error "numeric semantic width is outside the sealed profile"
        else
          let profile_full_key = target.profile_full_key in
          let target_full_key = target.full_key in
          let full_key =
            Numeric_receipt_private.encode ~schema
              [ profile_full_key; target_full_key; string_of_int width;
                evidence_full_key ]
          in
          let checked_digest =
            Numeric_receipt_private.digest ~domain:digest_domain full_key
          in
          Ok
            { width; profile_full_key; target_full_key; evidence_full_key;
              full_key; checked_digest }
  in
  (match result with
  | Ok (semantic [@log_value.trace]) ->
      [%log.trace "issued authenticated numeric semantic width"
        ~stage:(Delator.Field.string "numeric-semantic-width")
        ~semantic_width:
          (Delator.Field.int (semantic [@log_value.trace]).width)
        ~machine_width:(Delator.Field.int target.target_claim.width)
        ~target:(Delator.Field.string target.full_key)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected numeric semantic width"
        ~stage:(Delator.Field.string "numeric-semantic-width")
        ~semantic_width:(Delator.Field.int width)
        ~machine_width:(Delator.Field.int target.target_claim.width)
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let equal left right = String.equal left.full_key right.full_key
