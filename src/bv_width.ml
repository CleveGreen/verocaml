type t = {
  width : int;
  profile_full_key : string;
  target_full_key : string option;
  capability_full_key : string;
  capability_id : string;
}

let ( let* ) = Result.bind

let bounded_decimal @ portable = fun text ->
  let length = String.length text in
  if length > 4 then Error "BV width text exceeds four bytes"
  else if length = 0 then Error "BV width text is empty"
  else if length > 1 && Char.equal text.[0] '0' then
    Error "BV width text is not canonical decimal"
  else
    let rec accumulate index value =
      if index = length then Ok value
      else
        let code = Char.code text.[index] - Char.code '0' in
        if code < 0 || code > 9 then
          Error "BV width text is not unsigned decimal"
        else accumulate (index + 1) ((value * 10) + code)
    in
    accumulate 0 0

let make ~profile capability candidate =
  let* receipt, _ =
    Bv_backend_capability_receipt_private.authenticate capability
  in
  let candidate_z = Z.of_int candidate in
  if candidate <= 0 then Error "BV width must be positive"
  else if candidate > receipt.maximum_bv_width then
    Error "BV width exceeds the sealed backend capability"
  else if candidate_z > receipt.ocaml_max_int then
    Error "BV width exceeds the pinned OCaml ABI"
  else if candidate_z > receipt.c_unsigned_maximum then
    Error "BV width exceeds the pinned Z3 C ABI"
  else
    let* () =
      Bv_backend_capability_receipt_private.authenticate_profile capability
        profile
    in
    if Build_target_profile_private.logical_bv_width_permitted profile candidate_z
    then
      Ok
        { width = candidate; profile_full_key = profile.full_key;
          target_full_key = None; capability_full_key = receipt.full_key;
          capability_id =
            Bv_backend_capability_receipt_private.id_to_hex
              receipt.checked_id }
    else Error "sealed target profile does not permit this logical BV width"

let of_string ~profile capability text =
  let result = Result.bind (bounded_decimal text) (make ~profile capability) in
  (match result with
  | Ok (width [@log_value.trace]) ->
      [%log.trace "validated bounded BV width with exact provenance"
        ~stage:(Delator.Field.string "bv-width-validation")
        ~source:(Delator.Field.string "bounded-canonical-decimal")
        ~width:(Delator.Field.int (width [@log_value.trace]).width)
        ~profile:
          (Delator.Field.string (width [@log_value.trace]).profile_full_key)
        ~capability_id:
          (Delator.Field.string (width [@log_value.trace]).capability_id)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected BV width before width-derived allocation"
        ~stage:(Delator.Field.string "bv-width-validation")
        ~input_bytes:(Delator.Field.int (String.length text))
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let of_z ~profile capability value =
  let result =
    let* receipt, _ =
      Bv_backend_capability_receipt_private.authenticate capability
    in
    if Z.sign value <= 0 then Error "BV width must be positive"
    else if value > Z.of_int receipt.maximum_bv_width then
      Error "BV width exceeds the sealed backend capability"
    else if value > receipt.ocaml_max_int then
      Error "BV width exceeds the pinned OCaml ABI"
    else if value > receipt.c_unsigned_maximum then
      Error "BV width exceeds the pinned Z3 C ABI"
    else make ~profile capability (Z.to_int value)
  in
  (match result with
  | Ok (width [@log_value.trace]) ->
      [%log.trace "validated mathematical BV width with exact provenance"
        ~stage:(Delator.Field.string "bv-width-validation")
        ~source:(Delator.Field.string "mathematical-integer")
        ~width:(Delator.Field.int (width [@log_value.trace]).width)
        ~profile:
          (Delator.Field.string (width [@log_value.trace]).profile_full_key)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected mathematical BV width before host conversion"
        ~stage:(Delator.Field.string "bv-width-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let for_instance capability instance width =
  let result =
    let* () =
      Bv_backend_capability_receipt_private.authenticate_instance capability
        instance
    in
    let build_capability = Build_target_profile_private.capability () in
    let* profile =
      Build_target_profile_private.decode_profile build_capability
        instance.profile_full_key
    in
    let* () =
      if String.equal profile.full_key width.profile_full_key then Ok ()
      else Error "BV width belongs to a different sealed profile"
    in
    let* receipt, _ =
      Bv_backend_capability_receipt_private.authenticate capability
    in
    let* () =
      if String.equal receipt.full_key width.capability_full_key then Ok ()
      else Error "BV width belongs to a different backend capability"
    in
    if
      Build_target_profile_private.instance_logical_bv_width_permitted instance
        (Z.of_int width.width)
    then Ok { width with target_full_key = Some instance.full_key }
    else Error "sealed target instance does not permit this logical BV width"
  in
  (match result with
  | Ok (width [@log_value.trace]) ->
      [%log.trace "bound logical BV width independently to machine target"
        ~stage:(Delator.Field.string "bv-instance-width-admission")
        ~logical_width:(Delator.Field.int (width [@log_value.trace]).width)
        ~machine_width:(Delator.Field.int instance.target_claim.width)
        ~target:(Delator.Field.string instance.full_key)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected logical BV width for target instance"
        ~stage:(Delator.Field.string "bv-instance-width-admission")
        ~logical_width:(Delator.Field.int width.width)
        ~machine_width:(Delator.Field.int instance.target_claim.width)
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let to_int @ portable = fun width -> width.width
let to_z width = Z.of_int width.width
let to_string width = string_of_int width.width
let profile_full_key width = width.profile_full_key
let target_full_key width = width.target_full_key

let structural_identity_material width =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  let target_state, target_full_key =
    match width.target_full_key with
    | None -> ("unbound", "")
    | Some full_key -> ("bound", full_key)
  in
  "bv-width-identity-v1:"
  ^ String.concat ""
      (List.map field
         [ string_of_int width.width; width.profile_full_key;
           target_state; target_full_key;
           width.capability_full_key; width.capability_id ])

let compare left right =
  Stdlib.compare
    ( left.width, left.profile_full_key, left.target_full_key,
      left.capability_full_key, left.capability_id )
    ( right.width, right.profile_full_key, right.target_full_key,
      right.capability_full_key, right.capability_id )

let equal left right = compare left right = 0

let authenticate_bound capability width =
  let* receipt, _ =
    Bv_backend_capability_receipt_private.authenticate capability
  in
  if not (String.equal receipt.full_key width.capability_full_key) then
    Error "BV width backend capability full key changed"
  else if
    not
      (String.equal
         (Bv_backend_capability_receipt_private.id_to_hex receipt.checked_id)
         width.capability_id)
  then Error "BV width backend capability ID changed"
  else
    let profile_capability = Build_target_profile_private.capability () in
    let* profile =
      Build_target_profile_private.decode_profile profile_capability
        width.profile_full_key
    in
    match width.target_full_key with
    | None -> Error "BV width is not bound to an exact machine target"
    | Some target_full_key ->
        let* target =
          Build_target_profile_private.decode_instance profile_capability
            target_full_key
        in
        if not (String.equal target.profile_full_key profile.full_key) then
          Error "BV width target belongs to a different profile"
        else if
          Build_target_profile_private.instance_logical_bv_width_permitted target
            (Z.of_int width.width)
        then Ok ()
        else Error "BV width is outside its authenticated target permission"

let reference_schema = "verocaml.bv-width-reference.v1"

let encode_reference capability width =
  let* () = authenticate_bound capability width in
  let* backend_reference =
    Bv_backend_capability_receipt_private.reference capability
  in
  match width.target_full_key with
  | None -> Error "BV width is not bound to an exact machine target"
  | Some target_full_key ->
      Ok
        (Numeric_receipt_private.encode ~schema:reference_schema
           [ string_of_int width.width; width.profile_full_key; target_full_key;
             Bv_backend_capability_receipt_private.encode_reference
               backend_reference ])

exception Portable_decode of string

let decode_reference_fields @ portable = fun encoded ->
  if String.length encoded > 64 * 1024 * 1024 then
    Error "detached BV width reference exceeds the receipt bound"
  else
    try
      let offset = ref 0 in
      let take ~field ~maximum_bytes =
        let colon =
          match String.index_from_opt encoded !offset ':' with
          | Some index -> index
          | None -> raise (Portable_decode "truncated detached BV width frame")
        in
        let digits = colon - !offset in
        if digits <= 0 || digits > 8 then
          raise (Portable_decode "invalid detached BV width frame length");
        let length = ref 0 in
        for index = !offset to colon - 1 do
          let digit = Char.code encoded.[index] - Char.code '0' in
          if digit < 0 || digit > 9 then
            raise (Portable_decode "invalid detached BV width frame length");
          if index = !offset && digits > 1 && digit = 0 then
            raise (Portable_decode "noncanonical detached BV width frame length");
          length := (!length * 10) + digit
        done;
        (match maximum_bytes with
        | Some maximum when !length > maximum ->
            raise
              (Portable_decode
                 (Printf.sprintf "detached BV width %s exceeds %d bytes" field
                    maximum))
        | Some _ | None -> ());
        let start = colon + 1 in
        if !length > String.length encoded - start then
          raise (Portable_decode "detached BV width frame exceeds its record");
        let value = String.sub encoded start !length in
        offset := start + !length;
        value
      in
      let schema =
        take ~field:"schema" ~maximum_bytes:(Some (String.length reference_schema))
      in
      if not (String.equal schema reference_schema) then
        raise (Portable_decode "detached BV width reference schema mismatch");
      let width = take ~field:"text" ~maximum_bytes:(Some 4) in
      let profile = take ~field:"profile" ~maximum_bytes:None in
      let target = take ~field:"target" ~maximum_bytes:None in
      let backend = take ~field:"backend reference" ~maximum_bytes:(Some 512) in
      let fields = [ width; profile; target; backend ] in
      if !offset <> String.length encoded then
        raise (Portable_decode "detached BV width reference has trailing data");
      Ok fields
    with Portable_decode reason -> Error reason

let decode_reference capability encoded =
  let* fields = decode_reference_fields encoded in
  match fields with
  | [ width_text; profile_full_key; target_full_key; backend_reference ] ->
      if String.length width_text > 4 then
        Error "detached BV width text exceeds four bytes"
      else
        let* _ =
          Bv_backend_capability_receipt_private.decode_reference capability
            backend_reference
        in
        let profile_capability = Build_target_profile_private.capability () in
        let* profile =
          Build_target_profile_private.decode_profile profile_capability
            profile_full_key
        in
        let* target =
          Build_target_profile_private.decode_instance profile_capability
            target_full_key
        in
        let* width = of_string ~profile capability width_text in
        let* width = for_instance capability target width in
        let* () = authenticate_bound capability width in
        Ok width
  | _ -> Error "malformed detached BV width reference"

let decode_reference_for_worker @ portable = fun encoded ->
  let* fields = decode_reference_fields encoded in
  match fields with
  | [ width_text; profile_full_key; target_full_key; backend_reference ] ->
      if String.length width_text > 4 then
        Error "detached BV width text exceeds four bytes"
      else
        let* receipt =
          Bv_backend_capability_receipt_private.decode_reference_for_worker
            backend_reference
        in
        let* width = bounded_decimal width_text in
        if width <= 0 then Error "detached BV width must be positive"
        else if width > receipt.maximum_bv_width then
          Error "detached BV width exceeds the sealed backend capability"
        else
          if not receipt.abi_supports_ceiling then
            Error "detached pinned ABI cannot represent the BV width ceiling"
          else
            let* profile =
              Build_target_profile_private.decode_profile_for_worker
                profile_full_key
            in
            let* target =
              Build_target_profile_private.decode_instance_for_worker
                target_full_key
            in
            if not (String.equal target.profile_full_key profile.full_key) then
              Error "detached BV width target belongs to a different profile"
            else if
              not
                (Build_target_profile_private
                 .instance_logical_bv_width_permitted_for_worker target width)
            then Error "detached BV width is outside its local target permission"
            else
              Ok
                { width; profile_full_key; target_full_key = Some target_full_key;
                  capability_full_key = receipt.full_key;
                  capability_id = receipt.checked_id_hex }
  | _ -> Error "malformed detached BV width reference"

module For_testing = struct
  let decode_reference_fields = decode_reference_fields
end
