type t = { width : Bv_width.t; unsigned_bits : Z.t }

let maximum_decimal_bytes = 1234

let canonical_unsigned_decimal text =
  let length = String.length text in
  if length > maximum_decimal_bytes then
    Error "BV literal or residue exceeds 1234 bytes"
  else if length = 0 then Error "BV literal or residue is empty"
  else if length > 1 && Char.equal text.[0] '0' then
    Error "BV literal or residue is not canonical decimal"
  else if String.for_all (fun c -> c >= '0' && c <= '9') text then Ok ()
  else Error "BV literal or residue is not unsigned decimal"

let modulus width = Z.shift_left Z.one (Bv_width.to_int width)

let of_z ~width unsigned_bits =
  if Z.sign unsigned_bits < 0 then Error "BV literal or residue is negative"
  else if unsigned_bits >= modulus width then
    Error "BV literal or residue is outside its width"
  else Ok { width; unsigned_bits }

let of_string ~width text =
  let result =
    match canonical_unsigned_decimal text with
    | Error _ as error -> error
    | Ok () -> of_z ~width (Z.of_string text)
  in
  (match result with
  | Ok (_value [@log_value.trace]) ->
      [%log.trace "validated canonical BV literal or residue"
        ~stage:(Delator.Field.string "bv-value-validation")
        ~width:(Delator.Field.int (Bv_width.to_int width))
        ~decimal_bytes:(Delator.Field.int (String.length text))
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected BV literal or residue before backend construction"
        ~stage:(Delator.Field.string "bv-value-validation")
        ~width:(Delator.Field.int (Bv_width.to_int width))
        ~decimal_bytes:(Delator.Field.int (String.length text))
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let reduce_int ~width value =
  let modulus = modulus width in
  let unsigned_bits = Z.erem value modulus in
  let unsigned_bits =
    if Z.sign unsigned_bits < 0 then Z.add unsigned_bits modulus
    else unsigned_bits
  in
  { width; unsigned_bits }

let signed_value value =
  let width = Bv_width.to_int value.width in
  if Z.testbit value.unsigned_bits (width - 1) then
    Z.sub value.unsigned_bits (modulus value.width)
  else value.unsigned_bits

let render value =
  let width = Bv_width.to_int value.width in
  let bits =
    String.init width (fun index ->
        if Z.testbit value.unsigned_bits (width - index - 1) then '1' else '0')
  in
  Printf.sprintf "bits[%d]=%s unsigned=%s signed=%s" width bits
    (Z.to_string value.unsigned_bits) (Z.to_string (signed_value value))

let canonical_decimal value = Z.to_string value.unsigned_bits

let equal (left : t) (right : t) =
  Bv_width.equal left.width right.width
  && Z.equal left.unsigned_bits right.unsigned_bits
