let frame value = string_of_int (String.length value) ^ ":" ^ value

let maximum_record_bytes = 64 * 1024 * 1024
let maximum_field_count = 100_000

let encode ~schema fields =
  List.map frame (schema :: fields) |> String.concat ""

let digest ~domain value =
  Digest.string (domain ^ "\000" ^ value) |> Digest.to_hex

let nonempty ~label value =
  if String.equal value "" then Error (label ^ " is empty") else Ok ()

let positive ~label value =
  if value <= 0 then Error (label ^ " is not positive") else Ok ()

let unique ~label values =
  if List.length values = List.length (List.sort_uniq String.compare values) then
    Ok ()
  else Error (label ^ " contains an exact duplicate")

let sorted_unique ~label values =
  match unique ~label values with
  | Error _ as error -> error
  | Ok () -> Ok (List.sort String.compare values)

let canonical_members ~full_key:(full_key [@delator.skip]) (values [@delator.skip]) =
  let members = List.sort_uniq (fun left right -> String.compare (full_key left) (full_key right)) values in
  [%log.trace "canonicalized full-key receipt membership"
    ~input_count:(Delator.Field.int (List.length values))
    ~member_count:(Delator.Field.int (List.length members))];
  members
[@@delator.instrument] [@@delator.level trace]

let bool = string_of_bool
let int = string_of_int
let list values = encode ~schema:"verocaml.canonical-list.v1" values

exception Decode of string

type cursor = { encoded : string; mutable offset : int }

let take cursor =
  let fail reason = raise (Decode reason) in
  let colon =
    match String.index_from_opt cursor.encoded cursor.offset ':' with
    | Some index -> index
    | None -> fail "truncated-frame"
  in
  let length_text =
    String.sub cursor.encoded cursor.offset (colon - cursor.offset)
  in
  if
    String.equal length_text ""
    || (String.length length_text > 1 && Char.equal length_text.[0] '0')
  then fail "noncanonical-frame-length";
  let length =
    try int_of_string length_text with Failure _ -> fail "invalid-frame-length"
  in
  let start = colon + 1 in
  let available = String.length cursor.encoded - start in
  if length < 0 || length > available then fail "frame-bounds";
  let value = String.sub cursor.encoded start length in
  cursor.offset <- start + length;
  value

let decode ~schema ~field_count encoded =
  let result =
  if field_count < 0 || field_count > maximum_field_count then
    Error "field-count-bounds"
  else if String.length encoded > maximum_record_bytes then Error "record-size-bounds"
  else
  try
    let cursor = { encoded; offset = 0 } in
    let decoded_schema = take cursor in
    if not (String.equal decoded_schema schema) then raise (Decode "schema");
    let fields = List.init field_count (fun _ -> take cursor) in
    if cursor.offset <> String.length encoded then raise (Decode "trailing-data");
    if not (String.equal encoded (encode ~schema fields)) then
      raise (Decode "noncanonical-encoding");
    Ok fields
  with Decode reason -> Error reason
  in
  (match result with
  | Ok _ ->
      [%log.trace "decoded canonical numeric receipt record"
        ~stage:(Delator.Field.string "numeric-receipt-decode")
        ~schema:(Delator.Field.string schema)
        ~field_count:(Delator.Field.int field_count)
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected canonical numeric receipt record"
        ~stage:(Delator.Field.string "numeric-receipt-decode")
        ~schema:(Delator.Field.string schema)
        ~field_count:(Delator.Field.int field_count)
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]

let decode_list encoded =
  let result =
  if String.length encoded > maximum_record_bytes then Error "list-size-bounds"
  else
  try
    let cursor = { encoded; offset = 0 } in
    let decoded_schema = take cursor in
    if not (String.equal decoded_schema "verocaml.canonical-list.v1") then
      raise (Decode "list-schema");
    let rec loop count fields =
      if count > maximum_field_count then Error "list-count-bounds"
      else
      if cursor.offset = String.length encoded then
        let fields = List.rev fields in
        if String.equal encoded (list fields) then Ok fields
        else Error "noncanonical-list-encoding"
      else loop (count + 1) (take cursor :: fields)
    in
    loop 0 []
  with Decode reason -> Error reason
  in
  (match result with
  | Ok (fields [@log_value.trace]) ->
      [%log.trace "decoded canonical numeric receipt list"
        ~stage:(Delator.Field.string "numeric-receipt-list-decode")
        ~field_count:
          (Delator.Field.int
             (List.length (fields [@log_value.trace])))
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected canonical numeric receipt list"
        ~stage:(Delator.Field.string "numeric-receipt-list-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]
