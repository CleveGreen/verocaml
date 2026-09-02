type role = Mathematical_int

type t = {
  schema : string;
  role : role;
  provider_origin : string;
  type_path : string;
  type_uid : string;
  manifest_path : string;
  manifest_uid : string;
  integer_literal_path : string;
  integer_literal_uid : string;
  issuer : string;
}

let schema = "verocaml.base-logical-sort.v1"
let role_name Mathematical_int = "mathematical-int"

let frame value = string_of_int (String.length value) ^ ":" ^ value

let fields value =
  [
    value.schema;
    role_name value.role;
    value.provider_origin;
    value.type_path;
    value.type_uid;
    value.manifest_path;
    value.manifest_uid;
    value.integer_literal_path;
    value.integer_literal_uid;
    value.issuer;
  ]

let canonical_material value =
  fields value |> List.map frame |> String.concat ""

let origin_material value =
  [ value.provider_origin; value.type_path; value.type_uid ]
  |> List.map frame |> String.concat ""

let digest value =
  Digest.string ("verocaml.logical-sort.full-key\000" ^ canonical_material value)
  |> Digest.to_hex

let compare left right = String.compare (canonical_material left) (canonical_material right)
let equal left right = compare left right = 0

let create ~provider_origin ~type_path ~type_uid ~manifest_path ~manifest_uid
    ~integer_literal_path ~integer_literal_uid ~issuer =
  let critical =
    [
      provider_origin;
      type_path;
      type_uid;
      manifest_path;
      manifest_uid;
      integer_literal_path;
      integer_literal_uid;
      issuer;
    ]
  in
  if List.exists (String.equal "") critical then (
    [%log.debug "rejected incomplete base logical-sort descriptor"
      ~stage:(Delator.Field.string "logical-sort-descriptor")
      ~role:(Delator.Field.string "mathematical-int")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "empty-critical-field")];
    Error "base logical-sort descriptor contains an empty critical field")
  else if not (String.equal manifest_path "int") then (
    [%log.debug "rejected non-integer base logical-sort manifest"
      ~stage:(Delator.Field.string "logical-sort-descriptor")
      ~role:(Delator.Field.string "mathematical-int")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "wrong-manifest-sort")];
    Error "mathematical-int descriptor does not resolve to the compiler int type")
  else
    let value =
      {
        schema;
        role = Mathematical_int;
        provider_origin;
        type_path;
        type_uid;
        manifest_path;
        manifest_uid;
        integer_literal_path;
        integer_literal_uid;
        issuer;
      }
    in
    [%log.debug "issued canonical base logical-sort descriptor"
      ~stage:(Delator.Field.string "logical-sort-descriptor")
      ~role:(Delator.Field.string "mathematical-int")
      ~provider_class:(Delator.Field.string "compiler-resolved")
      ~decision:(Delator.Field.string "accepted")];
    Ok value
[@@delator.instrument] [@@delator.level debug]

exception Decode of string

type cursor = { encoded : string; mutable offset : int }

let take cursor =
  let fail reason = raise (Decode reason) in
  let colon =
    match String.index_from_opt cursor.encoded cursor.offset ':' with
    | Some index -> index
    | None -> fail "truncated-frame"
  in
  let length =
    try
      int_of_string
        (String.sub cursor.encoded cursor.offset (colon - cursor.offset))
    with Failure _ -> fail "invalid-frame-length"
  in
  let start = colon + 1 in
  let available = String.length cursor.encoded - start in
  if length < 0 || length > available then fail "frame-bounds";
  let value = String.sub cursor.encoded start length in
  cursor.offset <- start + length;
  value

let decode_canonical encoded =
  try
    let cursor = { encoded; offset = 0 } in
    let decoded_schema = take cursor in
    let decoded_role = take cursor in
    let provider_origin = take cursor in
    let type_path = take cursor in
    let type_uid = take cursor in
    let manifest_path = take cursor in
    let manifest_uid = take cursor in
    let integer_literal_path = take cursor in
    let integer_literal_uid = take cursor in
    let issuer = take cursor in
    if cursor.offset <> String.length encoded then raise (Decode "trailing-data");
    if not (String.equal decoded_schema schema) then raise (Decode "schema");
    if not (String.equal decoded_role "mathematical-int") then
      raise (Decode "role");
    match
      create ~provider_origin ~type_path ~type_uid ~manifest_path ~manifest_uid
        ~integer_literal_path ~integer_literal_uid ~issuer
    with
    | Error reason -> Error reason
    | Ok value ->
        if String.equal encoded (canonical_material value) then Ok value
        else Error "base logical-sort descriptor is not canonical"
  with Decode reason ->
    [%log.debug "rejected encoded base logical-sort descriptor"
      ~stage:(Delator.Field.string "logical-sort-descriptor-decode")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string reason)];
    Error reason
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]
