type kind = Declaration | Group
type trust = Proved | Trusted

type identity = {
  provider_origin : string;
  interface_digest : string;
  compiler_uid : string;
  kind : kind;
  canonical_path : string;
  dependency_receipt : string;
  witness_receipt : string;
  artifact_family : string;
  provider_route : string;
}

type interface_member = {
  identity : identity;
  source_members : source_reference list;
}

and source_reference = {
  member_slot : int;
  member_provider_origin : string;
  member_interface_receipt : string;
  member_dependency_receipt : string;
  member_compiler_uid : string;
  member_kind : kind;
  member_canonical_path : string;
}

let kind_name = function Declaration -> "declaration" | Group -> "group"

let framed fields =
  fields
  |> List.map (fun field -> Printf.sprintf "%d:%s" (String.length field) field)
  |> String.concat ""

let identity_key identity =
  framed
    [
      identity.provider_origin;
      identity.interface_digest;
      identity.compiler_uid;
      kind_name identity.kind;
      identity.canonical_path;
      identity.dependency_receipt;
      identity.witness_receipt;
      identity.artifact_family;
      identity.provider_route;
    ]

let compare_identity left right =
  String.compare (identity_key left) (identity_key right)

let equal_identity left right = compare_identity left right = 0

let correlation identity =
  "broadcast-"
  ^ (Digest.string ("retained-broadcast-v1" ^ identity_key identity)
    |> Digest.to_hex)

let hexadecimal value =
  String.length value = 32
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

let identity_validation ~provider:_provider ~kind:_kind ~decision:_decision
    ~reason_class:_reason_class =
  [%log.debug "validated retained broadcast identity envelope"
    ~provider:(Delator.Field.string _provider)
    ~family:(Delator.Field.string "retained-v1")
    ~stage:(Delator.Field.string "identity-validation")
    ~member_kind:(Delator.Field.string (kind_name _kind))
    ~decision:(Delator.Field.string _decision)
    ~reason_class:(Delator.Field.string _reason_class)]

let make_identity
    ~provider_origin:(provider_origin [@delator.field Fun.id])
    ~interface_digest:(interface_digest [@delator.skip])
    ~compiler_uid:(compiler_uid [@delator.skip])
    ~kind:(kind
      [@delator.field (fun kind -> kind_name kind)])
    ~canonical_path:(canonical_path [@delator.skip])
    ~dependency_receipt:(dependency_receipt [@delator.skip])
    ~witness_receipt:(witness_receipt [@delator.skip])
    ~artifact_family:(artifact_family [@delator.field Fun.id])
    ~provider_route:(provider_route [@delator.field Fun.id]) =
  if provider_origin = "" || compiler_uid = "" || canonical_path = "" then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"incomplete-identity";
    Error "retained broadcast identity is incomplete")
  else if
    not
      (String.equal canonical_path provider_origin
      || String.starts_with ~prefix:(provider_origin ^ ".") canonical_path)
  then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"provider-path-mismatch";
    Error "retained broadcast path is outside its provider origin")
  else if not (hexadecimal interface_digest) then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"interface-receipt-shape";
    Error "retained broadcast interface receipt is malformed")
  else if not (hexadecimal dependency_receipt) then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"dependency-receipt-shape";
    Error "retained broadcast dependency receipt is malformed")
  else if not (hexadecimal witness_receipt) then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"witness-receipt-shape";
    Error "retained broadcast witness receipt is malformed")
  else if not (String.equal artifact_family "retained-v1") then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"artifact-family";
    Error "retained broadcast artifact family is unsupported")
  else if
    not
      (String.equal provider_route "standalone-v1"
      || String.equal provider_route "ppxlib-v1")
  then (
    identity_validation ~provider:provider_origin ~kind ~decision:"rejected"
      ~reason_class:"provider-route";
    Error "retained broadcast provider route is unsupported")
  else (
    [%log.trace "accepted retained broadcast identity envelope"
      ~provider:(Delator.Field.string provider_origin)
      ~family:(Delator.Field.string artifact_family)
      ~route:(Delator.Field.string provider_route)
      ~stage:(Delator.Field.string "identity-validation")
      ~member_kind:(Delator.Field.string (kind_name kind))
      ~decision:(Delator.Field.string "accepted")];
    Ok
      {
        provider_origin;
        interface_digest;
        compiler_uid;
        kind;
        canonical_path;
        dependency_receipt;
        witness_receipt;
        artifact_family;
        provider_route;
      })
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]

let duplicate_identity identities =
  let sorted = List.sort compare_identity identities in
  let rec loop = function
    | left :: (right :: _ as rest) ->
        if equal_identity left right then Some left else loop rest
    | [] | [ _ ] -> None
  in
  loop sorted

let canonical_set
    (identities
      [@delator.field (fun values -> string_of_int (List.length values))]) =
  match duplicate_identity identities with
  | Some duplicate ->
      [%log.debug "rejected duplicate retained broadcast identity"
        ~provider:(Delator.Field.string duplicate.provider_origin)
        ~stage:(Delator.Field.string "identity-set-canonicalization")
        ~member_kind:(Delator.Field.string (kind_name duplicate.kind))
        ~set_cardinality:(Delator.Field.int (List.length identities))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "duplicate-full-identity")];
      Error duplicate
  | None ->
      [%log.trace "canonicalized retained broadcast identity set"
        ~stage:(Delator.Field.string "identity-set-canonicalization")
        ~set_cardinality:(Delator.Field.int (List.length identities))
        ~decision:(Delator.Field.string "accepted")];
      Ok (List.sort compare_identity identities)
[@@delator.instrument]
[@@delator.level trace]
[@@delator.no_exn_log]
