type dependency = {
  dependency_unit : string;
  dependency_compiler_receipt : string option;
  dependency_interface_receipt : string;
  dependency_authority_receipt : string option;
}

type payload =
  | Broadcast_witnesses of Retained_broadcast_private.interface_member list
  | Logical_sorts of Logical_sort_private.t list

type t = {
  provider_unit : string;
  provider_origin : string;
  compiler_abi : string;
  cmi_receipt : string;
  cmi_self_crc : string;
  cmi_imports : (string * string option) list;
  cmi_identity : string;
  cmti_receipt : string;
  cmti_interface_digest : string option;
  cmti_imports : (string * string option) list;
  cmti_identity : string;
  ordinary_cmi_receipts : string list;
  dependencies : dependency list;
  payloads : payload list;
}

let extension = ".vri"
let magic = "VEROCAML-RETAINED-INTERFACE\000"
let envelope_version = "2"
let section_version = "1"

let frame value = string_of_int (String.length value) ^ ":" ^ value

let kind = function
  | Retained_broadcast_private.Declaration -> "declaration"
  | Group -> "group"

let compare_import (left_unit, left_crc) (right_unit, right_crc) =
  compare (left_unit, left_crc) (right_unit, right_crc)

let canonical_imports imports = List.sort compare_import imports

let compare_dependency left right =
  compare
    ( left.dependency_unit,
      left.dependency_compiler_receipt,
      left.dependency_interface_receipt,
      left.dependency_authority_receipt )
    ( right.dependency_unit,
      right.dependency_compiler_receipt,
      right.dependency_interface_receipt,
      right.dependency_authority_receipt )

let canonical_dependencies dependencies = List.sort compare_dependency dependencies

let compare_source left right =
  compare
    ( left.Retained_broadcast_private.member_slot,
      left.member_provider_origin,
      left.member_interface_receipt,
      left.member_dependency_receipt,
      left.member_compiler_uid,
      kind left.member_kind,
      left.member_canonical_path )
    ( right.Retained_broadcast_private.member_slot,
      right.member_provider_origin,
      right.member_interface_receipt,
      right.member_dependency_receipt,
      right.member_compiler_uid,
      kind right.member_kind,
      right.member_canonical_path )

let canonical_sources sources = List.sort compare_source sources

let compare_member left right =
  let left = left.Retained_broadcast_private.identity
  and right = right.Retained_broadcast_private.identity in
  Retained_broadcast_private.compare_identity left right

let canonical_members members = List.sort compare_member members
let canonical_logical_sorts sorts = List.sort Logical_sort_private.compare sorts

let payload_tokens = function
  | Broadcast_witnesses members ->
      string_of_int (List.length members)
      :: List.concat_map
           (fun member ->
             let identity = member.Retained_broadcast_private.identity in
             let sources = canonical_sources member.source_members in
             [
               identity.provider_origin;
               identity.interface_digest;
               identity.compiler_uid;
               kind identity.kind;
               identity.canonical_path;
               identity.dependency_receipt;
               identity.witness_receipt;
               identity.artifact_family;
               identity.provider_route;
               string_of_int (List.length sources);
             ]
             @ List.concat_map
                 (fun source ->
                   [
                     string_of_int source.Retained_broadcast_private.member_slot;
                     source.member_provider_origin;
                     source.member_interface_receipt;
                     source.member_dependency_receipt;
                     source.member_compiler_uid;
                     kind source.member_kind;
                     source.member_canonical_path;
                   ])
                 sources)
           (canonical_members members)
  | Logical_sorts sorts ->
      string_of_int (List.length sorts)
      :: List.concat_map
           (fun sort ->
             [
               Logical_sort_private.canonical_material sort;
               Logical_sort_private.digest sort;
             ])
           (canonical_logical_sorts sorts)

let tokens values = values |> List.map frame |> String.concat ""

type section = {
  section_name : string;
  section_critical : bool;
  section_payload : string;
}

let section name payload =
  { section_name = name; section_critical = true; section_payload = payload }

let sections value =
  let imports_payload imports =
    string_of_int (List.length imports)
    :: List.concat_map
         (fun (unit_name, crc) -> [ unit_name; Option.value ~default:"" crc ])
         imports
  in
  let dependencies = canonical_dependencies value.dependencies in
  let payloads =
    value.payloads
    |> List.map (fun payload ->
           match payload with
           | Broadcast_witnesses members ->
               section "broadcast-witnesses"
                 (tokens (payload_tokens (Broadcast_witnesses members)))
           | Logical_sorts sorts ->
               section "base-logical-sorts"
                 (tokens (payload_tokens (Logical_sorts sorts))))
  in
  [
    section "provider"
      (tokens [ value.provider_unit; value.provider_origin; value.compiler_abi ]);
    section "cmi"
      (tokens
         ([ value.cmi_receipt; value.cmi_self_crc ]
         @ imports_payload (canonical_imports value.cmi_imports)
         @ [ value.cmi_identity ]));
    section "cmti"
      (tokens
         ([
            value.cmti_receipt;
            Option.value ~default:"" value.cmti_interface_digest;
          ]
         @ imports_payload (canonical_imports value.cmti_imports)
         @ [ value.cmti_identity ]));
    section "dependencies"
      (tokens
         (string_of_int (List.length dependencies)
         :: List.concat_map
              (fun dependency ->
                [
                  dependency.dependency_unit;
                  Option.value ~default:""
                    dependency.dependency_compiler_receipt;
                  dependency.dependency_interface_receipt;
                  Option.value ~default:""
                    dependency.dependency_authority_receipt;
                ])
              dependencies));
    section "ordinary-cmi-views"
      (tokens
         (string_of_int (List.length value.ordinary_cmi_receipts)
         :: List.sort_uniq String.compare value.ordinary_cmi_receipts));
  ]
  @ payloads
  |> List.sort (fun left right ->
         String.compare left.section_name right.section_name)

let body value =
  let sections = sections value in
  frame envelope_version ^ frame (string_of_int (List.length sections))
  ^ String.concat ""
      (List.map
         (fun section ->
           frame section.section_name
           ^ frame section_version
           ^ frame (if section.section_critical then "critical" else "optional")
           ^ frame section.section_payload)
         sections)

let index value = Digest.string (body value) |> Digest.to_hex

let encode value =
  let canonical_body = body value in
  magic ^ canonical_body ^ frame (Digest.string canonical_body |> Digest.to_hex)
[@@delator.instrument] [@@delator.level trace]

let semantic value =
  let payloads =
    List.map
      (function
        | Broadcast_witnesses members ->
            Broadcast_witnesses
              (canonical_members members
              |> List.map (fun member ->
                     {
                       member with
                       Retained_broadcast_private.source_members =
                         canonical_sources
                           member.Retained_broadcast_private.source_members;
                     }))
        | Logical_sorts sorts -> Logical_sorts (canonical_logical_sorts sorts))
      value.payloads
  in
  ( value.provider_unit,
    value.provider_origin,
    value.compiler_abi,
    value.cmi_receipt,
    value.cmi_self_crc,
    canonical_imports value.cmi_imports,
    value.cmi_identity,
    value.cmti_receipt,
    value.cmti_interface_digest,
    canonical_imports value.cmti_imports,
    value.cmti_identity,
    List.sort_uniq String.compare value.ordinary_cmi_receipts,
    canonical_dependencies value.dependencies,
    payloads )

let equal left right = compare (semantic left) (semantic right) = 0

exception Decode of string

type cursor = { encoded : string; mutable offset : int }

let take cursor =
  let fail reason = raise (Decode reason) in
  let colon =
    match String.index_from_opt cursor.encoded cursor.offset ':' with
    | Some value -> value
    | None -> fail "truncated-frame"
  in
  let length =
    try int_of_string (String.sub cursor.encoded cursor.offset (colon - cursor.offset))
    with Failure _ -> fail "frame-length"
  in
  let payload_offset = colon + 1 in
  let available = String.length cursor.encoded - payload_offset in
  if length < 0 || length > available then fail "frame-bounds";
  let value = String.sub cursor.encoded payload_offset length in
  cursor.offset <- payload_offset + length;
  value

let count cursor label =
  let value = try int_of_string (take cursor) with Failure _ -> raise (Decode label) in
  if value < 0 || value > 1_000_000 then raise (Decode label) else value

let exact_end cursor label =
  if cursor.offset <> String.length cursor.encoded then raise (Decode label)

let decode_imports cursor label =
  List.init (count cursor (label ^ "-count")) (fun _ ->
      let unit_name = take cursor in
      let crc = take cursor in
      (unit_name, if String.equal crc "" then None else Some crc))

let decode_kind cursor =
  match take cursor with
  | "declaration" -> Retained_broadcast_private.Declaration
  | "group" -> Group
  | _ -> raise (Decode "broadcast-kind")

let decode_broadcasts payload =
  let cursor = { encoded = payload; offset = 0 } in
  let members =
    List.init (count cursor "broadcast-count") (fun _ ->
        let provider_origin = take cursor
        and interface_digest = take cursor
        and compiler_uid = take cursor in
        let kind = decode_kind cursor in
        let canonical_path = take cursor
        and dependency_receipt = take cursor
        and witness_receipt = take cursor
        and artifact_family = take cursor
        and provider_route = take cursor in
        let identity =
          match
            Retained_broadcast_private.make_identity ~provider_origin
              ~interface_digest ~compiler_uid ~kind ~canonical_path
              ~dependency_receipt ~witness_receipt ~artifact_family
              ~provider_route
          with
          | Ok value -> value
          | Error _ -> raise (Decode "broadcast-identity")
        in
        let source_members =
          List.init (count cursor "source-count") (fun _ ->
              let member_slot =
                try int_of_string (take cursor)
                with Failure _ -> raise (Decode "source-slot")
              in
              if member_slot < 0 then raise (Decode "source-slot");
              let member_provider_origin = take cursor
              and member_interface_receipt = take cursor
              and member_dependency_receipt = take cursor
              and member_compiler_uid = take cursor in
              let member_kind = decode_kind cursor in
              let member_canonical_path = take cursor in
              {
                Retained_broadcast_private.member_slot;
                member_provider_origin;
                member_interface_receipt;
                member_dependency_receipt;
                member_compiler_uid;
                member_kind;
                member_canonical_path;
              })
        in
        { Retained_broadcast_private.identity; source_members })
  in
  exact_end cursor "broadcast-trailing-data";
  Broadcast_witnesses members

let decode_logical_sorts payload =
  let cursor = { encoded = payload; offset = 0 } in
  let sorts =
    List.init (count cursor "logical-sort-count") (fun _ ->
        let encoded_descriptor = take cursor in
        let checked_digest = take cursor in
        match Logical_sort_private.decode_canonical encoded_descriptor with
        | Error _ -> raise (Decode "logical-sort-descriptor")
        | Ok value ->
            let expected_digest = Logical_sort_private.digest value in
            if not (String.equal checked_digest expected_digest) then (
              [%log.debug "rejected inconsistent base logical-sort checked digest"
                ~stage:(Delator.Field.string "logical-sort-payload")
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "digest-record-mismatch")
                ~checked_digest:(Delator.Field.string checked_digest)
                ~expected_digest:(Delator.Field.string expected_digest)];
              raise (Decode "logical-sort-digest"));
            [%log.trace "decoded consistent base logical-sort record"
              ~stage:(Delator.Field.string "logical-sort-payload")
              ~decision:(Delator.Field.string "accepted")
              ~checked_digest:(Delator.Field.string checked_digest)];
            value)
  in
  exact_end cursor "logical-sort-trailing-data";
  Logical_sorts sorts
[@@delator.instrument] [@@delator.level trace]

let validate value =
  let fail reason = raise (Decode reason) in
  let dependency_units = List.map (fun item -> item.dependency_unit) value.dependencies
  and cmi_import_units = List.map fst value.cmi_imports
  and cmti_import_units = List.map fst value.cmti_imports in
  if
    List.exists (String.equal "")
      [
        value.provider_unit;
        value.provider_origin;
        value.compiler_abi;
        value.cmi_receipt;
        value.cmi_self_crc;
        value.cmi_identity;
        value.cmti_receipt;
        value.cmti_identity;
      ]
    || List.exists
         (fun dependency ->
           String.equal dependency.dependency_unit ""
           || String.equal dependency.dependency_interface_receipt "")
         value.dependencies
    || List.exists (fun (unit_name, _) -> String.equal unit_name "") value.cmi_imports
    || List.exists (fun (unit_name, _) -> String.equal unit_name "") value.cmti_imports
  then fail "empty-critical-field";
  let unique label values =
    if List.length values <> List.length (List.sort_uniq String.compare values) then
      fail label
  in
  unique "duplicate-dependency" dependency_units;
  unique "duplicate-cmi-import" cmi_import_units;
  unique "duplicate-cmti-import" cmti_import_units;
  if
    List.exists (String.equal "") value.ordinary_cmi_receipts
    || List.length value.ordinary_cmi_receipts
       <> List.length
            (List.sort_uniq String.compare value.ordinary_cmi_receipts)
  then fail "ordinary-cmi-view-set";
  let members =
    value.payloads
    |> List.concat_map (function
         | Broadcast_witnesses members -> members
         | Logical_sorts _ -> [])
  in
  let member_keys =
    List.map
      (fun member ->
        let identity = member.Retained_broadcast_private.identity in
        ( identity.provider_origin,
          identity.interface_digest,
          identity.compiler_uid,
          kind identity.kind,
          identity.canonical_path,
          identity.dependency_receipt,
          identity.witness_receipt,
          identity.artifact_family,
          identity.provider_route ))
      members
  in
  if List.length member_keys <> List.length (List.sort_uniq compare member_keys) then
    fail "duplicate-broadcast";
  List.iter
    (fun member ->
      let sources = canonical_sources member.Retained_broadcast_private.source_members in
      if List.length sources <> List.length (List.sort_uniq compare_source sources) then
        fail "duplicate-source";
      let slots =
        List.map (fun source -> source.Retained_broadcast_private.member_slot) sources
        |> List.sort Int.compare
      in
      if slots <> List.init (List.length slots) Fun.id then fail "source-slot-set")
    members;
  let logical_sorts =
    value.payloads
    |> List.concat_map (function
         | Logical_sorts sorts -> sorts
         | Broadcast_witnesses _ -> [])
  in
  if
    List.length logical_sorts
    <> List.length
         (List.sort_uniq Logical_sort_private.compare logical_sorts)
  then fail "duplicate-logical-sort"

let decode (encoded [@delator.skip]) =
  let fail reason = raise (Decode reason) in
  try
    if not (String.starts_with ~prefix:magic encoded) then fail "magic";
    let cursor = { encoded; offset = String.length magic } in
    if not (String.equal (take cursor) envelope_version) then fail "version";
    let section_count = count cursor "section-count" in
    let sections =
      List.init section_count (fun _ ->
          let section_name = take cursor in
          if not (String.equal (take cursor) section_version) then
            fail "section-version";
          let section_critical =
            match take cursor with
            | "critical" -> true
            | "optional" -> false
            | _ -> fail "section-criticality"
          in
          let section_payload = take cursor in
          { section_name; section_critical; section_payload })
    in
    let expected_index = take cursor in
    exact_end cursor "trailing-data";
    let names = List.map (fun section -> section.section_name) sections in
    if List.length names <> List.length (List.sort_uniq String.compare names) then
      fail "duplicate-section";
    let known =
      [
        "provider";
        "cmi";
        "cmti";
        "dependencies";
        "ordinary-cmi-views";
        "broadcast-witnesses";
        "base-logical-sorts";
      ]
    in
    List.iter
      (fun section ->
        if section.section_critical && not (List.mem section.section_name known) then
          fail "unknown-critical-section")
      sections;
    let required name =
      match List.find_opt (fun section -> String.equal section.section_name name) sections with
      | Some section -> section.section_payload
      | None -> fail ("missing-" ^ name ^ "-section")
    in
    let provider = { encoded = required "provider"; offset = 0 } in
    let provider_unit = take provider
    and provider_origin = take provider
    and compiler_abi = take provider in
    exact_end provider "provider-trailing-data";
    let cmi = { encoded = required "cmi"; offset = 0 } in
    let cmi_receipt = take cmi and cmi_self_crc = take cmi in
    let cmi_imports = decode_imports cmi "cmi-import" in
    let cmi_identity = take cmi in
    exact_end cmi "cmi-trailing-data";
    let cmti = { encoded = required "cmti"; offset = 0 } in
    let cmti_receipt = take cmti in
    let cmti_digest = take cmti in
    let cmti_interface_digest =
      if String.equal cmti_digest "" then None else Some cmti_digest
    in
    let cmti_imports = decode_imports cmti "cmti-import" in
    let cmti_identity = take cmti in
    exact_end cmti "cmti-trailing-data";
    let dependency_cursor = { encoded = required "dependencies"; offset = 0 } in
    let dependencies =
      List.init (count dependency_cursor "dependency-count") (fun _ ->
          let dependency_unit = take dependency_cursor in
          let compiler = take dependency_cursor in
          let dependency_interface_receipt = take dependency_cursor in
          let authority = take dependency_cursor in
          {
            dependency_unit;
            dependency_compiler_receipt =
              (if String.equal compiler "" then None else Some compiler);
            dependency_interface_receipt;
            dependency_authority_receipt =
              (if String.equal authority "" then None else Some authority);
          })
    in
    exact_end dependency_cursor "dependencies-trailing-data";
    let ordinary_cursor =
      { encoded = required "ordinary-cmi-views"; offset = 0 }
    in
    let ordinary_cmi_receipts =
      List.init (count ordinary_cursor "ordinary-cmi-view-count") (fun _ ->
          take ordinary_cursor)
    in
    exact_end ordinary_cursor "ordinary-cmi-views-trailing-data";
    let payloads =
      [
        decode_broadcasts (required "broadcast-witnesses");
        decode_logical_sorts (required "base-logical-sorts");
      ]
    in
    let value =
      {
        provider_unit;
        provider_origin;
        compiler_abi;
        cmi_receipt;
        cmi_self_crc;
        cmi_imports;
        cmi_identity;
        cmti_receipt;
        cmti_interface_digest;
        cmti_imports;
        cmti_identity;
        ordinary_cmi_receipts;
        dependencies;
        payloads;
      }
    in
    validate value;
    let canonical_body = body value in
    let canonical_index = Digest.string canonical_body |> Digest.to_hex in
    if not (String.equal expected_index canonical_index) then fail "index";
    if not (String.equal encoded (magic ^ canonical_body ^ frame canonical_index)) then
      fail "noncanonical";
    [%log.trace "decoded retained authority envelope"
      ~stage:(Delator.Field.string "authority-envelope")
      ~section_count:(Delator.Field.int section_count)
      ~decision:(Delator.Field.string "accepted")];
    Ok value
  with Decode reason ->
    [%log.debug "rejected retained authority envelope"
      ~stage:(Delator.Field.string "authority-envelope")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string reason)];
    Error reason
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let broadcast_witnesses value =
  value.payloads
  |> List.concat_map (function
       | Broadcast_witnesses members -> members
       | Logical_sorts _ -> [])

let logical_sorts value =
  value.payloads
  |> List.concat_map (function
       | Logical_sorts sorts -> sorts
       | Broadcast_witnesses _ -> [])
