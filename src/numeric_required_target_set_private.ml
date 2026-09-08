type mode = Concrete | Abstract
type selector = { mode : mode; profile_full_key : string; full_key : string }
type child = {
  ordinal : int;
  target : Build_target_profile_private.instance;
  original_full_key : string;
  parent_full_key : string;
  full_key : string;
  checked_digest : string;
}
type t = {
  mode : mode;
  original : Numeric_original_obligation_private.t;
  target_full_keys : string list;
  children : child list;
  abstract_edge : string option;
  full_key : string;
  checked_digest : string;
}

let ( let* ) = Result.bind
let mode_name = function Concrete -> "concrete" | Abstract -> "abstract"
let parent_schema = "verocaml.required-target-set-parent.v1"
let child_schema = "verocaml.required-target-set-child.v1"
let transport_schema = "verocaml.required-target-set-transport.v1"

let select ~capability:(capability [@delator.skip]) ~modes:(modes [@delator.skip]) =
  let result =
    let* mode = match modes with
      | [mode] -> Ok mode
      | [] -> Error "Select a numeric coverage mode explicitly."
      | _ -> Error "Numeric coverage requires exactly one mode; concrete and abstract cannot be combined." in
    let* profile = Build_target_profile_private.authenticate_profile capability in
    let full_key = Numeric_receipt_private.encode ~schema:"verocaml.numeric-coverage-selector.v1"
      [profile.build_declaration_full_key; profile.full_key; mode_name mode] in
    Ok {mode; profile_full_key = profile.full_key; full_key} in
  [%log.debug "selected authenticated numeric coverage mode"
    ~choice_count:(Delator.Field.int (List.length modes))
    ~decision:(Delator.Field.string (match result with Ok selector -> mode_name selector.mode | Error _ -> "rejected"))];
  result
[@@delator.instrument] [@@delator.level debug]

let create ~capability:(capability [@delator.skip]) ~selector:((selector : selector) [@delator.skip])
    ~registry:(registry [@delator.skip]) ~report:(report [@delator.skip])
    ~original:(original [@delator.skip]) ~coverage:(coverage [@delator.skip]) =
  let result =
    let report_matches = List.exists ((==) original) (Verification_driver_private.original_obligations report) in
    let consumer_matches = Numeric_original_obligation_private.matches_consumer original (Numeric_ghost_registry_private.consumer registry) in
    let missing = List.filter (fun key -> not (List.mem key (Numeric_ghost_registry_private.dependency_artifact_keys registry))) original.dependency_artifact_full_keys in
    let[@log_value.debug] missing_providers = missing |> List.filteri (fun index _ -> index<16)
      |> List.map (fun key -> match Numeric_receipt_private.decode ~schema:"verocaml.numeric-import-artifact.v1" ~field_count:3 key with
        | Ok (name :: _) -> Delator.Field.string name | _ -> Delator.Field.string "unresolved-artifact") in
    [%log.debug "correlated original numeric coverage provenance"
      ~report_matches:(Delator.Field.bool report_matches)
      ~consumer_matches:(Delator.Field.bool consumer_matches)
      ~missing_dependencies:(Delator.Field.seq ~dropped:(Int.max 0 (List.length missing-16)) (missing_providers [@log_value.debug]))];
    let* () = if not report_matches then Error "Numeric target coverage belongs to a different original obligation."
      else if not consumer_matches then Error "Numeric target coverage belongs to a different consumer build."
      else if missing<>[] then Error "Numeric target coverage is missing an original specification dependency build."
      else Ok () in
    let* profile = Build_target_profile_private.authenticate_profile capability in
    let* expected_selector = select ~capability ~modes:[selector.mode] in
    let* () = if String.equal selector.full_key expected_selector.full_key
      && String.equal selector.profile_full_key profile.full_key then Ok ()
      else Error "The numeric coverage selector belongs to a different build-target profile." in
    let* targets = Build_target_profile_private.authenticate_instances capability in
    let targets = Numeric_receipt_private.canonical_members
      ~full_key:(fun (target : Build_target_profile_private.instance) -> target.full_key) targets in
    let target_full_keys = List.map (fun (target : Build_target_profile_private.instance) -> target.full_key) targets in
    let* abstract_key = match selector.mode, coverage with
      | Concrete, None -> Ok None
      | Concrete, Some _ -> Error "Concrete target coverage cannot carry an abstract proof."
      | Abstract, None -> Error "Abstract target coverage requires an admitted all-target proof; no concrete fallback is performed."
      | Abstract, Some coverage ->
          if String.equal coverage.Numeric_abstract_coverage_private.original_full_key original.full_key
            && String.equal coverage.profile_full_key profile.full_key
            && coverage.target_full_keys = target_full_keys then Ok (Some coverage.full_key)
          else Error "The abstract proof does not cover this original obligation and complete target profile." in
    let full_key = Numeric_receipt_private.encode ~schema:parent_schema
      [Numeric_ghost_registry_private.full_key registry; profile.full_key;
       original.full_key; selector.full_key; mode_name selector.mode;
       Numeric_receipt_private.list target_full_keys;
       Numeric_receipt_private.list (Option.to_list abstract_key)] in
    let checked_digest = Numeric_receipt_private.digest ~domain:parent_schema full_key in
    let children, abstract_edge = match selector.mode, abstract_key with
      | Concrete, None ->
          List.mapi (fun ordinal target ->
            let child_key = Numeric_receipt_private.encode ~schema:child_schema
              [full_key; original.full_key; target.Build_target_profile_private.full_key; string_of_int ordinal] in
            let checked_digest = Numeric_receipt_private.digest ~domain:child_schema child_key in
            {ordinal; target; original_full_key = original.full_key; parent_full_key = full_key;
             full_key = child_key; checked_digest}) targets, None
      | Abstract, Some key -> [], Some (Numeric_receipt_private.encode
          ~schema:"verocaml.required-target-set-abstract-edge.v1" [full_key; original.full_key; key])
      | _ -> assert false in
    Ok {mode = selector.mode; original; target_full_keys; children; abstract_edge; full_key; checked_digest} in
  let[@log_value.debug] targets = match result with
    | Ok set -> set.children |> List.filteri (fun index _ -> index < 16) |> List.map (fun (child : child) -> Delator.Field.map
        ["ordinal", Delator.Field.int child.ordinal; "width", Delator.Field.int child.target.target_claim.width;
         "target_digest", Delator.Field.string child.target.checked_digest])
    | Error _ -> [] in
  (* FIXME(delator): ~targets:(targets [@log_value.debug]) with a Field.value log binding. *)
  [%log.debug "constructed required numeric target coverage"
    ~function_name:(Delator.Field.string original.obligation.function_ref.function_name)
    ~obligation_index:(Delator.Field.int original.obligation.obligation_index)
    ~mode:(Delator.Field.string (mode_name selector.mode))
    ~targets:(Delator.Field.seq ~dropped:(match result with Ok set -> Int.max 0 (List.length set.children - 16) | Error _ -> 0) (targets [@log_value.debug]))
    ~decision:(Delator.Field.string (match result with Ok _ -> "conjunctive-coverage-required" | Error reason -> reason))
    ~scheduled_queries:(Delator.Field.int 0)];
  result
[@@delator.instrument] [@@delator.level debug]

let encode (set : t) = Numeric_receipt_private.encode ~schema:transport_schema
  [set.full_key; set.checked_digest;
   Numeric_receipt_private.list (List.map (fun (child : child) ->
     Numeric_receipt_private.encode ~schema:"target-child-envelope" [child.full_key; child.checked_digest]) set.children);
   Numeric_receipt_private.list (Option.to_list set.abstract_edge)]

let decode ~capability:(capability [@delator.skip]) ~selector:(selector [@delator.skip])
    ~registry:(registry [@delator.skip]) ~report:(report [@delator.skip]) ~original:(original [@delator.skip])
    ~coverage:(coverage [@delator.skip]) (encoded [@delator.skip]) =
  let result =
    let* _ = Numeric_receipt_private.decode ~schema:transport_schema ~field_count:4 encoded in
    let* expected = create ~capability ~selector ~registry ~report ~original ~coverage in
    if String.equal encoded (encode expected) then Ok expected
    else Error "Numeric target coverage has a substituted parent, target, proof, digest, or noncanonical child sequence. Recreate the complete set from its original verification request." in
  [%log.debug "validated required numeric target handoff"
    ~decision:(Delator.Field.string (if Result.is_ok result then "accepted-exact-full-record" else "rejected"))
    ~transport_bytes:(Delator.Field.int (String.length encoded))];
  result
[@@delator.instrument] [@@delator.level debug]
