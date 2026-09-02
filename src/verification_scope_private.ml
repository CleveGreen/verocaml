type role = Root | Dependency

type artifact = {
  role : role;
  cmt : string;
  cmi : string;
  implementation : Cmt_input.implementation;
}

type plan = {
  roots : artifact list;
  dependencies : artifact list;
  closures : (string * artifact list) list;
  skipped : artifact list;
}

type error = {
  unit_name : string option;
  message : string;
}

let error ?unit_name message = Error { unit_name; message }

let artifact ~role ~cmt ~cmi implementation =
  { role; cmt; cmi; implementation }

let role artifact = artifact.role
let cmt artifact = artifact.cmt
let cmi artifact = artifact.cmi
let implementation artifact = artifact.implementation
let unit_name artifact = artifact.implementation.Cmt_input.unit_name
let error_unit_name error = error.unit_name
let error_message error = error.message
let roots plan = plan.roots
let dependencies plan = plan.dependencies
let skipped plan = plan.skipped

let compare_artifact left right =
  match String.compare (unit_name left) (unit_name right) with
  | 0 -> String.compare left.cmt right.cmt
  | comparison -> comparison

let sorted artifacts = List.sort compare_artifact artifacts

let family_agrees implementation =
  let implementation_family =
    implementation.Cmt_input.implementation_family_markers
  and interface_family = implementation.interface_family_markers in
  implementation_family = interface_family
  || (implementation.embedded_interface && interface_family = [])

let classification artifact =
  let implementation = artifact.implementation in
  if not implementation.Cmt_input.implementation_metadata_valid then
    error ~unit_name:implementation.unit_name
      "malformed implementation-unit VeroCaml metadata"
  else if not (family_agrees implementation) then
    error ~unit_name:implementation.unit_name
      (Printf.sprintf
         "implementation CMT family [%s] and explicit CMI family [%s] differ"
         (String.concat "," implementation.implementation_family_markers)
         (String.concat "," implementation.interface_family_markers))
  else
    match implementation.verification_scope_markers with
    | [] -> Ok `Unmarked
    | [ "marked-v1" ] -> Ok `Marked
    | [ marker ] ->
        error ~unit_name:implementation.unit_name
          (Printf.sprintf "unsupported verification-scope marker %s" marker)
    | _ ->
        error ~unit_name:implementation.unit_name
          "duplicate implementation-unit verification-scope metadata"

let duplicate artifacts =
  let rec loop seen = function
    | [] -> None
    | artifact :: rest ->
        let name = unit_name artifact in
        (match List.assoc_opt name seen with
        | None -> loop ((name, artifact.role) :: seen) rest
        | Some role -> Some (name, role, artifact.role))
  in
  loop [] artifacts

let authenticate role artifact =
  match
    Interface_specification_candidate_private.strict_candidate
      ~require_public_interface:(role = Dependency)
      artifact.implementation
  with
  | Ok _ -> Ok ()
  | Error candidate_error ->
      error
        ?unit_name:candidate_error.Interface_specification_environment_private.unit_name
        candidate_error.message

let dependency_named dependencies name =
  List.find_opt (fun artifact -> String.equal (unit_name artifact) name)
    dependencies

let dependency_closure dependencies root =
  let rec visit seen artifact =
    Array.fold_left
      (fun seen (import : Cmt_input.import) ->
        match dependency_named dependencies import.unit_name with
        | None -> seen
        | Some dependency ->
            let name = unit_name dependency in
            if List.mem name seen then seen
            else visit (name :: seen) dependency)
      seen artifact.implementation.imports
  in
  let names = visit [] root in
  dependencies
  |> List.filter (fun artifact -> List.mem (unit_name artifact) names)
  |> sorted

let missing_explicit_prefix =
  "missing explicit implementation CMT for imported unit "

let missing_inventoried_unmarked skipped message =
  if String.starts_with ~prefix:missing_explicit_prefix message then
    let name =
      String.sub message (String.length missing_explicit_prefix)
        (String.length message - String.length missing_explicit_prefix)
    in
    List.exists (fun artifact -> String.equal (unit_name artifact) name) skipped
  else false

let preflight_graph ~skipped root dependencies =
  match
    Interface_specification_candidate_private.graph_order
      (List.map implementation dependencies)
      (implementation root)
  with
  | Ok _ -> Ok ()
  | Error graph_error
    when missing_inventoried_unmarked skipped graph_error.message ->
      Ok ()
  | Error graph_error ->
      error
        ?unit_name:graph_error.Interface_specification_environment_private.unit_name
        graph_error.message

let validate_inventory_crcs (artifacts [@delator.skip]) =
  let digests_for name =
    List.find_map
      (fun artifact ->
        if String.equal (unit_name artifact) name then
          Option.map
            (fun digest ->
              digest
              :: artifact.implementation.Cmt_input.interface_view_receipts)
            artifact.implementation.Cmt_input.interface_digest
        else None)
      artifacts
  in
  List.find_map
    (fun artifact ->
      Array.find_map
        (fun (import : Cmt_input.import) ->
          match digests_for import.unit_name with
          | Some (interface_digest :: interface_views) -> (
              match import.crc with
              | Some receipt when String.equal receipt interface_digest ->
                  [%log.trace "matched inventory import to explicit interface receipt"
                    ~stage:(Delator.Field.string "inventory-crc-validation")
                    ~receipt_class:(Delator.Field.string "explicit-interface")
                    ~decision:(Delator.Field.string "accepted")];
                  None
              | Some receipt when List.mem receipt interface_views ->
                  [%log.trace "matched inventory import to retained interface view receipt"
                    ~stage:(Delator.Field.string "inventory-crc-validation")
                    ~receipt_class:(Delator.Field.string "retained-interface-view")
                    ~available_view_count:
                      (Delator.Field.int (List.length interface_views))
                    ~decision:(Delator.Field.string "accepted")];
                  None
              | Some _ | None -> Some (artifact, import.unit_name))
          | Some [] ->
              Some (artifact, import.unit_name)
          | None -> None)
        artifact.implementation.imports)
    artifacts
  |> function
  | None ->
      [%log.debug "validated inventory import interface receipts"
        ~stage:(Delator.Field.string "inventory-crc-validation")
        ~artifact_count:(Delator.Field.int (List.length artifacts))
        ~import_count:
          (Delator.Field.int
             (List.fold_left
                (fun count artifact ->
                  count + Array.length artifact.implementation.Cmt_input.imports)
                0 artifacts))
        ~decision:(Delator.Field.string "accepted")];
      Ok ()
  | Some (artifact, imported) ->
      [%log.debug "rejected inventory import interface receipt"
        ~stage:(Delator.Field.string "inventory-crc-validation")
        ~artifact_role:
          (Delator.Field.string
             (match artifact.role with
             | Root -> "root"
             | Dependency -> "dependency"))
        ~artifact_count:(Delator.Field.int (List.length artifacts))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "receipt-mismatch")];
      error ~unit_name:(unit_name artifact)
        (Printf.sprintf
           "inventory import CRC does not match explicit CMI for unit %s"
           imported)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let classify_all artifacts =
  let rec loop marked skipped = function
    | [] -> Ok (List.rev marked, List.rev skipped)
    | artifact :: rest -> (
        match classification artifact with
        | Error _ as error -> error
        | Ok `Marked -> loop (artifact :: marked) skipped rest
        | Ok `Unmarked -> loop marked (artifact :: skipped) rest)
  in
  loop [] [] artifacts

let rec authenticate_all = function
  | [] -> Ok ()
  | artifact :: rest -> (
      match authenticate artifact.role artifact with
      | Error _ as error -> error
      | Ok () -> authenticate_all rest)

let rec authenticate_retained_skipped = function
  | [] -> Ok ()
  | artifact :: rest ->
      if Cmt_input.retained_preprocessing artifact.implementation then
        (match authenticate artifact.role artifact with
        | Error _ as error ->
            [%log.warn "rejected retained skipped verification target"
              ~stage:(Delator.Field.string "scope-skipped-authentication")
              ~artifact_role:
                (Delator.Field.string
                   (match artifact.role with Root -> "root" | Dependency -> "dependency"))
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "strict-candidate")];
            error
        | Ok () ->
            [%log.debug "authenticated retained skipped verification target"
              ~stage:(Delator.Field.string "scope-skipped-authentication")
              ~decision:(Delator.Field.string "accepted")];
            authenticate_retained_skipped rest)
      else (
        [%log.trace "preserved ordinary skipped verification target"
          ~stage:(Delator.Field.string "scope-skipped-authentication")
          ~decision:(Delator.Field.string "skipped")
          ~reason_class:(Delator.Field.string "ordinary-nonauthority")];
        authenticate_retained_skipped rest)

let build_plan marked skipped =
  let roots =
    List.filter (fun artifact -> artifact.role = Root) marked |> sorted
  and dependencies =
    List.filter (fun artifact -> artifact.role = Dependency) marked |> sorted
  in
  match authenticate_retained_skipped skipped with
  | Error _ as error -> error
  | Ok () ->
  match authenticate_all (roots @ dependencies) with
  | Error _ as error -> error
  | Ok () ->
      let closures =
        List.map
          (fun root ->
            (unit_name root, dependency_closure dependencies root))
          roots
      in
      let reachable =
        closures
        |> List.concat_map (fun (_, closure) -> List.map unit_name closure)
        |> List.sort_uniq String.compare
      in
      (match
         List.find_opt
           (fun dependency ->
             not (List.mem (unit_name dependency) reachable))
           dependencies
       with
      | Some dependency ->
          error ~unit_name:(unit_name dependency)
            "marked dependency is not reachable from a marked root"
      | None ->
          let rec preflight = function
            | [] ->
                Ok
                  {
                    roots;
                    dependencies;
                    closures;
                    skipped = sorted skipped;
                  }
            | (root_name, closure) :: rest ->
                let root =
                  List.find
                    (fun root -> String.equal (unit_name root) root_name)
                    roots
                in
                (match preflight_graph ~skipped root closure with
                | Error _ as error -> error
                | Ok () -> preflight rest)
          in
          preflight closures)

let plan (artifacts [@delator.skip]) =
  match duplicate artifacts with
  | Some (name, left, right) ->
      let detail =
        if left = right then "duplicate inventory unit"
        else "inventory unit has duplicate root/dependency roles"
      in
      [%log.warn "rejected verification scope inventory"
        ~stage:(Delator.Field.string "scope-plan")
        ~artifact_count:(Delator.Field.int (List.length artifacts))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:
          (Delator.Field.string
             (if left = right then "duplicate-unit" else "role-conflict"))];
      error ~unit_name:name detail
  | None -> (
      match validate_inventory_crcs artifacts with
      | Error _ as error ->
          [%log.warn "rejected verification scope inventory"
            ~stage:(Delator.Field.string "scope-plan")
            ~artifact_count:(Delator.Field.int (List.length artifacts))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "inventory-crc")];
          error
      | Ok () -> (
          match classify_all artifacts with
          | Error _ as error ->
              [%log.warn "rejected verification scope inventory"
                ~stage:(Delator.Field.string "scope-plan")
                ~artifact_count:(Delator.Field.int (List.length artifacts))
                ~decision:(Delator.Field.string "rejected")
                ~reason_class:(Delator.Field.string "artifact-classification")];
              error
          | Ok (marked, skipped) -> (
              match build_plan marked skipped with
              | Error _ as error ->
                  [%log.warn "rejected verification scope graph"
                    ~stage:(Delator.Field.string "scope-plan")
                    ~artifact_count:(Delator.Field.int (List.length artifacts))
                    ~marked_count:(Delator.Field.int (List.length marked))
                    ~skipped_count:(Delator.Field.int (List.length skipped))
                    ~decision:(Delator.Field.string "rejected")
                    ~reason_class:(Delator.Field.string "authentication-or-graph")];
                  error
              | Ok plan ->
                  [%log.info "completed verification scope plan"
                    ~stage:(Delator.Field.string "scope-plan")
                    ~artifact_count:(Delator.Field.int (List.length artifacts))
                    ~root_count:(Delator.Field.int (List.length plan.roots))
                    ~dependency_count:
                      (Delator.Field.int (List.length plan.dependencies))
                    ~skipped_count:(Delator.Field.int (List.length plan.skipped))
                    ~decision:(Delator.Field.string "accepted")];
                  Ok plan)))
[@@delator.instrument]
[@@delator.level info]
[@@delator.no_exn_log]

let dependencies_for_root plan root =
  Option.value ~default:[]
    (List.assoc_opt (unit_name root) plan.closures)
