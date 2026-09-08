type conflict = {
  slot_full_key : string;
  law_full_keys : string list;
  carrier_path : string;
  callable_paths : string list;
  target_width : int;
  semantic_width : int;
  meaning : Numeric_ghost_law_private.meaning;
}

type rejected_law = {
  provider_unit : string;
  callable_path : string;
  law_full_key : string;
}

type error =
  | Invalid_dependency_graph of Interface_specification_environment_private.error
  | Unreachable_law of rejected_law
  | Artifact_mismatch of rejected_law
  | Conflicting_laws of conflict list

type t = {
  consumer : Cmt_input.implementation;
  reached_artifacts : string list;
  dependency_artifact_keys : string list;
  artifacts : Cmt_input.implementation list;
  laws : Numeric_ghost_law_private.t list;
  descriptors : Numeric_admitted_descriptor_private.t list;
  full_key : string;
}

module Slots = Map.Make (String)
let ( let* ) = Result.bind

let artifact_key = Imported_callable.artifact_full_key

let slot_key (law : Numeric_ghost_law_private.t) =
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-ghost-law-slot.v1"
    [law.carrier.carrier_claim.owner.owner_cmi_full_key;
     law.carrier.carrier_claim.carrier_uid; Numeric_ghost_law_private.meaning_name law.meaning; law.target.full_key;
     string_of_int law.semantic_width.width;
     (match law.meaning with
      | Relation Numeric_relation_match_private.Operation -> law.role.callable_uid
      | Relation Bounds -> (match law.prerequisites with view :: _ -> view.role.callable_uid | [] -> "")
      | _ -> "")]

let prerequisite_closure laws =
  let rec collect known = function
    | [] -> Slots.bindings known |> List.map snd
    | (law : Numeric_ghost_law_private.t) :: rest ->
        if Slots.mem law.full_key known then collect known rest
        else collect (Slots.add law.full_key law known) (law.prerequisites @ rest) in
  collect Slots.empty laws

let import_laws ~consumer:(consumer [@delator.skip])
    ~dependencies:(dependencies [@delator.skip]) ?(descriptors = ([] [@delator.skip])) (input_laws [@delator.skip]) =
  let descriptors = List.sort_uniq (fun a b -> String.compare a.Numeric_admitted_descriptor_private.full_key b.full_key) descriptors in
  let laws = prerequisite_closure (input_laws @ List.map (fun descriptor -> descriptor.Numeric_admitted_descriptor_private.law) descriptors) in
  let result =
    let* ordered = Interface_specification_candidate_private.graph_order dependencies consumer
      |> Result.map_error (fun error -> Invalid_dependency_graph error) in
    let reached = List.fold_left (fun reached candidate ->
        if List.exists (fun owner -> Cmt_input.exact_imports owner candidate) reached then candidate :: reached
        else reached) [consumer] (List.rev ordered) in
    let* () = List.fold_left (fun result (law : Numeric_ghost_law_private.t) ->
        let* () = result in
        let* () = match List.filter (fun candidate -> candidate.Cmt_input.unit_name=law.issuer_unit) reached with
          | [] -> Error (Unreachable_law {provider_unit=law.issuer_unit;callable_path=law.role.callable_path;law_full_key=law.full_key})
          | [issuer] when artifact_key issuer=law.issuer_artifact_full_key -> Ok ()
          | _ -> Error (Artifact_mismatch {provider_unit=law.issuer_unit;callable_path=law.role.callable_path;law_full_key=law.full_key}) in
        let owner = law.carrier.carrier_claim.owner in
        let rejected = {provider_unit = owner.owner_unit; callable_path = law.role.callable_path; law_full_key = law.full_key} in
        let candidates = List.filter (fun candidate -> String.equal candidate.Cmt_input.unit_name owner.owner_unit) reached in
        match candidates with
        | [] -> Error (Unreachable_law rejected)
        | [candidate] ->
            let binding = Numeric_artifact_binding_private.correlate candidate law.carrier.carrier_claim in
            let facts = Result.bind binding Numeric_artifact_binding_private.facts in
            let matched = match facts with
              | Ok facts -> String.equal facts.binding_full_key law.carrier.binding_full_key
                  && String.equal facts.provider_artifact_full_key law.carrier.provider_artifact_full_key
              | Error _ -> false in
            [%log.trace "matched imported numeric law to its original artifact"
              ~provider:(Delator.Field.string candidate.unit_name)
              ~law_digest:(Delator.Field.string law.checked_digest)
              ~expected_artifact_digest:(Delator.Field.string law.carrier.provider_artifact_checked_digest)
              ~actual_artifact_digest:(Delator.Field.string (match facts with Ok facts -> facts.provider_artifact_checked_digest | Error _ -> "unavailable"))
              ~matched:(Delator.Field.bool matched)];
            if matched then Ok () else Error (Artifact_mismatch rejected)
        | _ -> Error (Artifact_mismatch rejected)) (Ok ()) laws in
    let slots = List.fold_left (fun slots (law : Numeric_ghost_law_private.t) ->
        Slots.update (slot_key law) (fun current -> Some (law :: Option.value ~default:[] current)) slots)
        Slots.empty laws in
    let conflicts = Slots.bindings slots |> List.filter_map (fun (slot_full_key, grouped) ->
        match List.rev grouped with
        | [] | [_] -> None
        | first :: _ as laws -> Some {slot_full_key;
            law_full_keys = List.map (fun (law : Numeric_ghost_law_private.t) -> law.full_key) laws;
            carrier_path = first.carrier.carrier_claim.carrier_path;
            callable_paths = List.map (fun (law : Numeric_ghost_law_private.t) -> law.role.callable_path) laws;
            target_width = first.target.target_claim.width;
            semantic_width = first.semantic_width.width; meaning = first.meaning}) in
    let descriptor_slots = List.fold_left (fun slots (descriptor : Numeric_admitted_descriptor_private.t) ->
      Slots.update descriptor.law.full_key (fun current -> Some (descriptor :: Option.value ~default:[] current)) slots) Slots.empty descriptors in
    let extension_slots = List.fold_left (fun slots (law : Numeric_ghost_law_private.t) ->
      if law.meaning<>Relation Numeric_relation_match_private.Operation then slots else
      let key=Numeric_receipt_private.encode ~schema:"numeric-extension-carrier-target"
        [law.carrier.carrier_claim.owner.owner_cmi_full_key;law.carrier.carrier_claim.carrier_uid;law.target.full_key] in
      Slots.update key (fun current -> Some (law :: Option.value ~default:[] current)) slots) Slots.empty laws in
    let extension_conflicts = Slots.bindings extension_slots |> List.filter_map (fun (slot_full_key,group) ->
      let fingerprint (law : Numeric_ghost_law_private.t) = Numeric_receipt_private.encode ~schema:"numeric-immutable-extension-base"
        [law.base_int.base_int_full_key;law.base_int.authenticated_base_int_artifact_binding_key;
         law.carrier.binding_full_key;law.target.full_key;
         Numeric_receipt_private.list (List.map (fun (prior : Numeric_ghost_law_private.t) -> prior.full_key) law.prerequisites)] in
      if List.length (List.map fingerprint group |> List.sort_uniq String.compare)<=1 then None else
      match group with [] -> None | first :: _ -> Some {slot_full_key;
        law_full_keys=List.map (fun (law : Numeric_ghost_law_private.t) -> law.full_key) group;
        carrier_path=first.carrier.carrier_claim.carrier_path;
        callable_paths=List.map (fun (law : Numeric_ghost_law_private.t) -> law.role.callable_path) group;
        target_width=first.target.target_claim.width;
        semantic_width=first.semantic_width.width;meaning=first.meaning}) in
    let conflicts = conflicts @ extension_conflicts @ (Slots.bindings descriptor_slots |> List.filter_map (fun (slot_full_key,group) ->
      match group with
      | [] | [_] -> None
      | first :: _ -> Some {slot_full_key;
          law_full_keys=List.map (fun descriptor -> descriptor.Numeric_admitted_descriptor_private.full_key) group;
          carrier_path=first.law.carrier.carrier_claim.carrier_path;
          callable_paths=List.map (fun descriptor -> descriptor.Numeric_admitted_descriptor_private.law.role.callable_path) group;
          target_width=first.law.target.target_claim.width;
          semantic_width=first.law.semantic_width.width;meaning=first.law.meaning})) in
    if conflicts <> [] then Error (Conflicting_laws conflicts)
    else
      let reached_keys = List.map artifact_key reached |> List.sort_uniq String.compare in
      let dependency_artifact_keys = List.map artifact_key dependencies |> List.sort_uniq String.compare in
      let full_key = Numeric_receipt_private.encode ~schema:"verocaml.numeric-consumer-ghost-registry.v2"
        [artifact_key consumer; Numeric_receipt_private.list reached_keys;
         Numeric_receipt_private.list dependency_artifact_keys;
         Numeric_receipt_private.list (List.map (fun (law : Numeric_ghost_law_private.t) -> law.full_key) laws);
         Numeric_receipt_private.list (List.map (fun (descriptor : Numeric_admitted_descriptor_private.t) -> descriptor.full_key) descriptors)] in
      Ok {consumer; reached_artifacts = reached_keys; dependency_artifact_keys; artifacts=reached; laws; descriptors; full_key}
  in
  let[@log_value.debug] law_view =
    let sorted = laws in
    let shown = sorted |> List.filteri (fun index _ -> index < 16)
      |> List.map (fun (law : Numeric_ghost_law_private.t) ->
          Delator.Field.map
            ["provider", Delator.Field.string law.carrier.carrier_claim.owner.owner_unit;
             "issuer", Delator.Field.string law.issuer_unit;
             "carrier_uid", Delator.Field.string law.carrier.carrier_claim.carrier_uid;
             "view_uid", Delator.Field.string law.role.callable_uid;
             "role", Delator.Field.string (Numeric_ghost_law_private.meaning_name law.meaning);
             "target_width", Delator.Field.int law.target.target_claim.width;
             "semantic_width", Delator.Field.int law.semantic_width.width;
             "explicit_axiom", Delator.Field.bool (law.authority = Numeric_ghost_law_private.Explicit_axiom);
             "trusted_dependencies", Delator.Field.int (List.length (List.filter (fun dependency -> dependency.Numeric_ghost_law_private.trusted) law.dependencies));
             "law_digest", Delator.Field.string law.checked_digest]) in
    Delator.Field.seq ~dropped:(Int.max 0 (List.length sorted - 16)) shown in
  [%log.debug "completed consumer-scoped numeric ghost registry"
    ~consumer:(Delator.Field.string consumer.Cmt_input.unit_name)
    ~laws:(law_view [@log_value.debug])
    ~decision:(Delator.Field.string (match result with
      | Ok _ -> "imported-original-laws"
      | Error (Invalid_dependency_graph _) -> "invalid-dependency-graph"
      | Error (Unreachable_law _) -> "unreachable-law"
      | Error (Artifact_mismatch _) -> "artifact-mismatch"
      | Error (Conflicting_laws _) -> "conflicting-laws"))
    ~conflict_count:(Delator.Field.int (match result with Error (Conflicting_laws conflicts) -> List.length conflicts | _ -> 0))
    ~native_authority:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]

let laws registry = registry.laws
let descriptors registry = registry.descriptors
let consumer registry = registry.consumer
let reached_artifacts registry = registry.reached_artifacts
let dependency_artifact_keys registry = registry.dependency_artifact_keys
let full_key registry = registry.full_key
let equal left right = String.equal left.full_key right.full_key

let native_bv_source_request registry
    ~(target : Build_target_profile_private.instance) =
  let modular =
    registry.descriptors
    |> List.filter (fun descriptor ->
           descriptor.Numeric_admitted_descriptor_private.law.meaning
           = Numeric_ghost_law_private.Relation
               Numeric_relation_match_private.Modular
           && String.equal descriptor.law.target.full_key target.full_key)
  in
  let projections =
    List.filter_map
      (fun modular ->
        match
          List.find_opt
            (fun prerequisite ->
              prerequisite.Numeric_ghost_law_private.meaning
              = Numeric_ghost_law_private.Unsigned_range
              && String.equal prerequisite.target.full_key target.full_key)
            modular.Numeric_admitted_descriptor_private.law.prerequisites
        with
        | None -> None
        | Some unsigned -> (
            match
              List.find_opt
                (fun descriptor ->
                  Numeric_ghost_law_private.equal descriptor.Numeric_admitted_descriptor_private.law unsigned)
                registry.descriptors
            with
            | None -> None
            | Some outer
              when Option.fold ~none:false
                     ~some:(fun relation -> relation.Numeric_relation_match_private.required_guards = [])
                     modular.law.relation
                   && outer.law.result_interpretation
                      = Numeric_law_match_private.Mathematical_result
                   && String.equal modular.law.carrier.binding_full_key
                        outer.law.carrier.binding_full_key
                   && String.equal modular.law.base_int.base_int_full_key
                        outer.law.base_int.base_int_full_key
                   && modular.law.semantic_width.width
                      = outer.law.semantic_width.width ->
                let authority = function
                  | Numeric_ghost_law_private.Checked_proof ->
                      Numeric_bv_imported_law_evidence_private.Checked_proof
                  | Explicit_axiom -> Explicit_axiom
                in
                let trusted_dependencies =
                  outer.law.dependencies @ modular.law.dependencies
                  |> List.filter_map (fun dependency ->
                         if dependency.Numeric_ghost_law_private.trusted then
                           Some
                             ( dependency.compiler_uid,
                               dependency.full_key )
                         else None)
                  |> List.sort_uniq compare
                in
                let callable_abi law =
                  Numeric_receipt_private.encode
                    ~schema:"verocaml.numeric-callable-abi-observation.v1"
                    [ law.Numeric_ghost_law_private.role.callable_type_abi;
                      law.role.callable_mode_abi ]
                in
                Some
                  (Numeric_bv_imported_law_evidence_private.For_registry.issue ~target
                     ~issuer_unit:outer.law.issuer_unit
                     ~outer_callable_uid:outer.law.role.callable_uid
                     ~modular_callable_uid:modular.law.role.callable_uid
                     ~outer_callable_abi:(callable_abi outer.law)
                     ~modular_callable_abi:(callable_abi modular.law)
                     ~outer_law_full_key:outer.law.full_key
                     ~modular_law_full_key:modular.law.full_key
                     ~carrier_binding_full_key:
                       outer.law.carrier.binding_full_key
                     ~base_int_full_key:outer.law.base_int.base_int_full_key
                     ~semantic_width:outer.law.semantic_width.width
                     ~outer_authority:(authority outer.law.authority)
                     ~modular_authority:(authority modular.law.authority)
                     ~trusted_dependencies)
            | Some _ -> None))
      modular
  in
  if projections = [] then Ok None
  else
    Numeric_bv_source_admission_private.For_registry.request
      ~consumer_artifact_full_key:(artifact_key registry.consumer)
      ~registry_full_key:registry.full_key ~target ~projections
    |> Result.map Option.some

let authorize_occurrence registry ~completion ~implementation ~imported ~validated ~descriptor ~caller ~occurrence =
  let* () = if artifact_key implementation = artifact_key registry.consumer
    && List.exists (fun candidate -> candidate.Numeric_admitted_descriptor_private.full_key = descriptor.Numeric_admitted_descriptor_private.full_key) registry.descriptors
    then Ok () else Error "The numeric descriptor is not admitted in this consumer's dependency scope." in
  let* carrier_origin = match List.filter (fun candidate -> candidate.Cmt_input.unit_name=descriptor.law.carrier.carrier_claim.owner.owner_unit) registry.artifacts with
    | [origin] -> Ok origin | _ -> Error "The numeric descriptor's original provider is unavailable." in
  let* origin = match List.filter (fun candidate -> candidate.Cmt_input.unit_name=descriptor.law.issuer_unit) registry.artifacts with
    | [origin] -> Ok origin | _ -> Error "The numeric law's original issuer is unavailable." in
  Numeric_admitted_descriptor_private.For_registry.authorize_occurrence ~completion ~implementation ~origin ~carrier_origin ~imported
    ~validated ~descriptor ~caller ~occurrence

let error_message = function
  | Invalid_dependency_graph error -> Interface_specification_environment_private.error_to_string error
  | Unreachable_law law ->
      Printf.sprintf "The numeric law for %s is not available through the consumer's declared dependencies. Add its original provider %s to the project dependencies."
        law.callable_path law.provider_unit
  | Artifact_mismatch law ->
      Printf.sprintf "The numeric law for %s belongs to a different build of %s. Rebuild and reverify that provider before importing its numeric specification."
        law.callable_path law.provider_unit
  | Conflicting_laws (conflict :: _) ->
      Printf.sprintf "Multiple numeric laws define %s of %s at semantic width %d on the %d-bit machine target (%s). Keep one numeric-role declaration for that carrier and target."
        (Numeric_ghost_law_private.meaning_name conflict.meaning)
        conflict.carrier_path conflict.semantic_width conflict.target_width
        (String.concat ", " conflict.callable_paths)
  | Conflicting_laws [] -> "Conflicting numeric laws cannot be imported together."
