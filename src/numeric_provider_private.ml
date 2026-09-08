type unavailable = { callable_path : string; role : string; width : int; reason : string }
type t = {
  implementation : Cmt_input.implementation;
  laws : Numeric_ghost_law_private.t list;
  refinements : Numeric_runtime_refinement_private.t list;
  descriptors : Numeric_admitted_descriptor_private.t list;
  unavailable : unavailable list;
  full_key : string;
}
let ( let* ) = Result.bind
let role declaration = declaration.Numeric_semantics_correlation_private.role.numeric_role_source.role_identity
let runtime_role declaration = role declaration = "runtime-view" || role declaration = "trusted-runtime-view"
let rank declaration = match role declaration with
  | "unsigned-view" -> 0 | "signed-view" -> 1 | "bounds" -> 2
  | "checked-conversion" | "partial-conversion" | "modular-conversion" -> 3
  | "operation" -> 4 | _ -> 5
let relation_kind = function
  | "bounds" -> Some Numeric_relation_match_private.Bounds
  | "checked-conversion" -> Some Checked | "partial-conversion" -> Some Partial
  | "modular-conversion" -> Some Modular | "operation" -> Some Operation
  | _ -> None

let complete_impl ~completion:(completion [@delator.skip]) ~implementation:(implementation [@delator.skip])
    ~imported:(imported [@delator.skip]) ~prerequisites:(prerequisites [@delator.skip])
    ~validated:(validated [@delator.skip]) ~dependencies:(dependencies [@delator.skip])
    ~declarations:(declarations [@delator.skip]) =
  let result =
    let* () = if Verification_driver_private.completion_matches completion ~implementation ~validated then Ok ()
      else Error "Numeric declarations need successful verification of their original provider." in
    let* targets = Build_target_profile_private.authenticate_instances (Build_target_profile_private.capability ()) in
    let providers = implementation :: dependencies |> List.sort_uniq (fun a b ->
      String.compare (Imported_callable.artifact_full_key a) (Imported_callable.artifact_full_key b)) in
    let declarations = List.sort (fun a b -> let order = Int.compare (rank a) (rank b) in
      if order <> 0 then order else String.compare a.Numeric_semantics_correlation_private.role.numeric_role_callable_uid b.role.numeric_role_callable_uid) declarations in
    let admit laws declaration target =
      let source = declaration.Numeric_semantics_correlation_private.role in
      let* carrier = match List.filter (fun carrier ->
        String.equal carrier.Cmt_input.numeric_carrier_uid source.numeric_role_carrier_uid
        && String.equal carrier.numeric_carrier_owner_cmi_full_key source.numeric_role_carrier_owner_cmi_full_key)
        implementation.Cmt_input.interface_numeric_claims.numeric_carriers with
        | [carrier] -> Ok carrier | _ -> Error "The original numeric carrier is not uniquely available." in
      let* reference = match carrier.numeric_carrier_base with
        | Some reference -> Ok reference
        | None -> Error "Add base = Your_integer_module.t to the numeric carrier declaration so its mathematical base is compiler-resolved." in
      let* base_provider, logical_sort, _ = Numeric_base_int_binding_private.resolve ~providers reference in
      let wrap result = Result.map_error Numeric_ghost_law_private.reason_message result in
      let candidates predicate = List.filter (fun (law : Numeric_ghost_law_private.t) ->
        predicate law.meaning && Build_target_profile_private.equal_instance law.target target
        && String.equal law.carrier.carrier_claim.carrier_uid source.numeric_role_carrier_uid
        && String.equal law.carrier.carrier_claim.owner.owner_cmi_full_key source.numeric_role_carrier_owner_cmi_full_key) laws in
      let unique results =
        let accepted = List.filter_map Result.to_option results |> List.sort_uniq Numeric_ghost_law_private.compare in
        match accepted with
        | [law] -> Ok law
        | [] -> (match List.find_opt Result.is_error results with Some (Error reason) -> Error reason | _ -> Error "No admitted view prerequisite establishes this numeric role.")
        | _ -> Error "This numeric law has multiple distinct view prerequisites; keep one compatible numeric interpretation." in
      match role declaration with
      | "unsigned-view" -> wrap (Numeric_ghost_law_private.admit_unsigned_range ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration)
      | "signed-view" ->
          candidates ((=) Numeric_ghost_law_private.Unsigned_range)
          |> List.map (fun unsigned -> wrap (Numeric_ghost_law_private.admit_signed_view ~completion ~implementation ~validated ~base_provider ~logical_sort ~target ~unsigned declaration))
          |> unique
      | name ->
          (match relation_kind name with
          | None -> Error "This numeric role is not supported; it remains an ordinary specification."
          | Some kind ->
              candidates (function Numeric_ghost_law_private.Unsigned_range | Signed_view -> true | _ -> false)
              |> List.map (fun view ->
                let attempt bounds = wrap (Numeric_ghost_law_private.admit_relation ~completion ~implementation ~validated ~base_provider ~logical_sort ~target ~kind ~view ~bounds declaration) in
                match attempt None with
                | Ok _ as accepted -> accepted
                | Error _ as unavailable ->
                    if kind <> Numeric_relation_match_private.Checked && kind <> Partial then unavailable else
                    let bounds = candidates ((=) (Numeric_ghost_law_private.Relation Bounds))
                      |> List.filter (fun law -> match law.Numeric_ghost_law_private.prerequisites with [prior] -> Numeric_ghost_law_private.equal prior view | _ -> false) in
                    if bounds = [] then unavailable else unique (List.map (fun bounds -> attempt (Some bounds)) bounds))
              |> unique) in
    let admit_any laws declaration target =
      let source=declaration.Numeric_semantics_correlation_private.role in
      if role declaration<>"operation" || source.numeric_role_carrier_owner_unit=implementation.unit_name then admit laws declaration target else
      let* imported=match imported with Some imported -> Ok imported | None -> Error "Operation extensions require an authenticated specification-library import." in
      let candidates=List.filter (fun (view : Numeric_ghost_law_private.t) ->
        (view.meaning=Unsigned_range || view.meaning=Signed_view)
        && view.carrier.carrier_claim.carrier_uid=source.numeric_role_carrier_uid
        && view.carrier.carrier_claim.owner.owner_cmi_full_key=source.numeric_role_carrier_owner_cmi_full_key
        && Build_target_profile_private.equal_instance view.target target) prerequisites in
      let results=List.map (fun view ->
        let* origin=match List.find_opt (fun provider -> Imported_callable.artifact_full_key provider=view.Numeric_ghost_law_private.issuer_artifact_full_key) providers with
          | Some origin -> Ok origin | None -> Error "The operation extension's original view provider is unavailable." in
        Numeric_ghost_law_private.admit_operation_extension ~completion ~implementation ~validated ~imported ~origin ~view declaration
        |> Result.map_error Numeric_ghost_law_private.reason_message) candidates in
      match List.filter_map Result.to_option results |> List.sort_uniq Numeric_ghost_law_private.compare with
      | [law] -> Ok law
      | [] -> Error (match List.find_opt Result.is_error results with Some (Error reason) -> reason | _ -> "No exact imported numeric view establishes this extension.")
      | _ -> Error "The operation extension has conflicting numeric view prerequisites." in
    let laws, unavailable = List.fold_left (fun (laws, unavailable) declaration ->
      List.fold_left (fun (laws, unavailable) target ->
        match admit_any laws declaration target with
        | Ok law -> law :: laws, unavailable
        | Error reason ->
            [%log.debug "numeric source declaration remained unadmitted"
              ~callable:(Delator.Field.string declaration.Numeric_semantics_correlation_private.role.numeric_role_callable_path)
              ~role:(Delator.Field.string (role declaration))
              ~target_width:(Delator.Field.int target.Build_target_profile_private.target_claim.width)
              ~reason:(Delator.Field.string reason)
              ~ordinary_specification_preserved:(Delator.Field.bool true)];
            laws, {callable_path = declaration.role.numeric_role_callable_path; role = role declaration;
              width = target.target_claim.width; reason} :: unavailable) (laws, unavailable) targets)
      ([], []) (List.filter (fun declaration -> not (runtime_role declaration)) declarations) in
    let laws = List.sort_uniq Numeric_ghost_law_private.compare laws in
    let refinements = List.concat_map (fun declaration ->
      if not (runtime_role declaration) then [] else
      List.filter_map (fun law -> match Numeric_runtime_refinement_private.admit
        ~completion ~implementation ~validated ~law declaration with
        | Ok refinement -> Some refinement | Error _ -> None) laws) declarations
      |> List.sort_uniq (fun a b -> String.compare a.Numeric_runtime_refinement_private.full_key b.full_key) in
    let* descriptors = List.fold_left (fun result law ->
      let* descriptors = result in
      let* declaration = match List.find_opt (fun declaration ->
        declaration.Numeric_semantics_correlation_private.role.numeric_role_callable_uid = law.Numeric_ghost_law_private.role.callable_uid
        && declaration.role.numeric_role_semantics_uid = law.role.semantics_uid) declarations with
        | Some declaration -> Ok declaration | None -> Error "The admitted numeric law lost its original declaration." in
      let* carrier_origin=match List.find_opt (fun provider -> provider.Cmt_input.unit_name=law.carrier.carrier_claim.owner.owner_unit) providers with
        | Some provider -> Ok provider | None -> Error "The numeric carrier's original provider is unavailable." in
      let* descriptor = Numeric_admitted_descriptor_private.complete ~completion ~implementation ~carrier_origin ~validated
        ~law ~refinements declaration in
      Ok (descriptor :: descriptors)) (Ok []) laws in
    let descriptors = List.sort (fun a b -> String.compare a.Numeric_admitted_descriptor_private.full_key b.full_key) descriptors in
    let unavailable = List.sort compare unavailable in
    let full_key = Numeric_receipt_private.encode ~schema:"verocaml.completed-numeric-provider.v1"
      [Imported_callable.artifact_full_key implementation;
       Numeric_receipt_private.list (List.map (fun (law : Numeric_ghost_law_private.t) -> law.full_key) laws);
       Numeric_receipt_private.list (List.map (fun (refinement : Numeric_runtime_refinement_private.t) -> refinement.full_key) refinements);
       Numeric_receipt_private.list (List.map (fun (descriptor : Numeric_admitted_descriptor_private.t) -> descriptor.full_key) descriptors)] in
    Ok {implementation; laws; refinements; descriptors; unavailable; full_key} in
  let[@log_value.debug] descriptor_view =
    let descriptors = match result with Ok provider -> provider.descriptors | Error _ -> [] in
    descriptors |> List.filteri (fun index _ -> index < 16) |> List.map (fun (descriptor : Numeric_admitted_descriptor_private.t) ->
        Delator.Field.map ["callable",Delator.Field.string descriptor.law.role.callable_path;
          "origin",Delator.Field.string descriptor.law.carrier.carrier_claim.owner.owner_unit;
          "semantic_authority",Delator.Field.string (match descriptor.law.authority with Checked_proof -> "checked-proof" | Explicit_axiom -> "explicit-axiom");
          "runtime_refinements",Delator.Field.int (List.length descriptor.refinements);
          "runtime_trust",Delator.Field.bool (List.exists (fun (refinement : Numeric_runtime_refinement_private.t) -> refinement.authority=Explicit_external_body) descriptor.refinements);
          "visible",Delator.Field.bool (descriptor.visibility=Visible);
          "reveal",Delator.Field.bool descriptor.reveal;"inline",Delator.Field.bool descriptor.inline;
          "candidacy",Delator.Field.string (match descriptor.lowering_candidate with Bounded_mathematical_view -> "bounded-mathematical-view-only" | Unavailable_layout -> "unavailable-native-layout");
          "target_width",Delator.Field.int descriptor.law.target.target_claim.width;
          "semantic_width",Delator.Field.int descriptor.law.semantic_width.width]) in
  (* FIXME(delator): ~descriptors:(descriptor_view [@log_value.debug]) with a Field.value log binding. *)
  [%log.debug "completed numeric provider admission"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~admitted_laws:(Delator.Field.int (match result with Ok provider -> List.length provider.laws | Error _ -> 0))
    ~unadmitted_declarations:(Delator.Field.int (match result with Ok provider -> List.length provider.unavailable | Error _ -> 0))
    ~descriptors:(Delator.Field.seq ~dropped:(match result with Ok provider -> Int.max 0 (List.length provider.descriptors - 16) | Error _ -> 0) (descriptor_view [@log_value.debug]))
    ~runtime_refinement_count:(Delator.Field.int (match result with Ok provider -> List.length provider.refinements | Error _ -> 0))];
  result
[@@delator.instrument] [@@delator.level debug]

let complete ~completion ~implementation ~validated ~dependencies ~declarations =
  complete_impl ~completion ~implementation ~validated ~dependencies ~declarations ~imported:None ~prerequisites:[]

let complete_with_imports ~completion ~implementation ~validated ~imported ~prerequisites ~declarations =
  complete_impl ~completion ~implementation ~validated ~dependencies:(Imported_callable.artifacts imported)
    ~declarations ~imported:(Some imported) ~prerequisites
