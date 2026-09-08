type authority = Checked_proof | Explicit_axiom
type meaning = Unsigned_range | Signed_view | Relation of Numeric_relation_match_private.kind

let meaning_name = function Unsigned_range -> "unsigned-view" | Signed_view -> "signed-view"
  | Relation kind -> Numeric_relation_match_private.kind_name kind

type dependency = {
  compiler_uid : string;
  trusted : bool;
  full_key : string;
}

type t = {
  issuer_unit : string;
  issuer_artifact_full_key : string;
  source_evidence : Numeric_proof_dependencies_private.t;
  base_int : Numeric_base_int_binding_private.t;
  carrier : Numeric_artifact_binding_private.facts;
  role : Numeric_interface_claim_private.role;
  target : Build_target_profile_private.instance;
  semantic_width : Numeric_semantic_width_private.t;
  result_interpretation : Numeric_law_match_private.result_interpretation;
  authority : authority;
  meaning : meaning;
  prerequisites : t list;
  relation : Numeric_relation_match_private.t option;
  dependencies : dependency list;
  full_key : string;
  checked_digest : string;
}

type reason =
  | Completion_mismatch
  | Unsupported_role
  | Nonlocal_role
  | Carrier_domain_unavailable
  | Base_unavailable of string
  | Base_not_imported
  | Carrier_unavailable of string
  | Target_unavailable of string
  | Statement_mismatch
  | Prerequisite_mismatch
  | Dependencies_unavailable of Numeric_proof_dependencies_private.reason

let reason_message = function
  | Completion_mismatch -> "The numeric law does not belong to this completed provider verification."
  | Unsupported_role -> "This numeric semantic role is not supported."
  | Nonlocal_role -> "Admit this numeric law at its original provider, not at a reexport."
  | Carrier_domain_unavailable -> "The numeric callable needs a supported, fully instantiated compiler carrier signature."
  | Base_unavailable reason | Carrier_unavailable reason | Target_unavailable reason -> reason
  | Base_not_imported -> "The numeric provider must import its mathematical base through an exact compiler dependency."
  | Statement_mismatch -> "The proof does not state the required numeric relation with its exact arguments and domain."
  | Prerequisite_mismatch -> "The numeric relation's prerequisite belongs to a different view, base, artifact, or target."
  | Dependencies_unavailable _ -> "This law uses a proof dependency outside the supported numeric evidence context; ordinary verification is unchanged."

type prepared = {
  issuer_unit : string;
  issuer_artifact_full_key : string;
  base_int : Numeric_base_int_binding_private.t;
  carrier : Numeric_artifact_binding_private.facts;
  role : Numeric_interface_claim_private.role;
  target : Build_target_profile_private.instance;
  authority : authority;
}

let ( let* ) = Result.bind
let compare (left : t) (right : t) = String.compare left.full_key right.full_key
let equal left right = compare left right = 0

let prepare ~meaning ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration =
  let source_role = declaration.Numeric_semantics_correlation_private.role in
  if not (Verification_driver_private.completion_matches completion ~implementation ~validated)
    || not (List.exists (fun role -> role == source_role) implementation.Cmt_input.interface_numeric_claims.numeric_roles)
  then Error Completion_mismatch
  else if not (String.equal source_role.numeric_role_source.role_schema "numeric-role.v1"
    && String.equal source_role.numeric_role_source.role_identity (meaning_name meaning)) then Error Unsupported_role
  else if not (List.for_all (String.equal implementation.unit_name)
    [source_role.numeric_role_callable_owner_unit; source_role.numeric_role_semantics_owner_unit;
     source_role.numeric_role_carrier_owner_unit]) then Error Nonlocal_role
  else if (match meaning with
    | Unsigned_range | Signed_view -> source_role.numeric_role_callable_domain <> Numeric_callable_domain_private.Exact_carrier
    | Relation _ -> Option.is_none source_role.numeric_role_callable_shape) then
    Error Carrier_domain_unavailable
  else
    let* base_int = Numeric_base_int_binding_private.correlate ~provider:base_provider ~logical_sort
      |> Result.map_error (fun reason -> Base_unavailable reason) in
    let* () =
      if base_provider == implementation || Cmt_input.exact_imports implementation base_provider then Ok ()
      else Error Base_not_imported in
    let* target = Build_target_profile_private.decode_instance (Build_target_profile_private.capability ()) target.Build_target_profile_private.full_key
      |> Result.map_error (fun reason -> Target_unavailable reason) in
    let retained = Cmt_input.retained_numeric_claims implementation.interface_numeric_claims in
    let* carriers = List.fold_left (fun result encoded ->
        let* decoded = result in
        let* carrier = Numeric_interface_claim_private.decode_carrier encoded
          |> Result.map_error (fun reason -> Carrier_unavailable reason) in
        Ok (carrier :: decoded)) (Ok []) retained.carrier_reconstruction_claims in
    let* carrier_claim =
      match List.filter (fun (carrier : Numeric_interface_claim_private.carrier) ->
        String.equal carrier.carrier_uid source_role.numeric_role_carrier_uid
        && String.equal carrier.owner.owner_cmi_full_key source_role.numeric_role_carrier_owner_cmi_full_key) carriers with
      | [carrier] -> Ok carrier
      | _ -> Error (Carrier_unavailable "No unique original carrier declaration matches this law.") in
    let* () = match carrier_claim.base_reference with
      | None -> Ok ()
      | Some reference ->
          let* _, _, selected = Numeric_base_int_binding_private.resolve ~providers:[base_provider] reference
            |> Result.map_error (fun reason -> Base_unavailable reason) in
          if String.equal selected.authenticated_base_int_artifact_binding_key base_int.authenticated_base_int_artifact_binding_key then Ok ()
          else Error (Base_unavailable "The supplied mathematical base differs from the compiler-resolved carrier declaration.") in
    let* artifact = Numeric_artifact_binding_private.correlate implementation carrier_claim
      |> Result.map_error (fun reason -> Carrier_unavailable reason) in
    let* carrier = Numeric_artifact_binding_private.facts artifact
      |> Result.map_error (fun reason -> Carrier_unavailable reason) in
    let* role =
      let inventory = Cmt_input.retained_numeric_claims
        {implementation.interface_numeric_claims with numeric_roles = [source_role]} in
      match inventory.role_reconstruction_claims with
      | [encoded] -> Numeric_interface_claim_private.decode_role encoded
          |> Result.map_error (fun reason -> Carrier_unavailable reason)
      | _ -> Error (Carrier_unavailable "No unique callable declaration matches this law.") in
    let* authority = match declaration.kind with
      | Numeric_semantics_binding_private.Explicit_axiom_declaration -> Ok Explicit_axiom
      | Completed_proof_declaration -> Ok Checked_proof
      | Ordinary_specification_declaration -> Error Statement_mismatch in
    Ok {issuer_unit=implementation.unit_name;issuer_artifact_full_key=Imported_callable.artifact_full_key implementation;
      base_int; carrier; role; target; authority}

let finish (prepared : prepared) ~meaning ~(prerequisites : t list)
    ~semantic_width ~result_interpretation ?relation evidence =
  let dependencies = List.map (fun dependency ->
      let compiler_uid = dependency.Numeric_proof_dependencies_private.compiler_uid in
      let trusted = dependency.trusted in
      let full_key = Numeric_receipt_private.encode ~schema:"verocaml.numeric-local-law-dependency.v1"
        [dependency.owner_artifact_full_key; compiler_uid; Numeric_receipt_private.bool trusted] in
      {compiler_uid; trusted; full_key}) evidence.Numeric_proof_dependencies_private.dependencies in
  let dependencies = dependencies @ List.concat_map (fun (law : t) -> law.dependencies) prerequisites
    |> List.sort_uniq (fun (left : dependency) (right : dependency) -> String.compare left.full_key right.full_key) in
  let schema, extra = match meaning with
    | Unsigned_range -> "verocaml.numeric-ghost-unsigned-law.v3", []
    | Signed_view -> "verocaml.numeric-ghost-signed-law.v3",
        [Numeric_receipt_private.list (List.map (fun (law : t) -> law.full_key) prerequisites)]
    | Relation kind -> "verocaml.numeric-ghost-relation-law.v3",
        [Numeric_relation_match_private.kind_name kind;
         Numeric_receipt_private.list (List.map (fun (law : t) -> law.full_key) prerequisites);
         Option.fold ~none:"" ~some:(fun relation -> relation.Numeric_relation_match_private.material) relation] in
  let full_key = Numeric_receipt_private.encode ~schema
    ([prepared.issuer_unit;prepared.issuer_artifact_full_key;
      prepared.base_int.base_int_full_key; prepared.base_int.validated_base_int_digest;
      prepared.base_int.authenticated_base_int_artifact_binding_key; prepared.carrier.binding_full_key;
      prepared.role.transport_key; prepared.target.full_key;
      string_of_int semantic_width.Numeric_semantic_width_private.width;
      semantic_width.full_key;
      (match result_interpretation with Numeric_law_match_private.Mathematical_result -> "mathematical-result" | Lifted_runtime_result -> "lifted-runtime-result"
        | Boolean_result -> "boolean-result" | Carrier_result -> "carrier-result");
      (match prepared.authority with Checked_proof -> "checked-proof" | Explicit_axiom -> "explicit-axiom");
      Numeric_receipt_private.list (List.map (fun (dependency : dependency) -> dependency.full_key) dependencies)] @ extra) in
  let checked_digest = Numeric_receipt_private.digest ~domain:schema full_key in
  {issuer_unit=prepared.issuer_unit;issuer_artifact_full_key=prepared.issuer_artifact_full_key;source_evidence=evidence;
   base_int = prepared.base_int; carrier = prepared.carrier; role = prepared.role;
   target = prepared.target; semantic_width;
   authority = prepared.authority; result_interpretation; meaning; prerequisites; relation;
   dependencies; full_key; checked_digest}

type request = Unsigned_request | Signed_request of t
  | Relation_request of Numeric_relation_match_private.kind * t * t option

let rec admit ~request:(request [@delator.skip]) ~completion:(completion [@delator.skip])
    ~implementation:(implementation [@delator.skip]) ~validated:(validated [@delator.skip])
    ~base_provider:(base_provider [@delator.skip]) ~logical_sort:(logical_sort [@delator.skip])
    ~target:(target [@delator.skip]) (declaration [@delator.skip]) =
  (* FIXME(delator): private top-level let log_admission ~implementation:(implementation [@log_value.debug]) ~declaration:(declaration [@log_value.debug]) result creates mismatched ML/MLI ABI witnesses. *)
  let log_admission ~implementation:(implementation [@log_value.debug])
      ~declaration:(declaration [@log_value.debug]) (result : (t, reason) result) =
    let[@log_value.debug] source_role = (declaration [@log_value.debug]).Numeric_semantics_correlation_private.role in
    let[@log_value.debug] reason_class = match result with
      | Ok _ -> "admitted-ghost-law"
      | Error Completion_mismatch -> "completion-mismatch"
      | Error Unsupported_role -> "unsupported-role"
      | Error Nonlocal_role -> "nonlocal-role"
      | Error Carrier_domain_unavailable -> "carrier-domain-unavailable"
      | Error (Base_unavailable _) -> "base-unavailable"
      | Error Base_not_imported -> "base-not-imported"
      | Error (Carrier_unavailable _) -> "carrier-unavailable"
      | Error (Target_unavailable _) -> "target-unavailable"
      | Error Statement_mismatch -> "statement-mismatch"
      | Error Prerequisite_mismatch -> "prerequisite-mismatch"
      | Error (Dependencies_unavailable _) -> "dependencies-unavailable" in
    [%log.debug "completed local numeric ghost law admission"
      ~provider:(Delator.Field.string (implementation [@log_value.debug]).Cmt_input.unit_name)
      ~callable_uid:(Delator.Field.string (source_role [@log_value.debug]).numeric_role_callable_uid)
      ~carrier_uid:(Delator.Field.string (source_role [@log_value.debug]).numeric_role_carrier_uid)
      ~role:(Delator.Field.string (source_role [@log_value.debug]).numeric_role_source.role_identity)
      ~decision:(Delator.Field.string (reason_class [@log_value.debug]))
      ~prerequisites:(Delator.Field.seq (match result with Ok law -> List.map (fun (law : t) -> Delator.Field.string law.checked_digest) law.prerequisites | Error _ -> []))
      ~trusted_dependencies:(Delator.Field.int (match result with
        | Ok law -> List.length (List.filter (fun dependency -> dependency.trusted) law.dependencies)
        | Error _ -> 0))
      ~runtime_refinement:(Delator.Field.bool false)
      ~native_candidacy:(Delator.Field.bool false)];
    result in
  let result =
    let meaning = match request with Unsigned_request -> Unsigned_range | Signed_request _ -> Signed_view
      | Relation_request (kind, _, _) -> Relation kind in
    let* prepared = prepare ~meaning ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration in
    match request with
    | Unsigned_request ->
        let* statement = match Numeric_law_match_private.unsigned_range ~target:prepared.target declaration with
          | Some statement -> Ok statement | None -> Error Statement_mismatch in
        let* evidence = Numeric_proof_dependencies_private.complete_unsigned_range ~completion ~implementation ~validated statement
          |> Result.map_error (fun reason -> Dependencies_unavailable reason) in
        let* semantic_width =
          Numeric_semantic_width_private.issue ~target:prepared.target
            ~width:statement.semantic_width
            ~evidence_full_key:statement.semantic_width_evidence
          |> Result.map_error (fun reason -> Target_unavailable reason)
        in
        Ok (finish prepared ~meaning:Unsigned_range ~prerequisites:[]
          ~semantic_width
          ~result_interpretation:statement.result_interpretation evidence)
    | Signed_request unsigned ->
        let* () = if unsigned.meaning = Unsigned_range && unsigned.prerequisites = [] then Ok () else Error Prerequisite_mismatch in
        let* declarations = Numeric_semantics_binding_private.complete_local ~completion ~implementation ~validated
          |> Result.map_error (fun _ -> Prerequisite_mismatch) in
        let* unsigned_declaration = match List.filter (fun candidate ->
            String.equal candidate.Numeric_semantics_correlation_private.role.numeric_role_callable_uid unsigned.role.callable_uid
            && String.equal candidate.role.numeric_role_semantics_uid unsigned.role.semantics_uid) declarations with
          | [declaration] -> Ok declaration | _ -> Error Prerequisite_mismatch in
        let* current_unsigned = admit ~request:Unsigned_request ~completion ~implementation ~validated ~base_provider ~logical_sort ~target unsigned_declaration
          |> Result.map_error (fun _ -> Prerequisite_mismatch) in
        let* () =
          if equal unsigned current_unsigned
            && String.equal unsigned.carrier.binding_full_key prepared.carrier.binding_full_key
            && String.equal unsigned.base_int.authenticated_base_int_artifact_binding_key prepared.base_int.authenticated_base_int_artifact_binding_key
            && Build_target_profile_private.equal_instance unsigned.target prepared.target
            && Numeric_semantic_width_private.equal unsigned.semantic_width
                 current_unsigned.semantic_width
          then Ok () else Error Prerequisite_mismatch in
        let* unsigned_statement = match Numeric_law_match_private.unsigned_range ~target:prepared.target unsigned_declaration with
          | Some statement -> Ok statement | None -> Error Prerequisite_mismatch in
        let* statement = match Numeric_law_match_private.signed_view ~unsigned:unsigned_statement declaration with
          | Some statement -> Ok statement | None -> Error Statement_mismatch in
        let* evidence = Numeric_proof_dependencies_private.complete_signed_view ~completion ~implementation ~validated statement
          |> Result.map_error (fun reason -> Dependencies_unavailable reason) in
        let* semantic_width =
          Numeric_semantic_width_private.issue ~target:prepared.target
            ~width:statement.semantic_width
            ~evidence_full_key:statement.semantic_width_evidence
          |> Result.map_error (fun reason -> Target_unavailable reason)
        in
        Ok (finish prepared ~meaning:Signed_view ~prerequisites:[unsigned]
          ~semantic_width
          ~result_interpretation:statement.result_interpretation evidence)
    | Relation_request (kind, view, bounds) ->
        let* declarations = Numeric_semantics_binding_private.complete_local ~completion ~implementation ~validated
          |> Result.map_error (fun _ -> Prerequisite_mismatch) in
        let declaration_of (law : t) = match List.filter (fun declaration ->
          String.equal declaration.Numeric_semantics_correlation_private.role.numeric_role_callable_uid law.role.callable_uid
          && String.equal declaration.role.numeric_role_semantics_uid law.role.semantics_uid) declarations with
          | [declaration] -> Ok declaration | _ -> Error Prerequisite_mismatch in
        let readmit (law : t) =
          let* declaration = declaration_of law in
          let* request = match law.meaning, law.prerequisites with
            | Unsigned_range, [] -> Ok Unsigned_request
            | Signed_view, [unsigned] -> Ok (Signed_request unsigned)
            | Relation kind, [view] -> Ok (Relation_request (kind,view,None))
            | Relation kind, [view;bounds] -> Ok (Relation_request (kind,view,Some bounds))
            | _ -> Error Prerequisite_mismatch in
          let* current = admit ~request ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration in
          if equal law current && String.equal law.carrier.binding_full_key prepared.carrier.binding_full_key
            && String.equal law.base_int.authenticated_base_int_artifact_binding_key prepared.base_int.authenticated_base_int_artifact_binding_key
            && Build_target_profile_private.equal_instance law.target prepared.target then Ok declaration
          else Error Prerequisite_mismatch in
        let* signed = match view.meaning with Unsigned_range -> Ok false | Signed_view -> Ok true | _ -> Error Prerequisite_mismatch in
        let* view_declaration = readmit view in
        let* view_definition = match view_declaration.callable_definition with Some view -> Ok view | None -> Error Prerequisite_mismatch in
        let* bounds_definition = match bounds with
          | None -> Ok None
          | Some bounds ->
              let* () = if bounds.meaning = Relation Numeric_relation_match_private.Bounds
                && (match bounds.prerequisites with [original] -> equal original view | _ -> false)
                then Ok () else Error Prerequisite_mismatch in
              let* declaration = readmit bounds in
              (match declaration.callable_definition with Some definition -> Ok (Some definition) | None -> Error Prerequisite_mismatch) in
        let* relation = match Numeric_relation_match_private.match_relation ~kind ~base_full_key:prepared.base_int.base_int_full_key
          ~target:prepared.target ~semantic_width:view.semantic_width.width ~signed
          ~view:view_definition ~bounds:bounds_definition declaration with
          | Some relation -> Ok relation | None -> Error Statement_mismatch in
        let* evidence = Numeric_proof_dependencies_private.complete_definition ~completion ~implementation ~validated
          ~context:Ghost_context declaration.definition |> Result.map_error (fun reason -> Dependencies_unavailable reason) in
        let prerequisites = view :: Option.to_list bounds in
        let result_interpretation = match kind with Numeric_relation_match_private.Bounds -> Numeric_law_match_private.Boolean_result | _ -> Carrier_result in
        let* semantic_width =
          Numeric_semantic_width_private.issue ~target:prepared.target
            ~width:view.semantic_width.width
            ~evidence_full_key:
              (Numeric_receipt_private.encode
                 ~schema:"verocaml.numeric-relation-width-evidence.v1"
                 [ view.semantic_width.full_key; relation.material ])
          |> Result.map_error (fun reason -> Target_unavailable reason)
        in
        Ok (finish prepared ~meaning:(Relation kind) ~prerequisites
          ~semantic_width ~result_interpretation ~relation evidence) in
  log_admission ~implementation:(implementation [@log_value.debug]) ~declaration:(declaration [@log_value.debug]) result
[@@delator.instrument] [@@delator.level debug]

let admit_unsigned_range ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration =
  admit ~request:Unsigned_request ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration

let admit_signed_view ~completion ~implementation ~validated ~base_provider ~logical_sort ~target ~unsigned declaration =
  admit ~request:(Signed_request unsigned) ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration

let admit_relation ~completion ~implementation ~validated ~base_provider ~logical_sort ~target ~kind ~view ~bounds declaration =
  admit ~request:(Relation_request (kind,view,bounds)) ~completion ~implementation ~validated ~base_provider ~logical_sort ~target declaration

let admit_operation_extension ~completion:(completion [@delator.skip]) ~implementation:(implementation [@delator.skip])
    ~validated:(validated [@delator.skip]) ~imported:(imported [@delator.skip]) ~origin:(origin [@delator.skip])
    ~view:((view : t) [@delator.skip]) (declaration [@delator.skip]) =
  let result =
    let source_role=declaration.Numeric_semantics_correlation_private.role in
    let* () = if Verification_driver_private.completion_matches completion ~implementation ~validated
      && List.exists ((==) source_role) implementation.Cmt_input.interface_numeric_claims.numeric_roles
      then Ok () else Error Completion_mismatch in
    let* () = if source_role.numeric_role_source.role_schema="numeric-role.v1"
      && source_role.numeric_role_source.role_identity="operation" then Ok () else Error Unsupported_role in
    let* () = if source_role.numeric_role_callable_owner_unit=implementation.unit_name
      && source_role.numeric_role_semantics_owner_unit=implementation.unit_name
      && source_role.numeric_role_carrier_uid=view.carrier.carrier_claim.carrier_uid
      && source_role.numeric_role_carrier_owner_cmi_full_key=view.carrier.carrier_claim.owner.owner_cmi_full_key
      && (view.meaning=Unsigned_range || view.meaning=Signed_view)
      then Ok () else Error Prerequisite_mismatch in
    let origin_key=Imported_callable.artifact_full_key origin in
    let* () = if origin_key=view.issuer_artifact_full_key
      && origin_key=view.source_evidence.owner_artifact_full_key
      && List.exists (fun candidate -> Imported_callable.artifact_full_key candidate=origin_key) (Imported_callable.artifacts imported)
      then Ok () else Error Prerequisite_mismatch in
    let* artifact = Numeric_artifact_binding_private.correlate origin view.carrier.carrier_claim
      |> Result.map_error (fun reason -> Carrier_unavailable reason) in
    let* carrier=Numeric_artifact_binding_private.facts artifact |> Result.map_error (fun reason -> Carrier_unavailable reason) in
    let* () = if carrier.binding_full_key=view.carrier.binding_full_key then Ok () else Error Prerequisite_mismatch in
    let* target=Build_target_profile_private.decode_instance (Build_target_profile_private.capability ()) view.target.full_key
      |> Result.map_error (fun reason -> Target_unavailable reason) in
    let* role = match (Cmt_input.retained_numeric_claims {implementation.interface_numeric_claims with numeric_roles=[source_role]}).role_reconstruction_claims with
      | [encoded] -> Numeric_interface_claim_private.decode_role encoded |> Result.map_error (fun reason -> Carrier_unavailable reason)
      | _ -> Error (Carrier_unavailable "The operation extension has no unique compiler-issued declaration.") in
    let* authority = match declaration.kind with
      | Numeric_semantics_binding_private.Completed_proof_declaration -> Ok Checked_proof
      | Explicit_axiom_declaration -> Ok Explicit_axiom
      | Ordinary_specification_declaration -> Error Statement_mismatch in
    let program=Sst_validation.program validated in
    let* registration=Imported_callable.seal_calls imported ~implementation ~program
      |> Result.map_error (fun _ -> Prerequisite_mismatch) in
    Fun.protect ~finally:(fun () -> Imported_callable.invalidate_registration registration) (fun () ->
      let rec flatten acc expression = List.fold_left flatten (expression :: acc) (Sst_callback_private.expression_children expression) in
      let expressions=List.concat_map Numeric_proof_dependencies_private.definition_expressions program.functions
        |> List.fold_left flatten [] in
      let is_view (summary : Imported_callable.callable_snapshot) = summary.provider_unit=origin.unit_name
        && summary.binding_uid=view.role.callable_uid in
      let occurrences=List.filter_map (fun expression -> match Imported_callable.find_call registration expression with
        | Some call when is_view (Imported_callable.call_summary call) -> Some (expression,Imported_callable.call_summary call)
        | _ -> None) expressions in
      let* imported_view = match List.map (fun (_, (summary : Imported_callable.callable_snapshot)) -> summary.definition) occurrences
        |> List.sort_uniq (fun a b -> Stdlib.compare a.Sst.function_id b.Sst.function_id) with
        | [definition] -> Ok definition | _ -> Error Prerequisite_mismatch in
      let* leaves=List.fold_left (fun result (expression,_) ->
        let* leaves=result in
        let* leaf=Numeric_proof_dependencies_private.imported_spec_leaf ~registration ~origin
          ~evidence:view.source_evidence ~path:view.role.callable_path ~interface_uid:view.role.callable_uid expression
          |> Result.map_error (fun _ -> Prerequisite_mismatch) in
        Ok (leaf :: leaves)) (Ok []) occurrences in
      let* relation=match Numeric_relation_match_private.match_relation ~kind:Operation
        ~base_full_key:view.base_int.base_int_full_key ~target
        ~semantic_width:view.semantic_width.width ~signed:(view.meaning=Signed_view)
        ~view:imported_view ~bounds:None declaration with
        | Some relation -> Ok relation | None -> Error Statement_mismatch in
      let* evidence=Numeric_proof_dependencies_private.complete_definition_with_imported ~completion ~implementation ~validated
        ~imported_leaves:leaves declaration.definition |> Result.map_error (fun reason -> Dependencies_unavailable reason) in
      let prepared={issuer_unit=implementation.unit_name;issuer_artifact_full_key=Imported_callable.artifact_full_key implementation;
        base_int=view.base_int;carrier;role;target;authority} in
      let* semantic_width =
        Numeric_semantic_width_private.issue ~target
          ~width:view.semantic_width.width
          ~evidence_full_key:
            (Numeric_receipt_private.encode
               ~schema:"verocaml.numeric-operation-width-evidence.v1"
               [ view.semantic_width.full_key; relation.material ])
        |> Result.map_error (fun reason -> Target_unavailable reason)
      in
      Ok (finish prepared ~meaning:(Relation Operation) ~prerequisites:[view]
        ~semantic_width ~result_interpretation:Carrier_result
        ~relation evidence)) in
  [%log.debug "completed cross-provider numeric operation admission"
    ~issuer:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~carrier_owner:(Delator.Field.string origin.Cmt_input.unit_name)
    ~callable:(Delator.Field.string declaration.Numeric_semantics_correlation_private.role.numeric_role_callable_path)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason:(Delator.Field.string (match result with Ok _ -> "exact-imported-view-prerequisite" | Error reason -> reason_message reason))];
  result
[@@delator.instrument] [@@delator.level debug]
