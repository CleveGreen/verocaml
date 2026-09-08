type body_visibility = Visible | Opaque
type native_candidacy = Bounded_mathematical_view | Unavailable_layout
type t = {
  law : Numeric_ghost_law_private.t;
  refinements : Numeric_runtime_refinement_private.t list;
  visibility : body_visibility;
  reveal : bool;
  inline : bool;
  lowering_candidate : native_candidacy;
  full_key : string;
}
type occurrence_authority = Ghost_semantics | Runtime_equivalence of Numeric_runtime_refinement_private.t
let ( let* ) = Result.bind
let complete ~completion:(completion [@delator.skip]) ~implementation:(implementation [@delator.skip])
    ~carrier_origin:(carrier_origin [@delator.skip])
    ~validated:(validated [@delator.skip]) ~law:(law [@delator.skip]) ~refinements:(refinements [@delator.skip])
    (declaration [@delator.skip]) =
  let result =
    let role = declaration.Numeric_semantics_correlation_private.role in
    let* () = if Verification_driver_private.completion_matches completion ~implementation ~validated
      && role.numeric_role_callable_uid = law.Numeric_ghost_law_private.role.callable_uid
      && role.numeric_role_semantics_uid = law.role.semantics_uid
      && Numeric_source_claim_private.role_material role.numeric_role_source = law.role.source_claim
      && Imported_callable.artifact_full_key implementation=law.issuer_artifact_full_key
      then Ok () else Error "Numeric descriptor capabilities need the same completed law declaration." in
    let* binding = Numeric_artifact_binding_private.correlate carrier_origin law.carrier.carrier_claim in
    let* facts = Numeric_artifact_binding_private.facts binding in
    let* () = if facts.binding_full_key = law.carrier.binding_full_key then Ok ()
      else Error "Numeric descriptor capabilities belong to a different implementation." in
    let refinements = List.filter (fun (refinement : Numeric_runtime_refinement_private.t) ->
      Numeric_ghost_law_private.equal refinement.law law) refinements
      |> List.sort_uniq (fun a b -> String.compare a.Numeric_runtime_refinement_private.full_key b.full_key) in
    let body_available = match declaration.callable_definition with
      | Some {Sst.body=Spec_definition _;_} -> true | _ -> false in
    let source = role.numeric_role_source in
    let visibility = if source.visibility="visible" && body_available then Visible else Opaque in
    let reveal = source.reveal && body_available and inline = source.inline && body_available in
    let lowering_candidate = match law.meaning with Unsigned_range | Signed_view -> Bounded_mathematical_view
      | Relation _ -> Unavailable_layout in
    let full_key = Numeric_receipt_private.encode ~schema:"verocaml.admitted-numeric-descriptor.v1"
      [law.full_key; Numeric_receipt_private.list (List.map (fun (refinement : Numeric_runtime_refinement_private.t) -> refinement.full_key) refinements);
       (match visibility with Visible -> "visible" | Opaque -> "opaque");string_of_bool reveal;string_of_bool inline;
       (match lowering_candidate with Bounded_mathematical_view -> "bounded-mathematical-expression-only" | Unavailable_layout -> "native-layout-unavailable")] in
    Ok {law;refinements;visibility;reveal;inline;lowering_candidate;full_key} in
  [%log.debug "completed independent numeric descriptor capabilities"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~runtime_refinements:(Delator.Field.int (match result with Ok descriptor -> List.length descriptor.refinements | Error _ -> 0))
    ~mathematical_view_candidate:(Delator.Field.bool (match result with Ok {lowering_candidate=Bounded_mathematical_view;_} -> true | _ -> false))
    ~native_carrier_candidate:(Delator.Field.bool false)
    ~layout_reason:(Delator.Field.string "compiler-value-layout-does-not-establish-native-numeric-width")];
  result
[@@delator.instrument] [@@delator.level debug]

let authorize ~completion:(completion [@delator.skip]) ~implementation:(implementation [@delator.skip])
    ~origin:(origin [@delator.skip]) ~carrier_origin:(carrier_origin [@delator.skip]) ~imported:(imported [@delator.skip])
    ~validated:(validated [@delator.skip]) ~descriptor:(descriptor [@delator.skip])
    ~caller:(caller [@delator.skip]) ~occurrence:(occurrence [@delator.skip]) =
  let result =
    let program = Sst_validation.program validated in
    let* () = if Verification_driver_private.completion_matches completion ~implementation ~validated
      && List.exists (fun definition -> definition == caller) program.functions then Ok ()
      else Error "The numeric occurrence does not belong to this completed program." in
    let* () = if Imported_callable.artifact_full_key origin=descriptor.law.issuer_artifact_full_key then Ok ()
      else Error "The numeric law belongs to a different issuer implementation." in
    let* binding = Numeric_artifact_binding_private.correlate carrier_origin descriptor.law.carrier.carrier_claim in
    let* facts = Numeric_artifact_binding_private.facts binding in
    let* () = if facts.binding_full_key=descriptor.law.carrier.binding_full_key then Ok ()
      else Error "This occurrence and numeric descriptor belong to different providers." in
    let rec contains expression = expression == occurrence ||
      List.exists contains (Sst_callback_private.expression_children expression) in
    let logical_roots =
      List.map (fun (clause : Sst.predicate_clause) -> clause.predicate.expression)
        (caller.contracts.requires @ caller.contracts.assertions @ caller.contracts.decreases)
      @ List.map (fun (clause : Sst.ensures_clause) -> clause.predicate.expression) caller.contracts.ensures
      @ (match caller.body with Spec_definition body | Proof_body {body;_} -> [body.expression] | _ -> []) in
    let executable_roots = match caller.body with Checked_exec {body;_} -> [body.expression] | _ -> [] in
    let rec proof_region_contains expression = match expression.Sst.expression_desc with
      | Proof_region body -> contains body
      | _ -> List.exists proof_region_contains (Sst_callback_private.expression_children expression) in
    let rec executable_contains expression = match expression.Sst.expression_desc with
      | Proof_region _ -> false
      | _ -> expression == occurrence || List.exists executable_contains (Sst_callback_private.expression_children expression) in
    let in_logical = (caller.mode<>Sst.Exec && List.exists contains logical_roots)
      || List.exists proof_region_contains executable_roots
    and in_executable = List.exists executable_contains executable_roots in
    let* () = if in_logical || in_executable then Ok () else Error "The expression is not an authenticated source occurrence." in
    let* callee, form, recursive = match occurrence.Sst.expression_desc with
      | Direct_call {callee;call_form;recursive;type_arguments=[];_} -> Ok (callee,call_form,recursive)
      | _ -> Error "Numeric occurrence authority is unavailable for this call form." in
    let instances = Typedtree_adapter_private.Public.issued_callable_instances ~structure:implementation.structure ~program in
    let* imported_summary = match imported with
      | None -> Ok None
      | Some environment ->
          let* registration = Imported_callable.seal_calls environment ~implementation ~program in
          Fun.protect ~finally:(fun () -> Imported_callable.invalidate_registration registration) (fun () ->
            Ok (Option.map Imported_callable.call_summary (Imported_callable.find_call registration occurrence))) in
    let matches role definition = List.exists (fun instance ->
      Typedtree_adapter_private.Public.callable_instance_definition instance == definition
      && Cmt_input.interface_value_uid_correlates implementation ~path:role.Numeric_interface_claim_private.callable_path
        ~interface_uid:role.callable_uid ~implementation_uid:(Typedtree_adapter_private.Public.callable_instance_binding_uid instance)) instances in
    let* definition = match imported_summary, Sst_validation.find_callable validated callee with
      | Some summary, _ -> Ok summary.Imported_callable.definition
      | None, Some callable -> Ok (Sst_validation.callable_definition callable) | None, None -> Error "The numeric callee has no validated definition." in
    let semantic_matches = match imported_summary with
      | None -> matches descriptor.law.role definition
      | Some summary -> summary.provider_unit=origin.Cmt_input.unit_name
          && summary.binding_uid=descriptor.law.role.callable_uid in
    if form=Sst.Specification_call && not recursive && in_logical && not in_executable
      && (match descriptor.law.relation with None -> true | Some relation -> relation.required_guards=[])
      && semantic_matches then Ok Ghost_semantics
    else if form=Sst.Exec_call && not recursive && in_executable then
      (match List.find_opt (fun (refinement : Numeric_runtime_refinement_private.t) ->
        match imported_summary with None -> refinement.executable == definition
        | Some summary -> summary.provider_unit=origin.Cmt_input.unit_name && summary.binding_uid=refinement.interface_uid) descriptor.refinements with
       | Some refinement -> Ok (Runtime_equivalence refinement)
       | None -> Error "Using numeric semantics for an executable result requires its exact proved or explicitly trusted runtime equivalence.")
    else Error "This stage or executable observation cannot use ghost-only numeric authority." in
  [%log.debug "authenticated numeric authority for source occurrence"
    ~caller:(Delator.Field.string caller.Sst.function_id.function_name)
    ~authorized:(Delator.Field.bool (Result.is_ok result))
    ~authority:(Delator.Field.string (match result with Ok Ghost_semantics -> "ghost-semantic-law"
      | Ok (Runtime_equivalence refinement) -> (match refinement.authority with Checked_implementation -> "checked-runtime-equivalence" | Explicit_external_body -> "trusted-runtime-equivalence")
      | Error reason -> reason))
    ~native_operation_created:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]

let authorize_occurrence ~completion ~implementation ~validated ~descriptor ~caller ~occurrence =
  authorize ~completion ~implementation ~origin:implementation ~carrier_origin:implementation ~imported:None ~validated ~descriptor ~caller ~occurrence

module For_registry = struct
  let authorize_occurrence ~completion ~implementation ~origin ~carrier_origin ~imported ~validated ~descriptor ~caller ~occurrence =
    authorize ~completion ~implementation ~origin ~carrier_origin ~imported:(Some imported) ~validated ~descriptor ~caller ~occurrence
end
