type base_int_claim_v1 = {
  base_int_full_key : string;
  validated_base_int_digest : string;
  outer_artifact_binding_key_claim : string;
}

type representation = Immediate | Boxed | Unboxed | Unsupported of string

type carrier_claim = {
  type_path_claim : string;
  type_uid_claim : string;
  constructor_abi_claim : string;
  binder_abi_claim : string;
  representation_claim : representation;
  type_argument_claims : string list;
  fully_instantiated_claim : bool;
}

type callable_claim = {
  callable_path_claim : string;
  callable_uid_claim : string;
  binder_abi_claim : string;
  parameter_abi_claims : string list;
  result_abi_claim : string;
  total_claim : bool;
  effect_claims : string list;
  exception_claims : string list;
}

type semantic_role =
  | Unsigned_view
  | Signed_view
  | Representable_bounds
  | Checked_conversion
  | Partial_conversion of { success_abi : string; failure_abi : string }
  | Modular_conversion
  | Operation of { role_schema : string; role_identity : string }

type int_semantics =
  | Unsigned_range of { width : int }
  | Signed_twos_complement of { width : int }
  | Bounds of { minimum : Z.t; maximum : Z.t }
  | Checked_exact
  | Partial_exact
  | Modular_quotient of { width : int }
  | Int_relation of { relation_full_key_claim : string }

type proof_claim = {
  proof_full_key_claim : string;
  dependency_closure_claims : string list;
}

type semantic_authority_claim =
  | Kernel_schema_claim of { dual_role_receipt_claim : string }
  | Proved_law_claim of proof_claim
  | Trusted_axiom_claim of { trust_receipt_claim : string }
  | Ordinary_specification_claim

type implementation_refinement_claim =
  | Kernel_refinement_claim of { dual_role_receipt_claim : string }
  | Proved_refinement_claim of {
      proof_claim : proof_claim;
      implementation_artifact_claim : string;
      implementation_body_claim : string;
    }
  | Trusted_refinement_claim of {
      trust_receipt_claim : string;
      implementation_artifact_claim : string;
      implementation_body_claim : string;
    }
  | Tested_only_claim of { evidence_claim : string }
  | Unknown_refinement_claim

type visibility = Visible_body | Opaque_body
type reveal_permission = Reveal_allowed | Reveal_forbidden
type inline_permission = Inline_allowed | Inline_forbidden

type binding_claim = {
  law_identity_claim : string;
  callable_claim : callable_claim;
  semantic_role : semantic_role;
  int_semantics : int_semantics;
  semantic_authority_claim : semantic_authority_claim;
  implementation_refinement_claim : implementation_refinement_claim;
  visibility : visibility;
  reveal_permission : reveal_permission;
  inline_permission : inline_permission;
}

type claim = {
  schema : string;
  base_int_claim : base_int_claim_v1;
  provider_unit_claim : string;
  provider_origin_claim : string;
  provider_interface_receipt_claim : string;
  direct_import_provenance_claim : string;
  carrier_claim : carrier_claim;
  profile_claim : Target_profile_private.claim;
  target_claim : Target_profile_private.instance_claim;
  binding_claims : binding_claim list;
  issuer_claim : string;
  immutable_fingerprint_claim : string;
  full_key : string;
  checked_digest : string;
}

let schema = "verocaml.numeric-extension-claim.v1"
let digest_domain = "verocaml.numeric-extension-claim.full-key.v1"

let ( let* ) value continuation =
  match value with Ok value -> continuation value | Error _ as error -> error

let base_material value =
  Numeric_receipt_private.encode ~schema:"verocaml.base-int-reference-claim.v1"
    [ value.base_int_full_key; value.validated_base_int_digest;
      value.outer_artifact_binding_key_claim ]

let base_int_claim ~logical_sort ~validated_digest
    ~outer_artifact_binding_key_claim =
  let result =
    let base_int_full_key = Logical_sort_private.canonical_material logical_sort in
    if
      not
        (String.equal validated_digest (Logical_sort_private.digest logical_sort))
    then Error "base Int digest claim differs from its complete logical-sort key"
    else if String.equal outer_artifact_binding_key_claim "" then
      Error "base Int outer artifact binding claim is empty"
    else
      Ok
        { base_int_full_key; validated_base_int_digest = validated_digest;
          outer_artifact_binding_key_claim }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-base-int-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-base-int-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let representation_material = function
  | Immediate -> "immediate"
  | Boxed -> "boxed"
  | Unboxed -> "unboxed"
  | Unsupported identity ->
      Numeric_receipt_private.encode ~schema:"unsupported-representation-claim.v1"
        [ identity ]

let carrier_material value =
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-carrier-claim.v1"
    [ value.type_path_claim; value.type_uid_claim;
      value.constructor_abi_claim; value.binder_abi_claim;
      representation_material value.representation_claim;
      Numeric_receipt_private.list value.type_argument_claims;
      string_of_bool value.fully_instantiated_claim ]

let carrier_claim ~type_path_claim ~type_uid_claim ~constructor_abi_claim
    ~binder_abi_claim ~representation_claim ~type_argument_claims
    ~fully_instantiated_claim =
  let result =
    if
      List.exists (String.equal "")
        (type_path_claim :: type_uid_claim :: constructor_abi_claim
       :: binder_abi_claim :: type_argument_claims)
    then Error "numeric carrier claim contains an empty identity"
    else
      Ok
        { type_path_claim; type_uid_claim; constructor_abi_claim;
          binder_abi_claim; representation_claim; type_argument_claims;
          fully_instantiated_claim }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-carrier-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-carrier-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let callable_material value =
  Numeric_receipt_private.encode ~schema:"verocaml.callable-abi-claim.v1"
    [ value.callable_path_claim; value.callable_uid_claim;
      value.binder_abi_claim;
      Numeric_receipt_private.list value.parameter_abi_claims;
      value.result_abi_claim; string_of_bool value.total_claim;
      Numeric_receipt_private.list value.effect_claims;
      Numeric_receipt_private.list value.exception_claims ]

let callable_claim ~callable_path_claim ~callable_uid_claim ~binder_abi_claim
    ~parameter_abi_claims ~result_abi_claim ~total_claim ~effect_claims
    ~exception_claims =
  let result =
    if
      List.exists (String.equal "")
        (callable_path_claim :: callable_uid_claim :: binder_abi_claim
       :: result_abi_claim
       :: parameter_abi_claims @ effect_claims @ exception_claims)
    then Error "numeric callable claim contains an empty ABI identity"
    else
      let* effect_claims =
        Numeric_receipt_private.sorted_unique ~label:"effect claims"
          effect_claims
      in
      let* exception_claims =
        Numeric_receipt_private.sorted_unique ~label:"exception claims"
          exception_claims
      in
      Ok
        { callable_path_claim; callable_uid_claim; binder_abi_claim;
          parameter_abi_claims; result_abi_claim; total_claim; effect_claims;
          exception_claims }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-callable-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-callable-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let proof_material value =
  Numeric_receipt_private.encode ~schema:"verocaml.proof-receipt-claim.v1"
    [ value.proof_full_key_claim;
      Numeric_receipt_private.list value.dependency_closure_claims ]

let proof_claim ~proof_full_key_claim ~dependency_closure_claims
    ~forbidden_dependency_claims =
  let result =
    if String.equal proof_full_key_claim "" then Error "proof claim key is empty"
    else
      let* dependency_closure_claims =
        Numeric_receipt_private.sorted_unique ~label:"proof dependency claims"
          dependency_closure_claims
      in
      if List.mem proof_full_key_claim dependency_closure_claims then
        Error "proof claim is directly or transitively self-dependent"
      else if
        List.exists
          (fun forbidden -> List.mem forbidden dependency_closure_claims)
          forbidden_dependency_claims
      then Error "proof claim consumes forbidden authority"
      else Ok { proof_full_key_claim; dependency_closure_claims }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-proof-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-proof-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let semantic_role_material = function
  | Unsigned_view -> "unsigned-view"
  | Signed_view -> "signed-view"
  | Representable_bounds -> "representable-bounds"
  | Checked_conversion -> "checked-conversion"
  | Partial_conversion { success_abi; failure_abi } ->
      Numeric_receipt_private.encode ~schema:"partial-conversion-role-claim.v1"
        [ success_abi; failure_abi ]
  | Modular_conversion -> "modular-conversion"
  | Operation { role_schema; role_identity } ->
      Numeric_receipt_private.encode ~schema:"operation-role-claim.v1"
        [ role_schema; role_identity ]

let semantics_material = function
  | Unsigned_range { width } ->
      Numeric_receipt_private.encode ~schema:"unsigned-range-claim.v1"
        [ string_of_int width ]
  | Signed_twos_complement { width } ->
      Numeric_receipt_private.encode ~schema:"signed-view-claim.v1"
        [ string_of_int width ]
  | Bounds { minimum; maximum } ->
      Numeric_receipt_private.encode ~schema:"bounds-claim.v1"
        [ Z.to_string minimum; Z.to_string maximum ]
  | Checked_exact -> "checked-exact-claim-v1"
  | Partial_exact -> "partial-exact-claim-v1"
  | Modular_quotient { width } ->
      Numeric_receipt_private.encode ~schema:"modular-quotient-claim.v1"
        [ string_of_int width; "x=u+(2^w*q)"; "0<=u"; "u<2^w" ]
  | Int_relation { relation_full_key_claim } ->
      Numeric_receipt_private.encode ~schema:"int-relation-claim.v1"
        [ relation_full_key_claim ]

let semantic_authority_material = function
  | Kernel_schema_claim { dual_role_receipt_claim } ->
      Numeric_receipt_private.encode ~schema:"kernel-law-claim.v1"
        [ dual_role_receipt_claim ]
  | Proved_law_claim proof ->
      Numeric_receipt_private.encode ~schema:"proved-law-claim.v1"
        [ proof_material proof ]
  | Trusted_axiom_claim { trust_receipt_claim } ->
      Numeric_receipt_private.encode ~schema:"trusted-law-claim.v1"
        [ trust_receipt_claim ]
  | Ordinary_specification_claim -> "ordinary-law-claim-v1"

let refinement_material = function
  | Kernel_refinement_claim { dual_role_receipt_claim } ->
      Numeric_receipt_private.encode ~schema:"kernel-refinement-claim.v1"
        [ dual_role_receipt_claim ]
  | Proved_refinement_claim
      { proof_claim; implementation_artifact_claim;
        implementation_body_claim } ->
      Numeric_receipt_private.encode ~schema:"proved-refinement-claim.v1"
        [ proof_material proof_claim; implementation_artifact_claim;
          implementation_body_claim ]
  | Trusted_refinement_claim
      { trust_receipt_claim; implementation_artifact_claim;
        implementation_body_claim } ->
      Numeric_receipt_private.encode ~schema:"trusted-refinement-claim.v1"
        [ trust_receipt_claim; implementation_artifact_claim;
          implementation_body_claim ]
  | Tested_only_claim { evidence_claim } ->
      Numeric_receipt_private.encode ~schema:"tested-refinement-claim.v1"
        [ evidence_claim ]
  | Unknown_refinement_claim -> "unknown-refinement-claim-v1"

let binding_claim_material value =
  let visibility =
    match value.visibility with Visible_body -> "visible" | Opaque_body -> "opaque"
  and reveal =
    match value.reveal_permission with
    | Reveal_allowed -> "allowed"
    | Reveal_forbidden -> "forbidden"
  and inline =
    match value.inline_permission with
    | Inline_allowed -> "allowed"
    | Inline_forbidden -> "forbidden"
  in
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-binding-claim.v1"
    [ value.law_identity_claim; callable_material value.callable_claim;
      semantic_role_material value.semantic_role;
      semantics_material value.int_semantics;
      semantic_authority_material value.semantic_authority_claim;
      refinement_material value.implementation_refinement_claim; visibility;
      reveal; inline ]

let validate_semantics role semantics =
  match (role, semantics) with
  | Unsigned_view, Unsigned_range { width }
  | Signed_view, Signed_twos_complement { width }
  | Modular_conversion, Modular_quotient { width } ->
      Numeric_receipt_private.positive ~label:"semantic width" width
  | Representable_bounds, Bounds { minimum; maximum } ->
      if Z.leq minimum maximum then Ok () else Error "bounds are inverted"
  | Checked_conversion, Checked_exact -> Ok ()
  | Partial_conversion { success_abi; failure_abi }, Partial_exact ->
      if String.equal success_abi "" || String.equal failure_abi "" then
        Error "partial conversion result ABI claim is empty"
      else Ok ()
  | Operation { role_schema; role_identity },
    Int_relation { relation_full_key_claim } ->
      if
        String.equal role_schema "" || String.equal role_identity ""
        || String.equal relation_full_key_claim ""
      then Error "operation semantic identity claim is empty"
      else Ok ()
  | _ -> Error "semantic role and Int relation claims do not match"

let validate_authority law_identity = function
  | Kernel_schema_claim { dual_role_receipt_claim }
  | Trusted_axiom_claim { trust_receipt_claim = dual_role_receipt_claim } ->
      Numeric_receipt_private.nonempty ~label:"law authority claim"
        dual_role_receipt_claim
  | Proved_law_claim proof ->
      if List.mem law_identity proof.dependency_closure_claims then
        Error "law proof claim depends on the law claim it would authorize"
      else Ok ()
  | Ordinary_specification_claim -> Ok ()

let validate_refinement law_identity = function
  | Kernel_refinement_claim { dual_role_receipt_claim } ->
      Numeric_receipt_private.nonempty ~label:"kernel refinement claim"
        dual_role_receipt_claim
  | Proved_refinement_claim
      { proof_claim; implementation_artifact_claim;
        implementation_body_claim } ->
      if List.mem law_identity proof_claim.dependency_closure_claims then
        Error "refinement proof claim depends on enabled law claim"
      else if
        String.equal implementation_artifact_claim ""
        || String.equal implementation_body_claim ""
      then Error "proved refinement claim lacks implementation identity"
      else Ok ()
  | Trusted_refinement_claim
      { trust_receipt_claim; implementation_artifact_claim;
        implementation_body_claim } ->
      if
        List.exists (String.equal "")
          [ trust_receipt_claim; implementation_artifact_claim;
            implementation_body_claim ]
      then Error "trusted refinement claim is incomplete"
      else Ok ()
  | Tested_only_claim { evidence_claim } ->
      Numeric_receipt_private.nonempty ~label:"tested evidence claim"
        evidence_claim
  | Unknown_refinement_claim -> Ok ()

let binding_claim ~law_identity_claim ~callable_claim ~semantic_role
    ~int_semantics ~semantic_authority_claim ~implementation_refinement_claim
    ~visibility ~reveal_permission ~inline_permission =
  let result =
    let* () =
      Numeric_receipt_private.nonempty ~label:"law identity claim"
        law_identity_claim
    in
    let* () = validate_semantics semantic_role int_semantics in
    let* () = validate_authority law_identity_claim semantic_authority_claim in
    let* () =
      validate_refinement law_identity_claim implementation_refinement_claim
    in
    let kernel_pair =
      match (semantic_authority_claim, implementation_refinement_claim) with
      | ( Kernel_schema_claim { dual_role_receipt_claim = left },
          Kernel_refinement_claim { dual_role_receipt_claim = right } ) ->
          String.equal left right
      | Kernel_schema_claim _, _ | _, Kernel_refinement_claim _ -> false
      | _ -> true
    in
    if not kernel_pair then Error "kernel dual-role claims differ"
    else
      Ok
        { law_identity_claim; callable_claim; semantic_role; int_semantics;
          semantic_authority_claim; implementation_refinement_claim; visibility;
          reveal_permission; inline_permission }
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-binding-claim-validation")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-binding-claim-validation")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let rekey value =
  let binding_claims =
    List.sort
      (fun left right ->
        String.compare (binding_claim_material left)
          (binding_claim_material right))
      value.binding_claims
  in
  let immutable_fingerprint_claim =
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-immutable-fingerprint-claim.v1"
      [ base_material value.base_int_claim; carrier_material value.carrier_claim;
        value.profile_claim.Target_profile_private.full_key;
        value.target_claim.Target_profile_private.full_key;
        Numeric_receipt_private.list
          (binding_claims
          |> List.filter (fun binding ->
                 match binding.semantic_role with Operation _ -> false | _ -> true)
          |> List.map binding_claim_material) ]
  in
  let full_key =
    Numeric_receipt_private.encode ~schema
      [ base_material value.base_int_claim; value.provider_unit_claim;
        value.provider_origin_claim; value.provider_interface_receipt_claim;
        value.direct_import_provenance_claim;
        carrier_material value.carrier_claim;
        value.profile_claim.Target_profile_private.full_key;
        value.target_claim.Target_profile_private.full_key;
        Numeric_receipt_private.list
          (List.map binding_claim_material binding_claims);
        value.issuer_claim; immutable_fingerprint_claim ]
  in
  let checked_digest = Numeric_receipt_private.digest ~domain:digest_domain full_key in
  { value with binding_claims; immutable_fingerprint_claim; full_key;
    checked_digest }

let issue_claim ~base_int_claim ~provider_unit_claim ~provider_origin_claim
    ~provider_interface_receipt_claim ~direct_import_provenance_claim
    ~carrier_claim ~(profile_claim : Target_profile_private.claim)
    ~(target_claim : Target_profile_private.instance_claim) ~binding_claims
    ~issuer_claim =
  let result =
    if
      List.exists (String.equal "")
        [ provider_unit_claim; provider_origin_claim;
          provider_interface_receipt_claim; direct_import_provenance_claim;
          issuer_claim ]
    then Error "numeric descriptor claim has an empty origin field"
    else if binding_claims = [] then
      Error "numeric descriptor claim has no bindings"
    else if
      not
        (String.equal profile_claim.full_key target_claim.profile_full_key)
      || not
           (String.equal profile_claim.checked_digest
              target_claim.profile_checked_digest)
    then Error "target claim is substituted from another profile claim"
    else
      match carrier_claim.representation_claim with
      | Unboxed | Unsupported _ ->
          Error "numeric descriptor claim uses an unsupported representation"
      | Immediate | Boxed ->
          let roles =
            List.map
              (fun binding -> semantic_role_material binding.semantic_role)
              binding_claims
          in
          if
            List.length roles
            <> List.length (List.sort_uniq String.compare roles)
          then Error "numeric descriptor claim overlaps semantic roles"
          else
            let semantic_widths =
              binding_claims
              |> List.filter_map (fun binding ->
                     match binding.int_semantics with
                     | Unsigned_range { width }
                     | Signed_twos_complement { width }
                     | Modular_quotient { width } -> Some width
                     | Bounds _ | Checked_exact | Partial_exact
                     | Int_relation _ -> None)
              |> List.sort_uniq Int.compare
            in
            if
              List.exists
                (fun width ->
                  width <= 0
                  || not
                       (Target_profile_private.logical_bv_width_permitted
                          profile_claim.logical_bv_width_permission
                          (Z.of_int width)))
                semantic_widths
            then Error "numeric semantic width is outside the logical profile"
            else if List.length semantic_widths > 1 then
              Error "numeric descriptor bindings disagree on semantic width"
            else
              Ok
                (rekey
                   { schema; base_int_claim; provider_unit_claim;
                     provider_origin_claim; provider_interface_receipt_claim;
                     direct_import_provenance_claim; carrier_claim;
                     profile_claim; target_claim; binding_claims; issuer_claim;
                     immutable_fingerprint_claim = ""; full_key = "";
                     checked_digest = "" })
  in
  result |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-descriptor-claim-issuance")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-descriptor-claim-issuance")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let decode_bool label = function
  | "true" -> Ok true
  | "false" -> Ok false
  | _ -> Error (label ^ " is not canonical")

let decode_base expected encoded =
  (match
     Numeric_receipt_private.decode
       ~schema:"verocaml.base-int-reference-claim.v1" ~field_count:3 encoded
   with
  | Error reason -> Error reason
  | Ok [ full_key; digest; outer ] ->
      if
        String.equal full_key expected.base_int_full_key
        && String.equal digest expected.validated_base_int_digest
        && String.equal outer expected.outer_artifact_binding_key_claim
        && String.equal encoded (base_material expected)
      then Ok expected
      else Error "base Int claim substitution"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-base-int-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-base-int-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_representation encoded =
  match encoded with
  | "immediate" -> Ok Immediate
  | "boxed" -> Ok Boxed
  | "unboxed" -> Ok Unboxed
  | _ -> (
      match
        Numeric_receipt_private.decode
          ~schema:"unsupported-representation-claim.v1" ~field_count:1 encoded
      with
      | Ok [ identity ] -> Ok (Unsupported identity)
      | Ok _ -> assert false
      | Error _ -> Error "unknown representation claim")

let decode_carrier encoded =
  (match
     Numeric_receipt_private.decode ~schema:"verocaml.numeric-carrier-claim.v1"
       ~field_count:7 encoded
   with
  | Error reason -> Error reason
  | Ok
      [ type_path_claim; type_uid_claim; constructor_abi_claim;
        binder_abi_claim; representation; arguments; instantiated ] ->
      let* representation_claim = decode_representation representation in
      let* type_argument_claims = Numeric_receipt_private.decode_list arguments in
      let* fully_instantiated_claim = decode_bool "instantiation claim" instantiated in
      let* value =
        carrier_claim ~type_path_claim ~type_uid_claim ~constructor_abi_claim
          ~binder_abi_claim ~representation_claim ~type_argument_claims
          ~fully_instantiated_claim
      in
      if String.equal encoded (carrier_material value) then Ok value
      else Error "carrier claim is not canonical"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-carrier-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-carrier-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_callable encoded =
  (match
     Numeric_receipt_private.decode ~schema:"verocaml.callable-abi-claim.v1"
       ~field_count:8 encoded
   with
  | Error reason -> Error reason
  | Ok
      [ callable_path_claim; callable_uid_claim; binder_abi_claim; parameters;
        result_abi_claim; total; effects; exceptions ] ->
      let* parameter_abi_claims = Numeric_receipt_private.decode_list parameters in
      let* total_claim = decode_bool "totality claim" total in
      let* effect_claims = Numeric_receipt_private.decode_list effects in
      let* exception_claims = Numeric_receipt_private.decode_list exceptions in
      let* value =
        callable_claim ~callable_path_claim ~callable_uid_claim ~binder_abi_claim
          ~parameter_abi_claims ~result_abi_claim ~total_claim ~effect_claims
          ~exception_claims
      in
      if String.equal encoded (callable_material value) then Ok value
      else Error "callable claim is not canonical"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-callable-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-callable-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_role encoded =
  match encoded with
  | "unsigned-view" -> Ok Unsigned_view
  | "signed-view" -> Ok Signed_view
  | "representable-bounds" -> Ok Representable_bounds
  | "checked-conversion" -> Ok Checked_conversion
  | "modular-conversion" -> Ok Modular_conversion
  | _ -> (
      match
        Numeric_receipt_private.decode
          ~schema:"partial-conversion-role-claim.v1" ~field_count:2 encoded
      with
      | Ok [ success_abi; failure_abi ] ->
          Ok (Partial_conversion { success_abi; failure_abi })
      | Ok _ -> assert false
      | Error _ -> (
          match
            Numeric_receipt_private.decode ~schema:"operation-role-claim.v1"
              ~field_count:2 encoded
          with
          | Ok [ role_schema; role_identity ] ->
              Ok (Operation { role_schema; role_identity })
          | Ok _ -> assert false
          | Error _ -> Error "unknown semantic role claim"))

let decode_semantics encoded =
  let width schema make =
    match Numeric_receipt_private.decode ~schema ~field_count:1 encoded with
    | Ok [ value ] -> (
        match int_of_string_opt value with
        | Some width -> Ok (make width)
        | None -> Error "invalid semantic width claim")
    | Ok _ -> assert false
    | Error reason -> Error reason
  in
  match encoded with
  | "checked-exact-claim-v1" -> Ok Checked_exact
  | "partial-exact-claim-v1" -> Ok Partial_exact
  | _ -> (
      match width "unsigned-range-claim.v1" (fun width -> Unsigned_range { width }) with
      | Ok _ as value -> value
      | Error _ -> (
          match width "signed-view-claim.v1" (fun width -> Signed_twos_complement { width }) with
          | Ok _ as value -> value
          | Error _ -> (
              match Numeric_receipt_private.decode ~schema:"bounds-claim.v1" ~field_count:2 encoded with
              | Ok [ minimum; maximum ] -> (
                  try Ok (Bounds { minimum = Z.of_string minimum; maximum = Z.of_string maximum })
                  with Invalid_argument _ -> Error "invalid bounds claim")
              | Ok _ -> assert false
              | Error _ -> (
                  match Numeric_receipt_private.decode ~schema:"modular-quotient-claim.v1" ~field_count:4 encoded with
                  | Ok [ width; _; _; _ ] -> (
                      match int_of_string_opt width with Some width -> Ok (Modular_quotient { width }) | None -> Error "invalid modular width claim")
                  | Ok _ -> assert false
                  | Error _ -> (
                      match Numeric_receipt_private.decode ~schema:"int-relation-claim.v1" ~field_count:1 encoded with
                      | Ok [ relation_full_key_claim ] -> Ok (Int_relation { relation_full_key_claim })
                      | Ok _ -> assert false
                      | Error _ -> Error "unknown Int semantics claim")))))

let decode_proof ~forbidden encoded =
  (match Numeric_receipt_private.decode ~schema:"verocaml.proof-receipt-claim.v1" ~field_count:2 encoded with
  | Error reason -> Error reason
  | Ok [ proof_full_key_claim; dependencies ] ->
      let* dependency_closure_claims = Numeric_receipt_private.decode_list dependencies in
      let* value = proof_claim ~proof_full_key_claim ~dependency_closure_claims ~forbidden_dependency_claims:forbidden in
      if String.equal encoded (proof_material value) then Ok value else Error "proof claim is not canonical"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-proof-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-proof-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_authority ~law encoded =
  if String.equal encoded "ordinary-law-claim-v1" then Ok Ordinary_specification_claim
  else match Numeric_receipt_private.decode ~schema:"kernel-law-claim.v1" ~field_count:1 encoded with
  | Ok [ dual_role_receipt_claim ] -> Ok (Kernel_schema_claim { dual_role_receipt_claim })
  | Ok _ -> assert false
  | Error _ -> (match Numeric_receipt_private.decode ~schema:"proved-law-claim.v1" ~field_count:1 encoded with
      | Ok [ proof ] -> let* proof = decode_proof ~forbidden:[ law ] proof in Ok (Proved_law_claim proof)
      | Ok _ -> assert false
      | Error _ -> (match Numeric_receipt_private.decode ~schema:"trusted-law-claim.v1" ~field_count:1 encoded with
          | Ok [ trust_receipt_claim ] -> Ok (Trusted_axiom_claim { trust_receipt_claim })
          | Ok _ -> assert false
          | Error _ -> Error "unknown law authority claim"))

let decode_refinement ~law encoded =
  if String.equal encoded "unknown-refinement-claim-v1" then Ok Unknown_refinement_claim
  else match Numeric_receipt_private.decode ~schema:"kernel-refinement-claim.v1" ~field_count:1 encoded with
  | Ok [ dual_role_receipt_claim ] -> Ok (Kernel_refinement_claim { dual_role_receipt_claim })
  | Ok _ -> assert false
  | Error _ -> (match Numeric_receipt_private.decode ~schema:"proved-refinement-claim.v1" ~field_count:3 encoded with
      | Ok [ proof; implementation_artifact_claim; implementation_body_claim ] ->
          let* proof_claim = decode_proof ~forbidden:[ law ] proof in
          Ok (Proved_refinement_claim { proof_claim; implementation_artifact_claim; implementation_body_claim })
      | Ok _ -> assert false
      | Error _ -> (match Numeric_receipt_private.decode ~schema:"trusted-refinement-claim.v1" ~field_count:3 encoded with
          | Ok [ trust_receipt_claim; implementation_artifact_claim; implementation_body_claim ] ->
              Ok (Trusted_refinement_claim { trust_receipt_claim; implementation_artifact_claim; implementation_body_claim })
          | Ok _ -> assert false
          | Error _ -> (match Numeric_receipt_private.decode ~schema:"tested-refinement-claim.v1" ~field_count:1 encoded with
              | Ok [ evidence_claim ] -> Ok (Tested_only_claim { evidence_claim })
              | Ok _ -> assert false
              | Error _ -> Error "unknown refinement claim")))

let decode_binding encoded =
  (match Numeric_receipt_private.decode ~schema:"verocaml.numeric-binding-claim.v1" ~field_count:9 encoded with
  | Error reason -> Error reason
  | Ok [ law_identity_claim; callable; role; semantics; authority; refinement; visibility; reveal; inline ] ->
      let* callable_claim = decode_callable callable in
      let* semantic_role = decode_role role in
      let* int_semantics = decode_semantics semantics in
      let* semantic_authority_claim = decode_authority ~law:law_identity_claim authority in
      let* implementation_refinement_claim = decode_refinement ~law:law_identity_claim refinement in
      let* visibility = match visibility with "visible" -> Ok Visible_body | "opaque" -> Ok Opaque_body | _ -> Error "unknown visibility claim" in
      let* reveal_permission = match reveal with "allowed" -> Ok Reveal_allowed | "forbidden" -> Ok Reveal_forbidden | _ -> Error "unknown reveal claim" in
      let* inline_permission = match inline with "allowed" -> Ok Inline_allowed | "forbidden" -> Ok Inline_forbidden | _ -> Error "unknown inline claim" in
      let* value = binding_claim ~law_identity_claim ~callable_claim ~semantic_role ~int_semantics ~semantic_authority_claim ~implementation_refinement_claim ~visibility ~reveal_permission ~inline_permission in
      if String.equal encoded (binding_claim_material value) then Ok value else Error "binding claim is not canonical"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-binding-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-binding-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let decode_claim ~(expected_profile_claim : Target_profile_private.claim)
    ~expected_base_int_claim encoded =
  (match Numeric_receipt_private.decode ~schema ~field_count:11 encoded with
  | Error reason -> Error reason
  | Ok [ base; provider_unit_claim; provider_origin_claim; provider_interface_receipt_claim; direct_import_provenance_claim; carrier; profile; target; bindings; issuer_claim; fingerprint ] ->
      let* base_int_claim = decode_base expected_base_int_claim base in
      if not (String.equal profile expected_profile_claim.full_key) then Error "profile claim substitution"
      else
        let* carrier_claim = decode_carrier carrier in
        let* target_claim = Target_profile_private.decode_instance_claim ~profile:expected_profile_claim target in
        let* encoded_bindings = Numeric_receipt_private.decode_list bindings in
        let rec loop values = function [] -> Ok (List.rev values) | item :: rest -> let* value = decode_binding item in loop (value :: values) rest in
        let* binding_claims = loop [] encoded_bindings in
        let* value = issue_claim ~base_int_claim ~provider_unit_claim ~provider_origin_claim ~provider_interface_receipt_claim ~direct_import_provenance_claim ~carrier_claim ~profile_claim:expected_profile_claim ~target_claim ~binding_claims ~issuer_claim in
        if String.equal fingerprint value.immutable_fingerprint_claim && String.equal encoded value.full_key then Ok value
        else Error "numeric descriptor claim is noncanonical or substituted"
  | Ok _ -> assert false)
  |> fun result ->
  (match result with
  | Ok _ ->
      [%log.trace "validated unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-descriptor-claim-decode")
        ~authority:(Delator.Field.string "none-claim-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unauthenticated canonical claim"
        ~stage:(Delator.Field.string "numeric-descriptor-claim-decode")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result

let compare_claim left right = String.compare left.full_key right.full_key
let equal_claim left right = compare_claim left right = 0
