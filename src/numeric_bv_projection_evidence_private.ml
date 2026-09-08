type semantic_authority = Checked_proof | Explicit_axiom
type provenance = Imported_registry | Same_unit_prior_closure
type trusted_dependency_origin = Imported_law | Same_unit_law
type trusted_dependency = {
  origin : trusted_dependency_origin;
  compiler_identity : string;
  evidence_full_key : string;
}

type operation_observation = {
  descriptor_origin : string;
  occurrence_identity : string;
  call_span : Diagnostic.span;
  callable_uid : string;
  callable_tag : string;
  callable_abi : string;
  law_evidence_full_key : string;
  authority : semantic_authority;
}

type runtime_equivalence = Runtime_unknown

type source_observation = {
  occurrence_identity : string;
  semantic_width : Bv_width.t;
  provenance : provenance;
  trusted_dependencies : trusted_dependency list;
  outer_operation : operation_observation;
  modular_operation : operation_observation;
  carrier_binding_full_key : string;
  base_int_full_key : string;
  source_witness_full_key : string;
  profile_full_key : string;
  target_full_key : string;
  backend_capability_full_key : string;
  backend_capability_id : string;
  backend_abi_receipt : string;
  runtime_equivalence : runtime_equivalence;
}

type t = {
  target : Build_target_profile_private.instance;
  width : Bv_width.t;
  provenance : provenance;
  trusted_dependencies : trusted_dependency list;
  source_observation : source_observation;
  occurrence_full_key : string;
  full_key : string;
}

let authority_key = function
  | Checked_proof -> "checked-proof"
  | Explicit_axiom -> "explicit-axiom"

let provenance_key = function
  | Imported_registry -> "imported-registry"
  | Same_unit_prior_closure -> "same-unit-prior-closure"

let trusted_origin_key = function
  | Imported_law -> "imported-law"
  | Same_unit_law -> "same-unit-law"

let trusted_dependency_key dependency =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-trusted-dependency.v1"
    [ trusted_origin_key dependency.origin; dependency.compiler_identity;
      dependency.evidence_full_key ]

let span_key (span : Diagnostic.span) =
  Numeric_receipt_private.encode ~schema:"verocaml.source-span.v1"
    [ span.file; string_of_int span.start_pos.line;
      string_of_int span.start_pos.column; string_of_int span.end_pos.line;
      string_of_int span.end_pos.column ]

let operation_key operation =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-source-operation.v1"
    [ operation.descriptor_origin; operation.occurrence_identity;
      span_key operation.call_span; operation.callable_uid;
      operation.callable_tag; operation.callable_abi;
      operation.law_evidence_full_key; authority_key operation.authority ]

let source_observation_key observation =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-source-observation.v1"
    [ observation.occurrence_identity;
      Bv_width.to_string observation.semantic_width;
      provenance_key observation.provenance;
      Numeric_receipt_private.list
        (List.map trusted_dependency_key observation.trusted_dependencies);
      operation_key observation.outer_operation;
      operation_key observation.modular_operation;
      observation.carrier_binding_full_key; observation.base_int_full_key;
      observation.source_witness_full_key; observation.profile_full_key;
      observation.target_full_key; observation.backend_capability_full_key;
      observation.backend_capability_id;
      observation.backend_abi_receipt;
      (match observation.runtime_equivalence with
      | Runtime_unknown -> "runtime-equivalence-unknown") ]

let current_backend_observation () =
  let capability = Bv_backend_capability_receipt_private.capability () in
  match Bv_backend_capability_receipt_private.authenticate capability with
  | Error message -> invalid_arg message
  | Ok (receipt, _) ->
      let backend_abi_receipt =
        Numeric_receipt_private.encode
          ~schema:"verocaml.bv-backend-abi-observation.v1"
          [ receipt.build_compatibility; receipt.ocaml_abi; receipt.z3_package;
            string_of_int receipt.c_unsigned_width ]
      in
      ( receipt.full_key,
        Bv_backend_capability_receipt_private.id_to_hex receipt.checked_id,
        backend_abi_receipt )

let identity ~target ~width ~provenance ~trusted_dependencies
    ~source_observation ~occurrence_full_key =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-projection-evidence.v3"
    [ target.Build_target_profile_private.full_key; Bv_width.to_string width;
      Bv_width.profile_full_key width;
      Option.value ~default:"" (Bv_width.target_full_key width);
      provenance_key provenance;
      Numeric_receipt_private.list
        (List.map trusted_dependency_key trusted_dependencies);
      source_observation_key source_observation; occurrence_full_key ]

module For_source_admission = struct
  let trusted_dependency ~origin ~compiler_identity ~evidence_full_key =
    { origin; compiler_identity; evidence_full_key }

  let operation ~descriptor_origin ~occurrence_identity ~call_span ~callable_uid
      ~callable_tag ~callable_abi ~law_evidence_full_key ~authority =
    { descriptor_origin; occurrence_identity; call_span; callable_uid;
      callable_tag; callable_abi; law_evidence_full_key; authority }

  let issue ~(target : Build_target_profile_private.instance) ~width ~provenance
      ~trusted_dependencies
      ~occurrence_identity ~outer_operation ~modular_operation
      ~carrier_binding_full_key ~base_int_full_key ~source_witness_full_key
      ~occurrence_full_key =
    let trusted_dependencies =
      List.sort_uniq
        (fun left right ->
          String.compare (trusted_dependency_key left)
            (trusted_dependency_key right))
        trusted_dependencies
    in
    let backend_capability_full_key, backend_capability_id, backend_abi_receipt =
      current_backend_observation ()
    in
    let source_observation =
      { occurrence_identity; semantic_width = width; provenance;
        trusted_dependencies; outer_operation; modular_operation;
        carrier_binding_full_key; base_int_full_key;
        source_witness_full_key;
        profile_full_key = Bv_width.profile_full_key width;
        target_full_key = target.full_key; backend_capability_full_key;
        backend_capability_id;
        backend_abi_receipt; runtime_equivalence = Runtime_unknown }
    in
    let full_key =
      identity ~target ~width ~provenance ~trusted_dependencies
        ~source_observation ~occurrence_full_key
    in
    { target; width; provenance; trusted_dependencies; source_observation;
      occurrence_full_key; full_key }
end

let validate ~width evidence =
  let result =
    let ( let* ) = Result.bind in
    let capability = Bv_backend_capability_receipt_private.capability () in
    let build_capability = Build_target_profile_private.capability () in
    let* instances =
      Build_target_profile_private.authenticate_instances build_capability
    in
    let* () =
      if
        List.exists
          (fun instance ->
            String.equal instance.Build_target_profile_private.full_key
              evidence.target.full_key)
          instances
      then Ok ()
      else Error "BV source evidence target is not a sealed build instance"
    in
    let exact_original_target label candidate =
      match Bv_width.target_full_key candidate with
      | Some target_full_key
        when String.equal target_full_key evidence.target.full_key ->
          Ok ()
      | Some _ ->
          Error (label ^ " belongs to another exact target before translation")
      | None ->
          Error (label ^ " is not target-bound before translation")
    in
    let* () = exact_original_target "BV translation width" width in
    let* () =
      exact_original_target "BV source evidence width" evidence.width
    in
    let* _ = Bv_width.for_instance capability evidence.target width in
    let* _ =
      Bv_width.for_instance capability evidence.target evidence.width
    in
    let* () =
      if
        Bv_width.equal width evidence.width
      then Ok ()
      else Error "BV source evidence width changed before translation"
    in
    let backend_capability_full_key, backend_capability_id, backend_abi_receipt =
      current_backend_observation ()
    in
    let* () =
      if
        Bv_width.equal evidence.source_observation.semantic_width evidence.width
        && evidence.source_observation.provenance = evidence.provenance
        && evidence.source_observation.trusted_dependencies
           = evidence.trusted_dependencies
        && String.equal evidence.source_observation.profile_full_key
          (Bv_width.profile_full_key evidence.width)
        && String.equal evidence.source_observation.target_full_key
             evidence.target.full_key
        && String.equal
             evidence.source_observation.backend_capability_full_key
             backend_capability_full_key
        && String.equal evidence.source_observation.backend_capability_id
             backend_capability_id
        && String.equal evidence.source_observation.backend_abi_receipt
             backend_abi_receipt
      then Ok ()
      else Error "BV source evidence build provenance changed before translation"
    in
    let expected =
      identity ~target:evidence.target ~width:evidence.width
        ~provenance:evidence.provenance
        ~trusted_dependencies:evidence.trusted_dependencies
        ~source_observation:evidence.source_observation
        ~occurrence_full_key:evidence.occurrence_full_key
    in
    if String.equal expected evidence.full_key then Ok ()
    else Error "BV source evidence identity changed before translation"
  in
  let[@log_value.trace] reason_class =
    match result with
    | Ok () -> "exact-target-bound-authority"
    | Error _ -> "target-binding-or-identity-rejected"
  in
  [%log.trace "validated typed source authority for BV translation"
    ~stage:(Delator.Field.string "numeric-bv-projection-evidence")
    ~semantic_width:(Delator.Field.int (Bv_width.to_int width))
    ~provenance:(Delator.Field.string (provenance_key evidence.provenance))
    ~semantic_authorities:(Delator.Field.int 2)
    ~trusted_dependencies:
      (Delator.Field.int (List.length evidence.trusted_dependencies))
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason_class:
      (Delator.Field.string (reason_class [@log_value.trace]))
    ~decision:
      (Delator.Field.string
         (match result with Ok () -> "translate" | Error _ -> "reject"))];
  result
[@@delator.instrument] [@@delator.level trace]

let full_key evidence = evidence.full_key
let provenance evidence = evidence.provenance
let semantic_authorities evidence =
  ( evidence.source_observation.outer_operation.authority,
    evidence.source_observation.modular_operation.authority )
let trusted_dependencies evidence = evidence.trusted_dependencies
let source_observation evidence = evidence.source_observation
