type imported_projection = {
  issuer_unit : string;
  outer_callable_uid : string;
  modular_callable_uid : string;
  outer_callable_abi : string;
  modular_callable_abi : string;
  outer_law_full_key : string;
  modular_law_full_key : string;
  carrier_binding_full_key : string;
  base_int_full_key : string;
  semantic_width : int;
  outer_authority : Numeric_bv_projection_evidence_private.semantic_authority;
  modular_authority : Numeric_bv_projection_evidence_private.semantic_authority;
  trusted_dependencies :
    Numeric_bv_projection_evidence_private.trusted_dependency list;
}

type request = {
  consumer_artifact_full_key : string;
  target : Build_target_profile_private.instance;
  projections : imported_projection list;
  registry_full_key : string option;
  local_base_providers : Cmt_input.implementation list;
  full_key : string;
}

type local_candidate = {
  outer_definition : Sst.function_definition;
  modular_definition : Sst.function_definition;
  outer_law_root : Sst.function_definition;
  modular_law_root : Sst.function_definition;
  outer_law_is_explicit_axiom : bool;
  modular_law_is_explicit_axiom : bool;
  descriptor_origin : string;
  outer_callable_uid : string;
  modular_callable_uid : string;
  outer_callable_abi : string;
  modular_callable_abi : string;
  semantic_width : int;
  carrier_binding_full_key : string;
  base_int_full_key : string;
  statement_full_key : string;
}

type active_local = {
  candidate : local_candidate;
  outer_authority_full_key : string;
  modular_authority_full_key : string;
  trusted_dependencies :
    Numeric_bv_projection_evidence_private.trusted_dependency list;
  authority_full_key : string;
}

type prior_law_schedule = {
  dependencies : (int * int list) list;
  bootstrap_functions : int list;
}

type source = {
  implementation : Cmt_input.implementation;
  validated : Sst_validation.validated_program;
  registration : Imported_callable.registration option;
  request : request;
  local_candidates : local_candidate list;
  prior_law_schedule : prior_law_schedule;
  mutable active_local : active_local list;
  full_key : string;
}

type projection = {
  input : Sst.expression;
  width : Bv_width.t;
  authority : Numeric_bv_projection_evidence_private.t;
}

let ( let* ) = Result.bind
let artifact_key = Imported_callable.artifact_full_key

let projection_key claim =
  let trusted_dependency_key dependency =
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-bv-source-trusted-dependency.v1"
      [ dependency.Numeric_bv_projection_evidence_private.compiler_identity;
        dependency.evidence_full_key ]
  in
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-bv-projection-claim.v3"
    [ claim.issuer_unit; claim.outer_callable_uid;
      claim.modular_callable_uid; claim.outer_callable_abi;
      claim.modular_callable_abi; claim.outer_law_full_key;
      claim.modular_law_full_key; claim.carrier_binding_full_key;
      claim.base_int_full_key; string_of_int claim.semantic_width;
      (match claim.outer_authority with Checked_proof -> "checked-proof" | Explicit_axiom -> "explicit-axiom");
      (match claim.modular_authority with Checked_proof -> "checked-proof" | Explicit_axiom -> "explicit-axiom");
      Numeric_receipt_private.list
        (List.map trusted_dependency_key claim.trusted_dependencies) ]

let make_request ~consumer_artifact_full_key
    ~(target : Build_target_profile_private.instance) ~projections
    ~registry_full_key ~local_base_providers =
  let projections =
    List.sort_uniq
      (fun left right -> String.compare (projection_key left) (projection_key right))
      projections
  in
  let base_providers =
    local_base_providers
    |> List.sort_uniq (fun left right ->
           String.compare (artifact_key left) (artifact_key right))
  in
  let full_key =
    Numeric_receipt_private.encode
      ~schema:"verocaml.numeric-bv-source-request.v3"
      [ consumer_artifact_full_key; target.full_key;
        Option.value ~default:"" registry_full_key;
        Numeric_receipt_private.list (List.map projection_key projections);
        Numeric_receipt_private.list (List.map artifact_key base_providers) ]
  in
  { consumer_artifact_full_key; target; projections; registry_full_key;
    local_base_providers = base_providers; full_key }

let evidence_authority = function
  | Numeric_bv_imported_law_evidence_private.Checked_proof ->
      Numeric_bv_projection_evidence_private.Checked_proof
  | Explicit_axiom -> Explicit_axiom

module For_registry = struct
  let request ~consumer_artifact_full_key ~registry_full_key
      ~(target : Build_target_profile_private.instance) ~projections =
    let projection evidence =
      let* () =
        Numeric_bv_imported_law_evidence_private.validate ~target evidence
      in
      let trusted_dependencies =
        Numeric_bv_imported_law_evidence_private.trusted_dependencies evidence
        |> List.map (fun dependency ->
               Numeric_bv_projection_evidence_private.For_source_admission
               .trusted_dependency ~origin:Imported_law
                 ~compiler_identity:
                   dependency.Numeric_bv_imported_law_evidence_private
                   .compiler_identity
                 ~evidence_full_key:
                   dependency.Numeric_bv_imported_law_evidence_private
                   .evidence_full_key)
      in
      Ok
        { issuer_unit =
            Numeric_bv_imported_law_evidence_private.issuer_unit evidence;
          outer_callable_uid =
            Numeric_bv_imported_law_evidence_private.outer_callable_uid evidence;
          modular_callable_uid =
            Numeric_bv_imported_law_evidence_private.modular_callable_uid evidence;
          outer_callable_abi =
            Numeric_bv_imported_law_evidence_private.outer_callable_abi evidence;
          modular_callable_abi =
            Numeric_bv_imported_law_evidence_private.modular_callable_abi evidence;
          outer_law_full_key =
            Numeric_bv_imported_law_evidence_private.outer_law_full_key evidence;
          modular_law_full_key =
            Numeric_bv_imported_law_evidence_private.modular_law_full_key evidence;
          carrier_binding_full_key =
            Numeric_bv_imported_law_evidence_private.carrier_binding_full_key evidence;
          base_int_full_key =
            Numeric_bv_imported_law_evidence_private.base_int_full_key evidence;
          semantic_width =
            Numeric_bv_imported_law_evidence_private.semantic_width evidence;
          outer_authority =
            evidence_authority
              (Numeric_bv_imported_law_evidence_private.outer_authority evidence);
          modular_authority =
            evidence_authority
              (Numeric_bv_imported_law_evidence_private.modular_authority evidence);
          trusted_dependencies }
    in
    let* claims =
      List.fold_left
        (fun result pair ->
          let* claims = result in
          let* claim = projection pair in
          Ok (claim :: claims))
        (Ok []) projections
    in
    if claims = [] then
      Error "numeric BV source request has no exact imported projection laws"
    else
      Ok
        (make_request ~consumer_artifact_full_key ~target ~projections:claims
           ~registry_full_key:(Some registry_full_key)
           ~local_base_providers:[])
end

let with_local_provider ~imported ~implementation ~base_providers
    ~(target : Build_target_profile_private.instance) =
  let local_roles =
    implementation.Cmt_input.interface_numeric_claims.numeric_roles
    |> List.filter (fun role ->
             String.equal role.Cmt_input.numeric_role_semantics_owner_unit
             implementation.Cmt_input.unit_name)
  in
  match imported, local_roles with
  | None, [] -> Ok None
  | Some request, [] ->
      if String.equal request.target.full_key target.full_key then Ok (Some request)
      else Error "numeric BV source request target changed before source admission"
  | imported, _ :: _ ->
      let consumer_artifact_full_key = artifact_key implementation in
      let* projections, registry_full_key =
        match imported with
        | None -> Ok ([], None)
        | Some request ->
            if
              String.equal request.consumer_artifact_full_key
                consumer_artifact_full_key
              && String.equal request.target.full_key target.full_key
            then Ok (request.projections, request.registry_full_key)
            else
              Error
                "numeric BV imported and local source requests do not share one exact consumer and target"
      in
      let providers = implementation :: base_providers in
      Ok
        (Some
           (make_request ~consumer_artifact_full_key ~target ~projections
              ~registry_full_key ~local_base_providers:providers))

let exact_target (target : Build_target_profile_private.instance) =
  let capability = Build_target_profile_private.capability () in
  let* instances = Build_target_profile_private.authenticate_instances capability in
  if
    List.exists
      (fun instance ->
        String.equal instance.Build_target_profile_private.full_key target.full_key)
      instances
  then Ok ()
  else Error "numeric source target is not an exact sealed build instance"

let local_carrier_and_base implementation providers role =
  let retained =
    Cmt_input.retained_numeric_claims
      implementation.Cmt_input.interface_numeric_claims
  in
  let* matching_carriers =
    List.fold_left
      (fun result encoded ->
        let* matches = result in
        let* candidate = Numeric_interface_claim_private.decode_carrier encoded in
        if
          String.equal candidate.carrier_uid
            role.Cmt_input.numeric_role_carrier_uid
          && String.equal candidate.owner.owner_cmi_full_key
               role.numeric_role_carrier_owner_cmi_full_key
        then Ok (candidate :: matches)
        else Ok matches)
      (Ok []) retained.carrier_reconstruction_claims
  in
  let* carrier_claim =
    match matching_carriers with
    | [ claim ] -> Ok claim
    | _ -> Error "numeric source law has no unique exact carrier claim"
  in
  let* artifact =
    Numeric_artifact_binding_private.correlate implementation carrier_claim
  in
  let* carrier = Numeric_artifact_binding_private.facts artifact in
  let* base_reference =
    match carrier_claim.base_reference with
    | Some reference -> Ok reference
    | None ->
        Error
          "native numeric modular projection requires an exact mathematical base reference"
  in
  let* _provider, _logical_sort, base =
    Numeric_base_int_binding_private.resolve ~providers base_reference
  in
  Ok (carrier, base)

let role_is expected (role : Cmt_input.interface_numeric_role) =
  String.equal role.numeric_role_source.role_schema "numeric-role.v1"
  && String.equal role.numeric_role_source.role_identity expected

let unary_specification_call definition expression =
  match expression.Sst.expression_desc with
  | Direct_call
      { call_form = Specification_call; callee; type_arguments = [];
        arguments = [ Value_argument { label = None; value } ];
        recursive = false }
    when callee = definition.Sst.function_id ->
      Some value
  | _ -> None

let candidate_input candidate expression =
  match unary_specification_call candidate.outer_definition expression with
  | Some inner -> (
      match unary_specification_call candidate.modular_definition inner with
      | Some input when input.Sst.typ = Sst.Mathematical_int ->
          Some (inner, input)
      | Some _ | None -> None)
  | None -> None

let local_candidates ~implementation ~validated request =
  let* declarations =
    Numeric_semantics_correlation_private.correlate_local ~implementation
      ~validated
  in
  let unsigned =
    declarations
    |> List.filter_map
         (fun (declaration : Numeric_semantics_correlation_private.t) ->
           if
             role_is "unsigned-view" declaration.role
             && declaration.role.numeric_role_callable_domain
                = Numeric_callable_domain_private.Exact_carrier
           then
             Option.map
               (fun statement -> (declaration, statement))
               (Numeric_law_match_private.unsigned_range ~target:request.target
                  declaration)
           else None)
    |> List.filter (fun (_, (statement : Numeric_law_match_private.unsigned_range)) ->
           statement.Numeric_law_match_private.result_interpretation
           = Numeric_law_match_private.Mathematical_result)
  in
  let candidates =
    List.fold_left
      (fun result
           ((outer_declaration : Numeric_semantics_correlation_private.t),
            (unsigned_statement : Numeric_law_match_private.unsigned_range)) ->
        let* accumulated = result in
        let outer_role = outer_declaration.role in
        let* carrier, base =
          local_carrier_and_base implementation request.local_base_providers
            outer_role
        in
        let matching_modular =
          declarations
          |> List.filter
               (fun (declaration : Numeric_semantics_correlation_private.t) ->
                 let role = declaration.Numeric_semantics_correlation_private.role in
                 role_is "modular-conversion" role
                 && String.equal role.numeric_role_carrier_uid
                      outer_role.numeric_role_carrier_uid
                 && String.equal role.numeric_role_carrier_owner_cmi_full_key
                      outer_role.numeric_role_carrier_owner_cmi_full_key)
          |> List.filter_map
               (fun (declaration : Numeric_semantics_correlation_private.t) ->
                 match
                   Numeric_relation_match_private.match_relation
                     ~kind:Numeric_relation_match_private.Modular
                     ~base_full_key:base.base_int_full_key ~target:request.target
                     ~semantic_width:unsigned_statement.semantic_width
                     ~signed:false ~view:unsigned_statement.view ~bounds:None
                     declaration
                 with
                 | Some relation when relation.required_guards = [] ->
                     Some (declaration, relation)
                 | Some _ | None -> None)
        in
        let additions =
          matching_modular
          |> List.filter_map
               (fun
                 ((modular_declaration :
                    Numeric_semantics_correlation_private.t),
                  (relation : Numeric_relation_match_private.t)) ->
                 match modular_declaration.callable_definition with
                 | None -> None
                 | Some modular_definition ->
                     let statement_full_key =
                       Numeric_receipt_private.encode
                         ~schema:
                           "verocaml.numeric-local-bv-projection-statement.v1"
                         [ artifact_key implementation; request.target.full_key;
                           outer_role.numeric_role_callable_uid;
                           modular_declaration.role.numeric_role_callable_uid;
                           carrier.binding_full_key; base.base_int_full_key;
                           unsigned_statement.semantic_width_evidence;
                           relation.material ]
                     in
                     Some
                         { outer_definition = unsigned_statement.view;
                           modular_definition;
                         outer_law_root = outer_declaration.definition;
                         modular_law_root = modular_declaration.definition;
                         outer_law_is_explicit_axiom =
                           outer_declaration.kind
                           = Numeric_semantics_correlation_private
                             .Explicit_axiom_declaration;
                           modular_law_is_explicit_axiom =
                           modular_declaration.kind
                           = Numeric_semantics_correlation_private
                             .Explicit_axiom_declaration;
                         descriptor_origin = implementation.unit_name;
                         outer_callable_uid =
                           outer_role.numeric_role_callable_uid;
                         modular_callable_uid =
                           modular_declaration.role.numeric_role_callable_uid;
                         outer_callable_abi =
                           outer_role.numeric_role_callable_abi;
                         modular_callable_abi =
                           modular_declaration.role.numeric_role_callable_abi;
                         semantic_width = unsigned_statement.semantic_width;
                         carrier_binding_full_key = carrier.binding_full_key;
                         base_int_full_key = base.base_int_full_key;
                         statement_full_key })
        in
        Ok (List.rev_append additions accumulated))
      (Ok []) unsigned
  in
  Result.map
    (fun candidates ->
      let program = Sst_validation.program validated in
      let canonical definition =
        List.find
          (fun candidate ->
            candidate.Sst.function_id = definition.Sst.function_id)
          program.functions
      in
      candidates
      |> List.map (fun candidate ->
             { candidate with
               outer_definition = canonical candidate.outer_definition;
               modular_definition = canonical candidate.modular_definition;
               outer_law_root = canonical candidate.outer_law_root;
               modular_law_root = canonical candidate.modular_law_root })
      |> List.sort_uniq (fun left right ->
             String.compare left.statement_full_key right.statement_full_key))
    candidates

let scheduling_expression_roots (definition : Sst.function_definition) =
  let predicates clauses =
    List.map
      (fun (clause : Sst.predicate_clause) -> clause.predicate.expression)
      clauses
  in
  let body =
    match definition.body with
    | Sst.Spec_definition body | Proof_body { body; _ }
    | Checked_exec { body; _ } ->
        [ body.expression ]
    | Recursive_spec_definition _ | External_specification _
    | Trusted_external_spec_target _ | Trusted_external_body _
    | Symbolic_declaration _ -> []
  in
  body
  @ predicates definition.contracts.requires
  @ predicates definition.contracts.assertions
  @ predicates definition.contracts.decreases
  @ List.map
      (fun (clause : Sst.ensures_clause) -> clause.predicate.expression)
      definition.contracts.ensures

let expression_contains_candidate candidate expression =
  let rec contains expression =
    Option.is_some (candidate_input candidate expression)
    || List.exists contains
         (Sst_callback_private.expression_children expression)
  in
  contains expression

let prior_law_schedule ~validated candidates =
  let program = Sst_validation.program validated in
  let calls = Sst_validation.call_edge_descriptors validated in
  let direct_dependencies definition =
    calls
    |> List.filter_map (fun edge ->
           let caller =
             Sst_validation.callable_definition
               (Sst_validation.call_edge_caller edge)
           in
           let callee =
             Sst_validation.callable_definition
               (Sst_validation.call_edge_callee edge)
           in
           if
             caller == definition
             && List.exists (fun item -> item == callee) program.functions
           then Some callee
           else None)
    |> List.sort_uniq (fun left right ->
           Int.compare left.Sst.function_id.function_index
             right.Sst.function_id.function_index)
  in
  let rec visit active completed edges definition =
    let index = definition.Sst.function_id.function_index in
    if List.mem index completed then Ok (completed, edges)
    else if List.mem index active then
      Error "cyclic ordinary prior-law dependency"
    else
      let dependencies = direct_dependencies definition in
      let* completed, edges =
        List.fold_left
          (fun result dependency ->
            let* completed, edges = result in
            visit (index :: active) completed edges dependency)
          (Ok (completed, edges)) dependencies
      in
      Ok
        ( index :: completed,
          ( index,
            List.map
              (fun dependency -> dependency.Sst.function_id.function_index)
              dependencies )
          :: edges )
  in
  let plan_candidate candidate =
    let* completed, edges =
      visit [] [] [] candidate.outer_law_root
    in
    let* completed, edges =
      visit [] completed edges candidate.modular_law_root
    in
    Ok (candidate, List.sort_uniq Int.compare completed, edges)
  in
  let planned, _cyclic =
    List.fold_left
      (fun (planned, cyclic) candidate ->
        match plan_candidate candidate with
        | Ok plan -> (plan :: planned, cyclic)
        | Error _ -> (planned, candidate :: cyclic))
      ([], []) candidates
  in
  let dependency_edges =
    List.concat_map (fun (_, _, edges) -> edges) planned
  in
  let consumer_edges =
    program.functions
    |> List.filter_map (fun definition ->
           let index = definition.Sst.function_id.function_index in
           let dependencies =
             planned
             |> List.concat_map (fun (candidate, bootstrap, _) ->
                    if
                      List.mem index bootstrap
                      || not
                           (List.exists
                              (expression_contains_candidate candidate)
                              (scheduling_expression_roots definition))
                    then []
                    else
                      [ candidate.outer_law_root.function_id.function_index;
                        candidate.modular_law_root.function_id.function_index ])
             |> List.sort_uniq Int.compare
           in
           if dependencies = [] then None else Some (index, dependencies))
  in
  let dependencies =
    dependency_edges @ consumer_edges
    |> List.fold_left
         (fun combined (index, additions) ->
           let retained = List.remove_assoc index combined in
           let existing = Option.value ~default:[] (List.assoc_opt index combined) in
           (index, List.sort_uniq Int.compare (additions @ existing)) :: retained)
         []
    |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
  in
  let bootstrap_functions =
    planned
    |> List.concat_map (fun (_, bootstrap, _) -> bootstrap)
    |> List.sort_uniq Int.compare
  in
  [%log.debug "planned dependency-safe ordinary numeric law bootstrap"
    ~stage:(Delator.Field.string "numeric-prior-law-scheduling")
    ~candidate_laws:(Delator.Field.int (List.length candidates * 2))
    ~planned_statements:(Delator.Field.int (List.length planned))
    ~cyclic_statements:(Delator.Field.int (List.length _cyclic))
    ~bootstrap_functions:(Delator.Field.int (List.length bootstrap_functions))
    ~consumer_edges:(Delator.Field.int (List.length consumer_edges))
    ~decision:
      (Delator.Field.string
         (if planned = [] then "ordinary-source-order" else "law-first"))];
  { dependencies; bootstrap_functions }
[@@delator.instrument] [@@delator.level debug]

let explicit_axiom_authority source_full_key candidate root =
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-local-explicit-axiom-authority.v1"
    [ source_full_key; candidate.statement_full_key;
      string_of_int root.Sst.function_id.function_index;
      root.function_id.function_name;
      Sst.to_string
        { Sst.policy = root.policy; types = []; parametric_adts = [];
          functions = [ root ]; logical_constants = [] } ]

let create ~implementation ~validated ~registration request =
  let program = Sst_validation.program validated in
  let result =
    let* () =
      if
        String.equal (artifact_key implementation)
          request.consumer_artifact_full_key
      then Ok ()
      else Error "numeric BV source request belongs to a different consumer artifact"
    in
    let* () = exact_target request.target in
    let* local_candidates =
      if request.local_base_providers = [] then Ok []
      else local_candidates ~implementation ~validated request
    in
    let prior_law_schedule =
      prior_law_schedule ~validated local_candidates
    in
    let* () =
      if request.projections <> [] && Option.is_none registration then
        Error "native imported numeric source admission requires authenticated calls"
      else Ok ()
    in
    if request.projections = [] && local_candidates = [] then Ok None
    else
      let full_key =
        Numeric_receipt_private.encode
          ~schema:"verocaml.numeric-bv-source-witness.v3"
          [ request.full_key; artifact_key implementation;
            string_of_int (List.length program.functions);
            Numeric_receipt_private.list
              (List.map (fun candidate -> candidate.statement_full_key)
                 local_candidates);
            Numeric_receipt_private.list
              (List.map
                 (fun (index, dependencies) ->
                   Numeric_receipt_private.encode
                     ~schema:"verocaml.numeric-prior-law-schedule-edge.v1"
                     [ string_of_int index;
                       Numeric_receipt_private.list
                         (List.map string_of_int dependencies) ])
                 prior_law_schedule.dependencies) ]
      in
      let active_local =
        local_candidates
        |> List.filter_map (fun candidate ->
               if
                 candidate.outer_law_is_explicit_axiom
                 && candidate.modular_law_is_explicit_axiom
               then
                 let outer_authority_full_key =
                   explicit_axiom_authority full_key candidate
                     candidate.outer_law_root
                 and modular_authority_full_key =
                   explicit_axiom_authority full_key candidate
                     candidate.modular_law_root
                 in
                 let authority_full_key =
                   Numeric_receipt_private.encode
                     ~schema:
                       "verocaml.numeric-local-bv-projection-authority.v2"
                     [ full_key; candidate.statement_full_key;
                       outer_authority_full_key;
                       modular_authority_full_key ]
                 in
                 Some
                   { candidate; outer_authority_full_key;
                     modular_authority_full_key; trusted_dependencies = [];
                     authority_full_key }
               else None)
      in
      Ok
        (Some
           { implementation; validated; registration; request; local_candidates;
             prior_law_schedule; active_local; full_key })
  in
  [%log.debug "issued pre-solver numeric BV source witness"
    ~stage:(Delator.Field.string "numeric-bv-source-admission")
    ~consumer:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~target_width:(Delator.Field.int request.target.target_claim.width)
    ~imported_projections:
      (Delator.Field.int (List.length request.projections))
    ~local_projection_statements:
      (Delator.Field.int
         (match result with
         | Ok (Some source) -> List.length source.local_candidates
         | Ok None | Error _ -> 0))
    ~proof_success:(Delator.Field.bool false)
    ~admitted:
      (Delator.Field.bool
         (match result with Ok (Some _) -> true | Ok None | Error _ -> false))
    ~decision:
      (Delator.Field.string
         (match result with
         | Ok (Some _) -> "source-identity"
         | Ok None -> "ordinary-source-semantics"
         | Error _ -> "rejected"))];
  result
[@@delator.instrument] [@@delator.level debug]

let observe_prior_law source coordinator ~definition =
  let law_authority candidate ~explicit_axiom root =
    if explicit_axiom then
      Ok (explicit_axiom_authority source.full_key candidate root, [])
    else
      let* closure =
        Numeric_prior_law_closure_private.For_pipeline.complete coordinator
          ~root
      in
      if
        Numeric_prior_law_closure_private.matches closure
          ~implementation:source.implementation ~validated:source.validated
        && Numeric_prior_law_closure_private.authorizes_definition closure root
      then
        let trusted_dependencies =
          Numeric_prior_law_closure_private.dependency_evidence closure
          |> List.filter_map (fun dependency ->
                 if dependency.Numeric_proof_closure_walk_private.trusted then
                   Some
                     (Numeric_bv_projection_evidence_private
                      .For_source_admission.trusted_dependency
                        ~origin:Same_unit_law
                        ~compiler_identity:dependency.compiler_uid
                        ~evidence_full_key:dependency.full_key)
                 else None)
        in
        Ok
          ( Numeric_prior_law_closure_private.full_key closure,
            trusted_dependencies )
      else Error "prior-law closure belongs to another exact source"
  in
  let activate candidate =
    if
      List.exists
        (fun active ->
          String.equal active.candidate.statement_full_key
            candidate.statement_full_key)
        source.active_local
    then ()
    else
      match
        ( law_authority candidate
            ~explicit_axiom:candidate.outer_law_is_explicit_axiom
            candidate.outer_law_root,
          law_authority candidate
            ~explicit_axiom:candidate.modular_law_is_explicit_axiom
            candidate.modular_law_root )
      with
      | Ok (outer_authority_full_key, outer_trusted_dependencies),
        Ok (modular_authority_full_key, modular_trusted_dependencies) ->
          let trusted_dependencies =
            outer_trusted_dependencies @ modular_trusted_dependencies
            |> List.sort_uniq (fun left right ->
                   compare
                     ( left.Numeric_bv_projection_evidence_private
                       .compiler_identity,
                       left.evidence_full_key )
                     ( right.Numeric_bv_projection_evidence_private
                       .compiler_identity,
                       right.evidence_full_key ))
          in
          let authority_full_key =
            Numeric_receipt_private.encode
              ~schema:"verocaml.numeric-local-bv-projection-authority.v2"
              [ source.full_key; candidate.statement_full_key;
                outer_authority_full_key; modular_authority_full_key;
                Numeric_receipt_private.list
                  (List.map
                     (fun dependency -> dependency.Numeric_bv_projection_evidence_private.evidence_full_key)
                     trusted_dependencies) ]
          in
          source.active_local <-
            { candidate; outer_authority_full_key;
              modular_authority_full_key;
              trusted_dependencies;
              authority_full_key }
            :: source.active_local;
          [%log.debug "activated prior-law numeric BV source authority"
            ~stage:(Delator.Field.string "numeric-prior-law-activation")
            ~function_name:
              (Delator.Field.string definition.Sst.function_id.function_name)
            ~semantic_width:(Delator.Field.int candidate.semantic_width)
            ~law_authorities:(Delator.Field.int 2)
            ~prior_closures:
              (Delator.Field.int
                 ((if candidate.outer_law_is_explicit_axiom then 0 else 1)
                 + (if candidate.modular_law_is_explicit_axiom then 0 else 1)))
            ~explicit_axioms:
              (Delator.Field.int
                 ((if candidate.outer_law_is_explicit_axiom then 1 else 0)
                 + (if candidate.modular_law_is_explicit_axiom then 1 else 0)))
            ~whole_program_completion:(Delator.Field.bool false)
            ~trusted_dependencies:
              (Delator.Field.int (List.length trusted_dependencies))
            ~decision:(Delator.Field.string "activated")]
      | Error _, _ | _, Error _ ->
          [%log.trace "kept numeric BV source statement inactive"
            ~stage:(Delator.Field.string "numeric-prior-law-activation")
            ~function_name:
              (Delator.Field.string definition.Sst.function_id.function_name)
            ~semantic_width:(Delator.Field.int candidate.semantic_width)
            ~decision:(Delator.Field.string "pending-or-refused")]
  in
  source.local_candidates
  |> List.filter (fun candidate ->
         candidate.outer_law_root.Sst.function_id = definition.Sst.function_id
         || candidate.modular_law_root.Sst.function_id
            = definition.Sst.function_id)
  |> List.iter activate
[@@delator.instrument] [@@delator.level debug]

let logical_roots caller =
  let predicate_roots clauses =
    List.map
      (fun (clause : Sst.predicate_clause) -> clause.predicate.expression)
      clauses
  in
  predicate_roots
    (caller.Sst.contracts.requires @ caller.contracts.assertions
   @ caller.contracts.decreases)
  @ List.map
      (fun (clause : Sst.ensures_clause) -> clause.predicate.expression)
      caller.contracts.ensures
  @
  match caller.body with
  | Sst.Spec_definition body | Proof_body { body; _ } -> [ body.expression ]
  | Checked_exec { body; _ } ->
      let rec proof_regions found expression =
        match expression.Sst.expression_desc with
        | Proof_region body -> body :: found
        | _ ->
            List.fold_left proof_regions found
              (Sst_callback_private.expression_children expression)
      in
      proof_regions [] body.expression
  | Recursive_spec_definition _ | External_specification _
  | Trusted_external_spec_target _ | Trusted_external_body _
  | Symbolic_declaration _ -> []

let logical_occurrence_path caller occurrence =
  let rec paths reversed_path expression =
    let here = if expression == occurrence then [ List.rev reversed_path ] else [] in
    Sst_callback_private.expression_children expression
    |> List.mapi (fun index child -> paths (index :: reversed_path) child)
    |> List.concat |> List.rev_append here
  in
  logical_roots caller
  |> List.mapi (fun root_index root ->
         paths [] root |> List.map (fun path -> (root_index, path)))
  |> List.concat
  |> function [ path ] -> Some path | [] | _ :: _ :: _ -> None

let local_occurrence_identity source caller occurrence =
  Option.map
    (fun (root_index, child_path) ->
      Numeric_receipt_private.encode
        ~schema:"verocaml.numeric-local-source-occurrence.v1"
        [ artifact_key source.implementation;
          string_of_int caller.Sst.function_id.function_index;
          string_of_int root_index;
          Numeric_receipt_private.list (List.map string_of_int child_path) ])
    (logical_occurrence_path caller occurrence)

let exact_imported_call source expression =
  match source.registration with
  | None -> None
  | Some registration -> (
      match Imported_callable.find_call registration expression with
      | None -> None
      | Some call -> Some (call, Imported_callable.call_summary call))

let matching_imported_claim source
    (summary : Imported_callable.callable_snapshot) =
  List.filter
    (fun claim ->
      String.equal claim.issuer_unit summary.provider_unit
      && String.equal claim.outer_callable_uid summary.binding_uid)
    source.request.projections

let admitted_width source semantic_width =
  let capability = Bv_backend_capability_receipt_private.capability () in
  let build_capability = Build_target_profile_private.capability () in
  let* profile =
    Build_target_profile_private.decode_profile build_capability
      source.request.target.profile_full_key
  in
  let* width = Bv_width.of_z ~profile capability (Z.of_int semantic_width) in
  Bv_width.for_instance capability source.request.target width

let imported_projection source occurrence =
  match exact_imported_call source occurrence with
  | None -> Ok None
  | Some (outer_call, outer_summary) -> (
      match matching_imported_claim source outer_summary with
      | [] -> Ok None
      | [ claim ] -> (
          match occurrence.Sst.expression_desc with
          | Direct_call
              { call_form = Specification_call; recursive = false;
                type_arguments = [];
                arguments =
                  [ Value_argument { label = None; value = inner } ]; _ } -> (
              match exact_imported_call source inner with
              | Some (inner_call, inner_summary)
                when String.equal inner_summary.provider_unit claim.issuer_unit
                     && String.equal inner_summary.binding_uid
                          claim.modular_callable_uid -> (
                  match inner.Sst.expression_desc with
                  | Direct_call
                      { call_form = Specification_call; recursive = false;
                        type_arguments = [];
                        arguments =
                          [ Value_argument { label = None; value = input } ]; _ }
                    when input.typ = Sst.Mathematical_int ->
                      let* width = admitted_width source claim.semantic_width in
                      let occurrence_full_key =
                        Numeric_receipt_private.encode
                          ~schema:
                            "verocaml.numeric-bv-imported-occurrence.v1"
                          [ source.full_key; claim.outer_law_full_key;
                            claim.modular_law_full_key;
                            claim.carrier_binding_full_key;
                            claim.base_int_full_key;
                            Imported_callable.call_snapshot outer_call;
                            Imported_callable.call_snapshot inner_call;
                            Bv_width.to_string width ]
                      in
                      let operation =
                        Numeric_bv_projection_evidence_private
                        .For_source_admission.operation
                      in
                      let outer_operation =
                        operation ~descriptor_origin:claim.issuer_unit
                          ~occurrence_identity:
                            (Imported_callable.call_snapshot outer_call)
                          ~call_span:occurrence.span
                          ~callable_uid:claim.outer_callable_uid
                          ~callable_tag:"unsigned-view"
                          ~callable_abi:claim.outer_callable_abi
                          ~law_evidence_full_key:claim.outer_law_full_key
                          ~authority:claim.outer_authority
                      and modular_operation =
                        operation ~descriptor_origin:claim.issuer_unit
                          ~occurrence_identity:
                            (Imported_callable.call_snapshot inner_call)
                          ~call_span:inner.span
                          ~callable_uid:claim.modular_callable_uid
                          ~callable_tag:"modular-conversion"
                          ~callable_abi:claim.modular_callable_abi
                          ~law_evidence_full_key:claim.modular_law_full_key
                          ~authority:claim.modular_authority
                      in
                      let authority =
                        Numeric_bv_projection_evidence_private.For_source_admission.issue
                          ~target:source.request.target ~width
                          ~provenance:Imported_registry
                          ~trusted_dependencies:claim.trusted_dependencies
                          ~occurrence_identity:
                            (Imported_callable.call_snapshot outer_call)
                          ~outer_operation ~modular_operation
                          ~carrier_binding_full_key:
                            claim.carrier_binding_full_key
                          ~base_int_full_key:claim.base_int_full_key
                          ~source_witness_full_key:source.full_key
                          ~occurrence_full_key
                      in
                      Ok (Some { input; width; authority })
                  | _ -> Ok None)
              | Some _ | None -> Ok None)
          | _ -> Ok None)
      | _ :: _ :: _ ->
          Error "ambiguous numeric BV imported projection source authority")

let local_projection source ~caller occurrence =
  let matching =
    if Option.is_none (logical_occurrence_path caller occurrence) then []
    else
    source.active_local
    |> List.filter_map (fun active ->
           match candidate_input active.candidate occurrence with
           | None -> None
           | Some (inner, input) ->
               Some (active, inner, input))
  in
  match matching with
  | [] -> Ok None
  | [ active, inner, input ] ->
      let* width =
        admitted_width source active.candidate.semantic_width
      in
      let* outer_occurrence_identity =
        match local_occurrence_identity source caller occurrence with
        | Some identity -> Ok identity
        | None ->
            Error
              "numeric BV outer call has no unique original source occurrence"
      in
      let* modular_occurrence_identity =
        match local_occurrence_identity source caller inner with
        | Some identity -> Ok identity
        | None ->
            Error
              "numeric BV conversion call has no unique original source occurrence"
      in
      let occurrence_full_key =
        Numeric_receipt_private.encode
          ~schema:"verocaml.numeric-local-bv-occurrence.v2"
          [ active.authority_full_key;
            active.candidate.carrier_binding_full_key;
            active.candidate.base_int_full_key;
            active.outer_authority_full_key;
            active.modular_authority_full_key;
            outer_occurrence_identity; modular_occurrence_identity;
            Bv_width.to_string width ]
      in
      let semantic_authority explicit =
        if explicit then Numeric_bv_projection_evidence_private.Explicit_axiom
        else Checked_proof
      in
      let authority =
        let operation =
          Numeric_bv_projection_evidence_private.For_source_admission.operation
        in
        let outer_authority =
          semantic_authority active.candidate.outer_law_is_explicit_axiom
        and modular_authority =
          semantic_authority active.candidate.modular_law_is_explicit_axiom
        in
        let outer_operation =
          operation
            ~descriptor_origin:active.candidate.descriptor_origin
            ~occurrence_identity:outer_occurrence_identity
            ~call_span:occurrence.span
            ~callable_uid:active.candidate.outer_callable_uid
            ~callable_tag:"unsigned-view"
            ~callable_abi:active.candidate.outer_callable_abi
            ~law_evidence_full_key:active.outer_authority_full_key
            ~authority:outer_authority
        and modular_operation =
          operation
            ~descriptor_origin:active.candidate.descriptor_origin
            ~occurrence_identity:modular_occurrence_identity
            ~call_span:inner.span
            ~callable_uid:active.candidate.modular_callable_uid
            ~callable_tag:"modular-conversion"
            ~callable_abi:active.candidate.modular_callable_abi
            ~law_evidence_full_key:active.modular_authority_full_key
            ~authority:modular_authority
        in
        Numeric_bv_projection_evidence_private.For_source_admission.issue
          ~target:source.request.target ~width
          ~provenance:Same_unit_prior_closure
          ~trusted_dependencies:active.trusted_dependencies
          ~occurrence_identity:outer_occurrence_identity ~outer_operation
          ~modular_operation
          ~carrier_binding_full_key:active.candidate.carrier_binding_full_key
          ~base_int_full_key:active.candidate.base_int_full_key
          ~source_witness_full_key:source.full_key ~occurrence_full_key
      in
      Ok (Some { input; width; authority })
  | _ :: _ :: _ ->
      Error "ambiguous numeric BV local projection source authority"

let unsigned_modular_projection source ~caller ~occurrence =
  let result =
    let program = Sst_validation.program source.validated in
    if not (List.exists (fun definition -> definition == caller) program.functions)
    then Error "numeric BV occurrence caller is not in the exact validated program"
    else if
      List.mem caller.function_id.function_index
        source.prior_law_schedule.bootstrap_functions
    then Ok None
    else
      let* imported =
        if Option.is_some (logical_occurrence_path caller occurrence) then
          imported_projection source occurrence
        else Ok None
      in
      match imported with
      | Some _ -> Ok imported
      | None -> local_projection source ~caller occurrence
  in
  [%log.trace "classified original source occurrence for native BV projection"
    ~stage:(Delator.Field.string "numeric-bv-source-admission")
    ~caller:(Delator.Field.string caller.Sst.function_id.function_name)
    ~admitted:
      (Delator.Field.bool
         (match result with Ok (Some _) -> true | Ok None | Error _ -> false))
    ~decision:
      (Delator.Field.string
         (match result with
         | Ok (Some _) -> "native-unsigned-modular-projection"
         | Ok None -> "ordinary-source-semantics"
         | Error _ -> "rejected"))];
  result
[@@delator.instrument] [@@delator.level trace]

let prior_law_scheduling_dependencies source ~definition =
  Option.value ~default:[]
    (List.assoc_opt definition.Sst.function_id.function_index
       source.prior_law_schedule.dependencies)

let full_key source = source.full_key

module For_testing = struct
  let original_logical_occurrence ~caller occurrence =
    Option.is_some (logical_occurrence_path caller occurrence)
end
