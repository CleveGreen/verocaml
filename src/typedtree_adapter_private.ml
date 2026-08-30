module Public = struct
  open Typedtree
  include Typedtree_adapter_issuance_private
  module Broadcast = Broadcast_declaration_private
  let issued_callable_instance_registry = ref []
  let callable_instance_issuer = ref ()
  let authenticate_callable_instance ~structure ~program instance =
    instance.callable_instance_token == callable_instance_issuer
    && instance.callable_instance_structure == structure
    && instance.callable_instance_program == program
    && List.exists
         (fun issued -> issued == instance)
         !issued_callable_instance_registry
    && List.exists
         (fun definition -> definition == instance.callable_instance_definition)
         program.Sst.functions
  let issued_callable_instances ~structure ~program =
    !issued_callable_instance_registry
    |> List.filter (authenticate_callable_instance ~structure ~program)
    |> List.rev
  let callable_instance_definition instance =
    instance.callable_instance_definition
  let callable_instance_source_paths instance =
    instance.callable_instance_source_paths
  let callable_instance_binding_uid instance =
    instance.callable_instance_binding_uid
  let callable_instance_leaf_name instance =
    instance.callable_instance_leaf_name
  let callable_instance_profile_snapshot instance =
    instance.callable_instance_profile_snapshot
  let callable_instance_specialization_digest instance =
    instance.callable_instance_specialization_digest
  let callable_instance_rank_snapshots instance =
    instance.callable_instance_rank_snapshots
  type issued_rank_domain =
    Parametric_rank_domain_private.validated_rank_domain
  let issue_rank_domains program pending_domains =
    pending_domains
    |> List.filter (fun pending ->
           List.for_all
             (fun identity ->
               match
                 List.find_opt
                   (fun (definition : Sst.type_definition) ->
                     definition.type_id = identity.rank_type_id)
                   program.Sst.types
               with
               | Some { representation = Sst.Revealed; _ } -> true
               | Some { representation = Sst.Abstract_with_evidence _; _ }
               | None ->
                   false)
             pending.pending_component)
    |> List.map (fun pending ->
           {
             Parametric_rank_domain_private.component =
               pending.pending_component;
             positive_children = pending.pending_positive_children;
             ground_witnesses = pending.pending_ground_witnesses;
             actual_evidence = pending.pending_actual_evidence;
           })
    |> Parametric_rank_domain_private.issue_nominal ~program
  let issued_rank_domains =
    Parametric_rank_domain_private.nominal_domains
  let rank_domain_id = Parametric_rank_domain_private.domain_id
  let rank_domain_version = Parametric_rank_domain_private.domain_version
  let rank_snapshot_digest = Parametric_rank_domain_private.snapshot_digest
  let rank_component = Parametric_rank_domain_private.component
  let rank_positive_children =
    Parametric_rank_domain_private.positive_children
  let rank_ground_witnesses =
    Parametric_rank_domain_private.ground_witnesses
  let rank_immutable = Parametric_rank_domain_private.immutable
  let rank_profile_id profile = profile.rank_profile_id
  let rank_profile_snapshot_digest profile =
    profile.rank_profile_snapshot_digest
  let rank_profile_identity profile = profile.rank_profile_identity
  let rank_profile_parameters profile = profile.rank_profile_parameters
  let rank_profile_applications profile = profile.rank_profile_applications
  let rank_profile_dependencies profile = profile.rank_profile_dependencies
  let rank_profile_ground_traces profile = profile.rank_profile_ground_traces
  let rank_profile_independently_grounded profile =
    profile.rank_profile_independently_grounded
  let authenticate_rank_profile ~structure profile =
    Parametric_rank_domain_private.authenticate_profile ~structure profile
  let issued_rank_profiles structure =
    Parametric_rank_domain_private.profiles structure
  let authenticate_rank_application profile application =
    authenticate_rank_profile ~structure:profile.rank_profile_structure profile
    && List.mem application profile.rank_profile_applications
  let authenticate_rank_grounding profile trace =
    authenticate_rank_profile ~structure:profile.rank_profile_structure profile
    && List.mem trace profile.rank_profile_ground_traces
  type issued_abstraction = {
    token : unit ref;
    evidence_id : string;
    abstract_signature_type : Sst.type_id;
    hidden_implementation_type : Sst.type_id;
    signature_module_identity : Sst.resolved_source_identity;
    implementation_module_identity : Sst.resolved_source_identity;
    abstract_signature_type_identity : Sst.resolved_source_identity;
    hidden_implementation_type_identity : Sst.resolved_source_identity;
    constraint_span : Sst.span;
    declaration_spans : Sst.span list;
    public_surface : Sst.abstract_public_operation list;
    owned_tree_prerequisite : Sst.owned_tree_prerequisite option;
    frozen_spine_prerequisite : Sst.frozen_spine_prerequisite option;
    abstract_type_ids : Sst.type_id list;
    semantic_types : (Sst.type_id * Sst.type_kind * Sst.span) list;
    semantic_functions : Sst.function_definition list;
  }
  let issued_abstractions = ref []
  let issue_abstraction ~evidence_id ~abstract_signature_type
      ~hidden_implementation_type ~signature_module_identity
      ~implementation_module_identity ~abstract_signature_type_identity
      ~hidden_implementation_type_identity ~constraint_span ~declaration_spans
      ~public_surface ~owned_tree_prerequisite ~frozen_spine_prerequisite
      ~abstract_type_ids ~semantic_types ~semantic_functions =
    let token = ref () in
    issued_abstractions :=
      {
        token;
        evidence_id;
        abstract_signature_type;
        hidden_implementation_type;
        signature_module_identity;
        implementation_module_identity;
        abstract_signature_type_identity;
        hidden_implementation_type_identity;
        constraint_span;
        declaration_spans;
        public_surface;
        owned_tree_prerequisite;
        frozen_spine_prerequisite;
        abstract_type_ids = List.sort compare abstract_type_ids;
        semantic_types =
          List.map
            (fun (definition : Sst.type_definition) ->
              (definition.type_id, definition.type_kind, definition.span))
            semantic_types;
        semantic_functions;
      }
      :: !issued_abstractions;
    let evidence : Sst.same_cmt_abstraction_evidence =
      {
        evidence_id;
        abstract_signature_type;
        hidden_implementation_type;
        signature_module_identity;
        implementation_module_identity;
        abstract_signature_type_identity;
        hidden_implementation_type_identity;
        constraint_span;
        declaration_spans;
        public_surface;
        owned_tree_prerequisite;
        authentication_token = token;
      }
    in
    Option.iter
      (Sst.register_frozen_spine_prerequisite evidence)
      frozen_spine_prerequisite;
    Sst.Authenticated_same_cmt_abstraction evidence
  let authenticate_abstraction ~definition ~types ~functions
      (evidence : Sst.same_cmt_abstraction_evidence) =
    let semantic_types =
      List.map
        (fun (definition : Sst.type_definition) ->
          (definition.type_id, definition.type_kind, definition.span))
        types
    in
    let token_candidates =
      List.filter
        (fun expected -> expected.token == evidence.authentication_token)
        !issued_abstractions
    in
    let issued_type_ids expected =
      List.map (fun (type_id, _, _) -> type_id) expected.semantic_types
    in
    let current_local_semantic_types expected =
      let issued_type_ids = issued_type_ids expected in
      List.filter
        (fun (type_id, _, _) -> List.mem type_id issued_type_ids)
        semantic_types
    in
    let issued_function_ids expected =
      List.map
        (fun (definition : Sst.function_definition) -> definition.function_id)
        expected.semantic_functions
    in
    let current_local_semantic_functions expected =
      let issued_function_ids = issued_function_ids expected in
      List.filter
        (fun (definition : Sst.function_definition) ->
          List.mem definition.function_id issued_function_ids)
        functions
    in
    let current_local_abstract_type_ids expected =
      let issued_type_ids =
        issued_type_ids expected
      in
      List.filter_map
        (fun (definition : Sst.type_definition) ->
          if not (List.mem definition.type_id issued_type_ids) then None
          else
            match definition.representation with
            | Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction _) ->
                Some definition.type_id
            | Sst.Revealed
            | Sst.Abstract_with_evidence
                ( Sst.Incomplete_abstraction_evidence _
                | Sst.Proposed_same_cmt_abstraction _ ) ->
                None)
        types
      |> List.sort compare
    in
    let semantic_types_match expected =
      expected.semantic_types = current_local_semantic_types expected
    in
    let semantic_functions_match expected =
      expected.semantic_functions = current_local_semantic_functions expected
    in
    let abstract_type_set_matches expected =
      expected.abstract_type_ids = current_local_abstract_type_ids expected
    in
    let authenticates expected =
      String.equal expected.evidence_id evidence.evidence_id
      && expected.abstract_signature_type = evidence.abstract_signature_type
      && expected.hidden_implementation_type = evidence.hidden_implementation_type
      && expected.signature_module_identity = evidence.signature_module_identity
      && expected.implementation_module_identity
         = evidence.implementation_module_identity
      && expected.abstract_signature_type_identity
         = evidence.abstract_signature_type_identity
      && expected.hidden_implementation_type_identity
         = evidence.hidden_implementation_type_identity
      && expected.constraint_span = evidence.constraint_span
      && expected.declaration_spans = evidence.declaration_spans
      && expected.public_surface = evidence.public_surface
      && expected.owned_tree_prerequisite = evidence.owned_tree_prerequisite
      && expected.frozen_spine_prerequisite
         = Sst.frozen_spine_prerequisite evidence
      && expected.abstract_signature_type = definition.Sst.type_id
      && semantic_types_match expected
      && semantic_functions_match expected
      && abstract_type_set_matches expected
    in
    let authenticated = List.exists authenticates token_candidates in
    if not authenticated then
      match token_candidates with
      | [] ->
          [%log.debug "same-CMT abstraction authentication has no issued token"
            ~evidence_id:(Delator.Field.string evidence.evidence_id)
            ~issued_abstraction_count:
              (Delator.Field.int (List.length !issued_abstractions))]
      | _expected :: _ ->
          [%log.debug "same-CMT abstraction authentication mismatch"
            ~evidence_id:(Delator.Field.string evidence.evidence_id)
            ~token_candidate_count:
              (Delator.Field.int (List.length token_candidates))
            ~evidence_identity_matches:
              (Delator.Field.bool
                 (String.equal _expected.evidence_id evidence.evidence_id))
            ~type_identity_matches:
              (Delator.Field.bool
                 (_expected.abstract_signature_type
                    = evidence.abstract_signature_type
                 && _expected.hidden_implementation_type
                    = evidence.hidden_implementation_type
                 && _expected.abstract_signature_type = definition.Sst.type_id))
            ~module_identity_matches:
              (Delator.Field.bool
                 (_expected.signature_module_identity
                    = evidence.signature_module_identity
                 && _expected.implementation_module_identity
                    = evidence.implementation_module_identity
                 && _expected.abstract_signature_type_identity
                    = evidence.abstract_signature_type_identity
                 && _expected.hidden_implementation_type_identity
                    = evidence.hidden_implementation_type_identity))
            ~span_matches:
              (Delator.Field.bool
                 (_expected.constraint_span = evidence.constraint_span
                 && _expected.declaration_spans = evidence.declaration_spans))
            ~public_surface_matches:
              (Delator.Field.bool
                 (_expected.public_surface = evidence.public_surface))
            ~prerequisite_matches:
              (Delator.Field.bool
                 (_expected.owned_tree_prerequisite
                    = evidence.owned_tree_prerequisite
                 && _expected.frozen_spine_prerequisite
                    = Sst.frozen_spine_prerequisite evidence))
            ~semantic_types_match:
              (Delator.Field.bool (semantic_types_match _expected))
            ~issued_semantic_type_count:
              (Delator.Field.int (List.length _expected.semantic_types))
            ~current_semantic_type_count:
              (Delator.Field.int (List.length semantic_types))
            ~current_local_semantic_type_count:
              (Delator.Field.int
                 (List.length (current_local_semantic_types _expected)))
            ~semantic_functions_match:
              (Delator.Field.bool (semantic_functions_match _expected))
            ~issued_semantic_function_count:
              (Delator.Field.int (List.length _expected.semantic_functions))
            ~current_semantic_function_count:
              (Delator.Field.int (List.length functions))
            ~current_local_semantic_function_count:
              (Delator.Field.int
                 (List.length (current_local_semantic_functions _expected)))
            ~abstract_type_set_matches:
              (Delator.Field.bool (abstract_type_set_matches _expected))
            ~issued_abstract_type_count:
              (Delator.Field.int (List.length _expected.abstract_type_ids))
            ~current_abstract_type_count:
              (Delator.Field.int
                 (List.length (current_local_abstract_type_ids _expected)))]
    else ();
    authenticated
  include Typedtree_surface_private
  open Typedtree_logical_builtin_private
  open Callback_contract_private
  open Callback_call_private
  open Sst_callback_private
  let authenticate_proof_region =
    Typedtree_logical_builtin_private.authenticate_proof_region
  let authenticate_local_assertion =
    Typedtree_logical_builtin_private.authenticate_local_assertion
  let authenticate_local_assertion_candidate =
    Typedtree_logical_builtin_private.authenticate_local_assertion_candidate
  let local_assertion_parent_proof_region =
    Typedtree_logical_builtin_private.local_assertion_parent_proof_region
  let is_direct_exec_builtin_local_assertion =
    Typedtree_logical_builtin_private.is_direct_exec_builtin_local_assertion
  let issue_builtin_local_assertions =
    Typedtree_logical_builtin_private.issue_builtin_local_assertions
  let recursive_helper_certificates = ref []
  let recursive_helper_expansions = ref 0
  let recursive_helper_issuer = ref ()
  let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
    left.function_index = right.function_index
    && String.equal left.function_name right.function_name
  let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
    left.type_index = right.type_index
    && String.equal left.type_name right.type_name
  let recursive_helper_role program function_ definition =
    match function_.function_kind with
    | Top_spec _ ->
        if
          Option.is_some
            (Spec_definition.authenticated_model_domain program.Sst.types
               definition)
        then Direct_model
        else Ordinary_direct_spec
    | Top_type_invariant _ -> Direct_type_invariant
    | Top_exec | Top_recursive_spec _ | Top_proof _
    | Top_external_specification _ | Top_external_body _ ->
        Not_a_direct_spec
  let issue_recursive_helper_certificates ~local_type_ids program functions =
    let function_kind definition =
      List.find_opt
        (fun function_ ->
          same_function_id function_.function_id definition.Sst.function_id)
        functions
    in
    let members =
      List.mapi
        (fun helper_source_order helper_definition ->
          let helper_role =
            match function_kind helper_definition with
            | Some function_ ->
                recursive_helper_role program function_ helper_definition
            | None -> Not_a_direct_spec
          in
          { helper_definition; helper_source_order; helper_role })
        program.Sst.functions
    in
    let issued =
      List.filter_map
        (fun helper_root ->
          match
            ( helper_root.helper_definition.Sst.body,
              function_kind helper_root.helper_definition )
          with
          | ( Sst.Recursive_spec_definition _,
              Some { function_kind = Top_recursive_spec _; _ } ) ->
              Some
                {
                  helper_token = recursive_helper_issuer;
                  helper_program = program;
                  helper_program_snapshot = Sst.to_string program;
                  helper_root;
                  helper_members = members;
                  helper_local_types = local_type_ids;
                  helper_expanded_body = None;
                  helper_adversary_edges = None;
                  helper_adversary_body = None;
                  helper_adversary_call = None;
                }
          | _ -> None)
        members
    in
    recursive_helper_certificates :=
      List.rev_append issued
        (List.filter
           (fun certificate -> certificate.helper_program != program)
           !recursive_helper_certificates)
  let find_recursive_helper_certificate ~program ~definition =
    List.find_opt
      (fun certificate ->
        certificate.helper_token == recursive_helper_issuer
        && certificate.helper_program == program
        && certificate.helper_root.helper_definition == definition
        && List.exists
             (fun candidate -> candidate == certificate)
             !recursive_helper_certificates)
      !recursive_helper_certificates
  let authenticate_recursive_specification_in_program ~program ~definition =
    match definition.Sst.body with
    | Sst.Recursive_spec_definition _ ->
        Option.is_some (find_recursive_helper_certificate ~program ~definition)
    | _ -> false
  let authenticate_recursive_specification ~definition =
    match definition.Sst.body with
    | Sst.Recursive_spec_definition _ ->
        List.exists
          (fun certificate ->
            certificate.helper_token == recursive_helper_issuer
            && certificate.helper_root.helper_definition == definition)
          !recursive_helper_certificates
    | _ -> false
  let validate_builtin_assertion_conditions ~program
      (definition : Sst.function_definition) =
    let rec validate expression =
      let* () =
        match expression.Sst.expression_desc with
        | Sst.Local_assert { predicate; _ }
          when is_builtin_local_assertion ~program ~definition ~expression ->
            Callback_contract_private.validate_builtin_assertion_predicate
              predicate
        | _ -> Ok ()
      in
      List.fold_left
        (fun result child ->
          let* () = result in
          validate child)
        (Ok ())
        (recursive_helper_expression_children expression)
    in
    match definition.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Proof_body { body; _ }
    | Sst.Spec_definition body
    | Sst.Recursive_spec_definition { body; _ } ->
        validate body.expression
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        Ok ()
  let recursive_helper_binding_ids (expression : Sst.expression) =
    let rec pattern_ids (pattern : Sst.pattern) =
      match pattern.pattern_desc with
      | Sst.Bind binding -> [ binding.id ]
      | Sst.Constructor_pattern (_, arguments) ->
          List.concat_map pattern_ids arguments
      | Sst.Tuple_pattern components ->
          List.concat_map (fun (_, pattern) -> pattern_ids pattern) components
      | Sst.Record_pattern fields ->
          List.concat_map (fun (_, pattern) -> pattern_ids pattern) fields
      | Sst.Or_pattern (left, right) -> pattern_ids left @ pattern_ids right
      | Sst.Owned_tree_cursor_pattern cursor -> [ cursor.cursor_binding.id ]
      | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
        ->
          []
    in
    let rec expression_ids expression =
      let local =
        match expression.Sst.expression_desc with
        | Sst.Variable { binding; _ } | Sst.Mutable_read binding ->
            [ binding.id ]
        | Sst.Let_mutable (binding, _, _) -> [ binding.id ]
        | Sst.Let (bindings, _) ->
            List.concat_map (fun (pattern, _) -> pattern_ids pattern) bindings
        | Sst.Match (_, cases) ->
            List.concat_map
              (fun (case : Sst.case) -> pattern_ids case.case_pattern)
              cases
        | _ -> []
      in
      local
      @ List.concat_map expression_ids
          (recursive_helper_expression_children expression)
    in
    expression_ids expression
  let validate_recursive_helper_certificate certificate =
    let program = certificate.helper_program in
    let root = certificate.helper_root in
    let root_definition = root.helper_definition in
    let root_id = root_definition.Sst.function_id in
    let parametric_application typ =
      match typ with
      | Sst.Application (constructor, _) ->
          Option.is_some
            (Parametric_adt.find program.parametric_adts constructor)
          && Parametric_adt.deeply_immutable_instance program.parametric_adts
               typ
      | _ -> false
    in
    let aggregate_extension =
      match root_definition.result_type with
      | Sst.Aggregate _ -> true
      | Sst.Application _ as typ -> parametric_application typ
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> false
    in
    let member id =
      List.find_opt
        (fun candidate ->
          same_function_id candidate.helper_definition.Sst.function_id id)
        certificate.helper_members
    in
    let root_aggregate_seeds =
      (List.filter_map
         (fun parameter ->
           let parameter = Sst.require_value_parameter parameter in
           match parameter.pattern.pattern_desc with
           | Sst.Bind
               {
                 typ = Sst.Aggregate type_id;
                 uniqueness = Sst.Definitely_aliased;
                 _;
               } ->
               Some type_id
           | _ -> None)
         root_definition.parameters
      @
      match root_definition.result_type with
      | Sst.Aggregate type_id -> [ type_id ]
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          [])
      |> List.sort_uniq compare
    in
    let rank_domains = ref (issued_rank_domains program) in
    let local_type type_id =
      if not (List.exists (same_type_id type_id) certificate.helper_local_types)
      then None
      else
        List.find_opt
          (fun definition -> same_type_id definition.Sst.type_id type_id)
          program.Sst.types
    in
    let fields_of_kind = function
      | Sst.Record_definition fields -> fields
      | Sst.Variant_definition constructors ->
          List.concat_map
            (fun (constructor : Sst.constructor_definition) ->
              constructor.constructor_fields)
            constructors
    in
    let field_owner_type = function
      | Sst.Record_owner type_id -> type_id
      | Sst.Constructor_owner constructor -> constructor.Sst.constructor_type
    in
    let rec collect_type visiting collected = function
      | Sst.Unit | Sst.Bool | Sst.Int -> Some collected
      | Sst.Application (constructor, _) as typ -> (
          match
            ( Parametric_adt.find program.parametric_adts constructor,
              Parametric_rank_domain_private.derive_application ~program
                ~span:root_definition.span typ )
          with
          | Some descriptor, Ok domain when parametric_application typ ->
              rank_domains := domain :: !rank_domains;
              let type_id = Parametric_adt.type_id descriptor in
              Some
                (if List.exists (same_type_id type_id) collected then collected
                 else type_id :: collected)
          | Some _, Ok _ | Some _, Error _ | None, Ok _ | None, Error _ -> None)
      | Sst.Parameter _ | Sst.Tuple _ -> None
      | Sst.Aggregate type_id -> (
          if List.exists (same_type_id type_id) visiting then
            Some
              (if List.exists (same_type_id type_id) collected then collected
               else type_id :: collected)
          else
            match local_type type_id with
            | Some { representation = Sst.Revealed; type_kind; _ }
              when List.for_all
                     (fun (field : Sst.field_definition) ->
                       field.field_mutability = Sst.Immutable_field
                       && field.field_modalities.uniqueness_modality
                          = Sst.Preserve_uniqueness
                       && field.field_modalities.linearity_modality
                          = Sst.Preserve_linearity)
                     (fields_of_kind type_kind) ->
                List.fold_left
                  (fun state (field : Sst.field_definition) ->
                    Option.bind state (fun collected ->
                        collect_type (type_id :: visiting) collected
                          field.field_type))
                  (Some
                     (if List.exists (same_type_id type_id) collected then
                        collected
                      else type_id :: collected))
                  (fields_of_kind type_kind)
            | Some { representation = Sst.Abstract_with_evidence _; _ }
            | Some _ | None ->
                None)
    in
    let root_aggregate_types =
      if aggregate_extension then
        List.fold_left
          (fun state type_id ->
            Option.bind state (fun collected ->
                collect_type [] collected (Sst.Aggregate type_id)))
          (Some []) root_aggregate_seeds
        |> Option.value ~default:[] |> List.sort_uniq compare
      else root_aggregate_seeds
    in
    let selected_domains =
      List.filter
        (fun domain ->
          rank_immutable domain
          && List.exists
               (fun identity ->
                 List.exists
                   (same_type_id identity.rank_type_id)
                   root_aggregate_types)
               (rank_component domain))
        !rank_domains
      |> List.sort_uniq (fun left right ->
          String.compare (rank_domain_id left) (rank_domain_id right))
    in
    let root_aggregate_type type_id =
      List.exists (same_type_id type_id) root_aggregate_types
      &&
      if aggregate_extension then
        match selected_domains with [ _ ] -> true | [] | _ :: _ :: _ -> false
      else
        match local_type type_id with
        | Some
            {
              Sst.type_kind = Sst.Variant_definition constructors;
              representation = Sst.Revealed;
              _;
            } ->
            List.for_all
              (fun constructor ->
                List.for_all
                  (fun field ->
                    field.Sst.field_mutability = Sst.Immutable_field)
                  constructor.Sst.constructor_fields)
              constructors
            && List.exists
                 (fun domain ->
                   rank_immutable domain
                   && List.exists
                        (fun identity ->
                          same_type_id identity.rank_type_id type_id)
                        (rank_component domain))
                 !rank_domains
        | Some { type_kind = Sst.Record_definition _; _ }
        | Some { representation = Sst.Abstract_with_evidence _; _ }
        | None ->
            false
    in
    let admitted_type = function
      | Sst.Int | Sst.Bool -> true
      | typ when Parametric_type.is_spec_function typ -> true
      | Sst.Aggregate type_id -> root_aggregate_type type_id
      | Sst.Application _ as typ -> parametric_application typ
      | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ -> false
    in
    let owner_type_matches typ type_id =
      match typ with
      | Sst.Aggregate candidate -> candidate = type_id
      | Sst.Application (constructor, _) ->
          Parametric_adt.find program.parametric_adts constructor
          |> Option.fold ~none:false ~some:(fun descriptor ->
              Parametric_adt.type_id descriptor = type_id)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> false
    in
    let rec validate_pattern (pattern : Sst.pattern) =
      if not (admitted_type pattern.Sst.typ) then
        recursive_helper_error
          "helper pattern type is outside the authenticated recursive-helper \
           ABI"
      else
        match pattern.Sst.pattern_desc with
        | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ -> Ok ()
        | Sst.Bind binding when binding.typ = pattern.typ -> Ok ()
        | Sst.Constructor_pattern (constructor, arguments)
          when pattern.typ = Sst.Aggregate constructor.constructor_type
               && root_aggregate_type constructor.constructor_type
               ||
               match pattern.typ with
               | Sst.Application (type_constructor, _) ->
                   Parametric_adt.find program.parametric_adts type_constructor
                   |> Option.fold ~none:false ~some:(fun descriptor ->
                       Parametric_adt.type_id descriptor
                       = constructor.constructor_type)
               | _ -> false ->
            let rec loop = function
              | [] -> Ok ()
              | argument :: rest ->
                  let* () = validate_pattern argument in
                  loop rest
            in
            loop arguments
        | Sst.Record_pattern fields ->
            if not aggregate_extension then
              recursive_helper_error
                "record pattern escaped the scalar recursive-helper grammar"
            else
              let rec loop = function
                | [] -> Ok ()
                | (field, nested) :: rest ->
                    if
                      same_type_id
                        (field_owner_type field.Sst.field_owner)
                        (match pattern.typ with
                        | Sst.Aggregate type_id -> type_id
                        | Sst.Application (constructor, _) ->
                            Parametric_adt.find program.parametric_adts
                              constructor
                            |> Option.map Parametric_adt.type_id
                            |> Option.get
                        | _ -> assert false)
                    then
                      let* () = validate_pattern nested in
                      loop rest
                    else
                      recursive_helper_error
                        "helper record pattern field has the wrong nominal \
                         owner"
              in
              loop fields
        | Sst.Bind _ | Sst.Constructor_pattern _ | Sst.Unit_pattern
        | Sst.Tuple_pattern _ | Sst.Owned_tree_cursor_pattern _
        | Sst.Or_pattern _ ->
            recursive_helper_error
              "helper pattern is outside the authenticated recursive-helper \
               grammar"
    in
    let validate_signature ?body_override member =
      let definition = member.helper_definition in
      let physically_bound =
        match List.nth_opt program.functions member.helper_source_order with
        | Some candidate -> candidate == definition
        | None -> false
      in
      if not physically_bound then
        recursive_helper_error
          "recursive helper definition is not the issued physical source member"
      else if
        member.helper_role <> Ordinary_direct_spec
        || definition.mode <> Sst.Spec
        || definition.recursive
      then
        recursive_helper_error
          "recursive helper is not an earlier ordinary direct-source \
           nonrecursive Spec"
      else
        match Option.value ~default:definition.body body_override with
        | Sst.Spec_definition body
          when body.stage = Sst.Logical
               && (match definition.result_type with
                 | Sst.Int | Sst.Bool -> true
                 | Sst.Aggregate _ ->
                     aggregate_extension && admitted_type definition.result_type
                 | Sst.Application _ as typ ->
                     Parametric_type.is_spec_function typ
                     || aggregate_extension && parametric_application typ
                 | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ -> false)
               && body.expression.typ = definition.result_type
               && definition.contracts = Sst.empty_contracts
               && definition.returns_unique_parameter = None ->
            let rec parameters = function
              | [] -> Ok ()
              | Sst.Callback_parameter _ :: _ ->
                  recursive_helper_error
                    "recursive helper callback formals are unsupported"
              | Sst.Value_parameter { Sst.label = None; pattern; _ } :: rest
                -> (
                  match pattern.pattern_desc with
                  | Sst.Bind binding
                    when binding.typ = pattern.typ && admitted_type binding.typ
                    -> (
                      match binding.typ with
                      | Sst.Int | Sst.Bool -> parameters rest
                      | typ when Parametric_type.is_spec_function typ ->
                          parameters rest
                      | (Sst.Aggregate _ | Sst.Application _)
                        when binding.uniqueness = Sst.Definitely_aliased ->
                          parameters rest
                      | Sst.Aggregate _ | Sst.Application _ | Sst.Unit
                      | Sst.Tuple _ | Sst.Parameter _ ->
                          recursive_helper_error
                            "recursive helper aggregate parameters must be \
                             definitely aliased")
                  | _ ->
                      recursive_helper_error
                        "recursive helper parameters must be saturated \
                         unlabelled scalar or existing-root aggregate \
                         variables")
              | _ ->
                  recursive_helper_error
                    "recursive helper parameters must be saturated and \
                     unlabelled"
            in
            parameters definition.parameters
        | Sst.Spec_definition _ ->
            recursive_helper_error
              "recursive helper result/body is outside the authenticated \
               logical ABI"
        | Sst.Checked_exec _ | Sst.Recursive_spec_definition _
        | Sst.Proof_body _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            recursive_helper_error
              "recursive helper does not have an authenticated direct Spec body"
    in
    let validate_ordered_edge owner callee =
      if callee.helper_source_order < owner.helper_source_order then
        validate_signature callee
      else
        recursive_helper_error
          "recursive helper call is cyclic or not strictly earlier in direct \
           source order"
    in
    let validate_call_header owner callee arguments =
      let* () = validate_ordered_edge owner callee in
      if
        List.length arguments <> List.length callee.helper_definition.parameters
        || List.exists (fun (label, _) -> label <> None) arguments
      then
        recursive_helper_error
          "recursive helper call is partial, labelled, or has the wrong arity"
      else Ok ()
    in
    let rec validate_expression ~owner (expression : Sst.expression) =
      let nested expressions =
        let rec loop = function
          | [] -> Ok ()
          | expression :: rest ->
              let* () = validate_expression ~owner expression in
              loop rest
        in
        loop expressions
      in
      let validate_quantifier expected requires_trigger
          (quantifier : Sst.quantifier) =
        if
          expression.typ = Sst.Bool
          && quantifier.quantifier_body.typ = Sst.Bool
          && admitted_type quantifier.quantifier_binder.typ
          && Bool.equal requires_trigger
               (Option.is_some quantifier.quantifier_trigger)
        then
          let expected_owner =
            "function:"
            ^ owner.helper_definition.Sst.function_id.function_name
          in
          (match
             Quantifier_validation_private.validate_sst ~allow_unclassified:true
               ~expected_owner expected quantifier
           with
          | Ok () ->
              nested
                (quantifier.quantifier_body
                :: Option.to_list quantifier.quantifier_trigger)
          | Error _ ->
              recursive_helper_error
                "quantifier escaped its authenticated binder, type, trigger, \
                 or owner policy")
        else
          recursive_helper_error
            "quantifier escaped its authenticated binder, type, trigger, or \
             owner policy"
      in
      if not (admitted_type expression.Sst.typ) then
        recursive_helper_error
          "helper expression type is outside the authenticated \
           recursive-helper ABI"
      else
        match expression.expression_desc with
        | Sst.Int_constant _ | Sst.Bool_constant _ -> Ok ()
        | Sst.Variable { binding; use_uniqueness = Sst.Definitely_aliased }
          when binding.typ = expression.typ ->
            Ok ()
        | Sst.Let (bindings, body) ->
            let rec validate_bindings = function
              | [] -> validate_expression ~owner body
              | ((pattern : Sst.pattern), (value : Sst.expression)) :: rest ->
                  let* () =
                    if pattern.Sst.typ = value.Sst.typ then
                      validate_pattern pattern
                    else
                      recursive_helper_error
                        "helper let pattern and value types differ"
                  in
                  let* () = validate_expression ~owner value in
                  validate_bindings rest
            in
            validate_bindings bindings
        | Sst.If (condition, consequent, Some alternative)
          when condition.typ = Sst.Bool
               && (aggregate_extension || expression.typ = Sst.Bool)
               && consequent.typ = expression.typ
               && alternative.typ = expression.typ ->
            nested [ condition; consequent; alternative ]
        | Sst.Checked_arithmetic (_, operands) -> nested operands
        | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
            nested [ left; right ]
        | Sst.Boolean_not operand -> validate_expression ~owner operand
        | Sst.Forall quantifier ->
            validate_quantifier Logic_quantifier_private.Forall true quantifier
        | Sst.Exists quantifier ->
            validate_quantifier Logic_quantifier_private.Exists false quantifier
        | Sst.Field_read { record; field } -> (
            match field.field_owner with
            | Sst.Constructor_owner constructor
              when owner_type_matches record.typ constructor.constructor_type ->
                validate_expression ~owner record
            | Sst.Record_owner record_type
              when aggregate_extension
                   && owner_type_matches record.typ record_type ->
                validate_expression ~owner record
            | Sst.Record_owner _ | Sst.Constructor_owner _ ->
                recursive_helper_error
                  "record or foreign field escaped the recursive-helper grammar"
            )
        | Sst.Match (scrutinee, cases)
          when (aggregate_extension || expression.typ = Sst.Bool)
               &&
               match scrutinee.typ with
               | Sst.Aggregate type_id -> root_aggregate_type type_id
               | Sst.Application _ as typ -> parametric_application typ
               | _ -> false ->
            let* () = validate_expression ~owner scrutinee in
            let rec validate_cases = function
              | [] -> Ok ()
              | (case : Sst.case) :: rest ->
                  let* () = validate_pattern case.case_pattern in
                  let* () =
                    match case.case_guard with
                    | None -> Ok ()
                    | Some guard when guard.typ = Sst.Bool ->
                        validate_expression ~owner guard
                    | Some _ ->
                        recursive_helper_error
                          "recursive helper match guard must be Boolean"
                  in
                  let* () = validate_expression ~owner case.case_body in
                  validate_cases rest
            in
            validate_cases cases
        | Sst.Record_value { record_type; fields }
          when aggregate_extension
               && owner_type_matches expression.typ record_type ->
            nested (List.map snd fields)
        | Sst.Constructor_value { constructor; arguments }
          when aggregate_extension
               && owner_type_matches expression.typ constructor.constructor_type
          ->
            nested arguments
        | Sst.Direct_call _
          when Spec_function_sst_private.application expression <> None ->
            let application =
              Option.get (Spec_function_sst_private.application expression)
            in
            nested
              [
                application.application_function;
                application.application_argument;
              ]
        | Sst.Direct_call
            {
              call_form = Sst.Specification_call;
              callee;
              arguments;
              recursive = true;
              _;
            }
          when owner == root && same_function_id callee root_id ->
            if
              List.length arguments = List.length root_definition.parameters
              && List.for_all
                   (function
                     | Sst.Value_argument { label = None; _ } -> true
                     | Sst.Value_argument { label = Some _; _ }
                     | Sst.Callback_argument _ ->
                         false)
                   arguments
            then
              nested
                (List.map
                   (fun argument -> snd (Sst.require_value_argument argument))
                   arguments)
            else
              recursive_helper_error
                "recursive self call is partial, labelled, or has the wrong \
                 arity"
        | Sst.Direct_call
            {
              call_form = Sst.Specification_call;
              callee;
              arguments;
              recursive = false;
              _;
            } -> (
            match member callee with
            | Some callee_member ->
                let arguments = List.map Sst.require_value_argument arguments in
                let* () = validate_call_header owner callee_member arguments in
                nested (List.map snd arguments)
            | None ->
                recursive_helper_error
                  "recursive helper call is imported or lacks direct-source \
                   authority")
        | Sst.Unit_constant | Sst.Variable _ | Sst.Tuple_value _
        | Sst.Record_value _ | Sst.Constructor_value _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
        | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
        | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
        | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
        | Sst.Old _ | Sst.Optional_absent | Sst.Optional_present _
        | Sst.Optional_forward _ | Sst.Symbolic_application _ ->
            recursive_helper_error
              "expression is outside the authenticated recursive-helper grammar"
    in
    let rec validate_closure visited current_member =
      if
        List.exists
          (fun id ->
            same_function_id id current_member.helper_definition.Sst.function_id)
          visited
      then Ok visited
      else
        let* () = validate_signature current_member in
        let body =
          match current_member.helper_definition.Sst.body with
          | Sst.Spec_definition body -> body.expression
          | _ -> assert false
        in
        let* () = validate_expression ~owner:current_member body in
        let callees =
          let rec collect expression =
            let nested =
              List.concat_map collect
                (recursive_helper_expression_children expression)
            in
            match expression.Sst.expression_desc with
            | Sst.Direct_call _
              when Spec_function_sst_private.application expression <> None ->
                nested
            | Sst.Direct_call
                {
                  call_form = Sst.Specification_call;
                  callee;
                  recursive = false;
                  _;
                } ->
                callee :: nested
            | _ -> nested
          in
          collect body
        in
        let visited = current_member.helper_definition.function_id :: visited in
        let rec descend visited = function
          | [] -> Ok visited
          | callee :: rest -> (
              match member callee with
              | Some callee_member ->
                  let* visited = validate_closure visited callee_member in
                  descend visited rest
              | None ->
                  recursive_helper_error
                    "recursive helper closure contains an imported call")
        in
        descend visited callees
    in
    let physical_root =
      List.find_opt
        (fun definition -> same_function_id definition.Sst.function_id root_id)
        program.functions
    in
    if
      certificate.helper_program != program
      ||
      match physical_root with
      | Some definition -> definition != root.helper_definition
      | None -> true
    then
      recursive_helper_error
        "recursive helper certificate/program identity mismatch"
    else
      let* () =
        match certificate.helper_adversary_edges with
        | None -> Ok ()
        | Some edges ->
            let rec validate = function
              | [] -> Ok ()
              | (owner, callee) :: rest ->
                  let* () = validate_ordered_edge owner callee in
                  validate rest
            in
            validate edges
      in
      let* () =
        match certificate.helper_adversary_body with
        | None -> Ok ()
        | Some (member, body) -> validate_signature ~body_override:body member
      in
      let* () =
        match certificate.helper_adversary_call with
        | None -> Ok ()
        | Some (owner, callee, arguments) ->
            validate_call_header owner callee arguments
      in
      match root_definition.body with
      | Sst.Recursive_spec_definition { body; _ } ->
          (* Root-body grammar remains owned by semantic validation.  This pass
           authenticates every nested call edge without narrowing pre-existing
           recursive bodies that do not use helper authority. *)
          let rec validate_root_calls expression =
            match expression.Sst.expression_desc with
            | Sst.Direct_call _
              when Spec_function_sst_private.application expression <> None ->
                let application =
                  Option.get (Spec_function_sst_private.application expression)
                in
                let* () =
                  validate_root_calls application.application_function
                in
                validate_root_calls application.application_argument
            | Sst.Direct_call
                {
                  call_form = Sst.Specification_call;
                  callee;
                  arguments;
                  recursive = true;
                  _;
                }
              when same_function_id callee root_id ->
                if
                  List.length arguments = List.length root_definition.parameters
                  && List.for_all
                       (function
                         | Sst.Value_argument { label = None; _ } -> true
                         | Sst.Value_argument { label = Some _; _ }
                         | Sst.Callback_argument _ ->
                             false)
                       arguments
                then
                  let rec actuals = function
                    | [] -> Ok ()
                    | Sst.Value_argument { value = actual; _ } :: rest ->
                        let* () = validate_root_calls actual in
                        actuals rest
                    | Sst.Callback_argument _ :: _ ->
                        recursive_helper_error
                          "recursive root callback actuals are unsupported"
                  in
                  actuals arguments
                else
                  recursive_helper_error
                    "recursive self call is partial, labelled, or has the \
                     wrong arity"
            | Sst.Direct_call
                {
                  call_form = Sst.Specification_call;
                  callee;
                  arguments;
                  recursive = false;
                  _;
                } -> (
                match member callee with
                | Some helper ->
                    let arguments =
                      List.map Sst.require_value_argument arguments
                    in
                    let* () = validate_call_header root helper arguments in
                    let rec actuals = function
                      | [] -> Ok ()
                      | (_, actual) :: rest ->
                          let* () = validate_root_calls actual in
                          actuals rest
                    in
                    actuals arguments
                | None ->
                    recursive_helper_error
                      "recursive helper call is imported or lacks \
                       direct-source authority")
            | Sst.Direct_call { call_form = Sst.Specification_call; _ } ->
                recursive_helper_error
                  "recursive helper closure reaches a distinct recursive Spec"
            | _ ->
                let rec children = function
                  | [] -> Ok ()
                  | child :: rest ->
                      let* () = validate_root_calls child in
                      children rest
                in
                children (recursive_helper_expression_children expression)
          in
          let* () = validate_root_calls body.expression in
          let direct_helpers =
            let rec collect expression =
              let nested =
                List.concat_map collect
                  (recursive_helper_expression_children expression)
              in
              match expression.Sst.expression_desc with
              | Sst.Direct_call _
                when Spec_function_sst_private.application expression <> None ->
                  nested
              | Sst.Direct_call
                  {
                    call_form = Sst.Specification_call;
                    callee;
                    recursive = false;
                    _;
                  } ->
                  callee :: nested
              | _ -> nested
            in
            collect body.expression
          in
          let rec validate_helpers visited = function
            | [] -> Ok ()
            | callee :: rest -> (
                match member callee with
                | Some helper ->
                    let* visited = validate_closure visited helper in
                    validate_helpers visited rest
                | None ->
                    recursive_helper_error
                      "recursive helper closure contains an imported call")
          in
          validate_helpers [] direct_helpers
      | _ ->
          recursive_helper_error
            "recursive helper certificate root is not the issued recursive Spec"
  let expand_recursive_helper_certificate certificate =
    match certificate.helper_expanded_body with
    | Some expression -> Ok expression
    | None ->
        let* () = validate_recursive_helper_certificate certificate in
        let root_definition = certificate.helper_root.helper_definition in
        let root_body =
          match root_definition.Sst.body with
          | Sst.Recursive_spec_definition { body; _ } -> body.expression
          | _ -> assert false
        in
        let rec contains_helper_call expression =
          match expression.Sst.expression_desc with
          | Sst.Direct_call
              { call_form = Sst.Specification_call; recursive = false; _ } ->
              true
          | _ ->
              List.exists contains_helper_call
                (recursive_helper_expression_children expression)
        in
        (* Preserve the exact body when there is nothing to expand. *)
        if not (contains_helper_call root_body) then (
          certificate.helper_expanded_body <- Some root_body;
          Ok root_body)
        else
          let member id =
            List.find_opt
              (fun candidate ->
                same_function_id candidate.helper_definition.Sst.function_id id)
              certificate.helper_members
          in
          let maximum_binding_id =
            List.fold_left
              (fun maximum member ->
                let definition = member.helper_definition in
                let parameter_ids =
                  List.concat_map
                    (function
                      | Sst.Value_parameter parameter -> (
                          match parameter.pattern.pattern_desc with
                          | Sst.Bind binding -> [ binding.id ]
                          | _ -> [])
                      | Sst.Callback_parameter _ -> [])
                    definition.parameters
                in
                let body_ids =
                  match definition.body with
                  | Sst.Spec_definition body ->
                      recursive_helper_binding_ids body.expression
                  | Sst.Recursive_spec_definition { body; _ } ->
                      recursive_helper_binding_ids body.expression
                  | _ -> []
                in
                List.fold_left max maximum (parameter_ids @ body_ids))
              (-1) certificate.helper_members
          in
          let next_binding = ref (maximum_binding_id + 1) in
          let expansion_count = ref 0 in
          let fresh_binding (binding : Sst.binding) =
            let id = !next_binding in
            incr next_binding;
            { binding with Sst.id; name = binding.name ^ "$helper" }
          in
          let rec fresh_pattern environment (pattern : Sst.pattern) =
            match pattern.Sst.pattern_desc with
            | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
                (pattern, environment)
            | Sst.Bind binding ->
                let fresh = fresh_binding binding in
                ( { pattern with pattern_desc = Sst.Bind fresh },
                  (binding.id, fresh) :: environment )
            | Sst.Constructor_pattern (constructor, arguments) ->
                let arguments, environment =
                  List.fold_left
                    (fun (patterns, environment) pattern ->
                      let pattern, environment =
                        fresh_pattern environment pattern
                      in
                      (pattern :: patterns, environment))
                    ([], environment) arguments
                in
                ( {
                    pattern with
                    pattern_desc =
                      Sst.Constructor_pattern (constructor, List.rev arguments);
                  },
                  environment )
            | Sst.Record_pattern fields ->
                let fields, environment =
                  List.fold_left
                    (fun (fields, environment) (field, pattern) ->
                      let pattern, environment =
                        fresh_pattern environment pattern
                      in
                      ((field, pattern) :: fields, environment))
                    ([], environment) fields
                in
                ( {
                    pattern with
                    pattern_desc = Sst.Record_pattern (List.rev fields);
                  },
                  environment )
            | Sst.Tuple_pattern components ->
                let components, environment =
                  List.fold_left
                    (fun (components, environment) (label, pattern) ->
                      let pattern, environment =
                        fresh_pattern environment pattern
                      in
                      ((label, pattern) :: components, environment))
                    ([], environment) components
                in
                ( {
                    pattern with
                    pattern_desc = Sst.Tuple_pattern (List.rev components);
                  },
                  environment )
            | Sst.Unit_pattern | Sst.Owned_tree_cursor_pattern _
            | Sst.Or_pattern _ ->
                assert false
          and clone_expression ?(expand_helpers = true) environment expression =
            let clone = clone_expression ~expand_helpers environment in
            let clone_quantifier constructor quantifier =
              let binder = fresh_binding quantifier.Sst.quantifier_binder in
              let scoped =
                (quantifier.quantifier_binder.id, binder) :: environment
              in
              constructor
                {
                  Sst.quantifier_metadata =
                    Logic_quantifier_private.rebind ~binder_index:binder.id
                      ~binder_type:binder.typ quantifier.quantifier_metadata;
                  quantifier_binder = binder;
                  quantifier_body =
                    clone_expression ~expand_helpers scoped
                      quantifier.quantifier_body;
                  quantifier_trigger =
                    Option.map
                      (clone_expression ~expand_helpers:false scoped)
                      quantifier.quantifier_trigger;
                }
            in
            let expression_desc =
              match expression.Sst.expression_desc with
              | (Sst.Int_constant _ | Sst.Bool_constant _) as desc -> desc
              | Sst.Variable { binding; use_uniqueness } ->
                  let binding =
                    Option.value ~default:binding
                      (List.assoc_opt binding.id environment)
                  in
                  Sst.Variable { binding; use_uniqueness }
              | Sst.Let (bindings, body) ->
                  let bindings, environment =
                    List.fold_left
                      (fun (bindings, environment) (pattern, value) ->
                        let value = clone_expression environment value in
                        let pattern, environment =
                          fresh_pattern environment pattern
                        in
                        ((pattern, value) :: bindings, environment))
                      ([], environment) bindings
                  in
                  Sst.Let (List.rev bindings, clone_expression environment body)
              | Sst.If (condition, consequent, Some alternative) ->
                  Sst.If
                    (clone condition, clone consequent, Some (clone alternative))
              | Sst.Checked_arithmetic (operation, operands) ->
                  Sst.Checked_arithmetic (operation, List.map clone operands)
              | Sst.Compare (comparison, left, right) ->
                  Sst.Compare (comparison, clone left, clone right)
              | Sst.Boolean_not operand -> Sst.Boolean_not (clone operand)
              | Sst.Boolean_binary (operation, left, right) ->
                  Sst.Boolean_binary (operation, clone left, clone right)
              | Sst.Forall quantifier ->
                  clone_quantifier (fun value -> Sst.Forall value) quantifier
              | Sst.Exists quantifier ->
                  clone_quantifier (fun value -> Sst.Exists value) quantifier
              | Sst.Optional_absent -> Sst.Optional_absent
              | Sst.Optional_present payload ->
                  Sst.Optional_present (clone payload)
              | Sst.Optional_forward carrier ->
                  Sst.Optional_forward (clone carrier)
              | Sst.Constructor_value { constructor; arguments } ->
                  Sst.Constructor_value
                    { constructor; arguments = List.map clone arguments }
              | Sst.Record_value { record_type; fields } ->
                  Sst.Record_value
                    {
                      record_type;
                      fields =
                        List.map
                          (fun (field, value) -> (field, clone value))
                          fields;
                    }
              | Sst.Field_read { record; field } ->
                  Sst.Field_read { record = clone record; field }
              | Sst.Match (scrutinee, cases) ->
                  Sst.Match
                    ( clone scrutinee,
                      List.map
                        (fun (case : Sst.case) ->
                          let pattern, case_environment =
                            fresh_pattern environment case.case_pattern
                          in
                          {
                            case with
                            Sst.case_pattern = pattern;
                            case_guard =
                              Option.map
                                (clone_expression case_environment)
                                case.case_guard;
                            case_body =
                              clone_expression case_environment case.case_body;
                          })
                        cases )
              | Sst.Direct_call
                  ({
                     call_form = Sst.Specification_call;
                     callee;
                     arguments;
                     recursive = false;
                     _;
                   } as call) -> (
                  let arguments =
                    List.map
                      (fun argument ->
                        let label, argument =
                          Sst.require_value_argument argument
                        in
                        (label, clone_expression environment argument))
                      arguments
                  in
                  match if expand_helpers then member callee else None with
                  | Some helper ->
                      incr expansion_count;
                      instantiate_helper arguments helper
                  | None ->
                      Sst.Direct_call
                        {
                          call with
                          arguments =
                            List.map
                              (fun (label, value) ->
                                Sst.Value_argument { label; value })
                              arguments;
                        })
              | Sst.Direct_call call ->
                  Sst.Direct_call
                    {
                      call with
                      arguments =
                        List.map
                          (function
                            | Sst.Value_argument { label; value } ->
                                Sst.Value_argument
                                  {
                                    label;
                                    value = clone_expression environment value;
                                  }
                            | Sst.Callback_argument _ as argument -> argument)
                          call.arguments;
                    }
              | Sst.Tuple_value components ->
                  Sst.Tuple_value
                    (List.map
                       (fun (label, component) -> (label, clone component))
                       components)
              | Sst.Unit_constant | Sst.Field_write _
              | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
              | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
              | Sst.Mutable_write _ | Sst.Sequence _
              | Sst.If (_, _, None)
              | Sst.Callback_call _ | Sst.Callback_requires _
              | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
              | Sst.Use_type_invariant _ | Sst.Local_assert _
              | Sst.Proof_region _ | Sst.Old _
              | Sst.Symbolic_application _ ->
                  assert false
            in
            { expression with Sst.expression_desc }
          and instantiate_helper arguments helper =
            let definition = helper.helper_definition in
            let parameters =
              List.map Sst.require_value_parameter definition.Sst.parameters
            in
            let body =
              match definition.body with
              | Sst.Spec_definition body -> body.expression
              | _ -> assert false
            in
            let bindings, helper_environment =
              (* Fresh parameter lets evaluate caller actuals before the freshened
             body and implement simultaneous, capture-avoiding substitution. *)
              List.fold_left2
                (fun (bindings, environment) parameter (_, actual) ->
                  match parameter.Sst.pattern.pattern_desc with
                  | Sst.Bind binding ->
                      let fresh = fresh_binding binding in
                      let pattern =
                        {
                          parameter.pattern with
                          Sst.pattern_desc = Sst.Bind fresh;
                        }
                      in
                      ( (pattern, actual) :: bindings,
                        (binding.id, fresh) :: environment )
                  | _ -> assert false)
                ([], []) parameters arguments
            in
            let body = clone_expression helper_environment body in
            Sst.Let (List.rev bindings, body)
          in
          let expanded = clone_expression [] root_body in
          certificate.helper_expanded_body <- Some expanded;
          recursive_helper_expansions :=
            !recursive_helper_expansions + !expansion_count;
          Ok expanded
  let expanded_recursive_specification ~program ~definition =
    match find_recursive_helper_certificate ~program ~definition with
    | None ->
        recursive_helper_error
          "recursive helper closure lacks an exact direct-source certificate"
    | Some certificate -> expand_recursive_helper_certificate certificate
  let recursive_helper_closure_snapshot ~program ~definition =
    match find_recursive_helper_certificate ~program ~definition with
    | None ->
        recursive_helper_error
          "recursive helper closure lacks an exact direct-source certificate"
    | Some certificate ->
        let* expanded = expand_recursive_helper_certificate certificate in
        let members =
          certificate.helper_members
          |> List.filter (fun member ->
              member.helper_source_order
              <= certificate.helper_root.helper_source_order)
          |> List.map (fun member ->
              Printf.sprintf "%d:%s#%d:%s" member.helper_source_order
                member.helper_definition.Sst.function_id.function_name
                member.helper_definition.function_id.function_index
                (Marshal.to_string member.helper_definition
                   [ Marshal.No_sharing ]))
        in
        Ok
          (Digest.to_hex
             (Digest.string
                (String.concat "\000"
                   (certificate.helper_program_snapshot
                   :: Marshal.to_string expanded [ Marshal.No_sharing ]
                   :: members))))
  module Recursive_helper_for_testing = struct
    let expansion_count () = !recursive_helper_expansions
    let reset_expansion_count () = recursive_helper_expansions := 0
    let with_reordered_closure_copy ~program ~definition action =
      match find_recursive_helper_certificate ~program ~definition with
      | None -> None
      | Some certificate -> (
          let helpers =
            List.filter
              (fun member ->
                member.helper_role = Ordinary_direct_spec
                && (not member.helper_definition.Sst.recursive)
                && member.helper_source_order
                   < certificate.helper_root.helper_source_order)
              certificate.helper_members
          in
          match helpers with
          | earlier :: later :: _ ->
              let original = certificate.helper_adversary_edges in
              certificate.helper_adversary_edges <- Some [ (earlier, later) ];
              Fun.protect
                ~finally:(fun () ->
                  certificate.helper_adversary_edges <- original)
                (fun () -> Some (action ()))
          | [] | [ _ ] -> None)
    let with_raw_helper_copy ~program ~definition action =
      match find_recursive_helper_certificate ~program ~definition with
      | None -> None
      | Some certificate -> (
          match
            List.find_opt
              (fun member ->
                member.helper_role = Ordinary_direct_spec
                && not member.helper_definition.Sst.recursive)
              certificate.helper_members
          with
          | None -> None
          | Some helper ->
              let body =
                match helper.helper_definition.Sst.body with
                | Sst.Spec_definition body ->
                    Sst.Checked_exec
                      {
                        body =
                          {
                            Sst.stage = Sst.Runtime;
                            expression = body.expression;
                          };
                        provenance =
                          Sst.Raw_semantic_body
                            helper.helper_definition.Sst.span;
                      }
                | _ -> assert false
              in
              let original = certificate.helper_adversary_body in
              certificate.helper_adversary_body <- Some (helper, body);
              Fun.protect
                ~finally:(fun () ->
                  certificate.helper_adversary_body <- original)
                (fun () -> Some (action ())))
    let with_partial_helper_call ~program ~definition action =
      match find_recursive_helper_certificate ~program ~definition with
      | None -> None
      | Some certificate -> (
          match
            List.find_opt
              (fun member ->
                member.helper_role = Ordinary_direct_spec
                && (not member.helper_definition.Sst.recursive)
                && member.helper_definition.parameters <> [])
              certificate.helper_members
          with
          | None -> None
          | Some helper ->
              let original = certificate.helper_adversary_call in
              certificate.helper_adversary_call <-
                Some (certificate.helper_root, helper, []);
              Fun.protect
                ~finally:(fun () ->
                  certificate.helper_adversary_call <- original)
                (fun () -> Some (action ())))
  end
  type local_type = {
    resolved_paths : Path.t list;
    type_id : Sst.type_id;
    declaration : type_declaration;
  }
  type external_type_specification =
    | Ordinary_type
    | Authenticated_external_type
    | Malformed_external_type

  let external_type_specification declaration =
    let public_name = "verocaml.external_type_specification" in
    let retained_name =
      "verocaml.internal.external_type_specification.v1"
    in
    let relevant =
      List.filter
        (fun attribute ->
          String.equal attribute.Parsetree.attr_name.txt public_name
          || String.equal attribute.attr_name.txt retained_name)
        declaration.typ_attributes
    in
    match relevant with
    | [] -> Ordinary_type
    | [ attribute ]
      when String.equal attribute.attr_name.txt retained_name
           && attribute.attr_loc.loc_ghost
           && attribute.attr_name.loc.loc_ghost
           && attribute.attr_payload = Parsetree.PStr [] ->
        Authenticated_external_type
    | _ -> Malformed_external_type
  type constrained_module = {
    module_name : string;
    module_id : Ident.t;
    signature_declaration : module_type_declaration;
    constraint_location : Location.t;
    public_type_links : (type_declaration * local_type) list;
    public_functions : (value_description * top_function) list;
  }
  type aggregate_registry = {
    local_types : local_type list;
    parametric_adts : Parametric_adt_lowering_private.lowered list;
    ranked_types : Sst.type_id list;
    definitions : Sst.type_definition list;
    fields : (Uid.t * Sst.type_id * Sst.field_id) list;
    constructors : (Uid.t * Sst.type_id * Sst.constructor_id) list;
  }
  type owned_tree_root_version =
    | Known_owned_tree_version of int
    | Divergent_owned_tree_versions
  type imported_callable = {
    imported_path : string;
    imported_uid : string;
    imported_definition : Sst.function_definition;
    imported_signature : Parametric_signature_private.t;
    imported_broadcast_trigger_span : Diagnostic.span option;
  }
  type imported_broadcast_group = {
    imported_broadcast_group_path : string;
    imported_broadcast_target_paths : string list;
  }
  type imported_type = {
    imported_type_path : string;
    imported_type_uid : string;
    imported_type_definition : Sst.type_definition;
    imported_parametric_descriptor : Parametric_adt.t option;
  }
  type imported_rank_domain = {
    imported_rank_component :
      (Sst.type_id * string * string * Diagnostic.span) list;
    imported_rank_positive_children :
      (Sst.constructor_id
      * string
      * Sst.field_id
      * string
      * int list
      * Sst.type_id
      * string list)
      list;
    imported_rank_ground_witnesses : (Sst.constructor_id * string) list;
    imported_rank_actual_evidence : string list;
  }
  type imported_environment = {
    allow_public_parametric_signatures : bool;
    imported_callables : imported_callable list;
    imported_broadcast_groups : imported_broadcast_group list;
    imported_types : imported_type list;
    imported_rank_domains : imported_rank_domain list;
    external_target_specifications :
      External_target_specification_private.environment option;
  }
  let empty_imported_environment =
    {
      allow_public_parametric_signatures = false;
      imported_callables = [];
      imported_broadcast_groups = [];
      imported_types = [];
      imported_rank_domains = [];
      external_target_specifications = None;
    }
  let empty_callback_lowering_state = Typedtree_callback_private.create_state
  type lowering_context = {
    source_file : string;
    typing_environment : Env.t;
    proof_capture_artifact : proof_capture_artifact option;
    broadcast_scan : Typedtree_broadcast_private.t option;
    symbolic_scan : Typedtree_symbolic_private.t option;
    symbolic_definitions : (int * Sst.function_definition) list;
    imports : Cmt_input.import array;
    functions : top_function list;
    aggregates : aggregate_registry;
    current_function : top_function option;
    mutable next_binding : int;
    mutable binding_types : (int * Types.type_expr) list;
    mutable binding_locations : (int * Location.t) list;
    mutable proof_capture_type_hints : (Location.t * Types.type_expr) list;
    mutable seen_ghost_ids : string list;
    mutable contract_carriers : Callback_contract_private.carrier list;
    mutable logical_ghost_depth : int;
    mutable function_result_type : Types.type_expr option;
    mutable owned_tree_cursors : (int * Sst.owned_tree_cursor) list;
    mutable owned_tree_observation_paths :
      (int * Sst.binding * int * Sst.owned_tree_path_step list) list;
    mutable owned_tree_root_versions : (int * owned_tree_root_version) list;
    mutable shared_scalar_formal_roots : Sst.binding list;
    mutable shared_scalar_aliases : (int * Sst.binding list) list;
    mutable shared_scalar_next_epoch : int;
    owned_tree_candidate_roots : Sst.type_id list;
    imported : imported_environment;
    callbacks : Typedtree_callback_private.lowering_state;
  }
  let empty_aggregates =
    {
      local_types = [];
      parametric_adts = [];
      ranked_types = [];
      definitions = [];
      fields = [];
      constructors = [];
    }
  let function_environment function_ =
    function_.value_binding.Typedtree.vb_expr.Typedtree.exp_env
  let functions_environment = function
    | function_ :: _ -> function_environment function_
    | [] -> Env.empty
  let ranked_deeply_immutable_type aggregates type_id =
    let descriptors =
      List.map
        (fun item -> item.Parametric_adt_lowering_private.descriptor)
        aggregates.parametric_adts
    in
    let definition type_id =
      List.find_opt
        (fun (definition : Sst.type_definition) -> definition.type_id = type_id)
        aggregates.definitions
    in
    let rec deeply_immutable visiting = function
      | Sst.Unit | Sst.Bool | Sst.Int -> (true, false, false)
      | Sst.Parameter _ -> (false, false, false)
      | Sst.Application (constructor, arguments) -> (
          match Parametric_adt.find descriptors constructor with
          | Some descriptor
            when Result.is_ok
                   (Logical_adt_schema_private.instantiate ~descriptors
                      ~applications:
                        [
                          ( (Parametric_adt.type_id descriptor).type_index,
                            arguments );
                        ])
            ->
              (true, false, true)
          | Some _ | None -> (false, false, false))
      | Sst.Tuple components ->
          List.fold_left
            (fun (immutable, ranked, schema) (_, typ) ->
              let child_immutable, child_ranked, child_schema =
                deeply_immutable visiting typ
              in
              ( immutable && child_immutable,
                ranked || child_ranked,
                schema || child_schema ))
            (true, false, false) components
      | Sst.Aggregate nested -> (
          let directly_ranked = List.mem nested aggregates.ranked_types in
          if List.mem nested visiting then
            (directly_ranked, directly_ranked, false)
          else
            match definition nested with
            | None -> (false, false, false)
            | Some definition ->
                let fields =
                  match definition.type_kind with
                  | Sst.Record_definition fields -> fields
                  | Sst.Variant_definition constructors ->
                      List.concat_map
                        (fun constructor -> constructor.Sst.constructor_fields)
                        constructors
                in
                List.fold_left
                  (fun
                    (immutable, ranked, schema)
                    (field : Sst.field_definition)
                  ->
                    let child_immutable, child_ranked, child_schema =
                      deeply_immutable (nested :: visiting) field.field_type
                    in
                    ( immutable
                      && field.Sst.field_mutability = Sst.Immutable_field
                      && child_immutable,
                      ranked || child_ranked,
                      schema || child_schema ))
                  (true, directly_ranked, false) fields)
    in
    match deeply_immutable [] (Sst.Aggregate type_id) with
    | true, ranked, schema when ranked || schema -> true
    | _ -> false
  let span context location =
    Diagnostic.span_of_location ~fallback_file:context.source_file location
  let unsupported context (location : Location.t) construct =
    Error
      (Diagnostic.make (Diagnostic.Unsupported_construct construct)
         (span context location))
  let same_location = Callback_shape_private.same_location
  let find_ident ident bindings =
    List.find_opt (fun (candidate, _) -> Ident.same ident candidate) bindings
    |> Option.map snd
  let find_function_by_path path (functions : top_function list) =
    List.find_opt
      (fun (function_ : top_function) ->
        List.exists (Path.same path) function_.resolved_paths)
      functions
  let functions_by_path path (functions : top_function list) =
    List.filter
      (fun (function_ : top_function) ->
        List.exists (Path.same path) function_.resolved_paths)
      functions
  let find_local_type_by_path path (local_types : local_type list) =
    List.find_opt
      (fun (local : local_type) ->
        List.exists (Path.same path) local.resolved_paths)
      local_types
  let find_field_by_uid ?owner uid fields =
    List.find_opt
      (fun (candidate, candidate_owner, _) ->
        Uid.equal uid candidate
        &&
        match owner with
        | None -> true
        | Some owner -> owner = candidate_owner)
      fields
    |> Option.map (fun (_, _, field) -> field)
  let find_constructor_by_uid ?owner uid constructors =
    List.find_opt
      (fun (candidate, candidate_owner, _) ->
        Uid.equal uid candidate
        &&
        match owner with
        | None -> true
        | Some owner -> owner = candidate_owner)
      constructors
    |> Option.map (fun (_, _, constructor) -> constructor)
  let type_id_of_field (field : Sst.field_id) =
    match field.field_owner with
    | Sst.Record_owner type_id -> type_id
    | Sst.Constructor_owner constructor -> constructor.constructor_type
  let path_resolves_to context path module_name value =
    Spec_function_type_private.path_resolves_to
      ~imports:context.imports path module_name value
  let resolves_to_spec_carrier =
    Spec_function_type_private.resolves_to_spec_carrier
  let type_has_path = Callback_shape_private.compiler_type_has_path
  let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid
  let normalized_type_path environment path =
    try Env.normalize_type_path None environment path with Env.Error _ -> path
  let type_uid environment path =
    let path = normalized_type_path environment path in
    try Some (compiler_uid (Env.find_type path environment).Types.type_uid)
    with Not_found | Env.Error _ -> None
  let symbolic_context_is_logical context =
    context.logical_ghost_depth > 0
    ||
    match context.current_function with
    | Some
        {
          function_kind =
            ( Top_spec _ | Top_type_invariant _ | Top_recursive_spec _
            | Top_proof _ | Top_external_specification _
            | Top_external_body (Sst.Spec, _)
            | Top_external_body (Sst.Proof, _) );
          _;
        } ->
        true
    | Some { function_kind = Top_exec | Top_external_body (Sst.Exec, _); _ }
    | None ->
        false
  let normalized_type_with_substitutions context substitutions location typ =
    let binders =
      Option.fold ~none:[]
        ~some:(fun function_ -> function_.parametric_type_binders)
        context.current_function
    in
    let resolve path =
      match
        Parametric_adt_lowering_private.find_by_path
          context.aggregates.parametric_adts path
      with
      | Some item ->
          Spec_function_type_private.Parametric_application item.descriptor
      | None -> (
          match find_local_type_by_path path context.aggregates.local_types with
          | Some local when local.declaration.typ_params = [] ->
              Spec_function_type_private.Aggregate_application local.type_id
          | Some _ -> Spec_function_type_private.Polymorphic_application
          | None -> (
              let path_name = Path.name path in
              let normalized_path_name =
                Path.name
                  (normalized_type_path context.typing_environment path)
              in
              let path_uid = type_uid context.typing_environment path in
              let imported_matches =
                List.filter
                  (fun imported ->
                    let target_match =
                      match imported.imported_parametric_descriptor with
                      | Some descriptor -> (
                          match Parametric_adt.provenance descriptor with
                          | Parametric_adt.External _ ->
                              let constructor =
                                Parametric_adt.type_constructor descriptor
                              in
                              String.equal constructor.constructor_path
                                path_name
                              || String.equal constructor.constructor_path
                                   normalized_path_name
                              || Option.fold ~none:false
                                   ~some:
                                     (String.equal
                                        (Parametric_adt.compiler_uid descriptor))
                                   path_uid
                          | Parametric_adt.Local _ -> false)
                      | None -> false
                    in
                    target_match
                    || String.equal imported.imported_type_path path_name
                    || String.equal imported.imported_type_path
                         normalized_path_name
                    || Option.fold ~none:false
                         ~some:(String.equal imported.imported_type_uid)
                         path_uid)
                  context.imported.imported_types
              in
              match imported_matches with
              | [ { imported_parametric_descriptor = Some descriptor; _ } ] ->
                  Spec_function_type_private.Parametric_application descriptor
              | [ imported ] ->
                  Spec_function_type_private.Aggregate_application
                    imported.imported_type_definition.type_id
              | [] -> Spec_function_type_private.Unsupported_application
              | _ :: _ :: _ ->
                  Spec_function_type_private.Unsupported_application))
    in
    match
      Spec_function_type_private.lower_compiler_type ~substitutions ~binders
        ~logical:(symbolic_context_is_logical context) ~resolve typ
    with
    | Ok typ -> Ok typ
    | Error Parametric_lowering_private.Polymorphic_source_type ->
        unsupported context location Diagnostic.Unsupported_generic_use
    | Error Parametric_lowering_private.Higher_order_source_type ->
        unsupported context location Diagnostic.Higher_order_function
    | Error Parametric_lowering_private.Unsupported_source_type ->
        unsupported context location Diagnostic.Unsupported_type
  let normalized_type context location typ =
    let substitutions =
      Option.fold ~none:[]
        ~some:(fun function_ -> function_.type_substitutions)
        context.current_function
    in
    normalized_type_with_substitutions context substitutions location typ
  let optional_carrier context location payload =
    let descriptors =
      List.map
        (fun item -> item.Parametric_adt_lowering_private.descriptor)
        context.aggregates.parametric_adts
      @ List.filter_map
          (fun imported -> imported.imported_parametric_descriptor)
          context.imported.imported_types
    in
    match Parametric_adt.option_application descriptors payload with
    | Some carrier -> Ok carrier
    | None -> unsupported context location Diagnostic.Unsupported_type
  module Imported_specialization_for_testing = struct
    let accepts ~imported_path:_ ~definition_name:_ _argument_names = false
  end
  let parameter_label = Parametric_lowering_private.formal_label
  let lower_constant context location = function
    | Const_int value -> Ok (`Int (Z.of_int value))
    | _ -> unsupported context location Diagnostic.Unsupported_expression
  let boolean_or_unit_constructor typ name =
    if type_has_path typ Predef.path_bool then
      if String.equal name "true" then Some (`Bool true)
      else if String.equal name "false" then Some (`Bool false)
      else None
    else if type_has_path typ Predef.path_unit && String.equal name "()" then
      Some `Unit
    else None
  let fresh_binding context name typ source_type uniqueness location =
    let binding =
      {
        Sst.id = context.next_binding;
        name;
        typ;
        uniqueness;
        span = span context location;
      }
    in
    context.next_binding <- context.next_binding + 1;
    context.binding_types <- (binding.id, source_type) :: context.binding_types;
    context.binding_locations <-
      (binding.id, location) :: context.binding_locations;
    binding
  let imported_type_definition context type_id =
    context.imported.imported_types
    |> List.find_map (fun imported ->
        let definition = imported.imported_type_definition in
        if definition.Sst.type_id = type_id then Some definition else None)
  let imported_constructor context owner name =
    Option.bind (imported_type_definition context owner) (fun definition ->
        match definition.Sst.type_kind with
        | Sst.Record_definition _ -> None
        | Sst.Variant_definition constructors ->
            List.find_map
              (fun (constructor : Sst.constructor_definition) ->
                if String.equal constructor.constructor_id.constructor_name name
                then Some constructor.constructor_id
                else None)
              constructors)
  let imported_field context owner name =
    Option.bind (imported_type_definition context owner) (fun definition ->
        let fields =
          match definition.Sst.type_kind with
          | Sst.Record_definition fields -> fields
          | Sst.Variant_definition constructors ->
              List.concat_map
                (fun (constructor : Sst.constructor_definition) ->
                  constructor.constructor_fields)
                constructors
        in
        List.find_map
          (fun (field : Sst.field_definition) ->
            if String.equal field.field_id.field_name name then
              Some field.field_id
            else None)
          fields)
  let parametric_application_type_id context typ =
    match
      Parametric_adt_lowering_private.application_type_id
        context.aggregates.parametric_adts typ
    with
    | Some _ as owner -> owner
    | None -> (
        match typ with
        | Sst.Application (constructor, _) ->
            context.imported.imported_types
            |> List.find_map (fun imported ->
                Option.bind imported.imported_parametric_descriptor
                  (fun descriptor ->
                    if
                      Parametric_type.compare_constructor
                        (Parametric_adt.type_constructor descriptor)
                        constructor
                      = 0
                    then Some (Parametric_adt.type_id descriptor)
                    else None))
        | Sst.Aggregate owner -> Some owner
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> None)
  let field_for_description context typ description =
    let owner = parametric_application_type_id context typ in
    match
      find_field_by_uid ?owner description.Types.lbl_uid
        context.aggregates.fields
    with
    | Some _ as field -> field
    | None ->
        Option.bind owner (fun owner ->
            imported_field context owner description.Types.lbl_name)
  let constructor_for_description context typ description =
    let owner =
      match typ with
      | Sst.Aggregate owner -> Some owner
      | Sst.Application _ -> parametric_application_type_id context typ
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> None
    in
    match
      find_constructor_by_uid ?owner description.Types.cstr_uid
        context.aggregates.constructors
    with
    | Some constructor -> Some constructor
    | None ->
        Option.bind owner (fun owner ->
            imported_constructor context owner description.Types.cstr_name)
  let owned_tree_root_version context root =
    Option.value ~default:(Known_owned_tree_version 0)
      (List.assoc_opt root.Sst.id context.owned_tree_root_versions)
  let set_owned_tree_root_version context root version =
    context.owned_tree_root_versions <-
      (root.Sst.id, version)
      :: List.remove_assoc root.id context.owned_tree_root_versions
  let advance_owned_tree_root context root =
    match owned_tree_root_version context root with
    | Divergent_owned_tree_versions -> None
    | Known_owned_tree_version pre_version ->
        let successor_version = pre_version + 1 in
        set_owned_tree_root_version context root
          (Known_owned_tree_version successor_version);
        Some (pre_version, successor_version)
  let join_owned_tree_root_versions branches =
    let ids =
      branches |> List.concat_map (List.map fst) |> List.sort_uniq Int.compare
    in
    let version branch id =
      Option.value ~default:(Known_owned_tree_version 0)
        (List.assoc_opt id branch)
    in
    List.map
      (fun id ->
        let versions = List.map (fun branch -> version branch id) branches in
        match versions with
        | [] -> assert false
        | first :: rest
          when List.for_all (fun candidate -> candidate = first) rest ->
            (id, first)
        | _ -> (id, Divergent_owned_tree_versions))
      ids
  let find_owned_tree_cursor context binding =
    List.assoc_opt binding.Sst.id context.owned_tree_cursors
  let is_owned_tree_candidate_root context (root : Sst.binding) =
    match root.typ with
    | Sst.Aggregate type_id ->
        List.mem type_id context.owned_tree_candidate_roots
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
    | Sst.Application _ ->
        false
  let rec shared_scalar_type_mentions owner = function
    | Sst.Aggregate type_id -> type_id = owner
    | Sst.Tuple components ->
        List.exists
          (fun (_, typ) -> shared_scalar_type_mentions owner typ)
          components
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ | Sst.Application _ ->
        false
  let shared_scalar_field context type_id =
    let authenticated_local_shape =
      List.exists
        (fun (local : local_type) ->
          local.type_id = type_id
          && local.declaration.typ_params = []
          && local.declaration.typ_private = Asttypes.Public
          &&
          match local.declaration.typ_kind with
          | Ttype_record _ -> true
          | Ttype_abstract | Ttype_variant _ | Ttype_record_unboxed_product _
          | Ttype_open ->
              false)
        context.aggregates.local_types
    in
    match
      List.find_opt
        (fun (definition : Sst.type_definition) -> definition.type_id = type_id)
        context.aggregates.definitions
    with
    | Some
        {
          Sst.type_kind = Sst.Record_definition fields;
          representation = Sst.Revealed;
          _;
        }
      when authenticated_local_shape -> (
        let mutable_fields =
          List.filter
            (fun (field : Sst.field_definition) ->
              field.field_mutability = Sst.Mutable_field)
            fields
        in
        match mutable_fields with
        | [ field ]
          when field.field_type = Sst.Int
               && not
                    (List.exists
                       (fun (candidate : Sst.field_definition) ->
                         shared_scalar_type_mentions type_id
                           candidate.field_type)
                       fields) ->
            Some field
        | [] | _ :: _ -> None)
    | Some { Sst.type_kind = Sst.Variant_definition _; _ }
    | Some
        {
          Sst.type_kind = Sst.Record_definition _;
          representation = Sst.Abstract_with_evidence _;
          _;
        }
    | Some
        {
          Sst.type_kind = Sst.Record_definition _;
          representation = Sst.Revealed;
          _;
        }
    | None ->
        None
  let shared_scalar_alias_chain context binding =
    List.assoc_opt binding.Sst.id context.shared_scalar_aliases
  let source_parameter_is_explicitly_aliased authenticated_source_text
      (parameter : function_param) =
    match authenticated_source_text with
    | None -> false
    | Some source ->
        let start = parameter.fp_loc.loc_start.pos_cnum in
        let limit = parameter.fp_loc.loc_end.pos_cnum in
        let length = String.length source in
        if start < 0 || limit < start || limit > length then false
        else
          let rec skip_space index =
            if index < limit then
              match source.[index] with
              | ' ' | '\t' | '\r' | '\n' -> skip_space (index + 1)
              | _ -> index
            else index
          in
          let keyword = "aliased" in
          let keyword_length = String.length keyword in
          let rec search index =
            if index >= limit then false
            else if source.[index] <> '@' then search (index + 1)
            else
              let word_start = skip_space (index + 1) in
              if
                word_start + keyword_length > limit
                || String.sub source word_start keyword_length <> keyword
              then search (index + 1)
              else
                let word_end = word_start + keyword_length in
                word_end = limit
                ||
                match source.[word_end] with
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> false
                | _ -> true
          in
          search start
  let configure_shared_scalar_formals context authenticated_source_text
      source_parameters parameters =
    let aggregate_formals =
      List.combine source_parameters parameters
      |> List.filter_map
           (fun ((source_parameter : function_param), parameter) ->
             match parameter with
             | Sst.Callback_parameter _ -> None
             | Sst.Value_parameter parameter -> (
                 match parameter.pattern.pattern_desc with
                 | Sst.Bind
                     ({
                        typ = Sst.Aggregate type_id;
                        uniqueness = Sst.Definitely_aliased;
                        _;
                      } as binding)
                   when source_parameter_is_explicitly_aliased
                          authenticated_source_text source_parameter
                        && Option.is_some (shared_scalar_field context type_id)
                   ->
                     Some binding
                 | Sst.Wildcard | Sst.Bind _ | Sst.Owned_tree_cursor_pattern _
                 | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
                 | Sst.Tuple_pattern _ | Sst.Record_pattern _
                 | Sst.Constructor_pattern _ | Sst.Or_pattern _ ->
                     None))
    in
    let all_aggregate_formals =
      List.filter
        (function
          | Sst.Callback_parameter _ -> false
          | Sst.Value_parameter parameter -> (
              match parameter.pattern.typ with
              | Sst.Aggregate _ -> true
              | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
              | Sst.Application _ ->
                  false))
        parameters
    in
    match aggregate_formals with
    | (([ root ] as roots) | ([ root; _ ] as roots))
      when List.length all_aggregate_formals = List.length roots
           && List.for_all
                (fun (candidate : Sst.binding) -> candidate.typ = root.typ)
                roots ->
        context.shared_scalar_formal_roots <- roots;
        context.shared_scalar_aliases <-
          List.map (fun binding -> (binding.Sst.id, [ binding ])) roots
    | [] | _ :: _ ->
        context.shared_scalar_formal_roots <- [];
        context.shared_scalar_aliases <- []
  let register_shared_scalar_alias context pattern value =
    match (pattern.Sst.pattern_desc, value.Sst.expression_desc) with
    | Sst.Bind binding, Sst.Variable { binding = source; _ } -> (
        match shared_scalar_alias_chain context source with
        | Some chain
          when not
                 (List.exists
                    (fun candidate ->
                      String.equal candidate.Sst.name binding.Sst.name)
                    (List.concat_map snd context.shared_scalar_aliases)) ->
            context.shared_scalar_aliases <-
              (binding.id, chain @ [ binding ])
              :: List.remove_assoc binding.id context.shared_scalar_aliases
        | Some _ | None -> ())
    | _ -> ()
  let advance_shared_scalar_epoch context =
    if context.shared_scalar_next_epoch >= 2 then None
    else
      let predecessor = context.shared_scalar_next_epoch in
      let successor = predecessor + 1 in
      context.shared_scalar_next_epoch <- successor;
      Some (predecessor, successor)
  let symbolic_source context function_ =
    Option.bind context.symbolic_scan (fun scan ->
        Option.map
          (fun declaration -> (scan, declaration))
          (Typedtree_symbolic_private.find_binding scan function_.value_binding))
  let symbolic_candidates context path =
    functions_by_path path context.functions
    |> List.filter_map (fun function_ ->
           Option.map
             (fun (scan, declaration) ->
               ( function_,
                 scan,
                 declaration,
                 List.assoc_opt function_.function_id.function_index
                   context.symbolic_definitions ))
             (symbolic_source context function_))
  let imported_symbolic_candidates context path =
    let canonical_path = Path.name path in
    List.filter
      (fun imported ->
        String.equal imported.imported_path canonical_path
        && Symbolic_declaration_private.is_symbolic
             imported.imported_definition)
      context.imported.imported_callables
  let has_symbolic_candidate context path =
    symbolic_candidates context path <> []
    || imported_symbolic_candidates context path <> []
  let symbolic_use_diagnostic context kind location message =
    let classification =
      match kind with
      | `Authentication -> Diagnostic.Invalid_symbolic_authentication message
      | `Declaration -> Diagnostic.Invalid_symbolic_declaration message
      | `Application -> Diagnostic.Invalid_symbolic_application message
    in
    Diagnostic.make classification (span context location)
  let authenticate_symbolic_candidate context location path description =
    let local = symbolic_candidates context path in
    let imported = imported_symbolic_candidates context path in
    match (local, imported) with
    | _ :: _, _ :: _ ->
        Error
          (symbolic_use_diagnostic context `Authentication location
             "symbolic use resolves to both local and imported declarations")
    | [], [ imported ] ->
        if not (symbolic_context_is_logical context) then
          Error
            (symbolic_use_diagnostic context `Application location
               "symbolic declarations are unavailable to executable code")
        else
          Symbolic_declaration_private.authenticate_imported
            ~canonical_path:(Path.name path)
            ~value_uid:(compiler_uid description.Types.val_uid)
            imported.imported_definition
          |> Result.map (fun declaration ->
                 (imported.imported_definition.function_id, declaration))
          |> Result.map_error (fun message ->
                 symbolic_use_diagnostic context `Authentication location
                   message)
    | [], [] ->
        Error
          (symbolic_use_diagnostic context `Authentication location
             "symbolic use does not resolve to a declaration")
    | [], _ :: _ :: _ ->
        Error
          (symbolic_use_diagnostic context `Authentication location
             "symbolic use resolves to multiple imported declarations")
    | _ :: _, [] ->
        Typedtree_symbolic_private.authenticate_candidate
          ~logical:(symbolic_context_is_logical context)
          ~candidates:(fun _ -> local) ~path
          ~value_uid:(compiler_uid description.Types.val_uid) ~location
        |> Result.map (fun (function_, declaration) ->
               (function_.function_id, declaration))
        |> Result.map_error (fun error ->
               let kind =
                 match error.Typedtree_symbolic_private.use_error_kind with
                 | Authentication -> `Authentication
                 | Declaration -> `Declaration
                 | Application -> `Application
               in
               symbolic_use_diagnostic context kind error.location error.message)
  let normalize_value_path expression path =
    try
      Env.normalize_value_path (Some expression.exp_loc) expression.exp_env path
    with Env.Error _ -> path
  let specification_preclassification_services context bindings =
    {
      Typedtree_spec_function_private.logical =
        symbolic_context_is_logical context;
      normalize = normalize_value_path;
      symbolic = has_symbolic_candidate context;
      resolves_to =
        (fun path module_name name ->
          path_resolves_to context path module_name name);
      known =
        (fun path uid ->
          find_function_by_path path context.functions <> None
          || List.exists
               (fun imported ->
                 String.equal imported.imported_path (Path.name path)
                 && String.equal imported.imported_uid uid)
               context.imported.imported_callables
          || Option.fold ~none:false
               ~some:(fun environment ->
                 Option.is_some
                   (External_target_specification_private.find_summary
                      environment ~canonical_path:(Path.name path)
                      ~value_uid:uid))
               context.imported.external_target_specifications);
      local = (fun ident -> find_ident ident bindings <> None);
      callback =
        (fun ident ->
          List.exists
            (fun (candidate, _) -> Ident.same ident candidate)
            context.callbacks.callback_bindings);
      uid = (fun description -> compiler_uid description.Types.val_uid);
    }
  let symbolic_application_head context callee arguments =
    Typedtree_spec_function_private.symbolic_application_head
      (specification_preclassification_services context []) callee arguments
  let preclassify_application context bindings callee =
    Typedtree_spec_function_private.preclassify_application
      (specification_preclassification_services context bindings) callee
  let string_argument = function
    | { exp_desc = Texp_constant (Const_string (value, _, _)); _ } -> Some value
    | _ -> None
  let one_application_argument = function
    | [ (Nolabel, Arg (argument, _)) ] -> Some argument
    | _ -> None
  let ghost_call context name expression =
    match expression.exp_desc with
    | Texp_apply
        ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
          arguments,
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost" name ->
        Option.map
          (fun argument -> (callee, path, argument))
          (one_application_argument arguments)
    | _ -> None
  let parse_declaration_carrier_id role value =
    match String.split_on_char ':' value with
    | [
     "verocaml";
     candidate_role;
     "1";
     binding_start;
     binding_end;
     witness_start;
     witness_end;
     function_name;
    ] -> (
        if not (String.equal role candidate_role) then None
        else
          match
            ( int_of_string_opt binding_start,
              int_of_string_opt binding_end,
              int_of_string_opt witness_start,
              int_of_string_opt witness_end )
          with
          | ( Some binding_start,
              Some binding_end,
              Some witness_start,
              Some witness_end ) ->
              Some
                ( binding_start,
                  binding_end,
                  witness_start,
                  witness_end,
                  function_name )
          | _ -> None)
    | _ -> None
  let location_offsets location =
    ( location.Location.loc_start.Lexing.pos_cnum,
      location.Location.loc_end.Lexing.pos_cnum )
  let unit_thunk_body expression =
    match expression.exp_desc with
    | Texp_function
        {
          params =
            [
              {
                fp_kind =
                  Tparam_pat
                    { pat_desc = Tpat_construct (_, description, [], None); _ };
                fp_arg_label = Nolabel;
                fp_partial = Total;
                _;
              };
            ];
          body = Tfunction_body body;
          _;
        }
      when type_has_path description.Types.cstr_res Predef.path_unit
           && String.equal description.cstr_name "()" ->
        Some body
    | _ -> None
  let terminal_function_body expression =
    match expression.exp_desc with
    | Texp_function { body = Tfunction_body body; _ } -> Some body
    | _ -> None
  let authenticated_declaration_carrier context ~role ~ghost_name function_name
      binding_location expression =
    match expression.exp_desc with
    | Texp_apply
        ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
          [
            (Nolabel, Arg (id_expression, _));
            (Nolabel, Arg (definition_thunk, _));
          ],
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost" ghost_name -> (
        let malformed () =
          unsupported context expression.exp_loc Diagnostic.Malformed_ghost_call
        in
        if
          (not expression.exp_loc.Location.loc_ghost)
          || (not callee.exp_loc.loc_ghost)
          || (not id_expression.exp_loc.loc_ghost)
          || (not definition_thunk.exp_loc.loc_ghost)
          || (not (same_location callee.exp_loc expression.exp_loc))
          || (not (same_location id_expression.exp_loc expression.exp_loc))
          || not (same_location definition_thunk.exp_loc expression.exp_loc)
        then malformed ()
        else
          match
            (string_argument id_expression, unit_thunk_body definition_thunk)
          with
          | Some id, Some definition_body -> (
              match parse_declaration_carrier_id role id with
              | Some
                  ( binding_start,
                    binding_end,
                    witness_start,
                    witness_end,
                    carrier_function_name ) ->
                  let actual_binding_start, actual_binding_end =
                    location_offsets binding_location
                  in
                  let actual_witness_start, actual_witness_end =
                    location_offsets expression.exp_loc
                  in
                  if
                    binding_start = actual_binding_start
                    && binding_end = actual_binding_end
                    && witness_start = actual_witness_start
                    && witness_end = actual_witness_end
                    && String.equal function_name carrier_function_name
                  then
                    Ok
                      (Some
                         {
                           definition_body;
                           witness_location = expression.exp_loc;
                           recursive_visibility = None;
                         })
                  else malformed ()
              | None -> malformed ())
          | _ -> malformed ())
    | _ -> Ok None
  let external_body_carrier_role context expression =
    match expression.exp_desc with
    | Texp_apply
        ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
          (Nolabel, Arg (id_expression, _)) :: _,
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost" "external_body" -> (
        match string_argument id_expression with
        | Some id -> (
            match String.split_on_char ':' id with
            | "verocaml" :: "proof-external-body" :: _ ->
                ("proof-external-body", Sst.Proof)
            | _ -> ("external-body", Sst.Exec))
        | None -> ("external-body", Sst.Exec))
    | _ -> ("external-body", Sst.Exec)
  let authenticated_recursive_spec_carrier context function_name
      binding_location expression =
    match expression.exp_desc with
    | Texp_apply
        ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
          [
            (Nolabel, Arg (id_expression, _));
            (Nolabel, Arg (visibility_expression, _));
            (Nolabel, Arg (definition_thunk, _));
          ],
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost"
             "recursive_spec_definition" -> (
        let malformed () =
          unsupported context expression.exp_loc Diagnostic.Malformed_ghost_call
        in
        if
          (not expression.exp_loc.Location.loc_ghost)
          || (not callee.exp_loc.loc_ghost)
          || (not id_expression.exp_loc.loc_ghost)
          || (not visibility_expression.exp_loc.loc_ghost)
          || not definition_thunk.exp_loc.loc_ghost
        then malformed ()
        else
          match
            ( string_argument id_expression,
              string_argument visibility_expression,
              unit_thunk_body definition_thunk )
          with
          | Some id, Some visibility, Some definition_body -> (
              match parse_declaration_carrier_id "spec" id with
              | Some
                  ( binding_start,
                    binding_end,
                    witness_start,
                    witness_end,
                    carrier_function_name ) ->
                  let actual_binding_start, actual_binding_end =
                    location_offsets binding_location
                  in
                  let actual_witness_start, actual_witness_end =
                    location_offsets expression.exp_loc
                  in
                  let visibility =
                    match visibility with
                    | "opaque" -> Some `Opaque
                    | "revealed" -> Some `Revealed
                    | _ -> None
                  in
                  if
                    binding_start = actual_binding_start
                    && binding_end = actual_binding_end
                    && witness_start = actual_witness_start
                    && witness_end = actual_witness_end
                    && String.equal function_name carrier_function_name
                    && Option.is_some visibility
                  then
                    Ok
                      (Some
                         {
                           definition_body;
                           witness_location = expression.exp_loc;
                           recursive_visibility = visibility;
                         })
                  else malformed ()
              | None -> malformed ())
          | _ -> malformed ())
    | _ -> Ok None
  let authenticated_proof_region context expression =
    match expression.exp_desc with
    | Texp_apply
        ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
          [
            (Nolabel, Arg (id_expression, _));
            (Nolabel, Arg (definition_thunk, _));
          ],
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost" "proof_region" -> (
        let malformed () =
          unsupported context expression.exp_loc Diagnostic.Malformed_ghost_call
        in
        if
          (not expression.exp_loc.loc_ghost)
          || (not callee.exp_loc.loc_ghost)
          || (not id_expression.exp_loc.loc_ghost)
          || (not definition_thunk.exp_loc.loc_ghost)
          || (not (same_location callee.exp_loc expression.exp_loc))
          || (not (same_location id_expression.exp_loc expression.exp_loc))
          || not (same_location definition_thunk.exp_loc expression.exp_loc)
        then malformed ()
        else
          match
            (string_argument id_expression, unit_thunk_body definition_thunk)
          with
          | Some id, Some body -> (
              match String.split_on_char ':' id with
              | [ "verocaml"; "proof-region"; "1"; start_; end_ ] -> (
                  match (int_of_string_opt start_, int_of_string_opt end_) with
                  | Some start_, Some end_ ->
                      let actual_start, actual_end =
                        location_offsets expression.exp_loc
                      in
                      if start_ = actual_start && end_ = actual_end then
                        Ok (Some body)
                      else malformed ()
                  | _ -> malformed ())
              | _ -> malformed ())
          | _ -> malformed ())
    | _ -> Ok None
  let decode_hex value =
    let digit = function
      | '0' .. '9' as character -> Some (Char.code character - Char.code '0')
      | 'a' .. 'f' as character ->
          Some (10 + Char.code character - Char.code 'a')
      | _ -> None
    in
    if String.length value mod 2 <> 0 then None
    else
      let bytes = Bytes.create (String.length value / 2) in
      let rec loop source target =
        if source = String.length value then Some (Bytes.to_string bytes)
        else
          match (digit value.[source], digit value.[source + 1]) with
          | Some high, Some low ->
              Bytes.set bytes target (Char.chr ((high lsl 4) lor low));
              loop (source + 2) (target + 1)
          | _ -> None
      in
      loop 0 0
  let parse_offset_pair value =
    match String.split_on_char ',' value with
    | [ start_; end_ ] -> (
        match (int_of_string_opt start_, int_of_string_opt end_) with
        | Some start_, Some end_ when start_ >= 0 && end_ >= start_ ->
            Some (start_, end_)
        | _ -> None)
    | _ -> None
  let parse_proof_capture_manifest text =
    let prefix = "verocaml:proof-region-capture:1:" in
    if not (String.starts_with ~prefix text) then None
    else
      let body =
        String.sub text (String.length prefix)
          (String.length text - String.length prefix)
      in
      match String.split_on_char '|' body with
      | ( [ "issuer=ppx-v1"; callable; binding; body; region; slots ]
        | [
            "issuer=ppx-v1";
            callable;
            binding;
            body;
            region;
            slots;
            "kind=local-assert";
            _;
            _;
          ] ) as fields -> (
          let value prefix field =
            if String.starts_with ~prefix field then
              Some
                (String.sub field (String.length prefix)
                   (String.length field - String.length prefix))
            else None
          in
          let parse_slots value =
            if String.equal value "" then Some []
            else
              let rec loop parsed = function
                | [] -> Some (List.rev parsed)
                | entry :: rest -> (
                    match String.split_on_char ',' entry with
                    | [ name; start_; end_ ] -> (
                        match
                          ( decode_hex name,
                            int_of_string_opt start_,
                            int_of_string_opt end_ )
                        with
                        | Some name, Some start_, Some end_
                          when name <> "" && start_ >= 0 && end_ >= start_ ->
                            loop
                              ({
                                 proof_capture_slot_name = name;
                                 proof_capture_slot_start = start_;
                                 proof_capture_slot_end = end_;
                               }
                              :: parsed)
                              rest
                        | _ -> None)
                    | _ -> None)
              in
              loop [] (String.split_on_char ';' value)
          in
          let kind =
            match fields with
            | [ _; _; _; _; _; _ ] -> Some Captured_proof_region
            | [ _; _; _; _; _; _; "kind=local-assert"; ordinal; predicate ] ->
                Option.bind (value "ordinal=" ordinal) (fun ordinal ->
                    Option.bind (int_of_string_opt ordinal)
                      (fun assertion_ordinal ->
                        Option.map
                          (fun (predicate_start, predicate_end) ->
                            Captured_local_assert
                              {
                                assertion_ordinal;
                                predicate_start;
                                predicate_end;
                              })
                          (Option.bind
                             (value "predicate=" predicate)
                             parse_offset_pair)))
            | _ -> None
          in
          match
            ( value "callable=" callable,
              value "binding=" binding,
              value "body=" body,
              value "region=" region,
              value "slots=" slots,
              kind )
          with
          | ( Some callable,
              Some binding,
              Some body,
              Some region,
              Some slots,
              Some proof_capture_kind ) -> (
              match
                ( decode_hex callable,
                  parse_offset_pair binding,
                  parse_offset_pair body,
                  parse_offset_pair region,
                  parse_slots slots )
              with
              | ( Some proof_capture_callable_name,
                  Some (proof_capture_binding_start, proof_capture_binding_end),
                  Some (proof_capture_body_start, proof_capture_body_end),
                  Some (proof_capture_region_start, proof_capture_region_end),
                  Some proof_capture_slots ) ->
                  Some
                    {
                      proof_capture_callable_name;
                      proof_capture_binding_start;
                      proof_capture_binding_end;
                      proof_capture_body_start;
                      proof_capture_body_end;
                      proof_capture_region_start;
                      proof_capture_region_end;
                      proof_capture_slots;
                      proof_capture_kind;
                    }
              | _ -> None)
          | _ -> None)
      | _ -> None
  let retained_proof_region_pair context marker sidecar =
    match ghost_call context "marker" marker with
    | Some (marker_callee, _, marker_argument) -> (
        match string_argument marker_argument with
        | None -> None
        | Some proof_manifest_text -> (
            match sidecar.exp_desc with
            | Texp_ifthenelse
                ( {
                    exp_desc = Texp_construct (_, false_description, [], None);
                    _;
                  },
                  ignored,
                  Some
                    {
                      exp_desc = Texp_construct (_, unit_description, [], None);
                      _;
                    } )
              when type_has_path false_description.Types.cstr_res
                     Predef.path_bool
                   && String.equal false_description.cstr_name "false"
                   && type_has_path unit_description.Types.cstr_res
                        Predef.path_unit
                   && String.equal unit_description.cstr_name "()" -> (
                match ignored.exp_desc with
                | Texp_apply
                    ( { exp_desc = Texp_ident (ignore_path, _, _, _, _); _ },
                      ignore_arguments,
                      _,
                      _,
                      _ )
                  when path_resolves_to context ignore_path "Stdlib" "ignore"
                  -> (
                    match one_application_argument ignore_arguments with
                    | Some
                        {
                          exp_desc =
                            Texp_function
                              {
                                params = proof_shadow_parameters;
                                body =
                                  Tfunction_body
                                    {
                                      exp_desc =
                                        Texp_sequence
                                          ( sidecar_marker,
                                            _,
                                            proof_region_application );
                                      _;
                                    };
                                _;
                              };
                          _;
                        } -> (
                        match ghost_call context "sidecar" sidecar_marker with
                        | Some (sidecar_callee, _, sidecar_argument) -> (
                            match string_argument sidecar_argument with
                            | Some sidecar_text
                              when String.equal proof_manifest_text sidecar_text
                                   && marker.exp_loc.loc_ghost
                                   && marker_callee.exp_loc.loc_ghost
                                   && marker_argument.exp_loc.loc_ghost
                                   && sidecar.exp_loc.loc_ghost
                                   && sidecar_marker.exp_loc.loc_ghost
                                   && sidecar_callee.exp_loc.loc_ghost
                                   && sidecar_argument.exp_loc.loc_ghost ->
                                Some
                                  {
                                    Callback_call_private.proof_manifest_text;
                                    proof_shadow_parameters;
                                    proof_region_application;
                                  }
                            | Some _ | None -> None)
                        | None -> None)
                    | Some _ | None -> None)
                | _ -> None)
            | _ -> None))
    | None -> None
  let nested_local_capture_slots context expression =
    let slots = ref [] in
    let default = Tast_iterator.default_iterator in
    let add marker sidecar =
      match retained_proof_region_pair context marker sidecar with
      | Some retained -> (
          match parse_proof_capture_manifest retained.proof_manifest_text with
          | Some
              {
                proof_capture_kind = Captured_local_assert _;
                proof_capture_slots;
                _;
              } ->
              slots := List.rev_append proof_capture_slots !slots
          | Some { proof_capture_kind = Captured_proof_region; _ } | None -> ())
      | None -> ()
    in
    let iterator =
      {
        default with
        expr =
          (fun self expression ->
            (match expression.exp_desc with
            | Texp_sequence (marker, _, sidecar) -> (
                add marker sidecar;
                match sidecar.exp_desc with
                | Texp_sequence (nested_sidecar, _, _) ->
                    add marker nested_sidecar
                | _ -> ())
            | _ -> ());
            default.expr self expression);
      }
    in
    iterator.expr iterator expression;
    List.sort_uniq compare !slots
  let retained_ghost_pair context marker sidecar =
    match ghost_call context "marker" marker with
    | Some (_, _, marker_argument) -> (
        match string_argument marker_argument with
        | None -> None
        | Some marker_id -> (
            match sidecar.exp_desc with
            | Texp_ifthenelse
                ( {
                    exp_desc = Texp_construct (_, false_description, [], None);
                    _;
                  },
                  ignored,
                  Some
                    {
                      exp_desc = Texp_construct (_, unit_description, [], None);
                      _;
                    } )
              when type_has_path false_description.Types.cstr_res
                     Predef.path_bool
                   && String.equal false_description.cstr_name "false"
                   && type_has_path unit_description.Types.cstr_res
                        Predef.path_unit
                   && String.equal unit_description.cstr_name "()" -> (
                match ignored.exp_desc with
                | Texp_apply
                    ( { exp_desc = Texp_ident (ignore_path, _, _, _, _); _ },
                      ignore_arguments,
                      _,
                      _,
                      _ )
                  when path_resolves_to context ignore_path "Stdlib" "ignore"
                  -> (
                    match one_application_argument ignore_arguments with
                    | Some
                        {
                          exp_desc =
                            Texp_function
                              {
                                params = shadow_parameters;
                                body =
                                  Tfunction_body
                                    {
                                      exp_desc =
                                        Texp_sequence
                                          ( sidecar_marker,
                                            _,
                                            contract_application );
                                      _;
                                    };
                                _;
                              };
                          _;
                        } -> (
                        match ghost_call context "sidecar" sidecar_marker with
                        | Some (_, _, sidecar_argument) -> (
                            match string_argument sidecar_argument with
                            | Some sidecar_id
                              when String.equal marker_id sidecar_id ->
                                Some
                                  {
                                    Callback_contract_private.clause_id =
                                      marker_id;
                                    shadow_parameters;
                                    contract_application;
                                  }
                            | Some _ | None -> None)
                        | None -> None)
                    | Some _ | None -> None)
                | _ -> None)
            | _ -> None))
    | None -> None
  let binding_source_type context binding =
    List.assoc_opt binding.Sst.id context.binding_types
  let shadow_type_matches context location ~source_type ~expected_source
      expected =
    let rec exact seen left right =
      let left_id = Types.get_id left and right_id = Types.get_id right in
      if List.mem (left_id, right_id) seen then true
      else
        let seen = (left_id, right_id) :: seen in
        match (Types.get_desc left, Types.get_desc right) with
        | ( (Types.Tlink left | Types.Tsubst (left, _) | Types.Tpoly (left, [])),
            _ ) ->
            exact seen left right
        | ( _,
            (Types.Tlink right | Types.Tsubst (right, _)
            | Types.Tpoly (right, [])) ) ->
            exact seen left right
        | (Types.Tvar _, Types.Tvar _) | (Types.Tunivar _, Types.Tunivar _) ->
            left_id = right_id
        | Types.Tconstr (left, left_arguments, _), Types.Tconstr (right, right_arguments, _) ->
            Path.same left right
            && List.length left_arguments = List.length right_arguments
            && List.for_all2 (exact seen) left_arguments right_arguments
        | Types.Ttuple left, Types.Ttuple right ->
            List.length left = List.length right
            && List.for_all2
                 (fun (left_label, left) (right_label, right) ->
                   left_label = right_label && exact seen left right)
                 left right
        | ( Types.Tarrow (left_label, left_domain, left_range, _),
            Types.Tarrow (right_label, right_domain, right_range, _) ) ->
            left_label = right_label
            && exact seen left_domain right_domain
            && exact seen left_range right_range
        | _ -> false
    in
    let rec compatible seen substitutions source target =
      let source_id = Types.get_id source and target_id = Types.get_id target in
      if List.mem (source_id, target_id) seen then Some substitutions
      else
        let seen = (source_id, target_id) :: seen in
        match (Types.get_desc source, Types.get_desc target) with
        | ( (Types.Tlink source | Types.Tsubst (source, _)
            | Types.Tpoly (source, [])),
            _ ) ->
            compatible seen substitutions source target
        | ( _,
            (Types.Tlink target | Types.Tsubst (target, _)
            | Types.Tpoly (target, [])) ) ->
            compatible seen substitutions source target
        | (Types.Tvar _ | Types.Tunivar _), _ -> (
            match List.assoc_opt source_id substitutions with
            | None -> Some ((source_id, target) :: substitutions)
            | Some previous ->
                if exact [] previous target then Some substitutions else None)
        | _, (Types.Tvar _ | Types.Tunivar _) -> None
        | Types.Tconstr (source, source_arguments, _), Types.Tconstr (target, target_arguments, _)
          when Path.same source target
               && List.length source_arguments = List.length target_arguments ->
            List.fold_left2
              (fun result source target ->
                Option.bind result (fun substitutions ->
                    compatible seen substitutions source target))
              (Some substitutions) source_arguments target_arguments
        | Types.Ttuple source, Types.Ttuple target
          when List.length source = List.length target ->
            List.fold_left2
              (fun result (source_label, source) (target_label, target) ->
                if source_label <> target_label then None
                else
                  Option.bind result (fun substitutions ->
                      compatible seen substitutions source target))
              (Some substitutions) source target
        | ( Types.Tarrow (source_label, source_domain, source_range, _),
            Types.Tarrow (target_label, target_domain, target_range, _) )
          when source_label = target_label ->
            Option.bind
              (compatible seen substitutions source_domain target_domain)
              (fun substitutions ->
                compatible seen substitutions source_range target_range)
        | _ -> None
    in
    let compiler_match =
      Option.is_some (compatible [] [] source_type expected_source)
    in
    if compiler_match then Ok ()
    else
      match normalized_type context location source_type with
      | Ok actual
        when Parametric_type.equal actual expected
             || Parametric_type.alpha_equal actual expected ->
          Ok ()
      | Ok _ | Error _ ->
          unsupported context location Diagnostic.Malformed_ghost_call
  let exact_retained_family attributes =
    let prefix = "verocaml.internal.artifact_family." in
    attributes
    |> List.filter_map (fun attribute ->
        if
          attribute.Parsetree.attr_loc.Location.loc_ghost
          && attribute.attr_name.loc.loc_ghost
          && String.starts_with ~prefix attribute.attr_name.txt
        then
          Some
            (String.sub attribute.attr_name.txt (String.length prefix)
               (String.length attribute.attr_name.txt - String.length prefix))
        else None)
    |> fun families -> families = [ "retained-v1" ]
  let artifact_contains_function artifact function_ =
    let found = ref false in
    let default = Tast_iterator.default_iterator in
    let iterator =
      {
        default with
        value_binding =
          (fun self binding ->
            if binding == function_.value_binding then found := true;
            default.value_binding self binding);
      }
    in
    iterator.structure iterator
      artifact.proof_capture_implementation.Cmt_input.structure;
    !found
  let authenticated_proof_capture_artifact context function_ artifact =
    let implementation = artifact.proof_capture_implementation in
    artifact.proof_capture_artifact_issuer == proof_capture_artifact_issuer
    && artifact_contains_function artifact function_
    && String.equal implementation.source_file context.source_file
    && Option.is_some implementation.interface_digest
    && Option.is_some implementation.source_digest
    && implementation.has_implementation_shape
    && Cmt_input.retained_preprocessing implementation
    && exact_retained_family function_.value_binding.vb_attributes
  let proof_definition_body function_ =
    match function_.function_kind with
    | Top_spec carrier
    | Top_type_invariant carrier
    | Top_recursive_spec carrier
    | Top_proof carrier
    | Top_external_specification carrier
    | Top_external_body (_, carrier) ->
        Some carrier.definition_body
    | Top_exec -> (
        match function_.value_binding.vb_expr.exp_desc with
        | Texp_function { body = Tfunction_body body; _ } -> Some body
        | _ -> None)
  let free_typedtree_idents context expression =
    let bound = ref [] in
    let referenced = ref [] in
    let add_bound ident =
      if not (List.exists (Ident.same ident) !bound) then
        bound := ident :: !bound
    in
    let add_reference ident =
      if not (List.exists (Ident.same ident) !referenced) then
        referenced := ident :: !referenced
    in
    let default = Tast_iterator.default_iterator in
    let iterator =
      {
        default with
        pat =
          (fun (type k) self (pattern : k general_pattern) ->
            (match pattern.pat_desc with
            | Tpat_var (ident, _, _, _, _) -> add_bound ident
            | Tpat_alias (_, ident, _, _, _, _, _) -> add_bound ident
            | _ -> ());
            default.pat self pattern);
        expr =
          (fun self expression ->
            (match expression.exp_desc with
            | Texp_ident (Path.Pident ident, _, _, _, _) -> add_reference ident
            | Texp_mutvar ident -> add_reference ident.txt
            | _ -> ());
            default.expr self expression);
      }
    in
    iterator.expr iterator expression;
    List.filter
      (fun ident ->
        (not (Ident.is_global_or_predef ident))
        && (not
              (List.exists
                 (fun function_ -> Ident.same ident function_.ident)
                 context.functions))
        && not (List.exists (Ident.same ident) !bound))
      !referenced
  let authenticated_region_body context manifest expression =
    let malformed () =
      unsupported context expression.exp_loc Diagnostic.Malformed_ghost_call
    in
    match expression.exp_desc with
    | Texp_apply
        ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
          [
            (Nolabel, Arg (id_expression, _));
            (Nolabel, Arg (definition_thunk, _));
          ],
          _,
          _,
          _ )
      when path_resolves_to context path "Vero_ghost" "proof_region" -> (
        if
          (not expression.exp_loc.loc_ghost)
          || (not callee.exp_loc.loc_ghost)
          || (not id_expression.exp_loc.loc_ghost)
          || (not definition_thunk.exp_loc.loc_ghost)
          || (not (same_location callee.exp_loc expression.exp_loc))
          || (not (same_location id_expression.exp_loc expression.exp_loc))
          || not (same_location definition_thunk.exp_loc expression.exp_loc)
        then malformed ()
        else
          match
            (string_argument id_expression, unit_thunk_body definition_thunk)
          with
          | Some id, Some body ->
              let expected =
                match manifest.proof_capture_kind with
                | Captured_proof_region ->
                    Printf.sprintf "verocaml:proof-region:2:%d:%d"
                      manifest.proof_capture_region_start
                      manifest.proof_capture_region_end
                | Captured_local_assert
                    { assertion_ordinal; predicate_start; predicate_end } ->
                    Printf.sprintf "verocaml:local-assert:1:%d:%d:%d:%d:%d"
                      manifest.proof_capture_region_start
                      manifest.proof_capture_region_end assertion_ordinal
                      predicate_start predicate_end
              in
              let actual_start, actual_end =
                location_offsets expression.exp_loc
              in
              if
                String.equal id expected
                && actual_start = manifest.proof_capture_region_start
                && actual_end = manifest.proof_capture_region_end
              then
                match manifest.proof_capture_kind with
                | Captured_proof_region -> Ok body
                | Captured_local_assert { predicate_start; predicate_end; _ }
                  -> (
                    match ghost_call context "assert_" body with
                    | Some (assert_callee, _, predicate_thunk)
                      when body.exp_loc.loc_ghost
                           && assert_callee.exp_loc.loc_ghost
                           && predicate_thunk.exp_loc.loc_ghost
                           && same_location body.exp_loc expression.exp_loc
                           && same_location assert_callee.exp_loc
                                expression.exp_loc
                           && same_location predicate_thunk.exp_loc
                                expression.exp_loc -> (
                        match unit_thunk_body predicate_thunk with
                        | Some predicate
                          when location_offsets predicate.exp_loc
                               = (predicate_start, predicate_end) ->
                            Ok predicate
                        | Some _ | None -> malformed ())
                    | Some _ | None -> malformed ())
              else malformed ()
          | _ -> malformed ())
    | _ -> malformed ()
  let authenticate_retained_proof_region context bindings marker retained =
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    let* manifest =
      match parse_proof_capture_manifest retained.proof_manifest_text with
      | Some manifest -> Ok manifest
      | None -> malformed marker.exp_loc
    in
    let* function_, definition_body, artifact =
      match
        ( context.current_function,
          context.proof_capture_artifact,
          Option.bind context.current_function proof_definition_body )
      with
      | Some function_, Some artifact, Some definition_body
        when authenticated_proof_capture_artifact context function_ artifact ->
          Ok (function_, definition_body, artifact)
      | _ -> malformed marker.exp_loc
    in
    let binding_start, binding_end =
      location_offsets function_.value_binding.vb_loc
    in
    let body_start, body_end = location_offsets definition_body.exp_loc in
    let region_start, region_end = location_offsets marker.exp_loc in
    let* () =
      if
        String.equal manifest.proof_capture_callable_name
          (Ident.name function_.ident)
        && binding_start = manifest.proof_capture_binding_start
        && binding_end = manifest.proof_capture_binding_end
        && body_start = manifest.proof_capture_body_start
        && body_end = manifest.proof_capture_body_end
        && region_start = manifest.proof_capture_region_start
        && region_end = manifest.proof_capture_region_end
        && (not (List.mem retained.proof_manifest_text context.seen_ghost_ids))
        &&
        match (manifest.proof_capture_kind, function_.function_kind) with
        | Captured_proof_region, _ -> true
        | ( Captured_local_assert { assertion_ordinal; _ },
            (Top_proof _ | Top_exec) ) ->
            assertion_ordinal >= 0
        | Captured_local_assert _, _ -> false
      then Ok ()
      else malformed marker.exp_loc
    in
    let* body =
      authenticated_region_body context manifest
        retained.proof_region_application
    in
    let binding_location binding =
      List.assoc_opt binding.Sst.id context.binding_locations
    in
    let binding_for_slot slot =
      let candidates =
        List.filter_map
          (fun (ident, binding) ->
            match binding_location binding with
            | Some location ->
                let start_, end_ = location_offsets location in
                if
                  String.equal (Ident.name ident) slot.proof_capture_slot_name
                  && start_ = slot.proof_capture_slot_start
                  && end_ = slot.proof_capture_slot_end
                then Some binding
                else None
            | None -> None)
          bindings
      in
      match candidates with
      | [ binding ] -> Some binding
      | [] | _ :: _ :: _ -> None
    in
    let lower_shadow slot parameter =
      if
        parameter.fp_partial = Partial
        || parameter.fp_arg_label <> Nolabel
        || not parameter.fp_loc.loc_ghost
      then malformed parameter.fp_loc
      else
        match parameter.fp_kind with
        | Tparam_optional_default _ -> malformed parameter.fp_loc
        | Tparam_pat pattern -> (
            match (pattern.pat_desc, binding_for_slot slot) with
            | Tpat_var (ident, name, _, _, mode), Some live_binding -> (
                let parameter_start, parameter_end =
                  location_offsets parameter.fp_loc
                in
                if
                  not
                    (String.equal name.txt slot.proof_capture_slot_name
                    && parameter_start = slot.proof_capture_slot_start
                    && parameter_end = slot.proof_capture_slot_end
                    && uniqueness_of_pattern_mode mode = Sst.Definitely_aliased
                    )
                then malformed pattern.pat_loc
                else
                  match binding_source_type context live_binding with
                  | None -> malformed pattern.pat_loc
                  | Some live_type -> (
                      match
                        shadow_type_matches context pattern.pat_loc
                          ~source_type:pattern.pat_type
                          ~expected_source:live_type live_binding.typ
                      with
                      | Error _ as error -> error
                      | Ok () -> Ok (ident, live_binding)))
            | _ -> malformed pattern.pat_loc)
    in
    let slots = manifest.proof_capture_slots in
    let* shadow_bindings =
      match (slots, retained.proof_shadow_parameters) with
      | ( [],
          [
            {
              fp_partial = Total;
              fp_arg_label = Nolabel;
              fp_kind =
                Tparam_pat
                  { pat_desc = Tpat_construct (_, description, [], None); _ };
              fp_loc;
              _;
            };
          ] )
        when fp_loc.loc_ghost
             && type_has_path description.Types.cstr_res Predef.path_unit
             && String.equal description.Types.cstr_name "()" ->
          Ok []
      | [], _ -> malformed marker.exp_loc
      | _ :: _, parameters when List.length slots = List.length parameters ->
          let rec loop lowered slots parameters =
            match (slots, parameters) with
            | [], [] -> Ok (List.rev lowered)
            | slot :: slots, parameter :: parameters ->
                let* shadow = lower_shadow slot parameter in
                loop (shadow :: lowered) slots parameters
            | _ -> malformed marker.exp_loc
          in
          loop [] slots parameters
      | _ -> malformed marker.exp_loc
    in
    let live_bindings = List.map snd shadow_bindings in
    let live_ids =
      List.map (fun binding -> binding.Sst.id) live_bindings
      |> List.sort_uniq Int.compare
    in
    let shadow_idents = List.map fst shadow_bindings in
    let referenced = free_typedtree_idents context body in
    let nested_slots =
      match manifest.proof_capture_kind with
      | Captured_proof_region -> nested_local_capture_slots context body
      | Captured_local_assert _ -> []
    in
    let slot_is_nested slot = List.mem slot nested_slots in
    let same_ident_set left right =
      List.for_all (fun ident -> List.exists (Ident.same ident) right) left
      && List.for_all2
           (fun slot ident ->
             List.exists (Ident.same ident) left || slot_is_nested slot)
           slots right
    in
    let source_order =
      List.sort
        (fun left right ->
          Int.compare left.proof_capture_slot_start
            right.proof_capture_slot_start)
        slots
    in
    let* () =
      if
        List.length live_ids = List.length live_bindings
        && source_order = slots
        && same_ident_set referenced shadow_idents
        && not
             (Typedtree_spec_function_private.captures_live_binding bindings
                retained.proof_region_application)
      then Ok ()
      else malformed marker.exp_loc
    in
    let issued =
      {
        proof_capture_token = proof_capture_issuer;
        proof_capture_artifact = artifact;
        proof_capture_function = function_;
        proof_capture_manifest = manifest;
        proof_capture_bindings = live_bindings;
        proof_capture_shadow_idents = shadow_idents;
        proof_capture_program = None;
        proof_capture_expression = None;
      }
    in
    issued_proof_captures := issued :: !issued_proof_captures;
    incr proof_capture_issuance_count;
    incr proof_capture_remapping_count;
    context.seen_ghost_ids <-
      retained.proof_manifest_text :: context.seen_ghost_ids;
    Ok (body, shadow_bindings)
  let bind_issued_proof_capture_expression manifest_text expression =
    match parse_proof_capture_manifest manifest_text with
    | None -> ()
    | Some manifest ->
        List.find_opt
          (fun issued ->
            issued.proof_capture_token == proof_capture_issuer
            && issued.proof_capture_manifest = manifest
            && Option.is_none issued.proof_capture_program
            && Option.is_none issued.proof_capture_expression)
          !issued_proof_captures
        |> Option.iter (fun issued ->
            issued.proof_capture_expression <- Some expression)
  let prepare_proof_capture_type_hints context definition_body =
    let malformed location =
      Diagnostic.make
        (Diagnostic.Unsupported_construct Diagnostic.Malformed_ghost_call)
        (span context location)
    in
    match (context.current_function, context.proof_capture_artifact) with
    | Some function_, Some artifact
      when authenticated_proof_capture_artifact context function_ artifact -> (
        let source_patterns = ref [] in
        let default = Tast_iterator.default_iterator in
        let pattern_iterator =
          {
            default with
            pat =
              (fun (type k) self (pattern : k general_pattern) ->
                (match pattern.pat_desc with
                | Tpat_var (ident, name, _, _, _)
                  when not pattern.pat_loc.loc_ghost ->
                    source_patterns :=
                      (ident, name.txt, pattern.pat_loc, pattern.pat_type)
                      :: !source_patterns
                | _ -> ());
                default.pat self pattern);
          }
        in
        pattern_iterator.expr pattern_iterator function_.value_binding.vb_expr;
        let error = ref None in
        let hints = ref [] in
        let seen = ref [] in
        let set_error location =
          if Option.is_none !error then error := Some (malformed location)
        in
        let validate marker retained =
          match parse_proof_capture_manifest retained.proof_manifest_text with
          | None -> set_error marker.exp_loc
          | Some manifest -> (
              let binding_start, binding_end =
                location_offsets function_.value_binding.vb_loc
              in
              let body_start, body_end =
                location_offsets definition_body.exp_loc
              in
              let region_start, region_end = location_offsets marker.exp_loc in
              if
                not
                  (String.equal manifest.proof_capture_callable_name
                     (Ident.name function_.ident)
                  && binding_start = manifest.proof_capture_binding_start
                  && binding_end = manifest.proof_capture_binding_end
                  && body_start = manifest.proof_capture_body_start
                  && body_end = manifest.proof_capture_body_end
                  && region_start = manifest.proof_capture_region_start
                  && region_end = manifest.proof_capture_region_end
                  && not (List.mem retained.proof_manifest_text !seen))
              then set_error marker.exp_loc
              else
                match
                  authenticated_region_body context manifest
                    retained.proof_region_application
                with
                | Error diagnostic ->
                    if Option.is_none !error then error := Some diagnostic
                | Ok body -> (
                    let slots = manifest.proof_capture_slots in
                    let parameters = retained.proof_shadow_parameters in
                    let source_for_slot slot =
                      List.filter
                        (fun (_, name, location, _) ->
                          let start_, end_ = location_offsets location in
                          String.equal name slot.proof_capture_slot_name
                          && start_ = slot.proof_capture_slot_start
                          && end_ = slot.proof_capture_slot_end)
                        !source_patterns
                    in
                    let rec nonzero mapped slots parameters =
                      match (slots, parameters) with
                      | [], [] -> Some (List.rev mapped)
                      | slot :: slots, parameter :: parameters -> (
                          match (parameter.fp_kind, source_for_slot slot) with
                          | ( Tparam_pat
                                {
                                  pat_desc =
                                    Tpat_var
                                      (shadow_ident, shadow_name, _, _, mode);
                                  pat_type = shadow_type;
                                  _;
                                },
                              [ (source_ident, _, source_location, _) ] )
                            when parameter.fp_partial = Total
                                 && parameter.fp_arg_label = Nolabel
                                 && parameter.fp_loc.loc_ghost
                                 && String.equal shadow_name.txt
                                      slot.proof_capture_slot_name
                                 && uniqueness_of_pattern_mode mode
                                    = Sst.Definitely_aliased ->
                              nonzero
                                (( source_ident,
                                   source_location,
                                   shadow_ident,
                                   shadow_type )
                                :: mapped)
                                slots parameters
                          | _ -> None)
                      | _ -> None
                    in
                    let mapped =
                      match (slots, parameters) with
                      | ( [],
                          [
                            {
                              fp_partial = Total;
                              fp_arg_label = Nolabel;
                              fp_kind =
                                Tparam_pat
                                  {
                                    pat_desc =
                                      Tpat_construct (_, description, [], None);
                                    _;
                                  };
                              fp_loc;
                              _;
                            };
                          ] )
                        when fp_loc.loc_ghost
                             && type_has_path description.Types.cstr_res
                                  Predef.path_unit
                             && String.equal description.Types.cstr_name "()" ->
                          Some []
                      | [], _ -> None
                      | _ -> nonzero [] slots parameters
                    in
                    match mapped with
                    | None -> set_error marker.exp_loc
                    | Some mapped ->
                        let shadow_idents =
                          List.map (fun (_, _, shadow, _) -> shadow) mapped
                        in
                        let referenced = free_typedtree_idents context body in
                        let nested_slots =
                          match manifest.proof_capture_kind with
                          | Captured_proof_region ->
                              nested_local_capture_slots context body
                          | Captured_local_assert _ -> []
                        in
                        let exact_references =
                          List.for_all
                            (fun ident ->
                              List.exists (Ident.same ident) shadow_idents)
                            referenced
                          && List.for_all2
                               (fun slot shadow ->
                                 List.exists (Ident.same shadow) referenced
                                 || List.mem slot nested_slots)
                               slots shadow_idents
                        in
                        let live_occurrence =
                          let found = ref false in
                          let live_idents =
                            List.map (fun (source, _, _, _) -> source) mapped
                          in
                          let iterator =
                            {
                              default with
                              expr =
                                (fun self expression ->
                                  (match expression.exp_desc with
                                  | Texp_ident (Path.Pident ident, _, _, _, _)
                                    when List.exists (Ident.same ident)
                                           live_idents ->
                                      found := true
                                  | Texp_mutvar ident
                                    when List.exists (Ident.same ident.txt)
                                           live_idents ->
                                      found := true
                                  | _ -> ());
                                  default.expr self expression);
                            }
                          in
                          iterator.expr iterator
                            retained.proof_region_application;
                          !found
                        in
                        if (not exact_references) || live_occurrence then
                          set_error marker.exp_loc
                        else (
                          seen := retained.proof_manifest_text :: !seen;
                          hints :=
                            List.rev_append
                              (List.map
                                 (fun (_, location, _, typ) -> (location, typ))
                                 mapped)
                              !hints)))
        in
        let iterator =
          {
            default with
            expr =
              (fun self expression ->
                match expression.exp_desc with
                | Texp_sequence
                    ( { exp_desc = Texp_sequence (marker, _, sidecar); _ },
                      _,
                      tail ) -> (
                    match retained_proof_region_pair context marker sidecar with
                    | Some retained ->
                        validate marker retained;
                        self.expr self retained.proof_region_application;
                        self.expr self tail
                    | None -> default.expr self expression)
                | Texp_sequence (marker, _, sidecar) -> (
                    match retained_proof_region_pair context marker sidecar with
                    | Some retained -> (
                        match
                          parse_proof_capture_manifest
                            retained.proof_manifest_text
                        with
                        | Some
                            { proof_capture_kind = Captured_local_assert _; _ }
                          ->
                            validate marker retained
                        | Some { proof_capture_kind = Captured_proof_region; _ }
                        | None ->
                            set_error marker.exp_loc)
                    | None -> default.expr self expression)
                | _ -> (
                    match ghost_call context "marker" expression with
                    | Some (_, _, argument) -> (
                        match string_argument argument with
                        | Some value
                          when String.starts_with
                                 ~prefix:"verocaml:proof-region-capture:" value
                          ->
                            set_error expression.exp_loc
                        | Some _ | None -> default.expr self expression)
                    | None -> default.expr self expression));
          }
        in
        iterator.expr iterator definition_body;
        match !error with
        | Some diagnostic -> Error diagnostic
        | None ->
            context.proof_capture_type_hints <- !hints;
            Ok ())
    | Some _, Some _ | Some _, None | None, _ ->
        (* No proof carrier can be authenticated without the exact CMT-bound
         artifact. The ordinary/raw paths remain valid until a carrier is
         encountered by normal lowering. *)
        Ok ()
  let normalized_expression_type context bindings =
    Typedtree_spec_function_private.expression_type
      {
        fallback =
          (fun expression ->
            normalized_type context expression.exp_loc expression.exp_type);
        binding =
          (fun ident ->
            Option.map
              (fun (binding : Sst.binding) -> binding.typ)
              (find_ident ident bindings));
        symbolic =
          (fun callee arguments ->
            Option.map
              (fun (path, _, description, arguments) ->
                authenticate_symbolic_candidate context callee.exp_loc path
                  description
                |> Result.to_option
                |> Option.map (fun (_, declaration) ->
                       (declaration, arguments)))
              (symbolic_application_head context callee arguments));
        parameter_label;
        polymorphic_error =
          (fun expression ->
            Diagnostic.make
              (Diagnostic.Unsupported_construct Diagnostic.Unsupported_generic_use)
              (span context expression.exp_loc));
        higher_order_error =
          (fun expression ->
            Diagnostic.make
              (Diagnostic.Unsupported_construct
                 Diagnostic.Higher_order_function)
              (span context expression.exp_loc));
      }
  let hinted_source_type context location source_type =
    match Types.get_desc source_type with
    | Types.Tvar _ | Types.Tunivar _ ->
        Option.value ~default:source_type
          (List.find_map
             (fun (hint_location, hint_type) ->
               if same_location hint_location location then Some hint_type
               else None)
             context.proof_capture_type_hints)
    | _ -> source_type
  type owned_tree_cursor_origin = {
    cursor_root : Sst.binding;
    cursor_root_version : int;
    cursor_path : Sst.owned_tree_path_step list;
    cursor_runtime_authority : bool;
  }
  let rec lower_pattern_with_origin context bindings origin pattern =
    let source_pattern_type =
      hinted_source_type context pattern.pat_loc pattern.pat_type
    in
    let normalized =
      match (origin, pattern.pat_desc) with
      | Some { cursor_path = path; _ }, (Tpat_var _ | Tpat_any) -> (
          match List.rev path with
          | Sst.Owned_tree_constructor constructor :: _ ->
              Ok (Sst.Aggregate constructor.constructor_type)
          | _ -> normalized_type context pattern.pat_loc source_pattern_type)
      | _, Tpat_record ((_, description, _) :: _, _) -> (
          match normalized_type context pattern.pat_loc source_pattern_type with
          | Ok _ as normalized -> normalized
          | Error _ -> (
              match
                find_field_by_uid description.Types.lbl_uid
                  context.aggregates.fields
              with
              | Some field -> Ok (Sst.Aggregate (type_id_of_field field))
              | None ->
                  normalized_type context pattern.pat_loc source_pattern_type))
      | _ -> normalized_type context pattern.pat_loc source_pattern_type
    in
    match normalized with
    | Error _ as error -> error
    | Ok typ -> (
        let finish pattern_desc bindings =
          Ok
            ( { Sst.pattern_desc; typ; span = span context pattern.pat_loc },
              bindings )
        in
        match pattern.pat_desc with
        | Tpat_any -> finish Sst.Wildcard bindings
        | Tpat_var (ident, name, _, _, mode) -> (
            let binding =
              fresh_binding context name.txt typ source_pattern_type
                (match origin with
                | None -> uniqueness_of_pattern_mode mode
                | Some _ -> Sst.Definitely_aliased)
                pattern.pat_loc
            in
            match origin with
            | None -> finish (Sst.Bind binding) ((ident, binding) :: bindings)
            | Some ({ cursor_runtime_authority = false; _ } as origin) ->
                (* A read-only recursive-model binder carries only a statically
                 authenticated observation path.  It is deliberately lowered
                 as an ordinary aliased binding, never as mutation authority. *)
                context.owned_tree_observation_paths <-
                  ( binding.id,
                    origin.cursor_root,
                    origin.cursor_root_version,
                    origin.cursor_path )
                  :: context.owned_tree_observation_paths;
                finish (Sst.Bind binding) ((ident, binding) :: bindings)
            | Some origin ->
                let cursor =
                  {
                    Sst.cursor_binding = binding;
                    root = origin.cursor_root;
                    root_version = origin.cursor_root_version;
                    guarded_path = origin.cursor_path;
                  }
                in
                context.owned_tree_cursors <-
                  (binding.id, cursor) :: context.owned_tree_cursors;
                finish (Sst.Owned_tree_cursor_pattern cursor)
                  ((ident, binding) :: bindings))
        | Tpat_constant constant -> (
            match lower_constant context pattern.pat_loc constant with
            | Ok (`Int value) -> finish (Sst.Int_pattern value) bindings
            | Error _ as error -> error)
        | Tpat_tuple components ->
            let rec loop lowered bindings = function
              | [] -> finish (Sst.Tuple_pattern (List.rev lowered)) bindings
              | (label, pattern) :: rest -> (
                  match
                    lower_pattern_with_origin context bindings None pattern
                  with
                  | Error _ as error -> error
                  | Ok (pattern, bindings) ->
                      loop ((label, pattern) :: lowered) bindings rest)
            in
            loop [] bindings components
        | Tpat_construct (_, description, [], None) -> (
            match
              boolean_or_unit_constructor description.Types.cstr_res
                description.Types.cstr_name
            with
            | Some (`Bool value) -> finish (Sst.Bool_pattern value) bindings
            | Some `Unit -> finish Sst.Unit_pattern bindings
            | None -> (
                match constructor_for_description context typ description with
                | Some constructor ->
                    finish (Sst.Constructor_pattern (constructor, [])) bindings
                | None ->
                    unsupported context pattern.pat_loc Diagnostic.Aggregate))
        | Tpat_construct (_, description, arguments, None) -> (
            match constructor_for_description context typ description with
            | None -> unsupported context pattern.pat_loc Diagnostic.Aggregate
            | Some constructor ->
                let argument_origin =
                  Option.map
                    (fun origin ->
                      {
                        origin with
                        cursor_path =
                          origin.cursor_path
                          @ [ Sst.Owned_tree_constructor constructor ];
                      })
                    origin
                in
                let rec loop lowered bindings = function
                  | [] ->
                      finish
                        (Sst.Constructor_pattern (constructor, List.rev lowered))
                        bindings
                  | argument :: rest -> (
                      match
                        lower_pattern_with_origin context bindings
                          argument_origin argument
                      with
                      | Error _ as error -> error
                      | Ok (argument, bindings) ->
                          loop (argument :: lowered) bindings rest)
                in
                loop [] bindings arguments)
        | Tpat_construct (_, _, _, Some _) ->
            unsupported context pattern.pat_loc Diagnostic.Unsupported_generic_use
        | Tpat_record (fields, Asttypes.Closed) ->
            let rec loop lowered bindings = function
              | [] -> finish (Sst.Record_pattern (List.rev lowered)) bindings
              | (_, description, field_pattern) :: rest -> (
                  match field_for_description context typ description with
                  | None ->
                      unsupported context pattern.pat_loc Diagnostic.Aggregate
                  | Some field -> (
                      let field_origin =
                        Option.map
                          (fun origin ->
                            {
                              origin with
                              cursor_path =
                                origin.cursor_path
                                @ [ Sst.Owned_tree_field field ];
                            })
                          origin
                      in
                      match
                        lower_pattern_with_origin context bindings field_origin
                          field_pattern
                      with
                      | Error _ as error -> error
                      | Ok (field_pattern, bindings) ->
                          loop ((field, field_pattern) :: lowered) bindings rest
                      ))
            in
            loop [] bindings fields
        | Tpat_record (_, Asttypes.Open) ->
            unsupported context pattern.pat_loc Diagnostic.Partial_match
        | Tpat_variant _ ->
            unsupported context pattern.pat_loc Diagnostic.Aggregate
        | Tpat_array _ -> unsupported context pattern.pat_loc Diagnostic.Array
        | Tpat_alias _ | Tpat_or _ | Tpat_lazy _ | Tpat_record_unboxed_product _
        | Tpat_unboxed_tuple _ ->
            unsupported context pattern.pat_loc Diagnostic.Unsupported_pattern)
  let lower_pattern context bindings pattern =
    lower_pattern_with_origin context bindings None pattern
  let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
    left.type_index = right.type_index
    && String.equal left.type_name right.type_name
  let type_definition context type_id =
    match
      List.find_opt
        (fun (definition : Sst.type_definition) ->
          same_type_id definition.type_id type_id)
        context.aggregates.definitions
    with
    | Some definition -> Some definition
    | None -> imported_type_definition context type_id
  let field_definition context field =
    let fields =
      match field.Sst.field_owner with
      | Sst.Record_owner type_id -> (
          match type_definition context type_id with
          | Some { type_kind = Sst.Record_definition fields; _ } -> fields
          | _ -> [])
      | Sst.Constructor_owner constructor -> (
          match type_definition context constructor.constructor_type with
          | Some { type_kind = Sst.Variant_definition constructors; _ } ->
              Option.value ~default:[]
                (List.find_map
                   (fun (definition : Sst.constructor_definition) ->
                     if definition.constructor_id = constructor then
                       Some definition.constructor_fields
                     else None)
                   constructors)
          | _ -> [])
    in
    List.find_opt
      (fun (definition : Sst.field_definition) -> definition.field_id = field)
      fields
  let fields_of_owner context = function
    | Sst.Record_owner type_id -> (
        match type_definition context type_id with
        | Some { type_kind = Sst.Record_definition fields; _ } -> fields
        | _ -> [])
    | Sst.Constructor_owner constructor -> (
        match type_definition context constructor.constructor_type with
        | Some { type_kind = Sst.Variant_definition constructors; _ } ->
            Option.value ~default:[]
              (List.find_map
                 (fun (definition : Sst.constructor_definition) ->
                   if definition.constructor_id = constructor then
                     Some definition.constructor_fields
                   else None)
                 constructors)
        | _ -> [])
  let reconstruction_for_nested context (root : Sst.binding)
      (cursor : Sst.owned_tree_cursor) target_field =
    let target_layer =
      let fields = fields_of_owner context target_field.Sst.field_owner in
      let preserved_fields =
        fields
        |> List.filter_map (fun (definition : Sst.field_definition) ->
            if definition.field_id = target_field then None
            else Some definition.field_id)
      in
      match target_field.field_owner with
      | Sst.Record_owner record_type ->
          Sst.Reconstruct_record
            { record_type; changed_field = target_field; preserved_fields }
      | Sst.Constructor_owner constructor ->
          Sst.Reconstruct_record
            {
              record_type = constructor.constructor_type;
              changed_field = target_field;
              preserved_fields;
            }
    in
    let rec ancestors = function
      | [] -> []
      | Sst.Owned_tree_constructor constructor :: rest ->
          Sst.Reconstruct_constructor constructor :: ancestors rest
      | Sst.Owned_tree_field field :: rest ->
          let fields = fields_of_owner context field.field_owner in
          let preserved_fields =
            fields
            |> List.filter_map (fun (definition : Sst.field_definition) ->
                if definition.field_id = field then None
                else Some definition.field_id)
          in
          let layer =
            match field.field_owner with
            | Sst.Record_owner record_type ->
                Sst.Reconstruct_record
                  { record_type; changed_field = field; preserved_fields }
            | Sst.Constructor_owner constructor ->
                Sst.Reconstruct_record
                  {
                    record_type = constructor.constructor_type;
                    changed_field = field;
                    preserved_fields;
                  }
          in
          layer :: ancestors rest
    in
    let reversed_path = List.rev cursor.Sst.guarded_path in
    let layers = target_layer :: ancestors reversed_path in
    match root.Sst.typ with Sst.Aggregate _ -> layers | _ -> []
  let reconstruction_for_root context (root : Sst.binding) field =
    match root.Sst.typ with
    | Sst.Aggregate record_type ->
        let preserved_fields =
          fields_of_owner context (Sst.Record_owner record_type)
          |> List.filter_map (fun (definition : Sst.field_definition) ->
              if definition.field_id = field then None
              else Some definition.field_id)
        in
        [
          Sst.Reconstruct_record
            { record_type; changed_field = field; preserved_fields };
        ]
    | _ -> []
  let live_cursor_ids context root version =
    context.owned_tree_cursors
    |> List.filter_map (fun (_, (cursor : Sst.owned_tree_cursor)) ->
        if cursor.Sst.root.id = root.Sst.id && cursor.root_version = version
        then Some cursor.cursor_binding.id
        else None)
    |> List.sort_uniq Int.compare
  let retained_erased_expression (expression : Typedtree.expression) =
    List.exists
      (fun attribute ->
        attribute.Parsetree.attr_loc.Location.loc_ghost
        &&
        match attribute.attr_name.txt with
        | "verocaml.internal.instance_mode.expression.ghost"
        | "verocaml.internal.instance_mode.expression.tracked" ->
            true
        | _ -> false)
      expression.exp_attributes
  let planned_builtin_assertion context expression predicate keyword_location =
    match context.current_function with
    | None -> None
    | Some function_ -> (
        match
          List.filter
            (fun planned ->
              planned.builtin_source_expression == expression
              && planned.builtin_predicate_expression == predicate
              && same_location planned.builtin_keyword_location keyword_location
              && same_location planned.builtin_source_location
                   expression.exp_loc
              && same_location planned.builtin_predicate_location
                   predicate.exp_loc)
            function_.builtin_assertions
        with
        | [ planned ] -> Some (function_, planned)
        | [] | _ :: _ :: _ -> None)
  let callback_diagnostic context construct location =
    Diagnostic.make (Diagnostic.Unsupported_construct construct)
      (span context location)
  let callback_diagnostic_with_message context construct location message =
    callback_diagnostic context construct location
    |> Diagnostic.with_message message
  let callback_authentication_error context location message =
    Error
      (callback_diagnostic_with_message context Diagnostic.Callback_authentication
         location message)
  let callback_policy_error context location message =
    Error
      (callback_diagnostic_with_message context Diagnostic.Callback_policy
         location message)
  let explicit_top_level_callback_contract context function_ =
    Typedtree_callback_private.validate_top_level_contract
      ~retained_pair:(retained_ghost_pair context)
      ~application:(fun retained -> retained.contract_application)
      ~resolves:(path_resolves_to context)
      ~error:(fun location message ->
        callback_diagnostic_with_message context Diagnostic.Callback_contract
          location message)
      function_.value_binding
  let callback_caller_identity context =
    Typedtree_callback_private.caller_identity
      (Option.map
         (fun function_ -> function_.function_id)
         context.current_function)
  let callback_compilation_identity context location =
    match Typedtree_callback_private.compilation_identity context.callbacks with
    | Ok identity -> Ok identity
    | Error message -> callback_authentication_error context location message
  let seal_callback_binding context location binding =
    let caller_identity =
      Option.value ~default:"<none>" (callback_caller_identity context)
    in
    Typedtree_callback_private.seal_binding context.callbacks ~caller_identity
      ~span:(span context) location binding
  let fresh_callback_id context =
    Typedtree_callback_private.fresh_id_for_function context.callbacks
      (Option.map
         (fun function_ -> function_.function_id)
         context.current_function)
  let callback_shape context env location typ =
    match
      Typedtree_callback_private.shape ~env
        ~lower:(fun location typ -> normalized_type context location typ)
        ~location typ
    with
    | Ok shape -> Ok shape
    | Error (Typedtree_callback_private.Lowering_error error) -> Error error
    | Error (Typedtree_callback_private.Invalid_shape message) ->
        callback_policy_error context location message
  let callback_arrow_type = Typedtree_callback_private.callback_arrow_type
  let specification_capture_type context =
    Typedtree_spec_function_private.admissible_capture_type
      ~aggregate:(ranked_deeply_immutable_type context.aggregates)
  let issue_callback_formal context parameter pattern =
    let policy_error location message =
      callback_diagnostic_with_message context Diagnostic.Callback_policy
        location message
    in
    Typedtree_callback_private.issue_formal
      {
        shape = callback_shape context;
        compilation_identity = callback_compilation_identity context;
        fresh_id = (fun () -> fresh_callback_id context);
        owner_identity =
          Option.fold ~none:"<none>"
            ~some:(fun function_ ->
              Typedtree_callback_private.compiler_identity function_.ident)
            context.current_function;
        parameter_label;
        span = span context;
        policy_error;
      }
      context.callbacks parameter pattern
  let callback_application_binding context application =
    let caller_identity =
      Option.value ~default:"<none>" (callback_caller_identity context)
    in
    match
      Typedtree_callback_private.application_binding context.callbacks
        ~caller_identity ~span:(span context) application
    with
    | Ok binding -> Ok binding
    | Error message ->
        callback_authentication_error context application.exp_loc message
  let rec lower_authenticated_expression context bindings expression =
    match Broadcast.activation_body context.broadcast_scan expression with
    | Some body -> lower_authenticated_expression context bindings body
    | None ->
        let authenticated =
          Typedtree_logical_builtin_private.authenticate
            ~artifact:context.proof_capture_artifact
            ~source_file:context.source_file
            ~canonical_marker:(fun path ->
              path_resolves_to context path "Vero_ghost" "marker")
            expression
        in
        (match authenticated with
        | Error message ->
            callback_authentication_error context expression.exp_loc message
        | Ok (Some builtin) ->
            lower_logical_builtin context bindings expression builtin
        | Ok None -> lower_expression context bindings expression)
  and lower_expression context bindings expression =
    let preclassified =
      match expression.exp_desc with
      | Texp_match (_, _, _, Partial) -> Some Diagnostic.Partial_match
      | Texp_apply (callee, _, _, _, _) ->
          preclassify_application context bindings callee
      | Texp_function _ when not (symbolic_context_is_logical context) ->
          Some Diagnostic.Higher_order_function
      | Texp_function _ -> None
      | Texp_while _ | Texp_for _ | Texp_list_comprehension _
      | Texp_array_comprehension _ ->
          Some Diagnostic.Loop
      | Texp_try _ | Texp_letexception _ | Texp_unreachable
      | Texp_extension_constructor _ ->
          Some Diagnostic.Exception
      | Texp_array _ | Texp_idx _ -> Some Diagnostic.Array
      | Texp_object _ | Texp_send _ | Texp_new _ | Texp_instvar _ ->
          Some Diagnostic.Object
      | Texp_pack _ | Texp_letmodule _ -> Some Diagnostic.First_class_module
      | Texp_probe _ | Texp_probe_is_enabled _ -> Some Diagnostic.Effect
      | Texp_setinstvar _ | Texp_override _ | Texp_overwrite _ ->
          Some Diagnostic.Mutation
      | _ -> None
    in
    match preclassified with
    | Some construct -> unsupported context expression.exp_loc construct
    | None -> (
        let normalized =
          match expression.exp_desc with
          | Texp_assert _ -> Ok Sst.Unit
          | Texp_apply (callee, arguments, _, _, _) -> (
              match ghost_call context "old" expression with
              | Some (_, _, argument) ->
                  normalized_expression_type context bindings argument
              | None -> (
                  match
                    normalized_expression_type context bindings callee
                  with
                  | Ok arrow when Parametric_type.is_spec_function arrow ->
                      (match
                         Typedtree_spec_function_private.residual_type arrow
                           arguments
                       with
                      | Some typ -> Ok typ
                      | None ->
                          normalized_expression_type context bindings expression)
                  | Ok _ | Error _ ->
                      normalized_expression_type context bindings expression))
          | Texp_record { fields; _ } when Array.length fields > 0 -> (
              match normalized_expression_type context bindings expression with
              | Ok _ as normalized -> normalized
              | Error _ -> (
                  let description, _ = fields.(0) in
                  match
                    find_field_by_uid description.Types.lbl_uid
                      context.aggregates.fields
                  with
                  | Some field -> Ok (Sst.Aggregate (type_id_of_field field))
                  | None ->
                      normalized_type context expression.exp_loc
                        expression.exp_type))
          | _ -> normalized_expression_type context bindings expression
        in
        match normalized with
        | Error _ as error -> error
        | Ok typ -> (
            let finish expression_desc =
              Ok
                {
                  Sst.expression_desc;
                  typ;
                  span = span context expression.exp_loc;
                }
            in
            let lower = lower_authenticated_expression context bindings in
            match expression.exp_desc with
            | Texp_function
                {
                  params;
                  body = Tfunction_body body;
                  ret_mode = _;
                  alloc_mode = _;
                  _;
                } ->
                Typedtree_spec_function_private.lower_lambda
                  {
                    lower_pattern = lower_pattern context;
                    lower_expression =
                      lower_authenticated_expression context;
                    admit_capture =
                      (fun binding ->
                        specification_capture_type context binding.Sst.typ);
                    parameter_label;
                    span = span context;
                    higher_order_error =
                      (fun location ->
                        Diagnostic.make
                          (Diagnostic.Unsupported_construct
                             Diagnostic.Higher_order_function)
                          (span context location));
                  }
                  ~source_file:context.source_file ~bindings ~arrow:typ
                  ~location:expression.exp_loc ~parameters:params ~body
            | Texp_constant constant -> (
                match lower_constant context expression.exp_loc constant with
                | Ok (`Int value) -> finish (Sst.Int_constant value)
                | Error _ as error -> error)
            | Texp_ident (path, _, description, _, _)
              when
                has_symbolic_candidate context
                  (normalize_value_path expression path) ->
                lower_symbolic_identifier context expression typ path
                  description
            | Texp_ident (path, _, _, _, _)
              when
                symbolic_context_is_logical context
                && Parametric_type.is_spec_function typ
                &&
                (match path with
                | Path.Pident ident -> find_ident ident bindings = None
                | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> true) ->
                lower_spec_function_identifier context expression typ path
            | Texp_ident (Path.Pident ident, _, _, _, unique_use) -> (
                match find_ident ident bindings with
                | Some binding
                  when find_owned_tree_cursor context binding <> None ->
                    unsupported context expression.exp_loc Diagnostic.Mutation
                | Some binding ->
                    finish
                      (Sst.Variable
                         {
                           binding;
                           use_uniqueness = uniqueness_of_use unique_use;
                         })
                | None ->
                    unsupported context expression.exp_loc
                      Diagnostic.Unsupported_expression)
            | Texp_ident _ ->
                unsupported context expression.exp_loc
                  Diagnostic.Unknown_or_external_call
            | Texp_construct (_, description, [], None) -> (
                match
                  boolean_or_unit_constructor description.Types.cstr_res
                    description.Types.cstr_name
                with
                | Some (`Bool value) -> finish (Sst.Bool_constant value)
                | Some `Unit -> finish Sst.Unit_constant
                | None -> (
                    match
                      constructor_for_description context typ description
                    with
                    | Some constructor ->
                        finish
                          (Sst.Constructor_value { constructor; arguments = [] })
                    | None ->
                        unsupported context expression.exp_loc
                          Diagnostic.Aggregate))
            | Texp_construct (_, description, arguments, _) -> (
                match constructor_for_description context typ description with
                | None ->
                    unsupported context expression.exp_loc Diagnostic.Aggregate
                | Some constructor ->
                    let rec loop lowered = function
                      | [] ->
                          finish
                            (Sst.Constructor_value
                               { constructor; arguments = List.rev lowered })
                      | argument :: rest -> (
                          match
                            lower_authenticated_expression context bindings
                              argument
                          with
                          | Error _ as error -> error
                          | Ok argument
                            when Spec_function_type_private.contains argument.typ
                            ->
                              unsupported context expression.exp_loc
                                Diagnostic.Higher_order_function
                          | Ok argument -> loop (argument :: lowered) rest)
                    in
                    loop [] arguments)
            | Texp_record
                {
                  fields;
                  extended_expression = None;
                  representation = _;
                  alloc_mode = _;
                } -> (
                match parametric_application_type_id context typ with
                | Some record_type ->
                    let rec loop index lowered =
                      if index = Array.length fields then
                        finish
                          (Sst.Record_value
                             { record_type; fields = List.rev lowered })
                      else
                        let description, definition = fields.(index) in
                        match definition with
                        | Kept _ ->
                            unsupported context expression.exp_loc
                              Diagnostic.Aggregate
                        | Overridden (_, value) -> (
                            match
                              field_for_description context typ description
                            with
                            | None ->
                                unsupported context expression.exp_loc
                                  Diagnostic.Aggregate
                            | Some field -> (
                                match
                                  lower_authenticated_expression context
                                    bindings value
                                with
                                | Error _ as error -> error
                                | Ok value
                                  when
                                    Spec_function_type_private.contains value.typ
                                  ->
                                    unsupported context expression.exp_loc
                                      Diagnostic.Higher_order_function
                                | Ok value ->
                                    loop (index + 1) ((field, value) :: lowered)
                                ))
                    in
                    loop 0 []
                | None ->
                    unsupported context expression.exp_loc Diagnostic.Aggregate)
            | Texp_record { extended_expression = Some _; _ } ->
                unsupported context expression.exp_loc Diagnostic.Aggregate
            | Texp_field (record, _, _, description, _, _) -> (
                match
                  lower_authenticated_expression context bindings record
                with
                | Error _ as error -> error
                | Ok record -> (
                    match
                      field_for_description context record.typ description
                    with
                    | None ->
                        unsupported context expression.exp_loc
                          Diagnostic.Aggregate
                    | Some field -> finish (Sst.Field_read { record; field })))
            | Texp_setfield
                ( ({
                     exp_desc =
                       Texp_ident
                         (Path.Pident ident, _, _, _, receiver_unique_use);
                     _;
                   } as _receiver),
                  _,
                  _,
                  description,
                  value ) -> (
                match find_ident ident bindings with
                | None ->
                    unsupported context expression.exp_loc Diagnostic.Mutation
                | Some receiver_binding -> (
                    (* A set-field receiver is a non-consuming use in the pinned
                 compiler and therefore reports [aliased].  Admission rests
                 on the live binding pattern's definite [unique] mode; retain
                 the use mode in provenance so this compiler fact remains
                 reviewable. *)
                    let identifier_use_uniqueness =
                      uniqueness_of_use receiver_unique_use
                    in
                    if description.Types.lbl_private <> Asttypes.Public then
                      unsupported context expression.exp_loc Diagnostic.Mutation
                    else if not (Types.is_mutable description.Types.lbl_mut)
                    then
                      unsupported context expression.exp_loc Diagnostic.Mutation
                    else
                      match
                        find_field_by_uid description.Types.lbl_uid
                          context.aggregates.fields
                      with
                      | None ->
                          unsupported context expression.exp_loc
                            Diagnostic.Mutation
                      | Some field -> (
                          match
                            find_owned_tree_cursor context receiver_binding
                          with
                          | Some cursor -> (
                              let root = cursor.root in
                              let current_version =
                                owned_tree_root_version context root
                              in
                              let owner_matches_path =
                                match
                                  ( List.rev cursor.guarded_path,
                                    field.field_owner )
                                with
                                | ( Sst.Owned_tree_constructor selected :: _,
                                    Sst.Constructor_owner owner ) ->
                                    selected = owner
                                | _ -> false
                              in
                              match field_definition context field with
                              | Some target
                                when current_version
                                     = Known_owned_tree_version
                                         cursor.root_version
                                     && owner_matches_path
                                     && target.field_mutability
                                        = Sst.Mutable_field -> (
                                  match
                                    lower_authenticated_expression context
                                      bindings value
                                  with
                                  | Error _ as error -> error
                                  | Ok lowered_value -> (
                                      let ground =
                                        match
                                          lowered_value.Sst.expression_desc
                                        with
                                        | Sst.Int_constant _
                                        | Sst.Bool_constant _
                                        | Sst.Unit_constant
                                        | Sst.Constructor_value
                                            { arguments = []; _ } ->
                                            true
                                        | _ -> false
                                      in
                                      if not ground then
                                        unsupported context value.exp_loc
                                          Diagnostic.Mutation
                                      else
                                        match
                                          advance_owned_tree_root context root
                                        with
                                        | None ->
                                            unsupported context
                                              expression.exp_loc
                                              Diagnostic.Mutation
                                        | Some (pre_version, successor_version)
                                          ->
                                            let transition =
                                              {
                                                Sst.policy =
                                                  Sst
                                                  .Functional_owned_tree_no_heap;
                                                root;
                                                pre_version;
                                                successor_version;
                                                cursor = Some cursor;
                                                target_field = field;
                                                target_mutability =
                                                  target.field_mutability;
                                                target_modalities =
                                                  target.field_modalities;
                                                rhs_provenance =
                                                  Sst.Ground_owned_tree_value;
                                                reconstruction =
                                                  reconstruction_for_nested
                                                    context root cursor field;
                                                invalidated_cursor_ids =
                                                  live_cursor_ids context root
                                                    pre_version;
                                              }
                                            in
                                            finish
                                              (Sst.Owned_tree_nested_write
                                                 {
                                                   transition;
                                                   value = lowered_value;
                                                 })))
                              | _ ->
                                  unsupported context expression.exp_loc
                                    Diagnostic.Mutation)
                          | None -> (
                              let root = receiver_binding in
                              if
                                root.uniqueness = Sst.Definitely_aliased
                                && not (retained_erased_expression expression)
                              then
                                match
                                  ( root.typ,
                                    field.field_owner,
                                    shared_scalar_alias_chain context root,
                                    context.shared_scalar_formal_roots,
                                    context.current_function )
                                with
                                | ( Sst.Aggregate root_type,
                                    Sst.Record_owner owner,
                                    Some
                                      (({
                                          typ = Sst.Aggregate canonical_type;
                                          _;
                                        } as canonical_root)
                                       :: _ as alias_chain),
                                    (_ :: _ as formal_roots),
                                    Some function_ )
                                  when root_type = owner
                                       && canonical_type = root_type
                                       && List.for_all
                                            (fun (formal : Sst.binding) ->
                                              formal.typ = root.typ)
                                            formal_roots -> (
                                    match
                                      shared_scalar_field context root_type
                                    with
                                    | Some target
                                      when target.field_id = field
                                           && target.field_mutability
                                              = Sst.Mutable_field -> (
                                        match
                                          lower_authenticated_expression context
                                            bindings value
                                        with
                                        | Error _ as error -> error
                                        | Ok value -> (
                                            match
                                              advance_shared_scalar_epoch
                                                context
                                            with
                                            | None ->
                                                unsupported context
                                                  expression.exp_loc
                                                  Diagnostic.Mutation
                                            | Some
                                                ( predecessor_epoch,
                                                  successor_epoch ) ->
                                                finish
                                                  (Sst.Shared_scalar_field_write
                                                     {
                                                       provenance =
                                                         {
                                                           root;
                                                           binding_pattern_uniqueness =
                                                             root.uniqueness;
                                                           identifier_use_uniqueness;
                                                           field_is_local = true;
                                                           field_is_public =
                                                             true;
                                                           field_is_mutable =
                                                             true;
                                                         };
                                                       field;
                                                       value;
                                                       transition =
                                                         {
                                                           Sst.shared_policy =
                                                             Sst
                                                             .Bounded_shared_scalar_heap_v1;
                                                           shared_function_index =
                                                             function_
                                                               .function_id
                                                               .function_index;
                                                           shared_function_name =
                                                             function_
                                                               .function_id
                                                               .function_name;
                                                           shared_path_id = 0;
                                                           shared_record_type =
                                                             root_type;
                                                           shared_formal_roots =
                                                             formal_roots;
                                                           shared_target = root;
                                                           shared_canonical_root =
                                                             canonical_root;
                                                           shared_alias_chain =
                                                             alias_chain;
                                                           shared_target_field =
                                                             field;
                                                           shared_predecessor_epoch =
                                                             predecessor_epoch;
                                                           shared_successor_epoch =
                                                             successor_epoch;
                                                         };
                                                     })))
                                    | Some _ | None ->
                                        unsupported context expression.exp_loc
                                          Diagnostic.Mutation)
                                | _ ->
                                    unsupported context expression.exp_loc
                                      Diagnostic.Mutation
                              else if
                                root.uniqueness <> Sst.Definitely_unique
                                && not (retained_erased_expression expression)
                              then
                                unsupported context expression.exp_loc
                                  Diagnostic.Mutation
                              else
                                match (root.typ, field.field_owner) with
                                | ( Sst.Aggregate root_type,
                                    Sst.Record_owner owner )
                                  when root_type = owner -> (
                                    let moved_cursor =
                                      match value.exp_desc with
                                      | Texp_ident
                                          (Path.Pident moved, _, _, _, _) -> (
                                          match find_ident moved bindings with
                                          | Some binding ->
                                              find_owned_tree_cursor context
                                                binding
                                          | None -> None)
                                      | _ -> None
                                    in
                                    match moved_cursor with
                                    | Some cursor
                                      when cursor.root.id = root.id
                                           && owned_tree_root_version context
                                                root
                                              = Known_owned_tree_version
                                                  cursor.root_version
                                           && (match cursor.guarded_path with
                                             | Sst.Owned_tree_field source :: _
                                               ->
                                                 source = field
                                             | _ -> false)
                                           && cursor.cursor_binding.typ
                                              = Option.value ~default:Sst.Unit
                                                  (Option.map
                                                     (fun definition ->
                                                       definition.Sst.field_type)
                                                     (field_definition context
                                                        field)) -> (
                                        match
                                          advance_owned_tree_root context root
                                        with
                                        | None ->
                                            unsupported context
                                              expression.exp_loc
                                              Diagnostic.Mutation
                                        | Some (pre_version, successor_version)
                                          ->
                                            let target =
                                              Option.get
                                                (field_definition context field)
                                            in
                                            let transition =
                                              {
                                                Sst.policy =
                                                  Sst
                                                  .Functional_owned_tree_no_heap;
                                                root;
                                                pre_version;
                                                successor_version;
                                                cursor = None;
                                                target_field = field;
                                                target_mutability =
                                                  target.field_mutability;
                                                target_modalities =
                                                  target.field_modalities;
                                                rhs_provenance =
                                                  Sst.Guarded_descendant_move
                                                    cursor;
                                                reconstruction =
                                                  reconstruction_for_root
                                                    context root field;
                                                invalidated_cursor_ids =
                                                  live_cursor_ids context root
                                                    pre_version;
                                              }
                                            in
                                            finish
                                              (Sst.Owned_tree_rebase
                                                 { transition }))
                                    | Some _ ->
                                        unsupported context expression.exp_loc
                                          Diagnostic.Mutation
                                    | None -> (
                                        match
                                          lower_authenticated_expression context
                                            bindings value
                                        with
                                        | Error _ as error -> error
                                        | Ok value ->
                                            let ground =
                                              match
                                                value.Sst.expression_desc
                                              with
                                              | Sst.Int_constant _
                                              | Sst.Bool_constant _
                                              | Sst.Unit_constant
                                              | Sst.Constructor_value
                                                  { arguments = []; _ } ->
                                                  true
                                              | _ -> false
                                            in
                                            let exact_owned_contents_construction
                                                =
                                              match
                                                value.Sst.expression_desc
                                              with
                                              | Sst.Constructor_value
                                                  { arguments = _ :: _; _ } ->
                                                  true
                                              | Sst.Constructor_value
                                                  { arguments = []; _ }
                                              | _ ->
                                                  false
                                            in
                                            if
                                              is_owned_tree_candidate_root
                                                context root
                                              && ((not ground)
                                                  && not
                                                       exact_owned_contents_construction
                                                 || owned_tree_root_version
                                                      context root
                                                    = Divergent_owned_tree_versions
                                                 )
                                            then
                                              unsupported context
                                                expression.exp_loc
                                                Diagnostic.Mutation
                                            else
                                              let transition =
                                                if
                                                  is_owned_tree_candidate_root
                                                    context root
                                                then
                                                  match
                                                    field_definition context
                                                      field
                                                  with
                                                  | Some target ->
                                                      Option.map
                                                        (fun ( pre_version,
                                                               successor_version
                                                             ) ->
                                                          {
                                                            Sst.policy =
                                                              Sst
                                                              .Functional_owned_tree_no_heap;
                                                            root;
                                                            pre_version;
                                                            successor_version;
                                                            cursor = None;
                                                            target_field = field;
                                                            target_mutability =
                                                              target
                                                                .field_mutability;
                                                            target_modalities =
                                                              target
                                                                .field_modalities;
                                                            rhs_provenance =
                                                              Sst
                                                              .Ground_owned_tree_value;
                                                            reconstruction =
                                                              reconstruction_for_root
                                                                context root
                                                                field;
                                                            invalidated_cursor_ids =
                                                              live_cursor_ids
                                                                context root
                                                                pre_version;
                                                          })
                                                        (advance_owned_tree_root
                                                           context root)
                                                  | None -> None
                                                else None
                                              in
                                              finish
                                                (Sst.Field_write
                                                   {
                                                     provenance =
                                                       {
                                                         root;
                                                         binding_pattern_uniqueness =
                                                           root.uniqueness;
                                                         identifier_use_uniqueness;
                                                         field_is_local = true;
                                                         field_is_public = true;
                                                         field_is_mutable = true;
                                                       };
                                                     field;
                                                     value;
                                                     transition;
                                                   })))
                                | _ ->
                                    unsupported context expression.exp_loc
                                      Diagnostic.Mutation))))
            | Texp_setfield _ ->
                unsupported context expression.exp_loc Diagnostic.Mutation
            | Texp_letmutable (value_binding, body) -> (
                match value_binding.vb_pat.pat_desc with
                | Tpat_var _ -> (
                    match
                      lower_authenticated_expression context bindings
                        value_binding.vb_expr
                    with
                    | Error _ as error -> error
                    | Ok initial -> (
                        match
                          lower_pattern context bindings value_binding.vb_pat
                        with
                        | Error _ as error -> error
                        | Ok
                            ( ({ Sst.pattern_desc = Sst.Bind binding; _ } as
                               _pattern),
                              body_bindings ) -> (
                            if binding.uniqueness <> Sst.Definitely_unique then
                              unsupported context expression.exp_loc
                                Diagnostic.Mutation
                            else
                              match
                                lower_authenticated_expression context
                                  body_bindings body
                              with
                              | Error _ as error -> error
                              | Ok body ->
                                  finish
                                    (Sst.Let_mutable (binding, initial, body)))
                        | Ok _ ->
                            unsupported context expression.exp_loc
                              Diagnostic.Mutation))
                | _ ->
                    unsupported context expression.exp_loc Diagnostic.Mutation)
            | Texp_mutvar ident -> (
                match find_ident ident.txt bindings with
                | Some binding -> finish (Sst.Mutable_read binding)
                | None ->
                    unsupported context expression.exp_loc Diagnostic.Mutation)
            | Texp_setmutvar (ident, _, value) -> (
                match find_ident ident.txt bindings with
                | Some root when root.uniqueness = Sst.Definitely_unique -> (
                    match
                      lower_authenticated_expression context bindings value
                    with
                    | Error _ as error -> error
                    | Ok value ->
                        finish
                          (Sst.Mutable_write
                             {
                               provenance =
                                 {
                                   root;
                                   binding_pattern_uniqueness = root.uniqueness;
                                   identifier_use_uniqueness = root.uniqueness;
                                   field_is_local = true;
                                   field_is_public = true;
                                   field_is_mutable = true;
                                 };
                               value;
                             }))
                | Some _ | None ->
                    unsupported context expression.exp_loc Diagnostic.Mutation)
            | Texp_variant _ | Texp_record_unboxed_product _
            | Texp_unboxed_field _ | Texp_atomic_loc _ ->
                unsupported context expression.exp_loc Diagnostic.Aggregate
            | Texp_setinstvar _ | Texp_override _ | Texp_overwrite _ ->
                unsupported context expression.exp_loc Diagnostic.Mutation
            | Texp_tuple (components, _) ->
                let rec loop lowered = function
                  | [] -> finish (Sst.Tuple_value (List.rev lowered))
                  | (label, component) :: rest -> (
                      match
                        lower_authenticated_expression context bindings
                          component
                      with
                      | Error _ as error -> error
                      | Ok component
                        when
                          Spec_function_type_private.contains component.typ ->
                          unsupported context expression.exp_loc
                            Diagnostic.Higher_order_function
                      | Ok component ->
                          loop ((label, component) :: lowered) rest)
                in
                loop [] components
            | Texp_unboxed_tuple _ ->
                unsupported context expression.exp_loc
                  Diagnostic.Unsupported_expression
            | Texp_let
                ( Asttypes.Nonrecursive,
                  [
                    {
                      vb_pat = { pat_desc = Tpat_var (ident, name, _, _, _); _ };
                      vb_expr;
                      _;
                    };
                  ],
                  body )
              when callback_arrow_type vb_expr.exp_type
                   && not (symbolic_context_is_logical context)
                   &&
                   match vb_expr.exp_desc with
                   | Texp_function _ -> true
                   | _ -> false ->
                let* callback =
                  lower_local_callback context bindings ~ident ~name:name.txt
                    vb_expr
                in
                let outer_callbacks = context.callbacks.callback_bindings in
                context.callbacks.callback_bindings <-
                  (ident, callback) :: outer_callbacks;
                let lowered =
                  lower_authenticated_expression context bindings body
                in
                context.callbacks.callback_bindings <- outer_callbacks;
                lowered
            | Texp_let (Asttypes.Nonrecursive, [ { vb_expr; _ } ], _)
              when
                callback_arrow_type vb_expr.exp_type
                && not (symbolic_context_is_logical context) ->
                unsupported context vb_expr.exp_loc
                  Diagnostic.Higher_order_function
            | Texp_let (Asttypes.Nonrecursive, value_bindings, body) ->
                let rec lower_bindings lowered current_bindings = function
                  | [] -> (
                      match
                        lower_authenticated_expression context current_bindings
                          body
                      with
                      | Error _ as error -> error
                      | Ok body -> finish (Sst.Let (List.rev lowered, body)))
                  | binding :: rest -> (
                      match
                        lower_authenticated_expression context bindings
                          binding.vb_expr
                      with
                      | Error _ as error -> error
                      | Ok value -> (
                          match
                            lower_pattern context current_bindings
                              binding.vb_pat
                          with
                          | Error _ as error -> error
                          | Ok (pattern, current_bindings) ->
                              register_shared_scalar_alias context pattern value;
                              lower_bindings
                                ((pattern, value) :: lowered)
                                current_bindings rest))
                in
                lower_bindings [] bindings value_bindings
            | Texp_let (Asttypes.Recursive, _, _) ->
                unsupported context expression.exp_loc
                  Diagnostic.Higher_order_function
            | Texp_sequence (marker, _, sidecar)
              when Option.fold ~none:false
                     ~some:(fun retained ->
                       match
                         parse_proof_capture_manifest
                           retained.proof_manifest_text
                       with
                       | Some
                           { proof_capture_kind = Captured_local_assert _; _ }
                         ->
                           true
                       | Some { proof_capture_kind = Captured_proof_region; _ }
                       | None ->
                           false)
                     (retained_proof_region_pair context marker sidecar) -> (
                let retained =
                  Option.get (retained_proof_region_pair context marker sidecar)
                in
                match
                  authenticate_retained_proof_region context bindings marker
                    retained
                with
                | Error _ as error -> error
                | Ok (proof_body, shadow_bindings) -> (
                    context.logical_ghost_depth <-
                      context.logical_ghost_depth + 1;
                    let lowered =
                      lower_authenticated_expression context shadow_bindings
                        proof_body
                    in
                    context.logical_ghost_depth <-
                      context.logical_ghost_depth - 1;
                    match lowered with
                    | Error _ as error -> error
                    | Ok predicate ->
                        let assertion_ordinal =
                          match
                            parse_proof_capture_manifest
                              retained.proof_manifest_text
                          with
                          | Some
                              {
                                proof_capture_kind =
                                  Captured_local_assert
                                    { assertion_ordinal = ordinal; _ };
                                _;
                              } ->
                              ordinal
                          | Some
                              { proof_capture_kind = Captured_proof_region; _ }
                          | None ->
                              assert false
                        in
                        incr local_assertion_sst_count;
                        finish
                          (Sst.Local_assert { assertion_ordinal; predicate })))
            | Texp_sequence
                ( ({ exp_desc = Texp_sequence (marker, _, sidecar); _ } as first),
                  _,
                  tail ) -> (
                match retained_proof_region_pair context marker sidecar with
                | Some retained -> (
                    match
                      authenticate_retained_proof_region context bindings marker
                        retained
                    with
                    | Error _ as error -> error
                    | Ok (proof_body, shadow_bindings) -> (
                        context.logical_ghost_depth <-
                          context.logical_ghost_depth + 1;
                        let lowered =
                          lower_authenticated_expression context shadow_bindings
                            proof_body
                        in
                        context.logical_ghost_depth <-
                          context.logical_ghost_depth - 1;
                        match lowered with
                        | Error _ as error -> error
                        | Ok proof_body -> (
                            (match
                               parse_proof_capture_manifest
                                 retained.proof_manifest_text
                             with
                            | Some
                                {
                                  proof_capture_kind = Captured_proof_region;
                                  _;
                                } ->
                                incr proof_capture_sst_count
                            | Some
                                {
                                  proof_capture_kind = Captured_local_assert _;
                                  _;
                                } ->
                                incr local_assertion_sst_count
                            | None -> assert false);
                            let retained_statement =
                              {
                                Sst.expression_desc =
                                  (match
                                     retained.proof_manifest_text
                                     |> parse_proof_capture_manifest
                                   with
                                  | Some
                                      {
                                        proof_capture_kind =
                                          Captured_local_assert
                                            { assertion_ordinal; _ };
                                        _;
                                      } ->
                                      Sst.Local_assert
                                        {
                                          assertion_ordinal;
                                          predicate = proof_body;
                                        }
                                  | Some
                                      {
                                        proof_capture_kind =
                                          Captured_proof_region;
                                        _;
                                      } ->
                                      Sst.Proof_region proof_body
                                  | None -> assert false);
                                typ = Sst.Unit;
                                span = span context marker.exp_loc;
                              }
                            in
                            bind_issued_proof_capture_expression
                              retained.proof_manifest_text retained_statement;
                            match
                              lower_authenticated_expression context bindings
                                tail
                            with
                            | Error _ as error -> error
                            | Ok tail ->
                                finish (Sst.Sequence (retained_statement, tail))
                            )))
                | None -> (
                    match ghost_call context "marker" marker with
                    | Some _ ->
                        unsupported context marker.exp_loc
                          Diagnostic.Malformed_ghost_call
                    | None ->
                        let* first =
                          lower_authenticated_expression context bindings first
                        in
                        let* tail =
                          lower_authenticated_expression context bindings tail
                        in
                        finish (Sst.Sequence (first, tail))))
            | Texp_sequence
                ( marker,
                  _,
                  ({ exp_desc = Texp_sequence (sidecar, _, tail); _ } as
                   _retained_tail) ) -> (
                match retained_ghost_pair context marker sidecar with
                | Some retained -> (
                    match
                      lower_retained_ghost_clause context bindings marker
                        retained
                    with
                    | Error _ as error -> error
                    | Ok clause -> (
                        match
                          lower_authenticated_expression context bindings tail
                        with
                        | Error _ as error -> error
                        | Ok tail -> finish (Sst.Sequence (clause, tail))))
                | None -> (
                    match ghost_call context "marker" marker with
                    | Some _ ->
                        unsupported context marker.exp_loc
                          Diagnostic.Malformed_ghost_call
                    | None -> (
                        match lower marker with
                        | Error _ as error -> error
                        | Ok marker -> (
                            match
                              lower_authenticated_expression context bindings
                                _retained_tail
                            with
                            | Error _ as error -> error
                            | Ok tail -> finish (Sst.Sequence (marker, tail)))))
                )
            | Texp_sequence (first, _, second) -> (
                let* first =
                  match authenticated_proof_region context first with
                  | Ok (Some _) ->
                      unsupported context first.exp_loc
                        Diagnostic.Malformed_ghost_call
                  | Error _ as error -> error
                  | Ok None -> lower first
                in
                match lower second with
                | Error _ as error -> error
                | Ok second -> finish (Sst.Sequence (first, second)))
            | Texp_ifthenelse (condition, consequent, alternative) -> (
                match lower condition with
                | Error _ as error -> error
                | Ok condition -> (
                    let branch_root_versions =
                      context.owned_tree_root_versions
                    in
                    match lower consequent with
                    | Error _ as error -> error
                    | Ok consequent -> (
                        let consequent_root_versions =
                          context.owned_tree_root_versions
                        in
                        context.owned_tree_root_versions <- branch_root_versions;
                        match alternative with
                        | None ->
                            context.owned_tree_root_versions <-
                              join_owned_tree_root_versions
                                [
                                  consequent_root_versions; branch_root_versions;
                                ];
                            finish (Sst.If (condition, consequent, None))
                        | Some alternative -> (
                            match lower alternative with
                            | Error _ as error -> error
                            | Ok alternative ->
                                let alternative_root_versions =
                                  context.owned_tree_root_versions
                                in
                                context.owned_tree_root_versions <-
                                  join_owned_tree_root_versions
                                    [
                                      consequent_root_versions;
                                      alternative_root_versions;
                                    ];
                                finish
                                  (Sst.If
                                     (condition, consequent, Some alternative)))
                        )))
            | Texp_match (_, _, _, Partial) ->
                unsupported context expression.exp_loc Diagnostic.Partial_match
            | Texp_match (scrutinee, _, cases, Total) -> (
                match lower scrutinee with
                | Error _ as error -> error
                | Ok scrutinee ->
                    let cursor_origin =
                      match scrutinee.Sst.expression_desc with
                      | Sst.Field_read
                          {
                            record =
                              {
                                expression_desc =
                                  Sst.Variable { binding = root; _ };
                                _;
                              };
                            field;
                          }
                        when is_owned_tree_candidate_root context root
                             && (root.uniqueness = Sst.Definitely_unique
                                ||
                                match context.current_function with
                                | Some { function_kind = Top_spec _; _ } -> true
                                | Some
                                    {
                                      function_kind =
                                        ( Top_exec | Top_type_invariant _
                                        | Top_recursive_spec _ | Top_proof _
                                        | Top_external_specification _
                                        | Top_external_body _ );
                                      _;
                                    }
                                | None ->
                                    false) -> (
                          match owned_tree_root_version context root with
                          | Divergent_owned_tree_versions -> None
                          | Known_owned_tree_version cursor_root_version ->
                              Some
                                {
                                  cursor_root = root;
                                  cursor_root_version;
                                  cursor_path = [ Sst.Owned_tree_field field ];
                                  cursor_runtime_authority =
                                    root.uniqueness = Sst.Definitely_unique;
                                })
                      | Sst.Field_read
                          {
                            record =
                              {
                                expression_desc =
                                  Sst.Variable { binding = predecessor; _ };
                                _;
                              };
                            field;
                          } -> (
                          match
                            List.find_opt
                              (fun (binding_id, _, _, _) ->
                                binding_id = predecessor.id)
                              context.owned_tree_observation_paths
                          with
                          | Some
                              (_, cursor_root, cursor_root_version, cursor_path)
                            ->
                              Some
                                {
                                  cursor_root;
                                  cursor_root_version;
                                  cursor_path =
                                    cursor_path @ [ Sst.Owned_tree_field field ];
                                  cursor_runtime_authority = false;
                                }
                          | None -> None)
                      | _ -> None
                    in
                    let branch_root_versions =
                      context.owned_tree_root_versions
                    in
                    let rec lower_cases lowered lowered_root_versions = function
                      | [] ->
                          context.owned_tree_root_versions <-
                            join_owned_tree_root_versions
                              (List.rev lowered_root_versions);
                          finish (Sst.Match (scrutinee, List.rev lowered))
                      | case :: rest -> (
                          context.owned_tree_root_versions <-
                            branch_root_versions;
                          match case.c_lhs.pat_desc with
                          | Tpat_value value_pattern -> (
                              let value_pattern =
                                (value_pattern :> Typedtree.pattern)
                              in
                              match
                                lower_pattern_with_origin context bindings
                                  cursor_origin value_pattern
                              with
                              | Error _ as error -> error
                              | Ok (case_pattern, case_bindings) -> (
                                  match case.c_guard with
                                  | Some guard -> (
                                      match
                                        lower_authenticated_expression context
                                          case_bindings guard
                                      with
                                      | Error _ as error -> error
                                      | Ok case_guard -> (
                                          match
                                            lower_authenticated_expression
                                              context case_bindings case.c_rhs
                                          with
                                          | Error _ as error -> error
                                          | Ok case_body ->
                                              let case_root_versions =
                                                context.owned_tree_root_versions
                                              in
                                              lower_cases
                                                ({
                                                   Sst.case_pattern;
                                                   case_guard = Some case_guard;
                                                   case_body;
                                                   case_span =
                                                     span context
                                                       case.c_rhs.exp_loc;
                                                 }
                                                :: lowered)
                                                (case_root_versions
                                               :: lowered_root_versions)
                                                rest))
                                  | None -> (
                                      match
                                        lower_authenticated_expression context
                                          case_bindings case.c_rhs
                                      with
                                      | Error _ as error -> error
                                      | Ok case_body ->
                                          let case_root_versions =
                                            context.owned_tree_root_versions
                                          in
                                          lower_cases
                                            ({
                                               Sst.case_pattern;
                                               case_guard = None;
                                               case_body;
                                               case_span =
                                                 span context case.c_rhs.exp_loc;
                                             }
                                            :: lowered)
                                            (case_root_versions
                                           :: lowered_root_versions)
                                            rest)))
                          | Tpat_exception _ | Tpat_or _ ->
                              unsupported context case.c_lhs.pat_loc
                                Diagnostic.Exception)
                    in
                    lower_cases [] [] cases)
            | Texp_apply (callee, arguments, _, _, _) ->
                lower_application context bindings expression typ callee
                  arguments
            | Texp_assert (predicate, keyword_location) -> (
                match
                  planned_builtin_assertion context expression predicate
                    keyword_location
                with
                | None ->
                    unsupported context expression.exp_loc
                      Diagnostic.Malformed_ghost_call
                | Some (function_, planned) ->
                    let* predicate =
                      lower_authenticated_expression context bindings predicate
                    in
                    if typ <> Sst.Unit then
                      unsupported context expression.exp_loc
                        Diagnostic.Malformed_ghost_call
                    else
                      let lowered =
                        {
                          Sst.expression_desc =
                            Sst.Local_assert
                              {
                                assertion_ordinal =
                                  planned.builtin_assertion_ordinal;
                                predicate;
                              };
                          typ;
                          span = span context expression.exp_loc;
                        }
                      in
                      lowered_builtin_assertions :=
                        {
                          lowered_builtin_token = lowered_builtin_issuer;
                          lowered_builtin_function = function_;
                          lowered_builtin_source = planned;
                          lowered_builtin_expression = lowered;
                        }
                        :: !lowered_builtin_assertions;
                      incr local_assertion_sst_count;
                      Ok lowered)
            | Texp_function _ ->
                unsupported context expression.exp_loc
                  Diagnostic.Higher_order_function
            | Texp_while _ | Texp_for _ | Texp_list_comprehension _
            | Texp_array_comprehension _ ->
                unsupported context expression.exp_loc Diagnostic.Loop
            | Texp_try _ | Texp_letexception _ | Texp_unreachable
            | Texp_extension_constructor _ ->
                unsupported context expression.exp_loc Diagnostic.Exception
            | Texp_array _ | Texp_idx _ ->
                unsupported context expression.exp_loc Diagnostic.Array
            | Texp_object _ | Texp_send _ | Texp_new _ | Texp_instvar _ ->
                unsupported context expression.exp_loc Diagnostic.Object
            | Texp_pack _ | Texp_letmodule _ ->
                unsupported context expression.exp_loc
                  Diagnostic.First_class_module
            | Texp_letop _ ->
                unsupported context expression.exp_loc
                  Diagnostic.Unknown_or_external_call
            | Texp_probe _ | Texp_probe_is_enabled _ ->
                unsupported context expression.exp_loc Diagnostic.Effect
            | Texp_lazy _ | Texp_open _ | Texp_exclave _ | Texp_src_pos
            | Texp_hole _ | Texp_quotation _ | Texp_antiquotation _
            | Texp_eval _ ->
                unsupported context expression.exp_loc
                  Diagnostic.Unsupported_expression))
  and lower_retained_ghost_clause context bindings marker retained =
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    if List.mem retained.clause_id context.seen_ghost_ids then
      malformed marker.exp_loc
    else if
      Typedtree_spec_function_private.captures_live_binding
        ~allow:(fun (binding : Sst.binding) ->
          Parametric_type.is_spec_function binding.Sst.typ)
        bindings retained.contract_application
    then
      malformed marker.exp_loc
    else
      let lower_shadow parameter =
        if parameter.fp_partial = Partial || parameter.fp_arg_label <> Nolabel
        then malformed parameter.fp_loc
        else
          match parameter.fp_kind with
          | Tparam_optional_default _ -> malformed parameter.fp_loc
          | Tparam_pat pattern -> (
              match pattern.pat_desc with
              | Tpat_var (ident, name, _, _, mode) -> (
                  let shadow_uniqueness = uniqueness_of_pattern_mode mode in
                  let candidates =
                    List.filter
                      (fun (_, binding) ->
                        String.equal binding.Sst.name name.txt)
                      bindings
                  in
                  let specification_function_shadow =
                    match candidates with
                    | [ (_, (binding : Sst.binding)) ] ->
                        Parametric_type.is_spec_function binding.typ
                    | [] | _ :: _ :: _ -> false
                  in
                  if
                    shadow_uniqueness <> Sst.Definitely_aliased
                    && not specification_function_shadow
                  then malformed pattern.pat_loc
                  else
                    let callback_candidates =
                      List.filter
                        (fun (_, binding) ->
                          String.equal binding.Sst.callback_name name.txt)
                        context.callbacks.callback_bindings
                    in
                    match (candidates, callback_candidates) with
                    | [ (_, live_binding) ], []
                      when
                        Parametric_type.is_spec_function
                          live_binding.Sst.typ ->
                        Ok (`Value (ident, live_binding))
                    | [ (_, live_binding) ], [] -> (
                        match binding_source_type context live_binding with
                        | Some live_type -> (
                            match
                              shadow_type_matches context pattern.pat_loc
                                ~source_type:pattern.pat_type
                                ~expected_source:live_type live_binding.typ
                            with
                            | Ok () -> Ok (`Value (ident, live_binding))
                            | Error _ as error -> error)
                        | None -> malformed pattern.pat_loc)
                    | [], [ (_, live_callback) ] ->
                        Ok (`Callback (ident, live_callback))
                    | [], []
                    | [], _ :: _ :: _
                    | [ _ ], _ :: _
                    | _ :: _ :: _, []
                    | _ :: _ :: _, _ :: _ ->
                        malformed pattern.pat_loc)
              | _ -> malformed pattern.pat_loc)
      in
      let rec lower_shadows lowered = function
        | [] -> Ok (List.rev lowered)
        | parameter :: rest -> (
            match lower_shadow parameter with
            | Error _ as error -> error
            | Ok shadow -> lower_shadows (shadow :: lowered) rest)
      in
      let synthetic_unit_shadow =
        match retained.shadow_parameters with
        | [
         {
           fp_partial = Total;
           fp_arg_label = Nolabel;
           fp_kind =
             Tparam_pat
               { pat_desc = Tpat_construct (_, description, [], None); _ };
           _;
         };
        ]
          when type_has_path description.Types.cstr_res Predef.path_unit
               && String.equal description.Types.cstr_name "()" ->
            true
        | _ -> false
      in
      let lowered_shadows =
        if synthetic_unit_shadow then Ok []
        else lower_shadows [] retained.shadow_parameters
      in
      match lowered_shadows with
      | Error _ as error -> error
      | Ok shadow_bindings -> (
          let value_shadows, callback_shadows =
            List.fold_left
              (fun (values, callbacks) shadow ->
                match shadow with
                | `Value value -> (value :: values, callbacks)
                | `Callback callback -> (values, callback :: callbacks))
              ([], []) shadow_bindings
          in
          let value_shadows = List.rev value_shadows in
          let callback_shadows = List.rev callback_shadows in
          let shadow_ids =
            List.map (fun (_, binding) -> binding.Sst.id) value_shadows
            |> List.sort_uniq Int.compare
          in
          if List.length value_shadows <> List.length shadow_ids then
            malformed marker.exp_loc
          else
            match retained.contract_application.exp_desc with
            | Texp_apply
                ( ({ exp_desc = Texp_ident (path, _, _, _, _); _ } as callee),
                  arguments,
                  _,
                  _,
                  _ ) -> (
                let is_contract name =
                  path_resolves_to context path "Vero_ghost" name
                in
                if
                  not
                    (is_contract "requires" || is_contract "ensures"
                   || is_contract "decreases"
                    || is_contract "structural_decreases"
                    || is_contract "assert_")
                then malformed retained.contract_application.exp_loc
                else
                  let result_matches =
                    if not (is_contract "ensures") then Ok None
                    else
                      match
                        ( one_application_argument arguments,
                          context.function_result_type )
                      with
                      | ( Some
                            {
                              exp_desc =
                                Texp_function
                                  {
                                    params =
                                      [
                                        {
                                          fp_partial = Total;
                                          fp_kind =
                                            Tparam_pat
                                              ({
                                                 pat_desc =
                                                   Tpat_var _ | Tpat_any;
                                                 _;
                                               } as result_pattern);
                                          _;
                                        };
                                      ];
                                    body = Tfunction_body _;
                                    _;
                                  };
                              _;
                            },
                          Some function_result_type ) -> (
                          match
                            normalized_type context result_pattern.pat_loc
                              function_result_type
                          with
                          | Error _ ->
                              malformed result_pattern.pat_loc
                          | Ok function_type -> (
                              match
                                shadow_type_matches context
                                  result_pattern.pat_loc
                                  ~source_type:result_pattern.pat_type
                                  ~expected_source:function_result_type
                                  function_type
                              with
                              | Error _ as error -> error
                              | Ok () -> Ok (Some function_type)))
                      | _ -> malformed retained.contract_application.exp_loc
                  in
                  match result_matches with
                  | Error _ as error -> error
                  | Ok expected_binder_type ->
                      context.seen_ghost_ids <-
                        retained.clause_id :: context.seen_ghost_ids;
                      let outer_callbacks =
                        context.callbacks.callback_bindings
                      in
                      context.callbacks.callback_bindings <-
                        callback_shadows @ outer_callbacks;
                      let logical_function_bindings =
                        Typedtree_spec_function_private.function_bindings
                          bindings
                      in
                      let lowered =
                        lower_ghost_application context
                          (logical_function_bindings @ value_shadows)
                          retained.contract_application Sst.Unit callee
                          arguments path expected_binder_type
                      in
                      context.callbacks.callback_bindings <- outer_callbacks;
                      lowered)
            | _ -> malformed retained.contract_application.exp_loc)
  and local_callback_services context bindings =
    let diagnostic construct location message =
      Diagnostic.make (Diagnostic.Unsupported_construct construct)
        (span context location)
      |> Diagnostic.with_message message
    in
    {
      Typedtree_callback_private.shape = callback_shape context;
      lower_pattern = lower_pattern context;
      lower_body =
        (fun endpoint_bindings body ->
          let saved_carriers = context.contract_carriers in
          let saved_result_type = context.function_result_type in
          context.contract_carriers <- [];
          context.function_result_type <- Some body.exp_type;
          let lowered =
            lower_authenticated_expression context endpoint_bindings body
          in
          let carriers = context.contract_carriers in
          context.contract_carriers <- saved_carriers;
          context.function_result_type <- saved_result_type;
          let* body = lowered in
          Callback_contract_private.extract carriers body);
      parameter_label;
      find_value = (fun ident -> find_ident ident bindings);
      find_callback = Typedtree_callback_private.find_binding context.callbacks;
      source_type = binding_source_type context;
      immutable =
        Typedtree_callback_private.deeply_immutable
          ~parametric_adts:context.aggregates.parametric_adts
          ~definitions:context.aggregates.definitions;
      fresh_id = (fun () -> fresh_callback_id context);
      compilation_identity = callback_compilation_identity context;
      seal =
        (fun origin location ->
          let caller_identity =
            Option.value ~default:"<none>" (callback_caller_identity context)
          in
          Callback_certificate_private.seal_call_edge origin ~caller_identity
            ~call_edge_identity:
              (Typedtree_callback_private.call_edge_identity caller_identity
                 location));
      source_file = context.source_file;
      owner_name =
        Option.fold ~none:"<local>"
          ~some:(fun function_ -> function_.function_id.Sst.function_name)
          context.current_function;
      type_binders =
        Option.fold ~none:[]
          ~some:(fun function_ ->
            List.map snd function_.parametric_type_binders)
          context.current_function;
      span = span context;
      policy_error =
        (fun location message ->
          diagnostic Diagnostic.Callback_policy location message);
      authentication_error =
        (fun location message ->
          diagnostic Diagnostic.Callback_authentication location message);
      contract_error =
        (fun location message ->
          diagnostic Diagnostic.Callback_contract location message);
    }
  and lower_local_callback context bindings ?ident ~name expression =
    Typedtree_callback_private.lower_local
      (local_callback_services context bindings)
      context.callbacks bindings ?ident ~name expression
  and quantifier_lower_services context bindings =
    let diagnostic construct location message =
      Diagnostic.make (Diagnostic.Unsupported_construct construct)
        (span context location)
      |> Diagnostic.with_message message
    in
    let bind pattern =
      match pattern.pat_desc with
      | Tpat_var (ident, name, _, _, _) -> (
          match normalized_type context pattern.pat_loc pattern.pat_type with
          | Error _ as error -> error
          | Ok typ ->
              let binding =
                fresh_binding context name.txt typ pattern.pat_type
                  Sst.Definitely_aliased pattern.pat_loc
              in
              Ok (ident, binding))
      | Tpat_any | Tpat_alias _ | Tpat_constant _ | Tpat_construct _
      | Tpat_variant _ | Tpat_tuple _ | Tpat_record _ | Tpat_array _
      | Tpat_lazy _ | Tpat_or _ | Tpat_record_unboxed_product _
      | Tpat_unboxed_tuple _ ->
          Error
            (diagnostic Diagnostic.Quantifier_authentication pattern.pat_loc
               "quantifier binder is not a variable")
    in
    let nested_quantifier expression =
      match
        Typedtree_logical_builtin_private.authenticate
          ~artifact:context.proof_capture_artifact
          ~source_file:context.source_file
          ~canonical_marker:(fun path ->
            path_resolves_to context path "Vero_ghost" "marker")
          expression
      with
      | Ok (Some builtin) -> (
          match Typedtree_logical_builtin_private.kind builtin with
          | Typedtree_logical_builtin_private.Forall
          | Typedtree_logical_builtin_private.Exists ->
              true
          | Typedtree_logical_builtin_private.Call_requires
          | Typedtree_logical_builtin_private.Call_ensures ->
              false)
      | Ok None | Error _ -> false
    in
    let owner =
      Option.fold ~none:"<logical>"
        ~some:(fun function_ ->
          "function:" ^ function_.function_id.function_name)
        context.current_function
    in
    let shadowed kind =
      let name =
        match kind with
        | Typedtree_logical_builtin_private.Forall -> "forall"
        | Typedtree_logical_builtin_private.Exists -> "exists"
        | Typedtree_logical_builtin_private.Call_requires -> "call_requires"
        | Typedtree_logical_builtin_private.Call_ensures -> "call_ensures"
      in
      List.exists
        (fun (_, binding) -> String.equal binding.Sst.name name)
        bindings
      || List.exists
           (fun function_ ->
             String.equal function_.function_id.Sst.function_name name)
           context.functions
    in
    {
      Quantifier_validation_private.bind;
      lower =
        (fun ident binding expression ->
          lower_authenticated_expression context
            ((ident, binding) :: bindings)
            expression);
      admit_type =
        (function
        | Sst.Int | Sst.Bool | Sst.Parameter _ -> true
        | Sst.Application _ as typ
          when Parametric_type.is_spec_function typ ->
            true
        | Sst.Application _ as typ ->
            Typedtree_callback_private.deeply_immutable
              ~parametric_adts:context.aggregates.parametric_adts
              ~definitions:context.aggregates.definitions typ
        | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ -> false);
      nested_quantifier;
      owner;
      shadowed;
      span = span context;
      authentication_error =
        diagnostic Diagnostic.Quantifier_authentication;
      trigger_error = diagnostic Diagnostic.Quantifier_trigger;
      type_error = diagnostic Diagnostic.Quantifier_type;
    }
  and callback_lower_services context bindings =
    {
      Typedtree_logical_builtin_private.lower_expression =
        lower_authenticated_expression context bindings;
      application_binding = callback_application_binding context;
      parameter_label;
      span = span context;
      policy_error =
        (fun location message ->
          Diagnostic.make
            (Diagnostic.Unsupported_construct Diagnostic.Callback_policy)
            (span context location)
          |> Diagnostic.with_message message);
      authentication_error =
        (fun location message ->
          Diagnostic.make
            (Diagnostic.Unsupported_construct Diagnostic.Callback_authentication)
            (span context location)
          |> Diagnostic.with_message message);
      unsupported_quantifier =
        (fun location ->
          Diagnostic.make
            (Diagnostic.Unsupported_construct
               Diagnostic.Unsupported_logical_quantifier)
            (span context location));
    }
  and lower_logical_builtin context bindings expression builtin =
    match Typedtree_logical_builtin_private.kind builtin with
    | Typedtree_logical_builtin_private.Forall
    | Typedtree_logical_builtin_private.Exists as kind -> (
        match Typedtree_logical_builtin_private.quantifier builtin with
        | Some quantifier ->
            Quantifier_validation_private.lower
              (quantifier_lower_services context bindings)
              ~kind ~source:expression ~quantifier
        | None ->
            Error
              (Diagnostic.make
                 (Diagnostic.Unsupported_construct
                    Diagnostic.Quantifier_authentication)
                 (span context expression.exp_loc)))
    | Typedtree_logical_builtin_private.Call_requires
    | Typedtree_logical_builtin_private.Call_ensures ->
        Typedtree_logical_builtin_private.lower
          (callback_lower_services context bindings)
          ~expression builtin
  and lower_callback_application context bindings projection application result
      =
    Typedtree_logical_builtin_private.lower_application
      (callback_lower_services context bindings)
      projection application result
  and reveal_target_function context target_path target_type =
    ignore target_type;
    let candidates = functions_by_path target_path context.functions in
    match candidates with
    | [ candidate ] -> Some candidate
    | [] | _ :: _ :: _ -> None
  and lower_reveal_application context application result_type callee arguments
      name =
    let malformed () =
      unsupported context application.exp_loc Diagnostic.Malformed_ghost_call
    in
    let parsed =
      match (name, arguments) with
      | ( "reveal",
          [ (Nolabel, Arg (id_expression, _)); (Nolabel, Arg (target, _)) ] ) ->
          Option.map
            (fun id -> (id_expression, id, None, target))
            (string_argument id_expression)
      | ( "reveal_with_fuel",
          [
            (Nolabel, Arg (id_expression, _));
            (Nolabel, Arg (depth_expression, _));
            (Nolabel, Arg (target, _));
          ] ) -> (
          match
            (string_argument id_expression, string_argument depth_expression)
          with
          | Some id, Some depth ->
              Some (id_expression, id, Some (depth_expression, depth), target)
          | _ -> None)
      | _ -> None
    in
    match parsed with
    | None -> malformed ()
    | Some (id_expression, id, literal_depth, target) -> (
        let start_, end_ = location_offsets application.exp_loc in
        let expected_id =
          Printf.sprintf "verocaml:%s:1:%d:%d" name start_ end_
        in
        if
          result_type <> Sst.Unit
          || (not application.exp_loc.loc_ghost)
          || (not callee.exp_loc.loc_ghost)
          || (not id_expression.exp_loc.loc_ghost)
          || (not (String.equal id expected_id))
          ||
          match literal_depth with
          | Some (depth_expression, _) -> not depth_expression.exp_loc.loc_ghost
          | None -> false
        then malformed ()
        else
          match target.exp_desc with
          | Texp_ident (target_path, _, _, _, _) -> (
              match
                reveal_target_function context target_path target.exp_type
              with
              | Some target_function ->
                  Ok
                    {
                      Sst.expression_desc =
                        (match literal_depth with
                        | None -> Sst.Reveal target_function.function_id
                        | Some (_, literal_depth) ->
                            Sst.Reveal_with_fuel
                              {
                                function_id = target_function.function_id;
                                literal_depth;
                              });
                      typ = Sst.Unit;
                      span = span context application.exp_loc;
                    }
              | None -> malformed ())
          | _ -> malformed ())
  and lower_use_type_invariant_application context bindings application
      result_type callee arguments =
    let malformed () =
      unsupported context application.exp_loc Diagnostic.Malformed_ghost_call
    in
    match arguments with
    | [ (Nolabel, Arg (id_expression, _)); (Nolabel, Arg (value, _)) ] -> (
        match string_argument id_expression with
        | None -> malformed ()
        | Some use_id ->
            let start_, end_ = location_offsets application.exp_loc in
            let expected_id =
              Printf.sprintf "verocaml:use-type-invariant:1:%d:%d" start_ end_
            in
            if
              result_type <> Sst.Unit
              || (not application.exp_loc.loc_ghost)
              || (not callee.exp_loc.loc_ghost)
              || (not id_expression.exp_loc.loc_ghost)
              || not (String.equal use_id expected_id)
            then malformed ()
            else
              let* value =
                lower_authenticated_expression context bindings value
              in
              Ok
                {
                  Sst.expression_desc = Sst.Use_type_invariant { use_id; value };
                  typ = Sst.Unit;
                  span = span context application.exp_loc;
                })
    | _ -> malformed ()
  and exact_callback_callee context callee =
    Typedtree_spec_function_private.exact_callback_callee
      ~find:(Typedtree_callback_private.find_binding context.callbacks) callee
  and lower_application context bindings application result_type callee
      arguments =
    let unsupported_at construct =
      unsupported context application.exp_loc construct
    in
    let specification_callee, recursive_specification_call =
      Typedtree_spec_function_private.classify_callee
        ~normalize:normalize_value_path
        ~candidates:(fun path ->
          functions_by_path path context.functions
          |> List.filter (fun function_ ->
                 symbolic_source context function_ = None))
        ~kind:(fun candidate ->
          match candidate.function_kind with
          | Top_spec _ -> `Spec
          | Top_recursive_spec _ ->
              `Recursive (candidate.rec_flag = Asttypes.Recursive)
          | Top_exec | Top_type_invariant _ | Top_proof _
          | Top_external_specification _ | Top_external_body _ ->
              `Other)
        ~id:(fun candidate -> candidate.function_id)
        ~arity:(fun candidate ->
          match candidate.value_binding.vb_expr.exp_desc with
          | Texp_function { params; _ } -> List.length params
          | _ -> 0)
        ~first_class:(fun candidate ->
          Typedtree_spec_function_private.first_class_surface
            ~signature:
              (Callback_shape_private.source_signature_types
                 candidate.value_binding
                 ~definition_body:
                   (Sst_callback_private.source_definition_body candidate))
            candidate.value_binding.vb_expr)
        ~current:(Option.map (fun function_ -> function_.function_id)
                    context.current_function)
        ~local:(fun ident -> find_ident ident bindings <> None)
        ~argument_count:
          (List.fold_left
             (fun count (_, argument) ->
               match argument with
               | Typedtree.Arg _ -> count + 1
               | Typedtree.Omitted _ -> count)
             0 arguments)
        ~result_type callee
    in
    let specification_application =
      Typedtree_spec_function_private.lower_application
        {
          normalize = normalized_expression_type context bindings;
          lower = lower_authenticated_expression context bindings;
          parameter_label;
          span = span context;
          higher_order_error =
            (fun location ->
              Diagnostic.make
                (Diagnostic.Unsupported_construct
                   Diagnostic.Higher_order_call)
                (span context location));
        }
        ~enabled:
          (symbolic_context_is_logical context
          && specification_callee)
        ~application ~result_type ~callee ~arguments
    in
    match specification_application with
    | Some result -> result
    | None ->
        (match recursive_specification_call with
        | Some candidate ->
            let candidate_context =
              { context with current_function = Some candidate }
            in
            let* formal_types, formal_labels, formal_result =
              match
                Typedtree_spec_function_private.source_signature
                ~lower:(fun location typ ->
                  normalized_type_with_substitutions candidate_context
                    candidate.type_substitutions location typ)
                ~optional_carrier:(optional_carrier candidate_context)
                  candidate.value_binding.vb_expr
              with
              | Some result -> result
              | None -> unsupported_at Diagnostic.Higher_order_call
            in
            let* actuals = lower_arguments context bindings application arguments in
            Typedtree_spec_function_private.direct_call
              ~invalid:(fun _ ->
                Diagnostic.make
                  (Diagnostic.Unsupported_construct
                     Diagnostic.Unsupported_generic_use)
                  (span context application.exp_loc))
              ~span:(span context application.exp_loc) ~result_type ~actuals
              {
                function_id = candidate.function_id;
                type_binders =
                  List.map snd candidate.parametric_type_binders;
                formal_types;
                formal_labels;
                formal_result;
                recursive =
                  Option.fold ~none:false
                    ~some:(fun current ->
                      current.function_id = candidate.function_id)
                    context.current_function;
              }
        | None -> (
    match symbolic_application_head context callee arguments with
    | Some (path, _, description, arguments) ->
        lower_symbolic_application context bindings application result_type
          path description arguments
    | None ->
    if exact_callback_callee context callee then
      lower_callback_application context bindings `Call application None
    else
      match callee.exp_desc with
      | Texp_ident (path, source_name, description, _, _) -> (
          let path = normalize_value_path callee path in
          let uid = compiler_uid description.Types.val_uid in
          let is_stdlib name = path_resolves_to context path "Stdlib" name in
          let is_ghost name = path_resolves_to context path "Vero_ghost" name in
          if has_symbolic_candidate context path then
            lower_symbolic_application context bindings application result_type
              path description arguments
          else if is_ghost "reveal" then
            lower_reveal_application context application result_type callee
              arguments "reveal"
          else if is_ghost "reveal_with_fuel" then
            lower_reveal_application context application result_type callee
              arguments "reveal_with_fuel"
          else if is_ghost "use_type_invariant" then
            lower_use_type_invariant_application context bindings application
              result_type callee arguments
          else if is_ghost "old" then
            lower_ghost_application context bindings application result_type
              callee arguments path None
          else if
            is_ghost "requires" || is_ghost "ensures" || is_ghost "decreases"
            || is_ghost "structural_decreases"
            || is_ghost "assert_" || is_ghost "marker" || is_ghost "sidecar"
            || is_ghost "spec_definition"
            || is_ghost "recursive_spec_definition"
            || is_ghost "proof_definition"
            || is_ghost "proof_region"
            || is_ghost "type_invariant_definition"
            || is_ghost "use_type_invariant"
            || is_ghost "reveal"
            || is_ghost "reveal_with_fuel"
            || is_ghost "external_specification"
            || is_ghost "external_body"
          then unsupported_at Diagnostic.Malformed_ghost_call
          else if is_stdlib "+" then
            lower_fixed_arity context bindings application result_type arguments
              2 (fun operands -> Sst.Checked_arithmetic (Sst.Add, operands))
          else if is_stdlib "-" then
            lower_fixed_arity context bindings application result_type arguments
              2 (fun operands ->
                Sst.Checked_arithmetic (Sst.Subtract, operands))
          else if is_stdlib "~-" then
            lower_fixed_arity context bindings application result_type arguments
              1 (fun operands -> Sst.Checked_arithmetic (Sst.Negate, operands))
          else if is_stdlib "succ" then
            lower_fixed_arity context bindings application result_type arguments
              1 (fun operands ->
                Sst.Checked_arithmetic (Sst.Successor, operands))
          else if is_stdlib "pred" then
            lower_fixed_arity context bindings application result_type arguments
              1 (fun operands ->
                Sst.Checked_arithmetic (Sst.Predecessor, operands))
          else if is_stdlib "abs" then
            lower_fixed_arity context bindings application result_type arguments
              1 (fun operands ->
                Sst.Checked_arithmetic (Sst.Absolute_value, operands))
          else if is_stdlib "*" then
            lower_multiplication context bindings application result_type
              arguments
          else if is_stdlib "not" then
            lower_fixed_arity context bindings application result_type arguments
              1 (function
              | [ operand ] -> Sst.Boolean_not operand
              | _ -> assert false)
          else if is_stdlib "&&" then
            lower_fixed_arity context bindings application result_type arguments
              2 (function
              | [ left; right ] -> Sst.Boolean_binary (Sst.And, left, right)
              | _ -> assert false)
          else if is_stdlib "||" then
            lower_fixed_arity context bindings application result_type arguments
              2 (function
              | [ left; right ] -> Sst.Boolean_binary (Sst.Or, left, right)
              | _ -> assert false)
          else
            lower_named_application context bindings application result_type
              arguments path source_name uid is_stdlib unsupported_at)
      | _ -> unsupported_at Diagnostic.Higher_order_call))
  and lower_spec_function_identifier context expression result_type path =
    let path = normalize_value_path expression path in
    let candidates =
      functions_by_path path context.functions
      |> List.filter (fun candidate ->
             match candidate.function_kind with
             | Top_spec _ | Top_recursive_spec _ -> true
             | Top_exec | Top_type_invariant _ | Top_proof _
             | Top_external_specification _ | Top_external_body _ ->
                 false)
    in
    match Callback_call_private.select_candidate candidates with
    | None ->
        unsupported context expression.exp_loc Diagnostic.Higher_order_function
    | Some candidate ->
        let candidate_context =
          { context with current_function = Some candidate }
        in
        let* formal_types, formal_labels, formal_result =
          match
            Typedtree_spec_function_private.source_signature
              ~lower:(fun location typ ->
                normalized_type_with_substitutions candidate_context
                  candidate.type_substitutions location typ)
              ~optional_carrier:(optional_carrier candidate_context)
              candidate.value_binding.vb_expr
          with
          | Some result -> result
          | None ->
              unsupported context expression.exp_loc
                Diagnostic.Higher_order_function
        in
        Typedtree_spec_function_private.reference
          ~invalid:(fun _ ->
            Diagnostic.make
              (Diagnostic.Unsupported_construct
                 Diagnostic.Unsupported_generic_use)
              (span context expression.exp_loc))
          ~span:(span context expression.exp_loc) ~result_type
          ~function_id:candidate.function_id
          ~type_binders:(List.map snd candidate.parametric_type_binders)
          ~formal_types:(formal_types @ [ formal_result ]) ~formal_labels
          ~recursive:
            (Option.fold ~none:false
               ~some:(fun current ->
                 current.function_id = candidate.function_id)
               context.current_function)
  and authenticate_symbolic_use context location path description =
    authenticate_symbolic_candidate context location path description
  and lower_symbolic_identifier context expression result_type path description =
    let path = normalize_value_path expression path in
    let* function_id, declaration =
      authenticate_symbolic_use context expression.exp_loc path description
    in
    if
      Parametric_type.is_spec_function result_type
      && Symbolic_application_private.parameter_types declaration <> []
    then
      Typedtree_spec_function_private.reference
        ~invalid:(fun message ->
          Diagnostic.make (Diagnostic.Invalid_symbolic_application message)
            (span context expression.exp_loc))
        ~span:(span context expression.exp_loc) ~result_type
        ~function_id
        ~type_binders:(Symbolic_application_private.type_binders declaration)
        ~formal_types:
          (Symbolic_application_private.parameter_types declaration
          @ [
              Symbolic_application_private.declaration_result_type declaration;
            ])
        ~formal_labels:
          (List.map
             (fun _ -> None)
             (Symbolic_application_private.parameter_types declaration))
        ~recursive:false
    else
      Typedtree_symbolic_private.identifier_expression declaration ~result_type
        ~span:(span context expression.exp_loc)
      |> Result.map_error (fun message ->
             Diagnostic.make (Diagnostic.Invalid_symbolic_application message)
               (span context expression.exp_loc))
  and lower_symbolic_application context bindings application result_type path
      description arguments =
    let path = normalize_value_path application path in
    let* _function, declaration =
      authenticate_symbolic_use context application.exp_loc path description
    in
    let* lowered_arguments =
      lower_arguments context bindings application arguments
    in
    let application_span = span context application.exp_loc in
    Typedtree_spec_function_private.lower_symbolic_application
      ~invalid:(fun message ->
        Diagnostic.make (Diagnostic.Invalid_symbolic_application message)
          application_span)
      ~span:application_span ~declaration ~result_type
      ~arguments:lowered_arguments
  and lower_named_application context bindings application result_type arguments
      path source_name uid is_stdlib unsupported_at =
    let comparison =
      if is_stdlib "=" then Some Sst.Equal
      else if is_stdlib "<>" then Some Sst.Not_equal
      else if is_stdlib "<" then Some Sst.Less_than
      else if is_stdlib "<=" then Some Sst.Less_or_equal
      else if is_stdlib ">" then Some Sst.Greater_than
      else if is_stdlib ">=" then Some Sst.Greater_or_equal
      else None
    in
    match comparison with
    | Some comparison ->
        lower_comparison context bindings application result_type arguments
          comparison
    | None ->
        let wrapping =
          List.exists is_stdlib
            [ "/"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" ]
        in
        if wrapping then unsupported_at Diagnostic.Wrapping_arithmetic
        else if path_resolves_to context path "Effect" "perform" then
          unsupported_at Diagnostic.Effect
        else if
          List.exists
            (fun name -> path_resolves_to context path "Domain" name)
            [ "spawn"; "join" ]
        then unsupported_at Diagnostic.Concurrency
        else if
          match path with
          | Path.Pident ident -> find_ident ident bindings <> None
          | _ -> false
        then unsupported_at Diagnostic.Higher_order_call
        else
          match functions_by_path path context.functions with
          | [] ->
              let lower_direct definition signature =
                Parametric_interface_lowering_private.lower_direct_call
                  ~lower_expression:
                    (lower_authenticated_expression context bindings)
                  ~span:(span context)
                  ~reject:(fun construct ->
                    Diagnostic.make
                      (Diagnostic.Unsupported_construct construct)
                      (span context application.exp_loc))
                  ~application ~signature ~definition ~result_type arguments
              in
              let rec application_bearing = function
                | Sst.Application _ -> true
                | Sst.Tuple components ->
                    List.exists
                      (fun (_, typ) -> application_bearing typ)
                      components
                | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _
                | Sst.Parameter _ ->
                    false
              in
              let exact_application_model definition =
                match
                  ( definition.Sst.mode,
                    definition.recursive,
                    definition.body,
                    definition.parameters,
                    definition.result_type )
                with
                | ( Sst.Spec,
                    false,
                    Sst.Spec_definition _,
                    [
                      Sst.Value_parameter
                        {
                          pattern = { typ = Sst.Aggregate _; _ };
                          label = None;
                          _;
                        };
                    ],
                    (Sst.Application _ as result) ) ->
                    context.imported.imported_types
                    |> List.exists (fun imported ->
                        Option.fold ~none:false
                          ~some:(fun descriptor ->
                            Parametric_adt.same_application descriptor result)
                          imported.imported_parametric_descriptor)
                | _ -> false
              in
              let application_signature definition =
                application_bearing definition.Sst.result_type
                || List.exists
                     (function
                       | Sst.Callback_parameter _ -> false
                       | Sst.Value_parameter parameter ->
                           application_bearing parameter.pattern.typ)
                     definition.parameters
              in
              let has_application_model =
                List.exists
                  (fun imported ->
                    exact_application_model imported.imported_definition)
                  context.imported.imported_callables
              in
              let external_target_specification =
                match
                  ( context.imported.external_target_specifications,
                    source_name.txt )
                with
                | ( Some environment,
                    Longident.Ldot (Longident.Lident unit_name, value_name) )
                  when
                    String.equal (unit_name ^ "." ^ value_name) (Path.name path)
                    ->
                    External_target_specification_private.find_summary
                      environment ~canonical_path:(Path.name path) ~value_uid:uid
                | Some _, _ | None, _ -> None
              in
              (match external_target_specification with
              | Some summary
                when External_target_specification_private.call_is_after_summary
                       summary (span context application.exp_loc) ->
                  lower_direct
                    (External_target_specification_private.definition summary)
                    (External_target_specification_private.signature summary)
              | Some _ -> unsupported_at Diagnostic.Unknown_or_external_call
              | None -> (
                  match
                    List.find_opt
                      (fun imported ->
                        String.equal imported.imported_path (Path.name path)
                        && String.equal imported.imported_uid uid)
                      context.imported.imported_callables
                  with
                  | None ->
                      unsupported_at Diagnostic.Unknown_or_external_call
                  | Some imported
                    when has_application_model
                         && application_signature imported.imported_definition
                         && not
                              (exact_application_model
                                 imported.imported_definition) ->
                      unsupported_at Diagnostic.Unknown_or_external_call
                  | Some imported
                    when exact_application_model imported.imported_definition
                         && Option.fold ~none:false
                              ~some:(fun function_ ->
                                function_.rec_flag = Asttypes.Recursive)
                              context.current_function ->
                      unsupported_at Diagnostic.Unsupported_generic_use
                  | Some imported ->
                      lower_direct imported.imported_definition
                        imported.imported_signature))
          | candidates ->
              lower_direct_candidate_call context bindings application result_type
                arguments candidates
  and lower_direct_candidate_call context bindings application result_type
      source_arguments candidates =
    let selected = Callback_call_private.select_candidate candidates in
    match selected with
    | None ->
        unsupported context application.exp_loc Diagnostic.Unsupported_generic_use
    | Some callee_function ->
        let direct_candidate =
          {
            Callback_call_private.ident = callee_function.ident;
            function_id = callee_function.function_id;
            value_binding = callee_function.value_binding;
            type_substitutions = callee_function.type_substitutions;
            type_binders = List.map snd callee_function.parametric_type_binders;
          }
        in
        let callee_context =
          { context with current_function = Some callee_function }
        in
        let caller_identity =
          Option.value ~default:"<none>" (callback_caller_identity context)
        in
        let call_edge_identity =
          Typedtree_callback_private.call_edge_identity caller_identity
            application.exp_loc
        in
        let top_level_services =
          {
            Callback_contract_private.candidates =
              (fun path -> functions_by_path path context.functions);
            eligible =
              (fun actual ->
                actual.function_kind = Top_exec
                && actual.rec_flag = Asttypes.Nonrecursive);
            candidate_identity =
              (fun actual ->
                Typedtree_callback_private.compiler_identity actual.ident);
            candidate_id = (fun actual -> actual.function_id);
            shape = callback_shape context;
            validate_contract = explicit_top_level_callback_contract context;
            compilation_identity = callback_compilation_identity context;
            caller_identity;
            call_edge_identity;
            compiler_mode = Typedtree_callback_private.type_evidence;
            span = span context;
            policy_error =
              (fun location message ->
                callback_diagnostic_with_message context
                  Diagnostic.Callback_policy location message);
            authentication_error =
              (fun location message ->
                callback_diagnostic_with_message context
                  Diagnostic.Callback_authentication location message);
          }
        in
        let callback_actual expected_shape label source =
          Callback_contract_private.resolve_actual
            {
              find_binding =
                Typedtree_callback_private.find_binding context.callbacks;
              seal = seal_callback_binding context;
              lower_local =
                (fun ~name source ->
                  lower_local_callback context bindings ~name source);
              top_level =
                Callback_contract_private.top_level_argument top_level_services;
              call_location = application.exp_loc;
              next_local_name =
                (fun () ->
                  Printf.sprintf "$callback%d"
                    context.callbacks.next_callback_id);
              policy_error =
                (fun location message ->
                  callback_diagnostic_with_message context
                    Diagnostic.Callback_policy location message);
              authentication_error =
                (fun location message ->
                  callback_diagnostic_with_message context
                    Diagnostic.Callback_authentication location message);
            }
            expected_shape label source
        in
        Callback_call_private.lower_direct_candidate
          {
            normalized_type =
              (fun _ location typ ->
                normalized_type_with_substitutions callee_context
                  callee_function.type_substitutions location typ);
            optional_carrier = optional_carrier callee_context;
            lower_expression = lower_authenticated_expression context bindings;
            callback_actual;
            callback_candidate_contract =
              explicit_top_level_callback_contract context callee_function;
            parameter_label;
            policy_error =
              (fun location message ->
                callback_diagnostic_with_message context
                  Diagnostic.Callback_policy location message);
            polymorphic_error =
              (fun location ->
                callback_diagnostic context Diagnostic.Unsupported_generic_use
                  location);
            higher_order_error =
              (fun location ->
                callback_diagnostic context Diagnostic.Higher_order_call
                  location);
            span = span context;
            current =
              Option.map (fun current -> current.ident) context.current_function;
          }
          ~application ~result_type ~source_arguments direct_candidate
  and lower_arguments context bindings application arguments =
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | (label, Arg (argument, _)) :: rest -> (
          match lower_authenticated_expression context bindings argument with
          | Error _ as error -> error
          | Ok argument ->
              let argument =
                match (label, argument.Sst.expression_desc) with
                | ( Optional _,
                    ( Sst.Optional_absent | Sst.Optional_present _
                    | Sst.Optional_forward _ ) ) ->
                    argument
                | Optional _, _ ->
                    {
                      argument with
                      expression_desc = Sst.Optional_forward argument;
                    }
                | (Nolabel | Labelled _ | Position _), _ -> argument
              in
              loop ((parameter_label label, argument) :: lowered) rest)
      | (_, Omitted _) :: _ ->
          unsupported context application.exp_loc Diagnostic.Higher_order_call
    in
    loop [] arguments
  and lower_fixed_arity context bindings application result_type arguments arity
      make_desc =
    match lower_arguments context bindings application arguments with
    | Error _ as error -> error
    | Ok arguments ->
        let operands = List.map snd arguments in
        if List.length operands <> arity then
          unsupported context application.exp_loc Diagnostic.Higher_order_call
        else
          Ok
            {
              Sst.expression_desc = make_desc operands;
              typ = result_type;
              span = span context application.exp_loc;
            }
  and lower_multiplication context bindings application result_type arguments =
    let literal = function
      | { exp_desc = Texp_constant (Const_int value); _ } ->
          Some (Z.of_int value)
      | _ -> None
    in
    match arguments with
    | [ (Nolabel, Arg (left, _)); (Nolabel, Arg (right, _)) ] -> (
        match (literal left, literal right) with
        | Some constant, _ -> (
            match lower_authenticated_expression context bindings right with
            | Error _ as error -> error
            | Ok operand ->
                Ok
                  {
                    Sst.expression_desc =
                      Sst.Checked_arithmetic
                        (Sst.Multiply_constant constant, [ operand ]);
                    typ = result_type;
                    span = span context application.exp_loc;
                  })
        | None, Some constant -> (
            match lower_authenticated_expression context bindings left with
            | Error _ as error -> error
            | Ok operand ->
                Ok
                  {
                    Sst.expression_desc =
                      Sst.Checked_arithmetic
                        (Sst.Multiply_constant constant, [ operand ]);
                    typ = result_type;
                    span = span context application.exp_loc;
                  })
        | None, None ->
            unsupported context application.exp_loc
              Diagnostic.Nonlinear_multiplication)
    | _ -> unsupported context application.exp_loc Diagnostic.Higher_order_call
  and lower_comparison context bindings application result_type arguments
      comparison =
    let logical_parametric_equality =
      context.logical_ghost_depth > 0
      ||
      match context.current_function with
      | Some
          {
            function_kind =
              ( Top_spec _ | Top_recursive_spec _ | Top_proof _
              | Top_type_invariant _ | Top_external_specification _ );
            _;
          } ->
          true
      | Some { function_kind = Top_exec | Top_external_body _; _ } | None ->
          false
    in
    let descriptors =
      List.map
        (fun item -> item.Parametric_adt_lowering_private.descriptor)
        context.aggregates.parametric_adts
    in
    let imported_descriptors =
      context.imported.imported_types
      |> List.filter_map (fun imported ->
          imported.imported_parametric_descriptor)
    in
    let authenticated_model_result expected_type =
      match expected_type with
      | Sst.Aggregate type_id ->
          List.exists
            (fun imported_type ->
              imported_type.imported_type_definition.Sst.type_id = type_id)
            context.imported.imported_types
      | Sst.Application (constructor, arguments) as application ->
          List.exists
            (fun imported_type ->
              Option.fold ~none:false
                ~some:(fun descriptor ->
                  Parametric_type.compare_constructor
                    (Parametric_adt.type_constructor descriptor)
                    constructor
                  = 0
                  && Parametric_adt.same_application descriptor application
                  && Result.is_ok
                       (Logical_adt_schema_private.instantiate
                          ~descriptors:(descriptors @ imported_descriptors)
                          ~applications:
                            [
                              ( (Parametric_adt.type_id descriptor).type_index,
                                arguments );
                            ]))
                imported_type.imported_parametric_descriptor)
            context.imported.imported_types
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
          false
    in
    let exact_model_formal expected_type definition =
      match
        ( definition.Sst.mode,
          definition.recursive,
          definition.body,
          definition.parameters )
      with
      | ( Sst.Spec,
          false,
          Sst.Spec_definition _,
          [
            Sst.Value_parameter
              {
                Sst.pattern = ({ typ = Sst.Aggregate _; _ } as formal_pattern);
                label = None;
                _;
              };
          ] )
        when Parametric_type.equal definition.result_type expected_type
             && authenticated_model_result expected_type ->
          Some formal_pattern
      | _ -> None
    in
    let opaque_imported_model_result expected_type =
      List.exists
        (fun imported ->
          Option.is_some
            (exact_model_formal expected_type imported.imported_definition))
        context.imported.imported_callables
    in
    let exact_opaque_imported_model_application expected_type expression =
      match expression.exp_desc with
      | Texp_apply
          ( {
              exp_desc = Texp_ident (path, _, description, _, _);
              exp_env;
              exp_loc;
              _;
            },
            [ (Nolabel, Arg (actual, _)) ],
            _,
            _,
            _ ) ->
          let path =
            try Env.normalize_value_path (Some exp_loc) exp_env path
            with Env.Error _ -> path
          in
          let uid = compiler_uid description.Types.val_uid in
          let imported =
            List.find_opt
              (fun imported ->
                String.equal imported.imported_path (Path.name path)
                && String.equal imported.imported_uid uid)
              context.imported.imported_callables
          in
          (* This private environment is populated from an authenticated
           [Imported_callable.environment]. Provider sealing excludes every
           aggregate-returning non-model callable, and imports only the exact
           selected model-result closure. Recheck that narrowed descriptor
           shape here rather than treating the aggregate type or source path
           alone as equality authority. *)
          Option.bind imported (fun imported ->
              let definition = imported.imported_definition in
              match exact_model_formal expected_type definition with
              | Some formal_pattern -> (
                  match
                    ( normalized_expression_type context bindings expression,
                      normalized_expression_type context bindings actual )
                  with
                  | Ok expression_type, Ok actual_type
                    when Parametric_type.equal expression_type expected_type
                         && actual_type = formal_pattern.Sst.typ ->
                      Some imported
                  | Ok _, Ok _ | Error _, _ | _, Error _ -> None)
              | None -> None)
      | _ -> None
    in
    let lower_opaque_imported_model_equality type_id left right =
      match
        ( exact_opaque_imported_model_application type_id left,
          exact_opaque_imported_model_application type_id right )
      with
      | Some left_model, Some right_model
        when String.equal left_model.imported_path right_model.imported_path
             && String.equal left_model.imported_uid right_model.imported_uid
             && left_model.imported_definition.Sst.function_id
                = right_model.imported_definition.Sst.function_id -> (
          match lower_authenticated_expression context bindings left with
          | Error _ as error -> error
          | Ok left -> (
              match lower_authenticated_expression context bindings right with
              | Error _ as error -> error
              | Ok right ->
                  Ok
                    {
                      Sst.expression_desc = Sst.Compare (Sst.Equal, left, right);
                      typ = result_type;
                      span = span context application.exp_loc;
                    }))
      | Some _, Some _ | None, _ | _, None ->
          unsupported context application.exp_loc
            Diagnostic.Structural_aggregate_equality
    in
    match arguments with
    | [ (Nolabel, Arg (left, _)); (Nolabel, Arg (right, _)) ] -> (
        match normalized_expression_type context bindings left with
        | Error _ as error -> error
        | Ok (Sst.Aggregate type_id)
          when (comparison = Sst.Equal || comparison = Sst.Not_equal)
               && ranked_deeply_immutable_type context.aggregates type_id -> (
            match normalized_expression_type context bindings right with
            | Ok (Sst.Aggregate right_type) when right_type = type_id -> (
                match lower_authenticated_expression context bindings left with
                | Error _ as error -> error
                | Ok left -> (
                    match
                      lower_authenticated_expression context bindings right
                    with
                    | Error _ as error -> error
                    | Ok right ->
                        Ok
                          {
                            Sst.expression_desc =
                              Sst.Compare (comparison, left, right);
                            typ = result_type;
                            span = span context application.exp_loc;
                          }))
            | Ok _ ->
                unsupported context application.exp_loc
                  Diagnostic.Structural_aggregate_equality
            | Error _ as error -> error)
        | Ok (Sst.Aggregate type_id) when comparison = Sst.Equal ->
            lower_opaque_imported_model_equality (Sst.Aggregate type_id) left
              right
        | Ok (Sst.Application _ as application)
          when comparison = Sst.Equal
               && opaque_imported_model_result application ->
            lower_opaque_imported_model_equality application left right
        | Ok ((Sst.Parameter _ | Sst.Application _ | Sst.Tuple _) as left_type)
          when (comparison = Sst.Equal || comparison = Sst.Not_equal)
               && (logical_parametric_equality
                  ||
                  match left_type with
                  | Sst.Application _ ->
                      Parametric_adt.exec_scalar_equality descriptors left_type
                  | Sst.Parameter _ | Sst.Tuple _ -> false
                  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _ -> false)
          -> (
            match normalized_expression_type context bindings right with
            | Ok right_type when Parametric_type.equal left_type right_type ->
                let* left =
                  lower_authenticated_expression context bindings left
                in
                let* right =
                  lower_authenticated_expression context bindings right
                in
                Ok
                  {
                    Sst.expression_desc = Sst.Compare (comparison, left, right);
                    typ = result_type;
                    span = span context application.exp_loc;
                  }
            | Ok _ ->
                unsupported context application.exp_loc
                  Diagnostic.Structural_aggregate_equality
            | Error _ as error -> error)
        | Ok
            (Sst.Parameter _ | Sst.Application _ | Sst.Tuple _ | Sst.Aggregate _)
          ->
            unsupported context application.exp_loc
              Diagnostic.Unsupported_generic_use
        | Ok (Sst.Unit | Sst.Bool)
          when comparison <> Sst.Equal && comparison <> Sst.Not_equal ->
            unsupported context application.exp_loc
              Diagnostic.Unsupported_expression
        | Ok (Sst.Unit | Sst.Bool | Sst.Int) -> (
            match lower_authenticated_expression context bindings left with
            | Error _ as error -> error
            | Ok left -> (
                match lower_authenticated_expression context bindings right with
                | Error _ as error -> error
                | Ok right ->
                    Ok
                      {
                        Sst.expression_desc =
                          Sst.Compare (comparison, left, right);
                        typ = result_type;
                        span = span context application.exp_loc;
                      })))
    | _ -> unsupported context application.exp_loc Diagnostic.Higher_order_call
  and lower_ghost_application context bindings application result_type callee
      arguments path expected_binder_type =
    let is_ghost name = path_resolves_to context path "Vero_ghost" name in
    if not (same_location callee.exp_loc application.exp_loc) then
      unsupported context application.exp_loc Diagnostic.Malformed_ghost_call
    else
      match arguments with
      | [ (Nolabel, Arg (argument, _)) ] -> (
          if is_ghost "old" then
            match lower_authenticated_expression context bindings argument with
            | Error _ as error -> error
            | Ok payload ->
                Ok
                  {
                    Sst.expression_desc = Sst.Old payload;
                    typ = result_type;
                    span = span context application.exp_loc;
                  }
          else
            match argument.exp_desc with
            | Texp_function
                { params = [ parameter ]; body = Tfunction_body body; _ }
              when parameter.fp_partial = Total -> (
                match parameter.fp_kind with
                | Tparam_pat pattern -> (
                    let lowered_pattern =
                      match (expected_binder_type, pattern.pat_desc) with
                      | Some expected_type, Tpat_var (ident, name, _, _, mode)
                        ->
                          let binding =
                            fresh_binding context name.txt expected_type
                              pattern.pat_type
                              (uniqueness_of_pattern_mode mode)
                              pattern.pat_loc
                          in
                          Ok
                            ( {
                                Sst.pattern_desc = Sst.Bind binding;
                                typ = expected_type;
                                span = span context pattern.pat_loc;
                              },
                              (ident, binding) :: bindings )
                      | Some expected_type, Tpat_any ->
                          let binding =
                            fresh_binding context "$result" expected_type
                              pattern.pat_type Sst.Definitely_aliased
                              pattern.pat_loc
                          in
                          Ok
                            ( {
                                Sst.pattern_desc = Sst.Bind binding;
                                typ = expected_type;
                                span = span context pattern.pat_loc;
                              },
                              bindings )
                      | Some _, _ ->
                          unsupported context pattern.pat_loc
                            Diagnostic.Malformed_ghost_call
                      | None, _ -> lower_pattern context bindings pattern
                    in
                    match lowered_pattern with
                    | Error _ as error -> error
                    | Ok (binder, body_bindings) -> (
                        context.logical_ghost_depth <-
                          context.logical_ghost_depth + 1;
                        let lowered =
                          lower_authenticated_expression context body_bindings
                            body
                        in
                        context.logical_ghost_depth <-
                          context.logical_ghost_depth - 1;
                        match lowered with
                        | Error _ as error -> error
                        | Ok payload -> (
                            let contract =
                              if is_ghost "requires" then
                                Some (Callback_contract_private.Requires, None)
                              else if is_ghost "ensures" then
                                Some
                                  ( Callback_contract_private.Ensures,
                                    Some binder )
                              else if
                                is_ghost "decreases"
                                || is_ghost "structural_decreases"
                              then
                                Some (Callback_contract_private.Decreases, None)
                              else if is_ghost "assert_" then
                                Some (Callback_contract_private.Assert, None)
                              else None
                            in
                            match contract with
                            | Some (kind, binder) ->
                                let carrier_span =
                                  span context application.exp_loc
                                in
                                context.contract_carriers <-
                                  { kind; binder; payload; span = carrier_span }
                                  :: context.contract_carriers;
                                Ok
                                  {
                                    Sst.expression_desc = Sst.Unit_constant;
                                    typ = result_type;
                                    span = carrier_span;
                                  }
                            | None ->
                                unsupported context application.exp_loc
                                  Diagnostic.Malformed_ghost_call)))
                | Tparam_optional_default _ ->
                    unsupported context application.exp_loc
                      Diagnostic.Malformed_ghost_call)
            | _ ->
                unsupported context application.exp_loc
                  Diagnostic.Malformed_ghost_call)
      | _ ->
          unsupported context application.exp_loc
            Diagnostic.Malformed_ghost_call
  let top_level_binding ?module_identity context function_index rec_flag
      value_binding =
    let verifier_attributes =
      List.filter
        (fun attribute ->
          String.starts_with ~prefix:"verocaml."
            attribute.Parsetree.attr_name.txt)
        value_binding.vb_attributes
    in
    let* () =
      match
        List.filter
          (fun attribute ->
            not
              (attribute.Parsetree.attr_loc.Location.loc_ghost
             && attribute.attr_name.loc.loc_ghost
              && String.starts_with ~prefix:"verocaml.internal."
                   attribute.attr_name.txt))
          verifier_attributes
      with
      | [] -> Ok ()
      | attribute :: _ ->
          unsupported context attribute.attr_loc Diagnostic.Malformed_ghost_call
    in
    match value_binding.vb_pat.pat_desc with
    | Tpat_var (ident, name, _, _, _) ->
        let* function_kind =
          match
            Option.bind context.symbolic_scan (fun scan ->
                Typedtree_symbolic_private.find_binding scan value_binding)
          with
          | Some declaration ->
              Ok
                (Top_spec
                   {
                     definition_body =
                       Typedtree_symbolic_private.definition_body declaration;
                     witness_location =
                       Typedtree_symbolic_private.declaration_location
                         declaration;
                     recursive_visibility = None;
                   })
          | None -> (
          match terminal_function_body value_binding.vb_expr with
          | None -> Ok Top_exec
          | Some terminal -> (
              let* recursive_spec =
                authenticated_recursive_spec_carrier context name.txt
                  value_binding.vb_loc terminal
              in
              match recursive_spec with
              | Some carrier -> Ok (Top_recursive_spec carrier)
              | None -> (
                  let* spec =
                    authenticated_declaration_carrier context ~role:"spec"
                      ~ghost_name:"spec_definition" name.txt
                      value_binding.vb_loc terminal
                  in
                  match spec with
                  | Some carrier -> Ok (Top_spec carrier)
                  | None -> (
                      let* proof =
                        authenticated_declaration_carrier context ~role:"proof"
                          ~ghost_name:"proof_definition" name.txt
                          value_binding.vb_loc terminal
                      in
                      match proof with
                      | Some carrier -> Ok (Top_proof carrier)
                      | None -> (
                          let* type_invariant =
                            authenticated_declaration_carrier context
                              ~role:"type-invariant"
                              ~ghost_name:"type_invariant_definition" name.txt
                              value_binding.vb_loc terminal
                          in
                          match type_invariant with
                          | Some carrier -> Ok (Top_type_invariant carrier)
                          | None -> (
                              let* external_specification =
                                authenticated_declaration_carrier context
                                  ~role:"external-specification"
                                  ~ghost_name:"external_specification" name.txt
                                  value_binding.vb_loc terminal
                              in
                              match external_specification with
                              | Some carrier ->
                                  Ok (Top_external_specification carrier)
                              | None ->
                                  let external_role, external_mode =
                                    external_body_carrier_role context terminal
                                  in
                                  let* external_body =
                                    authenticated_declaration_carrier context
                                      ~role:external_role
                                      ~ghost_name:"external_body" name.txt
                                      value_binding.vb_loc terminal
                                  in
                                  Ok
                                    (match external_body with
                                    | Some carrier ->
                                        Top_external_body
                                          (external_mode, carrier)
                                    | None -> Top_exec)))))))
        in
        let* () =
          let role =
            match function_kind with
            | Top_proof _ -> Broadcast.Proved_body
            | Top_external_body (Sst.Proof, _) ->
                Broadcast.Trusted_proof_body
            | Top_exec | Top_spec _ | Top_type_invariant _
            | Top_recursive_spec _ | Top_external_specification _
            | Top_external_body _ ->
                Broadcast.Other_body
          in
          Option.fold ~none:(Ok ())
            ~some:(fun scan ->
              Broadcast.validate_source_role ~scan
                ~binding:value_binding
                ~recursive:(rec_flag <> Asttypes.Nonrecursive) ~role
                ~span:(span context value_binding.vb_loc))
            context.broadcast_scan
        in
        Ok
          {
            ident;
            resolved_paths =
              Path.Pident ident
              ::
              (match module_identity with
              | None -> []
              | Some (module_id, _) ->
                  [ Path.Pdot (Path.Pident module_id, name.txt) ]);
            function_id =
              {
                Sst.function_index;
                function_name =
                  (match module_identity with
                  | None -> name.txt
                  | Some (_, module_name) -> module_name ^ "." ^ name.txt);
              };
            value_binding;
            rec_flag;
            function_kind;
            type_substitutions = [];
            parametric_type_binders = [];
            proof_capture_hints = [];
            builtin_assertions = [];
          }
    | _ ->
        unsupported context value_binding.vb_loc
          Diagnostic.Unsupported_top_level_binding
  let collect_structure source_file imports allow_imported_opens broadcast_scan
      symbolic_scan structure =
    let context =
      {
        source_file;
        typing_environment = structure.str_final_env;
        proof_capture_artifact = None;
        broadcast_scan = Some broadcast_scan;
        symbolic_scan = Some symbolic_scan;
        symbolic_definitions = [];
        imports;
        functions = [];
        aggregates = empty_aggregates;
        current_function = None;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported = empty_imported_environment;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let module_type_declaration = ref None in
    let constrained_module = ref None in
    let unsupported_item item =
      unsupported context item.str_loc Diagnostic.Unsupported_structure_item
    in
    let authenticated_imported_open declaration =
      allow_imported_opens
      &&
      match declaration.open_expr.mod_desc with
      | Tmod_ident (Path.Pident ident, _) when Ident.is_global_or_predef ident
        ->
          Array.exists
            (fun (import : Cmt_input.import) ->
              String.equal import.unit_name (Ident.name ident)
              && Option.is_some import.crc)
            context.imports
      | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_apply _
      | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ ->
          false
    in
    let signature_items signature =
      let rec loop types values = function
        | [] -> Ok (List.rev types, List.rev values)
        | item :: rest -> (
            match item.sig_desc with
            | Tsig_type (_, declarations) ->
                loop (List.rev_append declarations types) values rest
            | Tsig_value value -> loop types (value :: values) rest
            | Tsig_attribute _ -> loop types values rest
            | Tsig_typesubst _ | Tsig_typext _ | Tsig_exception _
            | Tsig_module _ | Tsig_modsubst _ | Tsig_recmodule _
            | Tsig_modtype _ | Tsig_modtypesubst _ | Tsig_open _
            | Tsig_include _ | Tsig_class _ | Tsig_class_type _ ->
                unsupported context item.sig_loc
                  Diagnostic.Unsupported_structure_item)
      in
      loop [] [] signature.sig_items
    in
    let collect_constrained_module type_index function_index types functions
        binding signature_declaration =
      let* module_id, module_name =
        match (binding.mb_id, binding.mb_name.txt) with
        | Some id, Some name -> Ok (id, name)
        | None, _ | _, None ->
            unsupported context binding.mb_loc
              Diagnostic.Unsupported_structure_item
      in
      let* implementation, signature, constraint_location, hidden_coercion =
        match binding.mb_expr.mod_desc with
        | Tmod_constraint
            ( ({ mod_desc = Tmod_structure implementation; _ } as _inner),
              _,
              Tmodtype_explicit
                ({ mty_desc = Tmty_ident (Path.Pident signature_id, _); _ } as
                 explicit_signature),
              ((Tcoerce_none | Tcoerce_structure _) as coercion) )
          when Ident.same signature_id signature_declaration.mtd_id -> (
            match signature_declaration.mtd_type with
            | Some { mty_desc = Tmty_signature signature; _ } ->
                Ok
                  ( implementation,
                    signature,
                    explicit_signature.mty_loc,
                    match coercion with
                    | Tcoerce_structure _ -> true
                    | Tcoerce_none -> false
                    | Tcoerce_functor _ | Tcoerce_primitive _ | Tcoerce_alias _
                      ->
                        false )
            | Some _ | None ->
                unsupported context signature_declaration.mtd_loc
                  Diagnostic.Unsupported_structure_item)
        | Tmod_constraint (_, _, Tmodtype_explicit explicit_signature, _) ->
            unsupported context explicit_signature.mty_loc
              Diagnostic.Unsupported_structure_item
        | Tmod_constraint (_, _, Tmodtype_implicit, _)
        | Tmod_structure _ | Tmod_ident _ | Tmod_functor _ | Tmod_apply _
        | Tmod_apply_unit _ | Tmod_unpack _ ->
            unsupported context binding.mb_loc
              Diagnostic.Unsupported_structure_item
      in
      let* public_types, public_values = signature_items signature in
      let public_type name = List.find_opt (fun declaration -> String.equal declaration.typ_name.txt name) public_types in
      let public_value name = List.find_opt (fun value -> String.equal value.val_name.txt name) public_values in
      let symbolic_binding =
        Typedtree_symbolic_private.find_binding symbolic_scan
      in
      let* () =
        if not hidden_coercion then Ok ()
        else
          let hidden_bindings =
            implementation.str_items
            |> List.concat_map (fun item ->
                match item.str_desc with
                | Tstr_value (rec_flag, bindings) ->
                    Broadcast.source_bindings broadcast_scan bindings
                    |> List.filter_map (fun binding ->
                        match binding.vb_pat.pat_desc with
                        | Tpat_var (_, name, _, _, _)
                          when Option.is_none (public_value name.txt) ->
                            if Option.is_some (symbolic_binding binding) then None
                            else Some (rec_flag, binding)
                        | _ -> None)
                | _ -> [])
          in
          match hidden_bindings with
          | [ (Asttypes.Recursive, _) ] -> Ok ()
          | [] | (Asttypes.Nonrecursive, _) :: _ | _ :: _ :: _ ->
              unsupported context constraint_location
                Diagnostic.Unsupported_structure_item
      in
      let rec implementation_items type_index function_index types functions
          public_type_links public_functions = function
        | [] ->
            let linked_type_names =
              List.map
                (fun (declaration, _) -> declaration.typ_name.txt)
                public_type_links
            in
            let linked_value_names =
              List.map (fun (value, _) -> value.val_name.txt) public_functions
            in
            if
              List.length linked_type_names <> List.length public_types
              || List.length linked_value_names <> List.length public_values
            then
              unsupported context binding.mb_loc
                Diagnostic.Unsupported_structure_item
            else
              Ok
                ( type_index,
                  function_index,
                  types,
                  functions,
                  List.rev public_type_links,
                  List.rev public_functions )
        | item :: rest -> (
            match item.str_desc with
            | Tstr_attribute _ ->
                implementation_items type_index function_index types functions
                  public_type_links public_functions rest
            | Tstr_type (_, declarations) ->
                let rec declarations_loop type_index types public_type_links =
                  function
                  | [] ->
                      implementation_items type_index function_index types
                        functions public_type_links public_functions rest
                  | declaration :: remaining -> (
                      let public_declaration =
                        public_type declaration.typ_name.txt
                      in
                      let public_alias_target =
                        match
                          (public_declaration, declaration.typ_manifest)
                        with
                        | ( Some _,
                            Some { ctyp_desc = Ttyp_constr (path, _, []); _ } )
                          ->
                            List.find_opt
                              (fun local ->
                                List.exists
                                  (fun resolved -> Path.same resolved path)
                                  local.resolved_paths)
                              types
                        | Some _, Some _ | Some _, None | None, _ -> None
                      in
                      match public_alias_target with
                      | Some target ->
                          let resolved_paths =
                            Path.Pident declaration.typ_id
                            :: Path.Pdot
                                 ( Path.Pident module_id,
                                   declaration.typ_name.txt )
                            :: target.resolved_paths
                          in
                          let target = { target with resolved_paths } in
                          let types =
                            List.map
                              (fun local ->
                                if local.type_id = target.type_id then target
                                else local)
                              types
                          in
                          declarations_loop type_index types
                            ((Option.get public_declaration, target)
                            :: public_type_links)
                            remaining
                      | None ->
                          let resolved_paths =
                            Path.Pident declaration.typ_id
                            :: Option.fold ~none:[]
                                 ~some:(fun public_declaration ->
                                   [
                                     Path.Pdot
                                       ( Path.Pident module_id,
                                         public_declaration.typ_name.txt );
                                   ])
                                 public_declaration
                          in
                          let local =
                            {
                              resolved_paths;
                              type_id =
                                {
                                  Sst.type_index;
                                  type_name =
                                    module_name ^ "." ^ declaration.typ_name.txt;
                                };
                              declaration;
                            }
                          in
                          declarations_loop (type_index + 1) (local :: types)
                            (Option.fold ~none:public_type_links
                               ~some:(fun public ->
                                 (public, local) :: public_type_links)
                               public_declaration)
                            remaining)
                in
                declarations_loop type_index types public_type_links
                  declarations
            | Tstr_value (rec_flag, bindings) ->
                let bindings = Broadcast.source_bindings broadcast_scan bindings in
                if rec_flag = Asttypes.Recursive && List.length bindings <> 1 then
                  unsupported context item.str_loc Diagnostic.Mutual_recursion
                else
                let rec bindings_loop function_index functions public_functions
                    = function
                  | [] ->
                      implementation_items type_index function_index types
                        functions public_type_links public_functions rest
                  | value_binding :: remaining -> (
                      let* function_ =
                        top_level_binding
                          ~module_identity:(module_id, module_name) context
                          function_index rec_flag value_binding
                      in
                      let public = public_value (Ident.name function_.ident) in
                      match (public, function_.function_kind) with
                      | Some public, _ ->
                          bindings_loop (function_index + 1)
                            (function_ :: functions)
                            ((public, function_) :: public_functions)
                            remaining
                      | ( None,
                          Top_recursive_spec
                            { recursive_visibility = Some `Opaque; _ } ) ->
                          bindings_loop (function_index + 1) (function_ :: functions)
                            public_functions remaining
                      | None, Top_spec _
                        when Option.is_some (symbolic_binding value_binding) ->
                          bindings_loop (function_index + 1)
                            (function_ :: functions) public_functions remaining
                      | None, _ ->
                          unsupported context value_binding.vb_loc
                            Diagnostic.Unsupported_structure_item)
                in
                bindings_loop function_index functions public_functions bindings
            | Tstr_open declaration when authenticated_imported_open declaration
              ->
                implementation_items type_index function_index types functions
                  public_type_links public_functions rest
            | Tstr_eval _ | Tstr_primitive _ | Tstr_typext _ | Tstr_exception _
            | Tstr_module _ | Tstr_recmodule _ | Tstr_modtype _ | Tstr_open _
            | Tstr_class _ | Tstr_class_type _ | Tstr_include _ ->
                unsupported_item item)
      in
      let* ( type_index,
             function_index,
             types,
             functions,
             public_type_links,
             public_functions ) =
        implementation_items type_index function_index types functions [] []
          implementation.str_items
      in
      let candidate =
        {
          module_name;
          module_id;
          signature_declaration;
          constraint_location;
          public_type_links;
          public_functions;
        }
      in
      Ok (type_index, function_index, types, functions, candidate)
    in
    let rec items type_index function_index types functions = function
      | [] -> (
          match (!module_type_declaration, !constrained_module) with
          | None, None -> Ok (List.rev types, List.rev functions, [])
          | Some _, Some module_ ->
              Ok (List.rev types, List.rev functions, [ module_ ])
          | Some declaration, None ->
              unsupported context declaration.mtd_loc
                Diagnostic.Unsupported_structure_item
          | None, Some module_ ->
              unsupported context module_.constraint_location
                Diagnostic.Unsupported_structure_item)
      | item :: rest -> (
          match item.str_desc with
          | Tstr_value (rec_flag, bindings) ->
              let bindings =
                Broadcast.source_bindings broadcast_scan
                  bindings
              in
              let* () =
                if rec_flag = Asttypes.Recursive && List.length bindings > 1 then
                  unsupported context item.str_loc Diagnostic.Mutual_recursion
                else Ok ()
              in
              let rec group function_index functions = function
                | [] -> items type_index function_index types functions rest
                | binding :: bindings ->
                    let* function_ =
                      top_level_binding context function_index rec_flag binding
                    in
                    group (function_index + 1) (function_ :: functions) bindings
              in
              group function_index functions bindings
          | Tstr_attribute _ ->
              items type_index function_index types functions rest
          | Tstr_open declaration when authenticated_imported_open declaration
            ->
              items type_index function_index types functions rest
          | Tstr_type (_, declarations) ->
              let rec group type_index types = function
                | [] -> items type_index function_index types functions rest
                | declaration :: remaining ->
                    let local =
                      {
                        resolved_paths = [ Path.Pident declaration.typ_id ];
                        type_id =
                          {
                            Sst.type_index;
                            type_name = declaration.typ_name.txt;
                          };
                        declaration;
                      }
                    in
                    group (type_index + 1) (local :: types) remaining
              in
              group type_index types declarations
          | Tstr_modtype declaration -> (
              match !module_type_declaration with
              | None ->
                  module_type_declaration := Some declaration;
                  items type_index function_index types functions rest
              | Some _ -> unsupported_item item)
          | Tstr_module binding -> (
              match (!module_type_declaration, !constrained_module) with
              | Some signature_declaration, None ->
                  let* type_index, function_index, types, functions, module_ =
                    collect_constrained_module type_index function_index types
                      functions binding signature_declaration
                  in
                  constrained_module := Some module_;
                  items type_index function_index types functions rest
              | None, _ | _, Some _ -> unsupported_item item)
          | Tstr_eval _ | Tstr_primitive _ | Tstr_typext _ | Tstr_exception _
          | Tstr_recmodule _ | Tstr_open _ | Tstr_class _ | Tstr_class_type _
          | Tstr_include _ ->
              unsupported_item item)
    in
    items 0 0 [] [] structure.str_items
  let source_parameter_patterns function_ =
    Callback_shape_private.source_parameter_patterns function_.value_binding
  let source_definition_body = Sst_callback_private.source_definition_body
  let source_function_type_variables function_ =
    Typedtree_callback_private.source_type_variables
      ~parameters:
        (Option.value ~default:[] (source_parameter_patterns function_))
      ~result:(source_definition_body function_)
      ~substitutions:function_.type_substitutions
  let source_type_variable_ids =
    Parametric_lowering_private.source_type_variable_ids
  let first_order_type = Parametric_lowering_private.first_order_source_type
  let specification_function_source_type = Spec_function_type_private.source_type
  let contains_local_generic_aggregate local_types =
    Parametric_lowering_private.contains_source_application
      ~is_application:(fun path ->
        match find_local_type_by_path path local_types with
        | Some local -> local.declaration.typ_params <> []
        | None -> false)
  let variables_under_local_generic_aggregate local_types =
    Parametric_lowering_private.variables_under_source_application
      ~is_application:(fun path ->
        match find_local_type_by_path path local_types with
        | Some local -> local.declaration.typ_params <> []
        | None -> false)
  let result_kind_allowed function_ =
    match source_definition_body function_ with
    | None -> false
    | Some body -> (
        match (function_.function_kind, Types.get_desc body.exp_type) with
        | ( (Top_proof _ | Top_external_body (Sst.Proof, _)),
            Types.Tconstr (path, [], _) )
          when Path.same path Predef.path_unit ->
            true
        | (Top_spec _ | Top_recursive_spec _), Types.Tconstr (path, [], _)
          when Path.same path Predef.path_int || Path.same path Predef.path_bool
          ->
            true
        | Top_exec, _
        | Top_type_invariant _, _
        | Top_external_specification _, _
        | Top_external_body _, _
        | Top_proof _, _
        | (Top_spec _ | Top_recursive_spec _), _ ->
            false)
  let generic_signature_types function_ =
    Callback_shape_private.source_signature_types function_.value_binding
      ~definition_body:(source_definition_body function_)
  let generic_first_class_surface function_ = Typedtree_spec_function_private.supported_first_class_surface ~recursive:(function_.rec_flag = Asttypes.Recursive) ~signature:(generic_signature_types function_) function_.value_binding.vb_expr
  let generic_function_diagnostic source_file function_ =
    let location =
      match source_parameter_patterns function_ with
      | Some (parameter :: _) -> parameter.pat_loc
      | Some [] | None -> function_.value_binding.vb_loc
    in
    let construct =
      let first_order =
        match function_.function_kind with
        | (Top_spec _ | Top_recursive_spec _) when generic_first_class_surface function_ ->
            specification_function_source_type
        | _ -> first_order_type
      in
      if List.for_all first_order (generic_signature_types function_)
      then
        Diagnostic.Unsupported_generic_use
      else Diagnostic.Higher_order_function
    in
    Error
      (Diagnostic.make (Diagnostic.Unsupported_construct construct)
         (Diagnostic.span_of_location ~fallback_file:source_file location))
  let generic_is_public ~explicit_interface ~interface_value_paths
      constrained_modules function_ =
    List.exists
      (fun candidate ->
        List.exists
          (fun (_, public_function) ->
            Ident.same public_function.ident function_.ident)
          candidate.public_functions)
      constrained_modules
    || explicit_interface
       && List.exists
            (fun path ->
              List.mem ("value:" ^ Path.name path) interface_value_paths)
            function_.resolved_paths
  let reject_public_generic_functions ~allow_root_parametric:_
      ~explicit_interface ~interface_value_paths source_file functions
      constrained_modules =
    match
      List.find_opt
        (fun function_ ->
          source_function_type_variables function_ <> []
          && generic_is_public ~explicit_interface ~interface_value_paths
               constrained_modules function_
          && String.contains function_.function_id.function_name '.')
        functions
    with
    | None -> Ok ()
    | Some function_ -> generic_function_diagnostic source_file function_
  let rec parameterize_iter_result f = function
    | [] -> Ok ()
    | item :: rest ->
        let* () = f item in
        parameterize_iter_result f rest
  let parameterize_functions source_file broadcast_scan functions
      constrained_modules =
    let eligible function_ =
      let kind =
        match function_.function_kind with
        | Top_exec -> Parametric_function_selection_private.Exec
        | Top_spec _ -> Parametric_function_selection_private.Spec
        | Top_recursive_spec _ ->
            Parametric_function_selection_private.Recursive_spec
        | Top_proof _ -> Parametric_function_selection_private.Proof
        | Top_external_body (Sst.Proof, _) ->
            Parametric_function_selection_private.Proof
        | Top_type_invariant _ | Top_external_specification _
        | Top_external_body _ ->
            Parametric_function_selection_private.Other
      in
      let ordinary =
        Parametric_function_selection_private.eligible ~kind
          ~recursive:(function_.rec_flag = Asttypes.Recursive)
          ~type_variables:(source_function_type_variables function_)
          ~signature:(generic_signature_types function_)
      in
      let specification_function =
        let signature = generic_signature_types function_ in
        let kind =
          if
            Broadcast.is_declaration broadcast_scan function_.value_binding
          then Parametric_function_selection_private.Spec
          else kind
        in
        generic_first_class_surface function_
        && Spec_function_type_private.generic_eligible ~kind
             ~type_variables:(source_function_type_variables function_)
             ~signature
      in
      let callback_signature =
        Callback_shape_private.compiler_callback_signature
          ~first_order:first_order_type
      in
      let signature = generic_signature_types function_ in
      ordinary || specification_function
      || kind = Parametric_function_selection_private.Exec
         && function_.rec_flag = Asttypes.Nonrecursive
         && source_function_type_variables function_ <> []
         && List.exists callback_signature signature
         && List.for_all
              (fun typ -> first_order_type typ || callback_signature typ)
              signature
    in
    let* () =
      functions
      |> parameterize_iter_result (fun function_ ->
          if not (eligible function_) then Ok ()
          else
            match function_.function_kind with
            | Top_exec | Top_spec _ | Top_recursive_spec _ | Top_proof _
            | Top_external_specification _ ->
                Ok ()
            | Top_external_body (Sst.Proof, _) ->
                Ok ()
            | Top_type_invariant _ | Top_external_body _ ->
                generic_function_diagnostic source_file function_)
    in
    let functions =
      List.map
        (fun function_ ->
          if not (eligible function_) then function_
          else
            let owner =
              Parametric_type.owner ~index:function_.function_id.function_index
                ~name:function_.function_id.function_name
            in
            let parametric_type_binders =
              source_function_type_variables function_
              |> List.mapi (fun ordinal type_id ->
                  (type_id, Parametric_type.binder owner ~ordinal))
            in
            { function_ with parametric_type_binders })
        functions
    in
    let* () =
      functions
      |> parameterize_iter_result (fun function_ ->
          if
            source_function_type_variables function_ = []
            || function_.parametric_type_binders <> []
          then Ok ()
          else generic_function_diagnostic source_file function_)
    in
    let remap function_ =
      match
        List.filter
          (fun candidate -> Ident.same candidate.ident function_.ident)
          functions
      with
      | [ candidate ] -> Ok candidate
      | [] | _ :: _ :: _ -> generic_function_diagnostic source_file function_
    in
    let rec remap_modules remapped = function
      | [] -> Ok (List.rev remapped)
      | candidate :: rest ->
          let rec remap_public public = function
            | [] ->
                remap_modules
                  ({ candidate with public_functions = List.rev public }
                  :: remapped)
                  rest
            | (value, function_) :: functions ->
                let* function_ = remap function_ in
                remap_public ((value, function_) :: public) functions
          in
          remap_public [] candidate.public_functions
    in
    let* constrained_modules = remap_modules [] constrained_modules in
    Ok (functions, constrained_modules)
  type rank_polarity = Positive | Negative
  type rank_source_field = {
    source_constructor : constructor_declaration;
    source_constructor_index : int;
    source_field_index : int;
    source_field_name : string;
    source_field_uid : string;
    source_field_type : Types.type_expr;
    source_field_mutable : bool;
  }
  type rank_occurrence = {
    occurrence_target : local_type;
    occurrence_polarity : rank_polarity;
    occurrence_trace : string list;
    occurrence_child_path : int list;
  }
  let rank_type_path local =
    match local.resolved_paths with
    | path :: _ -> Path.name path
    | [] -> Ident.unique_name local.declaration.typ_id
  let rank_uid_to_string uid = Format.asprintf "%a" Types.Uid.print uid
  let rank_type_uid local =
    rank_uid_to_string local.declaration.typ_type.type_uid
  let rank_type_label local =
    Printf.sprintf "type:%s[path=%s,uid=%s]" local.declaration.typ_name.txt
      (rank_type_path local) (rank_type_uid local)
  let rank_source_fields local =
    match local.declaration.typ_kind with
    | Ttype_variant constructors ->
        List.mapi
          (fun constructor_index constructor ->
            let constructor_label =
              Printf.sprintf "constructor:%s[uid=%s]" constructor.cd_name.txt
                (rank_uid_to_string constructor.cd_uid)
            in
            match constructor.cd_args with
            | Cstr_tuple arguments ->
                List.mapi
                  (fun source_field_index argument ->
                    {
                      source_constructor = constructor;
                      source_constructor_index = constructor_index;
                      source_field_index;
                      source_field_name =
                        Printf.sprintf "$%d" source_field_index;
                      source_field_uid =
                        Printf.sprintf "tuple:%s:%d"
                          (rank_uid_to_string constructor.cd_uid)
                          source_field_index;
                      source_field_type = argument.ca_type.ctyp_type;
                      source_field_mutable = false;
                    })
                  arguments
                |> List.map (fun field -> (constructor_label, field))
            | Cstr_record labels ->
                List.mapi
                  (fun source_field_index label ->
                    {
                      source_constructor = constructor;
                      source_constructor_index = constructor_index;
                      source_field_index;
                      source_field_name = label.ld_name.txt;
                      source_field_uid = rank_uid_to_string label.ld_uid;
                      source_field_type = label.ld_type.ctyp_type;
                      source_field_mutable =
                        (match label.ld_mutable with
                        | Types.Immutable -> false
                        | Types.Mutable _ -> true);
                    })
                  labels
                |> List.map (fun field -> (constructor_label, field)))
          constructors
        |> List.concat
    | Ttype_abstract | Ttype_record _ | Ttype_record_unboxed_product _
    | Ttype_open ->
        []
  let rank_declaration_expressions local =
    let manifest =
      Option.to_list local.declaration.typ_manifest
      |> List.map (fun manifest -> ("manifest", manifest.ctyp_type))
    in
    let fields =
      match local.declaration.typ_kind with
      | Ttype_variant _ ->
          rank_source_fields local
          |> List.map (fun (constructor_label, field) ->
              ( Printf.sprintf "%s>field:%s[uid=%s]" constructor_label
                  field.source_field_name field.source_field_uid,
                field.source_field_type ))
      | Ttype_record labels | Ttype_record_unboxed_product labels ->
          List.map
            (fun label ->
              ( Printf.sprintf "record-field:%s[uid=%s]" label.ld_name.txt
                  (rank_uid_to_string label.ld_uid),
                label.ld_type.ctyp_type ))
            labels
      | Ttype_abstract | Ttype_open -> []
    in
    manifest @ fields
  let constrained_module_has_finite_signature candidate =
    List.exists
      (fun (public, function_) ->
        List.exists
          (fun attribute ->
            String.equal attribute.Parsetree.attr_name.txt
              "verocaml.internal.finite_signature")
          (public.Typedtree.val_attributes
         @ function_.value_binding.Typedtree.vb_attributes))
      candidate.public_functions
  let constrained_module_has_private_opaque_recursive_spec candidate functions =
    List.exists
      (fun function_ ->
        String.starts_with
          ~prefix:(candidate.module_name ^ ".")
          function_.function_id.function_name
        && (not
              (List.exists
                 (fun (_, public_function) ->
                   Ident.same public_function.ident function_.ident)
                 candidate.public_functions))
        &&
        match function_.function_kind with
        | Top_recursive_spec { recursive_visibility = Some `Opaque; _ } -> true
        | Top_exec | Top_spec _ | Top_type_invariant _ | Top_recursive_spec _
        | Top_proof _ | Top_external_specification _ | Top_external_body _ ->
            false)
      functions
  let rank_local_by_path local_types path =
    find_local_type_by_path path local_types
  let rank_substitute substitutions typ =
    match List.assoc_opt (Types.get_id typ) substitutions with
    | Some replacement -> replacement
    | None -> typ
  let rank_known_parameter_label local_types typ =
    let id = Types.get_id typ in
    local_types
    |> List.find_map (fun local ->
        local.declaration.typ_params
        |> List.mapi (fun index (parameter, _) ->
            if Types.get_id parameter.ctyp_type = id then
              Some
                (match parameter.ctyp_desc with
                | Ttyp_var (name, _) ->
                    Printf.sprintf "parameter:%s#%d"
                      (Option.value ~default:"_" name)
                      index
                | _ -> Printf.sprintf "parameter:<unsupported>#%d" index)
            else None)
        |> List.find_map Fun.id)
  let rec rank_actual_type_label local_types typ =
    match Types.get_desc typ with
    | Types.Tvar _ | Types.Tunivar _ ->
        Option.value ~default:"open"
          (rank_known_parameter_label local_types typ)
    | Types.Tarrow (_, domain, codomain, _) ->
        Printf.sprintf "arrow(%s,%s)"
          (rank_actual_type_label local_types domain)
          (rank_actual_type_label local_types codomain)
    | Types.Ttuple components ->
        components
        |> List.map (fun (label, component) ->
            Printf.sprintf "%s:%s"
              (Option.value ~default:"_" label)
              (rank_actual_type_label local_types component))
        |> String.concat "," |> Printf.sprintf "tuple(%s)"
    | Types.Tconstr (path, arguments, _) ->
        let target =
          match rank_local_by_path local_types path with
          | Some local -> rank_type_label local
          | None -> "external:" ^ Path.name path
        in
        Printf.sprintf "%s<%s>" target
          (String.concat ","
             (List.map (rank_actual_type_label local_types) arguments))
    | Types.Tpoly (body, _) -> rank_actual_type_label local_types body
    | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
        rank_actual_type_label local_types replacement
    | Types.Tunboxed_tuple _ -> "unsupported:unboxed-tuple"
    | Types.Tobject _ -> "unsupported:object"
    | Types.Tfield _ -> "unsupported:object-field"
    | Types.Tnil -> "unsupported:object-end"
    | Types.Tvariant _ -> "unsupported:polymorphic-variant"
    | Types.Tpackage _ -> "unsupported:package"
    | Types.Tquote _ -> "unsupported:quoted"
    | Types.Tsplice _ -> "unsupported:splice"
    | Types.Tof_kind _ -> "unsupported:higher-kinded"
  let rank_occurrences ?(stop_at = fun _ -> false)
      ?(trace_substitutions = false) local_types root initial_trace typ =
    let rec walk polarity substitutions active expansions trace child_path typ =
      let typ = rank_substitute substitutions typ in
      match Types.get_desc typ with
      | Types.Tvar _ | Types.Tunivar _ -> []
      | Types.Tarrow (_, domain, codomain, _) ->
          walk
            (match polarity with Positive -> Negative | Negative -> Positive)
            substitutions active expansions
            (trace @ [ "arrow-domain" ])
            child_path domain
          @ walk polarity substitutions active expansions
              (trace @ [ "arrow-codomain" ])
              child_path codomain
      | Types.Ttuple components ->
          List.mapi
            (fun index (label, component) ->
              walk polarity substitutions active expansions
                (trace
                @ [
                    Printf.sprintf "tuple:%s" (Option.value ~default:"_" label);
                  ])
                (child_path @ [ index ]) component)
            components
          |> List.concat
      | Types.Tconstr (path, arguments, _) -> (
          let arguments = List.map (rank_substitute substitutions) arguments in
          match rank_local_by_path local_types path with
          | None ->
              List.concat_map
                (walk polarity substitutions active expansions
                   (trace @ [ "external-application:" ^ Path.name path ])
                   child_path)
                arguments
          | Some target ->
              let parameters =
                List.map
                  (fun (parameter, _) -> Types.get_id parameter.ctyp_type)
                  target.declaration.typ_params
              in
              let substitution_trace =
                if
                  trace_substitutions
                  && List.length parameters = List.length arguments
                then
                  List.mapi
                    (fun index argument ->
                      Printf.sprintf "substitute:parameter#%d=%s" index
                        (rank_actual_type_label local_types argument))
                    arguments
                else []
              in
              let occurrence_trace =
                trace
                @ [ "apply:" ^ rank_type_label target ]
                @ substitution_trace
              in
              let current =
                {
                  occurrence_target = target;
                  occurrence_polarity = polarity;
                  occurrence_trace;
                  occurrence_child_path = child_path;
                }
              in
              let key =
                ( target.type_id.type_index,
                  List.map Types.get_id arguments,
                  polarity )
              in
              if stop_at target || List.mem key active then [ current ]
              else
                let parameter_bindings =
                  if List.length parameters = List.length arguments then
                    List.combine parameters arguments
                  else []
                in
                let nested =
                  rank_declaration_expressions target
                  |> List.concat_map (fun (step, expression) ->
                      walk polarity
                        (parameter_bindings @ substitutions)
                        (key :: active) (target :: expansions)
                        (occurrence_trace
                        @ [ "expand:" ^ rank_type_label target; step ])
                        child_path expression)
                in
                current :: nested)
      | Types.Tpoly (body, variables) ->
          if variables = [] then
            walk polarity substitutions active expansions trace child_path body
          else []
      | Types.Tlink replacement ->
          walk polarity substitutions active expansions trace child_path
            replacement
      | Types.Tsubst (replacement, _) ->
          walk polarity substitutions active expansions trace child_path
            replacement
      | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
      | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
      | Types.Tof_kind _ ->
          []
    in
    walk Positive [] [] [ root ] initial_trace [] typ
  let rank_direct_dependencies local_types local =
    let rec collect seen typ =
      match Types.get_desc typ with
      | Types.Tconstr (path, arguments, _) ->
          let seen =
            match rank_local_by_path local_types path with
            | Some target -> target.type_id :: seen
            | None -> seen
          in
          List.fold_left collect seen arguments
      | Types.Tarrow (_, domain, codomain, _) ->
          collect (collect seen domain) codomain
      | Types.Ttuple components ->
          List.fold_left
            (fun seen (_, component) -> collect seen component)
            seen components
      | Types.Tpoly (body, _) | Types.Tlink body | Types.Tsubst (body, _) ->
          collect seen body
      | Types.Tvar _ | Types.Tunivar _ | Types.Tunboxed_tuple _
      | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
      | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _
        ->
          seen
    in
    rank_declaration_expressions local
    |> List.fold_left
         (fun dependencies (_, expression) -> collect dependencies expression)
         []
    |> List.sort_uniq compare
  let rank_same_type left right = left.type_id = right.type_id
  let rank_reachable dependencies source target =
    let rec visit seen = function
      | [] -> false
      | current :: rest ->
          if current = target.type_id then true
          else if List.mem current seen then visit seen rest
          else
            let next =
              Option.value ~default:[] (List.assoc_opt current dependencies)
            in
            visit (current :: seen) (next @ rest)
    in
    visit []
      (Option.value ~default:[] (List.assoc_opt source.type_id dependencies))
  let rank_recursive_component dependencies local_types root =
    local_types
    |> List.filter (fun candidate ->
        rank_same_type candidate root
        || rank_reachable dependencies root candidate
           && rank_reachable dependencies candidate root)
    |> List.sort (fun left right ->
        Int.compare left.type_id.type_index right.type_id.type_index)
  let rank_is_recursive dependencies component =
    match component with
    | [] -> false
    | [ local ] ->
        List.mem local.type_id
          (Option.value ~default:[] (List.assoc_opt local.type_id dependencies))
    | _ :: _ -> true
  let invalid_rank source_file location detail =
    Error
      (Diagnostic.make (Diagnostic.Invalid_recursive_rank detail)
         (Diagnostic.span_of_location ~fallback_file:source_file location))
  let analyze_recursive_rank_domains source_file local_types roots =
    let dependencies =
      List.map
        (fun local ->
          (local.type_id, rank_direct_dependencies local_types local))
        local_types
    in
    let recursive_occurrences local =
      rank_declaration_expressions local
      |> List.concat_map (fun (step, expression) ->
          rank_occurrences ~trace_substitutions:true local_types local
            [ rank_type_label local; step ]
            expression)
      |> List.filter (fun occurrence ->
          rank_same_type occurrence.occurrence_target local)
    in
    let all_recursive_occurrences =
      List.map (fun local -> (local, recursive_occurrences local)) roots
    in
    let negative =
      List.find_map
        (fun (local, occurrences) ->
          Option.map
            (fun occurrence -> (local, occurrence))
            (List.find_opt
               (fun occurrence -> occurrence.occurrence_polarity = Negative)
               occurrences))
        all_recursive_occurrences
    in
    match negative with
    | Some (local, occurrence) ->
        let crosses_generic_application =
          List.exists
            (fun candidate ->
              candidate.declaration.typ_params <> []
              && List.mem
                   ("apply:" ^ rank_type_label candidate)
                   occurrence.occurrence_trace)
            local_types
        in
        invalid_rank source_file local.declaration.typ_loc
          (Printf.sprintf
             "%s in %s; expansion trace: %s"
             (if crosses_generic_application then
                "generic rank application uses a prohibited negative parameter"
              else "negative recursive occurrence")
             (rank_type_label local)
             (String.concat " -> " occurrence.occurrence_trace))
    | None ->
        let unsupported_external_expansion =
          List.find_map
            (fun (local, occurrences) ->
              Option.map
                (fun occurrence -> (local, occurrence))
                (List.find_opt
                   (fun occurrence ->
                     List.exists
                       (String.starts_with ~prefix:"external-application:")
                       occurrence.occurrence_trace)
                   occurrences))
            all_recursive_occurrences
        in
        let* () =
          match unsupported_external_expansion with
          | Some (local, occurrence) ->
              invalid_rank source_file local.declaration.typ_loc
                (Printf.sprintf
                   "recursive rank occurrence crosses an unsupported external \
                    expansion in %s; expansion trace: %s"
                   (rank_type_label local)
                   (String.concat " -> " occurrence.occurrence_trace))
          | None -> Ok ()
        in
        let components =
          roots
          |> List.filter_map (fun root ->
              let component =
                rank_recursive_component dependencies local_types root
              in
              if not (rank_is_recursive dependencies component) then None
              else
                let key =
                  List.map (fun local -> local.type_id.type_index) component
                in
                Some (key, component))
          |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)
          |> List.map snd
        in
        let rec certify certified = function
          | [] -> Ok (List.rev certified)
          | component :: rest -> (
              let unsupported =
                List.find_opt
                  (fun local ->
                    local.declaration.typ_params <> []
                    || local.declaration.typ_cstrs <> []
                    || local.declaration.typ_private <> Asttypes.Public
                    || local.declaration.typ_manifest <> None
                    ||
                    match local.declaration.typ_kind with
                    | Ttype_variant _ -> false
                    | Ttype_abstract | Ttype_record _
                    | Ttype_record_unboxed_product _ | Ttype_open ->
                        true)
                  component
              in
              match unsupported with
              | Some local
                when (match local.declaration.typ_kind with
                       | Ttype_record _ | Ttype_record_unboxed_product _ -> true
                       | Ttype_abstract | Ttype_variant _ | Ttype_open -> false)
                     && local.declaration.typ_manifest = None
                     && local.declaration.typ_params = [] ->
                  invalid_rank source_file local.declaration.typ_loc
                    (Printf.sprintf
                       "recursive rank component contains a record-only \
                        construction cycle at %s"
                       (rank_type_label local))
              | Some _ ->
                  (* Existing unsupported generic/alias/private/open forms
                       keep their established frontend diagnostic and receive
                       no rank certificate. *)
                  certify certified rest
              | None -> (
                  let unboxed =
                    List.find_opt
                      (fun local ->
                        match local.declaration.typ_type.type_kind with
                        | Types.Type_variant (_, Types.Variant_boxed _, _) ->
                            false
                        | _ -> true)
                      component
                  in
                  match unboxed with
                  | Some local ->
                      invalid_rank source_file local.declaration.typ_loc
                        (Printf.sprintf
                           "recursive rank requires a boxed closed variant: %s"
                           (rank_type_label local))
                  | None -> (
                      let mutable_field =
                        List.find_map
                          (fun local ->
                            rank_source_fields local
                            |> List.find_map (fun (_, field) ->
                                if field.source_field_mutable then
                                  Some (local, field)
                                else None))
                          component
                      in
                      match mutable_field with
                      | Some _ ->
                          (* Mutable recursive aggregates remain part of the
                               pre-existing executable/owned-tree subset, but
                               receive no rank authority. *)
                          certify certified rest
                      | None ->
                          let in_component target =
                            List.exists (rank_same_type target) component
                          in
                          let children =
                            List.concat_map
                              (fun local ->
                                rank_source_fields local
                                |> List.concat_map
                                     (fun (constructor_label, field) ->
                                       rank_occurrences ~stop_at:in_component
                                         local_types local
                                         [
                                           rank_type_label local;
                                           constructor_label;
                                           Printf.sprintf "field:%s[uid=%s]"
                                             field.source_field_name
                                             field.source_field_uid;
                                         ]
                                         field.source_field_type
                                       |> List.filter (fun occurrence ->
                                           in_component
                                             occurrence.occurrence_target)
                                       |> List.map (fun occurrence ->
                                           let constructor =
                                             {
                                               Sst.constructor_type =
                                                 local.type_id;
                                               constructor_index =
                                                 field.source_constructor_index;
                                               constructor_name =
                                                 field.source_constructor
                                                   .cd_name
                                                   .txt;
                                             }
                                           in
                                           {
                                             rank_constructor = constructor;
                                             rank_constructor_uid =
                                               rank_uid_to_string
                                                 field.source_constructor.cd_uid;
                                             rank_field =
                                               {
                                                 Sst.field_owner =
                                                   Sst.Constructor_owner
                                                     constructor;
                                                 field_index =
                                                   field.source_field_index;
                                                 field_name =
                                                   field.source_field_name;
                                               };
                                             rank_field_uid =
                                               field.source_field_uid;
                                             rank_child_path =
                                               occurrence.occurrence_child_path;
                                             rank_child_type =
                                               occurrence.occurrence_target
                                                 .type_id;
                                             rank_expansion_trace =
                                               occurrence.occurrence_trace;
                                           })))
                              component
                          in
                          let witnesses =
                            List.concat_map
                              (fun local ->
                                match local.declaration.typ_kind with
                                | Ttype_variant constructors ->
                                    List.mapi
                                      (fun constructor_index constructor ->
                                        let fields =
                                          rank_source_fields local
                                          |> List.filter (fun (_, field) ->
                                              field.source_constructor_index
                                              = constructor_index)
                                        in
                                        let depends_on_component =
                                          List.exists
                                            (fun (_, field) ->
                                              rank_occurrences
                                                ~stop_at:in_component
                                                local_types local []
                                                field.source_field_type
                                              |> List.exists (fun occurrence ->
                                                  in_component
                                                    occurrence.occurrence_target))
                                            fields
                                        in
                                        if depends_on_component then None
                                        else
                                          Some
                                            {
                                              rank_ground_constructor =
                                                {
                                                  Sst.constructor_type =
                                                    local.type_id;
                                                  constructor_index;
                                                  constructor_name =
                                                    constructor.cd_name.txt;
                                                };
                                              rank_ground_constructor_uid =
                                                rank_uid_to_string
                                                  constructor.cd_uid;
                                            })
                                      constructors
                                    |> List.filter_map Fun.id
                                | Ttype_abstract | Ttype_record _
                                | Ttype_record_unboxed_product _ | Ttype_open ->
                                    [])
                              component
                          in
                          if witnesses = [] then
                            let local = List.hd component in
                            invalid_rank source_file local.declaration.typ_loc
                              (Printf.sprintf
                                 "recursive rank component has no finite \
                                  ground constructor: %s"
                                 (String.concat ", "
                                    (List.map rank_type_label component)))
                          else
                            let identities =
                              List.map
                                (fun local ->
                                  {
                                    rank_type_id = local.type_id;
                                    rank_path = rank_type_path local;
                                    rank_uid = rank_type_uid local;
                                    rank_span =
                                      Diagnostic.span_of_location
                                        ~fallback_file:source_file
                                        local.declaration.typ_loc;
                                  })
                                component
                            in
                            certify
                              ({
                                 pending_component = identities;
                                 pending_positive_children = children;
                                 pending_ground_witnesses = witnesses;
                                 pending_actual_evidence = [];
                               }
                              :: certified)
                              rest)))
        in
        certify [] components
  let rec iter_result f = function
    | [] -> Ok ()
    | value :: rest ->
        let* () = f value in
        iter_result f rest
  type pending_rank_profile = {
    pending_profile_local : local_type;
    pending_profile_identity : rank_type_identity;
    pending_profile_parameters : rank_parameter_profile list;
    pending_profile_dependencies : rank_type_identity list;
    pending_profile_ground_traces : string list list;
    pending_profile_independently_grounded : bool;
    pending_profile_base_digest : string;
  }
  let pending_rank_profile_id profile =
    let identity = profile.pending_profile_identity in
    Printf.sprintf "rank-profile/v1/%s/%s/%s" identity.rank_path
      identity.rank_uid profile.pending_profile_base_digest
  let rank_identity source_file local =
    {
      rank_type_id = local.type_id;
      rank_path = rank_type_path local;
      rank_uid = rank_type_uid local;
      rank_span =
        Diagnostic.span_of_location ~fallback_file:source_file
          local.declaration.typ_loc;
    }
  let rank_parameter_identity index parameter =
    match parameter.ctyp_desc with
    | Ttyp_var (name, _) ->
        Printf.sprintf "parameter:%s#%d" (Option.value ~default:"_" name) index
    | _ -> Printf.sprintf "parameter:<non-variable>#%d" index
  let rank_variance_label variance =
    let may_positive, may_negative = Types.Variance.get_upper variance in
    let positive, negative, injective = Types.Variance.get_lower variance in
    Printf.sprintf
      "may-positive=%b,may-negative=%b,positive=%b,negative=%b,injective=%b"
      may_positive may_negative positive negative injective
  let rank_parameter_ids local =
    let variances = local.declaration.typ_type.type_variance in
    List.mapi
      (fun index (parameter, _) ->
        ( Types.get_id parameter.ctyp_type,
          index,
          rank_parameter_identity index parameter,
          Option.value ~default:Types.Variance.unknown
            (List.nth_opt variances index) ))
      local.declaration.typ_params
  let rank_scalar_external path arguments =
    arguments = []
    && match Path.name path with "int" | "bool" | "unit" -> true | _ -> false
  let rank_render_type local_types parameter_ids =
    let rec render active typ =
      match List.assoc_opt (Types.get_id typ) parameter_ids with
      | Some identity -> identity
      | None -> (
          match rank_known_parameter_label local_types typ with
          | Some identity -> identity
          | None -> (
              match Types.get_desc typ with
              | Types.Tvar { name; _ } ->
                  Printf.sprintf "open:%s#%d"
                    (Option.value ~default:"_" name)
                    (Types.get_id typ)
              | Types.Tunivar { name; _ } ->
                  Printf.sprintf "open:%s#%d"
                    (Option.value ~default:"_" name)
                    (Types.get_id typ)
              | Types.Tarrow (_, domain, codomain, _) ->
                  Printf.sprintf "arrow(%s,%s)" (render active domain)
                    (render active codomain)
              | Types.Ttuple components ->
                  components
                  |> List.map (fun (label, component) ->
                      Printf.sprintf "%s:%s"
                        (Option.value ~default:"_" label)
                        (render active component))
                  |> String.concat "," |> Printf.sprintf "tuple(%s)"
              | Types.Tconstr (path, arguments, _) ->
                  let target =
                    match rank_local_by_path local_types path with
                    | Some local ->
                        Printf.sprintf "%s[path=%s,uid=%s]"
                          local.declaration.typ_name.txt (rank_type_path local)
                          (rank_type_uid local)
                    | None -> "external:" ^ Path.name path
                  in
                  let key = target ^ "#" ^ string_of_int (Types.get_id typ) in
                  if List.mem key active then target ^ "<cycle>"
                  else
                    Printf.sprintf "%s<%s>" target
                      (String.concat ","
                         (List.map (render (key :: active)) arguments))
              | Types.Tpoly (body, variables) ->
                  Printf.sprintf "poly[%d](%s)" (List.length variables)
                    (render active body)
              | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
                  render active replacement
              | Types.Tunboxed_tuple _ -> "unsupported:unboxed-tuple"
              | Types.Tobject _ -> "unsupported:object"
              | Types.Tfield _ -> "unsupported:object-field"
              | Types.Tnil -> "unsupported:object-end"
              | Types.Tvariant _ -> "unsupported:polymorphic-variant"
              | Types.Tpackage _ -> "unsupported:package"
              | Types.Tquote _ -> "unsupported:quoted"
              | Types.Tsplice _ -> "unsupported:splice"
              | Types.Tof_kind _ -> "unsupported:higher-kinded"))
    in
    render []
  let rank_profile_declaration_snapshot local_types local =
    let parameter_ids =
      rank_parameter_ids local
      |> List.map (fun (id, _, identity, _) -> (id, identity))
    in
    let render = rank_render_type local_types parameter_ids in
    let expressions =
      rank_declaration_expressions local
      |> List.map (fun (step, expression) -> step ^ "=" ^ render expression)
    in
    let declaration_shape =
      match local.declaration.typ_kind with
      | Ttype_variant constructors ->
          constructors
          |> List.map (fun constructor ->
              let fields =
                match constructor.cd_args with
                | Cstr_tuple arguments ->
                    arguments
                    |> List.mapi (fun index argument ->
                        Printf.sprintf "$%d:%s" index
                          (render argument.ca_type.ctyp_type))
                | Cstr_record labels ->
                    labels
                    |> List.map (fun label ->
                        Printf.sprintf "%s:%s:%s" label.ld_name.txt
                          (rank_uid_to_string label.ld_uid)
                          (render label.ld_type.ctyp_type))
              in
              Printf.sprintf "constructor:%s:%s[%s]" constructor.cd_name.txt
                (rank_uid_to_string constructor.cd_uid)
                (String.concat "," fields))
          |> String.concat ";"
      | Ttype_abstract ->
          if local.declaration.typ_manifest = None then "abstract" else "alias"
      | Ttype_record _ -> "record"
      | Ttype_record_unboxed_product _ -> "unboxed-record"
      | Ttype_open -> "open"
    in
    String.concat "|"
      (rank_type_label local :: declaration_shape
       :: List.map
            (fun (_, index, identity, variance) ->
              Printf.sprintf "%d:%s:%s" index identity
                (rank_variance_label variance))
            (rank_parameter_ids local)
      @ expressions)
  let rank_profile_shape source_file local =
    let invalid detail =
      invalid_rank source_file local.declaration.typ_loc
        (Printf.sprintf "%s: %s" detail (rank_type_label local))
    in
    let* () =
      if local.declaration.typ_cstrs = [] then Ok ()
      else
        invalid "rank profile rejects constrained or higher-kinded parameters"
    in
    let* () =
      if local.declaration.typ_private = Asttypes.Public then Ok ()
      else invalid "rank profile rejects private dependencies"
    in
    let* () =
      local.declaration.typ_params
      |> iter_result (fun (parameter, _) ->
          match parameter.ctyp_desc with
          | Ttyp_var _ -> Ok ()
          | _ ->
              invalid
                "rank profile rejects higher-kinded or non-variable parameters")
    in
    match local.declaration.typ_kind with
    | Ttype_variant constructors ->
        let* () =
          match local.declaration.typ_type.type_kind with
          | Types.Type_variant (_, Types.Variant_boxed _, _) -> Ok ()
          | _ -> invalid "rank profile requires a boxed immutable variant"
        in
        constructors
        |> iter_result (fun constructor ->
            if constructor.cd_vars <> [] || constructor.cd_res <> None then
              invalid "rank profile rejects GADT dependencies"
            else
              match constructor.cd_args with
              | Cstr_tuple _ -> Ok ()
              | Cstr_record labels ->
                  if
                    List.exists
                      (fun label ->
                        match label.ld_mutable with
                        | Types.Mutable _ -> true
                        | Types.Immutable -> false)
                      labels
                  then invalid "rank profile rejects mutable dependencies"
                  else Ok ())
    | Ttype_abstract when local.declaration.typ_manifest <> None -> Ok ()
    | Ttype_abstract ->
        invalid
          "rank profile rejects abstract dependencies without a certificate"
    | Ttype_record _ | Ttype_record_unboxed_product _ ->
        invalid "rank profile rejects non-variant datatype dependencies"
    | Ttype_open -> invalid "rank profile rejects open datatype dependencies"
  let rank_profile_dependency_closure dependencies local_types roots =
    let rec visit seen = function
      | [] -> seen
      | local :: rest when List.exists (rank_same_type local) seen ->
          visit seen rest
      | local :: rest ->
          let direct =
            Option.value ~default:[] (List.assoc_opt local.type_id dependencies)
            |> List.filter_map (fun type_id ->
                List.find_opt
                  (fun candidate -> candidate.type_id = type_id)
                  local_types)
          in
          visit (local :: seen) (direct @ rest)
    in
    visit [] roots
    |> List.sort (fun left right ->
        Int.compare left.type_id.type_index right.type_id.type_index)
  let rank_validate_profile_expressions source_file local_types profiled local =
    let rec walk active trace typ =
      match Types.get_desc typ with
      | Types.Tvar _ | Types.Tunivar _ -> Ok ()
      | Types.Tarrow (_, domain, codomain, _) ->
          let* () = walk active (trace @ [ "arrow-domain" ]) domain in
          walk active (trace @ [ "arrow-codomain" ]) codomain
      | Types.Ttuple components ->
          components
          |> iter_result (fun (label, component) ->
              walk active
                (trace @ [ "tuple:" ^ Option.value ~default:"_" label ])
                component)
      | Types.Tconstr (path, arguments, _) -> (
          match rank_local_by_path local_types path with
          | None ->
              if rank_scalar_external path arguments then Ok ()
              else
                invalid_rank source_file local.declaration.typ_loc
                  (Printf.sprintf
                     "rank profile rejects unknown or external dependency %s; \
                      expansion trace: %s"
                     (Path.name path)
                     (String.concat " -> "
                        (trace @ [ "external-application:" ^ Path.name path ])))
          | Some target ->
              let target_parameters = rank_parameter_ids target in
              if List.length target_parameters <> List.length arguments then
                invalid_rank source_file local.declaration.typ_loc
                  (Printf.sprintf
                     "rank profile application arity mismatch for %s; \
                      expansion trace: %s"
                     (rank_type_label target)
                     (String.concat " -> "
                        (trace @ [ "apply:" ^ rank_type_label target ])))
              else if not (List.exists (rank_same_type target) profiled) then
                invalid_rank source_file local.declaration.typ_loc
                  (Printf.sprintf
                     "rank profile dependency is not sealed for %s; expansion \
                      trace: %s"
                     (rank_type_label target)
                     (String.concat " -> "
                        (trace @ [ "apply:" ^ rank_type_label target ])))
              else
                let key =
                  (target.type_id.type_index, List.map Types.get_id arguments)
                in
                let* () =
                  arguments
                  |> iter_result
                       (walk active (trace @ [ "application-argument" ]))
                in
                if List.mem key active then Ok ()
                else
                  rank_declaration_expressions target
                  |> iter_result (fun (step, expression) ->
                      walk (key :: active)
                        (trace
                        @ [
                            "apply:" ^ rank_type_label target;
                            "expand:" ^ rank_type_label target;
                            step;
                          ])
                        expression))
      | Types.Tpoly (body, variables) ->
          if variables = [] then walk active trace body
          else
            invalid_rank source_file local.declaration.typ_loc
              (Printf.sprintf
                 "rank profile rejects higher-kinded or locally polymorphic \
                  dependency; expansion trace: %s"
                 (String.concat " -> " trace))
      | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
          walk active trace replacement
      | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
      | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
      | Types.Tof_kind _ ->
          invalid_rank source_file local.declaration.typ_loc
            (Printf.sprintf
               "rank profile rejects unsupported or higher-kinded dependency; \
                expansion trace: %s"
               (String.concat " -> " trace))
    in
    rank_declaration_expressions local
    |> iter_result (fun (step, expression) ->
        walk [] [ rank_type_label local; step ] expression)
  type rank_parameter_occurrence = {
    profile_occurrence_polarity : rank_polarity;
    profile_occurrence_trace : string list;
  }
  let rank_parameter_occurrences local_types local parameter_id =
    let rec walk polarity substitutions active trace typ =
      let typ = rank_substitute substitutions typ in
      if Types.get_id typ = parameter_id then
        [
          {
            profile_occurrence_polarity = polarity;
            profile_occurrence_trace = trace;
          };
        ]
      else
        match Types.get_desc typ with
        | Types.Tvar _ | Types.Tunivar _ -> []
        | Types.Tarrow (_, domain, codomain, _) ->
            walk
              (match polarity with
              | Positive -> Negative
              | Negative -> Positive)
              substitutions active
              (trace @ [ "arrow-domain" ])
              domain
            @ walk polarity substitutions active
                (trace @ [ "arrow-codomain" ])
                codomain
        | Types.Ttuple components ->
            components
            |> List.concat_map (fun (label, component) ->
                walk polarity substitutions active
                  (trace @ [ "tuple:" ^ Option.value ~default:"_" label ])
                  component)
        | Types.Tconstr (path, arguments, _) -> (
            let arguments =
              List.map (rank_substitute substitutions) arguments
            in
            match rank_local_by_path local_types path with
            | None ->
                arguments
                |> List.concat_map
                     (walk polarity substitutions active
                        (trace @ [ "external-application:" ^ Path.name path ]))
            | Some target ->
                let target_parameters = rank_parameter_ids target in
                let bindings =
                  if List.length target_parameters = List.length arguments then
                    List.map2
                      (fun (id, _, _, _) argument -> (id, argument))
                      target_parameters arguments
                  else []
                in
                let substitution_trace =
                  List.map2
                    (fun (_, index, _, _) argument ->
                      Printf.sprintf "substitute:parameter#%d=%s" index
                        (rank_render_type local_types [] argument))
                    target_parameters arguments
                in
                let key =
                  ( target.type_id.type_index,
                    List.map Types.get_id arguments,
                    polarity )
                in
                let trace =
                  trace
                  @ [ "apply:" ^ rank_type_label target ]
                  @ substitution_trace
                in
                if List.mem key active then []
                else
                  rank_declaration_expressions target
                  |> List.concat_map (fun (step, expression) ->
                      walk polarity (bindings @ substitutions) (key :: active)
                        (trace @ [ "expand:" ^ rank_type_label target; step ])
                        expression))
        | Types.Tpoly (body, variables) ->
            if variables = [] then walk polarity substitutions active trace body
            else []
        | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
            walk polarity substitutions active trace replacement
        | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
        | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
        | Types.Tof_kind _ ->
            []
    in
    rank_declaration_expressions local
    |> List.concat_map (fun (step, expression) ->
        walk Positive [] [] [ rank_type_label local; step ] expression)
    |> List.sort_uniq compare
  let rank_construction_predicates local_types ~blocked_types
      ~forbidden_parameters =
    let rec construct_type substitutions active typ =
      let typ = rank_substitute substitutions typ in
      if List.mem (Types.get_id typ) forbidden_parameters then false
      else
        match Types.get_desc typ with
        | Types.Tvar _ | Types.Tunivar _ -> true
        | Types.Tarrow (_, _, codomain, _) ->
            construct_type substitutions active codomain
        | Types.Ttuple components ->
            List.for_all
              (fun (_, component) ->
                construct_type substitutions active component)
              components
        | Types.Tconstr (path, arguments, _) -> (
            let arguments =
              List.map (rank_substitute substitutions) arguments
            in
            match rank_local_by_path local_types path with
            | None -> rank_scalar_external path arguments
            | Some target ->
                if List.exists (rank_same_type target) blocked_types then false
                else
                  let key =
                    (target.type_id.type_index, List.map Types.get_id arguments)
                  in
                  if List.mem key active then false
                  else
                    let parameters = rank_parameter_ids target in
                    if List.length parameters <> List.length arguments then
                      false
                    else
                      let bindings =
                        List.map2
                          (fun (id, _, _, _) argument -> (id, argument))
                          parameters arguments
                      in
                      construct_local (bindings @ substitutions) (key :: active)
                        target)
        | Types.Tpoly (body, variables) ->
            variables = [] && construct_type substitutions active body
        | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
            construct_type substitutions active replacement
        | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
        | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
        | Types.Tof_kind _ ->
            false
    and construct_local substitutions active local =
      match local.declaration.typ_kind with
      | Ttype_abstract -> (
          match local.declaration.typ_manifest with
          | Some manifest ->
              construct_type substitutions active manifest.ctyp_type
          | None -> false)
      | Ttype_variant constructors ->
          List.exists
            (fun constructor ->
              let fields =
                match constructor.cd_args with
                | Cstr_tuple arguments ->
                    List.map
                      (fun argument -> argument.ca_type.ctyp_type)
                      arguments
                | Cstr_record labels ->
                    List.map (fun label -> label.ld_type.ctyp_type) labels
              in
              List.for_all (construct_type substitutions active) fields)
            constructors
      | Ttype_record _ | Ttype_record_unboxed_product _ | Ttype_open -> false
    in
    (construct_type [] [], construct_local [] [])
  let rank_can_construct local_types ~blocked_types ~forbidden_parameters root =
    let _, construct_local =
      rank_construction_predicates local_types ~blocked_types
        ~forbidden_parameters
    in
    construct_local root
  let rank_fields_constructible local_types ~blocked_types ~forbidden_parameters
      fields =
    let construct_type, _ =
      rank_construction_predicates local_types ~blocked_types
        ~forbidden_parameters
    in
    List.for_all construct_type fields
  let rank_ground_traces local_types ~blocked_types ~forbidden_parameters root =
    let rec trace_type substitutions active trace typ =
      let typ = rank_substitute substitutions typ in
      if List.mem (Types.get_id typ) forbidden_parameters then None
      else
        match Types.get_desc typ with
        | Types.Tvar _ | Types.Tunivar _ ->
            Some
              (trace @ [ Printf.sprintf "available-open#%d" (Types.get_id typ) ])
        | Types.Tarrow (_, _, codomain, _) ->
            trace_type substitutions active
              (trace @ [ "construct:arrow-codomain" ])
              codomain
        | Types.Ttuple components ->
            trace_fields substitutions active
              (trace @ [ "construct:tuple" ])
              (List.map snd components)
        | Types.Tconstr (path, arguments, _) -> (
            let arguments =
              List.map (rank_substitute substitutions) arguments
            in
            match rank_local_by_path local_types path with
            | None ->
                if rank_scalar_external path arguments then
                  Some (trace @ [ "scalar:" ^ Path.name path ])
                else None
            | Some target ->
                if List.exists (rank_same_type target) blocked_types then None
                else
                  let parameters = rank_parameter_ids target in
                  if List.length parameters <> List.length arguments then None
                  else
                    let key =
                      ( target.type_id.type_index,
                        List.map Types.get_id arguments )
                    in
                    if List.mem key active then None
                    else
                      let bindings =
                        List.map2
                          (fun (id, _, _, _) argument -> (id, argument))
                          parameters arguments
                      in
                      let substitutions_trace =
                        List.map2
                          (fun (_, index, _, _) argument ->
                            Printf.sprintf "substitute:parameter#%d=%s" index
                              (rank_actual_type_label local_types argument))
                          parameters arguments
                      in
                      trace_local (bindings @ substitutions) (key :: active)
                        (trace
                        @ [ "apply:" ^ rank_type_label target ]
                        @ substitutions_trace
                        @ [ "expand:" ^ rank_type_label target ])
                        target)
        | Types.Tpoly (body, variables) ->
            if variables = [] then trace_type substitutions active trace body
            else None
        | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
            trace_type substitutions active trace replacement
        | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
        | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
        | Types.Tof_kind _ ->
            None
    and trace_fields substitutions active trace = function
      | [] -> Some trace
      | field :: rest -> (
          match trace_type substitutions active trace field with
          | None -> None
          | Some trace -> trace_fields substitutions active trace rest)
    and trace_constructor substitutions active trace constructor =
      let fields =
        match constructor.cd_args with
        | Cstr_tuple arguments ->
            List.map (fun argument -> argument.ca_type.ctyp_type) arguments
        | Cstr_record labels ->
            List.map (fun label -> label.ld_type.ctyp_type) labels
      in
      trace_fields substitutions active
        (trace
        @ [
            Printf.sprintf "constructor:%s[uid=%s]" constructor.cd_name.txt
              (rank_uid_to_string constructor.cd_uid);
          ])
        fields
    and trace_local substitutions active trace local =
      match local.declaration.typ_kind with
      | Ttype_abstract -> (
          match local.declaration.typ_manifest with
          | Some manifest ->
              trace_type substitutions active (trace @ [ "manifest" ])
                manifest.ctyp_type
          | None -> None)
      | Ttype_variant constructors ->
          constructors
          |> List.find_map (trace_constructor substitutions active trace)
      | Ttype_record _ | Ttype_record_unboxed_product _ | Ttype_open -> None
    in
    match root.declaration.typ_kind with
    | Ttype_variant constructors ->
        constructors
        |> List.filter_map (trace_constructor [] [] [ rank_type_label root ])
    | Ttype_abstract -> (
        match trace_local [] [] [ rank_type_label root ] root with
        | Some trace -> [ trace ]
        | None -> [])
    | Ttype_record _ | Ttype_record_unboxed_product _ | Ttype_open -> []
  let collect_rank_profile_dependencies source_file local_types local =
    rank_direct_dependencies local_types local
    |> List.filter_map (fun type_id ->
        List.find_opt (fun candidate -> candidate.type_id = type_id) local_types)
    |> List.map (rank_identity source_file)
    |> List.sort_uniq compare
  let collect_rank_profile_applications local_types profile_ids source_file
      local =
    let owner = rank_identity source_file local in
    let rec walk substitutions active trace typ =
      let typ = rank_substitute substitutions typ in
      match Types.get_desc typ with
      | Types.Tvar _ | Types.Tunivar _ -> []
      | Types.Tarrow (_, domain, codomain, _) ->
          walk substitutions active (trace @ [ "arrow-domain" ]) domain
          @ walk substitutions active (trace @ [ "arrow-codomain" ]) codomain
      | Types.Ttuple components ->
          components
          |> List.concat_map (fun (label, component) ->
              walk substitutions active
                (trace @ [ "tuple:" ^ Option.value ~default:"_" label ])
                component)
      | Types.Tconstr (path, arguments, _) -> (
          let arguments = List.map (rank_substitute substitutions) arguments in
          match rank_local_by_path local_types path with
          | None -> []
          | Some target ->
              let target_parameters = rank_parameter_ids target in
              let substitution =
                List.map2
                  (fun (_, index, _, _) argument ->
                    (index, rank_render_type local_types [] argument))
                  target_parameters arguments
              in
              let substitution_trace =
                List.map
                  (fun (index, actual) ->
                    Printf.sprintf "substitute:parameter#%d=%s" index actual)
                  substitution
              in
              let application_trace =
                trace
                @ [ "apply:" ^ rank_type_label target ]
                @ substitution_trace
              in
              let application =
                {
                  rank_application_owner = owner;
                  rank_application_target = rank_identity source_file target;
                  rank_application_target_profile =
                    Option.value ~default:"<unsealed>"
                      (List.assoc_opt target.type_id profile_ids);
                  rank_application_substitution = substitution;
                  rank_application_trace = application_trace;
                }
              in
              let key =
                (target.type_id.type_index, List.map Types.get_id arguments)
              in
              if List.mem key active then [ application ]
              else
                let bindings =
                  List.map2
                    (fun (id, _, _, _) argument -> (id, argument))
                    target_parameters arguments
                in
                application
                :: (rank_declaration_expressions target
                   |> List.concat_map (fun (step, expression) ->
                       walk (bindings @ substitutions) (key :: active)
                         (application_trace
                         @ [ "expand:" ^ rank_type_label target; step ])
                         expression)))
      | Types.Tpoly (body, variables) ->
          if variables = [] then walk substitutions active trace body else []
      | Types.Tlink replacement | Types.Tsubst (replacement, _) ->
          walk substitutions active trace replacement
      | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
      | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
      | Types.Tof_kind _ ->
          []
    in
    rank_declaration_expressions local
    |> List.concat_map (fun (step, expression) ->
        walk [] [] [ rank_type_label local; step ] expression)
    |> List.sort_uniq compare
  let canonical_rank_profile local_types profile applications =
    let identity = profile.pending_profile_identity in
    let parameter fact =
      let classification =
        match fact.rank_parameter_classification with
        | Prohibited_negative_use -> "prohibited-negative"
        | Recursive_dependent_grounding -> "recursive-dependent"
        | Independently_grounded_construction -> "independent"
      in
      Printf.sprintf "%d|%s|%s|%s|%s" fact.rank_parameter_index
        fact.rank_parameter_identity classification fact.rank_parameter_variance
        (fact.rank_parameter_occurrence_traces
        |> List.map (String.concat ">")
        |> String.concat ",")
    in
    let application fact =
      Printf.sprintf "%s|%s|%s|%s|%s" fact.rank_application_target.rank_path
        fact.rank_application_target.rank_uid
        fact.rank_application_target_profile
        (fact.rank_application_substitution
        |> List.map (fun (index, actual) -> Printf.sprintf "%d=%s" index actual)
        |> String.concat ",")
        (String.concat ">" fact.rank_application_trace)
    in
    String.concat "\n"
      [
        "generic-rank-profile-v1";
        Printf.sprintf "%s#%d|%s|%s" identity.rank_type_id.type_name
          identity.rank_type_id.type_index identity.rank_path identity.rank_uid;
        String.concat ";"
          (List.map parameter profile.pending_profile_parameters);
        String.concat ";" (List.map application applications);
        String.concat ";"
          (List.map
             (fun dependency ->
               dependency.rank_path ^ "|" ^ dependency.rank_uid)
             profile.pending_profile_dependencies);
        String.concat ";"
          (List.map (String.concat ">") profile.pending_profile_ground_traces);
        string_of_bool profile.pending_profile_independently_grounded;
        rank_profile_declaration_snapshot local_types
          profile.pending_profile_local;
      ]
  let analyze_rank_profiles ?(allow_parameter_ground = false) source_file
      structure local_types =
    let dependencies =
      List.map
        (fun local ->
          (local.type_id, rank_direct_dependencies local_types local))
        local_types
    in
    let recursive_components =
      local_types
      |> List.filter_map (fun root ->
          let component =
            rank_recursive_component dependencies local_types root
          in
          if rank_is_recursive dependencies component then Some component
          else None)
      |> List.sort_uniq (fun left right ->
          compare
            (List.map (fun local -> local.type_id.type_index) left)
            (List.map (fun local -> local.type_id.type_index) right))
    in
    let generic_roots =
      List.filter (fun local -> local.declaration.typ_params <> []) local_types
    in
    let recursive_generic_roots =
      recursive_components |> List.concat
      |> List.filter (fun local ->
          local.declaration.typ_params <> []
          || List.exists
               (fun dependency -> dependency.declaration.typ_params <> [])
               (rank_profile_dependency_closure dependencies local_types
                  [ local ]))
    in
    let roots =
      List.sort_uniq
        (fun left right ->
          Int.compare left.type_id.type_index right.type_id.type_index)
        (generic_roots @ recursive_generic_roots)
    in
    let profiled =
      rank_profile_dependency_closure dependencies local_types roots
    in
    let* () = profiled |> iter_result (rank_profile_shape source_file) in
    let* () =
      profiled
      |> iter_result
           (rank_validate_profile_expressions source_file local_types profiled)
    in
    let mutually_recursive_generic =
      recursive_components
      |> List.find_opt (fun component ->
          List.length component > 1
          && List.exists
               (fun local -> local.declaration.typ_params <> [])
               component)
    in
    let* () =
      match mutually_recursive_generic with
      | None -> Ok ()
      | Some component ->
          let local = List.hd component in
          invalid_rank source_file local.declaration.typ_loc
            (Printf.sprintf
               "rank profile rejects mutually recursive generic dependencies: \
                %s"
               (String.concat ", " (List.map rank_type_label component)))
    in
    let* () =
      recursive_components
      |> List.filter (fun component ->
          List.exists
            (fun local -> List.exists (rank_same_type local) profiled)
            component)
      |> iter_result (fun component ->
          let negative =
            component
            |> List.find_map (fun local ->
                rank_declaration_expressions local
                |> List.concat_map (fun (step, expression) ->
                    rank_occurrences ~trace_substitutions:true local_types local
                      [ rank_type_label local; step ]
                      expression)
                |> List.find_opt (fun occurrence ->
                    occurrence.occurrence_polarity = Negative
                    && List.exists
                         (rank_same_type occurrence.occurrence_target)
                         component)
                |> Option.map (fun occurrence -> (local, occurrence)))
          in
          match negative with
          | Some (local, occurrence) ->
              invalid_rank source_file local.declaration.typ_loc
                (Printf.sprintf
                   "generic rank application uses a prohibited negative \
                    parameter in %s; expansion trace: %s"
                   (rank_type_label local)
                   (String.concat " -> " occurrence.occurrence_trace))
          | None ->
              let ground =
                component
                |> List.exists (fun local ->
                    match local.declaration.typ_kind with
                    | Ttype_variant constructors ->
                        List.exists
                          (fun constructor ->
                            let fields =
                              match constructor.cd_args with
                              | Cstr_tuple arguments ->
                                  List.map
                                    (fun argument -> argument.ca_type.ctyp_type)
                                    arguments
                              | Cstr_record labels ->
                                  List.map
                                    (fun label -> label.ld_type.ctyp_type)
                                    labels
                            in
                            rank_fields_constructible local_types
                              ~blocked_types:component
                              ~forbidden_parameters:
                                (if allow_parameter_ground then []
                                 else
                                   rank_parameter_ids local
                                   |> List.map (fun (id, _, _, _) -> id))
                              fields)
                          constructors
                    | Ttype_abstract | Ttype_record _
                    | Ttype_record_unboxed_product _ | Ttype_open ->
                        false)
              in
              if ground then Ok ()
              else
                let local = List.hd component in
                invalid_rank source_file local.declaration.typ_loc
                  (Printf.sprintf
                     "generic rank grounding fails for open parameters or \
                      all-recursive construction paths: %s"
                     (String.concat ", " (List.map rank_type_label component))))
    in
    let pending =
      profiled
      |> List.map (fun local ->
          let parameter_facts =
            rank_parameter_ids local
            |> List.map (fun (id, index, identity, variance) ->
                let occurrences =
                  rank_parameter_occurrences local_types local id
                in
                let negative =
                  List.exists
                    (fun occurrence ->
                      occurrence.profile_occurrence_polarity = Negative)
                    occurrences
                in
                let independent =
                  rank_can_construct local_types ~blocked_types:[]
                    ~forbidden_parameters:[ id ] local
                in
                let classification =
                  if negative then Prohibited_negative_use
                  else if independent then Independently_grounded_construction
                  else Recursive_dependent_grounding
                in
                let _, compiler_negative, _ =
                  Types.Variance.get_lower variance
                in
                ( classification,
                  compiler_negative,
                  {
                    rank_parameter_index = index;
                    rank_parameter_identity = identity;
                    rank_parameter_classification = classification;
                    rank_parameter_variance = rank_variance_label variance;
                    rank_parameter_occurrence_traces =
                      List.map
                        (fun occurrence -> occurrence.profile_occurrence_trace)
                        occurrences;
                  } ))
          in
          let inconsistent =
            List.find_opt
              (fun (classification, compiler_negative, _) ->
                match classification with
                | Prohibited_negative_use -> not compiler_negative
                | Recursive_dependent_grounding
                | Independently_grounded_construction ->
                    compiler_negative)
              parameter_facts
          in
          ( local,
            inconsistent,
            List.map (fun (_, _, fact) -> fact) parameter_facts ))
    in
    let* () =
      pending
      |> iter_result (fun (local, inconsistent, _) ->
          match inconsistent with
          | None -> Ok ()
          | Some _ ->
              invalid_rank source_file local.declaration.typ_loc
                (Printf.sprintf
                   "compiler variance is inconsistent with sealed rank \
                    polarity for %s"
                   (rank_type_label local)))
    in
    let pending =
      pending
      |> List.map (fun (local, _, parameters) ->
          let profile_dependencies =
            collect_rank_profile_dependencies source_file local_types local
          in
          let independently_grounded =
            rank_can_construct local_types ~blocked_types:[]
              ~forbidden_parameters:
                (rank_parameter_ids local |> List.map (fun (id, _, _, _) -> id))
              local
            && List.for_all
                 (fun parameter ->
                   parameter.rank_parameter_classification
                   = Independently_grounded_construction)
                 parameters
          in
          let ground_traces =
            if independently_grounded then
              let component =
                rank_recursive_component dependencies local_types local
              in
              let blocked_types =
                if rank_is_recursive dependencies component then component
                else []
              in
              rank_ground_traces local_types ~blocked_types
                ~forbidden_parameters:
                  (rank_parameter_ids local
                  |> List.map (fun (id, _, _, _) -> id))
                local
            else []
          in
          let base =
            let parameter fact =
              Printf.sprintf "%d:%s:%s:%s:%s" fact.rank_parameter_index
                fact.rank_parameter_identity
                (match fact.rank_parameter_classification with
                | Prohibited_negative_use -> "negative"
                | Recursive_dependent_grounding -> "dependent"
                | Independently_grounded_construction -> "independent")
                fact.rank_parameter_variance
                (fact.rank_parameter_occurrence_traces
                |> List.map (String.concat ">")
                |> String.concat ",")
            in
            String.concat "\n"
              [
                "generic-rank-profile-base-v1";
                rank_profile_declaration_snapshot local_types local;
                String.concat ";" (List.map parameter parameters);
                String.concat ";"
                  (List.map
                     (fun dependency ->
                       dependency.rank_path ^ "|" ^ dependency.rank_uid)
                     profile_dependencies);
                String.concat ";" (List.map (String.concat ">") ground_traces);
                string_of_bool independently_grounded;
              ]
          in
          {
            pending_profile_local = local;
            pending_profile_identity = rank_identity source_file local;
            pending_profile_parameters = parameters;
            pending_profile_dependencies = profile_dependencies;
            pending_profile_ground_traces = ground_traces;
            pending_profile_independently_grounded = independently_grounded;
            pending_profile_base_digest = Digest.to_hex (Digest.string base);
          })
    in
    let profile_ids =
      List.map
        (fun profile ->
          ( profile.pending_profile_local.type_id,
            pending_rank_profile_id profile ))
        pending
    in
    let profiles_with_applications =
      List.map
        (fun profile ->
          ( profile,
            collect_rank_profile_applications local_types profile_ids
              source_file profile.pending_profile_local ))
        pending
    in
    let* () =
      profiles_with_applications
      |> iter_result (fun (profile, applications) ->
          applications
          |> iter_result (fun application ->
              match
                List.find_opt
                  (fun candidate ->
                    candidate.pending_profile_identity
                    = application.rank_application_target)
                  pending
              with
              | Some target
                when String.equal application.rank_application_target_profile
                       (pending_rank_profile_id target) ->
                  Ok ()
              | Some _ | None ->
                  invalid_rank source_file
                    profile.pending_profile_local.declaration.typ_loc
                    (Printf.sprintf
                       "rank application does not resolve to the sealed target \
                        profile; expansion trace: %s"
                       (String.concat " -> " application.rank_application_trace))))
    in
    let snapshot () =
      profiles_with_applications
      |> List.map (fun (profile, applications) ->
          canonical_rank_profile local_types profile applications)
      |> String.concat "\n--profile--\n"
    in
    let snapshot_digest = Digest.to_hex (Digest.string (snapshot ())) in
    let issued =
      List.map
        (fun (profile, applications) ->
          let identity = profile.pending_profile_identity in
          {
            rank_profile_token = ref ();
            rank_profile_structure = structure;
            rank_profile_structure_snapshot = snapshot;
            rank_profile_id = pending_rank_profile_id profile;
            rank_profile_snapshot_digest = snapshot_digest;
            rank_profile_identity = identity;
            rank_profile_parameters = profile.pending_profile_parameters;
            rank_profile_applications = applications;
            rank_profile_dependencies = profile.pending_profile_dependencies;
            rank_profile_ground_traces = profile.pending_profile_ground_traces;
            rank_profile_independently_grounded =
              profile.pending_profile_independently_grounded;
          })
        profiles_with_applications
    in
    Parametric_rank_domain_private.register_profiles ~structure issued;
    Ok issued
  let certify_rank_profiles ~source_file ~imports structure =
    let* scan =
      Broadcast.authenticate_typedtree ~source_file ~imports
        ~artifact:None structure
    in
    match
      collect_structure source_file imports false scan
        (Typedtree_symbolic_private.empty ~source_file)
        structure
    with
    | Error _ as error -> error
    | Ok (local_types, functions, constrained_modules) ->
        let constrained_type_ids =
          constrained_modules
          |> List.concat_map (fun candidate ->
              List.map
                (fun (_, local) -> local.type_id)
                candidate.public_type_links)
        in
        let local_types =
          List.filter
            (fun local ->
              (not (List.mem local.type_id constrained_type_ids))
              && not
                   (List.exists
                      (fun candidate ->
                        String.starts_with
                          ~prefix:(candidate.module_name ^ ".")
                          local.type_id.type_name
                        && (constrained_module_has_private_opaque_recursive_spec
                              candidate functions
                           || (not
                                 (constrained_module_has_finite_signature
                                    candidate))
                           || List.exists
                                (fun (_, field) -> field.source_field_mutable)
                                (rank_source_fields local)))
                      constrained_modules))
            local_types
        in
        analyze_rank_profiles source_file structure local_types
  let lower_type_registry source_file imports env ~imported_parametric_adts
      ~load_path_visible ~load_path_hidden ~proof_capture_artifact local_types =
    let base_context =
      {
        source_file;
        typing_environment = env;
        proof_capture_artifact = None;
        broadcast_scan = None;
        symbolic_scan = None;
        symbolic_definitions = [];
        imports;
        functions = [];
        aggregates = { empty_aggregates with local_types };
        current_function = None;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported = empty_imported_environment;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let normalize_field_modalities location modalities =
      let apply modes =
        Mode.Modality.Const.apply modalities (Mode.Value.of_const modes)
        |> Mode.Value.to_const_exn
      in
      let minimum = Mode.Value.Const.min and maximum = Mode.Value.Const.max in
      let applied_minimum = apply minimum and applied_maximum = apply maximum in
      let uniqueness_modality =
        match
          ( applied_minimum.uniqueness,
            applied_maximum.uniqueness,
            minimum.uniqueness,
            maximum.uniqueness )
        with
        | actual_minimum, actual_maximum, expected_minimum, expected_maximum
          when Mode.Uniqueness.Const.equal actual_minimum expected_minimum
               && Mode.Uniqueness.Const.equal actual_maximum expected_maximum ->
            Ok Sst.Preserve_uniqueness
        | Mode.Uniqueness.Const.Unique, Unique, _, _ -> Ok Sst.Force_unique
        | Mode.Uniqueness.Const.Aliased, Aliased, _, _ -> Ok Sst.Force_aliased
        | _ -> unsupported base_context location Diagnostic.Unsupported_type
      in
      let linearity_modality =
        match
          ( applied_minimum.linearity,
            applied_maximum.linearity,
            minimum.linearity,
            maximum.linearity )
        with
        | actual_minimum, actual_maximum, expected_minimum, expected_maximum
          when Mode.Linearity.Const.equal actual_minimum expected_minimum
               && Mode.Linearity.Const.equal actual_maximum expected_maximum ->
            Ok Sst.Preserve_linearity
        | Mode.Linearity.Const.Once, Once, _, _ -> Ok Sst.Force_once
        | Mode.Linearity.Const.Many, Many, _, _ -> Ok Sst.Force_many
        | _ -> unsupported base_context location Diagnostic.Unsupported_type
      in
      let* uniqueness_modality = uniqueness_modality in
      let* linearity_modality = linearity_modality in
      Ok { Sst.uniqueness_modality; linearity_modality }
    in
    let* external_types, ordinary_types =
      let rec classify specifications ordinary = function
        | [] -> Ok (List.rev specifications, List.rev ordinary)
        | local :: rest -> (
            match external_type_specification local.declaration with
            | Ordinary_type ->
                classify specifications (local :: ordinary) rest
            | Authenticated_external_type -> (
                match proof_capture_artifact with
                | Some artifact
                  when Typedtree_adapter_issuance_private
                       .authenticate_logical_builtin_artifact artifact
                         ~source_file ->
                    classify (local :: specifications) ordinary rest
                | Some _ | None ->
                    unsupported base_context local.declaration.typ_loc
                      Diagnostic.Malformed_ghost_call)
            | Malformed_external_type ->
                unsupported base_context local.declaration.typ_loc
                  Diagnostic.Malformed_ghost_call)
      in
      classify [] [] local_types
    in
    let* external_sources =
      let rec lower sources = function
        | [] -> Ok (List.rev sources)
        | (local : local_type) :: rest -> (
            match
              Parametric_adt_lowering_private.external_source
                ~paths:local.resolved_paths ~type_id:local.type_id
                ~load_path_visible ~load_path_hidden env local.declaration
            with
            | Ok source -> lower (source :: sources) rest
            | Error message ->
                let diagnostic =
                  Diagnostic.make
                    (Diagnostic.Unsupported_construct Diagnostic.Aggregate)
                    (span base_context local.declaration.typ_loc)
                in
                Error { diagnostic with Diagnostic.message })
      in
      lower [] external_types
    in
    let parametric_sources =
      external_sources
      @ List.filter_map
          (fun (local : local_type) ->
            if local.declaration.typ_params = [] then None
            else
              Some
                (Parametric_adt_lowering_private.local_source
                   ~paths:local.resolved_paths ~type_id:local.type_id
                   local.declaration))
          ordinary_types
    in
    let aggregate path =
      match find_local_type_by_path path ordinary_types with
      | Some local when local.declaration.typ_params = [] -> Some local.type_id
      | Some _ | None -> None
    in
    let* parametric_adts =
      Parametric_adt_lowering_private.lower ~span:(span base_context) ~aggregate
        ~modalities:normalize_field_modalities parametric_sources
    in
    let local_external_specifications =
      parametric_adts
      |> List.filter_map (fun item ->
             let descriptor =
               item.Parametric_adt_lowering_private.descriptor
             in
             match Parametric_adt.provenance descriptor with
             | Parametric_adt.External _ -> Some descriptor
             | Parametric_adt.Local _ -> None)
    in
    let* () =
      match
        List.find_map
          (fun local ->
            let local_constructor = Parametric_adt.type_constructor local in
            imported_parametric_adts
            |> List.find_opt (fun imported ->
                   let imported_constructor =
                     Parametric_adt.type_constructor imported
                   in
                   String.equal (Parametric_adt.compiler_uid local)
                     (Parametric_adt.compiler_uid imported)
                   || String.equal local_constructor.constructor_path
                        imported_constructor.constructor_path)
            |> Option.map (fun imported ->
                   ( local_constructor.constructor_path,
                     (Parametric_adt.type_constructor imported).constructor_path
                   )))
          local_external_specifications
      with
      | None -> Ok ()
      | Some (local_path, imported_path) ->
          Error
            (Diagnostic.make
               (Diagnostic.Invalid_semantic_program
                  {
                    function_name = None;
                    detail =
                      Printf.sprintf
                        "local and imported external type specifications overlap for %s / %s"
                        local_path imported_path;
                  })
               (Diagnostic.file_span source_file))
    in
    let base_context =
      {
        base_context with
        aggregates = { base_context.aggregates with parametric_adts };
      }
    in
    let lower_field substitutions owner field_index name uid field_mutability
        modalities core_type location =
      let* field_type =
        normalized_type_with_substitutions base_context substitutions location
          core_type.ctyp_type
      in
      let* field_modalities = normalize_field_modalities location modalities in
      let field_id =
        { Sst.field_owner = owner; field_index; field_name = name }
      in
      Ok
        ( {
            Sst.field_id;
            field_type;
            field_mutability;
            field_modalities;
            span = span base_context location;
          },
          Option.map (fun uid -> (uid, type_id_of_field field_id, field_id)) uid
        )
    in
    let lower_labels substitutions owner labels =
      let rec loop index lowered registered = function
        | [] -> Ok (List.rev lowered, List.rev registered)
        | label :: rest ->
            let* field, registration =
              lower_field substitutions owner index label.ld_name.txt
                (Some label.ld_uid)
                (match label.ld_mutable with
                | Types.Immutable -> Sst.Immutable_field
                | Types.Mutable _ -> Sst.Mutable_field)
                label.ld_modalities label.ld_type label.ld_loc
            in
            loop (index + 1) (field :: lowered)
              (Option.fold ~none:registered
                 ~some:(fun registration -> registration :: registered)
                 registration)
              rest
      in
      loop 0 [] [] labels
    in
    let rec lower_types definitions fields constructors = function
      | [] ->
          Ok
            {
              local_types;
              parametric_adts;
              ranked_types = [];
              definitions =
                List.rev definitions
                @ List.map
                    (fun item ->
                      item.Parametric_adt_lowering_private.definition)
                    parametric_adts;
              fields =
                List.rev fields
                @ List.concat_map
                    (fun item -> item.Parametric_adt_lowering_private.fields)
                    parametric_adts;
              constructors =
                List.rev constructors
                @ List.concat_map
                    (fun item ->
                      item.Parametric_adt_lowering_private.constructors)
                    parametric_adts;
            }
      | (local, type_id, arguments) :: rest -> (
          let declaration = local.declaration in
          let parameters =
            List.map
              (fun (parameter, _) -> Types.get_id parameter.ctyp_type)
              declaration.typ_params
          in
          let substitutions =
            if List.length parameters = List.length arguments then
              List.combine parameters arguments
            else []
          in
          if
            declaration.typ_cstrs <> []
            || List.length parameters <> List.length arguments
          then
            unsupported base_context declaration.typ_loc
              Diagnostic.Unsupported_generic_use
          else if declaration.typ_private <> Asttypes.Public then
            unsupported base_context declaration.typ_loc Diagnostic.Aggregate
          else if declaration.typ_manifest <> None then
            unsupported base_context declaration.typ_loc
              Diagnostic.Unsupported_type
          else
            match declaration.typ_kind with
            | Ttype_record labels ->
                let* lowered_fields, registrations =
                  lower_labels substitutions (Sst.Record_owner type_id) labels
                in
                let definition =
                  {
                    Sst.type_id;
                    type_kind = Sst.Record_definition lowered_fields;
                    representation = Sst.Revealed;
                    span = span base_context declaration.typ_loc;
                  }
                in
                lower_types
                  (definition :: definitions)
                  (List.rev_append registrations fields)
                  constructors rest
            | Ttype_variant declarations ->
                let* () =
                  match declaration.typ_type.type_kind with
                  | Types.Type_variant (_, Types.Variant_boxed _, _) -> Ok ()
                  | Types.Type_variant
                      ( _,
                        ( Types.Variant_unboxed | Types.Variant_extensible
                        | Types.Variant_with_null ),
                        _ )
                  | Types.Type_abstract _ | Types.Type_record _
                  | Types.Type_record_unboxed_product _ | Types.Type_open ->
                      unsupported base_context declaration.typ_loc
                        Diagnostic.Aggregate
                in
                let rec lower_constructors index lowered field_regs
                    constructor_regs = function
                  | [] ->
                      Ok
                        ( List.rev lowered,
                          List.rev field_regs,
                          List.rev constructor_regs )
                  | constructor :: remaining ->
                      if constructor.cd_vars <> [] || constructor.cd_res <> None
                      then
                        unsupported base_context constructor.cd_loc
                          Diagnostic.Unsupported_generic_use
                      else
                        let constructor_id =
                          {
                            Sst.constructor_type = type_id;
                            constructor_index = index;
                            constructor_name = constructor.cd_name.txt;
                          }
                        in
                        let owner = Sst.Constructor_owner constructor_id in
                        let* constructor_fields, registrations =
                          match constructor.cd_args with
                          | Cstr_record labels ->
                              lower_labels substitutions owner labels
                          | Cstr_tuple arguments ->
                              let rec loop field_index lowered = function
                                | [] -> Ok (List.rev lowered, [])
                                | argument :: arguments ->
                                    let* field, _ =
                                      lower_field substitutions owner
                                        field_index
                                        (Printf.sprintf "$%d" field_index)
                                        None Sst.Immutable_field
                                        Mode.Modality.Const.id argument.ca_type
                                        argument.ca_loc
                                    in
                                    loop (field_index + 1) (field :: lowered)
                                      arguments
                              in
                              loop 0 [] arguments
                        in
                        let lowered_constructor =
                          {
                            Sst.constructor_id;
                            constructor_fields;
                            span = span base_context constructor.cd_loc;
                          }
                        in
                        lower_constructors (index + 1)
                          (lowered_constructor :: lowered)
                          (List.rev_append registrations field_regs)
                          ((constructor.cd_uid, type_id, constructor_id)
                          :: constructor_regs)
                          remaining
                in
                let* lowered, new_fields, new_constructors =
                  lower_constructors 0 [] [] [] declarations
                in
                let definition =
                  {
                    Sst.type_id;
                    type_kind = Sst.Variant_definition lowered;
                    representation = Sst.Revealed;
                    span = span base_context declaration.typ_loc;
                  }
                in
                lower_types
                  (definition :: definitions)
                  (List.rev_append new_fields fields)
                  (List.rev_append new_constructors constructors)
                  rest
            | Ttype_abstract | Ttype_open | Ttype_record_unboxed_product _ ->
                unsupported base_context declaration.typ_loc
                  Diagnostic.Aggregate)
    in
    let targets =
      List.filter_map
        (fun local ->
          if local.declaration.typ_params = [] then
            Some (local, local.type_id, [])
          else None)
        ordinary_types
    in
    lower_types [] [] [] targets
  let rec terminal_returns_unique_parameter function_id parameter_index
      parameter (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Variable { binding; use_uniqueness = Sst.Definitely_unique }
      when binding.id = parameter.Sst.id
           && binding.uniqueness = Sst.Definitely_unique ->
        true
    | Sst.Direct_call { callee; arguments; recursive = true; _ }
      when callee = function_id -> (
        match List.nth_opt arguments parameter_index with
        | Some
            (Sst.Value_argument
               {
                 value =
                   {
                     Sst.expression_desc =
                       Sst.Variable
                         { binding; use_uniqueness = Sst.Definitely_unique };
                     _;
                   };
                 _;
               }) ->
            binding.id = parameter.id
            && binding.uniqueness = Sst.Definitely_unique
        | None | Some _ -> false)
    | Sst.Sequence (_, body) | Sst.Let (_, body) | Sst.Let_mutable (_, _, body)
      ->
        terminal_returns_unique_parameter function_id parameter_index parameter
          body
    | Sst.If (_, consequent, Some alternative) ->
        terminal_returns_unique_parameter function_id parameter_index parameter
          consequent
        && terminal_returns_unique_parameter function_id parameter_index
             parameter alternative
    | Sst.Match (_, cases) ->
        cases <> []
        && List.for_all
             (fun case ->
               terminal_returns_unique_parameter function_id parameter_index
                 parameter case.Sst.case_body)
             cases
    | _ -> false
  let returned_unique_parameter function_id parameters body =
    let rec find index = function
      | [] -> None
      | Sst.Callback_parameter _ :: rest -> find (index + 1) rest
      | Sst.Value_parameter parameter :: rest -> (
          match parameter.Sst.pattern.pattern_desc with
          | Sst.Bind binding
            when binding.uniqueness = Sst.Definitely_unique
                 && terminal_returns_unique_parameter function_id index binding
                      body ->
              Some index
          | _ -> find (index + 1) rest)
    in
    find 0 parameters
  type external_target =
    | Same_unit_external_target of top_function
    | Imported_unverified_external_target of
        External_target_specification_private.candidate * Location.t
  type external_linkage = {
    wrapper : top_function;
    target : external_target;
    carrier : authenticated_spec_carrier;
  }
  let rec terminal_external_expression expression =
    match expression.exp_desc with
    | Texp_sequence (_, _, tail) -> terminal_external_expression tail
    | _ -> expression
  let resolve_external_linkages source_file imports imported functions =
    let context =
      {
        source_file;
        typing_environment = functions_environment functions;
        proof_capture_artifact = None;
        broadcast_scan = None;
        symbolic_scan = None;
        symbolic_definitions = [];
        imports;
        functions;
        aggregates = empty_aggregates;
        current_function = None;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let rec loop linked_targets linkages = function
      | [] -> Ok (List.rev linkages)
      | wrapper :: rest -> (
          match wrapper.function_kind with
          | Top_external_specification carrier ->
              let tail = terminal_external_expression carrier.definition_body in
              let* target, target_key =
                match tail.exp_desc with
                | Texp_apply
                    ( {
                        exp_desc =
                          Texp_ident (raw_path, source_name, description, _, _);
                        exp_env;
                        _;
                      },
                      _,
                      _,
                      _,
                      _ ) -> (
                    let path =
                      try
                        Env.normalize_value_path (Some tail.exp_loc) exp_env
                          raw_path
                      with Env.Error _ -> raw_path
                    in
                    match raw_path with
                    | Path.Pident _ -> (
                        match find_function_by_path raw_path functions with
                        | Some target ->
                            Ok
                              ( Same_unit_external_target target,
                                "local:"
                                ^ string_of_int
                                    target.function_id.function_index )
                        | None ->
                            unsupported context tail.exp_loc
                              Diagnostic.Malformed_ghost_call)
                    | Path.Pdot (Path.Pident _, _)
                      when match source_name.txt with
                           | Longident.Ldot
                               (Longident.Lident unit_name, value_name) ->
                               String.equal
                                 (unit_name ^ "." ^ value_name)
                                 (Path.name path)
                           | _ -> false -> (
                        match imported.external_target_specifications with
                        | None ->
                            unsupported context tail.exp_loc
                              Diagnostic.Malformed_ghost_call
                        | Some environment ->
                            let uid = compiler_uid description.Types.val_uid in
                            let* candidate =
                              External_target_specification_private
                              .resolve_candidate environment
                                ~canonical_path:(Path.name path) ~value_uid:uid
                              |> Result.map_error (fun message ->
                                  Diagnostic.make
                                    (Diagnostic.Unsupported_construct
                                       Diagnostic.Malformed_ghost_call)
                                    (span context tail.exp_loc)
                                  |> Diagnostic.with_message message)
                            in
                            let* () =
                              if
                                External_target_specification_private
                                .candidate_has_compatible_compiler_modes
                                  candidate
                                  wrapper.value_binding.vb_pat.pat_type
                              then Ok ()
                              else
                                unsupported context wrapper.value_binding.vb_loc
                                  Diagnostic.Malformed_ghost_call
                            in
                            Ok
                              ( Imported_unverified_external_target
                                  (candidate, tail.exp_loc),
                                "imported:" ^ Path.name path ^ "#" ^ uid ))
                    | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ ->
                        unsupported context tail.exp_loc
                          Diagnostic.Malformed_ghost_call)
                | _ ->
                    unsupported context tail.exp_loc
                      Diagnostic.Malformed_ghost_call
              in
              let* () =
                match target with
                | Imported_unverified_external_target _ -> Ok ()
                | Same_unit_external_target target -> (
                    if Ident.same wrapper.ident target.ident then
                      unsupported context tail.exp_loc
                        Diagnostic.Malformed_ghost_call
                    else if
                      target.rec_flag = Asttypes.Recursive
                      || target.function_id.function_index
                         >= wrapper.function_id.function_index
                    then
                      unsupported context target.value_binding.vb_loc
                        Diagnostic.Malformed_ghost_call
                    else
                      match target.function_kind with
                      | Top_exec -> Ok ()
                      | Top_spec _ | Top_type_invariant _ | Top_recursive_spec _
                      | Top_proof _ | Top_external_specification _
                      | Top_external_body _ ->
                          unsupported context target.value_binding.vb_loc
                            Diagnostic.Malformed_ghost_call)
              in
              if List.mem target_key linked_targets then
                unsupported context carrier.witness_location
                  Diagnostic.Malformed_ghost_call
              else
                loop
                  (target_key :: linked_targets)
                  ({ wrapper; target; carrier } :: linkages)
                  rest
          | Top_exec | Top_spec _ | Top_type_invariant _ | Top_recursive_spec _
          | Top_proof _ | Top_external_body _ ->
              loop linked_targets linkages rest)
    in
    loop [] [] functions
  let lower_parameter_list context parameters =
    let rec loop lowered bindings = function
      | [] -> Ok (List.rev lowered, bindings)
      | parameter :: rest -> (
          if parameter.fp_partial = Partial then
            unsupported context parameter.fp_loc
              Diagnostic.Partial_function_parameter
          else
            match parameter.fp_kind with
            | Tparam_optional_default _ ->
                unsupported context parameter.fp_loc
                  Diagnostic.Partial_function_parameter
            | Tparam_pat pattern ->
                let* lowered_pattern, bindings =
                  lower_pattern context bindings pattern
                in
                let lowered_pattern, bindings =
                  match
                    (pattern.pat_desc, lowered_pattern.Sst.pattern_desc)
                  with
                  | Tpat_var (ident, _, _, _, _), Sst.Bind binding ->
                      let binding =
                        {
                          binding with
                          Sst.uniqueness =
                            uniqueness_of_parameter_mode parameter.fp_mode;
                        }
                      in
                      ( { lowered_pattern with pattern_desc = Sst.Bind binding },
                        (ident, binding)
                        :: List.filter
                             (fun (candidate, _) ->
                               not (Ident.same ident candidate))
                             bindings )
                  | _ -> (lowered_pattern, bindings)
                in
                loop
                  (Sst.Value_parameter
                     {
                       Sst.label = parameter_label parameter.fp_arg_label;
                       pattern = lowered_pattern;
                       optional_default = None;
                     }
                  :: lowered)
                  bindings rest)
    in
    loop [] [] parameters
  let scalar_variable_parameter = function
    | Sst.Callback_parameter _ -> false
    | Sst.Value_parameter parameter -> (
        match parameter.pattern.pattern_desc with
        | Sst.Bind
            { typ = Sst.Int | Sst.Bool; uniqueness = Sst.Definitely_aliased; _ }
          ->
            true
        | _ -> false)
  let same_external_parameter_shape left right =
    match (left, right) with
    | Sst.Value_parameter left, Sst.Value_parameter right ->
        left.label = right.label
        && left.pattern.typ = right.pattern.typ
        && scalar_variable_parameter (Sst.Value_parameter left)
        && scalar_variable_parameter (Sst.Value_parameter right)
    | Sst.Callback_parameter _, _ | _, Sst.Callback_parameter _ -> false
  let function_expression_body context function_ =
    match function_.value_binding.vb_expr.exp_desc with
    | Texp_function { params; body = Tfunction_body body; ret_mode; _ } ->
        Ok (params, body, ret_mode)
    | Texp_function
        { body = Tfunction_cases { fc_partial = Partial; fc_loc; _ }; _ } ->
        unsupported context fc_loc Diagnostic.Partial_function_parameter
    | Texp_function _ ->
        unsupported context function_.value_binding.vb_loc
          Diagnostic.Higher_order_function
    | _ ->
        unsupported context function_.value_binding.vb_loc
          Diagnostic.Unsupported_top_level_binding
  let first_unknown_field_write context expression =
    let found = ref None in
    let default = Tast_iterator.default_iterator in
    let iterator =
      {
        default with
        expr =
          (fun self expression ->
            (match (expression.exp_desc, !found) with
            | Texp_setfield (_, _, _, description, _), None
              when find_field_by_uid description.Types.lbl_uid
                     context.aggregates.fields
                   = None ->
                found := Some expression.exp_loc
            | _ -> ());
            default.expr self expression);
      }
    in
    iterator.expr iterator expression;
    !found
  let rec lower_trusted_contract_prefix context bindings expression =
    match expression.exp_desc with
    | Texp_sequence
        (marker, _, { exp_desc = Texp_sequence (sidecar, _, tail); _ }) -> (
        match retained_ghost_pair context marker sidecar with
        | Some retained ->
            let* _ =
              lower_retained_ghost_clause context bindings marker retained
            in
            lower_trusted_contract_prefix context bindings tail
        | None -> (
            match ghost_call context "marker" marker with
            | Some _ ->
                unsupported context marker.exp_loc
                  Diagnostic.Malformed_ghost_call
            | None -> Ok expression))
    | _ -> Ok expression
  let lower_external_specification source_file imports aggregates functions
      proof_capture_artifact symbolic_scan symbolic_definitions linkage =
    let wrapper = linkage.wrapper in
    let target =
      match linkage.target with
      | Same_unit_external_target target -> target
      | Imported_unverified_external_target _ -> assert false
    in
    let context =
      {
        source_file;
        typing_environment = function_environment wrapper;
        proof_capture_artifact;
        broadcast_scan = None;
        symbolic_scan = Some symbolic_scan;
        symbolic_definitions;
        imports;
        functions;
        aggregates;
        current_function = Some wrapper;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported = empty_imported_environment;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    let* wrapper_parameters, _, wrapper_return_mode =
      function_expression_body context wrapper
    in
    let definition_body = linkage.carrier.definition_body in
    context.function_result_type <- Some definition_body.exp_type;
    let* parameters, bindings =
      lower_parameter_list context wrapper_parameters
    in
    let* lowered_body =
      lower_authenticated_expression context bindings definition_body
    in
    let* contracts, body =
      Callback_contract_private.extract context.contract_carriers lowered_body
    in
    let* () =
      if contracts.decreases <> [] || contracts.assertions <> [] then
        malformed definition_body.exp_loc
      else if
        parameters = []
        || not (List.for_all scalar_variable_parameter parameters)
      then malformed wrapper.value_binding.vb_loc
      else Ok ()
    in
    let* result_type =
      normalized_type context definition_body.exp_loc definition_body.exp_type
    in
    let* () =
      if result_type = Sst.Int || result_type = Sst.Bool then Ok ()
      else malformed definition_body.exp_loc
    in
    let* () =
      match body.expression_desc with
      | Sst.Direct_call
          {
            call_form = Sst.Unclassified_call;
            callee;
            arguments;
            recursive = false;
            _;
          }
        when callee = target.function_id
             && List.length arguments = List.length parameters ->
          List.fold_left2
            (fun result argument parameter ->
              let* () = result in
              match (argument, parameter) with
              | ( Sst.Value_argument { label = argument_label; value = argument },
                  Sst.Value_parameter parameter ) -> (
                  match
                    (parameter.pattern.pattern_desc, argument.expression_desc)
                  with
                  | ( Sst.Bind parameter_binding,
                      Sst.Variable
                        {
                          binding = argument_binding;
                          use_uniqueness = Sst.Definitely_aliased;
                        } )
                    when argument_label = parameter.label
                         && parameter_binding.id = argument_binding.id
                         && argument.typ = parameter.pattern.typ ->
                      Ok ()
                  | _ -> malformed definition_body.exp_loc)
              | Sst.Callback_argument _, _ | _, Sst.Callback_parameter _ ->
                  malformed definition_body.exp_loc)
            (Ok ()) arguments parameters
      | _ -> malformed definition_body.exp_loc
    in
    let target_context =
      {
        context with
        current_function = Some target;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported = empty_imported_environment;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let* target_parameter_nodes, target_body, target_return_mode =
      function_expression_body target_context target
    in
    let* target_parameters, _ =
      lower_parameter_list target_context target_parameter_nodes
    in
    let* target_result =
      normalized_type target_context target_body.exp_loc target_body.exp_type
    in
    let* () =
      if
        List.length parameters = List.length target_parameters
        && List.for_all2 same_external_parameter_shape parameters
             target_parameters
        && List.for_all2
             (fun wrapper_parameter target_parameter ->
               Mode.Alloc.Const.equal
                 (Mode.Alloc.zap_to_floor wrapper_parameter.fp_mode)
                 (Mode.Alloc.zap_to_floor target_parameter.fp_mode))
             wrapper_parameters target_parameter_nodes
        && Mode.Alloc.Const.equal
             (Mode.Alloc.zap_to_floor wrapper_return_mode)
             (Mode.Alloc.zap_to_floor target_return_mode)
        && result_type = target_result
      then Ok ()
      else malformed target.value_binding.vb_loc
    in
    let wrapper_span = span context wrapper.value_binding.vb_loc in
    let target_span = span context target.value_binding.vb_loc in
    let witness_span = span context linkage.carrier.witness_location in
    Ok
      (Sst_normalize.authenticated_external_specification
         ~wrapper_id:wrapper.function_id ~target_id:target.function_id
         ~parameters ~contracts ~result_type ~wrapper_span ~witness_span
         ~target_span)
  let lower_external_target_specification source_file imports aggregates
      functions proof_capture_artifact symbolic_scan symbolic_definitions
      imported linkage =
    let wrapper = linkage.wrapper in
    let candidate, target_location =
      match linkage.target with
      | Imported_unverified_external_target (candidate, target_location) ->
          (candidate, target_location)
      | Same_unit_external_target _ -> assert false
    in
    let context =
      {
        source_file;
        typing_environment = function_environment wrapper;
        proof_capture_artifact;
        broadcast_scan = None;
        symbolic_scan = Some symbolic_scan;
        symbolic_definitions;
        imports;
        functions;
        aggregates;
        current_function = Some wrapper;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    let* parameter_nodes, _, _ = function_expression_body context wrapper in
    let definition_body = linkage.carrier.definition_body in
    context.function_result_type <- Some definition_body.exp_type;
    let* parameters, bindings = lower_parameter_list context parameter_nodes in
    let* terminal =
      lower_trusted_contract_prefix context bindings definition_body
    in
    let contracts =
      Callback_contract_private.contracts (List.rev context.contract_carriers)
    in
    let* () =
      if
        parameters = [] || contracts.decreases <> []
        || contracts.assertions <> []
      then malformed definition_body.exp_loc
      else Ok ()
    in
    let* result_type =
      normalized_type context definition_body.exp_loc definition_body.exp_type
    in
    let* terminal_arguments =
      match terminal.exp_desc with
      | Texp_apply
          ( { exp_desc = Texp_ident (path, _, description, _, _); exp_env; _ },
            arguments,
            _,
            _,
            _ ) ->
          let path =
            try Env.normalize_value_path (Some terminal.exp_loc) exp_env path
            with Env.Error _ -> path
          in
          if
            String.equal (Path.name path)
              (External_target_specification_private.candidate_path candidate)
            && String.equal
                 (compiler_uid description.Types.val_uid)
                 (External_target_specification_private.candidate_uid candidate)
          then lower_arguments context bindings terminal arguments
          else malformed terminal.exp_loc
      | _ -> malformed terminal.exp_loc
    in
    let declaration_span = span context wrapper.value_binding.vb_loc in
    let resolve_application path =
      let path_name = Path.name path in
      let normalized_path_name =
        Path.name (normalized_type_path context.typing_environment path)
      in
      let path_uid = type_uid context.typing_environment path in
      let descriptors =
        List.map
          (fun item -> item.Parametric_adt_lowering_private.descriptor)
          context.aggregates.parametric_adts
        @ List.filter_map
            (fun imported -> imported.imported_parametric_descriptor)
            context.imported.imported_types
      in
      let matches =
        List.filter
          (fun descriptor ->
            let constructor = Parametric_adt.type_constructor descriptor in
            String.equal constructor.constructor_path path_name
            || String.equal constructor.constructor_path normalized_path_name
            || Option.fold ~none:false
                 ~some:(String.equal (Parametric_adt.compiler_uid descriptor))
                 path_uid)
          descriptors
      in
      match matches with [ descriptor ] -> Some descriptor | [] | _ :: _ :: _ -> None
    in
    External_target_specification_private.complete_summary
      (Option.get imported.external_target_specifications)
      ~candidate ~resolve_application ~wrapper_id:wrapper.function_id
      ~type_binders:(List.map snd wrapper.parametric_type_binders)
      ~parameter_nodes ~parameters ~contracts ~terminal_arguments ~result_type
      ~target_span:(span context target_location)
      ~declaration_span
      ~witness_span:(span context linkage.carrier.witness_location)
      ~wrapper_is_unannotated_exec:
        (not
           (External_target_specification_private.has_mode_bearing_syntax
              wrapper.value_binding))
    |> Result.map_error (fun message ->
        Diagnostic.make
          (Diagnostic.Unsupported_construct Diagnostic.Malformed_ghost_call)
          declaration_span
        |> Diagnostic.with_message message)
  let trusted_unique_return context parameters result_type location =
    let unique_parameters =
      List.filter_map
        (fun (index, parameter) ->
          match parameter with
          | Sst.Callback_parameter _ -> None
          | Sst.Value_parameter parameter -> (
              match parameter.pattern.pattern_desc with
              | Sst.Bind { uniqueness = Sst.Definitely_unique; typ; _ } ->
                  Some (index, typ)
              | _ -> None))
        (List.mapi (fun index parameter -> (index, parameter)) parameters)
    in
    match unique_parameters with
    | [] -> Ok None
    | [ (index, Sst.Aggregate parameter_type) ]
      when result_type = Sst.Aggregate parameter_type ->
        Ok (Some index)
    | _ -> unsupported context location Diagnostic.Malformed_ghost_call
  let lower_trusted_external_body source_file imports aggregates functions
      imported proof_capture_artifact broadcast_scan symbolic_scan
      symbolic_definitions mode function_ carrier =
    [%log.debug "lower trusted external body"
      ~function_name:
        (Delator.Field.string function_.function_id.function_name)
      ~type_binders:
        (Delator.Field.int (List.length function_.parametric_type_binders))
      ~imported_types:
        (Delator.Field.int (List.length imported.imported_types))
      ~imported_callables:
        (Delator.Field.int (List.length imported.imported_callables))];
    let context =
      {
        source_file;
        typing_environment = function_environment function_;
        proof_capture_artifact;
        broadcast_scan;
        symbolic_scan = Some symbolic_scan;
        symbolic_definitions;
        imports;
        functions;
        aggregates;
        current_function = Some function_;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    if function_.rec_flag = Asttypes.Recursive then
      malformed function_.value_binding.vb_loc
    else
      let* parameter_nodes, _, _ = function_expression_body context function_ in
      let definition_body = carrier.definition_body in
      context.function_result_type <- Some definition_body.exp_type;
      let* parameters, bindings =
        lower_parameter_list context parameter_nodes
      in
      let* _unscanned_body =
        lower_trusted_contract_prefix context bindings definition_body
      in
      let contracts =
        Callback_contract_private.contracts (List.rev context.contract_carriers)
      in
      let* () =
        if
          contracts.ensures = [] || contracts.decreases <> []
          || contracts.assertions <> []
        then malformed definition_body.exp_loc
        else Ok ()
      in
      let* result_type =
        normalized_type context definition_body.exp_loc definition_body.exp_type
      in
      let* returns_unique_parameter =
        match mode with
        | Sst.Proof ->
            if result_type = Sst.Unit then Ok None
            else malformed definition_body.exp_loc
        | Sst.Exec ->
            trusted_unique_return context parameters result_type
              function_.value_binding.vb_loc
        | Sst.Spec -> malformed definition_body.exp_loc
      in
      let declaration_span = span context function_.value_binding.vb_loc in
      let witness_span = span context carrier.witness_location in
      let definition =
        Sst_normalize.authenticated_trusted_external_body_in_mode ~source_file
          ~function_id:function_.function_id ~mode ~parameters ~contracts
          ~result_type ~returns_unique_parameter ~declaration_span ~witness_span
      in
      Ok
        {
          definition with
          Sst.type_binders = List.map snd function_.parametric_type_binders;
        }
  let lower_total_function_cases context source_file function_ fc_cases
      fc_ret_type fc_param fc_loc =
    context.function_result_type <- Some fc_ret_type;
    match fc_cases with
    | [] -> unsupported context fc_loc Diagnostic.Partial_function_parameter
    | first_case :: _ -> (
        match
          normalized_type context first_case.c_lhs.pat_loc
            first_case.c_lhs.pat_type
        with
        | Error _ as error -> error
        | Ok argument_type ->
            let argument_binding =
              fresh_binding context (Ident.name fc_param) argument_type
                first_case.c_lhs.pat_type Sst.Definitely_aliased fc_loc
            in
            let parameter_pattern =
              {
                Sst.pattern_desc = Sst.Bind argument_binding;
                typ = argument_type;
                span = span context fc_loc;
              }
            in
            let scrutinee =
              {
                Sst.expression_desc =
                  Sst.Variable
                    {
                      binding = argument_binding;
                      use_uniqueness = Sst.Definitely_aliased;
                    };
                typ = argument_type;
                span = span context fc_loc;
              }
            in
            let base_bindings = [ (fc_param, argument_binding) ] in
            let rec lower_cases lowered = function
              | [] -> (
                  match normalized_type context fc_loc fc_ret_type with
                  | Error _ as error -> error
                  | Ok result_type ->
                      let lowered_body =
                        {
                          Sst.expression_desc =
                            Sst.Match (scrutinee, List.rev lowered);
                          typ = result_type;
                          span = span context fc_loc;
                        }
                      in
                      let* contracts, body =
                        Callback_contract_private.extract
                          context.contract_carriers lowered_body
                      in
                      let declaration_span =
                        span context function_.value_binding.vb_loc
                      in
                      Ok
                        (Sst_normalize.authenticated_checked_exec ~source_file
                           ~function_id:function_.function_id
                           ~recursive:(function_.rec_flag = Asttypes.Recursive)
                           ~parameters:
                             [
                               Sst.Value_parameter
                                 {
                                   Sst.label = None;
                                   pattern = parameter_pattern;
                                   optional_default = None;
                                 };
                             ]
                           ~contracts ~body ~result_type
                           ~returns_unique_parameter:None ~span:declaration_span)
                  )
              | case :: rest -> (
                  match lower_pattern context base_bindings case.c_lhs with
                  | Error _ as error -> error
                  | Ok (case_pattern, case_bindings) -> (
                      let lower_body case_guard =
                        match
                          lower_authenticated_expression context case_bindings
                            case.c_rhs
                        with
                        | Error _ as error -> error
                        | Ok case_body ->
                            lower_cases
                              ({
                                 Sst.case_pattern;
                                 case_guard;
                                 case_body;
                                 case_span = span context case.c_rhs.exp_loc;
                               }
                              :: lowered)
                              rest
                      in
                      match case.c_guard with
                      | None -> lower_body None
                      | Some guard -> (
                          match
                            lower_authenticated_expression context case_bindings
                              guard
                          with
                          | Error _ as error -> error
                          | Ok guard -> lower_body (Some guard))))
            in
            lower_cases [] fc_cases)
  let callback_function_context source_file imports aggregates functions
      owned_tree_candidate_roots imported proof_capture_artifact broadcast_scan
      symbolic_scan symbolic_definitions callbacks function_ =
    {
      source_file;
      typing_environment = function_environment function_;
      proof_capture_artifact;
      broadcast_scan;
      symbolic_scan = Some symbolic_scan;
      symbolic_definitions;
      imports;
      functions;
      aggregates;
      current_function = Some function_;
      next_binding = 0;
      binding_types = [];
      binding_locations = [];
      proof_capture_type_hints = [];
      seen_ghost_ids = [];
      contract_carriers = [];
      logical_ghost_depth = 0;
      function_result_type = None;
      owned_tree_cursors = [];
      owned_tree_observation_paths = [];
      owned_tree_root_versions = [];
      shared_scalar_formal_roots = [];
      shared_scalar_aliases = [];
      shared_scalar_next_epoch = 0;
      owned_tree_candidate_roots;
      imported;
      callbacks;
    }
  let prepare_callback_function_context context definition_body =
    context.function_result_type <- Some definition_body.exp_type;
    prepare_proof_capture_type_hints context definition_body
  let lower_symbolic_definition source_file imports aggregates functions
      owned_tree_candidate_roots imported proof_capture_artifact broadcast_scan
      symbolic_scan symbolic_definitions function_ source =
    let callbacks = empty_callback_lowering_state () in
    let context =
      callback_function_context source_file imports aggregates functions
        owned_tree_candidate_roots imported proof_capture_artifact
        broadcast_scan symbolic_scan symbolic_definitions callbacks function_
    in
    let rec symbolic_parameters reversed expression =
      match expression.exp_desc with
      | Texp_function { params; body = Tfunction_body body; _ } ->
          symbolic_parameters (List.rev_append params reversed) body
      | _ -> (List.rev reversed, expression)
    in
    let parameters, result =
      symbolic_parameters [] function_.value_binding.vb_expr
    in
    context.function_result_type <- Some result.exp_type;
    let* parameters, _ =
      Callback_contract_private.lower_parameters
        {
          normalized_type = normalized_type context;
          optional_carrier = optional_carrier context;
          lower_expression = lower_authenticated_expression context;
          lower_pattern = lower_pattern context;
          issue_callback =
            (fun parameter _ ->
              unsupported context parameter.fp_loc
                Diagnostic.Higher_order_function);
          is_callback = (fun _ -> false);
          parameter_label;
          span = span context;
          partial_error =
            (fun location ->
              Diagnostic.make
                (Diagnostic.Unsupported_construct
                   Diagnostic.Partial_function_parameter)
                (span context location));
        }
        [] parameters
    in
    let* result_type =
      normalized_type context result.exp_loc result.exp_type
    in
    Symbolic_declaration_private.make_definition ~source
      ~function_id:function_.function_id
      ~type_binders:(List.map snd function_.parametric_type_binders)
      ~parameters ~result_type
      ~span:(span context function_.value_binding.vb_loc)
    |> Result.map_error (fun message ->
           Diagnostic.make (Diagnostic.Invalid_symbolic_declaration message)
             (span context function_.value_binding.vb_loc))
  let lower_symbolic_definitions source_file imports aggregates functions
      owned_tree_candidate_roots imported proof_capture_artifact broadcast_scan
      symbolic_scan =
    let rec loop definitions = function
      | [] -> Ok (List.rev definitions)
      | function_ :: rest -> (
          match
            Typedtree_symbolic_private.find_binding symbolic_scan
              function_.value_binding
          with
          | None -> loop definitions rest
          | Some source ->
              let* definition =
                lower_symbolic_definition source_file imports aggregates
                  functions owned_tree_candidate_roots imported
                  proof_capture_artifact broadcast_scan symbolic_scan
                  definitions function_ source
              in
              loop
                ((function_.function_id.function_index, definition)
                :: definitions)
                rest)
    in
    loop [] functions
  let callback_definition_body function_ body =
    match function_.function_kind with
    | Top_exec -> body
    | Top_spec carrier -> carrier.definition_body
    | Top_type_invariant carrier -> carrier.definition_body
    | Top_recursive_spec carrier -> carrier.definition_body
    | Top_proof carrier -> carrier.definition_body
    | Top_external_specification carrier -> carrier.definition_body
    | Top_external_body _ -> assert false
  let lower_function_with_callbacks authenticated_source_text source_file imports
      aggregates functions owned_tree_candidate_roots imported
      proof_capture_artifact broadcast_scan symbolic_scan symbolic_definitions
      callbacks function_ =
    match function_.function_kind with
    | Top_external_body (mode, carrier) ->
        lower_trusted_external_body source_file imports aggregates functions
          imported proof_capture_artifact broadcast_scan symbolic_scan
          symbolic_definitions mode function_ carrier
    | Top_exec | Top_spec _ | Top_type_invariant _ | Top_recursive_spec _ | Top_proof _
    | Top_external_specification _ -> (
        let context =
          callback_function_context source_file imports aggregates functions
            owned_tree_candidate_roots imported proof_capture_artifact
            broadcast_scan symbolic_scan symbolic_definitions callbacks function_
        in
        match
          first_unknown_field_write context function_.value_binding.vb_expr
        with
        | Some location -> unsupported context location Diagnostic.Mutation
        | None -> (
            match function_.value_binding.vb_expr.exp_desc with
            | Texp_function { params; body = Tfunction_body body; _ } ->
                let definition_body = callback_definition_body function_ body in
                let* () =
                  prepare_callback_function_context context definition_body
                in
                let* parameters, bindings =
                  Callback_contract_private.lower_parameters
                    {
                      normalized_type = normalized_type context;
                      optional_carrier = optional_carrier context;
                      lower_expression = lower_authenticated_expression context;
                      lower_pattern = lower_pattern context;
                      issue_callback = issue_callback_formal context;
                      is_callback =
                        (match function_.function_kind with
                        | Top_exec -> callback_arrow_type
                        | Top_spec _ | Top_type_invariant _
                        | Top_recursive_spec _ | Top_proof _
                        | Top_external_specification _ | Top_external_body _ ->
                            fun _ -> false);
                      parameter_label;
                      span = span context;
                      partial_error =
                        (fun location ->
                          callback_diagnostic context
                            Diagnostic.Partial_function_parameter location);
                    }
                    [] params
                in
                configure_shared_scalar_formals context
                  authenticated_source_text params parameters;
                let* lowered_body =
                  lower_authenticated_expression context bindings
                    definition_body
                in
                let* contracts, body =
                  Callback_contract_private.extract context.contract_carriers
                    lowered_body
                in
                let declaration_span =
                  span context function_.value_binding.vb_loc
                in
                let* kind =
                  match function_.function_kind with
                  | Top_exec ->
                      Ok
                        (Callback_contract_private.Checked_exec
                           (returned_unique_parameter function_.function_id
                              parameters body))
                  | Top_spec _ | Top_type_invariant _ ->
                      if function_.rec_flag = Asttypes.Recursive then
                        unsupported context function_.value_binding.vb_loc
                          Diagnostic.Malformed_ghost_call
                      else if contracts <> Sst.empty_contracts then
                        unsupported context definition_body.exp_loc
                          Diagnostic.Malformed_ghost_call
                      else Ok Callback_contract_private.Spec_definition
                  | Top_recursive_spec carrier ->
                      if function_.rec_flag <> Asttypes.Recursive then
                        unsupported context function_.value_binding.vb_loc
                          Diagnostic.Malformed_ghost_call
                      else
                        Ok
                          (Callback_contract_private.Recursive_spec_definition
                             (Option.get carrier.recursive_visibility))
                  | Top_proof _ -> Ok Callback_contract_private.Proof_definition
                  | Top_external_specification _ ->
                      unsupported context function_.value_binding.vb_loc
                        Diagnostic.Malformed_ghost_call
                  | Top_external_body _ -> assert false
                in
                let definition =
                  {
                    (Callback_contract_private.make_definition ~source_file
                       ~function_id:function_.function_id
                       ~recursive:(function_.rec_flag = Asttypes.Recursive)
                       ~parameters ~contracts ~body ~span:declaration_span kind)
                    with
                    Sst.type_binders = List.map snd function_.parametric_type_binders;
                  }
                in
                if Spec_function_sst_private.is_bare_carrier definition then
                  unsupported context definition_body.exp_loc Diagnostic.Higher_order_function
                else Ok definition
            | Texp_function
                {
                  body = Tfunction_cases { fc_partial = Partial; fc_loc; _ };
                  _;
                } ->
                unsupported context fc_loc Diagnostic.Partial_function_parameter
            | Texp_function
                {
                  params = [];
                  body =
                    Tfunction_cases
                      {
                        fc_cases;
                        fc_ret_type;
                        fc_partial = Total;
                        fc_param;
                        fc_loc;
                        _;
                      };
                  _;
                } ->
                lower_total_function_cases context source_file function_
                  fc_cases fc_ret_type fc_param fc_loc
            | Texp_function _ ->
                unsupported context function_.value_binding.vb_loc
                  Diagnostic.Higher_order_function
            | _ ->
                unsupported context function_.value_binding.vb_loc
                  Diagnostic.Unsupported_top_level_binding))
  let lower_function ?compilation_identity authenticated_source_text source_file
      imports aggregates functions owned_tree_candidate_roots imported
      proof_capture_artifact broadcast_scan symbolic_scan symbolic_definitions
      function_ =
    let callbacks = empty_callback_lowering_state ?compilation_identity () in
    let* definition =
      lower_function_with_callbacks authenticated_source_text source_file
        imports aggregates functions owned_tree_candidate_roots imported
        proof_capture_artifact broadcast_scan symbolic_scan
        symbolic_definitions callbacks function_
    in
    Ok (definition, List.rev callbacks.callback_definitions)
  let authenticate_constrained_module source_file imports aggregates functions
      lowered_definitions candidate =
    let context =
      {
        source_file;
        typing_environment = functions_environment functions;
        proof_capture_artifact = None;
        broadcast_scan = None;
        symbolic_scan = None;
        symbolic_definitions = [];
        imports;
        functions;
        aggregates;
        current_function = None;
        next_binding = 0;
        binding_types = [];
        binding_locations = [];
        proof_capture_type_hints = [];
        seen_ghost_ids = [];
        contract_carriers = [];
        logical_ghost_depth = 0;
        function_result_type = None;
        owned_tree_cursors = [];
        owned_tree_observation_paths = [];
        owned_tree_root_versions = [];
        shared_scalar_formal_roots = [];
        shared_scalar_aliases = [];
        shared_scalar_next_epoch = 0;
        owned_tree_candidate_roots = [];
        imported = empty_imported_environment;
        callbacks = empty_callback_lowering_state ();
      }
    in
    let malformed location =
      unsupported context location Diagnostic.Unsupported_structure_item
    in
    let definition_of function_ =
      List.find_opt
        (fun (definition : Sst.function_definition) ->
          definition.function_id = function_.function_id)
        lowered_definitions
    in
    let type_definition type_id =
      List.find_opt
        (fun (definition : Sst.type_definition) -> definition.type_id = type_id)
        aggregates.definitions
    in
    let rec deeply_immutable visiting = function
      | Sst.Unit | Sst.Bool | Sst.Int -> true
      | Sst.Parameter _ | Sst.Application _ -> false
      | Sst.Tuple components ->
          List.for_all
            (fun (_, typ) -> deeply_immutable visiting typ)
            components
      | Sst.Aggregate type_id -> (
          if
            (* A recursive immutable datatype is still deeply immutable.  This
           classifier authenticates representation mutability only; exact
           finite-value authority is issued separately after construction. *)
            List.mem type_id visiting
          then true
          else
            match type_definition type_id with
            | None -> false
            | Some definition ->
                let fields =
                  match definition.type_kind with
                  | Sst.Record_definition fields -> fields
                  | Sst.Variant_definition constructors ->
                      List.concat_map
                        (fun constructor -> constructor.Sst.constructor_fields)
                        constructors
                in
                List.for_all
                  (fun field ->
                    field.Sst.field_mutability = Sst.Immutable_field
                    && deeply_immutable (type_id :: visiting) field.field_type)
                  fields)
    in
    let public_type_ids =
      List.map (fun (_, local) -> local.type_id) candidate.public_type_links
    in
    let* () =
      let rec validate = function
        | [] -> Ok ()
        | (public, local) :: rest ->
            if
              public.typ_params <> [] || public.typ_cstrs <> []
              || public.typ_private <> Asttypes.Public
              || public.typ_kind <> Ttype_abstract
              || public.typ_manifest <> None
            then malformed public.typ_loc
            else if Option.is_none (type_definition local.type_id) then
              malformed local.declaration.typ_loc
            else validate rest
      in
      validate candidate.public_type_links
    in
    let linked_public_definitions =
      List.filter_map
        (fun (public, function_) ->
          Option.map
            (fun definition -> (public, function_, definition))
            (definition_of function_))
        candidate.public_functions
    in
    let* () =
      let rec authenticate = function
        | [] -> Ok ()
        | (public, function_, _) :: rest -> (
            match
              Finite_formal_requirement.authenticate_constrained_signature
                ~implementation:function_.value_binding.vb_attributes
                ~interface:public.val_attributes
            with
            | Ok () -> authenticate rest
            | Error _ -> malformed public.val_loc)
      in
      authenticate linked_public_definitions
    in
    let* () =
      if List.length linked_public_definitions = List.length candidate.public_functions
      then Ok ()
      else malformed candidate.constraint_location
    in
    let public_definitions =
      List.filter (fun (_, _, value) -> value.Sst.mode <> Sst.Proof)
        linked_public_definitions
    in
    let* () =
      if
        List.for_all
          (fun (_, _, (definition : Sst.function_definition)) ->
            match definition.body with
            | Sst.Checked_exec _ -> definition.mode = Sst.Exec
            | Sst.Spec_definition _ -> definition.mode = Sst.Spec
            | Sst.Recursive_spec_definition _ -> false
            | Sst.Proof_body _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
            | Sst.Symbolic_declaration _
              ->
                false)
          public_definitions
      then Ok ()
      else malformed candidate.constraint_location
    in
    let parameter_binding = function
      | Sst.Callback_parameter _ -> None
      | Sst.Value_parameter parameter -> (
          match parameter.pattern.pattern_desc with
          | Sst.Bind binding -> Some binding
          | _ -> None)
    in
    let return_is_unique function_ =
      match function_expression_body context function_ with
      | Ok (_, _, return_mode) -> (
          match (Mode.Alloc.zap_to_floor return_mode).uniqueness with
          | Mode.Uniqueness.Const.Unique -> true
          | Aliased -> false)
      | Error _ -> false
    in
    let transition_state = function
      | ( _,
          function_,
          ({
             parameters = [ parameter ];
             result_type = Sst.Aggregate result_type;
             returns_unique_parameter = Some 0;
             _;
           } :
            Sst.function_definition) ) -> (
          match parameter_binding parameter with
          | Some
              {
                typ = Sst.Aggregate parameter_type;
                uniqueness = Sst.Definitely_unique;
                _;
              }
            when parameter_type = result_type && return_is_unique function_ ->
              Some result_type
          | _ -> None)
      | _ -> None
    in
    let transition_states =
      List.filter_map transition_state public_definitions
      |> List.sort_uniq compare
    in
    let abstract_input_state = function
      | ( _,
          function_,
          ({
             parameters =
               [
                 Sst.Value_parameter
                   {
                     label = None;
                     pattern =
                       {
                         pattern_desc =
                           Sst.Bind
                             {
                               typ = Sst.Aggregate parameter_type;
                               uniqueness = Sst.Definitely_aliased;
                               _;
                             };
                         typ = Sst.Aggregate pattern_type;
                         _;
                       };
                     optional_default = _;
                   };
               ];
             body = Sst.Spec_definition _;
             _;
           } :
            Sst.function_definition) )
        when parameter_type = pattern_type
             && List.mem parameter_type public_type_ids
             &&
             match function_.function_kind with
             | Top_spec _ | Top_type_invariant _ -> true
             | Top_exec | Top_recursive_spec _ | Top_proof _
             | Top_external_specification _ | Top_external_body _ ->
                 false ->
          Some parameter_type
      | _ -> None
    in
    let abstract_input_states =
      List.filter_map abstract_input_state public_definitions
      |> List.sort_uniq compare
    in
    let* state_type =
      match
        List.sort_uniq compare (transition_states @ abstract_input_states)
      with
      | [ state_type ] when List.mem state_type public_type_ids -> Ok state_type
      | _ -> malformed candidate.constraint_location
    in
    let representation_ids =
      List.map (fun definition -> definition.Sst.type_id) aggregates.definitions
    in
    let is_representation_type = function
      | Sst.Aggregate type_id -> List.mem type_id representation_ids
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          false
    in
    let shared_invariant_state =
      match type_definition state_type with
      | Some { Sst.type_kind = Sst.Record_definition fields; _ } -> (
          let mutable_fields =
            List.filter
              (fun (field : Sst.field_definition) ->
                field.field_mutability = Sst.Mutable_field)
              fields
          in
          match mutable_fields with
          | [ field ] ->
              List.length fields = 1
              && field.field_type = Sst.Int
              && not
                   (List.exists
                      (fun (candidate : Sst.field_definition) ->
                        shared_scalar_type_mentions state_type
                          candidate.field_type)
                      fields)
          | [] | _ :: _ -> false)
      | Some { Sst.type_kind = Sst.Variant_definition _; _ } | None -> false
    in
    let simple_binding definition =
      match definition.Sst.parameters with
      | [ parameter ] -> parameter_binding parameter
      | [] | _ :: _ -> None
    in
    let function_calls target expression =
      let rec count (expression : Sst.expression) =
        let own =
          match expression.expression_desc with
          | Sst.Direct_call { callee; _ } when callee = target -> 1
          | _ -> 0
        in
        let children =
          match expression.expression_desc with
          | Sst.Tuple_value values -> List.map snd values
          | Sst.Record_value { fields; _ } -> List.map snd fields
          | Sst.Constructor_value { arguments; _ } -> arguments
          | Sst.Field_read { record; _ } -> [ record ]
          | Sst.Field_write { value; _ }
          | Sst.Shared_scalar_field_write { value; _ }
          | Sst.Owned_tree_nested_write { value; _ }
          | Sst.Mutable_write { value; _ } ->
              [ value ]
          | Sst.Owned_tree_rebase _ -> []
          | Sst.Let_mutable (_, initial, body) -> [ initial; body ]
          | Sst.Let (bindings, body) -> List.map snd bindings @ [ body ]
          | Sst.Sequence (left, right)
          | Sst.Compare (_, left, right)
          | Sst.Boolean_binary (_, left, right) ->
              [ left; right ]
          | Sst.If (condition, consequent, alternative) ->
              condition :: consequent :: Option.to_list alternative
          | Sst.Match (scrutinee, cases) ->
              scrutinee
              :: List.concat_map
                   (fun case ->
                     Option.to_list case.Sst.case_guard @ [ case.case_body ])
                   cases
          | Sst.Checked_arithmetic (_, operands) -> operands
          | Sst.Boolean_not operand
          | Sst.Old operand
          | Sst.Proof_region operand
          | Sst.Use_type_invariant { value = operand; _ }
          | Sst.Local_assert { predicate = operand; _ }
          | Sst.Optional_present operand
          | Sst.Optional_forward operand ->
              [ operand ]
          | Sst.Direct_call { arguments; _ } ->
              List.map
                (fun argument -> snd (Sst.require_value_argument argument))
                arguments
          | Sst.Callback_call application | Sst.Callback_requires application ->
              List.map snd application.Sst.arguments
          | Sst.Callback_ensures { application; result } ->
              List.map snd application.Sst.arguments @ [ result ]
          | Sst.Symbolic_application application ->
              Symbolic_application_private.arguments application
          | Sst.Forall quantifier | Sst.Exists quantifier ->
              [ quantifier.quantifier_body ]
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
          | Sst.Variable _ | Sst.Mutable_read _ | Sst.Reveal _
          | Sst.Reveal_with_fuel _ | Sst.Optional_absent ->
              []
        in
        own + List.fold_left (fun total child -> total + count child) 0 children
      in
      count expression
    in
    let frozen_spine_prerequisite =
      let root_shape =
        match type_definition state_type with
        | Some { Sst.type_kind = Sst.Record_definition fields; _ } -> (
            let payloads =
              List.filter
                (fun (field : Sst.field_definition) ->
                  field.field_mutability = Sst.Mutable_field
                  && field.field_type = Sst.Int)
                fields
            and edges =
              List.filter
                (fun (field : Sst.field_definition) ->
                  field.field_mutability = Sst.Immutable_field
                  &&
                  match field.field_type with
                  | Sst.Aggregate _ -> true
                  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                  | Sst.Parameter _ | Sst.Application _ ->
                      false)
                fields
            in
            match (payloads, edges) with
            | [ payload ], [ edge ] when List.length fields = 2 ->
                Some (payload, edge)
            | _ -> None)
        | Some { Sst.type_kind = Sst.Variant_definition _; _ } | None -> None
      in
      Option.bind root_shape (fun (payload, edge) ->
          let link_type =
            match edge.Sst.field_type with
            | Sst.Aggregate type_id -> type_id
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
            | Sst.Application _ ->
                assert false
          in
          let link_shape =
            match type_definition link_type with
            | Some { Sst.type_kind = Sst.Variant_definition constructors; _ }
              -> (
                let nils =
                  List.filter
                    (fun constructor -> constructor.Sst.constructor_fields = [])
                    constructors
                and nexts =
                  List.filter
                    (fun constructor ->
                      match constructor.Sst.constructor_fields with
                      | [ child ] ->
                          child.field_mutability = Sst.Immutable_field
                          && child.field_type = Sst.Aggregate state_type
                      | [] | _ :: _ -> false)
                    constructors
                in
                match (nils, nexts) with
                | [ nil ], [ next ] when List.length constructors = 2 ->
                    Some (nil, next, List.hd next.constructor_fields)
                | _ -> None)
            | Some { Sst.type_kind = Sst.Record_definition _; _ } | None -> None
          in
          Option.bind link_shape (fun (nil, next, child) ->
              let helpers =
                List.filter
                  (fun (definition : Sst.function_definition) ->
                    definition.recursive
                    &&
                    match (definition.body, simple_binding definition) with
                    | ( Sst.Recursive_spec_definition { visibility = `Opaque; _ },
                        Some binding ) -> (
                        binding.typ = Sst.Aggregate state_type
                        && binding.uniqueness = Sst.Definitely_aliased
                        &&
                        match definition.result_type with
                        | Sst.Aggregate _ -> true
                        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                        | Sst.Parameter _ | Sst.Application _ ->
                            false)
                    | _ -> false)
                  lowered_definitions
              in
              match helpers with
              | [ helper ] ->
                  let result_type =
                    match helper.result_type with
                    | Sst.Aggregate type_id -> type_id
                    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                    | Sst.Parameter _ | Sst.Application _ ->
                        assert false
                  in
                  let result_shape =
                    match type_definition result_type with
                    | Some
                        {
                          Sst.type_kind = Sst.Variant_definition constructors;
                          _;
                        } -> (
                        let ends =
                          List.filter
                            (fun constructor ->
                              constructor.Sst.constructor_fields = [])
                            constructors
                        and mores =
                          List.filter
                            (fun constructor ->
                              match constructor.Sst.constructor_fields with
                              | [ head; tail ] ->
                                  head.field_mutability = Sst.Immutable_field
                                  && head.field_type = Sst.Int
                                  && tail.field_mutability = Sst.Immutable_field
                                  && tail.field_type = Sst.Aggregate result_type
                              | [] | [ _ ] | _ :: _ :: _ :: _ -> false)
                            constructors
                        in
                        match (ends, mores) with
                        | [ end_ ], [ more ] when List.length constructors = 2
                          ->
                            Some
                              ( end_,
                                more,
                                List.nth more.constructor_fields 0,
                                List.nth more.constructor_fields 1 )
                        | _ -> None)
                    | Some { Sst.type_kind = Sst.Record_definition _; _ } | None
                      ->
                        None
                  in
                  Option.bind result_shape
                    (fun (end_, more, more_head, more_tail) ->
                      let models =
                        List.filter
                          (fun (_, _, (definition : Sst.function_definition)) ->
                            definition.body |> function
                            | Sst.Spec_definition body ->
                                definition.result_type
                                = Sst.Aggregate result_type
                                && Option.fold ~none:false
                                     ~some:(fun (binding : Sst.binding) ->
                                       binding.typ = Sst.Aggregate state_type)
                                     (simple_binding definition)
                                && function_calls helper.function_id
                                     body.expression
                                   = 1
                            | Sst.Checked_exec _
                            | Sst.Recursive_spec_definition _ | Sst.Proof_body _
                            | Sst.External_specification _
                            | Sst.Trusted_external_spec_target _
                            | Sst.Trusted_external_body _
                            | Sst.Symbolic_declaration _ ->
                                false)
                          public_definitions
                      and invariants =
                        List.filter
                          (fun (_, function_, definition) ->
                            match
                              ( function_.function_kind,
                                definition.Sst.body,
                                simple_binding definition )
                            with
                            | ( Top_type_invariant _,
                                Sst.Spec_definition _,
                                Some (binding : Sst.binding) ) ->
                                definition.result_type = Sst.Bool
                                && binding.typ = Sst.Aggregate state_type
                            | _ -> false)
                          public_definitions
                      and constructors =
                        List.filter
                          (fun (_, function_, definition) ->
                            match definition.Sst.body with
                            | Sst.Checked_exec _ ->
                                definition.result_type
                                = Sst.Aggregate state_type
                                && return_is_unique function_
                                && not
                                     (List.exists
                                        (function
                                          | Sst.Value_parameter parameter ->
                                              parameter.Sst.pattern.typ
                                              = Sst.Aggregate state_type
                                          | Sst.Callback_parameter _ -> false)
                                        definition.parameters)
                            | _ -> false)
                          public_definitions
                      and mutators =
                        List.filter
                          (fun (_, _, definition) ->
                            definition.Sst.result_type = Sst.Unit
                            && Option.fold ~none:false
                                 ~some:(fun (binding : Sst.binding) ->
                                   binding.typ = Sst.Aggregate state_type
                                   && binding.uniqueness
                                      = Sst.Definitely_aliased)
                                 (simple_binding
                                    {
                                      definition with
                                      parameters =
                                        List.filter
                                          (function
                                            | Sst.Value_parameter parameter ->
                                                parameter.Sst.pattern.typ
                                                = Sst.Aggregate state_type
                                            | Sst.Callback_parameter _ -> false)
                                          definition.parameters;
                                    })
                            && Sst.function_shared_scalar_transitions definition
                               <> [])
                          public_definitions
                      and terminals =
                        List.filter
                          (fun (_, _, definition) ->
                            definition.Sst.result_type = Sst.Int
                            && Option.fold ~none:false
                                 ~some:(fun (binding : Sst.binding) ->
                                   binding.typ = Sst.Aggregate state_type
                                   && binding.uniqueness
                                      = Sst.Definitely_aliased)
                                 (simple_binding definition)
                            && Sst.function_shared_scalar_transitions definition
                               = [])
                          public_definitions
                      in
                      match
                        (models, invariants, constructors, mutators, terminals)
                      with
                      | ( [ (_, _, model) ],
                          [ (_, _, invariant) ],
                          [ (_, _, constructor) ],
                          [ (_, _, mutator) ],
                          [ (_, _, terminal) ] ) ->
                          Some
                            {
                              Sst.frozen_root = state_type;
                              frozen_link = link_type;
                              frozen_payload_field = payload.field_id;
                              frozen_edge_field = edge.field_id;
                              frozen_nil_constructor = nil.constructor_id;
                              frozen_next_constructor = next.constructor_id;
                              frozen_next_child_field = child.field_id;
                              frozen_helper = helper.function_id;
                              frozen_model = model.function_id;
                              frozen_result_type = result_type;
                              frozen_end_constructor = end_.constructor_id;
                              frozen_more_constructor = more.constructor_id;
                              frozen_more_head_field = more_head.field_id;
                              frozen_more_tail_field = more_tail.field_id;
                              frozen_invariant = invariant.function_id;
                              frozen_constructor = constructor.function_id;
                              frozen_mutator = mutator.function_id;
                              frozen_terminal = terminal.function_id;
                            }
                      | _ -> None)
              | [] | _ :: _ -> None))
    in
    let one_scalar_snapshot = function
      | Sst.Aggregate type_id -> (
          match type_definition type_id with
          | Some
              {
                Sst.type_kind = Sst.Record_definition [ field ];
                representation = Sst.Revealed;
                _;
              } ->
              field.field_type = Sst.Int
              && field.field_mutability = Sst.Immutable_field
              && field.field_modalities.uniqueness_modality
                 = Sst.Preserve_uniqueness
              && field.field_modalities.linearity_modality
                 = Sst.Preserve_linearity
          | Some
              {
                Sst.type_kind =
                  Sst.Record_definition _ | Sst.Variant_definition _;
                representation = Sst.Revealed | Sst.Abstract_with_evidence _;
                _;
              }
          | None ->
              false)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          false
    in
    let classify_operation
        (public, function_, (definition : Sst.function_definition)) =
      let parameters =
        List.filter_map parameter_binding definition.parameters
      in
      let state_parameters =
        List.filter
          (fun (binding : Sst.binding) ->
            binding.Sst.typ = Sst.Aggregate state_type)
          parameters
      in
      match (definition.body, state_parameters, definition.result_type) with
      | ( Sst.Spec_definition _,
          [
            {
              typ = Sst.Aggregate parameter_type;
              uniqueness = Sst.Definitely_aliased;
              _;
            };
          ],
          Sst.Bool )
        when parameter_type = state_type
             && List.length parameters = 1
             &&
             match function_.function_kind with
             | Top_type_invariant _ -> true
             | Top_exec | Top_spec _ | Top_recursive_spec _ | Top_proof _
             | Top_external_specification _ | Top_external_body _ ->
                 false ->
          Ok Sst.Abstract_invariant
      | ( Sst.Spec_definition _,
          [
            {
              typ = Sst.Aggregate parameter_type;
              uniqueness = Sst.Definitely_aliased;
              _;
            };
          ],
          _ )
        when parameter_type = state_type && List.length parameters = 1 ->
          if
            shared_invariant_state && one_scalar_snapshot definition.result_type
          then Ok Sst.Current_model
          else if
            Option.fold ~none:false
              ~some:(fun frozen ->
                frozen.Sst.frozen_model = definition.function_id
                && definition.result_type
                   = Sst.Aggregate frozen.frozen_result_type)
              frozen_spine_prerequisite
          then Ok Sst.Current_model
          else if shared_invariant_state then malformed public.val_loc
          else Ok Sst.Abstract_model
      | Sst.Checked_exec _, [], Sst.Aggregate result_type
        when result_type = state_type && return_is_unique function_ ->
          if
            List.for_all
              (fun (binding : Sst.binding) ->
                deeply_immutable [] binding.Sst.typ)
              parameters
          then Ok Sst.Abstract_constructor
          else malformed public.val_loc
      | ( Sst.Checked_exec _,
          [
            {
              typ = Sst.Aggregate parameter_type;
              uniqueness = Sst.Definitely_unique;
              _;
            };
          ],
          Sst.Aggregate result_type )
        when parameter_type = state_type
             && result_type = state_type
             && definition.returns_unique_parameter = Some 0
             && return_is_unique function_ ->
          if List.length parameters = 1 then Ok Sst.Unique_transition
          else malformed public.val_loc
      | ( Sst.Checked_exec _,
          [
            {
              typ = Sst.Aggregate parameter_type;
              uniqueness = Sst.Definitely_aliased;
              _;
            };
          ],
          result_type )
        when parameter_type = state_type && deeply_immutable [] result_type ->
          if
            (shared_invariant_state
            || Option.fold ~none:false
                 ~some:(fun frozen ->
                   frozen.Sst.frozen_mutator = definition.function_id)
                 frozen_spine_prerequisite)
            && Sst.function_shared_scalar_transitions definition <> []
          then
            if result_type = Sst.Unit then Ok Sst.Shared_invariant_transition
            else malformed public.val_loc
          else if is_representation_type result_type then
            Ok Sst.Terminal_snapshot
          else if
            shared_invariant_state
            || Option.fold ~none:false
                 ~some:(fun frozen ->
                   frozen.Sst.frozen_terminal = definition.function_id)
                 frozen_spine_prerequisite
          then
            if result_type = Sst.Int && List.length parameters = 1 then
              Ok Sst.Current_terminal_read
            else malformed public.val_loc
          else Ok Sst.Terminal_read
      | Sst.Checked_exec _, [], result_type when deeply_immutable [] result_type
        ->
          if
            parameters <> []
            && List.for_all
                 (fun (binding : Sst.binding) ->
                   binding.Sst.uniqueness = Sst.Definitely_aliased
                   && deeply_immutable [] binding.typ
                   && binding.typ <> Sst.Aggregate state_type)
                 parameters
          then Ok Sst.Terminal_read
          else malformed public.val_loc
      | ( ( Sst.Proof_body _ | Sst.Recursive_spec_definition _
          | Sst.External_specification _ | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ),
          _,
          _ )
      | Sst.Spec_definition _, _, _
      | Sst.Checked_exec _, _, _ ->
          malformed public.val_loc
    in
    let rec classify surface constructors models invariants transitions
        snapshots = function
      | [] ->
          Ok
            ( List.rev surface,
              constructors,
              models,
              invariants,
              transitions,
              snapshots )
      | ((public, _, definition) as operation) :: rest ->
          let* role = classify_operation operation in
          let surface_operation =
            {
              Sst.public_function_index = definition.function_id.function_index;
              public_function_name = definition.function_id.function_name;
              public_role = role;
              public_span = span context public.val_loc;
            }
          in
          classify
            (surface_operation :: surface)
            (constructors + if role = Sst.Abstract_constructor then 1 else 0)
            (models
            +
            if role = Sst.Abstract_model || role = Sst.Current_model then 1
            else 0)
            (invariants + if role = Sst.Abstract_invariant then 1 else 0)
            (transitions
            +
            if
              role = Sst.Unique_transition
              || role = Sst.Shared_invariant_transition
            then 1
            else 0)
            (snapshots
            +
            if
              role = Sst.Terminal_snapshot
              || role = Sst.Terminal_read
              || role = Sst.Current_terminal_read
            then 1
            else 0)
            rest
    in
    let* ( public_surface,
           constructors,
           models,
           invariants,
           transitions,
           snapshots ) =
      classify [] 0 0 0 0 0 public_definitions
    in
    let* () =
      if
        (if shared_invariant_state || Option.is_some frozen_spine_prerequisite
         then
           constructors = 1 && models = 1 && invariants = 1 && transitions = 1
           && snapshots = 1
         else constructors > 0 && snapshots > 0)
        && ((invariants = 0 && transitions > 0)
           || (invariants = 1 && models = 1))
      then Ok ()
      else malformed candidate.constraint_location
    in
    let public_view_ids =
      List.filter (fun type_id -> type_id <> state_type) public_type_ids
    in
    let* () =
      if
        List.for_all
          (fun type_id -> deeply_immutable [] (Sst.Aggregate type_id))
          public_view_ids
      then Ok ()
      else malformed candidate.constraint_location
    in
    let fields definition =
      match definition.Sst.type_kind with
      | Sst.Record_definition fields -> [ fields ]
      | Sst.Variant_definition constructors ->
          List.map
            (fun constructor -> constructor.Sst.constructor_fields)
            constructors
    in
    let aggregate_targets definition =
      let rec type_targets = function
        | Sst.Aggregate target -> [ target ]
        | Sst.Tuple components ->
            List.concat_map (fun (_, typ) -> type_targets typ) components
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ | Sst.Application _ ->
            []
      in
      fields definition
      |> List.concat_map (fun fields ->
          List.concat_map
            (fun field ->
              List.map
                (fun target -> (field, target))
                (type_targets field.Sst.field_type))
            fields)
    in
    let rec can_reach seen source target =
      source = target
      ||
      if List.mem source seen then false
      else
        match type_definition source with
        | None -> false
        | Some definition ->
            List.exists
              (fun (_, next) -> can_reach (source :: seen) next target)
              (aggregate_targets definition)
    in
    let rec reachable seen = function
      | [] -> seen
      | type_id :: rest when List.mem type_id seen -> reachable seen rest
      | type_id :: rest -> (
          match type_definition type_id with
          | None -> reachable (type_id :: seen) rest
          | Some definition ->
              reachable (type_id :: seen)
                (List.map snd (aggregate_targets definition) @ rest))
    in
    let owned_types = reachable [] [ state_type ] in
    let recursive_edges =
      List.concat_map
        (fun owner ->
          match type_definition owner with
          | None -> []
          | Some definition ->
              List.filter_map
                (fun (field, target) ->
                  if can_reach [] target owner then Some field.Sst.field_id
                  else None)
                (aggregate_targets definition))
        owned_types
      |> List.sort_uniq compare
    in
    let* () =
      if shared_invariant_state then
        if recursive_edges = [] then Ok ()
        else malformed candidate.constraint_location
      else if Option.is_some frozen_spine_prerequisite then
        match frozen_spine_prerequisite with
        | Some frozen
          when List.sort compare recursive_edges
               = List.sort compare
                   [
                     frozen.Sst.frozen_edge_field;
                     frozen.frozen_next_child_field;
                   ] ->
            Ok ()
        | Some _ | None -> malformed candidate.constraint_location
      else if transitions = 0 then
        if deeply_immutable [] (Sst.Aggregate state_type) then Ok ()
        else malformed candidate.constraint_location
      else if recursive_edges = [] then malformed candidate.constraint_location
      else
        let valid_shape type_id =
          match type_definition type_id with
          | None -> false
          | Some definition ->
              List.for_all
                (fun constructor_fields ->
                  List.length
                    (List.filter
                       (fun (_, target) -> List.mem target owned_types)
                       (aggregate_targets
                          {
                            definition with
                            Sst.type_kind =
                              Sst.Record_definition constructor_fields;
                          }))
                  <= 1)
                (fields definition)
        in
        if List.for_all valid_shape owned_types then Ok ()
        else malformed candidate.constraint_location
    in
    let* () =
      let recursive_type type_id =
        List.exists
          (fun (_, target) -> can_reach [] target type_id)
          (Option.fold ~none:[] ~some:aggregate_targets
             (type_definition type_id))
      in
      let component type_id =
        List.filter
          (fun candidate ->
            can_reach [] type_id candidate && can_reach [] candidate type_id)
          owned_types
      in
      let component_has_ground_shape component =
        List.exists
          (fun type_id ->
            match type_definition type_id with
            | Some
                ({ Sst.type_kind = Sst.Variant_definition constructors; _ } as
                 definition :
                  Sst.type_definition) ->
                List.exists
                  (fun constructor ->
                    not
                      (List.exists
                         (fun (_, target) -> List.mem target component)
                         (aggregate_targets
                            {
                              definition with
                              Sst.type_kind =
                                Sst.Record_definition
                                  constructor.Sst.constructor_fields;
                            })))
                  constructors
            | Some { Sst.type_kind = Sst.Record_definition _; _ } | None ->
                false)
          component
      in
      if
        shared_invariant_state
        || Option.is_some frozen_spine_prerequisite
        || transitions = 0
      then Ok ()
      else if
        List.for_all
          (fun type_id ->
            (not (recursive_type type_id))
            || component_has_ground_shape (component type_id))
          owned_types
      then Ok ()
      else malformed candidate.constraint_location
    in
    let declaration_spans =
      span context candidate.signature_declaration.mtd_loc
      :: List.concat_map
           (fun (public, local) ->
             [
               span context public.typ_loc;
               span context local.declaration.typ_loc;
             ])
           candidate.public_type_links
    in
    let constraint_span = span context candidate.constraint_location in
    let update_definition definition =
      match
        List.find_opt
          (fun (_, local) -> local.type_id = definition.Sst.type_id)
          candidate.public_type_links
      with
      | None -> definition
      | Some (public, local) ->
          let owned_tree_prerequisite =
            if
              local.type_id = state_type && transitions > 0
              && (not shared_invariant_state)
              && Option.is_none frozen_spine_prerequisite
            then
              Some
                {
                  Sst.owned_root = state_type;
                  recursive_edges;
                  construction_authority =
                    Sst.Local_first_order_closed_construction;
                }
            else None
          in
          let evidence_id =
            Printf.sprintf "same-cmt:%d:%d:%d" local.type_id.type_index
              constraint_span.start_pos.line constraint_span.start_pos.column
          in
          {
            definition with
            Sst.representation =
              Sst.Abstract_with_evidence
                (issue_abstraction ~evidence_id
                   ~abstract_signature_type:local.type_id
                   ~hidden_implementation_type:local.type_id ~constraint_span
                   ~signature_module_identity:
                     {
                       Sst.source_name =
                         Ident.name candidate.signature_declaration.mtd_id;
                       resolved_identifier =
                         Ident.unique_name
                           candidate.signature_declaration.mtd_id;
                     }
                   ~implementation_module_identity:
                     {
                       Sst.source_name = candidate.module_name;
                       resolved_identifier =
                         Ident.unique_name candidate.module_id;
                     }
                   ~abstract_signature_type_identity:
                     {
                       Sst.source_name = public.typ_name.txt;
                       resolved_identifier = Ident.unique_name public.typ_id;
                     }
                   ~hidden_implementation_type_identity:
                     {
                       Sst.source_name = local.declaration.typ_name.txt;
                       resolved_identifier =
                         Ident.unique_name local.declaration.typ_id;
                     }
                   ~declaration_spans ~public_surface ~owned_tree_prerequisite
                   ~frozen_spine_prerequisite ~abstract_type_ids:public_type_ids
                   ~semantic_types:aggregates.definitions
                   ~semantic_functions:lowered_definitions);
          }
    in
    Ok
      {
        aggregates with
        definitions = List.map update_definition aggregates.definitions;
      }
  let issue_callable_instances structure program functions issued_domains =
    let rank_snapshot type_id =
      issued_domains
      |> List.find_map (fun domain ->
          if
            List.exists
              (fun identity -> identity.rank_type_id = type_id)
              (rank_component domain)
          then
            Some
              (String.concat "|"
                 [
                   rank_domain_id domain;
                   rank_domain_version domain;
                   rank_snapshot_digest domain;
                   Parametric_rank_domain_private.actual_arguments domain
                   |> List.map Parametric_type.to_string
                   |> String.concat ",";
                 ])
          else None)
    in
    let issued =
      functions
      |> List.filter_map (fun function_ ->
          let definition =
            List.find_opt
              (fun definition ->
                definition.Sst.function_id = function_.function_id)
              program.Sst.functions
          in
          let binding =
            match function_.value_binding.vb_pat.pat_desc with
            | Tpat_var (_, name, uid, _, _) -> Some (name.txt, compiler_uid uid)
            | _ -> None
          in
          match (definition, binding) with
          | Some definition, Some (leaf_name, binding_uid) ->
              let rank_snapshots =
                definition.parameters
                |> List.filter_map (function
                  | Sst.Callback_parameter _ -> None
                  | Sst.Value_parameter parameter -> (
                      match parameter.Sst.pattern.typ with
                      | Sst.Aggregate type_id ->
                          Option.map
                            (fun snapshot ->
                              Printf.sprintf "%s#%d=%s" type_id.type_name
                                type_id.type_index snapshot)
                            (rank_snapshot type_id)
                      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                      | Sst.Parameter _ | Sst.Application _ ->
                          None))
              in
              let profile_snapshot =
                match definition.type_binders with
                | [] -> "monomorphic-v1"
                | binders ->
                    "parametric-schema-v1:"
                    ^ String.concat ","
                        (List.map Parametric_type.binder_to_string binders)
              in
              Some
                {
                  callable_instance_token = callable_instance_issuer;
                  callable_instance_structure = structure;
                  callable_instance_program = program;
                  callable_instance_definition = definition;
                  callable_instance_source_paths = function_.resolved_paths;
                  callable_instance_binding_uid = binding_uid;
                  callable_instance_leaf_name = leaf_name;
                  callable_instance_profile_snapshot = profile_snapshot;
                  callable_instance_specialization_digest =
                    Digest.string
                      (String.concat "\000"
                         [
                           profile_snapshot; String.concat "\000" rank_snapshots;
                         ])
                    |> Digest.to_hex;
                  callable_instance_rank_snapshots = rank_snapshots;
                }
          | None, _ | _, None -> None)
    in
    let owners =
      List.filter_map
        (fun function_ ->
          match
            List.find_opt
              (fun definition ->
                definition.Sst.function_id = function_.function_id)
              program.Sst.functions
          with
          | Some definition ->
              Some
                ( function_.function_id,
                  definition.span,
                  function_.resolved_paths )
          | None -> None)
        functions
    in
    let callback_instances =
      match Sst_callback_private.local_callable_instances program ~owners with
      | Error message ->
          invalid_arg ("invalid local callback callable identity: " ^ message)
      | Ok instances ->
          List.map
            (fun (instance : Sst_callback_private.local_callable_instance) ->
              {
                callable_instance_token = callable_instance_issuer;
                callable_instance_structure = structure;
                callable_instance_program = program;
                callable_instance_definition = instance.definition;
                callable_instance_source_paths = instance.source_paths;
                callable_instance_binding_uid = instance.binding_uid;
                callable_instance_leaf_name =
                  instance.definition.function_id.function_name;
                callable_instance_profile_snapshot = "local-callback-v1";
                callable_instance_specialization_digest =
                  instance.relation_identity;
                callable_instance_rank_snapshots = [];
              })
            instances
    in
    let unordered = issued @ callback_instances in
    let issued =
      List.filter_map
        (fun definition ->
          List.find_opt
            (fun instance ->
              instance.callable_instance_definition.Sst.function_id
              = definition.Sst.function_id)
            unordered)
        program.Sst.functions
    in
    if List.length issued <> List.length unordered then
      invalid_arg "callable identity ordering lost an issued definition";
    issued_callable_instance_registry :=
      List.rev_append issued
        (List.filter
           (fun existing ->
             existing.callable_instance_structure != structure
             && existing.callable_instance_program != program)
           !issued_callable_instance_registry)
  let retained_shadow_type_aliases =
    Typedtree_callback_private.retained_shadow_type_aliases
  let retained_generic_shadow_aliases context function_ definition_body =
    match context.proof_capture_artifact with
    | Some artifact
      when authenticated_proof_capture_artifact context function_ artifact ->
        let source_parameters =
          Option.value ~default:[] (source_parameter_patterns function_)
          |> List.filter_map (fun pattern ->
              match pattern.pat_desc with
              | Tpat_var (_, name, _, _, _) ->
                  Some (name.txt, pattern.pat_loc, pattern.pat_type)
              | _ -> None)
        in
        let aliases = ref [] in
        let add_shadow parameter =
          match parameter.fp_kind with
          | Tparam_pat
              {
                pat_desc = Tpat_var (_, name, _, _, mode);
                pat_loc;
                pat_type;
                _;
              }
            when parameter.fp_partial = Total
                 && parameter.fp_arg_label = Nolabel
                 && uniqueness_of_pattern_mode mode = Sst.Definitely_aliased
            -> (
              match
                List.filter
                  (fun (source_name, source_location, _) ->
                    String.equal source_name name.txt
                    && same_location source_location pat_loc)
                  source_parameters
              with
              | [ (_, _, source_type) ] -> (
                  match
                    retained_shadow_type_aliases !aliases source_type pat_type
                  with
                  | Some updated -> aliases := updated
                  | None -> ())
              | [] | _ :: _ :: _ -> ())
          | Tparam_pat _ | Tparam_optional_default _ -> ()
        in
        let default = Tast_iterator.default_iterator in
        let iterator =
          {
            default with
            expr =
              (fun self expression ->
                (match expression.exp_desc with
                | Texp_sequence
                    (marker, _, { exp_desc = Texp_sequence (sidecar, _, _); _ })
                  -> (
                    match retained_ghost_pair context marker sidecar with
                    | Some retained ->
                        List.iter add_shadow retained.shadow_parameters
                    | None -> ())
                | Texp_sequence (marker, _, sidecar) -> (
                    match retained_ghost_pair context marker sidecar with
                    | Some retained ->
                        List.iter add_shadow retained.shadow_parameters
                    | None -> ())
                | _ -> ());
                default.expr self expression);
          }
        in
        iterator.expr iterator definition_body;
        List.sort_uniq
          (fun (left, _) (right, _) -> Int.compare left right)
          !aliases
    | Some _ | None -> []
  let prepare_builtin_assertion_plan context function_ definition_body =
    let malformed location =
      unsupported context location Diagnostic.Malformed_ghost_call
    in
    match context.proof_capture_artifact with
    | Some artifact
      when authenticated_proof_capture_artifact context function_ artifact ->
        let discoveries = ref [] in
        let retained_manifests = ref [] in
        let add_retained marker sidecar =
          match retained_proof_region_pair context marker sidecar with
          | Some retained
            when not (List.mem retained.proof_manifest_text !retained_manifests)
            -> (
              match
                parse_proof_capture_manifest retained.proof_manifest_text
              with
              | Some
                  ({ proof_capture_kind = Captured_local_assert _; _ } as
                   manifest) ->
                  retained_manifests :=
                    retained.proof_manifest_text :: !retained_manifests;
                  discoveries :=
                    Retained_assertion_discovery manifest :: !discoveries
              | Some { proof_capture_kind = Captured_proof_region; _ } | None ->
                  ())
          | Some _ | None -> ()
        in
        let default = Tast_iterator.default_iterator in
        let iterator =
          {
            default with
            expr =
              (fun self expression ->
                (match expression.exp_desc with
                | Texp_assert (predicate, keyword_location)
                  when not expression.exp_loc.loc_ghost ->
                    discoveries :=
                      Builtin_assertion_discovery
                        { source = expression; predicate; keyword_location }
                      :: !discoveries
                | Texp_sequence (marker, _, sidecar) -> (
                    add_retained marker sidecar;
                    match sidecar.exp_desc with
                    | Texp_sequence (nested_sidecar, _, _) ->
                        add_retained marker nested_sidecar
                    | _ -> ())
                | _ -> ());
                default.expr self expression);
          }
        in
        iterator.expr iterator definition_body;
        let location = function
          | Builtin_assertion_discovery { source; _ } -> source.exp_loc
          | Retained_assertion_discovery manifest ->
              {
                Location.loc_start =
                  {
                    definition_body.exp_loc.loc_start with
                    pos_cnum = manifest.proof_capture_region_start;
                  };
                loc_end =
                  {
                    definition_body.exp_loc.loc_end with
                    pos_cnum = manifest.proof_capture_region_end;
                  };
                loc_ghost = true;
              }
        in
        let ordered =
          List.sort
            (fun left right ->
              let left = location left and right = location right in
              let by_start =
                Int.compare left.loc_start.pos_cnum right.loc_start.pos_cnum
              in
              if by_start <> 0 then by_start
              else Int.compare left.loc_end.pos_cnum right.loc_end.pos_cnum)
            !discoveries
        in
        let rec build previous ordinal builtins = function
          | [] -> Ok (List.rev builtins)
          | discovery :: rest -> (
              let current = location discovery in
              let duplicate =
                Option.fold ~none:false
                  ~some:(fun previous ->
                    location_offsets previous = location_offsets current)
                  previous
              in
              if duplicate then malformed current
              else
                match discovery with
                | Retained_assertion_discovery
                    {
                      proof_capture_kind =
                        Captured_local_assert { assertion_ordinal; _ };
                      _;
                    } ->
                    if assertion_ordinal <> ordinal then malformed current
                    else build (Some current) (ordinal + 1) builtins rest
                | Retained_assertion_discovery
                    { proof_capture_kind = Captured_proof_region; _ } ->
                    assert false
                | Builtin_assertion_discovery
                    { source; predicate; keyword_location } ->
                    let planned =
                      {
                        builtin_source_expression = source;
                        builtin_predicate_expression = predicate;
                        builtin_keyword_location = keyword_location;
                        builtin_source_location = source.exp_loc;
                        builtin_predicate_location = predicate.exp_loc;
                        builtin_assertion_ordinal = ordinal;
                      }
                    in
                    build (Some current) (ordinal + 1) (planned :: builtins)
                      rest)
        in
        build None 0 [] ordered
    | Some _ | None -> Ok []
  let prepare_function_proof_captures ~source_file ~imports ~artifact functions
      =
    let prepare function_ =
      match proof_definition_body function_ with
      | None -> Ok function_
      | Some definition_body ->
          let context =
            {
              source_file;
              typing_environment = function_environment function_;
              proof_capture_artifact = artifact;
              broadcast_scan = None;
              symbolic_scan = None;
              symbolic_definitions = [];
              imports;
              functions;
              aggregates = empty_aggregates;
              current_function = Some function_;
              next_binding = 0;
              binding_types = [];
              binding_locations = [];
              proof_capture_type_hints = [];
              seen_ghost_ids = [];
              contract_carriers = [];
              logical_ghost_depth = 0;
              function_result_type = Some definition_body.exp_type;
              owned_tree_cursors = [];
              owned_tree_observation_paths = [];
              owned_tree_root_versions = [];
              shared_scalar_formal_roots = [];
              shared_scalar_aliases = [];
              shared_scalar_next_epoch = 0;
              owned_tree_candidate_roots = [];
              imported = empty_imported_environment;
              callbacks = empty_callback_lowering_state ();
            }
          in
          let* () = prepare_proof_capture_type_hints context definition_body in
          let source_patterns = ref [] in
          let default = Tast_iterator.default_iterator in
          let iterator =
            {
              default with
              pat =
                (fun (type k) self (pattern : k general_pattern) ->
                  if not pattern.pat_loc.loc_ghost then
                    source_patterns :=
                      (pattern.pat_loc, pattern.pat_type) :: !source_patterns;
                  default.pat self pattern);
            }
          in
          iterator.expr iterator function_.value_binding.vb_expr;
          let substitutions =
            context.proof_capture_type_hints
            |> List.filter_map (fun (location, replacement) ->
                List.find_map
                  (fun (source_location, source_type) ->
                    if same_location location source_location then
                      match Types.get_desc source_type with
                      | Types.Tvar _ | Types.Tunivar _ ->
                          Some (Types.get_id source_type, replacement)
                      | _ -> None
                    else None)
                  !source_patterns)
            |> List.sort_uniq (fun (left, _) (right, _) ->
                Int.compare left right)
          in
          let retained_aliases =
            retained_generic_shadow_aliases context function_ definition_body
          in
          let* builtin_assertions =
            prepare_builtin_assertion_plan context function_ definition_body
          in
          Ok
            {
              function_ with
              type_substitutions =
                substitutions @ retained_aliases @ function_.type_substitutions;
              proof_capture_hints = context.proof_capture_type_hints;
              builtin_assertions;
            }
    in
    let rec loop prepared = function
      | [] -> Ok (List.rev prepared)
      | function_ :: rest ->
          let* function_ = prepare function_ in
          loop (function_ :: prepared) rest
    in
    loop [] functions
  let bind_builtin_assertion_candidates authenticated functions =
    let program_snapshot = lazy (Sst.to_string authenticated) in
    List.iter
      (fun lowered ->
        if
          lowered.lowered_builtin_token == lowered_builtin_issuer
          && List.exists
               (fun function_ -> function_ == lowered.lowered_builtin_function)
               functions
        then
          match
            List.find_opt
              (fun definition ->
                definition.Sst.function_id
                = lowered.lowered_builtin_function.function_id)
              authenticated.Sst.functions
          with
          | None -> ()
          | Some definition -> (
              let root =
                match definition.Sst.body with
                | Sst.Checked_exec { body; _ } | Sst.Proof_body { body; _ } ->
                    Some body.expression
                | Sst.Spec_definition body
                | Sst.Recursive_spec_definition { body; _ } ->
                    Some body.expression
                | Sst.External_specification _
                | Sst.Trusted_external_spec_target _
                | Sst.Trusted_external_body _
                | Sst.Symbolic_declaration _ ->
                    None
              in
              let rec find parent found expression =
                let found =
                  match expression.Sst.expression_desc with
                  | Sst.Local_assert { assertion_ordinal; _ }
                    when assertion_ordinal
                         = lowered.lowered_builtin_source
                             .builtin_assertion_ordinal
                         && expression.span
                            = lowered.lowered_builtin_expression.span ->
                      (expression, parent) :: found
                  | _ -> found
                in
                match expression.Sst.expression_desc with
                | Sst.Proof_region body -> find (Some expression) found body
                | _ ->
                    List.fold_left (find parent) found
                      (recursive_helper_expression_children expression)
              in
              match
                Option.map (find None []) root |> Option.value ~default:[]
              with
              | [ (expression, parent_region) ] ->
                  let planned = lowered.lowered_builtin_source in
                  pending_builtin_assertions :=
                    {
                      pending_builtin_token = pending_builtin_issuer;
                      pending_builtin_program = authenticated;
                      pending_builtin_definition = definition;
                      pending_builtin_expression = expression;
                      pending_builtin_parent_region = parent_region;
                      pending_builtin_source =
                        Compiler_builtin_assertion
                          {
                            builtin_source_expression =
                              planned.builtin_source_expression;
                            builtin_predicate_expression =
                              planned.builtin_predicate_expression;
                            builtin_keyword_location =
                              planned.builtin_keyword_location;
                            builtin_source_location =
                              planned.builtin_source_location;
                            builtin_predicate_location =
                              planned.builtin_predicate_location;
                            builtin_assertion_ordinal =
                              planned.builtin_assertion_ordinal;
                          };
                      pending_builtin_program_snapshot =
                        Lazy.force program_snapshot;
                    }
                    :: !pending_builtin_assertions
              | [] | _ :: _ :: _ -> ()))
      !lowered_builtin_assertions
  let broadcast_sources functions =
    List.map
      (fun function_ -> (function_.value_binding, function_.function_id))
      functions
  let lower_with_imports_unscoped ?(allow_imported_opens = false)
      ?(explicit_interface = false) ?(interface_value_paths = [])
      ?(imported = empty_imported_environment) ?proof_capture_artifact
      ?compilation_identity ?authenticated_source_text
      ?(load_path_visible = [ Config.standard_library ])
      ?(load_path_hidden = []) ~source_file ~imports structure =
    let* symbolic_scan =
      match
        Typedtree_symbolic_private.authenticate ~source_file
          ~artifact:proof_capture_artifact ~compilation_identity
          ~resolves_to_spec_carrier:(resolves_to_spec_carrier imports)
          structure
      with
      | Ok scan -> Ok scan
      | Error { location; message } ->
          Error
            (Diagnostic.make
               (Diagnostic.Invalid_symbolic_authentication message)
               (Diagnostic.span_of_location ~fallback_file:source_file
                  location))
    in
    let* broadcast_scan =
      Broadcast.authenticate_typedtree
        ~imported_declaration_paths:
          (imported.imported_callables
          |> List.filter_map (fun callable ->
                 Option.map (Fun.const callable.imported_path)
                   callable.imported_broadcast_trigger_span))
        ~imported_group_paths:
          (List.map
             (fun group -> group.imported_broadcast_group_path)
             imported.imported_broadcast_groups)
        ~source_file ~imports
        ~artifact:proof_capture_artifact structure
    in
    match
      collect_structure source_file imports allow_imported_opens broadcast_scan
        symbolic_scan structure
    with
    | Error _ as error -> error
    | Ok (local_types, functions, constrained_modules) -> (
        let* functions =
          prepare_function_proof_captures ~source_file ~imports
            ~artifact:proof_capture_artifact functions
        in
        let* () =
          reject_public_generic_functions
            ~allow_root_parametric:imported.allow_public_parametric_signatures
            ~explicit_interface ~interface_value_paths source_file functions
            constrained_modules
        in
        let constrained_type_ids =
          constrained_modules
          |> List.concat_map (fun candidate ->
              List.map
                (fun (_, local) -> local.type_id)
                candidate.public_type_links)
        in
        let rank_analysis_types =
          List.filter
            (fun local ->
              external_type_specification local.declaration
                <> Authenticated_external_type
              && (not (List.mem local.type_id constrained_type_ids))
              && not
                   (List.exists
                      (fun candidate ->
                        String.starts_with
                          ~prefix:(candidate.module_name ^ ".")
                          local.type_id.type_name
                        && (constrained_module_has_private_opaque_recursive_spec
                              candidate functions
                           || (not
                                 (constrained_module_has_finite_signature
                                    candidate))
                           || List.exists
                                (fun (_, field) -> field.source_field_mutable)
                                (rank_source_fields local)))
                      constrained_modules))
            local_types
        in
        let rank_local_types =
          List.filter
            (fun local -> local.declaration.typ_params = [])
            rank_analysis_types
        in
        (* Exact parametric ADT applications are instantiated lazily from the
         canonical descriptor registry during VC translation.  Source
         observation never manufactures aggregate members. *)
        let* functions, constrained_modules =
          parameterize_functions source_file broadcast_scan functions
            constrained_modules
        in
        let* pending_rank_domains =
          analyze_recursive_rank_domains source_file rank_analysis_types
            rank_local_types
        in
        let* linkages =
          resolve_external_linkages source_file imports imported functions
        in
        let imported_parametric_adts =
          imported.imported_types
          |> List.filter_map (fun imported ->
                 imported.imported_parametric_descriptor)
        in
        match
          lower_type_registry source_file imports structure.str_final_env
            ~imported_parametric_adts
            ~load_path_visible
            ~load_path_hidden ~proof_capture_artifact local_types
        with
        | Error _ as error -> error
        | Ok aggregates ->
            let aggregates =
              {
                aggregates with
                ranked_types =
                  List.concat_map
                    (fun domain ->
                      List.map
                        (fun identity -> identity.rank_type_id)
                        domain.pending_component)
                    pending_rank_domains
                  |> List.sort_uniq compare;
              }
            in
            let owned_tree_candidate_roots =
              constrained_modules
              |> List.concat_map (fun candidate ->
                  List.map
                    (fun (_, local) -> local.type_id)
                    candidate.public_type_links)
              |> List.sort_uniq compare
            in
            let* symbolic_definitions =
              lower_symbolic_definitions source_file imports aggregates
                functions owned_tree_candidate_roots imported
                proof_capture_artifact (Some broadcast_scan) symbolic_scan
            in
            let* external_definitions =
              let rec loop definitions = function
                | [] -> Ok definitions
                | linkage :: rest -> (
                    match linkage.target with
                    | Same_unit_external_target _ ->
                        let* wrapper, target =
                          lower_external_specification source_file imports
                            aggregates functions proof_capture_artifact
                            symbolic_scan symbolic_definitions linkage
                        in
                        loop
                          ((wrapper.Sst.function_id.function_index, wrapper)
                          :: (target.Sst.function_id.function_index, target)
                          :: definitions)
                          rest
                    | Imported_unverified_external_target _ ->
                        let* wrapper =
                          lower_external_target_specification source_file
                            imports aggregates functions proof_capture_artifact
                            symbolic_scan symbolic_definitions imported linkage
                        in
                        loop
                          ((wrapper.Sst.function_id.function_index, wrapper)
                          :: definitions)
                          rest)
              in
              loop [] linkages
            in
            let rec lower_functions lowered = function
              | [] -> Ok (List.rev lowered)
                | function_ :: rest -> (
                  match
                    ( List.assoc_opt function_.function_id.function_index
                        symbolic_definitions,
                      List.assoc_opt function_.function_id.function_index
                        external_definitions )
                  with
                  | Some definition, _ ->
                      lower_functions (definition :: lowered) rest
                  | None, Some definition ->
                      lower_functions (definition :: lowered) rest
                  | None, None -> (
                      match
                        lower_function ?compilation_identity
                          authenticated_source_text source_file imports
                          aggregates functions owned_tree_candidate_roots
                          imported proof_capture_artifact
                          (Some broadcast_scan) symbolic_scan
                          symbolic_definitions function_
                      with
                      | Error _ as error -> error
                      | Ok (definition, local_definitions) ->
                          lower_functions
                            (List.rev_append
                               (definition :: local_definitions)
                               lowered)
                            rest))
            in
            let* lowered_definitions = lower_functions [] functions in
            let* () =
              match
                List.find_map
                  (fun (definition : Sst.function_definition) ->
                    if definition.mode <> Sst.Exec then None
                    else
                      match
                        Callback_contract_private.validate_explicit
                          definition.contracts
                      with
                      | Ok () -> None
                      | Error message ->
                          List.find_map
                            (function
                              | Sst.Callback_parameter { binding; _ } ->
                                  Some (binding.callback_span, message)
                              | Sst.Value_parameter _ -> None)
                            definition.parameters)
                  lowered_definitions
              with
              | None -> Ok ()
              | Some (callback_span, message) ->
                  Error
                    (Diagnostic.make
                       (Diagnostic.Unsupported_construct
                          Diagnostic.Callback_contract) callback_span
                    |> Diagnostic.with_message message)
            in
            let imported_parametric_adts =
              imported.imported_types
              |> List.filter_map (fun imported ->
                  imported.imported_parametric_descriptor)
            in
            let local_parametric_adts =
              List.map
                (fun item ->
                  item.Parametric_adt_lowering_private.descriptor)
                aggregates.parametric_adts
            in
            let normalized =
              Sst_normalize.classify_typedtree_program
                {
                  Sst.policy = Sst.Default_linear_z3;
                  parametric_adts =
                    local_parametric_adts @ imported_parametric_adts;
                  Sst.types =
                    aggregates.definitions
                    @ List.map
                        (fun imported -> imported.imported_type_definition)
                        imported.imported_types;
                  functions = lowered_definitions;
                }
            in
            let* aggregates =
              let rec authenticate aggregates = function
                | [] -> Ok aggregates
                | candidate :: rest ->
                    let* aggregates =
                      authenticate_constrained_module source_file imports
                        aggregates functions normalized.functions candidate
                    in
                    authenticate aggregates rest
              in
              authenticate aggregates constrained_modules
            in
            let authenticated =
              {
                normalized with
                Sst.types =
                  aggregates.definitions
                  @ List.map
                      (fun imported -> imported.imported_type_definition)
                      imported.imported_types;
              }
            in
            let* () =
              let imported_definitions =
                imported.imported_callables
                |> List.filter_map (fun imported ->
                       if
                         Symbolic_declaration_private.is_symbolic
                           imported.imported_definition
                       then Some imported.imported_definition
                       else None)
              in
              Symbolic_declaration_private.seal ~program:authenticated
                ~imported_definitions
              |> Result.map_error (fun message ->
                     Diagnostic.make
                       (Diagnostic.Invalid_symbolic_authentication message)
                       (Diagnostic.file_span source_file))
            in
            let* () =
              Broadcast.register ~source_file
                ~scan:broadcast_scan ~program:authenticated
                ~sources:(broadcast_sources functions)
                ~imported_declarations:
                  (imported.imported_callables
                  |> List.filter_map (fun callable ->
                         Option.map
                           (fun trigger_span ->
                             {
                               Broadcast.imported_path = callable.imported_path;
                               imported_definition =
                                 callable.imported_definition;
                               imported_trigger_span = trigger_span;
                             })
                           callable.imported_broadcast_trigger_span))
                ~imported_groups:
                  (List.map
                     (fun group ->
                       {
                         Broadcast.imported_group_path =
                           group.imported_broadcast_group_path;
                         imported_target_paths =
                           group.imported_broadcast_target_paths;
                       })
                     imported.imported_broadcast_groups)
            in
            let imported_application_model callee =
              List.exists
                (fun imported ->
                  let definition = imported.imported_definition in
                  definition.Sst.function_id = callee
                  &&
                  match
                    ( definition.mode,
                      definition.recursive,
                      definition.body,
                      definition.parameters,
                      definition.result_type )
                  with
                  | ( Sst.Spec,
                      false,
                      Sst.Spec_definition _,
                      [
                        Sst.Value_parameter
                          {
                            pattern = { typ = Sst.Aggregate _; _ };
                            label = None;
                            _;
                          };
                      ],
                      (Sst.Application _ as application) ) ->
                      List.exists
                        (fun descriptor ->
                          Parametric_adt.same_application descriptor application)
                        imported_parametric_adts
                  | _ -> false)
                imported.imported_callables
            in
            let rec uses_other_imported_call expression =
              let current =
                match expression.Sst.expression_desc with
                | Sst.Direct_call { callee; _ } ->
                    List.exists
                      (fun imported ->
                        imported.imported_definition.Sst.function_id = callee)
                      imported.imported_callables
                    && not (imported_application_model callee)
                | _ -> false
              in
              current
              || List.exists uses_other_imported_call
                   (recursive_helper_expression_children expression)
            in
            let definition_roots definition =
              let predicate (clause : Sst.predicate_clause) =
                clause.Sst.predicate.expression
              in
              let roots =
                List.map predicate definition.Sst.contracts.requires
                @ List.map
                    (fun clause -> clause.Sst.predicate.expression)
                    definition.contracts.ensures
                @ List.map predicate definition.contracts.decreases
                @ List.map predicate definition.contracts.assertions
              in
              match definition.body with
              | Sst.Checked_exec { body; _ }
              | Sst.Proof_body { body; _ }
              | Sst.Spec_definition body
              | Sst.Recursive_spec_definition { body; _ } ->
                  body.expression :: roots
              | Sst.External_specification _
              | Sst.Trusted_external_spec_target _
              | Sst.Trusted_external_body _
              | Sst.Symbolic_declaration _ ->
                  roots
            in
            let imported_rank_domains =
              if
                List.exists
                  (fun definition ->
                    List.exists uses_other_imported_call
                      (definition_roots definition))
                  authenticated.Sst.functions
              then imported.imported_rank_domains
              else []
            in
            let issued_domains =
              issue_rank_domains authenticated
                (pending_rank_domains
                @ List.map
                    (fun imported ->
                      {
                        pending_component =
                          List.map
                            (fun (type_id, path, uid, span) ->
                              {
                                rank_type_id = type_id;
                                rank_path = path;
                                rank_uid = uid;
                                rank_span = span;
                              })
                            imported.imported_rank_component;
                        pending_positive_children =
                          List.map
                            (fun ( constructor,
                                   constructor_uid,
                                   field,
                                   field_uid,
                                   child_path,
                                   child_type,
                                   expansion_trace ) ->
                              {
                                rank_constructor = constructor;
                                rank_constructor_uid = constructor_uid;
                                rank_field = field;
                                rank_field_uid = field_uid;
                                rank_child_path = child_path;
                                rank_child_type = child_type;
                                rank_expansion_trace = expansion_trace;
                              })
                            imported.imported_rank_positive_children;
                        pending_ground_witnesses =
                          List.map
                            (fun (constructor, uid) ->
                              {
                                rank_ground_constructor = constructor;
                                rank_ground_constructor_uid = uid;
                              })
                            imported.imported_rank_ground_witnesses;
                        pending_actual_evidence =
                          imported.imported_rank_actual_evidence;
                      })
                    imported_rank_domains)
            in
            bind_builtin_assertion_candidates authenticated functions;
            Option.iter
              (fun artifact ->
                List.iter
                  (fun issued ->
                    if
                      issued.proof_capture_token == proof_capture_issuer
                      && issued.proof_capture_artifact == artifact
                      && Option.is_none issued.proof_capture_program
                    then
                      match
                        List.find_opt
                          (fun definition ->
                            definition.Sst.function_id
                            = issued.proof_capture_function.function_id)
                          authenticated.Sst.functions
                      with
                      | None -> ()
                      | Some definition -> (
                          issued.proof_capture_program <- Some authenticated;
                          let root =
                            match definition.Sst.body with
                            | Sst.Checked_exec { body; _ }
                            | Sst.Proof_body { body; _ } ->
                                Some body.expression
                            | Sst.Spec_definition _
                            | Sst.Recursive_spec_definition _
                            | Sst.External_specification _
                            | Sst.Trusted_external_spec_target _
                            | Sst.Trusted_external_body _
                            | Sst.Symbolic_declaration _ ->
                                None
                          in
                          match
                            issued.proof_capture_manifest.proof_capture_kind
                          with
                          | Captured_proof_region -> (
                              match (root, issued.proof_capture_expression) with
                              | Some root, Some captured -> (
                                  let rec collect found expression =
                                    let found =
                                      match expression.Sst.expression_desc with
                                      | Sst.Proof_region _
                                        when expression.span = captured.span ->
                                          expression :: found
                                      | _ -> found
                                    in
                                    List.fold_left collect found
                                      (recursive_helper_expression_children
                                         expression)
                                  in
                                  match collect [] root with
                                  | [ expression ] ->
                                      issued_proof_regions :=
                                        {
                                          proof_region_token =
                                            proof_region_issuer;
                                          proof_region_program = authenticated;
                                          proof_region_definition = definition;
                                          proof_region_expression = expression;
                                          proof_region_manifest =
                                            issued.proof_capture_manifest;
                                        }
                                        :: !issued_proof_regions
                                  | [] | _ :: _ :: _ -> ())
                              | _ -> ())
                          | Captured_local_assert
                              { assertion_ordinal = ordinal; _ } -> (
                              let rec collect parent found expression =
                                let found =
                                  match expression.Sst.expression_desc with
                                  | Sst.Local_assert { assertion_ordinal; _ }
                                    when assertion_ordinal = ordinal ->
                                      (expression, parent) :: found
                                  | _ -> found
                                in
                                match expression.Sst.expression_desc with
                                | Sst.Proof_region body ->
                                    collect (Some expression) found body
                                | _ ->
                                    List.fold_left (collect parent) found
                                      (recursive_helper_expression_children
                                         expression)
                              in
                              match
                                Option.map (collect None []) root
                                |> Option.value ~default:[]
                              with
                              | [ (expression, parent_region) ]
                                when definition.mode = Sst.Proof
                                     && Option.is_none parent_region
                                     || definition.mode = Sst.Exec
                                        && Option.is_some parent_region ->
                                  issued_local_assertions :=
                                    {
                                      local_assertion_token =
                                        local_assertion_issuer;
                                      local_assertion_program = authenticated;
                                      local_assertion_definition = definition;
                                      local_assertion_expression = expression;
                                      local_assertion_parent_region =
                                        parent_region;
                                      local_assertion_source =
                                        Retained_ppx_assertion
                                          issued.proof_capture_manifest;
                                    }
                                    :: !issued_local_assertions;
                                  incr local_assertion_static_issuance_count
                              | [] | [ _ ] | _ :: _ :: _ -> ())))
                  !issued_proof_captures)
              proof_capture_artifact;
            issue_recursive_helper_certificates
              ~local_type_ids:
                (List.map
                   (fun definition -> definition.Sst.type_id)
                   aggregates.definitions)
              authenticated functions;
            issue_callable_instances structure authenticated functions
              issued_domains;
            List.iter
              (fun pending ->
                if
                  pending.pending_builtin_token == pending_builtin_issuer
                  && pending.pending_builtin_program == authenticated
                then
                  match pending.pending_builtin_expression.expression_desc with
                  | Sst.Local_assert { predicate; _ } ->
                      Instance_mode.register_builtin_local_assertion
                        ~program:authenticated
                        ~definition:pending.pending_builtin_definition
                        ~expression:pending.pending_builtin_expression
                        ~predicate
                        ~direct_exec:
                          (pending.pending_builtin_definition.mode = Sst.Exec
                          && Option.is_none
                               pending.pending_builtin_parent_region)
                  | _ -> ())
              !pending_builtin_assertions;
            Instance_mode.prepare ~structure ~program:authenticated;
            Finite_formal_requirement.prepare ~structure ~program:authenticated;
            Option.iter
              (fun _ ->
                let issued_program = Weak.create 1 in
                Weak.set issued_program 0 (Some authenticated);
                let issued_function_snapshots =
                  List.map
                    (fun definition ->
                      ( definition.Sst.function_id,
                        shared_scalar_function_snapshot authenticated definition
                      ))
                    authenticated.Sst.functions
                in
                issued_shared_scalar_programs :=
                  { issued_program; issued_function_snapshots }
                  :: !issued_shared_scalar_programs)
              authenticated_source_text;
            Ok authenticated)
  let lower_with_imports ?(allow_imported_opens = false)
      ?(explicit_interface = false) ?(interface_value_paths = [])
      ?(imported = empty_imported_environment) ?proof_capture_artifact
      ?compilation_identity ?authenticated_source_text
      ?(load_path_visible = [ Config.standard_library ])
      ?(load_path_hidden = []) ~source_file ~imports structure =
    Parametric_adt_lowering_private.with_load_path
      ~visible:load_path_visible ~hidden:load_path_hidden (fun () ->
        lower_with_imports_unscoped ~allow_imported_opens ~explicit_interface
          ~interface_value_paths ~imported ?proof_capture_artifact
          ?compilation_identity ?authenticated_source_text ~load_path_visible
          ~load_path_hidden ~source_file ~imports structure)
  let lower_with_capture_artifact ?allow_imported_opens ~proof_capture_artifact
      ?authenticated_source_text ?compilation_identity ~source_file ~imports
      structure =
    lower_with_imports ?allow_imported_opens ~proof_capture_artifact
      ?authenticated_source_text ?compilation_identity ~source_file ~imports
      structure
  let lower ?allow_imported_opens ~source_file ~imports structure =
    lower_with_imports ?allow_imported_opens ~source_file ~imports structure
  module Proof_region_capture_for_testing = struct
    type counters = {
      capture_issuances : int;
      capture_remappings : int;
      proof_region_sst_nodes : int;
    }
    type observation = {
      callable : Sst.function_id;
      region_start : int;
      region_end : int;
      binding_id : int;
      binding_name : string;
      binding_span : Diagnostic.span;
      binding_uniqueness : Sst.uniqueness;
      incoming_mode : Sst.instance_mode;
      synthetic_ghost_descriptors : int;
      synthetic_tracked_descriptors : int;
    }
    let reset () =
      proof_capture_issuance_count := 0;
      proof_capture_remapping_count := 0;
      proof_capture_sst_count := 0;
      local_assertion_sst_count := 0;
      local_assertion_static_issuance_count := 0;
      issued_local_assertions := [];
      pending_builtin_assertions := [];
      lowered_builtin_assertions := [];
      issued_proof_regions := [];
      issued_proof_captures := []
    let counters () =
      {
        capture_issuances = !proof_capture_issuance_count;
        capture_remappings = !proof_capture_remapping_count;
        proof_region_sst_nodes = !proof_capture_sst_count;
      }
    let observations program modes =
      !issued_proof_captures
      |> List.filter (fun issued ->
          issued.proof_capture_token == proof_capture_issuer
          && issued.proof_capture_manifest.proof_capture_kind
             = Captured_proof_region
          &&
          match issued.proof_capture_program with
          | Some candidate -> candidate == program
          | None -> false)
      |> List.rev
      |> List.concat_map (fun issued ->
          List.map
            (fun binding ->
              {
                callable = issued.proof_capture_function.function_id;
                region_start =
                  issued.proof_capture_manifest.proof_capture_region_start;
                region_end =
                  issued.proof_capture_manifest.proof_capture_region_end;
                binding_id = binding.Sst.id;
                binding_name = binding.name;
                binding_span = binding.span;
                binding_uniqueness = binding.uniqueness;
                incoming_mode =
                  Instance_mode.binding_mode modes
                    issued.proof_capture_function.function_id binding;
                synthetic_ghost_descriptors = 0;
                synthetic_tracked_descriptors = 0;
              })
            issued.proof_capture_bindings)
  end
  module Local_assertion_for_testing = struct
    type counters = { static_issuances : int; reached_sst_nodes : int }
    let counters () =
      {
        static_issuances = !local_assertion_static_issuance_count;
        reached_sst_nodes = !local_assertion_sst_count;
      }
  end
end
