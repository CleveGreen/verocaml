open Interface_specification_environment_private
open Interface_specification_candidate_private

let ( let* ) = Result.bind

type error = Interface_specification_environment_private.error

type result = {
  driver : Verification_driver_private.report;
  numeric_registry : Numeric_ghost_registry_private.t option;
  numeric_target_sets : Numeric_required_target_set_private.t list;
  provenance :
    (string * string * (string * string) list * (string * string) list) list;
}

let public_types_export_external_type_specification types =
  List.exists
    (fun (typ : public_type) ->
      match typ.parametric_descriptor with
      | Some descriptor -> (
          match Parametric_adt.provenance descriptor with
          | Parametric_adt.External _ -> true
          | Parametric_adt.Local _ -> false)
      | None -> false)
    types

let snapshot_clause clause =
  {
    clause_index = Sst_validation.contract_clause_index clause;
    clause_span = Sst_validation.contract_clause_span clause;
    clause_expression = Sst_validation.contract_clause_expression clause;
  }

let snapshot_callable validated descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  let contract = Sst_validation.callable_contract descriptor in
  {
    definition;
    callable_id = definition.Sst.function_id;
    callable_mode = definition.mode;
    parameter_types =
      List.map
        (fun parameter ->
          (Sst.require_value_parameter parameter).Sst.pattern.typ)
        definition.parameters;
    parameter_modes =
      List.mapi
        (fun index parameter ->
          Sst_validation.formal_instance_mode validated descriptor index
            parameter)
        definition.parameters;
    finite_formals =
      List.mapi
        (fun index _ ->
          if
            Sst_validation.formal_requires_finite validated descriptor index
          then Some index
          else None)
        definition.parameters
      |> List.filter_map Fun.id;
    result_type = definition.result_type;
    result_mode = Sst_validation.result_instance_mode validated descriptor;
    contract =
      {
        requires =
          List.map snapshot_clause (Sst_validation.contract_requires contract);
        ensures =
          List.map snapshot_clause (Sst_validation.contract_ensures contract);
        decreases =
          List.map
            (fun decrease ->
              snapshot_clause (Sst_validation.decrease_clause decrease))
            (Sst_validation.contract_decreases contract);
      };
  }

let type_name descriptor =
  (Sst_validation.type_id descriptor).Sst.type_name


let public_linked_type descriptor =
  let name = type_name descriptor in
  (not (String.contains name '.'))
  ||
  match
    Sst_validation.visibility_representation
      (Sst_validation.type_visibility descriptor)
  with
  | Sst.Abstract_with_evidence _ -> true
  | Sst.Revealed -> false

let callable_name descriptor =
  (Sst_validation.callable_id descriptor).Sst.function_name

let surface_callable_name descriptor =
  callable_name descriptor

let surface_type_name descriptor =
  type_name descriptor

let missing_public_type surface descriptors =
  List.find_opt
    (fun name ->
      let matching =
        List.filter
          (fun descriptor -> String.equal (surface_type_name descriptor) name)
          descriptors
      in
      match matching with
      | [] -> true
      | _ ->
          let exact_names =
            List.map type_name matching |> List.sort_uniq String.compare
          in
          List.length exact_names <> List.length matching)
    surface.public_type_names

let missing_public_callable surface descriptors =
  List.find_opt
    (fun name ->
      let matching =
        List.filter
          (fun descriptor -> String.equal (surface_callable_name descriptor) name)
          descriptors
      in
      match matching with
      | [] -> true
      | _ ->
          let exact_names =
            List.map callable_name matching |> List.sort_uniq String.compare
          in
          List.length exact_names <> List.length matching)
    surface.public_callable_names

let validate_surface_completeness ~unit_name surface type_descriptors
    callable_descriptors =
  match
    ( missing_public_type surface type_descriptors,
      missing_public_callable surface callable_descriptors )
  with
  | Some name, _ ->
      error ~unit_name
        (Printf.sprintf
           "embedded public type %s has no exact verified descriptor" name)
  | _, Some name ->
      error ~unit_name
        (Printf.sprintf
           "embedded public callable %s has no exact verified descriptor" name)
  | None, None -> Ok ()

let snapshot_type surface validated descriptor =
  let definition = Sst_validation.type_definition descriptor in
  let parametric_descriptor =
    (Sst_validation.program validated).Sst.parametric_adts
    |> List.find_opt (fun candidate ->
           Parametric_adt.type_id candidate = definition.Sst.type_id)
  in
  let external_specification =
    Option.fold ~none:false
      ~some:(fun descriptor ->
        match Parametric_adt.provenance descriptor with
        | Parametric_adt.External _ -> true
        | Parametric_adt.Local _ -> false)
      parametric_descriptor
  in
  match
    Sst_validation.visibility_representation
      (Sst_validation.type_visibility descriptor),
    surface_reveals_type surface definition.type_id || external_specification
  with
  | Sst.Revealed, true ->
      {
        definition;
        parametric_descriptor;
        type_id = definition.type_id;
        source_name = definition.Sst.type_id.type_name;
        visibility = Revealed;
        kind = Some definition.type_kind;
        logical = Option.is_some (Sst_validation.type_logical_type descriptor);
        field_modes =
          (match definition.type_kind with
          | Sst.Record_definition fields ->
              List.map
                (fun field ->
                  ( field.Sst.field_id,
                    Sst_validation.field_instance_mode validated field ))
                fields
          | Sst.Variant_definition constructors ->
              List.concat_map
                (fun constructor ->
                  List.map
                    (fun field ->
                      ( field.Sst.field_id,
                        Sst_validation.field_instance_mode validated field ))
                    constructor.Sst.constructor_fields)
                constructors);
      }
  | Sst.Revealed, false | Sst.Abstract_with_evidence _, _ ->
      {
        definition;
        parametric_descriptor;
        type_id = definition.type_id;
        source_name = definition.Sst.type_id.type_name;
        visibility = Abstract;
        kind = None;
        logical = false;
        field_modes = [];
      }

let snapshot_types ~unit_name surface validated descriptors =
  let exported =
    List.filter
      (fun descriptor -> surface_has_type surface (Sst_validation.type_id descriptor))
      descriptors
  in
  match
    List.find_opt
      (fun descriptor ->
        let definition = Sst_validation.type_definition descriptor in
        let binders =
          (Sst_validation.program validated).Sst.parametric_adts
          |> List.find_opt (fun candidate ->
                 Parametric_adt.type_id candidate = definition.Sst.type_id)
          |> Option.map Parametric_adt.binders
          |> Option.value ~default:[]
        in
        let external_specification =
          (Sst_validation.program validated).Sst.parametric_adts
          |> List.find_opt (fun candidate ->
                 Parametric_adt.type_id candidate = definition.Sst.type_id)
          |> Option.fold ~none:false ~some:(fun descriptor ->
                 match Parametric_adt.provenance descriptor with
                 | Parametric_adt.External _ -> true
                 | Parametric_adt.Local _ -> false)
        in
        match
          Sst_validation.visibility_representation
            (Sst_validation.type_visibility descriptor),
          surface_reveals_type surface definition.type_id
          || external_specification
        with
        | Sst.Revealed, true ->
            (not external_specification)
            && not
              (surface_type_kind_is_public surface binders
                 definition.type_kind)
        | Sst.Revealed, false | Sst.Abstract_with_evidence _, _ -> false)
      exported
  with
  | Some descriptor ->
      error ~unit_name
        (Printf.sprintf "public type %s contains a non-public identity"
           (type_name descriptor))
  | None ->
      exported
      |> List.sort (fun left right ->
             let left = Sst_validation.type_id left
             and right = Sst_validation.type_id right in
             match String.compare left.type_name right.type_name with
             | 0 -> Int.compare left.type_index right.type_index
             | comparison -> comparison)
      |> List.map (snapshot_type surface validated)
      |> Result.ok

let snapshot_models surface validated callables =
  let find_callable function_id =
    List.find_opt
      (fun callable -> same_function_id callable.callable_id function_id)
      callables
  in
  let model_descriptor descriptor =
    let callable_id =
      Sst_validation.model_callable descriptor |> Sst_validation.callable_id
    in
    if not (public_function surface callable_id) then None
    else
      let domain =
        Sst_validation.model_domain descriptor |> Sst_validation.type_id
      in
      let result_type =
        Sst_validation.model_result descriptor
        |> Sst_validation.logical_source_type
      in
      match find_callable callable_id with
      | Some callable
        when surface_has_type surface domain
             && public_typ surface [] result_type ->
          Some { callable; domain; result_type }
      | Some _ | None -> None
  in
  let descriptors = Sst_validation.model_descriptors validated in
  let public_descriptors =
    List.filter
      (fun descriptor ->
        Sst_validation.model_callable descriptor
        |> Sst_validation.callable_id
        |> public_function surface)
      descriptors
  in
  let models = List.filter_map model_descriptor descriptors in
  if List.length models = List.length public_descriptors then Some models else None

let snapshot_callables_and_models ~unit_name ~imported_specification_call surface validated descriptors =
  let exported =
    List.filter
      (fun descriptor ->
        public_function surface (Sst_validation.callable_id descriptor))
      descriptors
  in
  match
    List.find_opt
      (fun descriptor -> not (public_callable_descriptor ~imported_specification_call surface descriptor))
      exported
  with
  | Some descriptor ->
      error ~unit_name
        (Printf.sprintf "public callable %s contains a non-public identity"
           (callable_name descriptor))
  | None ->
      let callables = List.map (snapshot_callable validated) exported in
      (match snapshot_models surface validated callables with
      | Some models -> Ok (callables, models)
      | None ->
          error ~unit_name
            "public model contains a non-public callable, domain, or result identity")

let snapshot_external_specifications validated descriptors =
  descriptors
  |> List.filter (fun descriptor ->
         match (Sst_validation.callable_definition descriptor).Sst.body with
         | Sst.External_specification (Sst.Imported_unverified_target _) -> true
         | Sst.Checked_exec _ | Sst.Spec_definition _
         | Sst.Recursive_spec_definition _ | Sst.Proof_body _
         | Sst.External_specification _ | Sst.Trusted_external_spec_target _
         | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
             false)
  |> List.map (snapshot_callable validated)

let snapshot_logical_constants ~unit_name ~candidate surface validated =
  let program = Sst_validation.program validated in
  let full_path relative = unit_name ^ "." ^ relative in
  let provider_interface =
    Option.value ~default:"" candidate.Cmt_input.interface_digest
  in
  let defined_candidates relative
      (receipt : Retained_interface_authority_private.logical_value) =
    program.Sst.logical_constants
    |> List.filter (fun definition ->
           let origin = definition.Sst.constant_id.constant_origin in
           (String.equal origin.canonical_path relative
           || String.equal origin.canonical_path (full_path relative))
           && String.equal origin.provider_unit unit_name
           && String.equal origin.provider_interface provider_interface
           && Cmt_input.interface_value_uid_correlates candidate
                ~path:(full_path relative)
                ~interface_uid:receipt.Retained_interface_authority_private.logical_value_uid
                ~implementation_uid:origin.value_uid)
  in
  let symbolic_candidates relative
      (receipt : Retained_interface_authority_private.logical_value) =
    program.Sst.functions
    |> List.filter_map (fun definition ->
           match definition.Sst.body with
           | Sst.Symbolic_declaration declaration
             when definition.parameters = []
                  && (String.equal
                        (Symbolic_application_private.canonical_path declaration)
                        relative
                     || String.equal
                          (Symbolic_application_private.canonical_path declaration)
                          (full_path relative))
                  && Cmt_input.interface_value_uid_correlates candidate
                       ~path:(full_path relative)
                       ~interface_uid:receipt.logical_value_uid
                       ~implementation_uid:
                         (Symbolic_application_private.value_uid declaration) ->
               Some (definition, declaration)
           | Sst.Symbolic_declaration _ | Sst.Checked_exec _
           | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
           | Sst.Proof_body _ | Sst.External_specification _
           | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _ ->
               None)
  in
  let rec collect constants = function
    | [] -> Ok (List.rev constants)
    | (relative,
       (receipt : Retained_interface_authority_private.logical_value))
      :: rest -> (
        match receipt.Retained_interface_authority_private.logical_value_class with
        | Defined_value -> (
            match defined_candidates relative receipt with
            | [ definition ] ->
                let* expected_abi =
                  Logical_constant_private.interface_abi
                    ~path:(full_path relative)
                    ~uid:receipt.logical_value_uid definition
                  |> Result.map_error (fun message ->
                         { unit_name = Some unit_name; message })
                in
                let* () =
                  if
                    String.equal expected_abi receipt.logical_value_typed_abi
                    && not
                         (String.equal
                            definition.constant_id.constant_origin.semantic_class
                            "symbolic-origin-v1")
                  then Ok ()
                  else
                    error ~unit_name
                      (Printf.sprintf
                         "public logical constant %s ABI or semantic class differs from its completed descriptor"
                         relative)
                in
                let logical_constant =
                  match receipt.logical_value_visibility with
                  | Defined_opaque ->
                      { definition with
                        Sst.constant_provenance = Opaque_defined_identity;
                        constant_equation = None }
                  | Defined_revealed -> definition
                  | Symbolic_opaque -> assert false
                in
                let* ( logical_constant_dependency_closure,
                       logical_constant_trust_dependencies ) =
                  match receipt.logical_value_visibility with
                  | Defined_opaque ->
                      Ok
                        ( Digest.to_hex
                            (Digest.string
                               ("opaque-logical-value-v1:"
                               ^ definition.constant_descriptor_digest)),
                          [] )
                  | Defined_revealed ->
                      let closure_program =
                        {
                          program with
                          Sst.functions =
                            Sst_validation.callable_descriptors validated
                            |> List.map Sst_validation.callable_definition;
                        }
                      in
                      Logical_constant_private.dependency_closure
                        ~program:closure_program definition
                      |> Result.map_error (fun message ->
                             { unit_name = Some unit_name; message })
                  | Symbolic_opaque -> assert false
                in
                collect
                  (Public_defined_logical_value
                     { logical_constant;
                       logical_constant_path = full_path relative;
                       logical_constant_uid = receipt.logical_value_uid;
                       logical_constant_dependency_closure;
                       logical_constant_trust_dependencies }
                  :: constants)
                  rest
            | [] ->
                error ~unit_name
                  (Printf.sprintf
                     "public logical constant %s has no exact completed descriptor"
                     relative)
            | _ :: _ :: _ ->
                error ~unit_name
                  (Printf.sprintf
                     "public logical constant %s has multiple completed descriptors"
                     relative))
        | Symbolic_value -> (
            match symbolic_candidates relative receipt with
            | [ (definition, declaration) ] ->
                    let* expected_abi =
                      Symbolic_application_private.declaration_abi_material
                        ~canonical_path:(full_path relative)
                        ~value_uid:receipt.logical_value_uid
                        ~type_binders:
                          (Symbolic_application_private.type_binders declaration)
                        ~parameter_labels:[] ~parameter_types:[]
                        ~result_type:
                          (Symbolic_application_private.declaration_result_type
                             declaration)
                      |> Result.map_error (fun message ->
                             { unit_name = Some unit_name; message })
                    in
                    let* () =
                      if
                        String.equal expected_abi
                          receipt.logical_value_typed_abi
                      then Ok ()
                      else
                        error ~unit_name
                          (Printf.sprintf
                             "public symbolic value %s marker or ABI differs from its completed descriptor"
                             relative)
                    in
                    [%log.debug "retained VERO-115 symbolic value identity path"
                      ~provider:(Delator.Field.string unit_name)
                      ~stage:(Delator.Field.string "public-interface-seal")
                      ~route:(Delator.Field.string (full_path relative))
                      ~decision:(Delator.Field.string "symbolic-registry")];
                    let descriptors =
                      Sst_validation.callable_descriptors validated
                    in
                    let descriptor =
                      List.find
                        (fun descriptor ->
                          Sst_validation.callable_id descriptor
                          = definition.Sst.function_id)
                        descriptors
                    in
                    collect
                      (Public_symbolic_logical_value
                         { symbolic_callable =
                             snapshot_callable validated descriptor;
                           logical_constant_path = full_path relative;
                           logical_constant_uid = receipt.logical_value_uid }
                      :: constants)
                      rest
            | [] ->
                error ~unit_name
                  (Printf.sprintf
                     "public symbolic value %s has no exact completed descriptor"
                     relative)
            | _ :: _ :: _ ->
                error ~unit_name
                  (Printf.sprintf
                     "public symbolic value %s has multiple completed descriptors"
                     relative)))
  in
  let result = collect [] surface.public_logical_values in
  (match result with
  | Ok (constants [@log_value.info]) ->
      [%log.info "completed logical-value semantic partition"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "public-interface-seal")
        ~constant_count:
          (Delator.Field.int
             (List.length (constants [@log_value.info])))
        ~decision:(Delator.Field.string "accepted")]
  | Error _ ->
      [%log.warn "rejected logical-value semantic partition"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "public-interface-seal")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "descriptor-cardinality-or-class")]);
  result
[@@delator.instrument] [@@delator.level debug]

let snapshot_invariants ~unit_name surface validated =
  match Type_invariant.authenticate validated with
  | Error invariant -> error ~unit_name (Type_invariant.error_to_string invariant)
  | Ok invariant_environment ->
      let all_handles = Type_invariant.handles invariant_environment in
      let exported handle =
        surface_has_type surface (Type_invariant.abstract_type handle)
        && public_function surface (Type_invariant.model_callable handle)
        && public_function surface (Type_invariant.predicate_callable handle)
        && List.for_all
             (fun (operation, _) -> public_function surface operation)
             (Type_invariant.public_operations handle)
      in
      let public_handles = List.filter exported all_handles in
      if
        List.exists
          (fun handle ->
            surface_has_type surface (Type_invariant.abstract_type handle)
            && not (exported handle))
          all_handles
      then error ~unit_name "public invariant contains a non-public exact identity"
      else
        public_handles
        |> List.map (fun handle ->
               {
                 invariant_id = Type_invariant.invariant_id handle;
                 abstract_type = Type_invariant.abstract_type handle;
                 certificate_id = Type_invariant.certificate_id handle;
                 model_callable = Type_invariant.model_callable handle;
                 model_snapshot_type = Type_invariant.model_snapshot_type handle;
                 predicate_callable = Type_invariant.predicate_callable handle;
                 predicate_digest = Type_invariant.predicate_digest handle;
                 public_operations = Type_invariant.public_operations handle;
               })
        |> Result.ok

let verified_snapshot_with_calls ~imported_specification_call ~unit_name ~candidate validated =
  match embedded_public_surface ~unit_name candidate with
  | Error _ as error -> error
  | Ok surface ->
      let surface =
        {
          surface with
          public_external_type_constructors =
            (Sst_validation.program validated).Sst.parametric_adts
            |> List.filter_map (fun descriptor ->
                   match Parametric_adt.provenance descriptor with
                   | Parametric_adt.External _
                     when surface_has_type surface
                            (Parametric_adt.type_id descriptor) ->
                       Some (Parametric_adt.type_constructor descriptor)
                   | Parametric_adt.External _ | Parametric_adt.Local _ -> None);
        }
      in
      let type_descriptors = Sst_validation.type_descriptors validated in
      let public_linked_types =
        List.filter public_linked_type type_descriptors
      in
      let callable_descriptors = Sst_validation.callable_descriptors validated in
      Result.bind
        (snapshot_logical_constants ~unit_name ~candidate surface validated)
        (fun logical_constants ->
      Result.bind
        (validate_surface_completeness ~unit_name surface public_linked_types
           callable_descriptors)
        (fun () ->
          Result.bind
            (snapshot_types ~unit_name surface validated public_linked_types)
            (fun types ->
              Result.bind
                (snapshot_callables_and_models ~unit_name ~imported_specification_call surface validated
                   callable_descriptors)
                (fun (callables, models) ->
                  let external_specifications =
                    snapshot_external_specifications validated
                      callable_descriptors
                  in
                  Result.map
                    (fun invariants ->
                      ( types,
                        logical_constants,
                        callables,
                        external_specifications,
                        models,
                        invariants ))
                    (snapshot_invariants ~unit_name surface validated)))))

let verified_snapshot ?imported ~unit_name ~candidate validated =
  match imported with
  | None -> verified_snapshot_with_calls ~imported_specification_call:(fun _ -> false) ~unit_name ~candidate validated
  | Some imported ->
      (match Imported_callable.seal_calls imported ~implementation:candidate ~program:(Sst_validation.program validated) with
      | Error message -> error ~unit_name message
      | Ok registration -> Fun.protect ~finally:(fun () -> Imported_callable.invalidate_registration registration) (fun () ->
          let imported_specification_call expression =
            let accepted=match expression.Sst.expression_desc, Imported_callable.find_call registration expression with
              | Direct_call {call_form=Specification_call;recursive=false;_}, Some call ->
                  (Imported_callable.call_summary call).definition.mode=Sst.Spec
              | _ -> false in
            [%log.trace "validated imported specification reference in public contract"
              ~provider:(Delator.Field.string unit_name)
              ~accepted:(Delator.Field.bool accepted)
              ~reexported_callable:(Delator.Field.bool false)];
            accepted in
          verified_snapshot_with_calls ~imported_specification_call ~unit_name ~candidate validated))

type safe_verification_failure = {
  failure_class : string;
  remedy_class : string;
  message : string;
}

type verification_failure_classification =
  | Frontend_failure of Diagnostic.t
  | Dependency_failure of safe_verification_failure
  | Authenticated_internal_failure of safe_verification_failure

let safe_failure ~failure_class ~remedy_class message =
  { failure_class; remedy_class; message }

let classify_verification_failure ~authenticated_candidate = function
  | Verification_driver_private.Frontend_error diagnostic ->
      Frontend_failure diagnostic
  | Validation_error validation_error ->
      let[@log_value.debug] validation_detail =
        Sst_validation.error_to_string validation_error
      in
      [%log.debug "classified semantic SST validation failure"
        ~stage:(Delator.Field.string "verification-failure-classification")
        ~detail:
          (Delator.Field.string
             (validation_detail [@log_value.debug]))
        ~decision:(Delator.Field.string "frontend-diagnostic")];
      Frontend_failure (Sst_validation.to_diagnostic validation_error)
  | Provider_surface_error message ->
      Dependency_failure
        (safe_failure ~failure_class:"provider-interface-completion"
           ~remedy_class:"rebuild-provider-interface"
           message)
  | Invariant_error _ ->
      Dependency_failure
        (safe_failure ~failure_class:"invariant-authentication"
           ~remedy_class:"rebuild-with-current-toolchain"
           "verification metadata failed an integrity check. Remedy: rebuild the unit with the current VeroCaml toolchain and retry verification.")
  | Pipeline_error pipeline_error
    when authenticated_candidate
         && Option.is_some
              (Verification_pipeline.post_validation_invariant_breach
                 pipeline_error) ->
      Authenticated_internal_failure
        (safe_failure ~failure_class:"post-validation-invariant"
           ~remedy_class:"report-verifier-defect"
           "authenticated post-validation invariant breach")
  | Pipeline_error pipeline_error
    when Option.is_some
           (Verification_pipeline.post_validation_invariant_breach
              pipeline_error) ->
      Dependency_failure
        (safe_failure
           ~failure_class:"unauthenticated-post-validation-invariant"
           ~remedy_class:"rebuild-with-current-toolchain"
           "verification metadata failed an integrity check. Remedy: rebuild the unit with the current VeroCaml toolchain and retry verification.")
  | Pipeline_error (Verification_pipeline.Engine_error _engine_error) ->
      [%log.debug "captured symbolic engine failure"
        ~stage:(Delator.Field.string "verification-failure-classification")
        ~failure_class:(Delator.Field.string "symbolic-engine")
        ~technical_detail:
          (Delator.Field.string
             (Symbolic_executor_private.error_to_string _engine_error))];
      Dependency_failure
        (safe_failure ~failure_class:"symbolic-engine"
           ~remedy_class:"rebuild-and-retry"
           "the verifier rejected the derived verification artifact. Remedy: rebuild the unit and retry verification.")
  | Pipeline_error
      (Verification_pipeline.Setup_error
        (Verification_pipeline.Internal_setup_error _)) ->
      Dependency_failure
        (safe_failure ~failure_class:"verification-setup"
           ~remedy_class:"retry-then-rebuild"
           "verification setup could not be completed. Remedy: retry verification; rebuild the unit if the failure persists.")
  | Pipeline_error (Verification_pipeline.Solve_error _solve_error) ->
      [%log.debug "captured verification pipeline failure"
        ~stage:(Delator.Field.string "verification-failure-classification")
        ~failure_class:(Delator.Field.string "verification-pipeline")
        ~technical_detail:(Delator.Field.string _solve_error)];
      Dependency_failure
        (safe_failure ~failure_class:"verification-pipeline"
           ~remedy_class:"rebuild-and-retry"
           "verification of the derived artifact could not be completed. Remedy: rebuild the unit and retry verification.")
  | Pipeline_error
      (Verification_pipeline.Setup_error
        (Verification_pipeline.Solver_configuration_error _)) ->
      Dependency_failure
        (safe_failure ~failure_class:"solver-configuration"
           ~remedy_class:"fix-solver-settings"
           "solver settings are invalid. Remedy: fix the verifier solver settings and retry verification.")
  | Internal_error _internal_error ->
      [%log.debug "captured verification driver failure"
        ~stage:(Delator.Field.string "verification-failure-classification")
        ~failure_class:(Delator.Field.string "verification-driver")
        ~technical_detail:(Delator.Field.string _internal_error)];
      Dependency_failure
        (safe_failure ~failure_class:"verification-driver"
           ~remedy_class:"rebuild-and-retry"
           "the verifier could not prepare the loaded unit. Remedy: rebuild the unit and retry verification.")

let route_verification_failure ~unit_name ~authenticated_candidate failure =
  match classify_verification_failure ~authenticated_candidate failure with
  | Frontend_failure diagnostic ->
      error ~unit_name ~diagnostic
        (Printf.sprintf "frontend verification failed [%s]: %s" diagnostic.code
           diagnostic.message)
  | Dependency_failure failure ->
      [%log.debug "routing non-internal verification failure"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "verification-failure-classification")
        ~failure_class:(Delator.Field.string failure.failure_class)
        ~candidate_authenticated:(Delator.Field.bool authenticated_candidate)
        ~decision:(Delator.Field.string "dependency")
        ~remedy_class:(Delator.Field.string failure.remedy_class)];
      error ~unit_name failure.message
  | Authenticated_internal_failure failure ->
      [%log.error "routing authenticated post-validation failure"
        ~provider:(Delator.Field.string unit_name)
        ~stage:(Delator.Field.string "internal-diagnostic")
        ~failure_class:(Delator.Field.string failure.failure_class)
        ~candidate_authenticated:(Delator.Field.bool authenticated_candidate)
        ~remedy_class:(Delator.Field.string failure.remedy_class)];
      error ~unit_name ~internal:true failure.message

let provider_verification_entries = ref 0

let validate_provider_logical_value_surface ~unit_name candidate validated =
  match embedded_public_surface ~unit_name candidate with
  | Error (failure : error) -> Error failure.message
  | Ok surface ->
      snapshot_logical_constants ~unit_name ~candidate surface validated
      |> Result.map (fun _ -> ())
      |> Result.map_error (fun (failure : error) -> failure.message)

let verify_candidate_internal ?imported ?external_specifications ?(numeric_prerequisites=[]) ~solver_policy
    candidate =
  incr provider_verification_entries;
  let unit_name = candidate.Cmt_input.unit_name in
  let reject message = error ~unit_name message in
  match
    Verification_driver_private.run_with_policy ~solver_policy
      ~allow_imported_opens:true
      ~allow_public_parametric_signatures:candidate.explicit_interface
      ~validate_provider_surface:
        (validate_provider_logical_value_surface ~unit_name candidate)
      ?imported ?external_specifications candidate
  with
  | Error failure ->
      route_verification_failure ~unit_name ~authenticated_candidate:true failure
  | Ok report ->
      let validated = Verification_driver_private.validated report in
      let program = Sst_validation.program validated in
      let* numeric_semantics =
        if candidate.interface_numeric_claims.numeric_roles = [] then Ok []
        else
          match Verification_driver_private.verified_completion report with
          | None -> reject "Numeric semantics require successful verification of their provider."
          | Some completion -> (
              match Numeric_semantics_binding_private.complete_local ~completion
                ~implementation:candidate ~validated with
              | Ok bindings -> Ok bindings
              | Error message -> reject message)
      in
      let unsupported_external_specification_trust =
        List.exists
          (fun definition ->
            match definition.Sst.body with
            | Sst.External_specification
                (Sst.Imported_unverified_target _) ->
                false
            | Sst.External_specification _
            | Sst.Trusted_external_spec_target _ ->
                true
            | Sst.Trusted_external_body _ | Sst.Checked_exec _
            | Sst.Spec_definition _ | Sst.Proof_body _
            | Sst.Recursive_spec_definition _ | Sst.Symbolic_declaration _ ->
                false)
          program.functions
      in
      let* numeric_provider = match Verification_driver_private.verified_completion report with
        | None -> reject "retained provider lacks private-driver completion"
        | Some completion ->
            (match (match imported with
              | Some imported -> Numeric_provider_private.complete_with_imports ~completion ~implementation:candidate ~validated
                  ~imported ~prerequisites:numeric_prerequisites ~declarations:numeric_semantics
              | None -> Numeric_provider_private.complete ~completion ~implementation:candidate ~validated
                  ~dependencies:[] ~declarations:numeric_semantics) with
              | Ok provider -> Ok provider | Error message -> reject message) in
      let trusted_broadcasts =
        candidate.Cmt_input.interface_broadcasts
        |> List.filter_map (fun member ->
               let identity = member.Retained_broadcast_private.identity in
               if identity.kind = Retained_broadcast_private.Declaration then
                 let prefix = candidate.unit_name ^ "." in
                 if String.starts_with ~prefix identity.canonical_path then
                   Some
                     (String.sub identity.canonical_path (String.length prefix)
                        (String.length identity.canonical_path
                        - String.length prefix))
                 else None
               else None)
      in
      let unselected_trusted_body =
        List.exists
          (fun definition ->
            match definition.Sst.body with
            | Sst.Trusted_external_body _ ->
                not
                  (List.mem definition.function_id.function_name
                     trusted_broadcasts
                  || Numeric_semantics_binding_private.selects_explicit_axiom
                       numeric_semantics definition
                  || List.exists (fun (refinement : Numeric_runtime_refinement_private.t) ->
                       refinement.executable == definition
                       && refinement.authority = Explicit_external_body) numeric_provider.refinements)
            | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
            | Sst.Recursive_spec_definition _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _ | Sst.Symbolic_declaration _ ->
                false)
          program.functions
      in
      if unsupported_external_specification_trust || unselected_trusted_body then
        reject
          "axiomatic trusted declarations are not exportable under the selected interface policy"
      else
        match Verification_driver_private.verified_completion report with
        | None -> reject "retained provider lacks private-driver completion"
        | Some completion -> (
            match verified_snapshot ?imported ~unit_name ~candidate validated with
            | Error _ as error -> error
            | Ok (types, logical_constants, callables, external_specifications, models, invariants) ->
                [%log.debug "verified dependency provider"
                  ~unit_name
                  ~types:(Delator.Field.int (List.length types))
                  ~callables:(Delator.Field.int (List.length callables))
                  ~logical_constants:
                    (Delator.Field.int (List.length logical_constants))
                  ~external_specifications:
                    (Delator.Field.int (List.length external_specifications))
                  ~models:(Delator.Field.int (List.length models))
                  ~invariants:(Delator.Field.int (List.length invariants))];
                Ok
                  ( validated,
                    types,
                    logical_constants,
                    callables,
                    external_specifications,
                    models,
                    invariants,
                    completion,
                    numeric_provider ))
[@@delator.instrument] [@@delator.level debug]

let verify_candidate ?imported ?external_specifications ?numeric_prerequisites
    ~solver_policy:(solver_policy [@delator.skip])
    (candidate [@delator.skip]) =
  let result =
    verify_candidate_internal ?imported ?external_specifications ?numeric_prerequisites ~solver_policy
      candidate
  in
  (match result with
  | Ok (_validated, _, _, _, _, _, _, _, _) ->
      let[@log_value.info] program = Sst_validation.program _validated in
      let[@log_value.info] _trusted_count =
        List.fold_left
          (fun count definition ->
            match definition.Sst.body with
            | Sst.Trusted_external_body _ -> count + 1
            | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
            | Sst.Recursive_spec_definition _ | Sst.External_specification _
            | Sst.Trusted_external_spec_target _ | Sst.Symbolic_declaration _ ->
                count)
          0 (program [@log_value.info]).functions
      in
      [%log.info "completed retained provider verification"
        ~provider:(Delator.Field.string candidate.Cmt_input.unit_name)
        ~stage:(Delator.Field.string "loaded-provider-verification")
        ~function_count:
          (Delator.Field.int
             (List.length (program [@log_value.info]).functions))
        ~broadcast_count:
          (Delator.Field.int (List.length candidate.interface_broadcasts))
        ~trusted_count:
          (Delator.Field.int (_trusted_count [@log_value.info]))
        ~decision:(Delator.Field.string "accepted")]
  | Error _ ->
      [%log.debug "rejected retained provider verification"
        ~provider:(Delator.Field.string candidate.Cmt_input.unit_name)
        ~stage:(Delator.Field.string "loaded-provider-verification")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "loaded-diagnostic")]);
  result
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let authenticate_loaded_with_policy ~external_targets ~solver_policy
    ~dependencies:candidates ~consumer =
  let import_matches owner (staged : staged_dependency)
      (import : Cmt_input.import) =
    exact_import ~owner ~dependency:staged.candidate import
  in
  let graph_candidates =
    List.fold_left
      (fun candidates target ->
        if
          List.exists
            (fun candidate ->
              String.equal candidate.Cmt_input.unit_name target.Cmt_input.unit_name
              && candidate.interface_digest = target.interface_digest
              && String.equal candidate.raw_artifact_digest
                   target.raw_artifact_digest)
            candidates
        then candidates
        else candidates @ [ target ])
      candidates external_targets
  in
  match graph_order graph_candidates consumer with
  | Error _ as error -> error
  | Ok order ->
          let rec strict = function
            | [] -> strict_candidate ~require_public_interface:false consumer
            | candidate :: rest
              when not (Cmt_input.retained_preprocessing candidate) ->
                if
                  List.exists
                    (fun target ->
                      String.equal target.Cmt_input.unit_name
                        candidate.Cmt_input.unit_name
                      && target.interface_digest = candidate.interface_digest
                      && String.equal target.raw_artifact_digest
                           candidate.raw_artifact_digest)
                    external_targets
                then strict rest
                else
                  error ~unit_name:candidate.unit_name
                    "ordinary CMT is not an authenticated external target for this operation"
            | candidate :: rest -> (
            match strict_candidate ~require_public_interface:true candidate with
            | Error _ as error -> error
            | Ok _ -> strict rest)
      in
      (match strict order with
      | Error _ as error -> error
      | Ok _ -> (
          match
            preflight_broadcast_implementations ~dependencies:order ~consumer
          with
          | Error _ as error -> error
          | Ok () ->
          let rec verify (verified : staged_dependency list) = function
            | [] ->
                if
                  List.for_all
                    (fun (staged : staged_dependency) ->
                      Array.exists
                        (import_matches consumer staged)
                        consumer.imports)
                    (List.filter
                       (fun (staged : staged_dependency) ->
                         Array.exists
                           (fun (import : Cmt_input.import) ->
                             String.equal import.Cmt_input.unit_name
                               staged.candidate.unit_name)
                           consumer.imports)
                       verified)
                then
                  let rec issue issued = function
                    | [] ->
                        let roots =
                          List.filter
                            (fun (handle : handle) ->
                              public_types_export_external_type_specification
                                handle.types
                              || Array.exists
                                   (fun (import : Cmt_input.import) ->
                                     exact_import ~owner:consumer
                                       ~dependency:handle.private_implementation
                                       import)
                                   consumer.imports)
                            issued
                        in
                        let permitted =
                          unique_handles
                            (roots
                            @ List.concat_map
                                (fun (handle : handle) ->
                                  handle.direct_dependencies
                                  @ handle.transitive_dependencies)
                                roots)
                        in
                        (match
                           List.find_opt
                             (fun (handle : handle) ->
                               not
                                 (List.exists
                                    (fun (candidate : handle) ->
                                      String.equal candidate.unit_name
                                        handle.unit_name
                                      && String.equal
                                           candidate.interface_digest
                                           handle.interface_digest)
                                    permitted))
                             issued
                         with
                        | Some unused ->
                            error ~unit_name:unused.unit_name
                              "supplied dependency is not in the consumer import closure and exports no external type specification"
                        | None ->
                            Ok
                              ( { issuer = process_issuer; handles = issued },
                                consumer ))
                    | staged :: rest ->
                        let direct_dependencies =
                          List.filter
                            (fun (handle : handle) ->
                              List.exists
                                (fun dependency ->
                                  String.equal handle.unit_name
                                    dependency.candidate.unit_name)
                                staged.direct_dependencies)
                            issued
                        in
                        let transitive_dependencies =
                          direct_dependencies
                          @ List.concat_map
                              (fun (handle : handle) ->
                                handle.transitive_dependencies)
                              direct_dependencies
                          |> unique_handles
                        in
                        let handle =
                          construct_handle staged direct_dependencies
                            transitive_dependencies
                        in
                        issue (issued @ [ handle ]) rest
                  in
                  issue [] verified
                else
                  error ~unit_name:consumer.unit_name
                    "consumer import slot does not match a verified dependency \
                     interface"
            | (candidate : Cmt_input.implementation) :: rest ->
                if not (Cmt_input.retained_ppx_artifact candidate) then
                  verify verified rest
                else
                let direct_dependencies =
                  List.filter
                    (fun (staged : staged_dependency) ->
                      Array.exists
                        (fun (import : Cmt_input.import) ->
                          String.equal import.Cmt_input.unit_name
                            staged.candidate.unit_name)
                        candidate.Cmt_input.imports)
                    verified
                in
                if
                  not
                    (List.for_all
                       (fun (staged : staged_dependency) ->
                         Array.exists
                           (import_matches candidate staged)
                           candidate.imports)
                       direct_dependencies)
                then
                  error ~unit_name:candidate.unit_name
                    "dependency import CRC does not match the verified interface"
                else
                  match
                    imported_environment_of_staged_authenticated
                      direct_dependencies
                  with
                  | Error _ as error -> error
                  | Ok imported -> (
                  let external_specifications =
                    External_target_specification_private.environment
                      ~consumer:candidate ~targets:external_targets
                  in
                  (match external_specifications with
                  | Error message -> error ~unit_name:candidate.unit_name message
                  | Ok external_specifications ->
                  match
                    verify_candidate ~imported ~external_specifications
                      ~numeric_prerequisites:(List.concat_map (fun (staged : staged_dependency) -> staged.numeric_provider.laws) verified)
                      ~solver_policy candidate
                  with
                      | Error _ as error -> error
                      | Ok
                          ( validated,
                            types,
                            logical_constants,
                            callables,
                            external_specifications,
                            models,
                            invariants,
                            private_driver_completion,
                            numeric_provider ) ->
                          let interface_digest =
                            match candidate.interface_digest with
                            | Some digest -> digest
                            | None -> assert false
                          in
                          let mode_signature_digest =
                            Digest.string
                              (Sst_validation.instance_mode_dump validated)
                            |> Digest.to_hex
                          in
                          let staged =
                            {
                              numeric_provider;
                              candidate;
                              interface_digest;
                              mode_signature_digest;
                              types;
                              logical_constants;
                              callables;
                              external_specifications;
                              models;
                              invariants;
                              semantic_snapshot = validated;
                              private_driver_completion;
                              direct_dependencies;
                            }
                          in
                          verify (verified @ [ staged ]) rest))
          in
          verify [] order))
[@@delator.instrument] [@@delator.level debug]

type loaded_verification = {
  loaded_environment : environment;
  loaded_driver : Verification_driver_private.report;
  loaded_numeric_registry : Numeric_ghost_registry_private.t option;
  loaded_numeric_target_sets : Numeric_required_target_set_private.t list;
}

let run_loaded_consumer ~threads ~solver_policy ~authenticated_candidate
    ?numeric_target ?external_specifications environment
    (consumer : Cmt_input.implementation) =
  let exports_external_type_specification (handle : handle) =
    public_types_export_external_type_specification handle.types
  in
  let direct_environment =
    {
      issuer = process_issuer;
      handles =
        List.filter
          (fun (handle : handle) ->
            exports_external_type_specification handle
            || Array.exists
                 (fun (import : Cmt_input.import) ->
                   exact_import ~owner:consumer
                     ~dependency:handle.private_implementation import)
                 consumer.imports)
          environment.handles;
    }
  in
  match imported_environment_authenticated direct_environment with
  | Error _ as error -> error
  | Ok imported -> (
      let adopt_external_specifications =
        match external_specifications with
        | None ->
            if Imported_callable.external_specifications imported = [] then Ok ()
            else Error "imported external specifications have no target environment"
        | Some target_environment ->
            let imports_exact unit_name interface_digest =
              Array.exists
                (fun (import : Cmt_input.import) ->
                  String.equal import.unit_name unit_name
                  && import.crc = Some interface_digest)
                consumer.imports
            in
            let active =
              Imported_callable.external_specifications imported
              |> List.filter (fun
                   (specification :
                     Imported_callable.external_specification_snapshot) ->
                     imports_exact specification.provider_unit
                       specification.provider_interface
                     &&
                     match specification.target_link with
                     | Sst.Imported_unverified_target link ->
                         imports_exact link.target_unit
                           link.target_interface_digest
                     | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
                         false)
            in
            let rec overlap = function
              | [] -> None
              | (specification :
                  Imported_callable.external_specification_snapshot)
                :: rest -> (
                  match specification.target_link with
                  | Sst.Imported_unverified_target link -> (
                      match
                        List.find_map
                          (fun
                            (candidate :
                              Imported_callable.external_specification_snapshot) ->
                            match candidate.target_link with
                            | Sst.Imported_unverified_target other ->
                                if
                                  String.equal link.target_unit other.target_unit
                                  && String.equal link.target_interface_digest
                                       other.target_interface_digest
                                  && (String.equal link.canonical_path
                                        other.canonical_path
                                     || String.equal link.value_uid
                                          other.value_uid)
                                then Some (candidate, other.canonical_path)
                                else None
                            | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
                                None)
                          rest
                      with
                      | Some (other, other_path) ->
                          Some
                            ( specification.provider_unit,
                              other.provider_unit,
                              link.canonical_path,
                              other_path )
                      | None -> overlap rest)
                  | Sst.Same_unit_target _ | Sst.Unresolved_target _ ->
                      overlap rest)
            in
            (match overlap active with
            | Some (left, right, left_path, right_path) ->
                Error
                  (Printf.sprintf
                     "overlapping external function specifications from %s and %s target %s / %s"
                     left right left_path right_path)
            | None ->
            List.fold_left
              (fun result
                   (specification :
                     Imported_callable.external_specification_snapshot) ->
                Result.bind result (fun () ->
                    External_target_specification_private.adopt_imported
                      target_environment
                      ~provider_unit:specification.provider_unit
                      ~provider_interface:specification.provider_interface
                      ~definition:specification.definition
                      ~signature:specification.signature
                      ~target_link:specification.target_link))
              (Ok ()) active)
      in
      match adopt_external_specifications with
      | Error message -> error ~unit_name:consumer.unit_name message
      | Ok () ->
      let numeric_requested = consumer.interface_numeric_claims.numeric_roles <> []
        || List.exists (fun (handle : handle) -> handle.numeric_provider.laws <> []) environment.handles in
      let dependency_laws = List.concat_map (fun (handle : handle) -> handle.numeric_provider.laws) environment.handles in
      let dependency_descriptors = List.concat_map (fun (handle : handle) -> handle.numeric_provider.descriptors) environment.handles in
      let* pre_solver_numeric_registry =
        if not numeric_requested then Ok None else
        let admission = Numeric_ghost_registry_private.import_laws ~consumer
          ~dependencies:(Imported_callable.artifacts imported) ~descriptors:dependency_descriptors dependency_laws in
        [%log.debug "checked numeric dependency authority before consumer verification"
          ~consumer:(Delator.Field.string consumer.unit_name)
          ~law_count:(Delator.Field.int (List.length dependency_laws))
          ~admitted:(Delator.Field.bool (Result.is_ok admission))];
        match admission with
        | Ok registry -> Ok (Some registry)
        | Error failure -> error ~unit_name:consumer.unit_name (Numeric_ghost_registry_private.error_message failure) in
      let* numeric_native =
        match numeric_target with
        | None -> Ok None
        | Some target ->
            let* imported =
              match pre_solver_numeric_registry with
              | None -> Ok None
              | Some registry -> (
                  match
                    Numeric_ghost_registry_private.native_bv_source_request
                      registry ~target
                  with
                  | Ok request -> Ok request
                  | Error message -> error ~unit_name:consumer.unit_name message)
            in
            let base_providers =
              environment.handles
              |> List.map (fun (handle : handle) ->
                     handle.private_implementation)
            in
            (match
               Numeric_bv_source_admission_private.with_local_provider
                 ~imported ~implementation:consumer ~base_providers ~target
             with
            | Ok request -> Ok request
            | Error message -> error ~unit_name:consumer.unit_name message)
      in
      let verification =
        if threads = 1 then
          Verification_driver_private.run_with_policy ~solver_policy ~imported
            ?numeric_native
            ~capture_numeric_obligations:numeric_requested
            ?external_specifications
            ~allow_public_parametric_signatures:
              (consumer.explicit_interface && environment.handles <> [])
            ~allow_imported_opens:(environment.handles <> []) consumer
        else
          Verification_driver_private.run_with_policy_and_threads ~threads
            ~solver_policy ~imported ?numeric_native ?external_specifications
            ~capture_numeric_obligations:numeric_requested
            ~allow_public_parametric_signatures:
              (consumer.explicit_interface && environment.handles <> [])
            ~allow_imported_opens:(environment.handles <> []) consumer
      in
      match verification with
      | Ok report ->
          let* numeric_registry, numeric_target_sets =
            if not numeric_requested then Ok (None, []) else
            let reject message = error ~unit_name:consumer.unit_name message in
            let validated = Verification_driver_private.validated report in
            let* local_laws, local_descriptors = match Verification_driver_private.verified_completion report with
              | None -> Ok ([], [])
              | Some completion ->
                  let* declarations = match Numeric_semantics_binding_private.complete_local
                    ~completion ~implementation:consumer ~validated with
                    | Ok declarations -> Ok declarations | Error message -> reject message in
                  let* provider = match Numeric_provider_private.complete_with_imports ~completion
                    ~implementation:consumer ~validated ~imported
                    ~prerequisites:(List.concat_map (fun (handle : handle) -> handle.numeric_provider.laws) environment.handles)
                    ~declarations with
                    | Ok provider -> Ok provider | Error message -> reject message in
                  Ok (provider.laws, provider.descriptors) in
            let descriptors = local_descriptors @ dependency_descriptors in
            let* registry = match Numeric_ghost_registry_private.import_laws ~consumer
              ~dependencies:(Imported_callable.artifacts imported) ~descriptors (local_laws @ dependency_laws) with
              | Ok registry -> Ok registry
              | Error failure -> reject (Numeric_ghost_registry_private.error_message failure) in
            let capability = Build_target_profile_private.capability () in
            let* selector = match Numeric_required_target_set_private.select ~capability ~modes:[Concrete] with
              | Ok selector -> Ok selector | Error message -> reject message in
            let* sets = List.fold_left (fun result original ->
              let* sets = result in
              match Numeric_required_target_set_private.create ~capability ~selector ~registry ~report ~original ~coverage:None with
              | Ok set -> Ok (set :: sets) | Error message -> reject message)
              (Ok []) (Verification_driver_private.original_obligations report) in
            [%log.debug "collected numeric libraries and original target obligations"
              ~provider_count:(Delator.Field.int (List.length environment.handles))
              ~law_count:(Delator.Field.int (List.length (Numeric_ghost_registry_private.laws registry)))
              ~original_count:(Delator.Field.int (List.length sets))
              ~created_numeric_queries:(Delator.Field.bool false)];
            Ok (Some registry, List.rev sets) in
          Ok
            {
              loaded_environment = environment;
              loaded_driver = report;
              loaded_numeric_registry = numeric_registry;
              loaded_numeric_target_sets = numeric_target_sets;
            }
      | Error failure ->
          route_verification_failure ~unit_name:consumer.unit_name
            ~authenticated_candidate failure)

let verify_loaded_with_policy ~threads ~solver_policy ~numeric_target ~external_specifications
    ~external_targets ~dependencies ~consumer =
  match dependencies with
  | [] -> (
      match
        preflight_broadcast_implementations ~dependencies:[] ~consumer
      with
      | Error _ as error -> error
      | Ok () ->
      if Finite_formal_requirement.requires_authentication consumer then
        match strict_candidate ~require_public_interface:false consumer with
        | Error _ as error -> error
        | Ok _ ->
            run_loaded_consumer ~threads ~solver_policy
              ~authenticated_candidate:true ?numeric_target ?external_specifications
              { issuer = process_issuer; handles = [] }
              consumer
      else
        let authenticated_candidate =
          Result.is_ok
            (strict_candidate ~require_public_interface:false consumer)
        in
        run_loaded_consumer ~threads ~solver_policy ~authenticated_candidate
          ?numeric_target ?external_specifications { issuer = process_issuer; handles = [] }
          consumer)
  | _ -> (
      match
        authenticate_loaded_with_policy ~external_targets ~solver_policy
          ~dependencies ~consumer
      with
      | Error _ as error -> error
      | Ok (environment, consumer) ->
          run_loaded_consumer ~threads ~solver_policy
            ~authenticated_candidate:true ?numeric_target ?external_specifications environment
            consumer)
[@@delator.instrument] [@@delator.level debug]


let authenticate ~external_targets ~solver_policy ~dependencies ~consumer =
  authenticate_loaded_with_policy ~external_targets ~solver_policy ~dependencies
    ~consumer
[@@delator.instrument] [@@delator.level debug]

let verify ~threads ~solver_policy ~external_specifications ~external_targets
    ~consumer ~dependencies =
  match
    verify_loaded_with_policy ~threads ~solver_policy ~external_specifications
      ~numeric_target:None ~external_targets ~dependencies ~consumer
  with
  | Error _ as error -> error
  | Ok loaded ->
      let provenance =
        provenance loaded.loaded_environment
        |> List.map (fun (value : provenance) ->
               ( value.unit_name, value.interface_digest,
                 value.direct_dependencies, value.transitive_dependencies ))
      in
      Ok { driver = loaded.loaded_driver; provenance;
        numeric_registry = loaded.loaded_numeric_registry;
        numeric_target_sets = loaded.loaded_numeric_target_sets }
[@@delator.instrument] [@@delator.level info]

let verify_for_numeric_target ~threads ~solver_policy ~external_specifications
    ~external_targets ~target ~consumer ~dependencies =
  match
    verify_loaded_with_policy ~threads ~solver_policy ~external_specifications
      ~numeric_target:(Some target) ~external_targets ~dependencies ~consumer
  with
  | Error _ as error -> error
  | Ok loaded ->
      let provenance =
        provenance loaded.loaded_environment
        |> List.map (fun (value : provenance) ->
               ( value.unit_name, value.interface_digest,
                 value.direct_dependencies, value.transitive_dependencies ))
      in
      Ok
        { driver = loaded.loaded_driver; provenance;
          numeric_registry = loaded.loaded_numeric_registry;
          numeric_target_sets = loaded.loaded_numeric_target_sets }
[@@delator.instrument] [@@delator.level info]

let driver result = result.driver
let numeric_registry result = result.numeric_registry
let numeric_target_sets result = result.numeric_target_sets
let provenance result = result.provenance
let error_unit_name (error : error) = error.unit_name
let error_message (error : error) = error.message
let error_diagnostic =
  Interface_specification_environment_private.error_diagnostic
let error_is_internal =
  Interface_specification_environment_private.error_is_internal
let error = Interface_specification_environment_private.error

module For_testing = struct
  let reset_provider_verification_entries () =
    provider_verification_entries := 0

  let provider_verification_entries () = !provider_verification_entries
end
