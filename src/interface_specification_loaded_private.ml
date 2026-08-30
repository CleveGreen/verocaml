open Interface_specification_environment_private
open Interface_specification_candidate_private

type error = Interface_specification_environment_private.error

type result = {
  driver : Verification_driver_private.report;
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

let snapshot_callables_and_models ~unit_name surface validated descriptors =
  let exported =
    List.filter
      (fun descriptor ->
        public_function surface (Sst_validation.callable_id descriptor))
      descriptors
  in
  match
    List.find_opt
      (fun descriptor -> not (public_callable_descriptor surface descriptor))
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

let verified_snapshot ~unit_name ~candidate validated =
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
        (validate_surface_completeness ~unit_name surface public_linked_types
           callable_descriptors)
        (fun () ->
          Result.bind
            (snapshot_types ~unit_name surface validated public_linked_types)
            (fun types ->
              Result.bind
                (snapshot_callables_and_models ~unit_name surface validated
                   callable_descriptors)
                (fun (callables, models) ->
                  let external_specifications =
                    snapshot_external_specifications validated
                      callable_descriptors
                  in
                  Result.map
                    (fun invariants ->
                      ( types,
                        callables,
                        external_specifications,
                        models,
                        invariants ))
                    (snapshot_invariants ~unit_name surface validated))))
let provider_verification_entries = ref 0

let verify_candidate ?imported ?external_specifications ~solver_policy candidate =
  incr provider_verification_entries;
  let unit_name = candidate.Cmt_input.unit_name in
  let reject message = error ~unit_name message in
  match
    Verification_driver_private.run_with_policy ~solver_policy
      ~allow_imported_opens:true
      ~allow_public_parametric_signatures:candidate.explicit_interface
      ?imported ?external_specifications candidate
  with
  | Error (Verification_driver_private.Frontend_error diagnostic) ->
      error ~unit_name ~diagnostic
        (Printf.sprintf "frontend verification failed [%s]: %s" diagnostic.code
           diagnostic.message)
  | Error (Validation_error validation_error) ->
      error ~unit_name
        ~diagnostic:(Sst_validation.to_diagnostic validation_error)
        (Sst_validation.error_to_string validation_error)
  | Error (Invariant_error invariant_error) ->
      reject (Type_invariant.error_to_string invariant_error)
  | Error (Pipeline_error _) -> reject "private verification pipeline rejected provider"
  | Error (Internal_error message) -> reject message
  | Ok report ->
      let validated = Verification_driver_private.validated report in
      match Verification_driver_private.verified_completion report with
        | None -> reject "retained provider lacks private-driver completion"
        | Some completion -> (
            match verified_snapshot ~unit_name ~candidate validated with
            | Error _ as error -> error
            | Ok (types, callables, external_specifications, models, invariants) ->
                [%log.debug "verified dependency provider"
                  ~unit_name
                  ~types:(Delator.Field.int (List.length types))
                  ~callables:(Delator.Field.int (List.length callables))
                  ~external_specifications:
                    (Delator.Field.int (List.length external_specifications))
                  ~models:(Delator.Field.int (List.length models))
                  ~invariants:(Delator.Field.int (List.length invariants))];
                Ok
                  ( validated,
                    types,
                    callables,
                    external_specifications,
                    models,
                    invariants,
                    completion ))
[@@delator.instrument] [@@delator.level debug]

let authenticate_loaded_with_policy ~external_targets ~solver_policy
    ~dependencies:candidates ~consumer =
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
      (match strict candidates with
      | Error _ as error -> error
      | Ok _ ->
          let rec verify (verified : staged_dependency list) = function
            | [] ->
                if
                  List.for_all
                    (fun (staged : staged_dependency) ->
                      Array.exists
                        (fun (import : Cmt_input.import) ->
                          String.equal import.unit_name staged.candidate.unit_name
                          && import.crc = Some staged.interface_digest)
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
                                     String.equal import.unit_name
                                       handle.unit_name
                                     && import.crc
                                        = Some handle.interface_digest)
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
                if not (Cmt_input.retained_preprocessing candidate) then
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
                           (fun (import : Cmt_input.import) ->
                             String.equal import.unit_name
                               staged.candidate.unit_name
                             && import.crc = Some staged.interface_digest)
                           candidate.imports)
                       direct_dependencies)
                then
                  error ~unit_name:candidate.unit_name
                    "dependency import CRC does not match the verified interface"
                else
                  match imported_environment_of_staged direct_dependencies with
                  | Error message -> error ~unit_name:candidate.unit_name message
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
                      ~solver_policy candidate
                  with
                      | Error _ as error -> error
                      | Ok
                          ( validated,
                            types,
                            callables,
                            external_specifications,
                            models,
                            invariants,
                            private_driver_completion ) ->
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
                              candidate;
                              interface_digest;
                              mode_signature_digest;
                              types;
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
          verify [] order)
[@@delator.instrument] [@@delator.level debug]

type loaded_verification = {
  loaded_environment : environment;
  loaded_driver : Verification_driver_private.report;
}

let run_loaded_consumer ~threads ~solver_policy ?external_specifications environment
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
                   String.equal import.unit_name handle.unit_name
                   && import.crc = Some handle.interface_digest)
                 consumer.imports)
          environment.handles;
    }
  in
  match imported_environment direct_environment with
  | Error message -> error ~unit_name:consumer.unit_name message
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
      let verification =
        if threads = 1 then
          Verification_driver_private.run_with_policy ~solver_policy ~imported
            ?external_specifications
            ~allow_public_parametric_signatures:
              (consumer.explicit_interface && environment.handles <> [])
            ~allow_imported_opens:(environment.handles <> []) consumer
        else
          Verification_driver_private.run_with_policy_and_threads ~threads
            ~solver_policy ~imported ?external_specifications
            ~allow_public_parametric_signatures:
              (consumer.explicit_interface && environment.handles <> [])
            ~allow_imported_opens:(environment.handles <> []) consumer
      in
      match verification with
      | Ok report ->
          Ok
            {
              loaded_environment = environment;
              loaded_driver = report;
            }
      | Error (Verification_driver_private.Frontend_error diagnostic) ->
          error ~unit_name:consumer.unit_name ~diagnostic
            (Printf.sprintf "frontend verification failed [%s]: %s"
               diagnostic.code diagnostic.message)
      | Error (Validation_error validation_error) ->
          error ~unit_name:consumer.unit_name
            ~diagnostic:(Sst_validation.to_diagnostic validation_error)
            (Sst_validation.error_to_string validation_error)
      | Error (Invariant_error invariant_error) ->
          internal_error ~unit_name:consumer.unit_name
            (Type_invariant.error_to_string invariant_error)
      | Error
          (Pipeline_error (Verification_pipeline.Engine_error engine_error)) ->
          internal_error ~unit_name:consumer.unit_name
            (Symbolic_executor_private.error_to_string engine_error)
      | Error
          (Pipeline_error
            (Verification_pipeline.Setup_error
              (Verification_pipeline.Internal_setup_error message)))
      | Error (Pipeline_error (Verification_pipeline.Solve_error message)) ->
          internal_error ~unit_name:consumer.unit_name message
      | Error
          (Pipeline_error
            (Verification_pipeline.Setup_error
              (Verification_pipeline.Solver_configuration_error solver_error)))
        ->
          error ~unit_name:consumer.unit_name
            (Solver_backend.error_to_string solver_error)
      | Error (Internal_error message) ->
          internal_error ~unit_name:consumer.unit_name message)
[@@delator.instrument] [@@delator.level debug]

let verify_loaded_with_policy ~threads ~solver_policy ~external_specifications
    ~external_targets ~dependencies ~consumer =
  match dependencies with
  | [] ->
      if Finite_formal_requirement.requires_authentication consumer then
        match strict_candidate ~require_public_interface:false consumer with
        | Error _ as error -> error
        | Ok _ ->
            run_loaded_consumer ~threads ~solver_policy ?external_specifications
              { issuer = process_issuer; handles = [] }
              consumer
      else
        run_loaded_consumer ~threads ~solver_policy ?external_specifications
          { issuer = process_issuer; handles = [] }
          consumer
  | _ -> (
      match
        authenticate_loaded_with_policy ~external_targets ~solver_policy
          ~dependencies ~consumer
      with
      | Error _ as error -> error
      | Ok (environment, consumer) ->
          run_loaded_consumer ~threads ~solver_policy ?external_specifications environment
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
      ~external_targets ~dependencies ~consumer
  with
  | Error _ as error -> error
  | Ok loaded ->
      let provenance =
        provenance loaded.loaded_environment
        |> List.map (fun (value : provenance) ->
               ( value.unit_name, value.interface_digest,
                 value.direct_dependencies, value.transitive_dependencies ))
      in
      Ok { driver = loaded.loaded_driver; provenance }
[@@delator.instrument] [@@delator.level info]

let driver result = result.driver
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
