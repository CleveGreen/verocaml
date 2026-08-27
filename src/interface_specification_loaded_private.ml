open Interface_specification_environment_private
open Interface_specification_candidate_private

type error = Interface_specification_environment_private.error

type result = {
  driver : Verification_driver_private.report;
  provenance :
    (string * string * (string * string) list * (string * string) list) list;
}

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

let snapshot_type validated descriptor =
  let definition = Sst_validation.type_definition descriptor in
  let parametric_descriptor =
    (Sst_validation.program validated).Sst.parametric_adts
    |> List.find_opt (fun candidate ->
           Parametric_adt.type_id candidate = definition.Sst.type_id)
  in
  match
    Sst_validation.visibility_representation
      (Sst_validation.type_visibility descriptor)
  with
  | Sst.Revealed ->
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
  | Sst.Abstract_with_evidence _ ->
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
        match
          Sst_validation.visibility_representation
            (Sst_validation.type_visibility descriptor)
        with
        | Sst.Revealed ->
            not
              (surface_type_kind_is_public surface binders
                 definition.type_kind)
        | Sst.Abstract_with_evidence _ -> false)
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
      |> List.map (snapshot_type validated)
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
      let type_descriptors = Sst_validation.type_descriptors validated in
      let local_parametric_types =
        (Sst_validation.program validated).Sst.parametric_adts
        |> List.filter (fun descriptor ->
               match Parametric_adt.provenance descriptor with
               | Parametric_adt.Local _ -> true
               | Pinned_option _ | Pinned_list _ | Pinned_result _ -> false)
      in
      let canonical_public_type descriptor =
        let type_id = Sst_validation.type_id descriptor in
        match
          List.find_opt
            (fun parametric ->
              Parametric_adt.type_id parametric = type_id)
            local_parametric_types
        with
        | None -> true
        | Some parametric -> Parametric_adt.type_id parametric = type_id
      in
      let public_linked_types =
        List.filter
          (fun descriptor ->
            public_linked_type descriptor
            && canonical_public_type descriptor)
          type_descriptors
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
                  Result.map
                    (fun invariants -> (types, callables, models, invariants))
                    (snapshot_invariants ~unit_name surface validated))))
let provider_verification_entries = ref 0

let verify_candidate ?imported ~solver_policy candidate =
  incr provider_verification_entries;
  let unit_name = candidate.Cmt_input.unit_name in
  let reject message = error ~unit_name message in
  match
    Verification_driver_private.run_with_policy ~solver_policy
      ~allow_imported_opens:true
      ~allow_public_parametric_signatures:candidate.explicit_interface
      ?imported candidate
  with
  | Error (Verification_driver_private.Frontend_error diagnostic) ->
      reject
        (Printf.sprintf "frontend verification failed [%s]: %s" diagnostic.code
           diagnostic.message)
  | Error (Validation_error validation_error) ->
      reject (Sst_validation.error_to_string validation_error)
  | Error (Invariant_error invariant_error) ->
      reject (Type_invariant.error_to_string invariant_error)
  | Error (Pipeline_error _) -> reject "private verification pipeline rejected provider"
  | Error (Internal_error message) -> reject message
  | Ok report ->
      let validated = Verification_driver_private.validated report in
      let axiomatic =
        List.exists
          (fun descriptor ->
            match Sst_validation.feature_requirement descriptor with
            | Sst_validation.External_specification_trust
            | Trusted_external_body ->
                true
            | Specification_semantics | Proof_semantics
            | Owned_tree_reconstruction | Direct_recursion ->
                false)
          (Sst_validation.feature_descriptors validated)
      in
      if axiomatic then
        reject
          "axiomatic trusted declarations are not exportable under the selected interface policy"
      else
        match Verification_driver_private.verified_completion report with
        | None -> reject "retained provider lacks private-driver completion"
        | Some completion -> (
            match verified_snapshot ~unit_name ~candidate validated with
            | Error _ as error -> error
            | Ok (types, callables, models, invariants) ->
                Ok
                  ( validated,
                    types,
                    callables,
                    models,
                    invariants,
                    completion ))

let authenticate_loaded_with_policy ~solver_policy ~dependencies:candidates
    ~consumer =
  match graph_order candidates consumer with
  | Error _ as error -> error
  | Ok order ->
      let rec strict = function
        | [] -> strict_candidate ~require_public_interface:false consumer
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
                        Ok ({ issuer = process_issuer; handles = issued }, consumer)
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
                      match
                        verify_candidate ~imported ~solver_policy candidate
                      with
                      | Error _ as error -> error
                      | Ok
                          ( validated,
                            types,
                            callables,
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
                              models;
                              invariants;
                              semantic_snapshot = validated;
                              private_driver_completion;
                              direct_dependencies;
                            }
                          in
                          verify (verified @ [ staged ]) rest)
          in
          verify [] order)

type loaded_verification = {
  loaded_environment : environment;
  loaded_driver : Verification_driver_private.report;
}

let run_loaded_consumer ~threads ~solver_policy ?external_specifications environment
    (consumer : Cmt_input.implementation) =
  let direct_environment =
    {
      issuer = process_issuer;
      handles =
        List.filter
          (fun (handle : handle) ->
            Array.exists
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
            (Sst_validation.error_to_string validation_error)
      | Error (Invariant_error invariant_error) ->
          error ~unit_name:consumer.unit_name
            (Type_invariant.error_to_string invariant_error)
      | Error
          (Pipeline_error (Verification_pipeline.Engine_error engine_error)) ->
          error ~unit_name:consumer.unit_name
            (Symbolic_executor_private.error_to_string engine_error)
      | Error
          (Pipeline_error
            (Verification_pipeline.Setup_error
              (Verification_pipeline.Internal_setup_error message)))
      | Error (Pipeline_error (Verification_pipeline.Solve_error message)) ->
          error ~unit_name:consumer.unit_name message
      | Error
          (Pipeline_error
            (Verification_pipeline.Setup_error
              (Verification_pipeline.Solver_configuration_error solver_error)))
        ->
          error ~unit_name:consumer.unit_name
            (Solver_backend.error_to_string solver_error)
      | Error (Internal_error message) ->
          error ~unit_name:consumer.unit_name message)

let verify_loaded_with_policy ~threads ~solver_policy ~external_specifications ~dependencies
    ~consumer =
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
        authenticate_loaded_with_policy ~solver_policy ~dependencies ~consumer
      with
      | Error _ as error -> error
      | Ok (environment, consumer) ->
          run_loaded_consumer ~threads ~solver_policy ?external_specifications environment
            consumer)


let authenticate ~solver_policy ~dependencies ~consumer =
  authenticate_loaded_with_policy ~solver_policy ~dependencies ~consumer

let verify ~threads ~solver_policy ~external_specifications ~consumer ~dependencies =
  match
    verify_loaded_with_policy ~threads ~solver_policy ~external_specifications ~dependencies
      ~consumer
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

let driver result = result.driver
let provenance result = result.provenance
let error_unit_name (error : error) = error.unit_name
let error_message (error : error) = error.message
let error_diagnostic =
  Interface_specification_environment_private.error_diagnostic
let error = Interface_specification_environment_private.error

module For_testing = struct
  let reset_provider_verification_entries () =
    provider_verification_entries := 0

  let provider_verification_entries () = !provider_verification_entries
end
