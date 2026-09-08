type reason =
  | Missing_local_identity of Sst.function_id
  | Missing_successful_evidence of Sst.function_id
  | Missing_broadcast_evidence of Sst.function_id
  | Unsupported_dependency of Sst.function_id
  | Logical_constant_context
  | Dependency_cycle of Sst.function_id

type dependency = {
  definition : Sst.function_definition;
  compiler_uid : string;
  trusted : bool;
  full_key : string;
}

type t = { dependencies : dependency list; full_key : string }

type dependency_evidence = {
  function_index : int;
  function_name : string;
  compiler_uid : string;
  trusted : bool;
  full_key : string;
}

let ( let* ) = Result.bind

let scalar = function
  | Sst.Int | Mathematical_int | Bool | Unit -> true
  | _ -> false

let predicates clauses =
  List.map
    (fun (clause : Sst.predicate_clause) -> clause.predicate.expression)
    clauses

let definition_expressions (definition : Sst.function_definition) =
  let body =
    match definition.body with
    | Sst.Spec_definition body -> [ body.expression ]
    | Sst.Proof_body { body; _ } | Checked_exec { body; _ } ->
        [ body.expression ]
    | _ -> []
  in
  body @ predicates definition.contracts.requires
  @ predicates definition.contracts.assertions
  @ List.map
      (fun (clause : Sst.ensures_clause) -> clause.predicate.expression)
      definition.contracts.ensures

let function_id_key (definition : Sst.function_definition) =
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-function-id.v1"
    [ string_of_int definition.function_id.function_index;
      definition.function_id.function_name ]

let obligation_evidence_key
    (obligation : Verification_proof_evidence_private.obligation) =
  let broadcast_keys =
    match obligation.broadcasts with
    | None -> [ "missing" ]
    | Some insertions ->
        List.map
          (fun (insertion : Verification_proof_evidence_private.insertion) ->
            match insertion.theorem with
            | None -> "missing-theorem"
            | Some theorem -> function_id_key theorem)
          insertions
  in
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-proof-obligation-evidence.v1"
    [ Vir_identity_private.obligation obligation.materialized;
      (match obligation.base with
      | None -> "missing"
      | Some base -> Vir_identity_private.obligation base);
      Numeric_receipt_private.list broadcast_keys ]

let dependency_key ~owner_artifact_full_key ~compiler_uid ~trusted
    evidence definition =
  let trusted_use_keys =
    match evidence with
    | None -> []
    | Some (evidence :
        Verification_proof_evidence_private.function_evidence) ->
        List.map
          (function
            | Vir.Trusted_external_body_use use ->
                Numeric_receipt_private.encode
                  ~schema:"verocaml.numeric-trusted-use.v1"
                  [ string_of_int use.function_ref.function_index;
                    use.function_ref.function_name ]
            | Trusted_external_specification_use _ ->
                "unsupported-external-specification"
            | Trusted_external_target_specification_use _ ->
                "unsupported-external-target-specification")
          evidence.execution.trusted_summary_uses
  in
  let obligation_keys =
    match evidence with
    | None -> [ "validated-definition-only" ]
    | Some evidence ->
        List.map obligation_evidence_key evidence.obligations
  in
  Numeric_receipt_private.encode
    ~schema:"verocaml.numeric-proof-dependency.v1"
    [ owner_artifact_full_key; function_id_key definition; compiler_uid;
      string_of_bool trusted;
      Numeric_receipt_private.list obligation_keys;
      Numeric_receipt_private.list trusted_use_keys ]

let reason_name = function
  | Missing_local_identity _ -> "missing-local-identity"
  | Missing_successful_evidence _ -> "missing-successful-evidence"
  | Missing_broadcast_evidence _ -> "missing-broadcast-evidence"
  | Unsupported_dependency _ -> "unsupported-dependency"
  | Logical_constant_context -> "logical-constant-context"
  | Dependency_cycle _ -> "dependency-cycle"

let complete ~implementation ~validated ~successful ~evidence_for root =
  let program = Sst_validation.program validated in
  let result =
    if program.logical_constants <> [] then Error Logical_constant_context
    else if not (List.exists (fun definition -> definition == root) program.functions)
    then Error (Missing_local_identity root.function_id)
    else
      let instances =
        Typedtree_adapter_private.Public.issued_callable_instances
          ~structure:implementation.Cmt_input.structure ~program
      in
      let call_edges = Sst_validation.call_edge_descriptors validated in
      let local_uid definition =
        match
          List.filter
            (fun instance ->
              Typedtree_adapter_private.Public.callable_instance_definition
                instance
              == definition)
            instances
        with
        | [ instance ] ->
            Ok
              (Typedtree_adapter_private.Public.callable_instance_binding_uid
                 instance)
        | _ -> Error (Missing_local_identity definition.Sst.function_id)
      in
      let rec expression_dependencies definition accumulated = function
        | [] -> Ok accumulated
        | expression :: rest ->
            let* accumulated =
              if not (scalar expression.Sst.typ) then
                Error (Unsupported_dependency definition.Sst.function_id)
              else
                match expression.Sst.expression_desc with
                | Sst.Direct_call _
                  when Spec_function_sst_private.lambda expression <> None
                       || Spec_function_sst_private.is_reference expression
                       || Spec_function_sst_private.application expression
                          <> None ->
                    Error (Unsupported_dependency definition.function_id)
                | Sst.Direct_call { callee; _ } -> (
                    match Sst_validation.find_callable validated callee with
                    | Some descriptor ->
                        Ok
                          (Sst_validation.callable_definition descriptor
                          :: accumulated)
                    | None -> Error (Missing_local_identity callee))
                | Forall quantifier | Exists quantifier
                  when not (scalar quantifier.quantifier_binder.typ) ->
                    Error (Unsupported_dependency definition.function_id)
                | Sst.Callback_call _ | Callback_requires _
                | Callback_ensures _ | Field_read _ | Field_write _
                | Shared_scalar_field_write _ | Owned_tree_nested_write _
                | Owned_tree_rebase _ | Let_mutable _ | Mutable_read _
                | Mutable_write _ | Old _ | Use_type_invariant _
                | Logical_constant_reference _ ->
                    Error (Unsupported_dependency definition.function_id)
                | Reveal id | Reveal_with_fuel { function_id = id; _ } -> (
                    match Sst_validation.find_callable validated id with
                    | Some descriptor ->
                        Ok
                          (Sst_validation.callable_definition descriptor
                          :: accumulated)
                    | None -> Error (Missing_local_identity id))
                | Symbolic_application application ->
                    let symbol =
                      Symbolic_application_private.declaration application
                    in
                    (match
                       List.filter
                         (fun candidate ->
                           match candidate.Sst.body with
                           | Sst.Symbolic_declaration declaration ->
                               Symbolic_application_private.same_declaration
                                 declaration symbol
                           | _ -> false)
                         program.functions
                     with
                    | [ candidate ] -> Ok (candidate :: accumulated)
                    | _ ->
                        Error
                          (Unsupported_dependency definition.function_id))
                | _ -> Ok accumulated
            in
            expression_dependencies definition accumulated
              (Sst_callback_private.expression_children expression @ rest)
      in
      let rec visit active completed definition =
        if List.exists (fun item -> item.definition == definition) completed then
          Ok completed
        else if List.exists (fun candidate -> candidate == definition) active then
          Error (Dependency_cycle definition.Sst.function_id)
        else
          let* compiler_uid = local_uid definition in
          let supported =
            (not definition.recursive)
            && definition.type_binders = []
            && scalar definition.result_type
            && List.for_all
                 (function
                   | Sst.Value_parameter
                       { pattern; optional_default = None; _ } ->
                       scalar pattern.typ
                   | _ -> false)
                 definition.parameters
          in
          let* trusted, needs_successful_evidence =
            if not supported then
              Error (Unsupported_dependency definition.function_id)
            else
              match (definition.mode, definition.body) with
              | Sst.Proof, Sst.Proof_body _ -> Ok (false, true)
              | Proof,
                Trusted_external_body (Authenticated_external_body _) ->
                  Ok (true, false)
              | Spec, (Spec_definition _ | Symbolic_declaration _) ->
                  Ok (false, false)
              | _ -> Error (Unsupported_dependency definition.function_id)
          in
          let* evidence =
            if not needs_successful_evidence then Ok None
            else if not (successful definition) then
              Error (Missing_successful_evidence definition.function_id)
            else
              match evidence_for definition with
              | Some evidence -> Ok (Some evidence)
              | None -> Error (Missing_successful_evidence definition.function_id)
          in
          let calls =
            List.filter
              (fun edge ->
                Sst_validation.callable_definition
                  (Sst_validation.call_edge_caller edge)
                == definition)
              call_edges
          in
          let callees =
            List.map
              (fun edge ->
                Sst_validation.callable_definition
                  (Sst_validation.call_edge_callee edge))
              calls
          in
          let* callees =
            expression_dependencies definition callees
              (definition_expressions definition)
          in
          let* broadcasts =
            match evidence with
            | None -> Ok []
            | Some
                (evidence :
                  Verification_proof_evidence_private.function_evidence) ->
                List.fold_left
                  (fun result obligation ->
                    let* inserted = result in
                    match
                      ( obligation.Verification_proof_evidence_private.base,
                        obligation.broadcasts )
                    with
                    | Some _, Some items -> Ok (List.rev_append items inserted)
                    | _ ->
                        Error
                          (Missing_broadcast_evidence definition.function_id))
                  (Ok []) evidence.obligations
          in
          let* broadcast_callees =
            List.fold_left
              (fun result insertion ->
                let* definitions = result in
                match
                  insertion.Verification_proof_evidence_private.theorem
                with
                | Some theorem -> Ok (theorem :: definitions)
                | None ->
                    Error
                      (Missing_broadcast_evidence definition.function_id))
              (Ok []) broadcasts
          in
          let* dynamic_trust =
            let uses =
              match evidence with
              | None -> []
              | Some
                  (evidence :
                    Verification_proof_evidence_private.function_evidence) ->
                  evidence.execution.trusted_summary_uses
            in
            List.fold_left
              (fun result -> function
                | Vir.Trusted_external_body_use use ->
                    let* dependencies = result in
                    let id =
                      { Sst.function_index = use.function_ref.function_index;
                        function_name = use.function_ref.function_name }
                    in
                    (match Sst_validation.find_callable validated id with
                    | Some descriptor ->
                        Ok
                          (Sst_validation.callable_definition descriptor
                          :: dependencies)
                    | None -> Error (Missing_local_identity id))
                | Vir.Trusted_external_specification_use _
                | Trusted_external_target_specification_use _ ->
                    Error (Unsupported_dependency definition.function_id))
              (Ok []) uses
          in
          let* completed =
            List.fold_left
              (fun result callee ->
                let* completed = result in
                visit (definition :: active) completed callee)
              (Ok completed)
              (callees @ broadcast_callees @ dynamic_trust)
          in
          let owner_artifact_full_key =
            Imported_callable.artifact_full_key implementation
          in
          let dependency =
            { definition; compiler_uid; trusted;
              full_key =
                dependency_key ~owner_artifact_full_key ~compiler_uid ~trusted
                  evidence definition }
          in
          [%log.trace "closed scoped numeric proof dependency"
            ~stage:(Delator.Field.string "numeric-prior-law-closure")
            ~function_name:
              (Delator.Field.string definition.function_id.function_name)
            ~call_edges:(Delator.Field.int (List.length calls))
            ~broadcasts:(Delator.Field.int (List.length broadcasts))
            ~trusted:(Delator.Field.bool trusted)];
          Ok (dependency :: completed)
      in
      let* dependencies = visit [] [] root in
      let dependencies =
        List.sort
          (fun left right ->
            Int.compare left.definition.function_id.function_index
              right.definition.function_id.function_index)
          dependencies
      in
      let full_key =
        Numeric_receipt_private.encode
          ~schema:"verocaml.numeric-proof-closure-walk.v1"
          [ Imported_callable.artifact_full_key implementation;
            function_id_key root;
            Numeric_receipt_private.list
              (List.map
                 (fun (dependency : dependency) -> dependency.full_key)
                 dependencies) ]
      in
      Ok { dependencies; full_key }
  in
  [%log.debug "completed scoped numeric proof dependency walk"
    ~stage:(Delator.Field.string "numeric-prior-law-closure")
    ~root:(Delator.Field.string root.function_id.function_name)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason:
      (Delator.Field.string
         (match result with Ok _ -> "complete" | Error reason -> reason_name reason))
    ~dependency_count:
      (Delator.Field.int
         (match result with Ok closure -> List.length closure.dependencies | Error _ -> 0))];
  result
[@@delator.instrument] [@@delator.level debug]

let authorizes_definition (closure : t) definition =
  List.exists (fun item -> item.definition == definition) closure.dependencies

let full_key (closure : t) = closure.full_key
let dependency_count (closure : t) = List.length closure.dependencies

let trusted_dependency_count (closure : t) =
  List.fold_left
    (fun count (dependency : dependency) ->
      if dependency.trusted then count + 1 else count)
    0 closure.dependencies

let dependency_evidence (closure : t) =
  List.map
    (fun (dependency : dependency) ->
      { function_index = dependency.definition.Sst.function_id.function_index;
        function_name = dependency.definition.function_id.function_name;
        compiler_uid = dependency.compiler_uid;
        trusted = dependency.trusted; full_key = dependency.full_key })
    closure.dependencies
