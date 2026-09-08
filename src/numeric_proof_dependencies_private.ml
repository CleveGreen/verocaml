type reason =
  | Completion_mismatch
  | Missing_coordinator_evidence
  | Missing_local_identity of Sst.function_id
  | Missing_execution_evidence of Sst.function_id
  | Missing_broadcast_evidence of Sst.function_id
  | Unsupported_dependency of Sst.function_id
  | Native_bv_dependency of Sst.function_id
  | Logical_constant_context
  | Dependency_cycle of Sst.function_id

type dependency = {
  owner_artifact_full_key : string;
  definition : Sst.function_definition;
  compiler_uid : string;
  calls : Sst_validation.call_edge_descriptor list;
  broadcasts : Verification_proof_evidence_private.insertion list;
  trusted : bool;
}

type t = {
  owner_artifact_full_key : string;
  root : Sst.function_definition;
  dependencies : dependency list;
  trusted_dependencies : dependency list;
}

type imported_spec_leaf = {
  expression : Sst.expression;
  summary : Imported_callable.callable_snapshot;
  original_evidence : t;
}

type context = Ghost_context | Runtime_context

let ( let* ) = Result.bind

let imported_spec_leaf ~registration:(registration [@delator.skip]) ~origin:(origin [@delator.skip])
    ~evidence:(evidence [@delator.skip]) ~path ~interface_uid (expression [@delator.skip]) =
  let result =
    let* call = match Imported_callable.find_call registration expression with
      | Some call -> Ok call | None -> Error "The prerequisite view is not an authenticated imported call." in
    let summary=Imported_callable.call_summary call in
    let* () = if Imported_callable.artifact_full_key origin=evidence.owner_artifact_full_key
      && summary.provider_unit=origin.Cmt_input.unit_name
      && summary.binding_uid=interface_uid
      && (match expression.Sst.expression_desc with Direct_call {call_form=Specification_call;recursive=false;type_arguments=[];_} -> true | _ -> false)
      && List.exists (fun dependency -> Cmt_input.interface_value_uid_correlates origin ~path ~interface_uid ~implementation_uid:dependency.compiler_uid
        && dependency.owner_artifact_full_key=evidence.owner_artifact_full_key
        && dependency.definition.Sst.mode=Spec && not dependency.definition.recursive
        && dependency.definition.type_binders=[]) evidence.dependencies
      then Ok () else Error "The imported view does not match its original completed proof evidence and compiler identity." in
    Ok {expression;summary;original_evidence=evidence} in
  [%log.debug "authenticated imported numeric prerequisite proof leaf"
    ~provider:(Delator.Field.string origin.Cmt_input.unit_name)
    ~callable:(Delator.Field.string path)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason:(Delator.Field.string (match result with Ok _ -> "exact-completed-prerequisite" | Error reason -> reason))];
  result
[@@delator.instrument] [@@delator.level debug]

let scalar = function
  | Sst.Int | Mathematical_int | Bool | Unit -> true
  | _ -> false

let definition_expressions (definition : Sst.function_definition) =
  let predicates clauses = List.map (fun (clause : Sst.predicate_clause) -> clause.predicate.expression) clauses in
  let body = match definition.body with
    | Sst.Spec_definition body -> [body.expression]
    | Sst.Proof_body {body; _} | Checked_exec {body; _} -> [body.expression]
    | _ -> [] in
  body @ predicates definition.contracts.requires
  @ predicates definition.contracts.assertions
  @ List.map (fun (clause : Sst.ensures_clause) -> clause.predicate.expression) definition.contracts.ensures

let complete ~completion:(completion [@delator.skip])
    ~implementation:(implementation [@delator.skip])
    ~validated:(validated [@delator.skip]) ~context:(context [@delator.skip])
    ~imported_leaves:(imported_leaves [@delator.skip])
    ~views:(views [@delator.skip]) (root [@delator.skip]) =
  let program = Sst_validation.program validated in
  let result =
    if not (Verification_driver_private.completion_matches completion ~implementation ~validated)
      || not (List.exists (fun definition -> definition == root) program.functions)
      || not (List.for_all (fun view -> List.exists (fun definition -> definition == view) program.functions)
        views)
    then Error Completion_mismatch
    else if program.logical_constants <> [] then Error Logical_constant_context
    else
      let* evidence = match Verification_driver_private.proof_evidence completion with
        | Some evidence -> Ok evidence
        | None -> Error Missing_coordinator_evidence in
      let instances = Typedtree_adapter_private.Public.issued_callable_instances
          ~structure:implementation.Cmt_input.structure ~program in
      let call_edges = Sst_validation.call_edge_descriptors validated in
      let local_uid definition =
        match List.filter (fun instance ->
            Typedtree_adapter_private.Public.callable_instance_definition instance == definition) instances with
        | [instance] -> Ok (Typedtree_adapter_private.Public.callable_instance_binding_uid instance)
        | _ -> Error (Missing_local_identity definition.Sst.function_id) in
      let execution definition =
        match List.filter (fun item -> Option.fold ~none:false ~some:(fun candidate -> candidate == definition)
              item.Verification_proof_evidence_private.definition)
            (Verification_proof_evidence_private.functions evidence) with
        | [item] -> Ok item
        | _ -> Error (Missing_execution_evidence definition.Sst.function_id) in
      let rec expression_dependencies definition accumulated = function
        | [] -> Ok accumulated
        | expression :: rest ->
            let* accumulated =
              if not (scalar expression.Sst.typ) then Error (Unsupported_dependency definition.Sst.function_id)
              else
              match expression.Sst.expression_desc with
              | Sst.Direct_call _
                when Spec_function_sst_private.lambda expression <> None
                  || Spec_function_sst_private.is_reference expression
                  || Spec_function_sst_private.application expression <> None ->
                  Error (Unsupported_dependency definition.Sst.function_id)
              | Sst.Direct_call _ when List.exists (fun leaf -> leaf.expression == expression) imported_leaves ->
                  Ok accumulated
              | Sst.Direct_call {callee; _} -> (
                  match Sst_validation.find_callable validated callee with
                  | Some descriptor -> Ok (Sst_validation.callable_definition descriptor :: accumulated)
                  | None -> Error (Missing_local_identity callee))
              | Forall quantifier | Exists quantifier
                when not (scalar quantifier.quantifier_binder.typ) ->
                  Error (Unsupported_dependency definition.function_id)
              | Sst.Callback_call _ | Callback_requires _ | Callback_ensures _
              | Field_read _ | Field_write _ | Shared_scalar_field_write _
              | Owned_tree_nested_write _ | Owned_tree_rebase _ | Let_mutable _
              | Mutable_read _ | Mutable_write _ | Old _ | Use_type_invariant _
              | Logical_constant_reference _ -> Error (Unsupported_dependency definition.Sst.function_id)
              | Reveal id | Reveal_with_fuel {function_id = id; _} -> (
                  match Sst_validation.find_callable validated id with
                  | Some descriptor -> Ok (Sst_validation.callable_definition descriptor :: accumulated)
                  | None -> Error (Missing_local_identity id))
              | Symbolic_application application ->
                  let symbol = Symbolic_application_private.declaration application in
                  (match List.filter (fun candidate -> match candidate.Sst.body with
                      | Sst.Symbolic_declaration declaration -> Symbolic_application_private.same_declaration declaration symbol
                      | _ -> false) program.functions with
                  | [candidate] -> Ok (candidate :: accumulated)
                  | _ -> Error (Unsupported_dependency definition.function_id))
              | _ -> Ok accumulated in
            expression_dependencies definition accumulated (Sst_callback_private.expression_children expression @ rest)
      in
      let rec visit active completed definition =
        if List.exists (fun item -> item.definition == definition) completed then Ok completed
        else if List.exists (fun candidate -> candidate == definition) active then
          Error (Dependency_cycle definition.Sst.function_id)
        else
          let* compiler_uid = local_uid definition in
          let supported = not definition.recursive && definition.type_binders = []
            && scalar definition.result_type
            && List.for_all (function Sst.Value_parameter {pattern; optional_default = None; _} -> scalar pattern.typ | _ -> false) definition.parameters in
          let* trusted, has_execution =
            if not supported then Error (Unsupported_dependency definition.function_id)
            else match definition.mode, definition.body with
            | Sst.Proof, Sst.Proof_body _ -> Ok (false, true)
            | Proof, Trusted_external_body (Authenticated_external_body _) -> Ok (true, false)
            | Spec, (Spec_definition _ | Symbolic_declaration _) -> Ok (false, false)
            | Exec, Checked_exec _ when context = Runtime_context -> Ok (false, true)
            | Exec, Trusted_external_body (Authenticated_external_body _)
              when context = Runtime_context -> Ok (true, false)
            | _ -> Error (Unsupported_dependency definition.function_id) in
          let calls = List.filter (fun edge ->
              Sst_validation.callable_definition (Sst_validation.call_edge_caller edge) == definition) call_edges in
          let callees = List.map (fun edge -> Sst_validation.callable_definition (Sst_validation.call_edge_callee edge)) calls
            |> List.filter (fun definition -> not (List.exists (fun leaf -> leaf.summary.definition.function_id=definition.Sst.function_id) imported_leaves)) in
          let* callees = expression_dependencies definition callees (definition_expressions definition) in
          let* broadcasts, dynamic_trust =
            if not has_execution then Ok ([], [])
            else
              let* item = execution definition in
              let* () =
                if Vir.function_execution_has_native_bv_projection item.execution
                then (
                  [%log.error "refused native-assisted numeric proof dependency"
                    ~provider:
                      (Delator.Field.string implementation.Cmt_input.unit_name)
                    ~function_name:
                      (Delator.Field.string definition.function_id.function_name)
                    ~decision:
                      (Delator.Field.string
                         "native-proof-is-not-law-authority")];
                  Error (Native_bv_dependency definition.function_id))
                else Ok ()
              in
              let* broadcasts = List.fold_left (fun result obligation ->
                  let* inserted = result in
                  match obligation.Verification_proof_evidence_private.base, obligation.broadcasts with
                  | Some _, Some items -> Ok (List.rev_append items inserted)
                  | _ -> Error (Missing_broadcast_evidence definition.function_id)) (Ok []) item.obligations in
              let* dynamic_trust = List.fold_left (fun result use ->
                  let* dependencies = result in
                  match use with
                  | Vir.Trusted_external_body_use use ->
                      let id = {Sst.function_index = use.function_ref.function_index; function_name = use.function_ref.function_name} in
                      (match Sst_validation.find_callable validated id with
                      | Some descriptor -> Ok (Sst_validation.callable_definition descriptor :: dependencies)
                      | None -> Error (Missing_local_identity id))
                  | _ -> Error (Unsupported_dependency definition.function_id)) (Ok []) item.execution.trusted_summary_uses in
              Ok (List.rev broadcasts, dynamic_trust) in
          let* broadcast_callees = List.fold_left (fun result insertion ->
              let* definitions = result in
              match insertion.Verification_proof_evidence_private.theorem with
              | Some theorem -> Ok (theorem :: definitions)
              | None -> Error (Missing_broadcast_evidence definition.function_id)) (Ok []) broadcasts in
          let* completed = List.fold_left (fun result callee ->
              let* completed = result in visit (definition :: active) completed callee)
              (Ok completed) (callees @ broadcast_callees @ dynamic_trust) in
          [%log.trace "completed local numeric proof dependency"
            ~function_name:(Delator.Field.string definition.function_id.function_name)
            ~compiler_uid:(Delator.Field.string compiler_uid)
            ~call_edges:(Delator.Field.int (List.length calls))
            ~broadcast_insertions:(Delator.Field.int (List.length broadcasts))
            ~trusted:(Delator.Field.bool trusted)];
          Ok ({owner_artifact_full_key=Imported_callable.artifact_full_key implementation;
            definition; compiler_uid; calls; broadcasts; trusted} :: completed)
      in
      let imported_dependencies = List.concat_map (fun leaf -> leaf.original_evidence.dependencies) imported_leaves
        |> List.sort_uniq (fun (a : dependency) (b : dependency) -> compare (a.owner_artifact_full_key,a.compiler_uid) (b.owner_artifact_full_key,b.compiler_uid)) in
      let* dependencies = visit [] imported_dependencies root in
      let dependencies = List.sort (fun (left : dependency) (right : dependency) ->
          compare (left.owner_artifact_full_key,left.definition.function_id.function_index)
            (right.owner_artifact_full_key,right.definition.function_id.function_index)) dependencies in
      Ok {owner_artifact_full_key=Imported_callable.artifact_full_key implementation;
        root; dependencies; trusted_dependencies = List.filter (fun dependency -> dependency.trusted) dependencies}
  in
  let[@log_value.debug] reason_class =
    match result with
    | Ok _ -> "complete-local-closure"
    | Error Completion_mismatch -> "completion-mismatch"
    | Error Missing_coordinator_evidence -> "missing-coordinator-evidence"
    | Error (Missing_local_identity _) -> "missing-local-identity"
    | Error (Missing_execution_evidence _) -> "missing-execution-evidence"
    | Error (Missing_broadcast_evidence _) -> "missing-broadcast-evidence"
    | Error (Unsupported_dependency _) -> "unsupported-dependency"
    | Error (Native_bv_dependency _) -> "native-bv-dependency"
    | Error Logical_constant_context -> "logical-constant-context"
    | Error (Dependency_cycle _) -> "dependency-cycle"
  in
  let[@log_value.debug] dependency_view =
    let dependencies = match result with Ok evidence -> evidence.dependencies | Error _ -> [] in
    let shown = dependencies |> List.filteri (fun index _ -> index < 16)
      |> List.map (fun dependency ->
          Delator.Field.map
            ["function", Delator.Field.string dependency.definition.function_id.function_name;
             "compiler_uid", Delator.Field.string dependency.compiler_uid;
             "trusted", Delator.Field.bool dependency.trusted]) in
    Delator.Field.seq ~dropped:(Int.max 0 (List.length dependencies - 16)) shown
  in
  [%log.debug "completed numeric theorem dependency admission"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason_class:(Delator.Field.string (reason_class [@log_value.debug]))
    ~dependency_count:(Delator.Field.int (match result with Ok evidence -> List.length evidence.dependencies | Error _ -> 0))
    ~trusted_dependency_count:(Delator.Field.int (match result with Ok evidence -> List.length evidence.trusted_dependencies | Error _ -> 0))
    ~dependencies:(dependency_view [@log_value.debug])
    ~runtime_refinement:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]

let complete_unsigned_range ~completion ~implementation ~validated (statement : Numeric_law_match_private.unsigned_range) =
  complete ~completion ~implementation ~validated ~context:Ghost_context
    ~imported_leaves:[]
    ~views:[statement.Numeric_law_match_private.view] statement.declaration.definition

let complete_signed_view ~completion ~implementation ~validated (statement : Numeric_law_match_private.signed_view) =
  complete ~completion ~implementation ~validated ~context:Ghost_context
    ~imported_leaves:[]
    ~views:[statement.Numeric_law_match_private.view; statement.unsigned.view] statement.declaration.definition

let complete_definition ~completion ~implementation ~validated ~context definition =
  complete ~completion ~implementation ~validated ~context ~imported_leaves:[] ~views:[] definition

let complete_definition_with_imported ~completion ~implementation ~validated ~imported_leaves definition =
  complete ~completion ~implementation ~validated ~context:Ghost_context ~imported_leaves ~views:[] definition
