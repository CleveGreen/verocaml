type authority = Checked_implementation | Explicit_external_body
type t = {
  law : Numeric_ghost_law_private.t;
  executable : Sst.function_definition;
  compiler_uid : string;
  interface_uid : string;
  authority : authority;
  trusted_dependency_keys : string list;
  full_key : string;
}
let ( let* ) = Result.bind
let parameter definition = match definition.Sst.parameters with
  | [Sst.Value_parameter {label=None; optional_default=None; pattern={pattern_desc=Bind binding;_}}]
    when definition.type_binders=[] && not definition.recursive -> Some binding
  | _ -> None
let variable binding expression = match expression.Sst.expression_desc with
  | Variable {binding=actual;_} -> binding=actual
  | _ -> false
let lift predicate expression = match expression.Sst.typ, expression.expression_desc with
  | Mathematical_int, Lift_runtime_int argument when argument.typ = Int -> predicate argument
  | _ -> false
let admit ~completion:(completion [@delator.skip]) ~implementation:(implementation [@delator.skip])
    ~validated:(validated [@delator.skip]) ~law:(law [@delator.skip]) (declaration [@delator.skip]) =
  let result =
    let role = declaration.Numeric_semantics_correlation_private.role in
    let trusted_request = role.numeric_role_source.role_identity = "trusted-runtime-view" in
    let* () = if (role.numeric_role_source.role_identity = "runtime-view" || trusted_request)
      && role.numeric_role_source.role_schema = "numeric-role.v1"
      && Verification_driver_private.completion_matches completion ~implementation ~validated
      && (law.Numeric_ghost_law_private.meaning = Unsigned_range || law.meaning = Signed_view)
      && role.numeric_role_semantics_uid = law.role.callable_uid
      && role.numeric_role_semantics_owner_unit = law.role.callable_owner.owner_unit
      && role.numeric_role_semantics_owner_cmi_full_key = law.role.callable_owner.owner_cmi_full_key
      && role.numeric_role_carrier_uid = law.carrier.carrier_claim.carrier_uid
      && role.numeric_role_carrier_owner_cmi_full_key = law.carrier.carrier_claim.owner.owner_cmi_full_key
      && (match role.numeric_role_callable_shape with
          | Some {parameters=[Numeric_callable_domain_private.Carrier];result=Runtime_integer} -> true
          | _ -> false)
      then Ok () else Error "Runtime equivalence needs the exact admitted view and completed provider." in
    let* binding = Numeric_artifact_binding_private.correlate implementation law.carrier.carrier_claim in
    let* facts = Numeric_artifact_binding_private.facts binding in
    let* () = if facts.binding_full_key = law.carrier.binding_full_key then Ok ()
      else Error "The runtime implementation and numeric law belong to different artifacts." in
    let* executable = match declaration.callable_definition with Some executable -> Ok executable
      | None -> Error "The runtime view has no exact completed executable definition." in
    let view = declaration.definition in
    let* argument, view_argument = match parameter executable, parameter view with
      | Some argument, Some view_argument when executable.mode=Exec && view.mode=Spec
        && executable.result_type=Int && executable.contracts.requires=[] && view.contracts.requires=[]
        && argument.typ=view_argument.typ -> Ok (argument,view_argument)
      | _ -> Error "Runtime view equivalence currently requires an unconditional, nonrecursive scalar function." in
    let _ = view_argument in
    let* () = match Sst_validation.find_callable validated executable.function_id with
      | Some callable when Sst_validation.result_instance_mode validated callable=Sst.Exec_instance
        && List.for_all (fun (index,parameter) -> Sst_validation.formal_instance_mode validated callable index parameter=Sst.Exec_instance)
          (List.mapi (fun index parameter -> index,parameter) executable.parameters) -> Ok ()
      | _ -> Error "Runtime numeric equivalence requires an executable parameter and result, not erased ghost or tracked values." in
    let* authority = match executable.body with
      | Checked_exec {provenance=Authenticated_typedtree _;_} -> Ok Checked_implementation
      | Trusted_external_body (Authenticated_external_body _) when trusted_request -> Ok Explicit_external_body
      | _ -> Error "Runtime equivalence needs a checked implementation or explicit external-body authority." in
    let projected expression =
      let direct expression = match expression.Sst.expression_desc with
        | Direct_call {callee;call_form=Specification_call;type_arguments=[];recursive=false;
            arguments=[Value_argument {label=None;value}]} -> callee=view.function_id && variable argument value
        | _ -> false in
      match view.result_type with
      | Mathematical_int -> expression.Sst.typ=Mathematical_int && direct expression
      | Int -> lift direct expression
      | _ -> false in
    let* clause = match List.find_opt (fun (clause : Sst.ensures_clause) ->
      match clause.binder,clause.predicate.stage,clause.predicate.expression.expression_desc with
      | Some {pattern_desc=Bind result;typ=Int;_}, Logical, Compare (Equal,a,b) ->
          (lift (variable result) a && projected b) || (projected a && lift (variable result) b)
      | _ -> false) executable.contracts.ensures with
      | Some clause -> Ok clause
      | None -> Error "The runtime contract must equate its actual result with the exact mathematical view." in
    let* closure = match Numeric_proof_dependencies_private.complete_definition ~completion ~implementation
      ~validated ~context:Runtime_context executable with
      | Ok closure -> Ok closure
      | Error _ -> Error "The runtime equivalence has an unsupported or circular proof dependency." in
    let* () = if List.for_all (fun (dependency : Numeric_proof_dependencies_private.dependency) ->
      not (dependency.trusted && dependency.definition.mode=Exec)
      || (trusted_request && dependency.definition == executable)) closure.dependencies then Ok ()
      else Error "An ordinary external-body contract does not establish purity, termination or exception behavior for runtime replacement." in
    let program = Sst_validation.program validated in
    let instances = Typedtree_adapter_private.Public.issued_callable_instances ~structure:implementation.structure ~program in
    let* instance = match List.filter (fun instance ->
      Typedtree_adapter_private.Public.callable_instance_definition instance == executable
      && Cmt_input.interface_value_uid_correlates implementation ~path:role.numeric_role_callable_path
        ~interface_uid:role.numeric_role_callable_uid
        ~implementation_uid:(Typedtree_adapter_private.Public.callable_instance_binding_uid instance)) instances with
      | [instance] -> Ok instance | _ -> Error "The runtime implementation has no unique compiler-issued callable identity." in
    let compiler_uid = Typedtree_adapter_private.Public.callable_instance_binding_uid instance in
    let dependency_key dependency = Numeric_receipt_private.encode ~schema:"numeric-runtime-dependency"
      [dependency.Numeric_proof_dependencies_private.compiler_uid;
       string_of_bool dependency.trusted;
       Sst.to_string {program with functions=[dependency.definition]}] in
    let dependency_keys = List.map dependency_key closure.dependencies |> List.sort String.compare in
    let trusted_dependency_keys = List.map dependency_key closure.trusted_dependencies |> List.sort String.compare in
    let full_key = Numeric_receipt_private.encode ~schema:"verocaml.numeric-runtime-refinement.v2"
      [law.full_key; law.target.full_key; facts.provider_artifact_full_key; compiler_uid;role.numeric_role_callable_uid;
       role.numeric_role_callable_abi;
       Typedtree_adapter_private.Public.callable_instance_profile_snapshot instance;
       Typedtree_adapter_private.Public.callable_instance_specialization_digest instance;
       Sst.to_string {program with functions=[executable]}; string_of_int clause.clause_index;
       (match authority with Checked_implementation -> "checked" | Explicit_external_body -> "explicit-external-body");
       "scalar-pure-total-no-exceptions-under-completed-contract";
       Numeric_receipt_private.list dependency_keys; Numeric_receipt_private.list trusted_dependency_keys] in
    Ok {law;executable;compiler_uid;interface_uid=role.numeric_role_callable_uid;authority;trusted_dependency_keys;full_key} in
  [%log.debug "completed numeric runtime refinement admission"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~callable:(Delator.Field.string declaration.Numeric_semantics_correlation_private.role.numeric_role_callable_path)
    ~admitted:(Delator.Field.bool (Result.is_ok result))
    ~reason:(Delator.Field.string (match result with Ok _ -> "exact-result-equivalence" | Error reason -> reason))
    ~explicit_runtime_trust:(Delator.Field.bool (match result with Ok {authority=Explicit_external_body;_} -> true | _ -> false))];
  result
[@@delator.instrument] [@@delator.level debug]
