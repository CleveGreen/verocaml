type call_target = Logical_spec_capability_private.call_target =
  | Local_nonrecursive of Sst.function_definition
  | Captured_model of Logical_spec_capability_private.model_capability
  | Opaque_recursive
  | Unsupported

type permit = {
  definitions : (int * Sst.function_definition) list;
  integer_conditionals : bool;
}

type authorization =
  | Ordinary of permit
  | Strict of
      Logical_spec_capability_private.permit
      * Sst_validation.validated_program

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let supported_match_pattern (pattern : Sst.pattern) =
  let rec supported pattern =
    match pattern.Sst.pattern_desc with
    | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Unit_pattern ->
        true
    | Sst.Tuple_pattern components ->
        List.for_all (fun (_, nested) -> supported nested) components
    | Sst.Record_pattern fields ->
        List.for_all (fun (_, nested) -> supported nested) fields
    | Sst.Constructor_pattern (_, arguments) -> List.for_all supported arguments
    | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ -> false
  in
  supported pattern

let supported_binding_pattern (pattern : Sst.pattern) =
  let rec supported pattern =
    match pattern.Sst.pattern_desc with
    | Sst.Wildcard | Sst.Bind _ | Sst.Unit_pattern -> true
    | Sst.Tuple_pattern components ->
        List.for_all (fun (_, nested) -> supported nested) components
    | Sst.Record_pattern fields ->
        List.for_all (fun (_, nested) -> supported nested) fields
    | Sst.Owned_tree_cursor_pattern _ | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Constructor_pattern _ | Sst.Or_pattern _ ->
        false
  in
  supported pattern

let rec supported_conditional_type ~integer_conditionals = function
  | Sst.Unit | Sst.Bool | Sst.Aggregate _ | Sst.Parameter _
  | Sst.Application _ -> true
  | Sst.Int -> integer_conditionals
  | Sst.Tuple components ->
      List.for_all (fun (_, typ) -> supported_conditional_type ~integer_conditionals typ) components

let rec recursive_argument_is_branch_free classify (expression : Sst.expression)
    =
  let recurse = recursive_argument_is_branch_free classify in
  match expression.expression_desc with
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ ->
      true
  | Sst.Optional_present payload | Sst.Optional_forward payload ->
      recurse payload
  | Sst.Optional_absent -> true
  | Sst.Tuple_value components ->
      List.for_all (fun (_, component) -> recurse component) components
  | Sst.Record_value { fields; _ } -> (
      match expression.typ with
      | Sst.Application _ ->
          List.for_all (fun (_, field) -> recurse field) fields
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _ | Sst.Tuple _
      | Sst.Parameter _ ->
          false)
  | Sst.Constructor_value { arguments; _ } -> List.for_all recurse arguments
  | Sst.Field_read { record; _ } -> recurse record
  | Sst.Checked_arithmetic (_, arguments) -> List.for_all recurse arguments
  | Sst.Compare (_, left, right) -> recurse left && recurse right
  | Sst.Boolean_not operand -> recurse operand
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      recurse quantifier.quantifier_body
      && Option.fold ~none:true ~some:recurse quantifier.quantifier_trigger
  | Sst.Symbolic_application application ->
      List.for_all recurse
        (Symbolic_application_private.arguments application)
  | Sst.Direct_call _
    when Spec_function_sst_private.lambda expression <> None ->
      let lambda =
        Option.get (Spec_function_sst_private.lambda expression)
      in
      List.for_all
        (fun (binding : Sst.binding) ->
          recurse
            {
              Sst.expression_desc =
                Sst.Variable
                  {
                    binding;
                    use_uniqueness = Sst.Definitely_aliased;
                  };
              typ = binding.typ;
              span = binding.span;
            })
        lambda.lambda_captures
      && recurse lambda.lambda_body
  | Sst.Direct_call _ -> (
      match Spec_function_sst_private.application expression with
      | Some application ->
          recurse application.application_function
          && recurse application.application_argument
      | None -> false)
  | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _ | Sst.Boolean_binary _
  | Sst.Callback_call _ | Sst.Callback_requires _
  | Sst.Callback_ensures _ | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ | Sst.Let_mutable _
  | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Reveal _
  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _ | Sst.Local_assert _
  | Sst.Proof_region _ | Sst.Old _ ->
      false

let authenticate_with ~integer_conditionals ~classify root =
  let definitions = ref [] in
  let visiting = ref [] in
  let visited = ref [] in
  let same_context id branch_sensitive (candidate_id, candidate_sensitive) =
    Bool.equal branch_sensitive candidate_sensitive
    && same_function_id id candidate_id
  in
  let rec eligible_expression branch_sensitive (expression : Sst.expression) =
    let recurse = eligible_expression branch_sensitive in
    match expression.expression_desc with
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
    | Sst.Variable _ ->
        true
    | Sst.Optional_present payload | Sst.Optional_forward payload ->
        recurse payload
    | Sst.Optional_absent -> true
    | Sst.Tuple_value components ->
        List.for_all (fun (_, component) -> recurse component) components
    | Sst.Record_value { fields; _ } -> (
        match expression.typ with
        | Sst.Application _ ->
            List.for_all (fun (_, field) -> recurse field) fields
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _ | Sst.Tuple _
        | Sst.Parameter _ ->
            false)
    | Sst.Constructor_value { arguments; _ } -> List.for_all recurse arguments
    | Sst.Field_read { record; _ } -> recurse record
    | Sst.Let (bindings, body) ->
        List.for_all
          (fun (pattern, value) ->
            supported_binding_pattern pattern && recurse value)
          bindings
        && recurse body
    | Sst.Sequence (first, second) -> recurse first && recurse second
    | Sst.If (condition, consequent, Some alternative) ->
        supported_conditional_type ~integer_conditionals expression.typ
        && recurse condition
        && eligible_expression true consequent
        && eligible_expression true alternative
    | Sst.Match (scrutinee, cases) ->
        supported_conditional_type ~integer_conditionals expression.typ
        && cases <> [] && recurse scrutinee
        && List.for_all
             (fun (case : Sst.case) ->
               supported_match_pattern case.case_pattern
               && Option.fold ~none:true ~some:(eligible_expression true)
                    case.case_guard
               && eligible_expression true case.case_body)
             cases
    | Sst.Checked_arithmetic (_, arguments) -> List.for_all recurse arguments
    | Sst.Compare (_, left, right) -> recurse left && recurse right
    | Sst.Boolean_not operand -> recurse operand
    | Sst.Boolean_binary (_, left, right) -> recurse left && recurse right
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        recurse quantifier.quantifier_body
        && Option.fold ~none:true ~some:recurse quantifier.quantifier_trigger
    | Sst.Symbolic_application application ->
        List.for_all recurse
          (Symbolic_application_private.arguments application)
    | Sst.Direct_call _
      when Spec_function_sst_private.lambda expression <> None ->
        let lambda =
          Option.get (Spec_function_sst_private.lambda expression)
        in
        List.for_all
          (fun (binding : Sst.binding) ->
            recurse
              {
                Sst.expression_desc =
                  Sst.Variable
                    {
                      binding;
                      use_uniqueness = Sst.Definitely_aliased;
                    };
                typ = binding.typ;
                span = binding.span;
              })
          lambda.lambda_captures
        && recurse lambda.lambda_body
    | Sst.Direct_call _
      when Spec_function_sst_private.application expression <> None ->
        let application =
          Option.get (Spec_function_sst_private.application expression)
        in
        recurse application.application_function
        && recurse application.application_argument
    | Sst.Direct_call
        { call_form = Sst.Specification_call; callee; arguments; _ } -> (
        match classify callee with
        | Local_nonrecursive
            { Sst.body = Sst.Symbolic_declaration _; _ } ->
            List.for_all
              (function
                | Sst.Value_argument { value; _ } -> recurse value
                | Sst.Callback_argument _ -> false)
              arguments
        | Local_nonrecursive definition ->
            List.for_all
              (function
                | Sst.Value_argument { value; _ } -> recurse value
                | Sst.Callback_argument _ -> false)
              arguments
            && definition_graph branch_sensitive definition
        | Opaque_recursive ->
            (not branch_sensitive)
            && List.for_all
                 (function
                   | Sst.Value_argument { value; _ } ->
                       recursive_argument_is_branch_free classify value
                       && eligible_expression false value
                   | Sst.Callback_argument _ -> false)
                 arguments
        | Captured_model _ | Unsupported -> false)
    | Sst.If (_, _, None)
    | Sst.Field_write _ | Sst.Shared_scalar_field_write _
    | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
    | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
    | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
    | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
    | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
    | Sst.Old _ ->
        false
  and definition_graph branch_sensitive definition =
    let id = definition.Sst.function_id in
    if List.exists (same_context id branch_sensitive) !visited then true
    else if List.exists (same_context id branch_sensitive) !visiting then false
    else
      match (definition.mode, definition.recursive, definition.body) with
      | ( Sst.Spec,
          false,
          Sst.Spec_definition { stage = Sst.Logical; expression = body } ) ->
          visiting := (id, branch_sensitive) :: !visiting;
          let supported =
            List.for_all
              (function
                | Sst.Value_parameter parameter ->
                    supported_binding_pattern parameter.Sst.pattern
                | Sst.Callback_parameter _ -> false)
              definition.parameters
            && eligible_expression branch_sensitive body
          in
          visiting :=
            List.filter
              (fun candidate ->
                not (same_context id branch_sensitive candidate))
              !visiting;
          if supported then (
            visited := (id, branch_sensitive) :: !visited;
            if
              not
                (List.exists
                   (fun (_, candidate) ->
                     same_function_id id candidate.Sst.function_id)
                   !definitions)
            then definitions := (id.function_index, definition) :: !definitions);
          supported
      | _ -> false
  in
  let supported = definition_graph false root in
  if supported then
    Some { definitions = !definitions; integer_conditionals }
  else None

let authenticate = authenticate_with ~integer_conditionals:false

let find_definition permit callee =
  match List.assoc_opt callee.Sst.function_index permit.definitions with
  | Some definition when same_function_id definition.function_id callee ->
      Some definition
  | Some _ | None -> None

let classify_definition ~excluded definition =
  if excluded definition then Unsupported
  else
    match (definition.Sst.mode, definition.recursive, definition.body) with
    | Sst.Spec, false, (Sst.Spec_definition _ | Sst.Symbolic_declaration _) ->
        Local_nonrecursive definition
    | Sst.Spec, _, Sst.Recursive_spec_definition _ -> Opaque_recursive
    | (Sst.Exec | Sst.Proof), _, _
    | Sst.Spec, true, (Sst.Spec_definition _ | Sst.Symbolic_declaration _)
    | Sst.Spec, _,
      ( Sst.Checked_exec _ | Sst.Proof_body _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _ ) ->
        Unsupported

let classify_invariant_call ~imported ~excluded ~models definitions callee =
  match List.assoc_opt callee.Sst.function_index definitions with
  | Some descriptor
    when not (imported callee)
         && same_function_id (Sst_validation.callable_id descriptor) callee ->
      let definition = Sst_validation.callable_definition descriptor in
      (match
         List.find_opt
           (fun model ->
             same_function_id
               (Logical_spec_capability_private.model_callable model)
               callee)
           models
       with
      | Some model -> Captured_model model
      | None -> classify_definition ~excluded definition)
  | Some _ | None -> Unsupported

module type Invariant_source = sig
  type handle
  type environment

  val handles : environment -> handle list
  val model_callable : handle -> Sst.function_id
  val public_operations :
    handle -> (Sst.function_id * Sst.abstract_operation_role) list
  val find_for_operation :
    environment ->
    Sst.function_id ->
    (handle * Sst.abstract_operation_role) option
end

module Make_invariant_policy (Source : Invariant_source) = struct
  let materialization_decisions ~imports ~validated ~invariants ~frozen ~owned =
    Source.handles invariants
    |> List.map (fun handle ->
           let callable = Source.model_callable handle in
           let definition =
             Option.map Sst_validation.callable_definition
               (Sst_validation.find_callable validated callable)
           in
           let shared =
             List.exists
               (fun (_, role) -> role = Sst.Shared_invariant_transition)
               (Source.public_operations handle)
           in
           let imported =
             Option.fold ~none:false
               ~some:(fun registration ->
                 Imported_callable.is_imported registration callable)
               imports
           in
           let excluded_definition =
             Option.fold ~none:true
               ~some:(fun definition ->
                 Option.is_some (owned definition)
                 ||
                 match definition.Sst.body with
                 | Sst.External_specification _
                 | Sst.Trusted_external_spec_target _
                 | Sst.Trusted_external_body _ ->
                     true
                 | _ -> false)
               definition
           in
           ( handle,
             not
               (Option.is_some (frozen callable) || imported || shared
              || excluded_definition),
             if shared then Some "shared-invariant" else None ))

  let classify ~imports ~validated ~invariants ~models definitions =
    classify_invariant_call
      ~imported:(fun callee ->
        Option.fold ~none:false
          ~some:(fun registration -> Imported_callable.is_imported registration callee)
          imports)
      ~excluded:(fun definition ->
        Option.is_some
          (Source.find_for_operation invariants definition.Sst.function_id)
        || Option.is_some
             (Sst_validation.find_model validated definition.function_id))
      ~models definitions
end

let excluded_contract_root imports definition =
  Option.fold ~none:false
    ~some:(fun registration ->
      Imported_callable.is_imported registration definition.Sst.function_id)
    imports
  ||
  match definition.body with
  | Sst.Checked_exec _ | Sst.Proof_body _ -> false
  | _ -> true

let authenticate_expression ~classify expression =
  let definition =
    {
      Sst.function_id =
        { Sst.function_index = -110; function_name = "$spec-function-root" };
      type_binders = [];
      mode = Sst.Spec;
      recursive = false;
      parameters = [];
      contracts = Sst.empty_contracts;
      body = Sst.Spec_definition { Sst.stage = Sst.Logical; expression };
      policy = Sst.Default_linear_z3;
      result_type = expression.typ;
      returns_unique_parameter = None;
      span = expression.span;
    }
  in
  authenticate_with ~integer_conditionals:true ~classify definition

let authorize_definition authorization ~classify callee ~result_type =
  match authorization with
  | Strict (permit, validated) ->
      Logical_spec_capability_private.authorize_call permit ~validated callee
        ~result_type
      |> Result.map Option.some
  | Ordinary permit ->
      let found =
        match find_definition permit callee with
        | Some definition -> Some (definition, None)
        | None -> (
            match classify callee with
            | Local_nonrecursive
                ({ Sst.body = Sst.Symbolic_declaration _; _ } as definition) ->
                Some (definition, None)
            | Local_nonrecursive definition ->
                Option.bind
                  (authenticate_with
                     ~integer_conditionals:permit.integer_conditionals
                     ~classify definition)
                  (fun permit ->
                    find_definition permit callee
                    |> Option.map (fun definition -> (definition, None)))
            | Captured_model capability ->
                Some
                  ( Logical_spec_capability_private.model_definition capability,
                    Some capability )
            | Opaque_recursive | Unsupported -> None)
      in
      Ok
        (Option.map
           (fun (definition, model) ->
             Logical_spec_capability_private.{ definition; model })
           found)

let is_strict = function Ordinary _ -> false | Strict _ -> true
