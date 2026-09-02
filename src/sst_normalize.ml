let rec classify_calls_with classify_form stage (expression : Sst.expression) =
  let recurse = classify_calls_with classify_form stage in
  let expression_desc =
    match expression.expression_desc with
    | Sst.Tuple_value values ->
        Sst.Tuple_value
          (List.map (fun (label, value) -> (label, recurse value)) values)
    | Sst.Record_value record ->
        Sst.Record_value
          {
            record with
            fields =
              List.map
                (fun (field, value) -> (field, recurse value))
                record.fields;
          }
    | Sst.Constructor_value constructor ->
        Sst.Constructor_value
          {
            constructor with
            arguments = List.map recurse constructor.arguments;
          }
    | Sst.Field_read read ->
        Sst.Field_read { read with record = recurse read.record }
    | Sst.Field_write write ->
        Sst.Field_write { write with value = recurse write.value }
    | Sst.Shared_scalar_field_write write ->
        Sst.Shared_scalar_field_write
          { write with value = recurse write.value }
    | Sst.Owned_tree_nested_write write ->
        Sst.Owned_tree_nested_write
          { write with value = recurse write.value }
    | Sst.Owned_tree_rebase _ as transition -> transition
    | Sst.Let_mutable (binding, initial, body) ->
        Sst.Let_mutable (binding, recurse initial, recurse body)
    | Sst.Mutable_write write ->
        Sst.Mutable_write { write with value = recurse write.value }
    | Sst.Let (bindings, body) ->
        Sst.Let
          ( List.map
              (fun (pattern, value) -> (pattern, recurse value))
              bindings,
            recurse body )
    | Sst.Sequence (first, second) -> Sst.Sequence (recurse first, recurse second)
    | Sst.If (condition, consequent, alternative) ->
        Sst.If
          (recurse condition, recurse consequent, Option.map recurse alternative)
    | Sst.Match (scrutinee, cases) ->
        Sst.Match
          ( recurse scrutinee,
            List.map
              (fun (case : Sst.case) ->
                {
                  case with
                  Sst.case_guard = Option.map recurse case.case_guard;
                  case_body = recurse case.case_body;
                })
              cases )
    | Sst.Checked_arithmetic (operation, operands) ->
        Sst.Checked_arithmetic (operation, List.map recurse operands)
    | Sst.Compare (comparison, left, right) ->
        Sst.Compare (comparison, recurse left, recurse right)
    | Sst.Boolean_not operand -> Sst.Boolean_not (recurse operand)
    | Sst.Boolean_binary (operation, left, right) ->
        Sst.Boolean_binary (operation, recurse left, recurse right)
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let quantifier =
          {
            quantifier with
            Sst.quantifier_body = recurse quantifier.quantifier_body;
            quantifier_trigger = Option.map recurse quantifier.quantifier_trigger;
          }
        in
        (match expression.expression_desc with
        | Sst.Forall _ -> Sst.Forall quantifier
        | Sst.Exists _ -> Sst.Exists quantifier
        | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
        | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
        | Sst.Match _ | Sst.Checked_arithmetic _ | Sst.Compare _
        | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Direct_call _
        | Sst.Symbolic_application _
        | Sst.Callback_call _ | Sst.Callback_requires _
        | Sst.Callback_ensures _ | Sst.Optional_absent
        | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
        | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _
        | Sst.Lift_runtime_int _ ->
            assert false)
    | Sst.Direct_call call ->
        Sst.Direct_call
          {
            call with
            call_form = classify_form call.call_form call.callee;
            arguments =
              List.map
                (function
                  | Sst.Value_argument { label; value } ->
                      Sst.Value_argument { label; value = recurse value }
                  | Sst.Callback_argument _ as argument -> argument)
                call.arguments;
          }
    | Sst.Symbolic_application application ->
        Sst.Symbolic_application
          (Symbolic_application_private.map_arguments recurse application)
    | Sst.Callback_call application ->
        Sst.Callback_call
          { application with
            arguments =
              List.map (fun (label, value) -> (label, recurse value))
                application.arguments }
    | Sst.Callback_requires application ->
        Sst.Callback_requires
          { application with
            arguments =
              List.map (fun (label, value) -> (label, recurse value))
                application.arguments }
    | Sst.Callback_ensures { application; result } ->
        Sst.Callback_ensures
          { application =
              { application with
                arguments =
                  List.map (fun (label, value) -> (label, recurse value))
                    application.arguments };
            result = recurse result }
    | Sst.Reveal _ | Sst.Reveal_with_fuel _ as reveal -> reveal
    | Sst.Use_type_invariant use ->
        Sst.Use_type_invariant { use with value = recurse use.value }
    | Sst.Local_assert assertion ->
        Sst.Local_assert
          { assertion with predicate = recurse assertion.predicate }
    | Sst.Proof_region body ->
        Sst.Proof_region
          (classify_calls_with classify_form Sst.Proof_stage body)
    | Sst.Old payload -> Sst.Old (recurse payload)
    | Sst.Optional_absent -> Sst.Optional_absent
    | Sst.Optional_present payload -> Sst.Optional_present (recurse payload)
    | Sst.Optional_forward payload -> Sst.Optional_forward (recurse payload)
    | Sst.Lift_runtime_int operand -> Sst.Lift_runtime_int (recurse operand)
    | (Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Mutable_read _) as desc ->
        desc
  in
  { expression with expression_desc }

let classify_calls stage =
  classify_calls_with
    (fun _ _ ->
      match stage with
      | Sst.Runtime -> Sst.Exec_call
      | Sst.Logical | Sst.Proof_stage -> Sst.Unclassified_call)
    stage

let make ~provenance ~function_id ~recursive ~parameters ~contracts ~body
    ~result_type ~returns_unique_parameter ~span =
  {
    Sst.function_id;
    type_binders = [];
    mode = Sst.Exec;
    recursive;
    parameters;
    contracts;
    body =
      Sst.Checked_exec
        { body = { stage = Sst.Runtime; expression = body }; provenance };
    policy = Sst.Default_linear_z3;
    result_type;
    returns_unique_parameter;
    span;
  }

let authenticated_checked_exec ~source_file ~function_id ~recursive ~parameters
    ~contracts ~body ~result_type ~returns_unique_parameter ~span =
  make
    ~provenance:
      (Sst.Authenticated_typedtree
         { source_file; declaration_span = span })
    ~function_id ~recursive ~parameters ~contracts
    ~body ~result_type
    ~returns_unique_parameter ~span

let checked_exec_raw ~function_id ~recursive ~parameters ~contracts ~body
    ~result_type ~returns_unique_parameter ~span =
  make ~provenance:(Sst.Raw_semantic_body span) ~function_id ~recursive
    ~parameters ~contracts ~body:(classify_calls Sst.Runtime body) ~result_type
    ~returns_unique_parameter ~span

let authenticated_spec_definition ~function_id ~parameters ~body ~result_type
    ~span =
  {
    Sst.function_id;
    type_binders = [];
    mode = Sst.Spec;
    recursive = false;
    parameters;
    contracts = Sst.empty_contracts;
    body =
      Sst.Spec_definition
        { Sst.stage = Sst.Logical; expression = body };
    policy = Sst.Default_linear_z3;
    result_type;
    returns_unique_parameter = None;
    span;
  }

let classify_typedtree_program (program : Sst.program) =
  let callee_mode callee =
    List.find_opt
      (fun (definition : Sst.function_definition) ->
        definition.function_id = callee)
      program.functions
    |> Option.map (fun definition -> definition.Sst.mode)
  in
  let classify stage expression =
    classify_calls_with
      (fun call_form callee ->
        match (call_form, callee_mode callee) with
        | Sst.Unclassified_call, Some Sst.Spec -> Sst.Specification_call
        | Sst.Unclassified_call, Some Sst.Proof -> Sst.Proof_call
        | Sst.Unclassified_call, Some Sst.Exec -> Sst.Exec_call
        | call_form, _ -> call_form)
      stage expression
  in
  let staged staged =
    { staged with Sst.expression = classify staged.Sst.stage staged.expression }
  in
  let contracts (contracts : Sst.contracts) =
    {
      Sst.requires =
        List.map
          (fun (clause : Sst.predicate_clause) ->
            { clause with Sst.predicate = staged clause.Sst.predicate })
          contracts.Sst.requires;
      ensures =
        List.map
          (fun (clause : Sst.ensures_clause) ->
            { clause with Sst.predicate = staged clause.Sst.predicate })
          contracts.ensures;
      decreases =
        List.map
          (fun (clause : Sst.predicate_clause) ->
            { clause with Sst.predicate = staged clause.Sst.predicate })
          contracts.decreases;
      assertions =
        List.map
          (fun (clause : Sst.predicate_clause) ->
            { clause with Sst.predicate = staged clause.Sst.predicate })
          contracts.assertions;
    }
  in
  let definition definition =
    match definition.Sst.body with
    | Sst.Symbolic_declaration _ -> definition
    | ( Sst.Checked_exec _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.Proof_body _
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ ) ->
      let body =
        match definition.Sst.body with
      | Sst.Checked_exec checked ->
          Sst.Checked_exec { checked with body = staged checked.body }
      | Sst.Spec_definition body -> Sst.Spec_definition (staged body)
      | Sst.Recursive_spec_definition recursive ->
          Sst.Recursive_spec_definition
            { recursive with body = staged recursive.body }
      | Sst.Proof_body proof ->
          Sst.Proof_body { proof with body = staged proof.body }
      | ( Sst.External_specification _ | Sst.Trusted_external_spec_target _
        | Sst.Trusted_external_body _ ) as body ->
          body
        | Sst.Symbolic_declaration _ -> assert false
      in
      {
        definition with
        Sst.contracts = contracts definition.contracts;
        body;
      }
  in
  { program with Sst.functions = List.map definition program.functions }

let authenticated_proof_definition ~source_file ~function_id ~parameters
    ~contracts ~body ~span =
  {
    Sst.function_id;
    type_binders = [];
    mode = Sst.Proof;
    recursive = false;
    parameters;
    contracts;
    body =
      Sst.Proof_body
        {
          body = { Sst.stage = Sst.Proof_stage; expression = body };
          provenance =
            Sst.Authenticated_typedtree
              { source_file; declaration_span = span };
        };
    policy = Sst.Default_linear_z3;
    result_type = Sst.Unit;
    returns_unique_parameter = None;
    span;
  }

let authenticated_external_specification ~wrapper_id ~target_id ~parameters
    ~contracts ~result_type ~wrapper_span ~witness_span ~target_span =
  let link =
    Sst.Same_unit_target
      {
        wrapper = wrapper_id;
        target = target_id;
        target_span;
        declaration_span = wrapper_span;
        witness_span;
      }
  in
  let definition function_id body span =
    {
      Sst.function_id;
      type_binders = [];
      mode = Sst.Exec;
      recursive = false;
      parameters;
      contracts;
      body;
      policy = Sst.Default_linear_z3;
      result_type;
      returns_unique_parameter = None;
      span;
    }
  in
  ( definition wrapper_id (Sst.External_specification link) wrapper_span,
    definition target_id (Sst.Trusted_external_spec_target link) target_span )

let authenticated_trusted_external_body_in_mode ~source_file ~function_id ~mode
    ~parameters ~contracts ~result_type ~returns_unique_parameter
    ~declaration_span ~witness_span =
  {
    Sst.function_id;
    type_binders = [];
    mode;
    recursive = false;
    parameters;
    contracts;
    body =
      Sst.Trusted_external_body
        (Sst.Authenticated_external_body
           { source_file; declaration_span; witness_span });
    policy = Sst.Default_linear_z3;
    result_type;
    returns_unique_parameter;
    span = declaration_span;
  }

let authenticated_trusted_external_body ~source_file ~function_id ~parameters
    ~contracts ~result_type ~returns_unique_parameter ~declaration_span
    ~witness_span =
  authenticated_trusted_external_body_in_mode ~source_file ~function_id
    ~mode:Sst.Exec ~parameters ~contracts ~result_type
    ~returns_unique_parameter ~declaration_span ~witness_span
