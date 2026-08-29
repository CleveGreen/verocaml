let fail format =
  Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line span.end_pos.column

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let classify filename =
  match Typedtree_lowering.lower_file filename with
  | Ok _ -> print_endline "accepted"
  | Error diagnostic ->
      Printf.printf "%s @ %s\n" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let lower_vir filename =
  match Symbolic_executor.lower_program (lower filename) with
  | Ok program -> program
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let verify filename =
  let program = lower_vir filename in
  Printf.printf "verified VIR (%d functions, %d owned-tree transitions)\n"
    (List.length program.Vir.functions)
    (List.fold_left
       (fun count execution ->
         count + List.length execution.Vir.owned_tree_transitions)
       0 program.functions)

let map_expressions transform program =
  let rec expression node =
    let recurse = expression in
    let expression_desc =
      match node.Sst.expression_desc with
      | Sst.Owned_tree_nested_write write ->
          Sst.Owned_tree_nested_write
            { write with value = recurse write.value }
      | Sst.Optional_absent -> Sst.Optional_absent
      | Sst.Optional_present value -> Sst.Optional_present (recurse value)
      | Sst.Optional_forward value -> Sst.Optional_forward (recurse value)
      | Sst.Tuple_value values ->
          Sst.Tuple_value
            (List.map (fun (label, value) -> (label, recurse value)) values)
      | Sst.Record_value value ->
          Sst.Record_value
            {
              value with
              fields =
                List.map
                  (fun (field, payload) -> (field, recurse payload))
                  value.fields;
            }
      | Sst.Constructor_value value ->
          Sst.Constructor_value
            { value with arguments = List.map recurse value.arguments }
      | Sst.Field_read read ->
          Sst.Field_read { read with record = recurse read.record }
      | Sst.Field_write write ->
          Sst.Field_write { write with value = recurse write.value }
      | Sst.Shared_scalar_field_write write ->
          Sst.Shared_scalar_field_write
            { write with value = recurse write.value }
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
      | Sst.Sequence (left, right) ->
          Sst.Sequence (recurse left, recurse right)
      | Sst.If (condition, consequent, alternative) ->
          Sst.If
            ( recurse condition,
              recurse consequent,
              Option.map recurse alternative )
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
      | Sst.Forall quantifier ->
          Sst.Forall
            {
              quantifier with
              quantifier_body = recurse quantifier.quantifier_body;
              quantifier_trigger =
                Option.map recurse quantifier.quantifier_trigger;
            }
      | Sst.Exists quantifier ->
          Sst.Exists
            {
              quantifier with
              quantifier_body = recurse quantifier.quantifier_body;
              quantifier_trigger =
                Option.map recurse quantifier.quantifier_trigger;
            }
      | Sst.Direct_call call ->
          Sst.Direct_call
            {
              call with
              arguments =
                List.map
                  (fun argument ->
                    let label, value = Sst.require_value_argument argument in
                    Sst.Value_argument { label; value = recurse value })
                  call.arguments;
            }
      | (Sst.Symbolic_application _ as application) ->
          Option.get
            (Sst.map_symbolic_application_arguments recurse application)
      | Sst.Reveal _ | Sst.Reveal_with_fuel _ as reveal -> reveal
      | Sst.Use_type_invariant use ->
          Sst.Use_type_invariant { use with value = recurse use.value }
      | Sst.Local_assert assertion ->
          Sst.Local_assert
            { assertion with predicate = recurse assertion.predicate }
      | Sst.Proof_region body -> Sst.Proof_region (recurse body)
      | Sst.Old payload -> Sst.Old (recurse payload)
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
          fail "callback rewrite is not implemented"
      | (Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _ | Sst.Mutable_read _ | Sst.Owned_tree_rebase _) as leaf ->
          leaf
    in
    transform { node with Sst.expression_desc }
  in
  let functions =
    List.map
      (fun definition ->
        let body =
          match definition.Sst.body with
          | Sst.Checked_exec checked ->
              Sst.Checked_exec
                {
                  checked with
                  body =
                    {
                      checked.body with
                      expression = expression checked.body.expression;
                    };
                }
          | body -> body
        in
        { definition with Sst.body })
      program.Sst.functions
  in
  { program with Sst.functions }

let map_first_nested_transition mutate program =
  let changed = ref false in
  let mapped =
    map_expressions
      (fun expression ->
        match expression.Sst.expression_desc with
        | Sst.Owned_tree_nested_write write when not !changed ->
            changed := true;
            {
              expression with
              expression_desc =
                Sst.Owned_tree_nested_write
                  {
                    write with
                    transition = mutate write.transition;
                  };
            }
        | _ -> expression)
      program
  in
  if not !changed then fail "positive program had no nested transition";
  mapped

let remove_first_root_transition program =
  let changed = ref false in
  let mapped =
    map_expressions
      (fun expression ->
        match expression.Sst.expression_desc with
        | Sst.If
            ( condition,
              ({
                 expression_desc =
                   Sst.Field_write
                     ({ transition = Some _; _ } as write);
                 _;
               } as consequent),
              alternative )
          when not !changed ->
            changed := true;
            {
              expression with
              expression_desc =
                Sst.If
                  ( condition,
                    {
                      consequent with
                      expression_desc =
                        Sst.Field_write { write with transition = None };
                    },
                    alternative );
            }
        | _ -> expression)
      program
  in
  if not !changed then fail "positive program had no root transition";
  mapped

let validator_attacks filename =
  let program = lower filename in
  let reject_malformed label expected attacked =
    match Sst_validation.validate attacked with
    | Error
        {
          Sst_validation.kind = Malformed_expression actual;
          _;
        }
      when String.equal actual expected ->
        Printf.printf "%s: malformed transition (%s)\n" label actual
    | Error error ->
        fail "%s: wrong structural rejection: %s" label
          (Sst_validation.error_to_string error)
    | Ok _ -> fail "%s: forged closed transition was accepted" label
  in
  reject_malformed "stale-version"
    "owned-tree transition uses a stale root version"
    (map_first_nested_transition
       (fun transition ->
         {
           transition with
           pre_version = transition.pre_version + 1;
           successor_version = transition.successor_version + 1;
         })
       program);
  reject_malformed "missing-invalidation"
    "owned-tree cursor is stale or not invalidated"
    (map_first_nested_transition
       (fun transition -> { transition with invalidated_cursor_ids = [] })
       program);
  reject_malformed "malformed-path"
    "owned-tree cursor path result type mismatch"
    (map_first_nested_transition
       (fun transition ->
         {
           transition with
           cursor =
             Option.map
               (fun cursor -> { cursor with Sst.guarded_path = [] })
               transition.cursor;
         })
       program);
  reject_malformed "altered-modality"
    "owned-tree transition target metadata does not match registry"
    (map_first_nested_transition
       (fun transition ->
         {
           transition with
           target_modalities =
             {
               transition.target_modalities with
               uniqueness_modality = Sst.Preserve_uniqueness;
             };
         })
       program);
  reject_malformed "incomplete-reconstruction"
    "owned-tree nested reconstruction metadata is incomplete"
    (map_first_nested_transition
       (fun transition -> { transition with reconstruction = [] })
       program);
  let missing_evidence =
    {
      program with
      Sst.types =
        List.map
          (fun definition ->
            if String.equal definition.Sst.type_id.type_name "Stack.t" then
              {
                definition with
                representation =
                  Sst.Abstract_with_evidence
                    (Sst.Incomplete_abstraction_evidence
                       {
                         evidence_id = "missing-abstraction-evidence";
                         evidence_span = definition.span;
                       });
              }
            else definition)
          program.types;
    }
  in
  (match Sst_validation.validate missing_evidence with
  | Error { Sst_validation.kind = Forged_abstract_evidence _; _ } ->
      print_endline
        "missing-abstraction-evidence: forged abstraction evidence"
  | Error error ->
      fail "missing-abstraction-evidence: wrong rejection: %s"
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "missing-abstraction-evidence: accepted")

let validator_join_attack filename =
  let program = lower filename in
  let attacked = remove_first_root_transition program in
  match Sst_validation.validate attacked with
  | Error
      {
        Sst_validation.kind =
          Malformed_expression
            "owned-tree transition uses a stale root version";
        _;
      } ->
      print_endline
        "divergent-branch-join: malformed transition (owned-tree transition \
         uses a stale root version)"
  | Error error ->
      fail "divergent-branch-join: wrong rejection: %s"
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "divergent-branch-join: accepted"

let validate label program =
  match Sst_validation.validate program with
  | Ok validated ->
      Printf.printf "%s: accepted\n" label;
      Some validated
  | Error { Sst_validation.kind = Forged_abstract_evidence detail; _ } ->
      Printf.printf "%s: rejected forged certificate (%s)\n" label detail;
      None
  | Error error ->
      fail "%s: wrong validation result: %s" label
        (Sst_validation.error_to_string error)

let map_type name f program =
  {
    program with
    Sst.types =
      List.map
        (fun (definition : Sst.type_definition) ->
          if String.equal definition.type_id.type_name name then f definition
          else definition)
        program.Sst.types;
  }

let map_function name f program =
  {
    program with
    Sst.functions =
      List.map
        (fun (definition : Sst.function_definition) ->
          if String.equal definition.function_id.function_name name then
            f definition
          else definition)
        program.Sst.functions;
  }

let authenticated definition =
  match definition.Sst.representation with
  | Sst.Abstract_with_evidence
      (Sst.Authenticated_same_cmt_abstraction evidence) ->
      evidence
  | Sst.Revealed
  | Sst.Abstract_with_evidence
      (Sst.Incomplete_abstraction_evidence _
      | Sst.Proposed_same_cmt_abstraction _) ->
      fail "expected authenticated abstraction for %s" definition.type_id.type_name

let registry filename =
  let program = lower filename in
  let validated =
    match validate "issued program" program with
    | Some validated -> validated
    | None -> exit 3
  in
  let state =
    List.find
      (fun (definition : Sst.type_definition) ->
        String.equal definition.type_id.type_name "Stack.t")
      program.types
  in
  let view =
    List.find
      (fun (definition : Sst.type_definition) ->
        String.equal definition.type_id.type_name "Stack.view")
      program.types
  in
  (match Sst_validation.abstraction_evidence validated state.type_id with
  | Some _ -> print_endline "validated registry query: Stack.t authenticated"
  | None -> fail "validated registry query did not return Stack.t");
  let state_evidence = authenticated state in
  let altered_evidence =
    {
      state_evidence with
      public_surface = List.rev state_evidence.public_surface;
    }
  in
  let altered_certificate =
    map_type "Stack.t"
      (fun definition ->
        {
          definition with
          representation =
            Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction altered_evidence);
        })
      program
  in
  ignore (validate "valid token with altered certificate" altered_certificate);
  let altered_shape =
    map_type "Stack.node"
      (fun definition ->
        match definition.type_kind with
        | Sst.Variant_definition (empty :: node :: rest) ->
            let fields =
              List.map
                (fun (field : Sst.field_definition) ->
                  if String.equal field.field_id.field_name "next" then
                    { field with field_mutability = Sst.Immutable_field }
                  else field)
                node.constructor_fields
            in
            {
              definition with
              type_kind =
                Sst.Variant_definition
                  (empty :: { node with constructor_fields = fields } :: rest);
            }
        | _ -> fail "unexpected Stack.node shape")
      program
  in
  (match Sst_validation.validate altered_shape with
  | Error
      {
        Sst_validation.kind =
          Malformed_expression
            "owned-tree transition target metadata does not match registry";
        _;
      } ->
      print_endline
        "valid token with altered representation: rejected malformed \
         transition (owned-tree transition target metadata does not match \
         registry)"
  | Error error ->
      fail "valid token with altered representation: wrong validation result: %s"
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "valid token with altered representation: accepted");
  let altered_callable =
    map_function "Stack.snapshot"
      (fun definition -> { definition with result_type = Sst.Int })
      program
  in
  ignore (validate "valid token with altered callable" altered_callable);
  let retargeted =
    map_type "Stack.view"
      (fun definition ->
        {
          definition with
          representation =
            Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction state_evidence);
        })
      program
  in
  ignore (validate "valid token retargeted to another type" retargeted);
  let missing_certificate =
    map_type "Stack.view"
      (fun definition -> { definition with representation = Sst.Revealed })
      program
  in
  ignore (validate "valid token with incomplete abstract set" missing_certificate);
  ignore (authenticated view)

let () =
  match Array.to_list Sys.argv with
  | [ _; "classify"; filename ] -> classify filename
  | [ _; "dump-sst"; filename ] -> print_string (Sst.to_string (lower filename))
  | [ _; "dump-vir"; filename ] ->
      print_string (Vir.to_string (lower_vir filename))
  | [ _; "verify"; filename ] -> verify filename
  | [ _; "validator-attacks"; filename ] -> validator_attacks filename
  | [ _; "validator-join-attack"; filename ] ->
      validator_join_attack filename
  | [ _; "registry"; filename ] -> registry filename
  | _ ->
      fail
        "usage: tool \
         (classify|dump-sst|dump-vir|verify|validator-attacks|validator-join-attack|registry) \
         FILE.cmt"
