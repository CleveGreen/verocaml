let () = ignore Parametric_core_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let reset_solver_counters () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let solver_work () =
  let counters = Z3_bridge.counters () in
  Solver_backend.For_testing.solver_creation_count ()
  + counters.contexts_created + counters.solvers_created

let validate label program =
  reset_solver_counters ();
  match Sst_validation.validate program with
  | Error error ->
      Printf.printf "attack=%s rejected boundary=sst solver-work=%d detail=%s\n"
        label (solver_work ())
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "attack %s unexpectedly passed SST validation" label

let validate_executor_boundary label program =
  reset_solver_counters ();
  match Symbolic_executor_private.lower_program program with
  | Error { Symbolic_executor_private.unsupported = Malformed_sst detail; _ } ->
      Printf.printf
        "attack=%s rejected boundary=pre-executor solver-work=%d detail=%s\n"
        label (solver_work ()) detail
  | Error error ->
      fail "attack %s reached unexpected executor error: %s" label
        (Symbolic_executor_private.error_to_string error)
  | Ok _ -> fail "attack %s unexpectedly reached symbolic execution" label

type direct_call = {
  call_form : Sst.call_form;
  callee : Sst.function_id;
  type_arguments : Sst.typ list;
  arguments : (string option * Sst.expression) list;
  recursive : bool;
}

let direct_call call =
  Sst.Direct_call
    {
      call_form = call.call_form;
      callee = call.callee;
      type_arguments = call.type_arguments;
      arguments =
        List.map
          (fun (label, value) -> Sst.Value_argument { label; value })
          call.arguments;
      recursive = call.recursive;
    }

let rec map_expression transform (expression : Sst.expression) =
  let recurse = map_expression transform in
  let expression_desc =
    match expression.expression_desc with
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
    | Sst.Field_read value ->
        Sst.Field_read { value with record = recurse value.record }
    | Sst.Field_write value ->
        Sst.Field_write { value with value = recurse value.value }
    | Sst.Shared_scalar_field_write value ->
        Sst.Shared_scalar_field_write { value with value = recurse value.value }
    | Sst.Owned_tree_nested_write value ->
        Sst.Owned_tree_nested_write { value with value = recurse value.value }
    | Sst.Let_mutable (binding, initial, body) ->
        Sst.Let_mutable (binding, recurse initial, recurse body)
    | Sst.Mutable_write value ->
        Sst.Mutable_write { value with value = recurse value.value }
    | Sst.Let (bindings, body) ->
        Sst.Let
          ( List.map (fun (pattern, value) -> (pattern, recurse value)) bindings,
            recurse body )
    | Sst.Sequence (left, right) -> Sst.Sequence (recurse left, recurse right)
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
                  case_guard = Option.map recurse case.case_guard;
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
    | Sst.Direct_call
        { call_form; callee; type_arguments; arguments; recursive } ->
        direct_call
          {
            call_form;
            callee;
            type_arguments;
            recursive;
            arguments =
              List.map
                (fun argument ->
                  let label, value = Sst.require_value_argument argument in
                  (label, recurse value))
                arguments;
          }
    | (Sst.Symbolic_application _ as application) ->
        Option.get
          (Sst.map_symbolic_application_arguments recurse application)
    | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
        invalid_arg "callback rewrite is not implemented"
    | Sst.Optional_present payload -> Sst.Optional_present (recurse payload)
    | Sst.Optional_forward carrier -> Sst.Optional_forward (recurse carrier)
    | Sst.Use_type_invariant value ->
        Sst.Use_type_invariant { value with value = recurse value.value }
    | Sst.Local_assert assertion ->
        Sst.Local_assert
          { assertion with predicate = recurse assertion.predicate }
    | Sst.Proof_region body -> Sst.Proof_region (recurse body)
    | Sst.Old payload -> Sst.Old (recurse payload)
    | ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Mutable_read _ | Sst.Owned_tree_rebase _
      | Sst.Optional_absent | Sst.Reveal _ | Sst.Reveal_with_fuel _ ) as leaf ->
        leaf
  in
  transform { expression with expression_desc }

let map_staged transform (staged : Sst.staged_expression) =
  { staged with Sst.expression = map_expression transform staged.expression }

let map_definition transform (definition : Sst.function_definition) =
  let contracts =
    let map_clause (clause : Sst.predicate_clause) =
      { clause with Sst.predicate = map_staged transform clause.predicate }
    in
    let map_ensure (clause : Sst.ensures_clause) =
      { clause with Sst.predicate = map_staged transform clause.predicate }
    in
    {
      Sst.requires = List.map map_clause definition.contracts.requires;
      ensures = List.map map_ensure definition.contracts.ensures;
      decreases = List.map map_clause definition.contracts.decreases;
      assertions = List.map map_clause definition.contracts.assertions;
    }
  in
  let body =
    match definition.body with
    | Sst.Checked_exec body ->
        Sst.Checked_exec { body with body = map_staged transform body.body }
    | Sst.Spec_definition body ->
        Sst.Spec_definition (map_staged transform body)
    | Sst.Recursive_spec_definition body ->
        Sst.Recursive_spec_definition
          { body with body = map_staged transform body.body }
    | Sst.Proof_body body ->
        Sst.Proof_body { body with body = map_staged transform body.body }
    | Sst.Symbolic_declaration _ as body ->
        body
    | ( Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ ) as body ->
        body
  in
  { definition with contracts; body }

let rewrite_first_call callee_name rewrite (program : Sst.program) =
  let changed = ref false in
  let transform expression =
    match expression.Sst.expression_desc with
    | Sst.Direct_call
        { call_form; callee; type_arguments; arguments; recursive }
      when (not !changed) && String.equal callee.function_name callee_name ->
        changed := true;
        let call : direct_call =
          {
            call_form;
            callee;
            type_arguments;
            arguments =
              List.map Sst.require_value_argument arguments;
            recursive;
          }
        in
        { expression with expression_desc = direct_call (rewrite call) }
    | _ -> expression
  in
  let program =
    {
      program with
      Sst.functions = List.map (map_definition transform) program.functions;
    }
  in
  if not !changed then fail "call to %s not found" callee_name;
  program

let map_nth index transform values =
  List.mapi
    (fun ordinal value -> if ordinal = index then transform value else value)
    values

let rewrite_definition name rewrite (program : Sst.program) =
  let changed = ref false in
  let functions =
    List.map
      (fun (definition : Sst.function_definition) ->
        if String.equal definition.function_id.function_name name then (
          changed := true;
          rewrite definition)
        else definition)
      program.functions
  in
  if not !changed then fail "definition %s not found" name;
  { program with Sst.functions }

let call_attacks filename =
  let program = load filename |> lower in
  (match Sst_validation.validate program with
  | Ok _ -> ()
  | Error error -> fail "baseline: %s" (Sst_validation.error_to_string error));
  rewrite_first_call "relay"
    (fun call -> { call with type_arguments = [] })
    program
  |> validate "missing-type-argument";
  rewrite_first_call "relay"
    (fun call ->
      { call with type_arguments = call.type_arguments @ [ Sst.Bool ] })
    program
  |> validate "extra-type-argument";
  rewrite_first_call "relay"
    (fun call -> { call with type_arguments = [ Sst.Bool ] })
    program
  |> validate "mismatched-type-argument";
  rewrite_first_call "first"
    (fun call -> { call with type_arguments = List.rev call.type_arguments })
    program
  |> validate "reordered-type-arguments";
  rewrite_first_call "labelled"
    (fun call ->
      {
        call with
        arguments = map_nth 0 (fun (_, value) -> (None, value)) call.arguments;
      })
    program
  |> validate "missing-label";
  rewrite_first_call "labelled"
    (fun call ->
      {
        call with
        arguments =
          map_nth 1 (fun (_, value) -> (Some "first", value)) call.arguments;
      })
    program
  |> validate "duplicate-label";
  rewrite_first_call "labelled"
    (fun call ->
      {
        call with
        arguments =
          map_nth 0 (fun (_, value) -> (Some "unknown", value)) call.arguments;
      })
    program
  |> validate "unknown-label";
  rewrite_first_call "labelled"
    (fun call -> { call with arguments = List.rev call.arguments })
    program
  |> validate "reordered-label-vector";
  rewrite_first_call "pick"
    (fun call ->
      let fallback = snd (List.hd call.arguments) in
      let arguments =
        List.map
          (fun (label, argument) ->
            match argument.Sst.expression_desc with
            | Sst.Optional_forward _ ->
                ( label,
                  {
                    argument with
                    expression_desc = Sst.Optional_forward fallback;
                  } )
            | _ -> (label, argument))
          call.arguments
      in
      { call with arguments })
    program
  |> validate "wrong-optional-forwarding"

let optional_contract_attack filename =
  let program = load filename |> lower in
  let changed = ref false in
  let functions =
    List.map
      (fun (definition : Sst.function_definition) ->
        if String.equal definition.function_id.function_name "pick" then
          let parameters =
            List.map
              (function
                | Sst.Callback_parameter _ as parameter -> parameter
                | Sst.Value_parameter parameter ->
                    match parameter.optional_default with
                    | None -> Sst.Value_parameter parameter
                    | Some optional_default ->
                        changed := true;
                        Sst.Value_parameter
                          {
                            parameter with
                            optional_default =
                              Some
                                {
                                  optional_default with
                                  optional_pattern =
                                    {
                                      optional_default.optional_pattern with
                                      pattern_desc = Sst.Wildcard;
                                    };
                                };
                          })
              definition.parameters
          in
          { definition with parameters }
        else definition)
      program.functions
  in
  if not !changed then fail "optional default not found";
  validate "optional-contract-substitution" { program with Sst.functions }

let structural_attacks filename =
  let program = load filename |> lower in
  let relay =
    List.find
      (fun definition ->
        String.equal definition.Sst.function_id.function_name "relay")
      program.functions
  in
  let binder = List.hd relay.type_binders in
  let foreign_owner =
    Parametric_type.owner
      ~index:(binder.owner.owner_index + 1000)
      ~name:binder.owner.owner_name
  in
  let foreign = Parametric_type.binder foreign_owner ~ordinal:binder.ordinal in
  rewrite_definition "relay"
    (fun definition -> { definition with type_binders = [ foreign ] })
    program
  |> validate_executor_boundary "forged-binder-owner";
  rewrite_definition "relay"
    (fun definition -> { definition with type_binders = [ binder; binder ] })
    program
  |> validate_executor_boundary "duplicate-binder";
  rewrite_definition "relay"
    (fun definition -> { definition with result_type = Sst.Parameter foreign })
    program
  |> validate_executor_boundary "unbound-binder";
  let option_constructor =
    program.parametric_adts
    |> List.find_map (fun descriptor ->
           let constructor = Parametric_adt.type_constructor descriptor in
           if
             String.equal
               (Filename.basename constructor.constructor_path)
               "option"
             || String.equal constructor.constructor_path "option"
           then Some constructor
           else None)
    |> Option.get
  in
  rewrite_definition "pick"
    (fun definition ->
      {
        definition with
        result_type = Sst.Application (option_constructor, []);
      })
    program
  |> validate_executor_boundary "option-arity";
  let forged_option =
    {
      Parametric_type.constructor_path = "Stdlib.option";
      constructor_identity = "forged:option/1";
    }
  in
  rewrite_definition "pick"
    (fun definition ->
      {
        definition with
        result_type =
          Sst.Tuple [ (None, Sst.Application (forged_option, [ Sst.Int ])) ];
      })
    program
  |> validate_executor_boundary "forged-constructor-identity"

let span =
  Diagnostic.
    {
      file = "parametric_core_tool.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let backend_control () =
  let owner = Parametric_type.owner ~index:0 ~name:"backend" in
  let binder = Parametric_type.binder owner ~ordinal:0 in
  let symbol id name =
    Vir.
      {
        symbol_id = id;
        source_name = name;
        sort = Parametric binder;
        role = Input;
        span;
      }
  in
  let term symbol =
    Vir.{ parametric_sort = binder; parametric_desc = Parametric_symbol symbol }
  in
  let left = term (symbol 0 "left") and right = term (symbol 1 "right") in
  let obligation =
    Vir.
      {
        obligation_index = 0;
        function_ref = { function_index = 0; function_name = "backend" };
        kind = Assertion { assertion_ordinal = 0 };
        span;
        assumptions = [];
        required_preceding_safety = [];
        path_condition = [];
        goal = Parametric_equal (left, right);
        projection_symbols = [];
      }
  in
  let config =
    match Solver_backend.config ~timeout_ms:1000 with
    | Ok value -> value
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  reset_solver_counters ();
  match
    Solver_backend.For_testing.solve_after_translation Unknown config obligation
  with
  | Error (Solver_backend.Malformed_vir message) ->
      Printf.printf
        "backend-incapable rejected pre-solver solver-work=%d detail=%s\n"
        (solver_work ()) message
  | Error error ->
      fail "unexpected backend error: %s" (Solver_backend.error_to_string error)
  | Ok _ -> fail "named-sort-incapable backend unexpectedly accepted query"

let vir_structure_attacks () =
  let owner_a = Parametric_type.owner ~index:20 ~name:"vir-a" in
  let owner_b = Parametric_type.owner ~index:21 ~name:"vir-b" in
  let binder_a = Parametric_type.binder owner_a ~ordinal:0 in
  let binder_b = Parametric_type.binder owner_b ~ordinal:0 in
  let symbol binder id name =
    Vir.
      {
        symbol_id = id;
        source_name = name;
        sort = Parametric binder;
        role = Input;
        span;
      }
  in
  let leaf node_binder symbol =
    Vir.
      {
        parametric_sort = node_binder;
        parametric_desc = Parametric_symbol symbol;
      }
  in
  let valid = leaf binder_a (symbol binder_a 0 "valid") in
  let attack label forged =
    reset_solver_counters ();
    (match Parametric_logic_private.equal forged valid with
    | Error _ -> ()
    | Ok _ -> fail "attack %s passed canonical equality validation" label);
    let obligation =
      Vir.
        {
          obligation_index = 0;
          function_ref =
            { function_index = 20; function_name = "vir-structure" };
          kind = Assertion { assertion_ordinal = 0 };
          span;
          assumptions = [];
          required_preceding_safety = [];
          path_condition = [];
          goal = Parametric_equal (forged, valid);
          projection_symbols = [];
        }
    in
    let config = Z3_bridge.{ timeout_ms = 1000; model = false } in
    match
      Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_unknown config obligation
    with
    | Error (Z3_bridge.Malformed_vir detail) ->
        Printf.printf
          "attack=%s rejected boundary=vir-translation solver-work=%d detail=%s\n"
          label (solver_work ()) detail
    | Error error ->
        fail "attack %s reached unexpected Z3 error: %s" label
          (Z3_bridge.error_to_string error)
    | Ok _ -> fail "attack %s unexpectedly created a solver" label
  in
  let wrong_node =
    Vir.
      {
        parametric_sort = binder_a;
        parametric_desc =
          Parametric_conditional
            ( Boolean_constant true,
              leaf binder_b (symbol binder_a 1 "wrong-node"),
              valid );
      }
  in
  attack "nested-parametric-node-sort" wrong_node;
  let wrong_symbol =
    Vir.
      {
        parametric_sort = binder_a;
        parametric_desc =
          Parametric_conditional
            ( Boolean_constant true,
              leaf binder_a (symbol binder_b 2 "wrong-symbol"),
              valid );
      }
  in
  attack "nested-parametric-symbol-sort" wrong_symbol

let owner_unit () =
  let left_owner = Parametric_type.owner ~index:7 ~name:"left" in
  let right_owner = Parametric_type.owner ~index:11 ~name:"renamed" in
  let left = Parametric_type.binder left_owner ~ordinal:0 in
  let right = Parametric_type.binder right_owner ~ordinal:0 in
  let constructor =
    {
      Parametric_type.constructor_path = "test.generic";
      constructor_identity = "test:generic/1";
    }
  in
  let application argument = Parametric_type.Application (constructor, [ argument ]) in
  let left_type =
    application
      (Parametric_type.Tuple
         [ (None, Parametric_type.Parameter left); (None, Parametric_type.Int) ])
  in
  let right_type =
    application
      (Parametric_type.Tuple
         [
           (None, Parametric_type.Parameter right); (None, Parametric_type.Int);
         ])
  in
  if not (Parametric_type.alpha_equal left_type right_type) then
    fail "alpha comparison rejected renamed owner";
  let instantiated =
    Parametric_type.instantiate [ left ] [ Parametric_type.Bool ] left_type
  in
  (match instantiated with
  | Ok typ
    when Parametric_type.equal typ
           (application
              (Parametric_type.Tuple
                 [ (None, Parametric_type.Bool); (None, Parametric_type.Int) ]))
    ->
      ()
  | Ok _ | Error _ -> fail "capture-avoiding substitution failed");
  if not (Parametric_type.is_open left_type) then
    fail "open type was classified closed";
  (match Parametric_type.instantiate [ left ] [] left_type with
  | Error _ -> ()
  | Ok _ -> fail "missing type argument was accepted");
  print_endline
    "parametric owner checks: alpha, substitution, application, open-type, \
     arity"

let () =
  match Array.to_list Sys.argv with
  | [ _; "owner-unit" ] -> owner_unit ()
  | [ _; "call-attacks"; filename ] -> call_attacks filename
  | [ _; "structural-attacks"; filename ] -> structural_attacks filename
  | [ _; "optional-contract-attack"; filename ] ->
      optional_contract_attack filename
  | [ _; "backend-control" ] -> backend_control ()
  | [ _; "vir-structure-attacks" ] -> vir_structure_attacks ()
  | _ ->
      fail
        "usage: parametric_core_tool (owner-unit|call-attacks \
         CMT|structural-attacks CMT|optional-contract-attack \
         CMT|backend-control|vir-structure-attacks)"
