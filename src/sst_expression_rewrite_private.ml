type rule =
  (Sst.expression, Sst.expression_desc) Rewrite_private.rule

type phase = {
  direction : Rewrite_private.direction;
  repetition : Rewrite_private.repetition;
  max_nodes : int option;
  max_rewrites : int option;
  rules : rule list;
  rewrite_quantifier_triggers : bool;
}

let map_list_preserving map values =
  let rec loop changed reversed = function
    | [] -> if changed then List.rev reversed else values
    | value :: remaining ->
        let mapped = map value in
        loop (changed || mapped != value) (mapped :: reversed) remaining
  in
  loop false [] values

let map_option_preserving map = function
  | None as value -> value
  | Some value as option ->
      let mapped = map value in
      if mapped == value then option else Some mapped

let map_second_preserving map values =
  map_list_preserving
    (fun ((key, value) as pair) ->
      let mapped = map value in
      if mapped == value then pair else (key, mapped))
    values

let rewrite_trigger_if_valid ~kind recurse
    (quantifier [@delator.skip]) =
  match quantifier.Sst.quantifier_trigger with
  | None -> None
  | Some original as unchanged ->
      let rewritten = recurse original in
      if rewritten == original then unchanged
      else
        let candidate =
          { quantifier with Sst.quantifier_trigger = Some rewritten }
        in
        (match
           Quantifier_validation_private.validate_sst
             ~allow_unclassified:true kind candidate
         with
        | Ok () -> Some rewritten
        | Error (reason [@log_value.debug]) ->
            [%log.debug "retained quantifier trigger after unsafe rewrite"
              ~stage:(Delator.Field.string "sst-expression-rewrite")
              ~reason:
                (Delator.Field.string (reason [@log_value.debug]))
              ~decision:(Delator.Field.string "retained-original-trigger")];
            unchanged)
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let map_children_with_policy ~rewrite_quantifier_triggers recurse
    ((expression : Sst.expression) [@delator.skip]) =
  let original_desc = expression.expression_desc in
  let expression_desc =
    match original_desc with
    | Sst.Tuple_value values ->
        let values' = map_second_preserving recurse values in
        if values' == values then original_desc else Sst.Tuple_value values'
    | Sst.Record_value record ->
        let fields = map_second_preserving recurse record.fields in
        if fields == record.fields then original_desc
        else Sst.Record_value { record with fields }
    | Sst.Constructor_value constructor ->
        let arguments =
          map_list_preserving recurse constructor.arguments
        in
        if arguments == constructor.arguments then original_desc
        else Sst.Constructor_value { constructor with arguments }
    | Sst.Field_read read ->
        let record = recurse read.record in
        if record == read.record then original_desc
        else Sst.Field_read { read with record }
    | Sst.Field_write write ->
        let value = recurse write.value in
        if value == write.value then original_desc
        else Sst.Field_write { write with value }
    | Sst.Shared_scalar_field_write write ->
        let value = recurse write.value in
        if value == write.value then original_desc
        else Sst.Shared_scalar_field_write { write with value }
    | Sst.Owned_tree_nested_write write ->
        let value = recurse write.value in
        if value == write.value then original_desc
        else Sst.Owned_tree_nested_write { write with value }
    | Sst.Let_mutable (binding, initial, body) ->
        let initial' = recurse initial in
        let body' = recurse body in
        if initial' == initial && body' == body then original_desc
        else Sst.Let_mutable (binding, initial', body')
    | Sst.Mutable_write write ->
        let value = recurse write.value in
        if value == write.value then original_desc
        else Sst.Mutable_write { write with value }
    | Sst.Let (bindings, body) ->
        let bindings' = map_second_preserving recurse bindings in
        let body' = recurse body in
        if bindings' == bindings && body' == body then original_desc
        else Sst.Let (bindings', body')
    | Sst.Sequence (first, second) ->
        let first' = recurse first in
        let second' = recurse second in
        if first' == first && second' == second then original_desc
        else Sst.Sequence (first', second')
    | Sst.If (condition, consequent, alternative) ->
        let condition' = recurse condition in
        let consequent' = recurse consequent in
        let alternative' = map_option_preserving recurse alternative in
        if
          condition' == condition && consequent' == consequent
          && alternative' == alternative
        then original_desc
        else Sst.If (condition', consequent', alternative')
    | Sst.Match (scrutinee, cases) ->
        let scrutinee' = recurse scrutinee in
        let cases' =
          map_list_preserving
            (fun ((case : Sst.case) as original_case) ->
              let case_guard =
                map_option_preserving recurse case.case_guard
              in
              let case_body = recurse case.case_body in
              if
                case_guard == case.case_guard
                && case_body == case.case_body
              then original_case
              else
                {
                  case with
                  Sst.case_guard = case_guard;
                  case_body = case_body;
                })
            cases
        in
        if scrutinee' == scrutinee && cases' == cases then original_desc
        else Sst.Match (scrutinee', cases')
    | Sst.Checked_arithmetic (operation, operands) ->
        let operands' = map_list_preserving recurse operands in
        if operands' == operands then original_desc
        else Sst.Checked_arithmetic (operation, operands')
    | Sst.Compare (comparison, left, right) ->
        let left' = recurse left in
        let right' = recurse right in
        if left' == left && right' == right then original_desc
        else Sst.Compare (comparison, left', right')
    | Sst.Boolean_not operand ->
        let operand' = recurse operand in
        if operand' == operand then original_desc
        else Sst.Boolean_not operand'
    | Sst.Boolean_binary (operation, left, right) ->
        let left' = recurse left in
        let right' = recurse right in
        if left' == left && right' == right then original_desc
        else Sst.Boolean_binary (operation, left', right')
    | Sst.Forall quantifier ->
        let quantifier_body = recurse quantifier.quantifier_body in
        let quantifier_trigger =
          if rewrite_quantifier_triggers then
            rewrite_trigger_if_valid ~kind:Logic_quantifier_private.Forall
              recurse quantifier
          else quantifier.quantifier_trigger
        in
        if
          quantifier_body == quantifier.quantifier_body
          && quantifier_trigger == quantifier.quantifier_trigger
        then original_desc
        else Sst.Forall { quantifier with quantifier_body; quantifier_trigger }
    | Sst.Exists quantifier ->
        let quantifier_body = recurse quantifier.quantifier_body in
        let quantifier_trigger =
          if rewrite_quantifier_triggers then
            rewrite_trigger_if_valid ~kind:Logic_quantifier_private.Exists
              recurse quantifier
          else quantifier.quantifier_trigger
        in
        if
          quantifier_body == quantifier.quantifier_body
          && quantifier_trigger == quantifier.quantifier_trigger
        then original_desc
        else Sst.Exists { quantifier with quantifier_body; quantifier_trigger }
    | Sst.Direct_call call ->
        let arguments =
          map_list_preserving
            (function
              | Sst.Value_argument { label; value } as argument ->
                  let mapped = recurse value in
                  if mapped == value then argument
                  else Sst.Value_argument { label; value = mapped }
              | Sst.Callback_argument _ as argument -> argument)
            call.arguments
        in
        if arguments == call.arguments then original_desc
        else Sst.Direct_call { call with arguments }
    | Sst.Symbolic_application application ->
        let changed = ref false in
        let mapped =
          Symbolic_application_private.map_arguments
            (fun argument ->
              let mapped = recurse argument in
              if mapped != argument then changed := true;
              mapped)
            application
        in
        if not !changed then original_desc else Sst.Symbolic_application mapped
    | Sst.Callback_call application ->
        let arguments =
          map_second_preserving recurse application.arguments
        in
        if arguments == application.arguments then original_desc
        else Sst.Callback_call { application with arguments }
    | Sst.Callback_requires application ->
        let arguments =
          map_second_preserving recurse application.arguments
        in
        if arguments == application.arguments then original_desc
        else Sst.Callback_requires { application with arguments }
    | Sst.Callback_ensures { application; result } ->
        let arguments =
          map_second_preserving recurse application.arguments
        in
        let result' = recurse result in
        if arguments == application.arguments && result' == result then
          original_desc
        else
          Sst.Callback_ensures
            { application = { application with arguments }; result = result' }
    | Sst.Use_type_invariant use ->
        let value = recurse use.value in
        if value == use.value then original_desc
        else Sst.Use_type_invariant { use with value }
    | Sst.Local_assert assertion ->
        let predicate = recurse assertion.predicate in
        if predicate == assertion.predicate then original_desc
        else Sst.Local_assert { assertion with predicate }
    | Sst.Proof_region body ->
        let body' = recurse body in
        if body' == body then original_desc else Sst.Proof_region body'
    | Sst.Old payload ->
        let payload' = recurse payload in
        if payload' == payload then original_desc else Sst.Old payload'
    | Sst.Optional_present payload ->
        let payload' = recurse payload in
        if payload' == payload then original_desc
        else Sst.Optional_present payload'
    | Sst.Optional_forward payload ->
        let payload' = recurse payload in
        if payload' == payload then original_desc
        else Sst.Optional_forward payload'
    | Sst.Lift_runtime_int operand ->
        let operand' = recurse operand in
        if operand' == operand then original_desc
        else Sst.Lift_runtime_int operand'
    | ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Owned_tree_rebase _ | Sst.Mutable_read _
      | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Optional_absent ) ->
        original_desc
  in
  let rewritten =
    if expression_desc == original_desc then expression
    else { expression with Sst.expression_desc = expression_desc }
  in
  [%log.trace "mapped immediate SST expression children"
    ~stage:(Delator.Field.string "sst-expression-rewrite")
    ~semantic_sort:
      (Delator.Field.string (Parametric_type.to_string rewritten.Sst.typ))
    ~decision:
      (Delator.Field.string
         (if rewritten == expression then "unchanged" else "rebuilt"))];
  rewritten
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let replace expression expression_desc =
  if expression_desc == expression.Sst.expression_desc then expression
  else { expression with Sst.expression_desc = expression_desc }

let map_children =
  map_children_with_policy ~rewrite_quantifier_triggers:true

let generic_phase (phase : phase) :
    (Sst.expression, Sst.expression_desc) Rewrite_private.phase =
  {
    Rewrite_private.direction = phase.direction;
    repetition = phase.repetition;
    max_nodes = phase.max_nodes;
    max_rewrites = phase.max_rewrites;
    rules = phase.rules;
  }

let apply_rules ~rules expression =
  Rewrite_private.apply_rules ~replace ~rules expression

let bottom_up ~rules expression =
  Rewrite_private.bottom_up ~map_children ~replace ~rules expression

let top_down ~max_nodes ~rules expression =
  Rewrite_private.top_down ~max_nodes ~map_children ~replace ~rules expression

let apply_phase_with_stats phase expression =
  let map_children =
    map_children_with_policy
      ~rewrite_quantifier_triggers:phase.rewrite_quantifier_triggers
  in
  Rewrite_private.apply_phase_with_stats ~map_children ~replace
    (generic_phase phase) expression

let apply_phase phase expression =
  fst (apply_phase_with_stats phase expression)

let apply_phases_with_stats phases initial =
  let expression, reversed_stats =
    List.fold_left
      (fun (expression, stats) phase ->
        let expression, phase_stats =
          apply_phase_with_stats phase expression
        in
        (expression, phase_stats :: stats))
      (initial, []) phases
  in
  (expression, List.rev reversed_stats)

let apply_phases phases expression =
  fst (apply_phases_with_stats phases expression)
