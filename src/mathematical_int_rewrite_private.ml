type ring_mode =
  | Ring_disabled
  | Ring_reduce_only
  | Ring_canonicalize_bounded

type application_policy =
  | Preserve_applications
  | Treat_pure_specifications_as_atoms

type atom_class = Cheap_atom | Heavy_atom

type config = {
  ring_mode : ring_mode;
  application_policy : application_policy;
  allow_recursive_specification_atoms : bool;
  allow_heavy_atom_duplication : bool;
  additional_pure_total_atom : Sst.expression -> atom_class option;
  additional_obligation_free_atom : Sst.expression -> bool;
  additional_semantic_atom_equal :
    Sst.expression -> Sst.expression -> bool option;
  rewrite_quantifier_triggers : bool;
  normalize_comparisons : bool;
  normalize_absolute_value : bool;
  max_constant_nodes : int;
  max_constant_bits : int;
  max_ring_atoms : int;
  max_ring_input_nodes : int;
  max_ring_inspections : int;
  max_ring_monomials : int;
  max_ring_degree : int;
  max_ring_products : int;
  max_ring_output_nodes : int;
  ring_output_slack : int;
  max_phase_nodes : int;
  max_fixpoint_passes : int;
  max_rewrites : int option;
}

let e_matching_friendly_config =
  {
    ring_mode = Ring_reduce_only;
    application_policy = Preserve_applications;
    allow_recursive_specification_atoms = false;
    allow_heavy_atom_duplication = false;
    additional_pure_total_atom = (fun _ -> None);
    additional_obligation_free_atom = (fun _ -> false);
    additional_semantic_atom_equal = (fun _ _ -> None);
    rewrite_quantifier_triggers = false;
    normalize_comparisons = true;
    normalize_absolute_value = false;
    max_constant_nodes = 4_096;
    max_constant_bits = 8_192;
    max_ring_atoms = 32;
    max_ring_input_nodes = 512;
    max_ring_inspections = 4_096;
    max_ring_monomials = 128;
    max_ring_degree = 6;
    max_ring_products = 4_096;
    max_ring_output_nodes = 384;
    ring_output_slack = 0;
    max_phase_nodes = 1_000_000;
    max_fixpoint_passes = 4;
    max_rewrites = Some 100_000;
  }

let aggressive_config =
  {
    ring_mode = Ring_canonicalize_bounded;
    application_policy = Treat_pure_specifications_as_atoms;
    allow_recursive_specification_atoms = true;
    (* Applications may be collected as affine atoms, but are not duplicated by
       polynomial expansion. Inlining should expose their arithmetic first. *)
    allow_heavy_atom_duplication = false;
    additional_pure_total_atom = (fun _ -> None);
    additional_obligation_free_atom = (fun _ -> false);
    additional_semantic_atom_equal = (fun _ _ -> None);
    rewrite_quantifier_triggers = true;
    normalize_comparisons = true;
    normalize_absolute_value = true;
    max_constant_nodes = 16_384;
    max_constant_bits = 65_536;
    max_ring_atoms = 96;
    max_ring_input_nodes = 2_048;
    max_ring_inspections = 16_384;
    max_ring_monomials = 512;
    max_ring_degree = 10;
    max_ring_products = 32_768;
    max_ring_output_nodes = 2_048;
    ring_output_slack = 24;
    max_phase_nodes = 4_000_000;
    max_fixpoint_passes = 8;
    max_rewrites = Some 250_000;
  }

let default_config = e_matching_friendly_config

let require_positive name value =
  if value <= 0 then
    invalid_arg
      ("Mathematical_int_rewrite_private: " ^ name
     ^ " must be strictly positive")

let require_non_negative name value =
  if value < 0 then
    invalid_arg
      ("Mathematical_int_rewrite_private: " ^ name
     ^ " must be non-negative")

let validate_config config =
  require_positive "max_constant_nodes" config.max_constant_nodes;
  require_positive "max_constant_bits" config.max_constant_bits;
  require_positive "max_ring_atoms" config.max_ring_atoms;
  require_positive "max_ring_input_nodes" config.max_ring_input_nodes;
  require_positive "max_ring_inspections" config.max_ring_inspections;
  require_positive "max_ring_monomials" config.max_ring_monomials;
  require_positive "max_ring_degree" config.max_ring_degree;
  require_positive "max_ring_products" config.max_ring_products;
  require_positive "max_ring_output_nodes" config.max_ring_output_nodes;
  require_non_negative "ring_output_slack" config.ring_output_slack;
  require_positive "max_phase_nodes" config.max_phase_nodes;
  require_positive "max_fixpoint_passes" config.max_fixpoint_passes;
  match config.max_rewrites with
  | None -> ()
  | Some maximum -> require_non_negative "max_rewrites" maximum

let operation_has_valid_arity operation operands =
  match (operation, operands) with
  | ( (Sst.Negate | Sst.Multiply_constant _ | Sst.Successor | Sst.Predecessor
      | Sst.Absolute_value),
      [ _ ] ) ->
      true
  | (Sst.Add | Sst.Subtract | Sst.Multiply), [ _; _ ] -> true
  | ( (Sst.Add | Sst.Subtract | Sst.Negate | Sst.Multiply
      | Sst.Multiply_constant _ | Sst.Successor | Sst.Predecessor
      | Sst.Absolute_value),
      _ ) ->
      false

let bit_count value = Z.numbits (Z.abs value)
let is_zero value = Z.equal value Z.zero
let is_one value = Z.equal value Z.one
let minus_one = Z.neg Z.one
let is_minus_one value = Z.equal value minus_one

let rec constant_value (expression [@delator.skip]) =
  if expression.Sst.typ <> Sst.Mathematical_int then None
  else
    match expression.expression_desc with
    | Sst.Int_constant value -> Some value
    | Sst.Checked_arithmetic (operation, operands) ->
        let unary apply =
          match operands with
          | [ operand ] -> Option.map apply (constant_value operand)
          | _ -> None
        in
        let binary apply =
          match operands with
          | [ left; right ] ->
              Option.bind (constant_value left) (fun left ->
                  Option.map (apply left) (constant_value right))
          | _ -> None
        in
        let result =
          match operation with
          | Sst.Add -> binary Z.add
          | Sst.Subtract -> binary Z.sub
          | Sst.Negate -> unary Z.neg
          | Sst.Multiply -> binary Z.mul
          | Sst.Multiply_constant coefficient ->
              unary (Z.mul coefficient)
          | Sst.Successor -> unary Z.succ
          | Sst.Predecessor -> unary Z.pred
          | Sst.Absolute_value -> unary Z.abs
        in
        [%log.trace "evaluated mathematical integer constant subtree"
          ~stage:(Delator.Field.string "mathematical-int-rewrite")
          ~operation:
            (Delator.Field.string
               (match operation with
               | Sst.Add -> "add"
               | Sst.Subtract -> "subtract"
               | Sst.Negate -> "negate"
               | Sst.Multiply -> "multiply"
               | Sst.Multiply_constant _ -> "multiply-constant"
               | Sst.Successor -> "successor"
               | Sst.Predecessor -> "predecessor"
               | Sst.Absolute_value -> "absolute-value"))
          ~operand_count:(Delator.Field.int (List.length operands))
          ~decision:
            (Delator.Field.string
               (if Option.is_some result then "constant" else "not-constant"))];
        result
    | _ -> None
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

exception Constant_evaluation_limit

type constant_state = {
  config : config;
  mutable visited_nodes : int;
}

let check_constant_result state value =
  if bit_count value > state.config.max_constant_bits then
    raise Constant_evaluation_limit;
  value

let bounded_add state left right =
  if is_zero left then right
  else if is_zero right then left
  else check_constant_result state (Z.add left right)

let bounded_subtract state left right =
  if is_zero right then left
  else if Z.equal left right then Z.zero
  else check_constant_result state (Z.sub left right)

let bounded_multiply state left right =
  if is_zero left || is_zero right then Z.zero
  else if is_one left then right
  else if is_one right then left
  else if is_minus_one left then check_constant_result state (Z.neg right)
  else if is_minus_one right then check_constant_result state (Z.neg left)
  else (
    let estimated_bits = bit_count left + bit_count right in
    if estimated_bits > state.config.max_constant_bits + 1 then
      raise Constant_evaluation_limit;
    check_constant_result state (Z.mul left right))

let rec bounded_constant_value state expression =
  state.visited_nodes <- state.visited_nodes + 1;
  if state.visited_nodes > state.config.max_constant_nodes then
    raise Constant_evaluation_limit;
  if expression.Sst.typ <> Sst.Mathematical_int then None
  else
    match expression.expression_desc with
    | Sst.Int_constant value -> Some value
    | Sst.Checked_arithmetic (operation, operands) ->
        let unary apply =
          match operands with
          | [ operand ] -> Option.map apply (bounded_constant_value state operand)
          | _ -> None
        in
        let binary apply =
          match operands with
          | [ left; right ] ->
              Option.bind (bounded_constant_value state left) (fun left ->
                  Option.map (apply left) (bounded_constant_value state right))
          | _ -> None
        in
        (match operation with
        | Sst.Add -> binary (bounded_add state)
        | Sst.Subtract -> binary (bounded_subtract state)
        | Sst.Negate ->
            unary (fun value -> check_constant_result state (Z.neg value))
        | Sst.Multiply -> binary (bounded_multiply state)
        | Sst.Multiply_constant coefficient ->
            unary (bounded_multiply state coefficient)
        | Sst.Successor ->
            unary (fun value -> check_constant_result state (Z.succ value))
        | Sst.Predecessor ->
            unary (fun value -> check_constant_result state (Z.pred value))
        | Sst.Absolute_value ->
            unary (fun value -> check_constant_result state (Z.abs value)))
    | _ -> None

let constant_value_with config expression =
  try
    bounded_constant_value { config; visited_nodes = 0 } expression
  with Constant_evaluation_limit -> None

let immediate_constant_value expression =
  match (expression.Sst.typ, expression.expression_desc) with
  | Sst.Mathematical_int, Sst.Int_constant value -> Some value
  | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
    | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ), _ ->
      None

let immediate_operation_value config operation operands =
  let state = { config; visited_nodes = 0 } in
  let unary apply =
    match operands with
    | [ operand ] -> Option.map apply (immediate_constant_value operand)
    | _ -> None
  in
  let binary apply =
    match operands with
    | [ left; right ] ->
        Option.bind (immediate_constant_value left) (fun left ->
            Option.map (apply left) (immediate_constant_value right))
    | _ -> None
  in
  try
    match operation with
    | Sst.Add -> binary (bounded_add state)
    | Sst.Subtract -> binary (bounded_subtract state)
    | Sst.Negate ->
        unary (fun value -> check_constant_result state (Z.neg value))
    | Sst.Multiply -> binary (bounded_multiply state)
    | Sst.Multiply_constant coefficient ->
        unary (bounded_multiply state coefficient)
    | Sst.Successor ->
        unary (fun value -> check_constant_result state (Z.succ value))
    | Sst.Predecessor ->
        unary (fun value -> check_constant_result state (Z.pred value))
    | Sst.Absolute_value ->
        unary (fun value -> check_constant_result state (Z.abs value))
  with Constant_evaluation_limit -> None

let rec same_list same left right =
  match (left, right) with
  | [], [] -> true
  | left :: left_tail, right :: right_tail ->
      same left right && same_list same left_tail right_tail
  | [], _ :: _ | _ :: _, [] -> false

let same_option same left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> same left right
  | None, Some _ | Some _, None -> false

let same_checked_arithmetic left right =
  match (left, right) with
  | Sst.Add, Sst.Add
  | Sst.Subtract, Sst.Subtract
  | Sst.Negate, Sst.Negate
  | Sst.Multiply, Sst.Multiply
  | Sst.Successor, Sst.Successor
  | Sst.Predecessor, Sst.Predecessor
  | Sst.Absolute_value, Sst.Absolute_value ->
      true
  | Sst.Multiply_constant left, Sst.Multiply_constant right ->
      Z.equal left right
  | ( (Sst.Add | Sst.Subtract | Sst.Negate | Sst.Multiply
      | Sst.Multiply_constant _ | Sst.Successor | Sst.Predecessor
      | Sst.Absolute_value),
      _ ) ->
      false

let same_comparison left right =
  match (left, right) with
  | Sst.Equal, Sst.Equal
  | Sst.Not_equal, Sst.Not_equal
  | Sst.Less_than, Sst.Less_than
  | Sst.Less_or_equal, Sst.Less_or_equal
  | Sst.Greater_than, Sst.Greater_than
  | Sst.Greater_or_equal, Sst.Greater_or_equal ->
      true
  | ( (Sst.Equal | Sst.Not_equal | Sst.Less_than | Sst.Less_or_equal
      | Sst.Greater_than | Sst.Greater_or_equal),
      _ ) ->
      false

let same_boolean_operation left right =
  match (left, right) with
  | Sst.And, Sst.And | Sst.Or, Sst.Or -> true
  | (Sst.And | Sst.Or), _ -> false

let same_function_id left right =
  left.Sst.function_index = right.Sst.function_index
  && String.equal left.function_name right.function_name

let same_binding left right =
  left.Sst.id = right.Sst.id && left.typ = right.typ

let same_constructor_id left right =
  left.Sst.constructor_index = right.Sst.constructor_index
  && String.equal left.constructor_name right.constructor_name
  && left.constructor_type = right.constructor_type

let same_callback_binding left right =
  left.Sst.callback_id = right.Sst.callback_id

let is_obviously_pure_total_with_fuel config remaining root =
  (* Purity/totality inspection is deliberately bounded. Exhausting this
     budget classifies the expression as a barrier, which is conservative. *)
  let rec visit node =
    if !remaining = 0 then false
    else (
      remaining := !remaining - 1;
      match config.additional_pure_total_atom node with
      | Some _ -> true
      | None ->
          (match node.Sst.expression_desc with
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
          | Sst.Variable _ ->
              true
          | Sst.Tuple_value values ->
              List.for_all (fun (_, value) -> visit value) values
          | Sst.Record_value record ->
              List.for_all (fun (_, value) -> visit value) record.fields
          | Sst.Constructor_value constructor ->
              List.for_all visit constructor.arguments
          | Sst.Lift_runtime_int operand ->
              node.typ = Sst.Mathematical_int
              && (match (operand.Sst.typ, operand.expression_desc) with
              | Sst.Int, (Sst.Int_constant _ | Sst.Variable _) -> true
              | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
                | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _
                | Sst.Application _ ), _ ->
                  false)
          | Sst.Checked_arithmetic (operation, operands) ->
              node.typ = Sst.Mathematical_int
              && operation_has_valid_arity operation operands
              && List.for_all
                   (fun operand ->
                     operand.Sst.typ = Sst.Mathematical_int
                     && visit operand)
                   operands
          | Sst.Compare (_, left, right) ->
              node.typ = Sst.Bool
              && left.typ = right.typ
              && visit left && visit right
          | Sst.Boolean_not operand ->
              node.typ = Sst.Bool && operand.typ = Sst.Bool && visit operand
          | Sst.Boolean_binary (_, left, right) ->
              node.typ = Sst.Bool
              && left.typ = Sst.Bool && right.typ = Sst.Bool
              && visit left && visit right
          | Sst.If (condition, consequent, alternative) ->
              condition.typ = Sst.Bool
              && consequent.typ = node.typ
              && visit condition
              && visit consequent
              && (match alternative with
                 | None -> node.typ = Sst.Unit
                 | Some alternative ->
                     alternative.typ = node.typ && visit alternative)
          | Sst.Direct_call call ->
              config.application_policy
              = Treat_pure_specifications_as_atoms
              && call.call_form = Sst.Specification_call
              && ((not call.recursive)
                 || config.allow_recursive_specification_atoms)
              && List.for_all call_argument call.arguments
          | Sst.Symbolic_application _ ->
              config.application_policy
              = Treat_pure_specifications_as_atoms
              && (match
                    Sst.symbolic_application_arguments node.expression_desc
                  with
                 | None -> false
                 | Some arguments -> List.for_all visit arguments)
          | Sst.Old payload -> visit payload
          | Sst.Optional_absent -> true
          | Sst.Optional_present payload | Sst.Optional_forward payload ->
              visit payload
          | ( Sst.Field_read _ | Sst.Field_write _
            | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
            | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
            | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.Match _
            | Sst.Forall _ | Sst.Exists _ | Sst.Callback_call _
            | Sst.Callback_requires _ | Sst.Callback_ensures _ | Sst.Reveal _
            | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
            | Sst.Local_assert _ | Sst.Proof_region _ ) ->
              false))
  and call_argument = function
    | Sst.Value_argument { value; _ } -> visit value
    | Sst.Callback_argument _ -> true
  in
  visit root

let is_obviously_obligation_free config root =
  let remaining = ref config.max_ring_inspections in
  let rec visit expression =
    if !remaining = 0 then false
    else (
      remaining := !remaining - 1;
      match expression.Sst.expression_desc with
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ ->
          true
      | Sst.Lift_runtime_int operand ->
          expression.typ = Sst.Mathematical_int
          && (match (operand.Sst.typ, operand.expression_desc) with
             | Sst.Int, (Sst.Int_constant _ | Sst.Variable _) -> true
             | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
               | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _
               | Sst.Application _ ), _ ->
                 false)
      | Sst.Checked_arithmetic (operation, operands) ->
          expression.typ = Sst.Mathematical_int
          && operation_has_valid_arity operation operands
          && List.for_all visit operands
      | Sst.Compare (_, left, right) ->
          expression.typ = Sst.Bool && left.typ = right.typ
          && visit left && visit right
      | Sst.Boolean_not operand ->
          expression.typ = Sst.Bool && operand.typ = Sst.Bool && visit operand
      | Sst.Boolean_binary (_, left, right) ->
          expression.typ = Sst.Bool && left.typ = Sst.Bool
          && right.typ = Sst.Bool && visit left && visit right
      | Sst.Tuple_value values ->
          List.for_all (fun (_, value) -> visit value) values
      | Sst.Record_value record ->
          List.for_all (fun (_, value) -> visit value) record.fields
      | Sst.Constructor_value constructor ->
          List.for_all visit constructor.arguments
      | Sst.Old payload -> visit payload
      | Sst.Optional_absent -> true
      | Sst.Optional_present payload | Sst.Optional_forward payload ->
          visit payload
      | ( Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
        | Sst.Match _ | Sst.Forall _ | Sst.Exists _ | Sst.Direct_call _
        | Sst.Symbolic_application _ | Sst.Callback_call _
        | Sst.Callback_requires _ | Sst.Callback_ensures _ | Sst.Reveal _
        | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
        | Sst.Local_assert _ | Sst.Proof_region _ ) ->
          false)
  in
  visit root

let same_pure_expression_with_fuel config remaining left right =
  (* This is semantic-expression equality for the admitted pure fragment;
     source spans are intentionally ignored. Budget exhaustion merely misses a
     simplification and therefore remains sound. *)
  let rec expression left right =
    left == right
    || (if !remaining = 0 || left.Sst.typ <> right.Sst.typ then false
       else (
         remaining := !remaining - 1;
         match config.additional_semantic_atom_equal left right with
         | Some equal -> equal
         | None ->
             (match (left.expression_desc, right.expression_desc) with
             | Sst.Int_constant left, Sst.Int_constant right ->
                 Z.equal left right
             | Sst.Bool_constant left, Sst.Bool_constant right -> left = right
             | Sst.Unit_constant, Sst.Unit_constant -> true
             | Sst.Variable left, Sst.Variable right ->
                 same_binding left.binding right.binding
                 && left.use_uniqueness = right.use_uniqueness
             | Sst.Tuple_value left, Sst.Tuple_value right ->
                 same_list
                   (fun (left_label, left) (right_label, right) ->
                     left_label = right_label && expression left right)
                   left right
             | Sst.Record_value left, Sst.Record_value right ->
                 left.record_type = right.record_type
                 && same_list
                      (fun (left_field, left) (right_field, right) ->
                        left_field = right_field && expression left right)
                      left.fields right.fields
             | Sst.Constructor_value left, Sst.Constructor_value right ->
                 same_constructor_id left.constructor right.constructor
                 && same_list expression left.arguments right.arguments
             | Sst.Lift_runtime_int left, Sst.Lift_runtime_int right
             | Sst.Boolean_not left, Sst.Boolean_not right
             | Sst.Old left, Sst.Old right
             | Sst.Optional_present left, Sst.Optional_present right
             | Sst.Optional_forward left, Sst.Optional_forward right ->
                 expression left right
             | Sst.Optional_absent, Sst.Optional_absent -> true
             | ( Sst.Checked_arithmetic (left_operation, left_operands),
                 Sst.Checked_arithmetic (right_operation, right_operands) ) ->
                 same_checked_arithmetic left_operation right_operation
                 && same_list expression left_operands right_operands
             | ( Sst.Compare (left_comparison, left_left, left_right),
                 Sst.Compare (right_comparison, right_left, right_right) ) ->
                 same_comparison left_comparison right_comparison
                 && expression left_left right_left
                 && expression left_right right_right
             | ( Sst.Boolean_binary (left_operation, left_left, left_right),
                 Sst.Boolean_binary
                   (right_operation, right_left, right_right) ) ->
                 same_boolean_operation left_operation right_operation
                 && expression left_left right_left
                 && expression left_right right_right
             | ( Sst.If (left_condition, left_consequent, left_alternative),
                 Sst.If
                   (right_condition, right_consequent, right_alternative) ) ->
                 expression left_condition right_condition
                 && expression left_consequent right_consequent
                 && same_option expression left_alternative right_alternative
             | Sst.Direct_call left, Sst.Direct_call right ->
                 left.call_form = right.call_form
                 && same_function_id left.callee right.callee
                 && left.type_arguments = right.type_arguments
                 && left.recursive = right.recursive
                 && same_list call_argument left.arguments right.arguments
             | ( Sst.Symbolic_application left,
                 Sst.Symbolic_application right ) ->
                 Symbolic_application_private.same_head left right
                 && same_list expression
                      (Symbolic_application_private.arguments left)
                      (Symbolic_application_private.arguments right)
             | _, _ -> false)))
  and call_argument left right =
    match (left, right) with
    | ( Sst.Value_argument { label = left_label; value = left_value },
        Sst.Value_argument { label = right_label; value = right_value } ) ->
        left_label = right_label && expression left_value right_value
    | ( Sst.Callback_argument
          { label = left_label; callback = left_callback },
        Sst.Callback_argument
          { label = right_label; callback = right_callback } ) ->
        left_label = right_label
        && same_callback_binding left_callback right_callback
    | Sst.Value_argument _, Sst.Callback_argument _
    | Sst.Callback_argument _, Sst.Value_argument _ ->
        false
  in
  expression left right

let same_pure_expression config left right =
  let remaining = ref config.max_ring_inspections in
  same_pure_expression_with_fuel config remaining left right

type atom_policy = { cost : atom_class; must_preserve : bool }

let ring_atom_policy_with_fuel config remaining root =
  let rec classify expression =
    if !remaining = 0 || expression.Sst.typ <> Sst.Mathematical_int then None
    else (
      remaining := !remaining - 1;
      match config.additional_pure_total_atom expression with
      | Some cost ->
          Some
            {
              cost;
              must_preserve =
                not (config.additional_obligation_free_atom expression);
            }
      | None ->
          (match expression.expression_desc with
          | Sst.Variable _ -> Some { cost = Cheap_atom; must_preserve = false }
          | Sst.Lift_runtime_int operand ->
              (match (operand.Sst.typ, operand.expression_desc) with
              | Sst.Int, (Sst.Int_constant _ | Sst.Variable _) ->
                  Some { cost = Cheap_atom; must_preserve = false }
              | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
                | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _
                | Sst.Application _ ), _ ->
                  None)
          | Sst.Old operand -> classify operand
          | Sst.Checked_arithmetic (Sst.Absolute_value, [ operand ])
            when
              is_obviously_pure_total_with_fuel config remaining operand ->
              Some { cost = Heavy_atom; must_preserve = true }
          | Sst.Direct_call _ | Sst.Symbolic_application _ | Sst.If _
            when config.application_policy
                 = Treat_pure_specifications_as_atoms
                 && is_obviously_pure_total_with_fuel config remaining
                      expression ->
              Some { cost = Heavy_atom; must_preserve = true }
          | ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
            | Sst.Tuple_value _ | Sst.Record_value _ | Sst.Constructor_value _
            | Sst.Field_read _ | Sst.Field_write _
            | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
            | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
            | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
            | Sst.Match _ | Sst.Checked_arithmetic _ | Sst.Compare _
            | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Forall _
            | Sst.Exists _ | Sst.Direct_call _ | Sst.Symbolic_application _
            | Sst.Callback_call _ | Sst.Callback_requires _
            | Sst.Callback_ensures _ | Sst.Optional_absent
            | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
            | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
            | Sst.Local_assert _ | Sst.Proof_region _ ) ->
              None))
  in
  classify root

let is_well_formed_mathematical_node expression =
  expression.Sst.typ = Sst.Mathematical_int
  && (match expression.expression_desc with
     | Sst.Checked_arithmetic (operation, operands) ->
         operation_has_valid_arity operation operands
         && List.for_all
              (fun operand -> operand.Sst.typ = Sst.Mathematical_int)
              operands
     | _ -> true)

let replacement_if_changed config expression expression_desc =
  let candidate = { expression with Sst.expression_desc = expression_desc } in
  if same_pure_expression config expression candidate then None
  else Some expression_desc

let math_expression span expression_desc =
  { Sst.expression_desc = expression_desc; typ = Sst.Mathematical_int; span }

let constant_expression span value = math_expression span (Sst.Int_constant value)

let checked_expression span operation operands =
  math_expression span (Sst.Checked_arithmetic (operation, operands))

let fold_constant_arithmetic_with (config [@delator.skip])
    (expression [@delator.skip]) =
  match (expression.Sst.typ, expression.expression_desc) with
  | ( Sst.Mathematical_int,
      Sst.Checked_arithmetic
        ( (operation [@log_value.debug]),
          (operands [@log_value.debug]) ) ) -> (
      match constant_value_with config expression with
      | None -> None
      | Some value ->
          [%log.debug "folded mathematical integer constant subtree"
            ~stage:(Delator.Field.string "mathematical-int-rewrite")
            ~operation:
              (Delator.Field.string
                 (match (operation [@log_value.debug]) with
                 | Sst.Add -> "add"
                 | Sst.Subtract -> "subtract"
                 | Sst.Negate -> "negate"
                 | Sst.Multiply -> "multiply"
                 | Sst.Multiply_constant _ -> "multiply-constant"
                 | Sst.Successor -> "successor"
                 | Sst.Predecessor -> "predecessor"
                 | Sst.Absolute_value -> "absolute-value"))
            ~operand_count:
              (Delator.Field.int
                 (List.length (operands [@log_value.debug])))
            ~bit_count:(Delator.Field.int (bit_count value))
            ~rewrite_class:(Delator.Field.string "constant-fold")
            ~decision:(Delator.Field.string "rewritten")];
          Some (Sst.Int_constant value))
  | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
    | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ), _ ->
      None
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let fold_constant_arithmetic =
  fold_constant_arithmetic_with default_config

(* The default simplifier is bottom-up, so a constant child has already been
   replaced by [Int_constant]. Restricting the hot-path rule to immediate
   literals avoids repeatedly rescanning non-constant arithmetic chains. The
   exported [fold_constant_arithmetic] above retains the recursive behaviour
   expected of the legacy standalone rule. *)
let fold_bottom_up_constant_arithmetic_with (config [@delator.skip])
    (expression [@delator.skip]) =
  match (expression.Sst.typ, expression.expression_desc) with
  | Sst.Mathematical_int, Sst.Checked_arithmetic (operation, operands) -> (
      match immediate_operation_value config operation operands with
      | None -> None
      | Some value ->
          [%log.debug "folded bottom-up mathematical integer constants"
            ~stage:(Delator.Field.string "mathematical-int-rewrite")
            ~operation:
              (Delator.Field.string
                 (match operation with
                 | Sst.Add -> "add"
                 | Sst.Subtract -> "subtract"
                 | Sst.Negate -> "negate"
                 | Sst.Multiply -> "multiply"
                 | Sst.Multiply_constant _ -> "multiply-constant"
                 | Sst.Successor -> "successor"
                 | Sst.Predecessor -> "predecessor"
                 | Sst.Absolute_value -> "absolute-value"))
            ~operand_count:(Delator.Field.int (List.length operands))
            ~bit_count:(Delator.Field.int (bit_count value))
            ~rewrite_class:(Delator.Field.string "constant-fold")
            ~decision:(Delator.Field.string "rewritten")];
          Some (Sst.Int_constant value))
  | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
    | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ), _ ->
      None
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let normalize_constant_multiplication (expression [@delator.skip]) =
  match (expression.Sst.typ, expression.expression_desc) with
  | ( Sst.Mathematical_int,
      Sst.Checked_arithmetic
        ( Sst.Multiply,
          [
            {
              Sst.expression_desc = Sst.Int_constant value;
              typ = Sst.Mathematical_int;
              _;
            };
            ({ Sst.typ = Sst.Mathematical_int; _ } as operand);
          ] ) )
  | ( Sst.Mathematical_int,
      Sst.Checked_arithmetic
        ( Sst.Multiply,
          [
            ({ Sst.typ = Sst.Mathematical_int; _ } as operand);
            {
              Sst.expression_desc = Sst.Int_constant value;
              typ = Sst.Mathematical_int;
              _;
            };
          ] ) ) ->
      [%log.debug "normalized mathematical integer constant scaling"
        ~stage:(Delator.Field.string "mathematical-int-rewrite")
        ~operation:(Delator.Field.string "multiply")
        ~operand_count:(Delator.Field.int 2)
        ~coefficient_bit_count:(Delator.Field.int (bit_count value))
        ~rewrite_class:(Delator.Field.string "constant-scale")
        ~decision:(Delator.Field.string "rewritten")];
      Some (Sst.Checked_arithmetic (Sst.Multiply_constant value, [ operand ]))
  | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
    | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ), _ ->
      None
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let bounded_coefficient_product config left right =
  try
    let state = { config; visited_nodes = 0 } in
    Some (bounded_multiply state left right)
  with Constant_evaluation_limit -> None

let bounded_coefficient_unary config operation coefficient =
  try
    let state = { config; visited_nodes = 0 } in
    Some (check_constant_result state (operation coefficient))
  with Constant_evaluation_limit -> None

let coefficient_desc config coefficient operand =
  if is_zero coefficient && is_obviously_obligation_free config operand then
    Sst.Int_constant Z.zero
  else if is_one coefficient then operand.Sst.expression_desc
  else if is_minus_one coefficient then
    Sst.Checked_arithmetic (Sst.Negate, [ operand ])
  else
    Sst.Checked_arithmetic
      (Sst.Multiply_constant coefficient, [ operand ])

let simplify_arithmetic_identities_with config
    (expression [@delator.skip]) =
  let replacement expression_desc =
    replacement_if_changed config expression expression_desc
  in
  if not (is_well_formed_mathematical_node expression) then None
  else
    match expression.expression_desc with
    | Sst.Checked_arithmetic
        (Sst.Add, [ ({ Sst.expression_desc = Sst.Int_constant zero; _ }); right ])
      when is_zero zero ->
        replacement right.expression_desc
    | Sst.Checked_arithmetic
        (Sst.Add, [ left; ({ Sst.expression_desc = Sst.Int_constant zero; _ }) ])
      when is_zero zero ->
        replacement left.expression_desc
    | Sst.Checked_arithmetic
        ( Sst.Subtract,
          [ left; ({ Sst.expression_desc = Sst.Int_constant zero; _ }) ] )
      when is_zero zero ->
        replacement left.expression_desc
    | Sst.Checked_arithmetic
        ( Sst.Subtract,
          [ ({ Sst.expression_desc = Sst.Int_constant zero; _ }); right ] )
      when is_zero zero ->
        replacement (Sst.Checked_arithmetic (Sst.Negate, [ right ]))
    | Sst.Checked_arithmetic
        ( Sst.Negate,
          [
            {
              Sst.expression_desc = Sst.Checked_arithmetic
                (Sst.Negate, [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ]);
              _;
            };
          ] ) ->
        replacement inner.expression_desc
    | Sst.Checked_arithmetic
        ( Sst.Negate,
          [
            {
              Sst.expression_desc =
                Sst.Checked_arithmetic
                  ( Sst.Multiply_constant coefficient,
                    [ ({ Sst.typ = Sst.Mathematical_int; _ } as operand) ] );
              _;
            };
          ] ) ->
        Option.bind
          (bounded_coefficient_unary config Z.neg coefficient)
          (fun coefficient ->
            replacement (coefficient_desc config coefficient operand))
    | Sst.Checked_arithmetic
        (Sst.Multiply_constant coefficient, [ operand ]) -> (
        match operand.expression_desc with
        | Sst.Checked_arithmetic
            (Sst.Negate, [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ]) ->
            Option.bind
              (bounded_coefficient_unary config Z.neg coefficient)
              (fun coefficient ->
                replacement (coefficient_desc config coefficient inner))
        | Sst.Checked_arithmetic
            ( Sst.Multiply_constant inner_coefficient,
              [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ] ) -> (
            match
              bounded_coefficient_product config coefficient inner_coefficient
            with
            | None -> None
            | Some combined ->
                replacement (coefficient_desc config combined inner))
        | _ ->
            if is_zero coefficient then
              if is_obviously_obligation_free config operand then
                replacement (Sst.Int_constant Z.zero)
              else None
            else if is_one coefficient then replacement operand.expression_desc
            else if is_minus_one coefficient then
              replacement
                (Sst.Checked_arithmetic (Sst.Negate, [ operand ]))
            else None)
    | Sst.Checked_arithmetic
        ( Sst.Successor,
          [
            {
              Sst.expression_desc =
                Sst.Checked_arithmetic
                  ( Sst.Predecessor,
                    [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ] );
              _;
            };
          ] )
    | Sst.Checked_arithmetic
        ( Sst.Predecessor,
          [
            {
              Sst.expression_desc =
                Sst.Checked_arithmetic
                  ( Sst.Successor,
                    [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ] );
              _;
            };
          ] ) ->
        replacement inner.expression_desc
    | Sst.Checked_arithmetic
        ( Sst.Absolute_value,
          [
            {
              Sst.expression_desc = Sst.Checked_arithmetic
                (Sst.Negate, [ ({ Sst.typ = Sst.Mathematical_int; _ } as inner) ]);
              _;
            };
          ] ) ->
        replacement
          (Sst.Checked_arithmetic (Sst.Absolute_value, [ inner ]))
    | Sst.Checked_arithmetic
        ( Sst.Absolute_value,
          [
            ({
               Sst.expression_desc =
                 Sst.Checked_arithmetic (Sst.Absolute_value, [ _ ]);
               _;
             } as inner);
          ] ) ->
        replacement inner.expression_desc
    | Sst.Checked_arithmetic
        ( Sst.Absolute_value,
          [
            {
              Sst.expression_desc =
                Sst.Checked_arithmetic
                  ( Sst.Multiply_constant coefficient,
                    [ ({ Sst.typ = Sst.Mathematical_int; _ } as operand) ] );
              _;
            };
          ] )
      when config.normalize_absolute_value ->
        let absolute_operand =
          checked_expression expression.span Sst.Absolute_value [ operand ]
        in
        Option.bind
          (bounded_coefficient_unary config Z.abs coefficient)
          (fun coefficient ->
            replacement
              (coefficient_desc config coefficient absolute_operand))
    | ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
      | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
      | Sst.Match _ | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _
      | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
      | Sst.Forall _ | Sst.Exists _ | Sst.Direct_call _
      | Sst.Symbolic_application _ | Sst.Callback_call _
      | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _
      | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
      | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _ ) ->
        None

let simplify_arithmetic_identities =
  simplify_arithmetic_identities_with default_config

module Ring = struct
  module Monomial = struct
    type t = int list

    let compare = Stdlib.compare
  end

  module Polynomial = Map.Make (Monomial)

  type abort_reason =
    | Missing_atom
    | Inadmissible_atom
    | Atom_budget
    | Coefficient_budget
    | Monomial_budget
    | Coefficient_product_budget
    | Product_budget
    | Degree_budget
    | Input_budget
    | Sort_mismatch
    | Invalid_arity
    | Empty_monomial

  exception Abort of abort_reason

  type atom = {
    id : int;
    expression : Sst.expression;
    mutable cost : atom_class;
    mutable must_preserve : bool;
    mutable input_occurrences : int;
  }

  type state = {
    config : config;
    mutable atoms : atom list;
    mutable atom_count : int;
    mutable input_nodes : int;
    inspection_fuel : int ref;
    mutable product_count : int;
    mutable collection_events : int;
    mutable cancellation_events : int;
  }

  let abort reason = raise (Abort reason)

  let create_state config =
    {
      config;
      atoms = [];
      atom_count = 0;
      input_nodes = 0;
      inspection_fuel = ref config.max_ring_inspections;
      product_count = 0;
      collection_events = 0;
      cancellation_events = 0;
    }

  let find_atom state id =
    let rec find = function
      | [] -> abort Missing_atom
      | atom :: remaining ->
          if atom.id = id then atom else find remaining
    in
    find state.atoms

  let intern_atom state expression =
    let policy =
      match
        ring_atom_policy_with_fuel state.config state.inspection_fuel expression
      with
      | None -> abort Inadmissible_atom
      | Some policy -> policy
    in
    let rec find = function
      | [] -> None
      | atom :: remaining ->
          if
            same_pure_expression_with_fuel state.config state.inspection_fuel
              atom.expression expression
          then
            Some atom
          else find remaining
    in
    match find state.atoms with
    | Some atom ->
        atom.input_occurrences <- atom.input_occurrences + 1;
        if policy.cost = Heavy_atom then atom.cost <- Heavy_atom;
        atom.must_preserve <- atom.must_preserve || policy.must_preserve;
        atom.id
    | None ->
        if state.atom_count >= state.config.max_ring_atoms then
          abort Atom_budget;
        let id = state.atom_count in
        state.atom_count <- state.atom_count + 1;
        state.atoms <-
          {
            id;
            expression;
            cost = policy.cost;
            must_preserve = policy.must_preserve;
            input_occurrences = 1;
          }
          :: state.atoms;
        id

  let polynomial_find monomial polynomial =
    try Some (Polynomial.find monomial polynomial) with Not_found -> None

  let check_coefficient state coefficient =
    if bit_count coefficient > state.config.max_constant_bits then
      abort Coefficient_budget;
    coefficient

  let set_coefficient state monomial coefficient polynomial =
    let polynomial =
      if is_zero coefficient then Polynomial.remove monomial polynomial
      else
        Polynomial.add monomial (check_coefficient state coefficient)
          polynomial
    in
    if Polynomial.cardinal polynomial > state.config.max_ring_monomials then
      abort Monomial_budget;
    polynomial

  let add_term state monomial coefficient polynomial =
    let coefficient =
      match polynomial_find monomial polynomial with
      | None -> coefficient
      | Some previous ->
          state.collection_events <- state.collection_events + 1;
          let combined = Z.add previous coefficient in
          if is_zero combined then
            state.cancellation_events <- state.cancellation_events + 1;
          combined
    in
    set_coefficient state monomial coefficient polynomial

  let constant state value =
    if is_zero value then Polynomial.empty
    else Polynomial.singleton [] (check_coefficient state value)

  let atom id = Polynomial.singleton [ id ] Z.one

  let add state left right =
    Polynomial.fold
      (fun monomial coefficient result ->
        add_term state monomial coefficient result)
      right left

  let negate state polynomial =
    Polynomial.fold
      (fun monomial coefficient result ->
        set_coefficient state monomial (Z.neg coefficient) result)
      polynomial Polynomial.empty

  let subtract state left right = add state left (negate state right)

  let multiply_coefficient state left right =
    match bounded_coefficient_product state.config left right with
    | None -> abort Coefficient_product_budget
    | Some coefficient -> coefficient

  let scale state coefficient polynomial =
    if is_zero coefficient then Polynomial.empty
    else
      Polynomial.fold
        (fun monomial inner_coefficient result ->
          set_coefficient state monomial
            (multiply_coefficient state coefficient inner_coefficient)
            result)
        polynomial Polynomial.empty

  let rec merge_monomials left right =
    match (left, right) with
    | [], remaining | remaining, [] -> remaining
    | left_head :: left_tail, right_head :: right_tail ->
        if left_head <= right_head then
          left_head :: merge_monomials left_tail right
        else right_head :: merge_monomials left right_tail


  let multiply state left right =
    if Polynomial.is_empty left || Polynomial.is_empty right then
      Polynomial.empty
    else (
      let left_size = Polynomial.cardinal left in
      let right_size = Polynomial.cardinal right in
      if left_size > state.config.max_ring_products / right_size then
        abort Product_budget;
      let products = left_size * right_size in
      if state.product_count > state.config.max_ring_products - products then
        abort Product_budget;
      state.product_count <- state.product_count + products;
      Polynomial.fold
        (fun left_monomial left_coefficient result ->
          Polynomial.fold
            (fun right_monomial right_coefficient result ->
              let monomial =
                merge_monomials left_monomial right_monomial
              in
              if List.length monomial > state.config.max_ring_degree then
                abort Degree_budget;
              add_term state monomial
                (multiply_coefficient state left_coefficient right_coefficient)
                result)
            right result)
        left Polynomial.empty)

  let rec of_expression state expression =
    state.input_nodes <- state.input_nodes + 1;
    if state.input_nodes > state.config.max_ring_input_nodes then
      abort Input_budget;
    if expression.Sst.typ <> Sst.Mathematical_int then abort Sort_mismatch;
    match expression.expression_desc with
    | Sst.Int_constant value -> constant state value
    | Sst.Checked_arithmetic (operation, operands) -> (
        match (operation, operands) with
        | Sst.Add, [ left; right ] ->
            let left = of_expression state left in
            let right = of_expression state right in
            add state left right
        | Sst.Subtract, [ left; right ] ->
            let left = of_expression state left in
            let right = of_expression state right in
            subtract state left right
        | Sst.Negate, [ operand ] ->
            let operand = of_expression state operand in
            negate state operand
        | Sst.Multiply, [ left; right ] ->
            let left = of_expression state left in
            let right = of_expression state right in
            multiply state left right
        | Sst.Multiply_constant coefficient, [ operand ] ->
            let operand = of_expression state operand in
            scale state coefficient operand
        | Sst.Successor, [ operand ] ->
            let operand = of_expression state operand in
            add state operand (constant state Z.one)
        | Sst.Predecessor, [ operand ] ->
            let operand = of_expression state operand in
            subtract state operand (constant state Z.one)
        | Sst.Absolute_value, [ _ ] -> atom (intern_atom state expression)
        | ( (Sst.Add | Sst.Subtract | Sst.Negate | Sst.Multiply
            | Sst.Multiply_constant _ | Sst.Successor | Sst.Predecessor
            | Sst.Absolute_value),
            _ ) ->
            abort Invalid_arity)
    | _ -> atom (intern_atom state expression)

  type measure = {
    arithmetic_nodes : int;
    nonlinear_nodes : int;
    max_degree : int;
    leaf_occurrences : int;
    coefficient_bits : int;
  }

  let zero_measure =
    {
      arithmetic_nodes = 0;
      nonlinear_nodes = 0;
      max_degree = 0;
      leaf_occurrences = 0;
      coefficient_bits = 0;
    }

  let combine_measure left right =
    {
      arithmetic_nodes = left.arithmetic_nodes + right.arithmetic_nodes;
      nonlinear_nodes = left.nonlinear_nodes + right.nonlinear_nodes;
      max_degree = max left.max_degree right.max_degree;
      leaf_occurrences = left.leaf_occurrences + right.leaf_occurrences;
      coefficient_bits = left.coefficient_bits + right.coefficient_bits;
    }

  let add_arithmetic_node measure =
    { measure with arithmetic_nodes = measure.arithmetic_nodes + 1 }

  let rec expression_measure expression =
    if expression.Sst.typ <> Sst.Mathematical_int then
      { zero_measure with max_degree = 1; leaf_occurrences = 1 }
    else
      match expression.expression_desc with
      | Sst.Int_constant value ->
          if is_zero value then zero_measure
          else
            {
              zero_measure with
              leaf_occurrences = 1;
              coefficient_bits = bit_count value;
            }
      | Sst.Checked_arithmetic (operation, operands) -> (
          match (operation, operands) with
          | (Sst.Add | Sst.Subtract), [ left; right ] ->
              add_arithmetic_node
                (combine_measure
                   (expression_measure left)
                   (expression_measure right))
          | Sst.Negate, [ operand ] ->
              add_arithmetic_node (expression_measure operand)
          | Sst.Multiply, [ left; right ] ->
              let left = expression_measure left in
              let right = expression_measure right in
              let combined = combine_measure left right in
              {
                combined with
                arithmetic_nodes = combined.arithmetic_nodes + 1;
                nonlinear_nodes =
                  combined.nonlinear_nodes
                  + (if left.max_degree > 0 && right.max_degree > 0 then 1
                     else 0);
                max_degree = left.max_degree + right.max_degree;
              }
          | Sst.Multiply_constant coefficient, [ operand ] ->
              let operand = expression_measure operand in
              {
                operand with
                arithmetic_nodes = operand.arithmetic_nodes + 1;
                max_degree =
                  if is_zero coefficient then 0 else operand.max_degree;
                coefficient_bits =
                  operand.coefficient_bits + bit_count coefficient;
              }
          | (Sst.Successor | Sst.Predecessor), [ operand ] ->
              let operand = expression_measure operand in
              {
                operand with
                arithmetic_nodes = operand.arithmetic_nodes + 1;
                leaf_occurrences = operand.leaf_occurrences + 1;
                coefficient_bits = operand.coefficient_bits + 1;
              }
          | Sst.Absolute_value, [ operand ] ->
              let operand = expression_measure operand in
              {
                operand with
                arithmetic_nodes = operand.arithmetic_nodes + 1;
                max_degree = 1;
              }
          | ( (Sst.Add | Sst.Subtract | Sst.Negate | Sst.Multiply
              | Sst.Multiply_constant _ | Sst.Successor | Sst.Predecessor
              | Sst.Absolute_value),
              _ ) ->
              {
                zero_measure with
                arithmetic_nodes = 1;
                max_degree = 1;
                leaf_occurrences = 1;
              })
      | _ -> { zero_measure with max_degree = 1; leaf_occurrences = 1 }

  let polynomial_measure polynomial =
    let term_count = Polynomial.cardinal polynomial in
    let measure =
      Polynomial.fold
        (fun monomial coefficient measure ->
          let degree = List.length monomial in
          let coefficient_node =
            if degree = 0 || is_one coefficient then 0 else 1
          in
          let multiplication_nodes = max 0 (degree - 1) in
          {
            arithmetic_nodes =
              measure.arithmetic_nodes + coefficient_node
              + multiplication_nodes;
            nonlinear_nodes =
              measure.nonlinear_nodes + multiplication_nodes;
            max_degree = max measure.max_degree degree;
            leaf_occurrences =
              measure.leaf_occurrences + (if degree = 0 then 1 else degree);
            coefficient_bits =
              measure.coefficient_bits
              + (if is_one coefficient || is_minus_one coefficient then 0
                 else bit_count coefficient);
          })
        polynomial zero_measure
    in
    {
      measure with
      arithmetic_nodes = measure.arithmetic_nodes + max 0 (term_count - 1);
    }

  let compare_measure left right =
    let compare_field left right continuation =
      let compared = Stdlib.compare left right in
      if compared <> 0 then compared else continuation ()
    in
    compare_field (left.max_degree > 1) (right.max_degree > 1) (fun () ->
        compare_field left.max_degree right.max_degree (fun () ->
            compare_field left.nonlinear_nodes right.nonlinear_nodes (fun () ->
                compare_field left.leaf_occurrences right.leaf_occurrences
                  (fun () ->
                    compare_field left.arithmetic_nodes right.arithmetic_nodes
                      (fun () ->
                        Stdlib.compare left.coefficient_bits
                          right.coefficient_bits)))))

  let expression_of_monomial state span monomial =
    match monomial with
    | [] -> abort Empty_monomial
    | first :: remaining ->
        List.fold_left
          (fun product id ->
            checked_expression span Sst.Multiply
              [ product; (find_atom state id).expression ])
          (find_atom state first).expression remaining

  let expression_of_polynomial state span polynomial =
    let bindings = Polynomial.bindings polynomial in
    let non_constants, constants =
      List.partition (fun (monomial, _) -> monomial <> []) bindings
    in
    let ordered = non_constants @ constants in
    let term_expression (monomial, coefficient) =
      match monomial with
      | [] -> constant_expression span coefficient
      | _ ->
          let base = expression_of_monomial state span monomial in
          math_expression span (coefficient_desc state.config coefficient base)
    in
    match List.map term_expression ordered with
    | [] -> constant_expression span Z.zero
    | first :: remaining ->
        List.fold_left
          (fun sum term -> checked_expression span Sst.Add [ sum; term ])
          first remaining

  let heavy_atom_occurrences_are_safe state polynomial =
    let output_occurrences = Array.make state.atom_count 0 in
    Polynomial.iter
      (fun monomial _ ->
        List.iter
          (fun id ->
            output_occurrences.(id) <- output_occurrences.(id) + 1)
          monomial)
      polynomial;
    List.for_all
      (fun atom ->
        (not atom.must_preserve || output_occurrences.(atom.id) > 0)
        && (atom.cost = Cheap_atom
           || state.config.allow_heavy_atom_duplication
           || output_occurrences.(atom.id) <= atom.input_occurrences))
      state.atoms

  let emitted_node_count measure =
    measure.arithmetic_nodes + measure.leaf_occurrences

  let should_emit config ~cancelled_terms input output =
    if emitted_node_count output > config.max_ring_output_nodes then false
    else
      let strict_improvement = compare_measure output input < 0 in
      match config.ring_mode with
      | Ring_disabled -> false
      | Ring_reduce_only -> strict_improvement
      | Ring_canonicalize_bounded ->
          strict_improvement
          || (cancelled_terms
             && emitted_node_count output
                <= emitted_node_count input + config.ring_output_slack)

  let normalize_arithmetic config expression =
    match (config.ring_mode, expression.Sst.typ, expression.expression_desc) with
    | Ring_disabled, _, _ -> None
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        Sst.Mathematical_int,
        Sst.Checked_arithmetic _ ) -> (
        try
          let state = create_state config in
          let polynomial = of_expression state expression in
          let input = expression_measure expression in
          let output = polynomial_measure polynomial in
          let cancelled_terms = state.cancellation_events > 0 in
          if not (heavy_atom_occurrences_are_safe state polynomial) then (
            [%log.debug "preserved arithmetic carrying verifier obligations"
              ~stage:(Delator.Field.string "mathematical-int-rewrite")
              ~atom_count:(Delator.Field.int state.atom_count)
              ~rewrite_class:(Delator.Field.string "bounded-ring")
              ~decision:(Delator.Field.string "preserved-original")];
            None)
          else if not (should_emit config ~cancelled_terms input output) then (
            [%log.trace "preserved arithmetic after solver-cost comparison"
              ~stage:(Delator.Field.string "mathematical-int-rewrite")
              ~input_node_count:
                (Delator.Field.int (emitted_node_count input))
              ~output_node_count:
                (Delator.Field.int (emitted_node_count output))
              ~cancelled_terms:(Delator.Field.bool cancelled_terms)
              ~rewrite_class:(Delator.Field.string "bounded-ring")
              ~decision:(Delator.Field.string "preserved-original")];
            None)
          else
            let candidate =
              expression_of_polynomial state expression.span polynomial
            in
            match
              replacement_if_changed config expression
                candidate.expression_desc
            with
            | None -> None
            | Some replacement ->
                [%log.debug "normalized bounded mathematical integer ring"
                  ~stage:(Delator.Field.string "mathematical-int-rewrite")
                  ~input_degree:(Delator.Field.int input.max_degree)
                  ~output_degree:(Delator.Field.int output.max_degree)
                  ~input_arithmetic_node_count:
                    (Delator.Field.int input.arithmetic_nodes)
                  ~output_arithmetic_node_count:
                    (Delator.Field.int output.arithmetic_nodes)
                  ~visited_input_node_count:
                    (Delator.Field.int state.input_nodes)
                  ~certification_inspection_count:
                    (Delator.Field.int
                       (config.max_ring_inspections
                       - !(state.inspection_fuel)))
                  ~estimated_output_node_count:
                    (Delator.Field.int (emitted_node_count output))
                  ~atom_count:(Delator.Field.int state.atom_count)
                  ~monomial_count:
                    (Delator.Field.int (Polynomial.cardinal polynomial))
                  ~collection_event_count:
                    (Delator.Field.int state.collection_events)
                  ~cancellation_event_count:
                    (Delator.Field.int state.cancellation_events)
                  ~rewrite_class:(Delator.Field.string "bounded-ring")
                  ~decision:(Delator.Field.string "rewritten")];
                Some replacement
        with Abort (reason [@log_value.debug]) ->
          [%log.debug "bounded ring analysis preserved the original expression"
            ~stage:(Delator.Field.string "mathematical-int-rewrite")
            ~reason:
              (Delator.Field.string
                 (match (reason [@log_value.debug]) with
                 | Missing_atom -> "missing-atom"
                 | Inadmissible_atom -> "inadmissible-atom"
                 | Atom_budget -> "atom-budget"
                 | Coefficient_budget -> "coefficient-budget"
                 | Monomial_budget -> "monomial-budget"
                 | Coefficient_product_budget -> "coefficient-product-budget"
                 | Product_budget -> "product-budget"
                 | Degree_budget -> "degree-budget"
                 | Input_budget -> "input-budget"
                 | Sort_mismatch -> "sort-mismatch"
                 | Invalid_arity -> "invalid-arity"
                 | Empty_monomial -> "empty-monomial"))
            ~rewrite_class:(Delator.Field.string "bounded-ring")
            ~decision:(Delator.Field.string "preserved-original")];
          None)
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
        | Sst.Parameter _ | Sst.Application _ ),
        _ )
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        Sst.Mathematical_int,
        ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
        | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
        | Sst.Match _ | Sst.Lift_runtime_int _ | Sst.Compare _
        | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Forall _
        | Sst.Exists _ | Sst.Direct_call _ | Sst.Symbolic_application _
        | Sst.Callback_call _ | Sst.Callback_requires _
        | Sst.Callback_ensures _ | Sst.Optional_absent
        | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
        | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _ ) ) ->
        None

  let comparison_result comparison left right =
    let ordering = Z.compare left right in
    match comparison with
    | Sst.Equal -> ordering = 0
    | Sst.Not_equal -> ordering <> 0
    | Sst.Less_than -> ordering < 0
    | Sst.Less_or_equal -> ordering <= 0
    | Sst.Greater_than -> ordering > 0
    | Sst.Greater_or_equal -> ordering >= 0

  let polynomial_gcd polynomial =
    Polynomial.fold
      (fun _ coefficient divisor -> Z.gcd divisor (Z.abs coefficient))
      polynomial Z.zero

  let divide_polynomial state divisor polynomial =
    if is_zero divisor || is_one divisor then polynomial
    else
      Polynomial.fold
        (fun monomial coefficient result ->
          set_coefficient state monomial (Z.div coefficient divisor) result)
        polynomial Polynomial.empty

  let leading_coefficient polynomial =
    let bindings = List.rev (Polynomial.bindings polynomial) in
    let rec non_constant = function
      | [] -> None
      | ([], _) :: remaining -> non_constant remaining
      | (_, coefficient) :: _ -> Some coefficient
    in
    match non_constant bindings with
    | Some coefficient -> Some coefficient
    | None -> (
        match bindings with
        | [] -> None
        | (_, coefficient) :: _ -> Some coefficient)

  let reverse_comparison = function
    | Sst.Equal -> Sst.Equal
    | Sst.Not_equal -> Sst.Not_equal
    | Sst.Less_than -> Sst.Greater_than
    | Sst.Less_or_equal -> Sst.Greater_or_equal
    | Sst.Greater_than -> Sst.Less_than
    | Sst.Greater_or_equal -> Sst.Less_or_equal

  let primitive_relation state comparison polynomial =
    let polynomial =
      divide_polynomial state (polynomial_gcd polynomial) polynomial
    in
    match (state.config.ring_mode, leading_coefficient polynomial) with
    | Ring_canonicalize_bounded, Some coefficient
      when Z.sign coefficient < 0 ->
        (reverse_comparison comparison, scale state minus_one polynomial)
    | (Ring_disabled | Ring_reduce_only | Ring_canonicalize_bounded), _ ->
        (comparison, polynomial)

  let normalize_comparison config expression =
    match
      ( config.ring_mode,
        config.normalize_comparisons,
        expression.Sst.typ,
        expression.expression_desc )
    with
    | _, false, _, _ | Ring_disabled, _, _, _ -> None
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        true,
        Sst.Bool,
        Sst.Compare
          ( comparison,
            ({ Sst.typ = Sst.Mathematical_int; _ } as left_expression),
            ({ Sst.typ = Sst.Mathematical_int; _ } as right_expression) ) ) -> (
        try
          let state = create_state config in
          let left = of_expression state left_expression in
          let right = of_expression state right_expression in
          let difference = subtract state left right in
          let comparison, difference =
            primitive_relation state comparison difference
          in
          if not (heavy_atom_occurrences_are_safe state difference) then (
            [%log.debug "preserved comparison carrying verifier obligations"
              ~stage:(Delator.Field.string "mathematical-int-rewrite")
              ~atom_count:(Delator.Field.int state.atom_count)
              ~rewrite_class:
                (Delator.Field.string "bounded-ring-comparison")
              ~decision:(Delator.Field.string "preserved-original")];
            None)
          else
            match Polynomial.bindings difference with
            | [] ->
                Some
                  (Sst.Bool_constant
                     (comparison_result comparison Z.zero Z.zero))
            | [ ([], constant) ] ->
                Some
                  (Sst.Bool_constant
                     (comparison_result comparison constant Z.zero))
            | _ ->
              let input =
                combine_measure (expression_measure left_expression)
                  (expression_measure right_expression)
              in
              let output = polynomial_measure difference in
              let cancelled_terms = state.cancellation_events > 0 in
              if
                not (should_emit config ~cancelled_terms input output)
              then None
              else
                let normalized_left =
                  expression_of_polynomial state expression.span difference
                in
                let zero = constant_expression expression.span Z.zero in
                (match
                   replacement_if_changed config expression
                     (Sst.Compare (comparison, normalized_left, zero))
                 with
                | None -> None
                | Some replacement ->
                    [%log.debug
                      "normalized bounded mathematical integer comparison"
                      ~stage:
                        (Delator.Field.string "mathematical-int-rewrite")
                      ~input_degree:(Delator.Field.int input.max_degree)
                      ~output_degree:(Delator.Field.int output.max_degree)
                      ~visited_input_node_count:
                        (Delator.Field.int state.input_nodes)
                      ~certification_inspection_count:
                        (Delator.Field.int
                           (config.max_ring_inspections
                           - !(state.inspection_fuel)))
                      ~estimated_output_node_count:
                        (Delator.Field.int (emitted_node_count output))
                      ~atom_count:(Delator.Field.int state.atom_count)
                      ~monomial_count:
                        (Delator.Field.int (Polynomial.cardinal difference))
                      ~collection_event_count:
                        (Delator.Field.int state.collection_events)
                      ~cancellation_event_count:
                        (Delator.Field.int state.cancellation_events)
                      ~rewrite_class:
                        (Delator.Field.string "bounded-ring-comparison")
                      ~decision:(Delator.Field.string "rewritten")];
                    Some replacement)
        with Abort (reason [@log_value.debug]) ->
          [%log.debug
            "bounded comparison analysis preserved the original expression"
            ~stage:(Delator.Field.string "mathematical-int-rewrite")
            ~reason:
              (Delator.Field.string
                 (match (reason [@log_value.debug]) with
                 | Missing_atom -> "missing-atom"
                 | Inadmissible_atom -> "inadmissible-atom"
                 | Atom_budget -> "atom-budget"
                 | Coefficient_budget -> "coefficient-budget"
                 | Monomial_budget -> "monomial-budget"
                 | Coefficient_product_budget -> "coefficient-product-budget"
                 | Product_budget -> "product-budget"
                 | Degree_budget -> "degree-budget"
                 | Input_budget -> "input-budget"
                 | Sort_mismatch -> "sort-mismatch"
                 | Invalid_arity -> "invalid-arity"
                 | Empty_monomial -> "empty-monomial"))
            ~rewrite_class:(Delator.Field.string "bounded-ring-comparison")
            ~decision:(Delator.Field.string "preserved-original")];
          None)
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        true,
        ( Sst.Unit | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
        | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ),
        _ )
    | ( (Ring_reduce_only | Ring_canonicalize_bounded),
        true,
        Sst.Bool,
        ( Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
        | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
        | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
        | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
        | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
        | Sst.Match _ | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _
        | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
        | Sst.Forall _ | Sst.Exists _ | Sst.Direct_call _
        | Sst.Symbolic_application _ | Sst.Callback_call _
        | Sst.Callback_requires _ | Sst.Callback_ensures _
        | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _
        | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _ ) ) ->
        None
end

let normalize_ring_arithmetic =
  Ring.normalize_arithmetic default_config

let fold_constant_comparison_with _config (expression [@delator.skip]) =
  match (expression.Sst.typ, expression.expression_desc) with
  | ( Sst.Bool,
      Sst.Compare
        ( comparison,
          ({ Sst.typ = Sst.Mathematical_int; _ } as left),
          ({ Sst.typ = Sst.Mathematical_int; _ } as right) ) ) ->
      Option.bind (immediate_constant_value left) (fun left ->
          Option.map
            (fun right ->
              Sst.Bool_constant (Ring.comparison_result comparison left right))
            (immediate_constant_value right))
  | ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
    | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ), _ ->
      None

let fold_constant_comparison =
  fold_constant_comparison_with default_config

let normalize_mathematical_comparison =
  Ring.normalize_comparison default_config

let rules config =
  validate_config config;
  let ring_rules =
    match config.ring_mode with
    | Ring_disabled -> []
    | Ring_reduce_only | Ring_canonicalize_bounded ->
        [ Ring.normalize_arithmetic config ]
  in
  let comparison_rules =
    if config.normalize_comparisons then
      [ Ring.normalize_comparison config ]
    else []
  in
  [
    fold_bottom_up_constant_arithmetic_with config;
    normalize_constant_multiplication;
    simplify_arithmetic_identities_with config;
  ]
  @ ring_rules
  @ [ fold_constant_comparison_with config ]
  @ comparison_rules

let default_rules = rules default_config

let simplification_phase config : Sst_expression_rewrite_private.phase =
  {
    Sst_expression_rewrite_private.direction = Rewrite_private.Bottom_up;
    repetition =
      Rewrite_private.Until_stable
        { max_passes = config.max_fixpoint_passes };
    max_nodes = Some config.max_phase_nodes;
    max_rewrites = config.max_rewrites;
    rules = rules config;
    rewrite_quantifier_triggers = config.rewrite_quantifier_triggers;
  }

let simplify_with_stats config expression =
  [%log.debug "starting mathematical integer simplification"
    ~stage:(Delator.Field.string "mathematical-int-rewrite")
    ~ring_mode:
      (Delator.Field.string
         (match config.ring_mode with
         | Ring_disabled -> "disabled"
         | Ring_reduce_only -> "reduce-only"
         | Ring_canonicalize_bounded -> "canonicalize-bounded"))
    ~application_policy:
      (Delator.Field.string
         (match config.application_policy with
         | Preserve_applications -> "preserve"
         | Treat_pure_specifications_as_atoms -> "pure-specification-atoms"))
    ~phase_node_budget:(Delator.Field.int config.max_phase_nodes)
    ~fixpoint_pass_budget:(Delator.Field.int config.max_fixpoint_passes)
    ~trigger_rewriting:(Delator.Field.bool config.rewrite_quantifier_triggers)
    ~decision:(Delator.Field.string "started")];
  Sst_expression_rewrite_private.apply_phase_with_stats
    (simplification_phase config) expression
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let simplify_with config expression =
  fst (simplify_with_stats config expression)

let simplify expression = simplify_with default_config expression
