open Logical_spec_capability_private

type builder = {
  validated : Sst_validation.validated_program;
  type_definitions : Sst.type_definition list;
  classify : Sst.function_id -> call_target;
  definitions : (int * Sst.function_definition) list ref;
  callable_descriptors : Sst_validation.callable_descriptor list ref;
  visiting : Sst.function_id list ref;
  captured_models : model_capability list ref;
  mutable_model_fields : (Sst.function_id * Sst.field_id) list ref;
  aggregate_descriptors : Sst.type_definition list ref;
  option_descriptors : Sst.typ list ref;
  field_descriptors : Sst.field_definition list ref;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let rec all_ok f = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = f value in
      all_ok f rest
let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name
let same_field_id (left : Sst.field_id) (right : Sst.field_id) =
  left.field_index = right.field_index
  && String.equal left.field_name right.field_name
  &&
  match (left.field_owner, right.field_owner) with
  | Sst.Record_owner left, Sst.Record_owner right -> same_type_id left right
  | Sst.Constructor_owner left, Sst.Constructor_owner right ->
      left.constructor_index = right.constructor_index
      && String.equal left.constructor_name right.constructor_name
      && same_type_id left.constructor_type right.constructor_type
  | Sst.Record_owner _, Sst.Constructor_owner _
  | Sst.Constructor_owner _, Sst.Record_owner _ ->
      false
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
let rec supported_conditional_type = function
  | Sst.Unit | Sst.Bool | Sst.Bit_vector _ | Sst.Aggregate _ | Sst.Parameter _
  | Sst.Application _ ->
      true
  | Sst.Tuple components ->
      List.for_all (fun (_, typ) -> supported_conditional_type typ) components
  | Sst.Int | Sst.Mathematical_int -> false

let fields_of_type_definitions type_definitions =
  List.concat_map
    (fun (definition : Sst.type_definition) ->
      match definition.type_kind with
      | Sst.Record_definition fields -> fields
      | Sst.Variant_definition constructors ->
          List.concat_map
            (fun (constructor : Sst.constructor_definition) ->
              constructor.constructor_fields)
            constructors)
    type_definitions

let find_field type_definitions field =
  fields_of_type_definitions type_definitions
  |> List.find_opt (fun (candidate : Sst.field_definition) ->
         same_field_id candidate.field_id field)

let find_type type_definitions type_id =
  List.find_opt
    (fun (definition : Sst.type_definition) ->
      same_type_id definition.type_id type_id)
    type_definitions

let owner_type = function
  | Sst.Record_owner type_id -> type_id
  | Sst.Constructor_owner constructor -> constructor.constructor_type

let rec aggregate_type_ids = function
  | Sst.Aggregate type_id -> [ type_id ]
  | Sst.Tuple components ->
      List.concat_map (fun (_, typ) -> aggregate_type_ids typ) components
  | Sst.Application (_, arguments) -> List.concat_map aggregate_type_ids arguments
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _
  | Sst.Parameter _ ->
      []

let representation_type_reachable type_definitions ~root ~target =
  let rec visit visited type_id =
    if List.exists (same_type_id type_id) visited then false
    else if same_type_id type_id target then true
    else
      match find_type type_definitions type_id with
      | None -> false
      | Some definition ->
          let fields =
            match definition.Sst.type_kind with
            | Sst.Record_definition fields -> fields
            | Sst.Variant_definition constructors ->
                List.concat_map
                  (fun (constructor : Sst.constructor_definition) ->
                    constructor.constructor_fields)
                  constructors
          in
          fields
          |> List.concat_map (fun (field : Sst.field_definition) ->
                 aggregate_type_ids field.field_type)
          |> List.exists (visit (type_id :: visited))
  in
  visit [] root

let rec irrefutable_pattern (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Wildcard | Sst.Bind _ | Sst.Unit_pattern -> true
  | Sst.Tuple_pattern components ->
      List.for_all (fun (_, nested) -> irrefutable_pattern nested) components
  | Sst.Record_pattern fields ->
      List.for_all (fun (_, nested) -> irrefutable_pattern nested) fields
  | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Constructor_pattern _
  | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
      false

let total_match type_definitions scrutinee cases =
  let unguarded case = Option.is_none case.Sst.case_guard in
  if
    List.exists
      (fun (case : Sst.case) ->
        unguarded case && irrefutable_pattern case.case_pattern)
      cases
  then true
  else
    match scrutinee.Sst.typ with
    | Sst.Bool ->
        List.for_all
          (fun expected ->
            List.exists
              (fun (case : Sst.case) ->
                unguarded case
                && case.case_pattern.pattern_desc = Sst.Bool_pattern expected)
              cases)
          [ false; true ]
    | Sst.Aggregate type_id -> (
        match find_type type_definitions type_id with
        | Some { Sst.type_kind = Sst.Record_definition _; _ } ->
            List.exists
              (fun (case : Sst.case) ->
                unguarded case
                &&
                match case.case_pattern.pattern_desc with
                | Sst.Record_pattern fields ->
                    List.for_all
                      (fun (_, nested) -> irrefutable_pattern nested)
                      fields
                | _ -> false)
              cases
        | Some { Sst.type_kind = Sst.Variant_definition constructors; _ } ->
            List.for_all
              (fun (constructor : Sst.constructor_definition) ->
                List.exists
                  (fun (case : Sst.case) ->
                    unguarded case
                    &&
                    match case.case_pattern.pattern_desc with
                    | Sst.Constructor_pattern (candidate, arguments) ->
                        candidate.constructor_index
                        = constructor.constructor_id.constructor_index
                        && String.equal candidate.constructor_name
                             constructor.constructor_id.constructor_name
                        && same_type_id candidate.constructor_type
                             constructor.constructor_id.constructor_type
                        && List.for_all irrefutable_pattern arguments
                    | _ -> false)
                  cases)
              constructors
        | None -> false)
    | Sst.Unit ->
        List.exists
          (fun (case : Sst.case) ->
            unguarded case
            && case.case_pattern.pattern_desc = Sst.Unit_pattern)
          cases
    | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ | Sst.Tuple _
    | Sst.Parameter _
    | Sst.Application _ ->
        false

let add_definition builder definition =
  let id = definition.Sst.function_id in
  match Sst_validation.find_callable builder.validated id with
  | Some descriptor
    when Sst_validation.callable_definition descriptor == definition ->
      if
        not
          (List.exists
             (fun (_, candidate) ->
               same_function_id id candidate.Sst.function_id)
             !(builder.definitions))
      then (
        builder.definitions :=
          (id.function_index, definition) :: !(builder.definitions);
        builder.callable_descriptors :=
          descriptor :: !(builder.callable_descriptors));
      Ok ()
  | Some _ | None -> Error "stale-callable-descriptor"

let add_aggregate_descriptor builder type_id =
  match find_type builder.type_definitions type_id with
  | None -> Error "missing-aggregate-descriptor"
  | Some descriptor -> (
      match Sst_validation.find_type builder.validated type_id with
      | Some validated_descriptor
        when Sst_validation.type_definition validated_descriptor == descriptor ->
          if
            not
              (List.exists
                 (fun candidate ->
                   same_type_id candidate.Sst.type_id descriptor.type_id)
                 !(builder.aggregate_descriptors))
          then
            builder.aggregate_descriptors :=
              descriptor :: !(builder.aggregate_descriptors);
          Ok ()
      | Some _ | None -> Error "stale-aggregate-descriptor")

let add_field_descriptor builder field =
  match find_field builder.type_definitions field with
  | None -> Error "missing-field-descriptor"
  | Some descriptor ->
      if
        not
          (List.exists
             (fun candidate -> same_field_id candidate.Sst.field_id field)
             !(builder.field_descriptors))
      then
        builder.field_descriptors := descriptor :: !(builder.field_descriptors);
      let* () = add_aggregate_descriptor builder (owner_type field.field_owner) in
      Ok descriptor

let add_option_descriptor builder typ =
  let program = Sst_validation.program builder.validated in
  match Parametric_adt.option_instance program.parametric_adts typ with
  | None -> Error "missing-option-descriptor"
  | Some _ ->
      if not (List.mem typ !(builder.option_descriptors)) then
        builder.option_descriptors := typ :: !(builder.option_descriptors);
      Ok ()

let add_model builder capability =
  let id = model_callable capability in
  if
    not
      (List.exists
         (fun candidate -> same_function_id (model_callable candidate) id)
         !(builder.captured_models))
  then builder.captured_models := capability :: !(builder.captured_models)

let rec eligible_expression builder current_model (expression : Sst.expression) =
  let recurse = eligible_expression builder current_model in
  match expression.expression_desc with
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Bv_literal _
  | Sst.Variable _ | Sst.Logical_constant_reference _ ->
      Ok ()
  | Sst.Lift_runtime_int operand -> recurse operand
  | Sst.Bv_int_to_bv_mod { input; _ }
  | Sst.Bv_to_int_unsigned input
  | Sst.Bv_to_int_signed input
  | Sst.Bv_not input ->
      recurse input
  | Sst.Bv_binary (_, left, right) | Sst.Bv_compare (_, left, right) ->
      let* () = recurse left in
      recurse right
  | Sst.Optional_present payload | Sst.Optional_forward payload ->
      let* () = add_option_descriptor builder expression.typ in
      recurse payload
  | Sst.Optional_absent -> add_option_descriptor builder expression.typ
  | Sst.Tuple_value components -> all_ok recurse (List.map snd components)
  | Sst.Record_value _ | Sst.Constructor_value _ ->
      Error "aggregate-construction-requires-general-evaluation"
  | Sst.Field_read { record; field } ->
      let* () = recurse record in
      let* descriptor = add_field_descriptor builder field in
      if descriptor.field_type <> expression.typ then
        Error "field-result-type-mismatch"
      else field_read_profile builder current_model field descriptor
  | Sst.Let (bindings, body) ->
      let* () =
        all_ok
          (fun (pattern, value) ->
            if supported_binding_pattern pattern then recurse value
            else Error "unsupported-let-pattern")
          bindings
      in
      recurse body
  | Sst.Sequence (first, second) ->
      if first.typ <> Sst.Unit then Error "non-unit-sequence"
      else
        let* () = recurse first in
        recurse second
  | Sst.If (condition, consequent, Some alternative) ->
      if not (supported_conditional_type expression.typ) then
        Error "unsupported-conditional-result"
      else
        let* () = recurse condition in
        let* () = recurse consequent in
        recurse alternative
  | Sst.Match (scrutinee, cases) ->
      match_profile builder current_model expression scrutinee cases
  | Sst.Checked_arithmetic (_, arguments) -> all_ok recurse arguments
  | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
      let* () = recurse left in
      recurse right
  | Sst.Boolean_not operand -> recurse operand
  | Sst.Direct_call _
    when Spec_function_sst_private.lambda expression <> None
         || Spec_function_sst_private.application expression <> None
         || Parametric_type.is_spec_function expression.typ ->
      Error "lambda-or-higher-order-application"
  | Sst.Direct_call
      { call_form = Sst.Specification_call; callee; arguments; _ } ->
      let* () =
        all_ok
          (function
            | Sst.Value_argument { value; _ } -> recurse value
            | Sst.Callback_argument _ -> Error "callback-argument")
          arguments
      in
      definition_graph builder callee
  | Sst.If (_, _, None) -> Error "partial-if"
  | Sst.Forall _ | Sst.Exists _ -> Error "quantifier-or-trigger"
  | Sst.Symbolic_application _ -> Error "symbolic-application"
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
      Error "callback"
  | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ ->
      Error "write-or-rebase"
  | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _ ->
      Error "mutable-operation"
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ -> Error "reveal-or-fuel"
  | Sst.Use_type_invariant _ -> Error "invariant-use"
  | Sst.Local_assert _ -> Error "assertion"
  | Sst.Proof_region _ -> Error "proof-region"
  | Sst.Old _ -> Error "old"
  | Sst.Direct_call _ -> Error "exec-proof-or-higher-order-call"

and field_read_profile builder current_model field descriptor =
  match descriptor.Sst.field_mutability with
  | Sst.Immutable_field -> Ok ()
  | Sst.Mutable_field -> (
      match current_model with
      | None -> Error "mutable-field-read-outside-captured-model"
      | Some capability ->
          if
            representation_type_reachable builder.type_definitions
              ~root:(model_representation_root capability)
              ~target:(owner_type field.Sst.field_owner)
          then (
            builder.mutable_model_fields :=
              (model_callable capability, field) :: !(builder.mutable_model_fields);
            Ok ())
          else Error "mutable-field-outside-model-representation")

and match_profile builder current_model expression scrutinee cases =
  let recurse = eligible_expression builder current_model in
  if cases = [] then Error "empty-match"
  else if not (supported_conditional_type expression.Sst.typ) then
    Error "unsupported-conditional-result"
  else if not (total_match builder.type_definitions scrutinee cases) then
    Error "non-total-match"
  else
    let* () =
      match scrutinee.Sst.typ with
      | Sst.Aggregate type_id -> add_aggregate_descriptor builder type_id
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
      | Sst.Bit_vector _ | Sst.Tuple _
      | Sst.Parameter _
      | Sst.Application _ ->
          Ok ()
    in
    let* () = recurse scrutinee in
    all_ok
      (fun (case : Sst.case) ->
        if not (supported_match_pattern case.case_pattern) then
          Error "unsupported-match-pattern"
        else
          let* () =
            match case.case_guard with None -> Ok () | Some guard -> recurse guard
          in
          recurse case.case_body)
      cases

and definition_graph builder callee =
  if List.exists (same_function_id callee) !(builder.visiting) then
    Error "recursive-or-cyclic-call"
  else
    match builder.classify callee with
    | Unsupported -> Error "unsupported-or-stale-callable"
    | Opaque_recursive -> Error "recursive-callable"
    | Local_nonrecursive definition ->
        authenticate_definition builder None definition
    | Captured_model capability ->
        let* () = validate_model_capture builder.validated capability in
        if
          (model_definition capability).result_type
          <> model_result_type capability
        then Error "stale-model-result-type"
        else authenticate_definition builder (Some capability)
            (model_definition capability)

and authenticate_definition builder model definition =
  let id = definition.Sst.function_id in
  if
    List.exists
      (fun (_, candidate) -> same_function_id id candidate.Sst.function_id)
      !(builder.definitions)
  then Ok ()
  else
    match (definition.mode, definition.recursive, definition.body) with
    | Sst.Spec, false,
      Sst.Spec_definition { stage = Sst.Logical; expression } ->
        if
          List.exists
            (function
              | Sst.Callback_parameter _ -> true
              | Sst.Value_parameter _ -> false)
            definition.parameters
        then Error "callback-parameter"
        else (
          builder.visiting := id :: !(builder.visiting);
          let result = eligible_expression builder model expression in
          builder.visiting :=
            List.filter
              (fun candidate -> not (same_function_id id candidate))
              !(builder.visiting);
          let* () = result in
          let* () = add_definition builder definition in
          Option.iter (add_model builder) model;
          Ok ())
    | Sst.Spec, false, Sst.Symbolic_declaration _ ->
        Error "symbolic-only-callable"
    | Sst.Spec, false,
      Sst.Spec_definition { stage = (Sst.Proof_stage | Sst.Runtime); _ } ->
        Error "non-logical-specification"
    | Sst.Spec, _, Sst.Recursive_spec_definition _ -> Error "recursive-callable"
    | (Sst.Exec | Sst.Proof), _, _ -> Error "exec-or-proof-callable"
    | Sst.Spec, _,
      ( Sst.Checked_exec _ | Sst.Proof_body _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _ ) ->
        Error "external-or-trusted-callable"
    | Sst.Spec, true, (Sst.Spec_definition _ | Sst.Symbolic_declaration _) ->
        Error "recursive-callable"

let admit ~validated ~type_definitions ~classify ~root_identity root =
  let builder =
    {
      validated;
      type_definitions;
      classify;
      definitions = ref [];
      callable_descriptors = ref [];
      visiting = ref [];
      captured_models = ref [];
      mutable_model_fields = ref [];
      aggregate_descriptors = ref [];
      option_descriptors = ref [];
      field_descriptors = ref [];
    }
  in
  let* () = eligible_expression builder None root in
  let model_capabilities =
    List.map
      (fun capability ->
        let model = model_callable capability in
        let fields =
          !(builder.mutable_model_fields)
          |> List.filter_map (fun (candidate, field) ->
                 if same_function_id candidate model then Some field else None)
          |> List.sort_uniq compare
        in
        (capability, fields))
      !(builder.captured_models)
  in
  Ok
    (create_permit ~validated ~root_identity ~root
       ~definitions:!(builder.definitions)
       ~callable_descriptors:!(builder.callable_descriptors)
       ~aggregate_descriptors:!(builder.aggregate_descriptors)
       ~option_descriptors:!(builder.option_descriptors)
       ~field_descriptors:!(builder.field_descriptors) ~model_capabilities)

type disposition = Eligible of permit | Abstain of string

type registry_entry = {
  root : Sst.expression;
  identity : root_identity;
  disposition : disposition;
}

let contract_identity definition clause_kind ordinal =
  contract_root ~callable:definition.Sst.function_id ~clause_kind ~ordinal

let model_permit entries =
  List.find_map
    (fun entry ->
      match entry.disposition with
      | Eligible permit
        when Logical_spec_capability_private.For_testing.has_model_capability
               permit ->
          Some permit
      | Eligible _ | Abstain _ -> None)
    entries

let run_capability_controls entries validated =
  match Sys.getenv_opt "VEROCAML_TEST_FORMULA_CAPABILITY_CONTROLS" with
  | Some "1" ->
      model_permit entries
      |> Option.iter (fun permit ->
             Logical_spec_capability_private.For_testing
             .capability_negative_controls permit validated
             |> ignore)
  | Some _ | None -> ()

let run_validator_controls entries validated =
  if Sys.getenv_opt "VEROCAML_TEST_FORMULA_VALIDATOR_MATRIX" = Some "1" then
    match model_permit entries with
    | None -> ()
    | Some permit ->
        Logical_spec_capability_private.For_testing.stale_model_control permit
          validated
        |> ignore

let prepare ~validated ~type_definitions ~classify ~excluded_contract
    ~authority_snapshot ~invariant_roots =
  Logical_spec_capability_private.For_testing.reset_formula_observations ();
  let authenticate identity root =
    match admit ~validated ~type_definitions ~classify ~root_identity:identity root with
    | Ok permit -> Eligible permit
    | Error reason -> Abstain reason
  in
  let entries = ref [] in
  let add identity root reason =
    Logical_spec_capability_private.For_testing.before_authentication identity
      ~authority_snapshot;
    let disposition =
      match reason with Some reason -> Abstain reason | None -> authenticate identity root
    in
    entries := { root; identity; disposition } :: !entries
  in
  List.iter
    (fun (identity, root, reason) -> add identity root reason)
    invariant_roots;
  Sst_validation.callable_descriptors validated
  |> List.iter (fun descriptor ->
         let definition = Sst_validation.callable_definition descriptor in
         let reason =
           if excluded_contract definition then Some "external-or-trusted-root"
           else None
         in
         let contract = Sst_validation.callable_contract descriptor in
         let add_clauses clause_kind clauses =
           List.iter
             (fun clause ->
               let root = Sst_validation.contract_clause_expression clause in
               let identity =
                 contract_root ~callable:definition.Sst.function_id ~clause_kind
                   ~ordinal:(Sst_validation.contract_clause_index clause)
               in
               add identity root reason)
             clauses
         in
         add_clauses Requires (Sst_validation.contract_requires contract);
         add_clauses Ensures (Sst_validation.contract_ensures contract));
  let entries = List.rev !entries in
  List.iter
    (fun entry ->
      Logical_spec_capability_private.For_testing.trace_profile entry.identity
        (match entry.disposition with
        | Eligible _ -> "permit"
        | Abstain reason -> "abstain:" ^ reason))
    entries;
  run_capability_controls entries validated;
  run_validator_controls entries validated;
  entries

module For_testing = struct
  let[@log_value.info] tag (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Int_constant _ -> "int"
    | Sst.Lift_runtime_int _ -> "lift-runtime-int"
    | Sst.Bool_constant _ -> "bool"
    | Sst.Unit_constant -> "unit"
    | Sst.Bv_literal _ -> "bv-literal"
    | Sst.Bv_int_to_bv_mod _ -> "bv-int-to-mod"
    | Sst.Bv_to_int_unsigned _ -> "bv-to-unsigned"
    | Sst.Bv_to_int_signed _ -> "bv-to-signed"
    | Sst.Bv_not _ -> "bv-not"
    | Sst.Bv_binary (operation, _, _) ->
        "bv-" ^ Bv_operation_private.binary_name operation
    | Sst.Bv_compare (comparison, _, _) ->
        "bv-" ^ Bv_operation_private.comparison_name comparison
    | Sst.Variable _ -> "variable"
    | Sst.Tuple_value _ -> "tuple"
    | Sst.Record_value _ -> "record"
    | Sst.Constructor_value _ -> "constructor"
    | Sst.Field_read _ -> "field-read"
    | Sst.Field_write _ -> "field-write"
    | Sst.Shared_scalar_field_write _ -> "shared-scalar-write"
    | Sst.Owned_tree_nested_write _ -> "owned-tree-write"
    | Sst.Owned_tree_rebase _ -> "owned-tree-rebase"
    | Sst.Let_mutable _ -> "let-mutable"
    | Sst.Mutable_read _ -> "mutable-read"
    | Sst.Mutable_write _ -> "mutable-write"
    | Sst.Let _ -> "let"
    | Sst.Sequence _ -> "sequence"
    | Sst.If _ -> "if"
    | Sst.Match _ -> "match"
    | Sst.Checked_arithmetic _ -> "arithmetic"
    | Sst.Compare _ -> "compare"
    | Sst.Boolean_not _ -> "not"
    | Sst.Boolean_binary _ -> "boolean"
    | Sst.Forall _ -> "forall"
    | Sst.Exists _ -> "exists"
    | Sst.Direct_call _ -> "direct-call"
    | Sst.Symbolic_application _ -> "symbolic-application"
    | Sst.Logical_constant_reference _ -> "logical-constant-reference"
    | Sst.Callback_call _ -> "callback-call"
    | Sst.Callback_requires _ -> "callback-requires"
    | Sst.Callback_ensures _ -> "callback-ensures"
    | Sst.Optional_absent -> "optional-absent"
    | Sst.Optional_present _ -> "optional-present"
    | Sst.Optional_forward _ -> "optional-forward"
    | Sst.Reveal _ -> "reveal"
    | Sst.Reveal_with_fuel _ -> "reveal-with-fuel"
    | Sst.Use_type_invariant _ -> "invariant-use"
    | Sst.Local_assert _ -> "local-assert"
    | Sst.Proof_region _ -> "proof-region"
    | Sst.Old _ -> "old"

  let body = function
    | Sst.Checked_exec { body; _ } | Sst.Proof_body { body; _ }
    | Sst.Recursive_spec_definition { body; _ }
    | Sst.Spec_definition body ->
        Some body.expression
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        None
  let observe_program_candidates ~validated ~type_definitions ~classify =
    if Sys.getenv_opt "VEROCAML_TEST_FORMULA_VALIDATOR_MATRIX" = Some "1" then
      let descriptors = Sst_validation.callable_descriptors validated in
      let symbolic =
        List.find_map
          (fun descriptor ->
            let definition = Sst_validation.callable_definition descriptor in
            match definition.Sst.body with
            | Sst.Symbolic_declaration _ -> Some definition
            | _ -> None)
          descriptors
      in
      descriptors
      |> List.iter (fun descriptor ->
             let definition = Sst_validation.callable_definition descriptor in
             let identity =
               contract_root ~callable:definition.function_id ~clause_kind:Ensures
                 ~ordinal:(-1)
             in
             let report (candidate [@log_value.info]) ~type_definitions ~classify
                 expression =
               match
                 admit ~validated ~type_definitions ~classify
                   ~root_identity:identity expression
               with
               | Ok _ -> ()
               | Error (reason [@log_value.info]) ->
                   [%log.info "formula validator rejection"
                     ~function_name:
                       (Delator.Field.string
                          definition.function_id.function_name)
                     ~function_index:
                       (Delator.Field.int definition.function_id.function_index)
                     ~expression_tag:
                       (Delator.Field.string
                          ((tag [@log_value.info]) expression))
                     ~candidate:
                       (Delator.Field.string (candidate [@log_value.info]))
                     ~reason:(Delator.Field.string (reason [@log_value.info]))
                     ~span_file:(Delator.Field.string expression.span.file)
                     ~span_start_line:
                       (Delator.Field.int expression.span.start_pos.line)
                     ~span_start_column:
                       (Delator.Field.int expression.span.start_pos.column)
                     ~span_end_line:
                       (Delator.Field.int expression.span.end_pos.line)
                     ~span_end_column:
                       (Delator.Field.int expression.span.end_pos.column)]
             in
             let rec visit expression =
               report ("source" [@log_value.info]) ~type_definitions ~classify expression;
               (match expression.Sst.expression_desc with
               | Sst.Direct_call call ->
                   let specification =
                     { expression with expression_desc = Sst.Direct_call { call with call_form = Sst.Specification_call } }
                   in
                   report ("specification-call" [@log_value.info]) ~type_definitions ~classify specification;
                   let unsupported candidate =
                     if same_function_id candidate call.callee then Unsupported
                     else classify candidate
                   in
                   report ("unsupported-callable" [@log_value.info]) ~type_definitions
                     ~classify:unsupported specification;
                   (match Sst_validation.find_callable validated call.callee with
                   | Some callable_descriptor ->
                       let callable = Sst_validation.callable_definition callable_descriptor in
                       let descriptor candidate =
                         if same_function_id candidate call.callee then Local_nonrecursive callable
                         else classify candidate
                       in
                       let stale candidate =
                         if same_function_id candidate call.callee then Local_nonrecursive { callable with span = callable.span }
                         else classify candidate
                       in
                       report ("descriptor-call" [@log_value.info]) ~type_definitions ~classify:descriptor specification;
                       report ("stale-callable" [@log_value.info]) ~type_definitions ~classify:stale specification;
                       if
                         List.exists
                           (function Sst.Callback_parameter _ -> true | Sst.Value_parameter _ -> false)
                           callable.parameters
                       then
                         let value_arguments =
                           List.filter
                             (function Sst.Value_argument _ -> true | Sst.Callback_argument _ -> false)
                             call.arguments
                         in
                         report ("callback-parameter" [@log_value.info]) ~type_definitions ~classify:descriptor
                           { specification with expression_desc = Sst.Direct_call { call with call_form = Sst.Specification_call; arguments = value_arguments } }
                   | None -> ())
               | Sst.Record_value _ ->
                   report ("missing-aggregate" [@log_value.info]) ~type_definitions:[] ~classify expression
               | Sst.Field_read _ ->
                   report ("missing-field" [@log_value.info]) ~type_definitions:[] ~classify expression
               | Sst.Optional_absent | Sst.Optional_present _
               | Sst.Optional_forward _ ->
                   report ("missing-option" [@log_value.info]) ~type_definitions ~classify
                     { expression with typ = Sst.Int }
               | Sst.Variable { binding; _ } ->
                   report ("mutable-read" [@log_value.info]) ~type_definitions ~classify
                     { expression with expression_desc = Sst.Mutable_read binding };
                   report ("assertion" [@log_value.info]) ~type_definitions ~classify
                     { expression with expression_desc = Sst.Local_assert { assertion_ordinal = -1; predicate = expression } };
                   report ("proof-region" [@log_value.info]) ~type_definitions ~classify
                     { expression with expression_desc = Sst.Proof_region expression };
                   report ("invariant-use" [@log_value.info]) ~type_definitions ~classify
                     { expression with expression_desc = Sst.Use_type_invariant { use_id = "validator-candidate"; value = expression } };
                   if expression.typ <> Sst.Unit then
                     report ("non-unit-sequence" [@log_value.info]) ~type_definitions ~classify
                       { expression with expression_desc = Sst.Sequence (expression, expression) }
               | Sst.Match (scrutinee, cases) ->
                   report ("empty-match" [@log_value.info]) ~type_definitions ~classify
                     { expression with expression_desc = Sst.Match (scrutinee, []) };
                   Sst_callback_private.expression_children scrutinee
                   |> List.find_opt (fun child ->
                          match child.Sst.expression_desc with
                          | Sst.Variable _ -> true
                          | _ -> false)
                   |> Option.iter (fun variable ->
                          report ("owned-cursor-pattern" [@log_value.info]) ~type_definitions ~classify
                            { expression with expression_desc = Sst.Match (variable, cases) })
               | Sst.Callback_call application ->
                   let candidate callee =
                     {
                       expression with
                       expression_desc =
                         Sst.Direct_call
                           {
                             call_form = Sst.Specification_call;
                             callee;
                             type_arguments = [];
                             arguments = List.map (fun (label, value) -> Sst.Value_argument { label; value }) application.arguments;
                             recursive = false;
                           };
                     }
                   in
                   let callback_candidate = candidate definition.function_id in
                   let callback_definition =
                     { definition with mode = Sst.Spec; body = Sst.Spec_definition { stage = Sst.Logical; expression } }
                   in
                   let callback_classifier candidate =
                     if same_function_id candidate definition.function_id then Local_nonrecursive callback_definition
                     else classify candidate
                   in
                   report ("callback-parameter" [@log_value.info]) ~type_definitions
                     ~classify:callback_classifier callback_candidate;
                   Option.iter
                     (fun (symbolic : Sst.function_definition) ->
                       let symbolic_classifier candidate =
                         if same_function_id candidate symbolic.function_id then Local_nonrecursive symbolic
                         else classify candidate
                       in
                       report ("symbolic-callable" [@log_value.info]) ~type_definitions
                         ~classify:symbolic_classifier
                         (candidate symbolic.function_id))
                     symbolic
               | _ -> ());
               Sst_callback_private.expression_children expression
               |> List.iter visit
             in
             Option.iter visit (body definition.body);
             let contract = Sst_validation.callable_contract descriptor in
             Sst_validation.contract_requires contract
             @ Sst_validation.contract_ensures contract
             |> List.iter (fun clause -> visit (Sst_validation.contract_clause_expression clause)))
end
