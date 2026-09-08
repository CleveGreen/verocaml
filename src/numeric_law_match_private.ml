type result_interpretation = Mathematical_result | Lifted_runtime_result | Boolean_result | Carrier_result

type unsigned_range = {
  declaration : Numeric_semantics_correlation_private.t;
  target : Build_target_profile_private.instance;
  view : Sst.function_definition;
  result_interpretation : result_interpretation;
  parameter : Sst.binding;
  lower_bound : Sst.ensures_clause;
  upper_bound : Sst.ensures_clause;
  semantic_width : int;
  semantic_width_evidence : string;
}

type signed_view = {
  declaration : Numeric_semantics_correlation_private.t;
  unsigned : unsigned_range;
  view : Sst.function_definition;
  result_interpretation : result_interpretation;
  parameter : Sst.binding;
  relation : Sst.ensures_clause;
  semantic_width : int;
  semantic_width_evidence : string;
}

type statement = Unsigned of unsigned_range | Signed of signed_view

let declaration = function Unsigned statement -> statement.declaration | Signed statement -> statement.declaration
let views = function Unsigned statement -> [statement.view] | Signed statement -> [statement.view; statement.unsigned.view]

let scalar_parameter (definition : Sst.function_definition) =
  match definition.parameters, definition.type_binders with
  | [ Sst.Value_parameter
        { label = None; optional_default = None;
          pattern = { pattern_desc = Sst.Bind binding;
                      typ = (Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Unit); _ } } ], [] ->
      Some binding
  | _ -> None

let unconditional_view declaration =
  let proof = declaration.Numeric_semantics_correlation_private.definition in
  match declaration.kind, declaration.callable_definition, scalar_parameter proof with
  | (Completed_proof_declaration | Explicit_axiom_declaration), Some view, Some parameter
    when not proof.recursive && proof.result_type = Sst.Unit
      && proof.contracts.requires = [] && view.mode = Sst.Spec && not view.recursive
      && (view.result_type = Sst.Mathematical_int || view.result_type = Sst.Int)
      && view.contracts.requires = [] -> (
      match scalar_parameter view with
      | Some view_parameter when Parametric_type.equal view_parameter.typ parameter.typ -> Some (view, parameter)
      | _ -> None)
  | _ -> None

let constant expected expression =
  expression.Sst.typ = Sst.Mathematical_int
  && Option.fold ~none:false ~some:(Z.equal expected)
       (Mathematical_int_rewrite_private.constant_value_with
          Mathematical_int_rewrite_private.default_config expression)

let is_view ~view ~parameter expression =
  let call =
    match expression.Sst.typ, expression.expression_desc, view.Sst.result_type with
    | Sst.Mathematical_int, Sst.Lift_runtime_int operand, Sst.Int
      when operand.typ = Sst.Int -> Some operand
    | Sst.Mathematical_int, _, Sst.Mathematical_int -> Some expression
    | _ -> None
  in
  Option.fold ~none:false ~some:(fun expression ->
     match expression.Sst.expression_desc with
     | Sst.Direct_call
         { call_form = Sst.Specification_call; callee; type_arguments = [];
           arguments = [ Sst.Value_argument
             { label = None; value =
                 { expression_desc = Sst.Variable { binding; _ }; _ } } ];
           recursive = false } ->
         callee = view.Sst.function_id && binding = parameter
     | _ -> false) call

let conjuncts expression =
  let rec collect accumulated = function
    | [] -> List.rev accumulated
    | { Sst.expression_desc = Sst.Boolean_binary (Sst.And, left, right); _ } :: rest ->
        collect accumulated (left :: right :: rest)
    | expression :: rest -> collect (expression :: accumulated) rest
  in
  collect [] [ expression ]

let constant_value expression =
  if expression.Sst.typ <> Sst.Mathematical_int then None
  else
    Mathematical_int_rewrite_private.constant_value_with
      Mathematical_int_rewrite_private.default_config expression

let bounds ~view ~parameter (clause : Sst.ensures_clause) =
  if clause.predicate.stage <> Sst.Logical then (false, [])
  else
    List.fold_left (fun (lower, upper_limits) expression ->
        let is_view = is_view ~view ~parameter in
        match expression.Sst.expression_desc with
        | Sst.Compare (Sst.Less_or_equal, left, right) ->
            (lower || (constant Z.zero left && is_view right), upper_limits)
        | Sst.Compare (Sst.Greater_or_equal, left, right) ->
            (lower || (is_view left && constant Z.zero right), upper_limits)
        | Sst.Compare (Sst.Less_than, left, right) ->
            (match constant_value right with
            | Some limit when is_view left -> (lower, limit :: upper_limits)
            | None | Some _ -> (lower, upper_limits))
        | Sst.Compare (Sst.Greater_than, left, right) ->
            (match constant_value left with
            | Some limit when is_view right -> (lower, limit :: upper_limits)
            | None | Some _ -> (lower, upper_limits))
        | _ -> (lower, upper_limits))
      (false, []) (conjuncts clause.predicate.expression)

let power_of_two_width limit =
  if Z.sign limit <= 0 || Z.popcount limit <> 1 then None
  else
    let width = Z.numbits limit - 1 in
    if width <= 0 then None else Some width

let unsigned_range ~target:(target [@delator.skip])
    (declaration [@delator.skip]) =
  let unmatched (reason [@log_value.debug]) () =
    [%log.debug "numeric contract did not match unsigned-range statement"
      ~reason:(Delator.Field.string (reason [@log_value.debug]))];
    None
  in
  let result =
    let proof = declaration.Numeric_semantics_correlation_private.definition in
    match unconditional_view declaration with
    | Some (view, parameter) -> (
            let lower, upper_candidates =
                List.fold_left (fun (lower, upper) clause ->
                    let has_lower, upper_limits = bounds ~view ~parameter clause in
                    [%log.trace "checked numeric range contract clause"
                      ~clause_index:(Delator.Field.int clause.Sst.clause_index)
                      ~lower_bound_matched:(Delator.Field.bool has_lower)
                      ~upper_bound_candidates:
                        (Delator.Field.int (List.length upper_limits))];
                    (if has_lower && Option.is_none lower then Some clause else lower),
                    (List.fold_left
                       (fun candidates limit ->
                         match power_of_two_width limit with
                         | None -> candidates
                         | Some width -> (width, clause) :: candidates)
                       upper upper_limits))
                  (None, []) proof.contracts.ensures
              in
              let widths =
                upper_candidates |> List.map fst |> List.sort_uniq Int.compare
              in
              (match (lower, widths) with
              | Some lower_bound, [ semantic_width ] ->
                  let upper_bound =
                    upper_candidates
                    |> List.find (fun (width, _) -> width = semantic_width)
                    |> snd
                  in
                  let result_interpretation =
                    if view.result_type = Sst.Mathematical_int then Mathematical_result
                    else Lifted_runtime_result
                  in
                  let semantic_width_evidence =
                    Numeric_receipt_private.encode
                      ~schema:"verocaml.numeric-range-width-evidence.v1"
                      [ Numeric_source_claim_private.role_material
                          declaration.role.numeric_role_source;
                        declaration.role.numeric_role_callable_uid;
                        declaration.role.numeric_role_semantics_uid;
                        declaration.role.numeric_role_carrier_uid;
                        string_of_int lower_bound.clause_index;
                        string_of_int upper_bound.clause_index;
                        string_of_int semantic_width;
                        Sst.to_string
                          { Sst.policy = proof.policy; types = [];
                            parametric_adts = []; functions = [ proof ];
                            logical_constants = [] } ]
                  in
                  Some { declaration; target; view; result_interpretation;
                         parameter; lower_bound; upper_bound; semantic_width;
                         semantic_width_evidence }
              | _ -> unmatched ("unmatched-postconditions" [@log_value.debug]) ()))
    | _ -> unmatched ("not-an-unconditional-unary-ghost-contract" [@log_value.debug]) ()
  in
  [%log.debug "matched numeric unsigned-range statement"
    ~semantics:(Delator.Field.string declaration.Numeric_semantics_correlation_private.role.numeric_role_semantics_path)
    ~target_width:(Delator.Field.int target.Build_target_profile_private.target_claim.width)
    ~semantic_width:(Delator.Field.int (match result with Some range -> range.semantic_width | None -> 0))
    ~matched:(Delator.Field.bool (Option.is_some result))
    ~lowering_authority:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]

let signed_view ~unsigned:((unsigned : unsigned_range) [@delator.skip]) (declaration [@delator.skip]) =
  let result =
    match unconditional_view declaration with
    | Some (view, parameter)
      when Parametric_type.equal parameter.typ unsigned.parameter.typ ->
        let width = unsigned.semantic_width in
        if width >= Mathematical_int_rewrite_private.default_config.max_constant_bits then None
        else
          let half = Z.shift_left Z.one (width - 1) and modulus = Z.shift_left Z.one width in
          let is_unsigned = is_view ~view:unsigned.view ~parameter in
          let conditional expression =
            expression.Sst.typ = Sst.Mathematical_int &&
            match expression.expression_desc with
            | Sst.If (condition, positive, Some negative) ->
                let condition_matches = match condition.expression_desc with
                  | Sst.Compare (Less_than, value, limit) -> is_unsigned value && constant half limit
                  | Compare (Greater_than, limit, value) -> constant half limit && is_unsigned value
                  | _ -> false in
                condition_matches && is_unsigned positive && negative.typ = Sst.Mathematical_int
                && (match negative.expression_desc with
                  | Checked_arithmetic (Subtract, [value; limit]) -> is_unsigned value && constant modulus limit
                  | _ -> false)
            | _ -> false in
          let relation = List.find_opt (fun (clause : Sst.ensures_clause) ->
              let matched = clause.predicate.stage = Sst.Logical &&
                List.exists (fun expression -> match expression.Sst.expression_desc with
                  | Sst.Compare (Equal, left, right) ->
                      (is_view ~view ~parameter left && conditional right)
                      || (conditional left && is_view ~view ~parameter right)
                  | _ -> false) (conjuncts clause.predicate.expression) in
              [%log.trace "matched signed numeric relation clause"
                ~clause_index:(Delator.Field.int clause.clause_index)
                ~matched:(Delator.Field.bool matched)];
              matched) declaration.definition.contracts.ensures in
          Option.map (fun (relation : Sst.ensures_clause) ->
              let result_interpretation = if view.result_type = Sst.Mathematical_int then Mathematical_result else Lifted_runtime_result in
              let semantic_width_evidence =
                Numeric_receipt_private.encode
                  ~schema:"verocaml.numeric-signed-width-evidence.v1"
                  [ unsigned.semantic_width_evidence;
                    Numeric_source_claim_private.role_material
                      declaration.role.numeric_role_source;
                    declaration.role.numeric_role_callable_uid;
                    declaration.role.numeric_role_semantics_uid;
                    string_of_int relation.clause_index ]
              in
              { declaration; unsigned; view; result_interpretation; parameter;
                relation; semantic_width = width; semantic_width_evidence })
            relation
    | _ -> None in
  [%log.debug "matched exact signed numeric view relation"
    ~semantics:(Delator.Field.string declaration.Numeric_semantics_correlation_private.role.numeric_role_semantics_path)
    ~unsigned_semantics:(Delator.Field.string unsigned.declaration.role.numeric_role_semantics_path)
    ~target_width:(Delator.Field.int unsigned.target.target_claim.width)
    ~semantic_width:(Delator.Field.int unsigned.semantic_width)
    ~matched:(Delator.Field.bool (Option.is_some result))];
  result
[@@delator.instrument] [@@delator.level debug]
