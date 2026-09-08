let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let rec iter_result function_ = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = function_ value in
      iter_result function_ rest

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let scalar_type = function
  | Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Bit_vector _
  | Sst.Parameter _ -> true
  | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _
  | Sst.Application _ -> false

let validate ~admitted_type ~admit_tuple_match ~malformed definition expression =
  let function_id = definition.Sst.function_id in
  let reject expression = Error (malformed expression) in
  let rec admitted_pattern (pattern : Sst.pattern) =
    match pattern.pattern_desc with
    | Sst.Wildcard ->
        admitted_type pattern.typ
        ||
        (match pattern.typ with
        | Sst.Parameter _ -> true
        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
        | Sst.Bit_vector _ | Sst.Tuple _
        | Sst.Aggregate _
        | Sst.Application _ ->
            false)
    | Sst.Bind binding ->
        binding.typ = pattern.typ
        && (scalar_type pattern.typ
           || (binding.uniqueness = Sst.Definitely_aliased
              && admitted_type pattern.typ))
    | Sst.Int_pattern _ -> Parametric_type.is_integer pattern.typ
    | Sst.Bool_pattern _ -> pattern.typ = Sst.Bool
    | Sst.Record_pattern fields ->
        admitted_type pattern.typ
        && List.for_all
             (fun (_, nested) -> admitted_pattern nested)
             fields
    | Sst.Constructor_pattern (_, arguments) ->
        admitted_type pattern.typ && List.for_all admitted_pattern arguments
    | Sst.Unit_pattern | Sst.Tuple_pattern _ | Sst.Owned_tree_cursor_pattern _
    | Sst.Or_pattern _ ->
        false
  in
  let rec loop scoped (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Int_constant _ | Sst.Bool_constant _ -> Ok ()
    | Sst.Variable _
      when scalar_type expression.typ || admitted_type expression.typ ->
        Ok ()
    | Sst.Lift_runtime_int operand
      when expression.typ = Sst.Mathematical_int && operand.typ = Sst.Int ->
        loop scoped operand
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let kind =
          match expression.expression_desc with
          | Sst.Forall _ -> Logic_quantifier_private.Forall
          | Sst.Exists _ -> Logic_quantifier_private.Exists
          | _ -> assert false
        in
        let binder = quantifier.quantifier_binder in
        let* () =
          match
            Quantifier_validation_private.validate_sst
              ~allow_unclassified:true
              ~expected_owner:("function:" ^ function_id.function_name)
              kind quantifier
          with
          | Ok () -> Ok ()
          | Error _ -> reject expression
        in
        if
          binder.uniqueness <> Sst.Definitely_aliased
          || not
               (scalar_type binder.typ || admitted_type binder.typ)
          || expression.typ <> Sst.Bool
        then reject expression
        else
          let scoped = binder.id :: scoped in
          let* () = loop scoped quantifier.quantifier_body in
          iter_result (loop scoped)
            (Option.to_list quantifier.quantifier_trigger)
    | Sst.Let (bindings, body) ->
        let* () =
          iter_result
            (fun (pattern, value) ->
              match pattern.Sst.pattern_desc with
              | Sst.Tuple_pattern _
                when admit_tuple_match
                     &&
                     (match value.Sst.expression_desc with
                     | Sst.Tuple_value _ -> true
                     | _ -> false) -> (
                  [%log.trace
                    "validating exact tuple binding in recursive specification"
                    ~stage:
                      (Delator.Field.string "recursive-body-validation")
                    ~decision:(Delator.Field.string "validate")];
                  match
                    Recursive_spec_tuple_match_private.plan value pattern
                  with
                  | Ok leaves
                    when
                      List.for_all
                        (fun
                          (leaf :
                            Recursive_spec_tuple_match_private.leaf)
                        ->
                          admitted_type leaf.expression.typ
                          && admitted_pattern leaf.pattern)
                        leaves ->
                      [%log.trace
                        "admitting exact tuple destructuring in recursive specification"
                        ~stage:
                          (Delator.Field.string
                             "recursive-body-validation")
                        ~decision:(Delator.Field.string "accepted")
                        ~leaf_count:(Delator.Field.int (List.length leaves))];
                      iter_result
                        (fun
                          (leaf :
                            Recursive_spec_tuple_match_private.leaf)
                        -> loop scoped leaf.expression)
                        leaves
                  | Ok _ ->
                      [%log.debug
                        "rejecting recursive tuple binding with inadmissible leaves"
                        ~stage:
                          (Delator.Field.string "recursive-body-validation")
                        ~decision:(Delator.Field.string "rejected")
                        ~reason:(Delator.Field.string "inadmissible-leaf")];
                      reject expression
                  | Error _ ->
                      [%log.debug
                        "rejecting malformed recursive tuple binding"
                        ~stage:
                          (Delator.Field.string "recursive-body-validation")
                        ~decision:(Delator.Field.string "rejected")
                        ~reason:(Delator.Field.string "invalid-plan")];
                      reject expression)
              | Sst.Bind
                  {
                    typ = ((Sst.Int | Sst.Bool) as typ);
                    _;
                  }
                when typ = value.Sst.typ ->
                  loop scoped value
              | Sst.Bind { typ; uniqueness = Sst.Definitely_aliased; _ }
                when typ = value.Sst.typ
                     && admitted_type typ ->
                  loop scoped value
              | Sst.Wildcard
                when scalar_type value.Sst.typ
                     || admitted_type value.Sst.typ ->
                  loop scoped value
              | _
                when pattern.Sst.typ = value.Sst.typ
                     && admitted_pattern pattern ->
                  loop scoped value
              | _ -> reject expression)
            bindings
        in
        loop scoped body
    | Sst.If (condition, consequent, Some alternative) ->
        let* () = loop scoped condition in
        let* () = loop scoped consequent in
        loop scoped alternative
    | Sst.Checked_arithmetic (_, operands) ->
        iter_result (loop scoped) operands
    | Sst.Compare (comparison, left, right) ->
        let supported =
          left.typ = right.typ
          &&
          match comparison with
          | Sst.Equal | Sst.Not_equal ->
              scalar_type left.typ
              || admitted_type left.typ
          | Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
          | Sst.Greater_or_equal ->
              left.typ = Sst.Int || left.typ = Sst.Mathematical_int
        in
        if not supported then reject expression
        else
          let* () = loop scoped left in
          loop scoped right
    | Sst.Boolean_binary (_, left, right) ->
        let* () = loop scoped left in
        loop scoped right
    | Sst.Boolean_not operand -> loop scoped operand
    | Sst.Constructor_value { arguments; _ } ->
        if admitted_type expression.typ then
          iter_result (loop scoped) arguments
        else reject expression
    | Sst.Record_value { fields; _ } ->
        if admitted_type expression.typ then
          iter_result (fun (_, value) -> loop scoped value) fields
        else reject expression
    | Sst.Field_read { record; _ } ->
        if admitted_type record.typ then loop scoped record
        else reject expression
    | Sst.Match
        (({ expression_desc = Sst.Tuple_value _; _ } as scrutinee), cases) ->
        [%log.trace
          "validating tuple destructuring in recursive specification"
          ~stage:(Delator.Field.string "recursive-body-validation")
          ~decision:(Delator.Field.string "validate")
          ~case_count:(Delator.Field.int (List.length cases))];
        if not admit_tuple_match then reject expression
        else
          let* plans =
            List.fold_left
              (fun result (case : Sst.case) ->
                let* plans = result in
                match
                  Recursive_spec_tuple_match_private.plan scrutinee
                    case.case_pattern
                with
                | Error _ ->
                    [%log.debug
                      "rejecting malformed recursive tuple destructuring"
                      ~stage:
                        (Delator.Field.string "recursive-body-validation")
                      ~decision:(Delator.Field.string "rejected")
                      ~reason:(Delator.Field.string "invalid-plan")];
                    reject expression
                | Ok leaves ->
                    let invalid_leaf_count =
                      List.length
                        (List.filter
                        (fun
                          (leaf :
                            Recursive_spec_tuple_match_private.leaf)
                        ->
                          not
                            (admitted_type leaf.expression.typ
                            && admitted_pattern leaf.pattern))
                        leaves)
                    in
                    if invalid_leaf_count = 0 then
                      Ok ((case, leaves) :: plans)
                    else (
                      [%log.debug
                        "rejecting inadmissible recursive tuple destructuring leaf"
                        ~stage:
                          (Delator.Field.string "recursive-body-validation")
                        ~decision:(Delator.Field.string "rejected")
                        ~reason:(Delator.Field.string "inadmissible-leaf")
                        ~leaf_count:(Delator.Field.int (List.length leaves))
                        ~invalid_leaf_count:
                          (Delator.Field.int invalid_leaf_count)];
                      reject expression))
              (Ok []) cases
          in
          let* () =
            iter_result
              (fun (_case, leaves) ->
                iter_result
                  (fun
                    (leaf : Recursive_spec_tuple_match_private.leaf)
                  -> loop scoped leaf.expression)
                  leaves)
              (List.rev plans)
          in
          iter_result
            (fun (case, _leaves) ->
              let* () =
                iter_result (loop scoped)
                  (Option.to_list case.Sst.case_guard)
              in
              loop scoped case.case_body)
            (List.rev plans)
    | Sst.Match (scrutinee, cases) ->
        if not (admitted_type scrutinee.typ) then
          reject expression
        else
          let* () = loop scoped scrutinee in
          iter_result
            (fun (case : Sst.case) ->
              let* () =
                if
                  case.case_pattern.typ = scrutinee.typ
                  && admitted_pattern case.case_pattern
                then Ok ()
                else reject expression
              in
              let* () =
                iter_result (loop scoped)
                  (Option.to_list case.case_guard)
              in
              loop scoped case.case_body)
            cases
    | Sst.Symbolic_application application ->
        if
          not
            (Parametric_type.equal expression.typ
               (Symbolic_application_private.result_type application))
          || not (scalar_type expression.typ || admitted_type expression.typ)
        then reject expression
        else (
          [%log.trace
            "admitting authenticated symbolic application in recursive specification"
            ~stage:(Delator.Field.string "recursive-body-validation")
            ~decision:(Delator.Field.string "accepted")
            ~correlation:
              (Delator.Field.string
                 (Symbolic_application_private.identity_digest application))
            ~type_arity:
              (Delator.Field.int
                 (List.length
                    (Symbolic_application_private.type_arguments application)))
            ~term_arity:
              (Delator.Field.int
                 (List.length
                    (Symbolic_application_private.arguments application)))];
          iter_result (loop scoped)
            (Symbolic_application_private.arguments application))
    | Sst.Logical_constant_reference
        (reference [@log_value.trace]) ->
        if scalar_type expression.typ || admitted_type expression.typ then (
          [%log.trace
            "admitting authenticated logical constant in recursive specification"
            ~stage:(Delator.Field.string "recursive-body-validation")
            ~constant_name:
              (Delator.Field.string
                 (reference [@log_value.trace]).constant.constant_name)
            ~type_argument_count:
              (Delator.Field.int
                 (List.length
                    (reference [@log_value.trace]).type_arguments))
            ~result_type:
              (Delator.Field.string
                 (Parametric_type.to_string expression.typ))
            ~decision:(Delator.Field.string "accepted")];
          Ok ())
        else reject expression
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          callee;
          arguments;
          recursive = true;
          _;
        }
      when same_function_id callee function_id ->
        iter_result
          (function
            | Sst.Value_argument { value; _ } -> loop scoped value
            | Sst.Callback_argument _ -> reject expression)
          arguments
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          arguments;
          recursive = false;
          _;
        } ->
        iter_result
          (function
            | Sst.Value_argument { value; _ } -> loop scoped value
            | Sst.Callback_argument _ -> reject expression)
          arguments
    | _ -> reject expression
  in
  loop [] expression

let validate_logical_tuple_match ~malformed ~validate_expression
    ~validate_pattern ~result_type (scrutinee : Sst.expression) cases =
  if cases = [] then malformed scrutinee.Sst.span "spec match has no cases"
  else
    iter_result
      (fun (case : Sst.case) ->
        match
          Recursive_spec_tuple_match_private.plan scrutinee case.case_pattern
        with
        | Error error ->
            malformed case.case_pattern.span
              (Recursive_spec_tuple_match_private.error_to_string error)
        | Ok leaves ->
            let* () =
              iter_result
                (fun (leaf : Recursive_spec_tuple_match_private.leaf) ->
                  let* () = validate_expression leaf.expression in
                  validate_pattern leaf.pattern)
                leaves
            in
            let* () =
              match case.case_guard with
              | None -> Ok ()
              | Some guard ->
                  if guard.typ = Sst.Bool then validate_expression guard
                  else malformed guard.span "spec match guard must be Boolean"
            in
            if case.case_body.typ = result_type then
              validate_expression case.case_body
            else
              malformed case.case_body.span
                "spec match case result type mismatch")
      cases
