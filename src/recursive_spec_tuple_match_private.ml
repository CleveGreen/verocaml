type leaf = {
  expression : Sst.expression;
  pattern : Sst.pattern;
}

type error =
  | Expected_tuple_value
  | Expected_tuple_pattern
  | Tuple_type_mismatch
  | Tuple_label_mismatch
  | Tuple_arity_mismatch
  | Tuple_nesting_mismatch
  | Empty_tuple
  | Unsupported_leaf_type

let error_to_string = function
  | Expected_tuple_value -> "tuple match scrutinee is not a syntactic tuple value"
  | Expected_tuple_pattern -> "tuple match case is not a syntactic tuple pattern"
  | Tuple_type_mismatch -> "tuple match component types do not agree"
  | Tuple_label_mismatch -> "tuple match component labels do not agree"
  | Tuple_arity_mismatch -> "tuple match component arity does not agree"
  | Tuple_nesting_mismatch -> "tuple match component nesting does not agree"
  | Empty_tuple -> "tuple match has an empty tuple node"
  | Unsupported_leaf_type -> "tuple match has an unsupported component type"

let same_type = Parametric_type.equal

let exact_tuple_type typ components component_type =
  match typ with
  | Sst.Tuple declared ->
      List.length declared = List.length components
      && List.for_all2
           (fun (declared_label, declared_type) (actual_label, component) ->
             declared_label = actual_label
             && same_type declared_type (component_type component))
           declared components
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Aggregate _
  | Sst.Parameter _ | Sst.Application _ ->
      false

let same_labels left right =
  List.map fst left = List.map fst right

let plan (expression : Sst.expression) (pattern : Sst.pattern) =
  let rec pair leaves (expression : Sst.expression) (pattern : Sst.pattern) =
    if not (same_type expression.typ pattern.typ) then Error Tuple_type_mismatch
    else
      match (expression.expression_desc, pattern.pattern_desc) with
      | Sst.Tuple_value expressions, Sst.Tuple_pattern patterns ->
          if expressions = [] || patterns = [] then Error Empty_tuple
          else if List.length expressions <> List.length patterns then
            Error Tuple_arity_mismatch
          else if not (same_labels expressions patterns) then
            Error Tuple_label_mismatch
          else if
            not
              (exact_tuple_type expression.typ expressions
                 (fun expression -> expression.Sst.typ))
            || not
                 (exact_tuple_type pattern.typ patterns
                    (fun pattern -> pattern.Sst.typ))
          then Error Tuple_type_mismatch
          else
            List.fold_left2
              (fun result (_, expression) (_, pattern) ->
                Result.bind result (fun leaves ->
                    pair leaves expression pattern))
              (Ok leaves) expressions patterns
      | Sst.Tuple_value _, _ | _, Sst.Tuple_pattern _ ->
          Error Tuple_nesting_mismatch
      | _, _ -> (
          match expression.typ with
          | Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Aggregate _
          | Sst.Application _ ->
              Ok ({ expression; pattern } :: leaves)
          | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ ->
              Error Unsupported_leaf_type)
  in
  match expression.expression_desc with
  | Sst.Tuple_value _ -> (
      match pattern.pattern_desc with
      | Sst.Tuple_pattern _ -> Result.map List.rev (pair [] expression pattern)
      | _ -> Error Expected_tuple_pattern)
  | _ -> Error Expected_tuple_value
