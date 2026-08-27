val validate :
  admitted_type:(Sst.typ -> bool) ->
  admit_tuple_match:bool ->
  malformed:(Sst.expression -> 'error) ->
  Sst.function_definition ->
  Sst.expression ->
  (unit, 'error) result

val validate_logical_tuple_match :
  malformed:(Diagnostic.span -> string -> (unit, 'error) result) ->
  validate_expression:(Sst.expression -> (unit, 'error) result) ->
  validate_pattern:(Sst.pattern -> (unit, 'error) result) ->
  result_type:Sst.typ ->
  Sst.expression ->
  Sst.case list ->
  (unit, 'error) result
