val lower_arguments :
  lower_expression:
    (Typedtree.expression -> (Sst.expression, 'error) result) ->
  span:(Location.t -> Diagnostic.span) ->
  reject:(Diagnostic.unsupported_construct -> 'error) ->
  application:Typedtree.expression ->
  signature:Parametric_signature_private.t ->
  result_type:Sst.typ ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  (Sst.typ list * Sst.call_argument list, 'error) result

val lower_direct_call :
  lower_expression:(Typedtree.expression -> (Sst.expression, 'error) result) ->
  span:(Location.t -> Diagnostic.span) ->
  reject:(Diagnostic.unsupported_construct -> 'error) ->
  application:Typedtree.expression ->
  signature:Parametric_signature_private.t ->
  definition:Sst.function_definition ->
  result_type:Sst.typ ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  (Sst.expression, 'error) result
