val make_definition :
  source:Typedtree_symbolic_private.declaration ->
  function_id:Sst.function_id ->
  type_binders:Parametric_type.binder list ->
  parameters:Sst.parameter list ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  (Sst.function_definition, string) result

val seal : program:Sst.program -> (unit, string) result

val authenticate :
  program:Sst.program ->
  definition:Sst.function_definition ->
  (Symbolic_application_private.declaration, string) result

val validate_application :
  program:Sst.program ->
  logical:bool ->
  expression_type:Sst.typ ->
  Sst.expression Symbolic_application_private.t ->
  (Sst.expression list, string) result

val validate_definition :
  program:Sst.program ->
  supported:(Sst.typ -> bool) ->
  Sst.function_definition ->
  Symbolic_application_private.declaration ->
  (unit, string) result

val application :
  declaration:Symbolic_application_private.declaration ->
  type_arguments:Parametric_type.t list ->
  arguments:Sst.expression list ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  (Sst.expression Symbolic_application_private.t, string) result

val is_symbolic : Sst.function_definition -> bool
val destroy : Sst.program -> unit
