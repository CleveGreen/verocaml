type role = Wrapper | Target

type error = { span : Diagnostic.span; message : string }

val validate :
  external_specifications:External_target_specification_private.registration option ->
  program:Sst.program ->
  functions:Sst.function_definition list ->
  definition:Sst.function_definition ->
  role:role ->
  Sst.target_link ->
  (unit, error) result
