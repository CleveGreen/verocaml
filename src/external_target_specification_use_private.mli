val create :
  Sst.target_link ->
  call_span:Diagnostic.span ->
  requires_count:int ->
  ensures_count:int ->
  Vir.trusted_summary_use

val executable_summary :
  Sst.function_definition -> Sst.function_definition * Sst.expression

val collect :
  validated:Sst_validation.validated_program ->
  caller:Sst.function_id ->
  Vir.trusted_summary_use list
