type error = { span : Diagnostic.span; detail : string }
val validate : Sst.program -> (unit, error) result
