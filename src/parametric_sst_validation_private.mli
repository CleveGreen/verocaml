type error = {
  function_id : Sst.function_id;
  span : Diagnostic.span;
  message : string;
}

val validate_program : Sst.program -> (unit, error) result
