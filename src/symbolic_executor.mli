type unsupported =
  | Ghost_call
  | Or_pattern
  | Missing_summary of Sst.function_id
  | Recursion_awaits_totality of Sst.function_id
  | Missing_decreases
  | Duplicate_decreases
  | Inapplicable_decreases
  | Non_integer_decreases
  | Malformed_sst of string

type error = {
  function_name : string;
  span : Diagnostic.span;
  unsupported : unsupported;
}

val lower_function :
  Sst.function_definition -> (Vir.function_execution, error) result

val lower_program : Sst.program -> (Vir.program, error) result
val error_to_string : error -> string
