type position = {
  line : int;
  column : int;
}

type span = {
  file : string;
  start_pos : position;
  end_pos : position;
}

type unsupported_input =
  | Interface
  | Packed
  | Partial_implementation
  | Partial_interface
  | Explicit_cmi_parameters
  | Explicit_cmi_argument_for

type unsupported_construct =
  | Unsupported_type
  | Unsupported_structure_item
  | Unsupported_top_level_binding
  | Partial_match
  | Partial_function_parameter
  | Mutual_recursion
  | Polymorphic_function
  | Higher_order_function
  | Higher_order_call
  | Unknown_or_external_call
  | Loop
  | Effect
  | Exception
  | Array
  | Object
  | First_class_module
  | Concurrency
  | Nonlinear_multiplication
  | Structural_aggregate_equality
  | Wrapping_arithmetic
  | Aggregate
  | Mutation
  | Unsupported_pattern
  | Unsupported_expression
  | Malformed_ghost_call
  | Callback_authentication
  | Callback_policy
  | Callback_contract
  | Quantifier_authentication
  | Quantifier_trigger
  | Quantifier_type
  | Unsupported_logical_quantifier

type classification =
  | Unsupported_target of {
      expected_int_size : int;
      actual_int_size : int;
    }
  | Unsupported_input of unsupported_input
  | Malformed_input
  | Incompatible_magic
  | Input_io_error
  | Invalid_recursive_rank of string
  | Invalid_broadcast of string
  | Invalid_symbolic_declaration of string
  | Invalid_symbolic_application of string
  | Invalid_symbolic_authentication of string
  | Unsupported_construct of unsupported_construct

type t = {
  classification : classification;
  code : string;
  message : string;
  span : span;
}

val file_span : string -> span
val span_of_location : fallback_file:string -> Location.t -> span
val make : classification -> span -> t
