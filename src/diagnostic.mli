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
  | Unsupported_generic_use
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
  | Structural_aggregate_equality
  | Wrapping_arithmetic
  | Aggregate
  | Mutation
  | Unsupported_pattern
  | Refutable_parameter_pattern
  | Unsupported_expression
  | Malformed_ghost_call
  | Callback_authentication
  | Callback_policy
  | Callback_contract
  | Quantifier_authentication
  | Quantifier_trigger
  | Quantifier_type
  | Unsupported_logical_quantifier

type failure_class = Source_failure | Artifact_failure | Internal_failure

type broadcast_artifact_failure =
  | Missing_provider_artifact
  | Malformed_provider_artifact
  | Stale_provider_artifact
  | Mismatched_provider_artifact
  | Conflicting_provider_artifact

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
  | Invalid_broadcast_dependency of {
      provider : string;
      failure : broadcast_artifact_failure;
    }
  | Invalid_symbolic_declaration of string
  | Invalid_symbolic_application of string
  | Invalid_symbolic_authentication of string
  | Invalid_symbolic_dependency of {
      provider : string;
      reason : string;
      remedy : string;
    }
  | Invalid_logical_constant_declaration of string
  | Invalid_logical_constant_use of string
  | Invalid_logical_constant_authentication of string
  | Invalid_numeric_declaration of string
  | Executable_function_in_specification of { function_name : string }
  | Unannotated_erased_call of {
      caller_name : string;
      callee_name : string;
      callee_mode : string;
    }
  | Invalid_verification_call of {
      callee_name : string;
      callee_mode : string;
      context : string;
    }
  | Invalid_imported_specification of string
  | Invalid_semantic_program of {
      function_name : string option;
      detail : string;
    }
  | Unsupported_construct of unsupported_construct

type submessage =
  | Hint of string
  | Note of {
      span : span;
      message : string;
    }

type t = {
  classification : classification;
  code : string;
  message : string;
  span : span;
  submessages : submessage list;
}

val file_span : string -> span
val failure_class : classification -> failure_class
val span_of_location : fallback_file:string -> Location.t -> span
val location_of_span : span -> Location.t
val make : classification -> span -> t
val with_message : string -> t -> t
val with_hint : string -> t -> t
val with_hints : string list -> t -> t
val with_note : span:span -> string -> t -> t
val report : t -> Location.report
