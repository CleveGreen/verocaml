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

let position_of_lexing_position position =
  {
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol;
  }

let file_span file =
  {
    file;
    start_pos = { line = 1; column = 0 };
    end_pos = { line = 1; column = 0 };
  }

let span_of_location ~fallback_file location =
  if Location.is_none location then file_span fallback_file
  else
    let file =
      if String.equal location.Location.loc_start.Lexing.pos_fname "" then
        fallback_file
      else location.Location.loc_start.Lexing.pos_fname
    in
    {
      file;
      start_pos = position_of_lexing_position location.Location.loc_start;
      end_pos = position_of_lexing_position location.Location.loc_end;
    }

let code_and_message = function
  | Unsupported_target { expected_int_size; actual_int_size } ->
      ( "VERO_UNSUPPORTED_TARGET",
        Printf.sprintf "unsupported OCaml integer width: expected %d, found %d"
          expected_int_size actual_int_size )
  | Unsupported_input Interface ->
      ("VERO_UNSUPPORTED_INTERFACE", "interface typed trees are not supported")
  | Unsupported_input Packed ->
      ("VERO_UNSUPPORTED_PACK", "packed typed trees are not supported")
  | Unsupported_input Partial_implementation ->
      ( "VERO_UNSUPPORTED_PARTIAL_IMPLEMENTATION",
        "partial implementation typed trees are not supported" )
  | Unsupported_input Partial_interface ->
      ( "VERO_UNSUPPORTED_PARTIAL_INTERFACE",
        "partial interface typed trees are not supported" )
  | Unsupported_input Explicit_cmi_parameters ->
      ( "VERO_UNSUPPORTED_CMI_PARAMETERS",
        "explicit CMI compilation-unit parameters are not supported" )
  | Unsupported_input Explicit_cmi_argument_for ->
      ( "VERO_UNSUPPORTED_CMI_ARGUMENT_FOR",
        "explicit CMI argument-for metadata is not supported" )
  | Malformed_input ->
      ("VERO_MALFORMED_INPUT", "input is not a complete typed-tree artifact")
  | Incompatible_magic ->
      ( "VERO_INCOMPATIBLE_CMT",
        "input uses a CMT ABI incompatible with the pinned compiler" )
  | Input_io_error -> ("VERO_INPUT_IO", "input could not be read")
  | Invalid_recursive_rank detail ->
      ("VERO_INVALID_RECURSIVE_RANK", detail)
  | Invalid_broadcast detail -> ("VERO_BROADCAST_AUTHENTICATION", detail)
  | Invalid_symbolic_declaration detail ->
      ("VERO_SYMBOLIC_DECLARATION", detail)
  | Invalid_symbolic_application detail ->
      ("VERO_SYMBOLIC_APPLICATION", detail)
  | Invalid_symbolic_authentication detail ->
      ("VERO_SYMBOLIC_AUTHENTICATION", detail)
  | Unsupported_construct construct -> (
      match construct with
      | Unsupported_type ->
          ("VERO_UNSUPPORTED_TYPE", "type is outside the pure SST subset")
      | Unsupported_structure_item ->
          ( "VERO_UNSUPPORTED_STRUCTURE_ITEM",
            "structure item is outside the pure SST subset" )
      | Unsupported_top_level_binding ->
          ( "VERO_UNSUPPORTED_TOP_LEVEL_BINDING",
            "top-level binding is not a supported direct function" )
      | Partial_match ->
          ("VERO_UNSUPPORTED_PARTIAL_MATCH", "partial matches are not supported")
      | Partial_function_parameter ->
          ( "VERO_UNSUPPORTED_PARTIAL_PARAMETER",
            "partial function parameters are not supported" )
      | Mutual_recursion ->
          ("VERO_UNSUPPORTED_MUTUAL_RECURSION", "mutual recursion is not supported")
      | Polymorphic_function ->
          ("VERO_UNSUPPORTED_POLYMORPHISM", "polymorphic functions are not supported")
      | Higher_order_function ->
          ( "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION",
            "nested and higher-order functions are not supported" )
      | Higher_order_call ->
          ("VERO_UNSUPPORTED_HIGHER_ORDER_CALL", "higher-order calls are not supported")
      | Unknown_or_external_call ->
          ( "VERO_UNSUPPORTED_EXTERNAL_CALL",
            "unknown and external calls are not supported" )
      | Loop -> ("VERO_UNSUPPORTED_LOOP", "loops are not supported")
      | Effect -> ("VERO_UNSUPPORTED_EFFECT", "effects are not supported")
      | Exception ->
          ("VERO_UNSUPPORTED_EXCEPTION", "exceptions are not supported")
      | Array -> ("VERO_UNSUPPORTED_ARRAY", "arrays are not supported")
      | Object -> ("VERO_UNSUPPORTED_OBJECT", "objects are not supported")
      | First_class_module ->
          ( "VERO_UNSUPPORTED_FIRST_CLASS_MODULE",
            "first-class modules are not supported" )
      | Concurrency ->
          ("VERO_UNSUPPORTED_CONCURRENCY", "concurrency is not supported")
      | Nonlinear_multiplication ->
          ( "VERO_UNSUPPORTED_NONLINEAR_MULTIPLICATION",
            "multiplication between symbolic values is not supported" )
      | Structural_aggregate_equality ->
          ( "VERO_UNSUPPORTED_AGGREGATE_EQUALITY",
            "structural equality over aggregates is not supported" )
      | Wrapping_arithmetic ->
          ( "VERO_UNSUPPORTED_WRAPPING_ARITHMETIC",
            "wrapping, division, remainder, bitwise, and shift operations are not supported"
          )
      | Aggregate ->
          ( "VERO_UNSUPPORTED_AGGREGATE",
            "records and variants are reserved for aggregate lowering" )
      | Mutation ->
          ("VERO_UNSUPPORTED_MUTATION", "mutation is reserved for unique-state lowering")
      | Unsupported_pattern ->
          ("VERO_UNSUPPORTED_PATTERN", "pattern is outside the pure SST subset")
      | Unsupported_expression ->
          ("VERO_UNSUPPORTED_EXPRESSION", "expression is outside the pure SST subset")
      | Malformed_ghost_call ->
          ( "VERO_MALFORMED_GHOST_CALL",
            "resolved ghost call does not have the PPX-generated shape" )
      | Callback_authentication ->
          ( "VERO_CALLBACK_AUTHENTICATION",
            "callback identity, shape, certificate, session, or call edge is not authenticated"
          )
      | Callback_policy ->
          ( "VERO_CALLBACK_POLICY",
            "callback use is outside the local verified-callback policy" )
      | Callback_contract ->
          ( "VERO_CALLBACK_CONTRACT",
            "verified callbacks require explicit requires and ensures clauses" )
      | Quantifier_authentication ->
          ( "VERO_QUANTIFIER_AUTHENTICATION",
            "quantifier identity, carrier, binder, or lexical owner is not authenticated"
          )
      | Quantifier_trigger ->
          ( "VERO_QUANTIFIER_TRIGGER",
            "forall requires one explicit application-headed trigger covering its binder"
          )
      | Quantifier_type ->
          ( "VERO_QUANTIFIER_TYPE",
            "quantifier binder type is outside the supported first-order immutable subset"
          )
      | Unsupported_logical_quantifier ->
          ( "VERO_UNSUPPORTED_LOGICAL_QUANTIFIER",
            "forall and exists are authenticated but unavailable here" ))

let make classification span =
  let code, message = code_and_message classification in
  { classification; code; message; span }
