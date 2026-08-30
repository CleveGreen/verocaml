type position = {
  line : int;
  column : int;
}

let () =
  Delator.init ();
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> Delator.set_default_level Delator.Warn
  | Some _ -> ()

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
  | Executable_function_in_specification of { function_name : string }
  | Unannotated_erased_call of {
      caller_name : string;
      callee_name : string;
      callee_mode : string;
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

let location_of_span span =
  let position position =
    {
      Lexing.pos_fname = span.file;
      pos_lnum = position.line;
      pos_bol = 0;
      pos_cnum = position.column;
    }
  in
  {
    Location.loc_start = position span.start_pos;
    loc_end = position span.end_pos;
    loc_ghost = false;
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
  | Executable_function_in_specification { function_name } ->
      ( "VERO_EXEC_IN_SPEC",
        Printf.sprintf
          "Executable function %S cannot be used in a specification."
          function_name )
  | Unannotated_erased_call { caller_name; callee_name; callee_mode } ->
      ( "VERO_ERASED_CALL",
        Printf.sprintf
          "Executable function %S uses %s-only function %S without marking that call as ghost."
          caller_name callee_mode callee_name )
  | Invalid_imported_specification detail ->
      ( "VERO_DEPENDENCY",
        "Imported specification authentication failed: " ^ detail )
  | Invalid_semantic_program { function_name; detail } ->
      let subject =
        Option.fold ~none:"this verification unit"
          ~some:(fun name -> Printf.sprintf "function %S" name)
          function_name
      in
      ( "VERO_INVALID_PROGRAM",
        Printf.sprintf "VeroCaml could not validate %s: %s" subject detail )
  | Unsupported_construct construct -> (
      match construct with
      | Unsupported_type ->
          ( "VERO_UNSUPPORTED_TYPE",
            "This type is not supported in verified code." )
      | Unsupported_structure_item ->
          ( "VERO_UNSUPPORTED_STRUCTURE_ITEM",
            "This top-level declaration is not supported in a verified compilation unit."
          )
      | Unsupported_top_level_binding ->
          ( "VERO_UNSUPPORTED_TOP_LEVEL_BINDING",
            "This top-level value is not a supported function definition." )
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
          ( "VERO_UNSUPPORTED_PATTERN",
            "VeroCaml cannot verify this pattern." )
      | Unsupported_expression ->
          ( "VERO_UNSUPPORTED_EXPRESSION",
            "VeroCaml cannot verify this expression." )
      | Malformed_ghost_call ->
          ( "VERO_MALFORMED_GHOST_CALL",
            "This retained ghost call does not match the output of the VeroCaml PPX."
          )
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

let default_submessages = function
  | Executable_function_in_specification { function_name } ->
      [
        Hint
          (Printf.sprintf
             "If %S is a logical definition, add [@@verocaml.spec] after its definition."
             function_name);
      ]
  | Unannotated_erased_call { caller_name; callee_name; _ } ->
      [
        Hint
          (Printf.sprintf
             "If %S is a proof, add [@@verocaml.proof] after its definition."
             caller_name);
        Hint
          (Printf.sprintf
             "Otherwise, explicitly mark the call to %S as ghost code."
             callee_name);
      ]
  | Invalid_imported_specification _ ->
      [
        Hint
          "Rebuild the provider and consumer together, and ensure the consumer directly imports every specification provider and external target it uses.";
      ]
  | Unsupported_construct Unsupported_type ->
      [
        Hint
          "Use a supported scalar or immutable algebraic data type, or provide an external type specification.";
      ]
  | Unsupported_construct Unsupported_structure_item ->
      [
        Hint
          "Move this declaration to an unverified dependency, or rewrite it as a supported type or function declaration.";
      ]
  | Unsupported_construct Unsupported_top_level_binding ->
      [
        Hint
          "Define a direct function, or move the value to an unverified dependency.";
      ]
  | Unsupported_construct Unsupported_pattern ->
      [ Hint "Rewrite the match using supported variable and constructor patterns." ]
  | Unsupported_construct Unsupported_expression ->
      [ Hint "Rewrite this expression using supported pure VeroCaml operations." ]
  | Unsupported_construct Malformed_ghost_call ->
      [
        Hint
          "Recompile the source with the matching VeroCaml PPX and verify the newly generated CMT.";
      ]
  | Unsupported_target _ | Unsupported_input _ | Malformed_input
  | Incompatible_magic | Input_io_error | Invalid_recursive_rank _
  | Invalid_broadcast _ | Invalid_symbolic_declaration _
  | Invalid_symbolic_application _ | Invalid_symbolic_authentication _
  | Invalid_semantic_program _ | Unsupported_construct _ ->
      []

let make classification span =
  let code, message = code_and_message classification in
  [%log.info "diagnostic created"
    ~code:(Delator.Field.string code)
    ~file:(Delator.Field.string span.file)
    ~start_line:(Delator.Field.int span.start_pos.line)
    ~start_column:(Delator.Field.int span.start_pos.column)
    ~end_line:(Delator.Field.int span.end_pos.line)
    ~end_column:(Delator.Field.int span.end_pos.column)];
  {
    classification;
    code;
    message;
    span;
    submessages = default_submessages classification;
  }

let with_message message diagnostic = { diagnostic with message }

let with_hint hint diagnostic =
  {
    diagnostic with
    submessages = diagnostic.submessages @ [ Hint hint ];
  }

let with_note ~span message diagnostic =
  {
    diagnostic with
    submessages = diagnostic.submessages @ [ Note { span; message } ];
  }

let report diagnostic =
  let main =
    Location.msg ~loc:(location_of_span diagnostic.span) "[%s] %s"
      diagnostic.code diagnostic.message
  in
  let sub =
    List.map
      (function
        | Hint message -> Location.msg "%s" ("Hint: " ^ message)
        | Note { span; message } ->
            Location.msg ~loc:(location_of_span span) "%s" message)
      diagnostic.submessages
  in
  { Location.kind = Location.Report_error; main; sub }
