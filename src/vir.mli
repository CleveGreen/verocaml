type span = Diagnostic.span

type aggregate_type = {
  aggregate_type_index : int;
  aggregate_type_name : string;
  aggregate_type_arguments : Parametric_type.t list;
}

type sort =
  | Integer
  | Boolean
  | Aggregate of aggregate_type
  | Parametric of Parametric_type.binder

type symbol_role = Input | Local | Result

type symbol = {
  symbol_id : int;
  source_name : string;
  sort : sort;
  role : symbol_role;
  span : span;
}

type rank_domain

type parametric_term = {
  parametric_sort : Parametric_type.binder;
  parametric_desc : parametric_term_desc;
}

and parametric_term_desc =
  | Parametric_symbol of symbol
  | Parametric_selector of selector * aggregate_term
  | Parametric_conditional of boolean_term * parametric_term * parametric_term
  | Parametric_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and integer_term =
  | Integer_constant of Z.t
  | Integer_symbol of symbol
  | Integer_add of integer_term * integer_term
  | Integer_subtract of integer_term * integer_term
  | Integer_negate of integer_term
  | Integer_multiply of integer_term * integer_term
  | Integer_multiply_constant of Z.t * integer_term
  | Integer_absolute_value of integer_term
  | Integer_conditional of boolean_term * integer_term * integer_term
  | Integer_rank_project of rank_domain * aggregate_term
  | Aggregate_tag of aggregate_type * aggregate_term
  | Integer_selector of selector * aggregate_term
  | Integer_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Integer_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and aggregate_term = {
  aggregate_type : aggregate_type;
  aggregate_desc : aggregate_term_desc;
}

and aggregate_term_desc =
  | Aggregate_symbol of symbol
  | Aggregate_imported_model_application of {
      callee : Sst.function_id;
      callable_path : string;
      callable_uid : string;
      provider_unit : string;
      provider_interface : string;
      provider_source : string;
      provider_family : string;
      provider_import : string;
      summary_digest : string;
      closure_digest : string;
      call_snapshot : string;
      registration_snapshot : string;
      invocation_ordinal : int;
      application_identity : Imported_callable.aggregate_application_identity;
      arguments : recursive_spec_argument list;
      result_type : aggregate_type;
      span : span;
    }
  | Aggregate_selector of selector * aggregate_term
  | Aggregate_constructor of {
      constructor : Sst.constructor_id;
      arguments : recursive_spec_argument list;
    }
  | Aggregate_record of {
      record_type : Sst.type_id;
      fields : (Sst.field_id * recursive_spec_argument) list;
    }
  | Aggregate_conditional of boolean_term * aggregate_term * aggregate_term
  | Aggregate_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      result_type : aggregate_type;
      span : span;
      application_identity : Recursive_spec_application_identity.t;
    }
  | Aggregate_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and selector = {
  selector_domain : aggregate_type;
  selector_range : sort;
  selector_namespace : string;
  selector_index : int;
  selector_name : string;
  selector_path : int list;
}

and recursive_spec_argument =
  | Recursive_integer_argument of integer_term
  | Recursive_boolean_argument of boolean_term
  | Recursive_aggregate_argument of aggregate_term
  | Recursive_parametric_argument of parametric_term

and callback_application = {
  callback : Sst.callback_binding;
  arguments : recursive_spec_argument list;
  call_span : span;
}

and boolean_quantifier = private {
  boolean_quantifier_schema : Logic_quantifier_private.vector;
  boolean_quantifier_binders : symbol list;
  boolean_quantifier_body : boolean_term;
  boolean_quantifier_trigger : application_term option;
}

and application_term =
  | Integer_application of integer_term
  | Boolean_application of boolean_term
  | Aggregate_application of aggregate_term
  | Parametric_application of parametric_term

and boolean_term =
  | Logical_adt_schema of Logical_adt_schema_private.t list
  | Boolean_constant of bool
  | Boolean_symbol of symbol
  | Boolean_not of boolean_term
  | Boolean_and of boolean_term * boolean_term
  | Boolean_or of boolean_term * boolean_term
  | Forall_term of boolean_quantifier
  | Exists_term of boolean_quantifier
  | Integer_compare of comparison * integer_term * integer_term
  | Boolean_equal of boolean_term * boolean_term
  | Boolean_not_equal of boolean_term * boolean_term
  | Boolean_selector of selector * aggregate_term
  | Aggregate_equal of aggregate_term * aggregate_term
  | Parametric_equal of parametric_term * parametric_term
  | Boolean_invariant_application of {
      invariant_id : string;
      model : Sst.function_id;
      predicate : Sst.function_id;
      value : aggregate_term;
    }
  | Boolean_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Boolean_specification_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Boolean_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t
  | Callback_requires of callback_application
  | Callback_ensures of {
      application : callback_application;
      result : recursive_spec_argument;
    }

and comparison =
  | Equal
  | Not_equal
  | Less_than
  | Less_or_equal
  | Greater_than
  | Greater_or_equal

type spec_function_term = parametric_term

val symbolic_application_arguments :
  application_term -> recursive_spec_argument list option

val symbolic_application :
  aggregate_type:(Parametric_type.t -> aggregate_type option) ->
  recursive_spec_argument Symbolic_application_private.t ->
  (application_term, string) result

val make_boolean_quantifier :
  sort_of_type:(Parametric_type.t -> (sort, string) result) ->
  schema:Logic_quantifier_private.vector ->
  binders:symbol list ->
  body:boolean_term ->
  trigger:application_term option ->
  (boolean_quantifier, string) result

val boolean_term_symbol_ids : boolean_term -> int list
val application_term_symbol_ids : application_term -> int list

type checked_operation =
  | Add
  | Subtract
  | Negate
  | Multiply
  | Multiply_constant of Z.t
  | Successor
  | Predecessor
  | Absolute_value

type violated_bound = Lower_bound | Upper_bound

type function_ref = {
  function_index : int;
  function_name : string;
}

type invariant_transition_kind =
  | Direct_root_transition
  | Nested_transition
  | Rebase_transition

type invariant_boundary =
  | Constructor_establishment
  | Transition_preservation of {
      transition_kind : invariant_transition_kind;
      root_binding_id : int;
      pre_version : int;
      successor_version : int;
    }
  | Call_argument of {
      callee : function_ref;
      argument_index : int;
    }
  | Call_result of { callee : function_ref }
  | Function_return
  | Shared_invariant_close of {
      entry_epoch : int;
      final_epoch : int;
    }
  | Terminal_observation of {
      operation : function_ref;
      snapshot : bool;
    }

type vc_kind =
  | Arithmetic_safety of {
      operation : checked_operation;
      mathematical_result : integer_term;
      violated_bound : violated_bound;
    }
  | Assertion of { assertion_ordinal : int }
  | Local_assertion of { local_assertion_ordinal : int }
  | Postcondition of {
      postcondition_ordinal : int;
      declaration_span : span;
    }
  | Call_precondition of {
      callee : function_ref;
      precondition_ordinal : int;
      declaration_span : span;
      call_span : span;
    }
  | Callback_precondition of {
      callback : Sst.callback_binding;
      call_span : span;
    }
  | Invariant_validity of {
      invariant_id : string;
      abstract_type : aggregate_type;
      model : function_ref;
      predicate : function_ref;
      operation : function_ref;
      boundary : invariant_boundary;
    }
  | Entry_measure_nonnegative of { declaration_span : span }
  | Recursive_call_measure_nonnegative of {
      callee : function_ref;
      declaration_span : span;
      call_span : span;
    }
  | Recursive_call_strict_descent of {
      callee : function_ref;
      declaration_span : span;
      call_span : span;
    }

type obligation = {
  obligation_index : int;
  function_ref : function_ref;
  kind : vc_kind;
  span : span;
  assumptions : boolean_term list;
  required_preceding_safety : boolean_term list;
  path_condition : boolean_term list;
  goal : boolean_term;
  projection_symbols : symbol list;
}

type result_value =
  | Unit_result
  | Integer_result of symbol
  | Boolean_result of symbol
  | Tuple_result of result_value list
  | Aggregate_result of symbol
  | Parametric_result of symbol

type trusted_summary_use =
  | Trusted_external_specification_use of {
      target : function_ref;
      wrapper : function_ref;
      target_span : span;
      wrapper_span : span;
      witness_span : span;
      call_span : span;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_target_specification_use of {
      consumer_artifact_digest : string;
      target_unit : string;
      target_interface_digest : string;
      import_crc : string;
      canonical_path : string;
      value_uid : string;
      callable_abi_digest : string;
      wrapper : function_ref;
      target_span : span;
      wrapper_span : span;
      witness_span : span;
      call_span : span;
      summary_digest : string;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_body_use of {
      function_ref : function_ref;
      mode : Sst.verification_mode;
      call_form : Sst.call_form;
      broadcast_use : bool;
      declaration_span : span;
      witness_span : span;
      call_span : span;
      requires_count : int;
      ensures_count : int;
    }

type reached_callback_call = {
  application : callback_application;
  result : result_value;
  ensures : boolean_term;
}

type trusted_external_body_declaration = {
  function_ref : function_ref;
  mode : Sst.verification_mode;
  declaration_span : span;
  witness_span : span;
  requires_count : int;
  ensures_count : int;
}

type exit = {
  assumptions : boolean_term list;
  path_condition : boolean_term list;
  result : result_value;
  projection_symbols : symbol list;
  trusted_summary_uses : trusted_summary_use list;
}

type shared_scalar_heap_read = {
  shared_read_field : Sst.field_id;
  shared_read_path_id : int;
  shared_read_epoch : int;
  shared_read_location : aggregate_term;
  shared_read_term : integer_term;
  shared_read_entry_view : bool;
}

type shared_scalar_heap_write = {
  shared_write_transition : Sst.shared_scalar_heap_transition;
  shared_write_location : aggregate_term;
  shared_write_value : integer_term;
}

type function_execution = {
  function_ref : function_ref;
  mode : Sst.verification_mode;
  body_provenance : Sst.body_provenance;
  policy : Sst.verification_policy;
  trusted_summary_uses : trusted_summary_use list;
  reached_callback_calls : reached_callback_call list;
  owned_tree_transitions : Sst.owned_tree_transition list;
  shared_scalar_heap_reads : shared_scalar_heap_read list;
  shared_scalar_heap_writes : shared_scalar_heap_write list;
  obligations : obligation list;
  exits : exit list;
}

type rank_term

type rank_fact =
  | Ground_rank_base of {
      constructor : Sst.constructor_id;
      rank : Z.t;
    }
  | Constructor_rank_nonnegative of {
      constructor : Sst.constructor_id;
    }
  | Positive_child_rank_smaller of {
      constructor : Sst.constructor_id;
      field : Sst.field_id;
      child_path : int list;
      child_type : aggregate_type;
    }

type program = {
  policy : Sst.verification_policy;
  rank_domains : rank_domain list;
  trusted_external_body_declarations :
    trusted_external_body_declaration list;
  functions : function_execution list;
}

val rank_domains_of_validated :
  Sst_validation.validated_program -> rank_domain list
val rank_domain_id : rank_domain -> string
val rank_domain_version : rank_domain -> string
val rank_domain_digest : rank_domain -> string
val rank_domain_component : rank_domain -> aggregate_type list
val rank_domain_facts : rank_domain -> rank_fact list
val rank_selector_is_positive_child : rank_domain -> selector -> bool
val rank_project :
  rank_domain -> aggregate_term -> (rank_term, string) result
val rank_term_to_string : rank_term -> string

val integer_range : integer_term -> boolean_term list
val obligation_has_recursive_specification : obligation -> bool
val recursive_spec_argument_has_recursive_specification :
  recursive_spec_argument -> bool
val obligation_aggregate_recursive_specifications :
  obligation ->
  ((Sst.function_id * span * Recursive_spec_application_identity.t) list,
   string)
  result
val obligation_aggregate_types : obligation -> aggregate_type list
val obligation_has_structural_rank : obligation -> bool
val obligation_has_logical_aggregate_construction : obligation -> bool
val obligation_rank_domains : obligation -> rank_domain list
val integer_term_to_string : integer_term -> string
val boolean_term_to_string : boolean_term -> string
val aggregate_term_to_string : aggregate_term -> string
val specification_application_name :
  Sst.function_id ->
  Parametric_type.t list ->
  recursive_spec_argument list ->
  string
val imported_model_application_snapshot :
  callee:Sst.function_id ->
  arguments:recursive_spec_argument list ->
  result_type:aggregate_type ->
  span:span ->
  string
val to_string : program -> string
