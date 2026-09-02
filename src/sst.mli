type span = Diagnostic.span

type type_id = Parametric_type.type_id = {
  type_index : int;
  type_name : string;
}

type typ = Parametric_type.t =
  | Unit
  | Bool
  | Int
  | Mathematical_int
  | Tuple of (string option * typ) list
  | Aggregate of type_id
  | Parameter of Parametric_type.binder
  | Application of Parametric_type.constructor * typ list

type constructor_id = {
  constructor_type : type_id;
  constructor_index : int;
  constructor_name : string;
}

type field_owner =
  | Record_owner of type_id
  | Constructor_owner of constructor_id

type field_id = {
  field_owner : field_owner;
  field_index : int;
  field_name : string;
}

type field_definition = {
  field_id : field_id;
  field_type : typ;
  field_mutability : field_mutability;
  field_modalities : field_modalities;
  span : span;
}

and field_mutability = Immutable_field | Mutable_field

and field_uniqueness_modality =
  | Preserve_uniqueness
  | Force_unique
  | Force_aliased

and field_linearity_modality =
  | Preserve_linearity
  | Force_once
  | Force_many

and field_modalities = {
  uniqueness_modality : field_uniqueness_modality;
  linearity_modality : field_linearity_modality;
}

type constructor_definition = {
  constructor_id : constructor_id;
  constructor_fields : field_definition list;
  span : span;
}

type type_kind =
  | Record_definition of field_definition list
  | Variant_definition of constructor_definition list

type function_id = {
  function_index : int;
  function_name : string;
}

type same_cmt_abstraction_evidence = {
  evidence_id : string;
  abstract_signature_type : type_id;
  hidden_implementation_type : type_id;
  signature_module_identity : resolved_source_identity;
  implementation_module_identity : resolved_source_identity;
  abstract_signature_type_identity : resolved_source_identity;
  hidden_implementation_type_identity : resolved_source_identity;
  constraint_span : span;
  declaration_spans : span list;
  public_surface : abstract_public_operation list;
  owned_tree_prerequisite : owned_tree_prerequisite option;
  authentication_token : unit ref;
}

and resolved_source_identity = {
  source_name : string;
  resolved_identifier : string;
}

and abstract_public_operation = {
  public_function_index : int;
  public_function_name : string;
  public_role : abstract_operation_role;
  public_span : span;
}

and abstract_operation_role =
  | Abstract_constructor
  | Abstract_model
  | Current_model
  | Abstract_invariant
  | Terminal_read
  | Current_terminal_read
  | Terminal_snapshot
  | Unique_transition
  | Shared_invariant_transition

and owned_tree_prerequisite = {
  owned_root : type_id;
  recursive_edges : field_id list;
  construction_authority : closed_construction_authority;
}

and frozen_spine_prerequisite = {
  frozen_root : type_id;
  frozen_link : type_id;
  frozen_payload_field : field_id;
  frozen_edge_field : field_id;
  frozen_nil_constructor : constructor_id;
  frozen_next_constructor : constructor_id;
  frozen_next_child_field : field_id;
  frozen_helper : function_id;
  frozen_model : function_id;
  frozen_result_type : type_id;
  frozen_end_constructor : constructor_id;
  frozen_more_constructor : constructor_id;
  frozen_more_head_field : field_id;
  frozen_more_tail_field : field_id;
  frozen_invariant : function_id;
  frozen_constructor : function_id;
  frozen_mutator : function_id;
  frozen_terminal : function_id;
}

and closed_construction_authority =
  | Local_first_order_closed_construction

val register_frozen_spine_prerequisite :
  same_cmt_abstraction_evidence -> frozen_spine_prerequisite -> unit

val frozen_spine_prerequisite :
  same_cmt_abstraction_evidence -> frozen_spine_prerequisite option

type abstraction_evidence =
  | Incomplete_abstraction_evidence of {
      evidence_id : string;
      evidence_span : span;
    }
  | Proposed_same_cmt_abstraction of same_cmt_abstraction_evidence
  | Authenticated_same_cmt_abstraction of same_cmt_abstraction_evidence

type representation_visibility =
  | Revealed
  | Abstract_with_evidence of abstraction_evidence

type type_definition = {
  type_id : type_id;
  type_kind : type_kind;
  representation : representation_visibility;
  span : span;
}

type uniqueness =
  | Definitely_unique
  | Definitely_aliased

type binding = {
  id : int;
  name : string;
  typ : typ;
  uniqueness : uniqueness;
  span : span;
}

type callback_binding = {
  callback_id : int;
  callback_name : string;
  callback_shape : Callback_shape_private.t;
  callback_certificate : Callback_certificate_private.t;
  callback_span : span;
}

type mutation_provenance = {
  root : binding;
  binding_pattern_uniqueness : uniqueness;
  identifier_use_uniqueness : uniqueness;
  field_is_local : bool;
  field_is_public : bool;
  field_is_mutable : bool;
}

type owned_tree_policy = Functional_owned_tree_no_heap

type owned_tree_path_step =
  | Owned_tree_field of field_id
  | Owned_tree_constructor of constructor_id

type owned_tree_cursor = {
  cursor_binding : binding;
  root : binding;
  root_version : int;
  guarded_path : owned_tree_path_step list;
}

type owned_tree_reconstruction_layer =
  | Reconstruct_record of {
      record_type : type_id;
      changed_field : field_id;
      preserved_fields : field_id list;
    }
  | Reconstruct_constructor of constructor_id

type owned_tree_rhs_provenance =
  | Ground_owned_tree_value
  | Guarded_descendant_move of owned_tree_cursor

type owned_tree_transition = {
  policy : owned_tree_policy;
  root : binding;
  pre_version : int;
  successor_version : int;
  cursor : owned_tree_cursor option;
  target_field : field_id;
  target_mutability : field_mutability;
  target_modalities : field_modalities;
  rhs_provenance : owned_tree_rhs_provenance;
  reconstruction : owned_tree_reconstruction_layer list;
  invalidated_cursor_ids : int list;
}

type shared_scalar_heap_policy = Bounded_shared_scalar_heap_v1

type shared_scalar_heap_transition = {
  shared_policy : shared_scalar_heap_policy;
  shared_function_index : int;
  shared_function_name : string;
  shared_path_id : int;
  shared_record_type : type_id;
  shared_formal_roots : binding list;
  shared_target : binding;
  shared_canonical_root : binding;
  shared_alias_chain : binding list;
  shared_target_field : field_id;
  shared_predecessor_epoch : int;
  shared_successor_epoch : int;
}

type pattern = {
  pattern_desc : pattern_desc;
  typ : typ;
  span : span;
}

and pattern_desc =
  | Wildcard
  | Bind of binding
  | Owned_tree_cursor_pattern of owned_tree_cursor
  | Int_pattern of Z.t
  | Bool_pattern of bool
  | Unit_pattern
  | Tuple_pattern of (string option * pattern) list
  | Record_pattern of (field_id * pattern) list
  | Constructor_pattern of constructor_id * pattern list
  | Or_pattern of pattern * pattern

type checked_arithmetic =
  | Add
  | Subtract
  | Negate
  | Multiply
  | Multiply_constant of Z.t
  | Successor
  | Predecessor
  | Absolute_value

type comparison =
  | Equal
  | Not_equal
  | Less_than
  | Less_or_equal
  | Greater_than
  | Greater_or_equal

type boolean_operation = And | Or

type verification_mode = Spec | Proof | Exec
type instance_mode = Exec_instance | Tracked_instance | Ghost_instance
type expression_stage = Logical | Proof_stage | Runtime

type call_form =
  | Specification_call
  | Proof_call
  | Exec_call
  | Unclassified_call

type verification_policy =
  | Default_linear_z3
  | Default_linear_cvc5
  | Nonlinear_z3

type expression = {
  expression_desc : expression_desc;
  typ : typ;
  span : span;
}

and call_argument =
  | Value_argument of {
      label : string option;
      value : expression;
    }
  | Callback_argument of {
      label : string option;
      callback : callback_binding;
    }

and callback_application = {
  callback : callback_binding;
  arguments : (string option * expression) list;
  callback_span : span;
}

and quantifier = {
  quantifier_metadata : Logic_quantifier_private.t;
  quantifier_binder : binding;
  quantifier_body : expression;
  quantifier_trigger : expression option;
}

and expression_desc =
  | Int_constant of Z.t
  | Bool_constant of bool
  | Unit_constant
  | Variable of {
      binding : binding;
      use_uniqueness : uniqueness;
    }
  | Tuple_value of (string option * expression) list
  | Record_value of {
      record_type : type_id;
      fields : (field_id * expression) list;
    }
  | Constructor_value of {
      constructor : constructor_id;
      arguments : expression list;
    }
  | Field_read of {
      record : expression;
      field : field_id;
    }
  | Field_write of {
      provenance : mutation_provenance;
      field : field_id;
      value : expression;
      transition : owned_tree_transition option;
    }
  | Shared_scalar_field_write of {
      provenance : mutation_provenance;
      field : field_id;
      value : expression;
      transition : shared_scalar_heap_transition;
    }
  | Owned_tree_nested_write of {
      transition : owned_tree_transition;
      value : expression;
    }
  | Owned_tree_rebase of { transition : owned_tree_transition }
  | Let_mutable of binding * expression * expression
  | Mutable_read of binding
  | Mutable_write of {
      provenance : mutation_provenance;
      value : expression;
    }
  | Let of (pattern * expression) list * expression
  | Sequence of expression * expression
  | If of expression * expression * expression option
  | Match of expression * case list
  | Lift_runtime_int of expression
  | Checked_arithmetic of checked_arithmetic * expression list
  | Compare of comparison * expression * expression
  | Boolean_not of expression
  | Boolean_binary of boolean_operation * expression * expression
  | Forall of quantifier
  | Exists of quantifier
  | Direct_call of {
      call_form : call_form;
      callee : function_id;
      type_arguments : typ list;
      arguments : call_argument list;
      recursive : bool;
    }
  | Symbolic_application of expression Symbolic_application_private.t
  | Callback_call of callback_application
  | Callback_requires of callback_application
  | Callback_ensures of {
      application : callback_application;
      result : expression;
    }
  | Optional_absent
  | Optional_present of expression
  | Optional_forward of expression
  | Reveal of function_id
  | Reveal_with_fuel of {
      function_id : function_id;
      literal_depth : string;
    }
  | Use_type_invariant of {
      use_id : string;
      value : expression;
    }
  | Local_assert of {
      assertion_ordinal : int;
      predicate : expression;
    }
  | Proof_region of expression
  | Old of expression

and case = {
  case_pattern : pattern;
  case_guard : expression option;
  case_body : expression;
  case_span : span;
}

type optional_default = {
  optional_pattern : pattern;
  optional_expression : expression;
}

type value_parameter = {
  label : string option;
  pattern : pattern;
  optional_default : optional_default option;
}

type callback_formal = {
  label : string option;
  binding : callback_binding;
}

type parameter =
  | Value_parameter of value_parameter
  | Callback_parameter of callback_formal

val require_value_parameter : parameter -> value_parameter
val require_value_argument : call_argument -> string option * expression

type staged_expression = {
  stage : expression_stage;
  expression : expression;
}

type predicate_clause = {
  clause_index : int;
  predicate : staged_expression;
  span : span;
}

type ensures_clause = {
  clause_index : int;
  binder : pattern option;
  predicate : staged_expression;
  span : span;
}

type contracts = {
  requires : predicate_clause list;
  ensures : ensures_clause list;
  decreases : predicate_clause list;
  assertions : predicate_clause list;
}

type body_provenance =
  | Authenticated_typedtree of {
      source_file : string;
      declaration_span : span;
    }
  | Raw_semantic_body of span

type trusted_external_body_provenance =
  | Authenticated_external_body of {
      source_file : string;
      declaration_span : span;
      witness_span : span;
    }
  | Raw_external_body of span

type target_link =
  | Same_unit_target of {
      wrapper : function_id;
      target : function_id;
      target_span : span;
      declaration_span : span;
      witness_span : span;
    }
  | Imported_unverified_target of {
      wrapper : function_id;
      consumer_artifact_digest : string;
      target_unit : string;
      target_interface_digest : string;
      import_crc : string;
      canonical_path : string;
      value_uid : string;
      callable_abi_digest : string;
      summary_digest : string;
      target_span : span;
      declaration_span : span;
      witness_span : span;
    }
  | Unresolved_target of {
      target_name : string;
      witness_span : span;
    }

type body_disposition =
  | Checked_exec of {
      body : staged_expression;
      provenance : body_provenance;
    }
  | Spec_definition of staged_expression
  | Recursive_spec_definition of {
      body : staged_expression;
      visibility : [ `Opaque | `Revealed ];
      provenance : body_provenance;
    }
  | Proof_body of {
      body : staged_expression;
      provenance : body_provenance;
    }
  | External_specification of target_link
  | Trusted_external_spec_target of target_link
  | Trusted_external_body of trusted_external_body_provenance
  | Symbolic_declaration of Symbolic_application_private.declaration

type function_definition = {
  function_id : function_id;
  type_binders : Parametric_type.binder list;
  mode : verification_mode;
  recursive : bool;
  parameters : parameter list;
  contracts : contracts;
  body : body_disposition;
  policy : verification_policy;
  result_type : typ;
  returns_unique_parameter : int option;
  span : span;
}

type program = {
  policy : verification_policy;
  parametric_adts : Parametric_adt.t list;
  types : type_definition list;
  functions : function_definition list;
}

val string_of_type : typ -> string
val empty_contracts : contracts
val function_shared_scalar_transitions :
  function_definition -> shared_scalar_heap_transition list
val map_pattern_types : (typ -> typ) -> pattern -> pattern
val map_expression_types : (typ -> typ) -> expression -> expression
val symbolic_application_arguments :
  expression_desc -> expression list option
val map_symbolic_application_arguments :
  (expression -> expression) -> expression_desc -> expression_desc option
val substitute_contracts :
  Parametric_type.binder list ->
  Parametric_type.t list ->
  contracts ->
  (contracts, string) result
val value_parameter : parameter -> value_parameter option
val value_argument : call_argument -> (string option * expression) option
val value_parameters : parameter list -> value_parameter list option
val value_arguments :
  call_argument list -> (string option * expression) list option
val call_arguments :
  (string option * expression) list -> call_argument list
val to_string : program -> string
