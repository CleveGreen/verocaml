type span = Diagnostic.span

type type_id = Parametric_type.type_id = {
  type_index : int;
  type_name : string;
}

type typ = Parametric_type.t =
  | Unit
  | Bool
  | Int
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

and field_linearity_modality = Preserve_linearity
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
  function_name : string
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

and closed_construction_authority = Local_first_order_closed_construction

let frozen_spine_prerequisites :
    (unit ref * frozen_spine_prerequisite) list ref =
  ref []

let register_frozen_spine_prerequisite evidence prerequisite =
  if
    List.exists
      (fun (token, _) -> token == evidence.authentication_token)
      !frozen_spine_prerequisites
  then invalid_arg "frozen-spine prerequisite is already registered"
  else
    frozen_spine_prerequisites :=
      (evidence.authentication_token, prerequisite)
      :: !frozen_spine_prerequisites

let frozen_spine_prerequisite evidence =
  List.find_map
    (fun (token, prerequisite) ->
      if token == evidence.authentication_token then Some prerequisite
      else None)
    !frozen_spine_prerequisites

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

type uniqueness = Definitely_unique
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
  span : span
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
      use_uniqueness : uniqueness
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
      field : field_id
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
      value : expression
    }
  | Let of (pattern * expression) list * expression
  | Sequence of expression * expression
  | If of expression * expression * expression option
  | Match of expression * case list
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
      literal_depth : string
    }
  | Use_type_invariant of {
      use_id : string;
      value : expression
    }
  | Local_assert of {
      assertion_ordinal : int;
      predicate : expression
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

let require_value_parameter = function
  | Value_parameter parameter -> parameter
  | Callback_parameter _ ->
      invalid_arg "callback formal reached a value-only ABI"

let require_value_argument = function
  | Value_argument { label; value } -> (label, value)
  | Callback_argument _ ->
      invalid_arg "callback actual reached a value-only ABI"

type staged_expression = {
  stage : expression_stage;
  expression : expression
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
      declaration_span : span
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
      witness_span : span
    }

type body_disposition =
  | Checked_exec of {
      body : staged_expression;
      provenance : body_provenance
    }
  | Spec_definition of staged_expression
  | Recursive_spec_definition of {
      body : staged_expression;
      visibility : [ `Opaque | `Revealed ];
      provenance : body_provenance;
    }
  | Proof_body of {
      body : staged_expression;
      provenance : body_provenance
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

let empty_contracts =
  { requires = []; ensures = []; decreases = []; assertions = [] }

let string_of_type = Parametric_type.to_string

let owner_to_string = function
  | Record_owner type_id ->
      Printf.sprintf "type-%s#%d" type_id.type_name type_id.type_index
  | Constructor_owner constructor ->
      Printf.sprintf "constructor-%s#%d.%s#%d"
        constructor.constructor_type.type_name
        constructor.constructor_type.type_index constructor.constructor_name
        constructor.constructor_index

let field_to_string field =
  Printf.sprintf "%s.%s#%d" (owner_to_string field.field_owner)
    field.field_name field.field_index

let constructor_to_string constructor =
  Printf.sprintf "%s#%d.%s#%d" constructor.constructor_type.type_name
    constructor.constructor_type.type_index constructor.constructor_name
    constructor.constructor_index

let owned_tree_path_to_string path =
  path
  |> List.map (function
       | Owned_tree_field field -> "field(" ^ field_to_string field ^ ")"
       | Owned_tree_constructor constructor ->
           "constructor(" ^ constructor_to_string constructor ^ ")")
  |> String.concat "/"

let uniqueness_modality_to_string = function
  | Preserve_uniqueness -> "preserve"
  | Force_unique -> "force-unique"
  | Force_aliased -> "force-aliased"

let linearity_modality_to_string = function
  | Preserve_linearity -> "preserve"
  | Force_once -> "force-once"
  | Force_many -> "force-many"

let span_to_string span =
  let open Diagnostic in
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let binding_to_string binding =
  Printf.sprintf "%s#%d:%s%s" binding.name binding.id
    (string_of_type binding.typ)
    (match (binding.typ, binding.uniqueness) with
    | ( Aggregate _ | Parameter _ | Application _), Definitely_unique -> " uniqueness=unique"
    | (Unit | Bool | Int | Tuple _), Definitely_unique
    | _, Definitely_aliased ->
        "")

let line buffer indent format =
  Printf.ksprintf
    (fun text ->
      Buffer.add_string buffer (String.make indent ' ');
      Buffer.add_string buffer text;
      Buffer.add_char buffer '\n')
    format

let rec print_pattern buffer indent (pattern : pattern) =
  let suffix =
    Printf.sprintf " : %s @ %s" (string_of_type pattern.typ)
      (span_to_string pattern.span)
  in
  match pattern.pattern_desc with
  | Wildcard -> line buffer indent "pattern _%s" suffix
  | Bind binding ->
      line buffer indent "pattern bind %s%s" (binding_to_string binding) suffix
  | Owned_tree_cursor_pattern cursor ->
      line buffer indent
        "pattern owned-tree-cursor %s policy=functional-no-heap root=%s \
         version=%d path=%s%s"
        (binding_to_string cursor.cursor_binding)
        (binding_to_string cursor.root) cursor.root_version
        (owned_tree_path_to_string cursor.guarded_path) suffix
  | Int_pattern value ->
      line buffer indent "pattern int %s%s" (Z.to_string value) suffix
  | Bool_pattern value ->
      line buffer indent "pattern bool %b%s" value suffix
  | Unit_pattern -> line buffer indent "pattern unit%s" suffix
  | Tuple_pattern patterns ->
      line buffer indent "pattern tuple%s" suffix;
      List.iter
        (fun (label, pattern) ->
          line buffer (indent + 2) "component%s"
            (Option.fold ~none:"" ~some:(fun value -> " " ^ value) label);
          print_pattern buffer (indent + 4) pattern)
        patterns
  | Record_pattern fields ->
      line buffer indent "pattern record%s" suffix;
      List.iter
        (fun (field, pattern) ->
          line buffer (indent + 2) "field %s" (field_to_string field);
          print_pattern buffer (indent + 4) pattern)
        fields
  | Constructor_pattern (constructor, arguments) ->
      line buffer indent "pattern constructor %s#%d.%s#%d%s"
        constructor.constructor_type.type_name
        constructor.constructor_type.type_index constructor.constructor_name
        constructor.constructor_index suffix;
      List.iter (print_pattern buffer (indent + 2)) arguments
  | Or_pattern (left, right) ->
      line buffer indent "pattern or%s" suffix;
      print_pattern buffer (indent + 2) left;
      print_pattern buffer (indent + 2) right

let checked_name = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Negate -> "negate"
  | Multiply_constant constant ->
      "multiply-constant " ^ Z.to_string constant
  | Successor -> "successor"
  | Predecessor -> "predecessor"
  | Absolute_value -> "absolute-value"

let comparison_name = function
  | Equal -> "equal"
  | Not_equal -> "not-equal"
  | Less_than -> "less-than"
  | Less_or_equal -> "less-or-equal"
  | Greater_than -> "greater-than"
  | Greater_or_equal -> "greater-or-equal"

let mode_name = function Spec -> "spec" | Proof -> "proof" | Exec -> "exec"

let stage_name = function
  | Logical -> "logical"
  | Proof_stage -> "proof"
  | Runtime -> "runtime"

let call_form_name = function
  | Specification_call -> "specification"
  | Proof_call -> "proof"
  | Exec_call -> "exec"
  | Unclassified_call -> "unclassified"

let policy_name = function
  | Default_linear_z3 -> "default-linear/default-z3"
  | Default_linear_cvc5 -> "default-linear/default-cvc5"
  | Nonlinear_z3 -> "nonlinear/default-z3"

let abstraction_evidence_id = function
  | Incomplete_abstraction_evidence { evidence_id; _ }
  | Proposed_same_cmt_abstraction { evidence_id; _ }
  | Authenticated_same_cmt_abstraction { evidence_id; _ } ->
      evidence_id

let abstract_role_name = function
  | Abstract_constructor -> "constructor"
  | Abstract_model -> "model"
  | Current_model -> "current-model"
  | Abstract_invariant -> "invariant"
  | Terminal_read -> "terminal-read"
  | Current_terminal_read -> "current-terminal-read"
  | Terminal_snapshot -> "terminal-snapshot"
  | Unique_transition -> "unique-transition"
  | Shared_invariant_transition -> "shared-invariant-transition"

let rec print_expression buffer indent (expression : expression) =
  let suffix =
    Printf.sprintf " : %s @ %s" (string_of_type expression.typ)
      (span_to_string expression.span)
  in
  match expression.expression_desc with
  | Int_constant value ->
      line buffer indent "int %s%s" (Z.to_string value) suffix
  | Bool_constant value -> line buffer indent "bool %b%s" value suffix
  | Unit_constant -> line buffer indent "unit%s" suffix
  | Variable { binding; use_uniqueness } ->
      line buffer indent "variable %s%s%s" (binding_to_string binding)
        (match (binding.typ, use_uniqueness) with
        | ( Aggregate _ | Parameter _ | Application _), Definitely_unique -> " use=unique"
        | (Unit | Bool | Int | Tuple _), Definitely_unique
        | _, Definitely_aliased ->
            "")
        suffix
  | Tuple_value expressions ->
      line buffer indent "tuple%s" suffix;
      List.iter
        (fun (label, expression) ->
          line buffer (indent + 2) "component%s"
            (Option.fold ~none:"" ~some:(fun value -> " " ^ value) label);
          print_expression buffer (indent + 4) expression)
        expressions
  | Record_value { record_type; fields } ->
      line buffer indent "record %s#%d%s" record_type.type_name
        record_type.type_index suffix;
      List.iter
        (fun (field, value) ->
          line buffer (indent + 2) "field %s" (field_to_string field);
          print_expression buffer (indent + 4) value)
        fields
  | Constructor_value { constructor; arguments } ->
      line buffer indent "constructor %s#%d.%s#%d%s"
        constructor.constructor_type.type_name
        constructor.constructor_type.type_index constructor.constructor_name
        constructor.constructor_index suffix;
      List.iter (print_expression buffer (indent + 2)) arguments
  | Field_read { record; field } ->
      line buffer indent "field-read %s%s" (field_to_string field) suffix;
      print_expression buffer (indent + 2) record
  | Field_write { provenance; field; value; transition } ->
      line buffer indent
        "field-write %s root=%s pattern=%s use=%s local=%b public=%b \
         mutable=%b%s%s"
        (field_to_string field) (binding_to_string provenance.root)
        (match provenance.binding_pattern_uniqueness with
        | Definitely_unique -> "unique"
        | Definitely_aliased -> "aliased")
        (match provenance.identifier_use_uniqueness with
        | Definitely_unique -> "unique"
        | Definitely_aliased -> "aliased")
        provenance.field_is_local provenance.field_is_public
        provenance.field_is_mutable
        (Option.fold ~none:""
           ~some:(fun transition ->
             Printf.sprintf
               " policy=functional-no-heap pre-version=%d successor-version=%d \
                uniqueness=%s linearity=%s"
               transition.pre_version transition.successor_version
               (uniqueness_modality_to_string
                  transition.target_modalities.uniqueness_modality)
               (linearity_modality_to_string
                  transition.target_modalities.linearity_modality))
           transition)
        suffix;
      print_expression buffer (indent + 2) value
  | Shared_scalar_field_write { provenance; field; value; transition } ->
      line buffer indent
        "shared-scalar-field-write %s root=%s pattern=aliased use=%s local=%b \
         public=%b mutable=%b policy=bounded-shared-scalar-heap-v1 \
         function=%s#%d path=%d record=%s#%d canonical-root=%s target=%s \
         aliases=%s pre-epoch=%d successor-epoch=%d%s"
        (field_to_string field) (binding_to_string provenance.root)
        (match provenance.identifier_use_uniqueness with
        | Definitely_unique -> "unique"
        | Definitely_aliased -> "aliased")
        provenance.field_is_local provenance.field_is_public
        provenance.field_is_mutable transition.shared_function_name
        transition.shared_function_index transition.shared_path_id
        transition.shared_record_type.type_name
        transition.shared_record_type.type_index
        (binding_to_string transition.shared_canonical_root)
        (binding_to_string transition.shared_target)
        (String.concat "->"
           (List.map binding_to_string transition.shared_alias_chain))
        transition.shared_predecessor_epoch transition.shared_successor_epoch
        suffix;
      print_expression buffer (indent + 2) value
  | Owned_tree_nested_write { transition; value } ->
      line buffer indent
        "owned-tree-nested-write policy=functional-no-heap root=%s \
         pre-version=%d successor-version=%d target=%s mutability=mutable \
         uniqueness=%s linearity=%s rhs=ground invalidated=%s%s"
        (binding_to_string transition.root) transition.pre_version
        transition.successor_version
        (field_to_string transition.target_field)
        (uniqueness_modality_to_string
           transition.target_modalities.uniqueness_modality)
        (linearity_modality_to_string
           transition.target_modalities.linearity_modality)
        (String.concat ","
           (List.map string_of_int transition.invalidated_cursor_ids))
        suffix;
      Option.iter
        (fun cursor ->
          line buffer (indent + 2) "guarded-path cursor=%s version=%d path=%s"
            cursor.cursor_binding.name cursor.root_version
            (owned_tree_path_to_string cursor.guarded_path))
        transition.cursor;
      List.iter
        (function
          | Reconstruct_record { record_type; changed_field; preserved_fields } ->
              line buffer (indent + 2)
                "reconstruct record=%s#%d changed=%s preserve=%s"
                record_type.type_name record_type.type_index
                (field_to_string changed_field)
                (String.concat "," (List.map field_to_string preserved_fields))
          | Reconstruct_constructor constructor ->
              line buffer (indent + 2) "reconstruct constructor=%s"
                (constructor_to_string constructor))
        transition.reconstruction;
      print_expression buffer (indent + 2) value
  | Owned_tree_rebase { transition } ->
      let descendant =
        match transition.rhs_provenance with
        | Guarded_descendant_move cursor -> cursor
        | Ground_owned_tree_value -> assert false
      in
      line buffer indent
        "owned-tree-rebase policy=functional-no-heap root=%s pre-version=%d \
         successor-version=%d target=%s uniqueness=%s linearity=%s \
         rhs=guarded-descendant source-path=%s invalidated=%s%s"
        (binding_to_string transition.root) transition.pre_version
        transition.successor_version
        (field_to_string transition.target_field)
        (uniqueness_modality_to_string
           transition.target_modalities.uniqueness_modality)
        (linearity_modality_to_string
           transition.target_modalities.linearity_modality)
        (owned_tree_path_to_string descendant.guarded_path)
        (String.concat ","
           (List.map string_of_int transition.invalidated_cursor_ids))
        suffix;
      List.iter
        (function
          | Reconstruct_record { record_type; changed_field; preserved_fields } ->
              line buffer (indent + 2)
                "reconstruct record=%s#%d changed=%s preserve=%s"
                record_type.type_name record_type.type_index
                (field_to_string changed_field)
                (String.concat "," (List.map field_to_string preserved_fields))
          | Reconstruct_constructor constructor ->
              line buffer (indent + 2) "reconstruct constructor=%s"
                (constructor_to_string constructor))
        transition.reconstruction
  | Let_mutable (binding, initial, body) ->
      line buffer indent "let-mutable %s%s" (binding_to_string binding) suffix;
      line buffer (indent + 2) "initial";
      print_expression buffer (indent + 4) initial;
      line buffer (indent + 2) "body";
      print_expression buffer (indent + 4) body
  | Mutable_read binding ->
      line buffer indent "mutable-read %s%s" (binding_to_string binding) suffix
  | Mutable_write { provenance; value } ->
      line buffer indent
        "mutable-write root=%s pattern=unique use=unique local=true \
         public=true mutable=true%s"
        (binding_to_string provenance.root) suffix;
      print_expression buffer (indent + 2) value
  | Let (bindings, body) ->
      line buffer indent "let%s" suffix;
      List.iter
        (fun (pattern, value) ->
          line buffer (indent + 2) "binding";
          print_pattern buffer (indent + 4) pattern;
          print_expression buffer (indent + 4) value)
        bindings;
      line buffer (indent + 2) "body";
      print_expression buffer (indent + 4) body
  | Sequence (first, second) ->
      line buffer indent "sequence%s" suffix;
      print_expression buffer (indent + 2) first;
      print_expression buffer (indent + 2) second
  | If (condition, consequent, alternative) ->
      line buffer indent "if%s" suffix;
      line buffer (indent + 2) "condition";
      print_expression buffer (indent + 4) condition;
      line buffer (indent + 2) "then";
      print_expression buffer (indent + 4) consequent;
      Option.iter
        (fun alternative ->
          line buffer (indent + 2) "else";
          print_expression buffer (indent + 4) alternative)
        alternative
  | Match (scrutinee, cases) ->
      line buffer indent "match%s" suffix;
      line buffer (indent + 2) "scrutinee";
      print_expression buffer (indent + 4) scrutinee;
      List.iter
        (fun case ->
          line buffer (indent + 2) "case @ %s"
            (span_to_string case.case_span);
          print_pattern buffer (indent + 4) case.case_pattern;
          Option.iter
            (fun guard ->
              line buffer (indent + 4) "guard";
              print_expression buffer (indent + 6) guard)
            case.case_guard;
          line buffer (indent + 4) "body";
          print_expression buffer (indent + 6) case.case_body)
        cases
  | Checked_arithmetic (operation, operands) ->
      line buffer indent "checked-%s%s" (checked_name operation) suffix;
      List.iter (print_expression buffer (indent + 2)) operands
  | Compare (comparison, left, right) ->
      line buffer indent "compare-%s%s" (comparison_name comparison) suffix;
      print_expression buffer (indent + 2) left;
      print_expression buffer (indent + 2) right
  | Boolean_not operand ->
      line buffer indent "boolean-not%s" suffix;
      print_expression buffer (indent + 2) operand
  | Boolean_binary (operation, left, right) ->
      line buffer indent "boolean-%s%s"
        (match operation with And -> "and" | Or -> "or")
        suffix;
      print_expression buffer (indent + 2) left;
      print_expression buffer (indent + 2) right
  | Forall quantifier | Exists quantifier ->
      line buffer indent "%s binder=%s qid=%s skid=%s%s"
        (Logic_quantifier_private.kind_to_string
           (Logic_quantifier_private.kind quantifier.quantifier_metadata))
        (binding_to_string quantifier.quantifier_binder)
        (Logic_quantifier_private.qid quantifier.quantifier_metadata)
        (Logic_quantifier_private.skid quantifier.quantifier_metadata)
        suffix;
      line buffer (indent + 2) "body";
      print_expression buffer (indent + 4) quantifier.quantifier_body;
      Option.iter
        (fun trigger ->
          line buffer (indent + 2) "trigger";
          print_expression buffer (indent + 4) trigger)
        quantifier.quantifier_trigger
  | Direct_call { call_form; callee; type_arguments; arguments; recursive } ->
      line buffer indent "%s-call %s#%d recursive=%b type-arguments=[%s]%s"
        (call_form_name call_form) callee.function_name callee.function_index
        recursive
        (String.concat ", " (List.map string_of_type type_arguments)) suffix;
      List.iter
        (function
          | Value_argument { label; value } ->
              line buffer (indent + 2) "argument%s"
                (Option.fold ~none:"" ~some:(fun value -> " " ^ value) label);
              print_expression buffer (indent + 4) value
          | Callback_argument { label; callback } ->
              line buffer (indent + 2) "callback-argument%s %s#%d"
                (Option.fold ~none:"" ~some:(fun value -> " " ^ value) label)
                callback.callback_name callback.callback_id)
        arguments
  | Symbolic_application application ->
      line buffer indent "symbolic-application %s symbol=%s identity=%s%s"
        (Symbolic_application_private.declaration_name
           (Symbolic_application_private.declaration application))
        (Symbolic_application_private.symbol_name application)
        (Symbolic_application_private.identity_digest application) suffix;
      List.iter (print_expression buffer (indent + 2))
        (Symbolic_application_private.arguments application)
  | Callback_call application ->
      line buffer indent "callback-call %s#%d%s" application.callback.callback_name
        application.callback.callback_id suffix;
      List.iter (print_expression buffer (indent + 2))
        (List.map snd application.arguments)
  | Callback_requires application ->
      line buffer indent "callback-requires %s#%d%s"
        application.callback.callback_name application.callback.callback_id suffix;
      List.iter (print_expression buffer (indent + 2))
        (List.map snd application.arguments)
  | Callback_ensures { application; result } ->
      line buffer indent "callback-ensures %s#%d%s"
        application.callback.callback_name application.callback.callback_id suffix;
      List.iter (print_expression buffer (indent + 2))
        (List.map snd application.arguments);
      print_expression buffer (indent + 2) result
  | Optional_absent -> line buffer indent "optional-absent%s" suffix
  | Optional_present payload ->
      line buffer indent "optional-present%s" suffix;
      print_expression buffer (indent + 2) payload
  | Optional_forward payload ->
      line buffer indent "optional-forward%s" suffix;
      print_expression buffer (indent + 2) payload
  | Reveal function_id ->
      line buffer indent "reveal %s#%d%s" function_id.function_name
        function_id.function_index suffix
  | Reveal_with_fuel { function_id; literal_depth } ->
      line buffer indent "reveal-with-fuel %s#%d depth=%s%s"
        function_id.function_name function_id.function_index literal_depth suffix
  | Use_type_invariant { use_id; value } ->
      line buffer indent "use-type-invariant id=%s%s" use_id suffix;
      print_expression buffer (indent + 2) value
  | Local_assert { assertion_ordinal; predicate } ->
      line buffer indent "local-assert ordinal=%d stage=proof%s"
        assertion_ordinal suffix;
      print_expression buffer (indent + 2) predicate
  | Proof_region body ->
      line buffer indent "proof-region stage=proof%s" suffix;
      print_expression buffer (indent + 2) body
  | Old payload ->
      line buffer indent "old%s" suffix;
      print_expression buffer (indent + 2) payload

let rec shared_scalar_transitions expression =
  let nested =
    match expression.expression_desc with
    | Tuple_value values -> List.map snd values
    | Record_value { fields; _ } -> List.map snd fields
    | Constructor_value { arguments; _ } -> arguments
    | Field_read { record; _ } -> [ record ]
    | Field_write { value; _ } | Shared_scalar_field_write { value; _ }
    | Mutable_write { value; _ } ->
        [ value ]
    | Owned_tree_nested_write { value; _ } -> [ value ]
    | Owned_tree_rebase _ | Optional_absent -> []
    | Optional_present payload | Optional_forward payload -> [ payload]
    | Let_mutable (_, initial, body) -> [ initial; body ]
    | Let (bindings, body) -> List.map snd bindings @ [ body ]
    | Sequence (left, right) | Compare (_, left, right)
    | Boolean_binary (_, left, right) ->
        [ left; right ]
    | If (condition, consequent, alternative) ->
        condition :: consequent :: Option.to_list alternative
    | Match (scrutinee, cases) ->
        scrutinee
        :: List.concat_map
             (fun case -> Option.to_list case.case_guard @ [ case.case_body ])
             cases
    | Checked_arithmetic (_, operands) -> operands
    | Boolean_not operand | Old operand | Proof_region operand -> [ operand ]
    | Forall quantifier | Exists quantifier ->
        [ quantifier.quantifier_body ]
    | Direct_call { arguments; _ } ->
        List.filter_map
          (function
            | Value_argument { value; _ } -> Some value
            | Callback_argument _ -> None)
          arguments
    | Symbolic_application application ->
        Symbolic_application_private.arguments application
    | Callback_call application | Callback_requires application ->
        List.map snd application.arguments
    | Callback_ensures { application; result } ->
        List.map snd application.arguments @ [ result ]
    | Use_type_invariant { value; _ } -> [ value ]
    | Local_assert { predicate; _ } -> [ predicate ]
    | Int_constant _ | Bool_constant _ | Unit_constant | Variable _
    | Mutable_read _ | Reveal _ | Reveal_with_fuel _ ->
        []
  in
  let current =
    match expression.expression_desc with
    | Shared_scalar_field_write { transition; _ } -> [ transition ]
    | _ -> []
  in
  current @ List.concat_map shared_scalar_transitions nested

let function_shared_scalar_transitions definition =
  match definition.body with
  | Checked_exec { body; _ } | Proof_body { body; _ }
  | Recursive_spec_definition { body; _ } | Spec_definition body ->
      shared_scalar_transitions body.expression
  | External_specification _ | Trusted_external_spec_target _
  | Trusted_external_body _ | Symbolic_declaration _ ->
      []

let map_binding_type substitute (binding : binding) =
  { binding with typ = substitute binding.typ }

let map_cursor_types substitute (cursor : owned_tree_cursor) =
  { cursor with
    cursor_binding = map_binding_type substitute cursor.cursor_binding;
    root = map_binding_type substitute cursor.root }

let rec map_pattern_types substitute pattern =
  let pattern_desc =
    match pattern.pattern_desc with
    | Tuple_pattern components ->
        Tuple_pattern
          (List.map (fun (label, nested) ->
               (label, map_pattern_types substitute nested)) components)
    | Record_pattern fields ->
        Record_pattern
          (List.map (fun (field, nested) ->
               (field, map_pattern_types substitute nested)) fields)
    | Constructor_pattern (constructor, arguments) ->
        Constructor_pattern
          (constructor, List.map (map_pattern_types substitute) arguments)
    | Or_pattern (left, right) ->
        Or_pattern
          (map_pattern_types substitute left, map_pattern_types substitute right)
    | Bind binding -> Bind (map_binding_type substitute binding)
    | Owned_tree_cursor_pattern cursor ->
        Owned_tree_cursor_pattern (map_cursor_types substitute cursor)
    | (Wildcard | Int_pattern _ | Bool_pattern _
      | Unit_pattern) as desc -> desc
  in
  { pattern with pattern_desc; typ = substitute pattern.typ }

let map_mutation_types substitute (mutation : mutation_provenance) =
  { mutation with root = map_binding_type substitute mutation.root }

let map_transition_types substitute (transition : owned_tree_transition) =
  { transition with
    root = map_binding_type substitute transition.root;
    cursor = Option.map (map_cursor_types substitute) transition.cursor;
    rhs_provenance =
      (match transition.rhs_provenance with
      | Ground_owned_tree_value -> Ground_owned_tree_value
      | Guarded_descendant_move cursor ->
          Guarded_descendant_move (map_cursor_types substitute cursor)) }

let map_shared_transition_types substitute
    (transition : shared_scalar_heap_transition) =
  { transition with
    shared_formal_roots =
      List.map (map_binding_type substitute) transition.shared_formal_roots;
    shared_target = map_binding_type substitute transition.shared_target;
    shared_canonical_root =
      map_binding_type substitute transition.shared_canonical_root;
    shared_alias_chain =
      List.map (map_binding_type substitute) transition.shared_alias_chain }

let map_callback_application_types recurse application =
  {
    application with
    arguments =
      List.map (fun (label, value) -> (label, recurse value))
        application.arguments;
  }

let rec map_expression_types substitute expression =
  let recurse = map_expression_types substitute in
  let expression_desc =
    match expression.expression_desc with
    | Tuple_value values ->
        Tuple_value (List.map (fun (label, value) -> (label, recurse value)) values)
    | Record_value value ->
        Record_value { value with fields = List.map (fun (field, item) -> (field, recurse item)) value.fields }
    | Constructor_value value ->
        Constructor_value { value with arguments = List.map recurse value.arguments }
    | Field_read value -> Field_read { value with record = recurse value.record }
    | Variable value ->
        Variable
          { value with binding = map_binding_type substitute value.binding }
    | Field_write value ->
        Field_write
          { value with
            provenance = map_mutation_types substitute value.provenance;
            value = recurse value.value;
            transition =
              Option.map (map_transition_types substitute) value.transition }
    | Shared_scalar_field_write value ->
        Shared_scalar_field_write
          { value with
            provenance = map_mutation_types substitute value.provenance;
            value = recurse value.value;
            transition =
              map_shared_transition_types substitute value.transition }
    | Owned_tree_nested_write value ->
        Owned_tree_nested_write
          { transition = map_transition_types substitute value.transition;
            value = recurse value.value }
    | Owned_tree_rebase value ->
        Owned_tree_rebase
          { transition = map_transition_types substitute value.transition }
    | Let_mutable (binding, initial, body) ->
        Let_mutable
          (map_binding_type substitute binding, recurse initial, recurse body)
    | Mutable_read binding ->
        Mutable_read (map_binding_type substitute binding)
    | Mutable_write value ->
        Mutable_write
          { provenance = map_mutation_types substitute value.provenance;
            value = recurse value.value }
    | Let (bindings, body) ->
        Let
          (List.map (fun (pattern, value) ->
               (map_pattern_types substitute pattern, recurse value)) bindings,
           recurse body)
    | Sequence (left, right) -> Sequence (recurse left, recurse right)
    | If (condition, yes, no) ->
        If (recurse condition, recurse yes, Option.map recurse no)
    | Match (scrutinee, cases) ->
        Match
          (recurse scrutinee,
           List.map (fun (case : case) ->
               { case with
                 case_pattern = map_pattern_types substitute case.case_pattern;
                 case_guard = Option.map recurse case.case_guard;
                 case_body = recurse case.case_body }) cases)
    | Checked_arithmetic (operation, operands) ->
        Checked_arithmetic (operation, List.map recurse operands)
    | Compare (operation, left, right) ->
        Compare (operation, recurse left, recurse right)
    | Boolean_not operand -> Boolean_not (recurse operand)
    | Boolean_binary (operation, left, right) ->
        Boolean_binary (operation, recurse left, recurse right)
    | Forall quantifier | Exists quantifier ->
        let binder = map_binding_type substitute quantifier.quantifier_binder in
        let quantifier =
          {
            quantifier_metadata =
              Logic_quantifier_private.rebind ~binder_index:binder.id
                ~binder_type:binder.typ quantifier.quantifier_metadata;
            quantifier_binder = binder;
            quantifier_body = recurse quantifier.quantifier_body;
            quantifier_trigger = Option.map recurse quantifier.quantifier_trigger;
          }
        in
        (match expression.expression_desc with
        | Forall _ -> Forall quantifier
        | Exists _ -> Exists quantifier
        | Int_constant _ | Bool_constant _ | Unit_constant | Variable _
        | Tuple_value _ | Record_value _ | Constructor_value _ | Field_read _
        | Field_write _ | Shared_scalar_field_write _
        | Owned_tree_nested_write _ | Owned_tree_rebase _ | Let_mutable _
        | Mutable_read _ | Mutable_write _ | Let _ | Sequence _ | If _
        | Match _ | Checked_arithmetic _ | Compare _ | Boolean_not _
        | Boolean_binary _ | Direct_call _ | Callback_call _
        | Symbolic_application _
        | Callback_requires _ | Callback_ensures _ | Optional_absent
        | Optional_present _ | Optional_forward _ | Reveal _
        | Reveal_with_fuel _ | Use_type_invariant _ | Local_assert _
        | Proof_region _ | Old _ ->
            assert false)
    | Direct_call call ->
        Direct_call
          { call with
            type_arguments = List.map substitute call.type_arguments;
            arguments =
              List.map
                (function
                  | Value_argument { label; value } ->
                      Value_argument { label; value = recurse value }
                  | Callback_argument _ as argument -> argument)
                call.arguments }
    | Symbolic_application application ->
        Symbolic_application
          (application
          |> Symbolic_application_private.map_arguments recurse
          |> Symbolic_application_private.map_types substitute
          |> Result.get_ok)
    | Callback_call application ->
        Callback_call
          (map_callback_application_types recurse application)
    | Callback_requires application ->
        Callback_requires
          (map_callback_application_types recurse application)
    | Callback_ensures { application; result } ->
        Callback_ensures
          { application = map_callback_application_types recurse application;
            result = recurse result }
    | Use_type_invariant value ->
        Use_type_invariant { value with value = recurse value.value }
    | Local_assert value ->
        Local_assert { value with predicate = recurse value.predicate }
    | Proof_region body -> Proof_region (recurse body)
    | Old body -> Old (recurse body)
    | Optional_present payload -> Optional_present (recurse payload)
    | Optional_forward payload -> Optional_forward (recurse payload)
    | (Int_constant _ | Bool_constant _ | Unit_constant
      | Reveal _ | Reveal_with_fuel _ | Optional_absent) as desc ->
        desc
  in
  { expression with expression_desc; typ = substitute expression.typ }

let symbolic_application_arguments = function
  | Symbolic_application application ->
      Some (Symbolic_application_private.arguments application)
  | _ -> None

let map_symbolic_application_arguments map = function
  | Symbolic_application application ->
      Some
        (Symbolic_application
           (Symbolic_application_private.map_arguments map application))
  | _ -> None

let substitute_contracts binders arguments contracts =
  if List.length binders <> List.length arguments then
    Error "retained parametric signature type argument arity mismatch"
  else
    let substitute typ =
      Parametric_type.substitute (List.combine binders arguments) typ
    in
    let predicate (clause : predicate_clause) =
      { clause with predicate =
          { clause.predicate with expression = map_expression_types substitute clause.predicate.expression } }
    in
    let ensures (clause : ensures_clause) =
      { clause with
        binder = Option.map (map_pattern_types substitute) clause.binder;
        predicate =
          { clause.predicate with expression = map_expression_types substitute clause.predicate.expression } }
    in
    Ok
      { requires = List.map predicate contracts.requires;
        ensures = List.map ensures contracts.ensures;
        decreases = List.map predicate contracts.decreases;
        assertions = List.map predicate contracts.assertions }

let value_parameter = function
  | Value_parameter parameter -> Some parameter
  | Callback_parameter _ -> None

let value_argument = function
  | Value_argument { label; value } -> Some (label, value)
  | Callback_argument _ -> None

let value_parameters parameters =
  let rec collect values = function
    | [] -> Some (List.rev values)
    | Value_parameter parameter :: rest ->
        collect (parameter :: values) rest
    | Callback_parameter _ :: _ -> None
  in
  collect [] parameters

let value_arguments arguments =
  let rec collect values = function
    | [] -> Some (List.rev values)
    | Value_argument { label; value } :: rest ->
        collect ((label, value) :: values) rest
    | Callback_argument _ :: _ -> None
  in
  collect [] arguments

let call_arguments arguments =
  List.map
    (fun (label, value) -> Value_argument { label; value })
    arguments

let to_string program =
  let buffer = Buffer.create 4096 in
  line buffer 0 "policy %s" (policy_name program.policy);
  List.iter
    (fun descriptor -> line buffer 0 "%s" (Parametric_adt.to_string descriptor))
    program.parametric_adts;
  List.iter
    (fun definition ->
      line buffer 0 "type %s#%d representation=%s @ %s"
        definition.type_id.type_name definition.type_id.type_index
        (match definition.representation with
        | Revealed -> "revealed"
        | Abstract_with_evidence evidence ->
            "abstract evidence=" ^ abstraction_evidence_id evidence)
        (span_to_string definition.span);
      (match definition.representation with
      | Abstract_with_evidence
          (Proposed_same_cmt_abstraction evidence
          | Authenticated_same_cmt_abstraction evidence) ->
          line buffer 2
            "same-cmt-link semantic-type=%s#%d constraint=%s"
            evidence.abstract_signature_type.type_name
            evidence.abstract_signature_type.type_index
            (span_to_string evidence.constraint_span);
          line buffer 4 "signature-module %s identity=%s"
            evidence.signature_module_identity.source_name
            evidence.signature_module_identity.resolved_identifier;
          line buffer 4 "implementation-module %s identity=%s"
            evidence.implementation_module_identity.source_name
            evidence.implementation_module_identity.resolved_identifier;
          line buffer 4 "signature-type %s identity=%s"
            evidence.abstract_signature_type_identity.source_name
            evidence.abstract_signature_type_identity.resolved_identifier;
          line buffer 4 "implementation-type %s identity=%s"
            evidence.hidden_implementation_type_identity.source_name
            evidence.hidden_implementation_type_identity.resolved_identifier;
          List.iter
            (fun declaration_span ->
              line buffer 4 "declaration %s"
                (span_to_string declaration_span))
            evidence.declaration_spans;
          List.iter
            (fun operation ->
              line buffer 4 "public %s#%d role=%s @ %s"
                operation.public_function_name
                operation.public_function_index
                (abstract_role_name operation.public_role)
                (span_to_string operation.public_span))
            evidence.public_surface;
          Option.iter
            (fun prerequisite ->
              line buffer 4
                "owned-tree root=%s#%d construction=local-first-order-closed"
                prerequisite.owned_root.type_name
                prerequisite.owned_root.type_index;
              List.iter
                (fun field ->
                  line buffer 6 "recursive-edge %s"
                    (field_to_string field))
                prerequisite.recursive_edges)
            evidence.owned_tree_prerequisite;
          Option.iter
            (fun frozen ->
              line buffer 4
                "frozen-spine root=%s#%d link=%s#%d payload=%s edge=%s \
                 helper=%s#%d model=%s#%d result=%s#%d invariant=%s#%d \
                 constructor=%s#%d mutator=%s#%d terminal=%s#%d"
                frozen.frozen_root.type_name frozen.frozen_root.type_index
                frozen.frozen_link.type_name frozen.frozen_link.type_index
                (field_to_string frozen.frozen_payload_field)
                (field_to_string frozen.frozen_edge_field)
                frozen.frozen_helper.function_name
                frozen.frozen_helper.function_index
                frozen.frozen_model.function_name
                frozen.frozen_model.function_index
                frozen.frozen_result_type.type_name
                frozen.frozen_result_type.type_index
                frozen.frozen_invariant.function_name
                frozen.frozen_invariant.function_index
                frozen.frozen_constructor.function_name
                frozen.frozen_constructor.function_index
                frozen.frozen_mutator.function_name
                frozen.frozen_mutator.function_index
                frozen.frozen_terminal.function_name
                frozen.frozen_terminal.function_index;
              line buffer 6 "nil=%s next=%s child=%s end=%s more=%s head=%s tail=%s"
                (constructor_to_string frozen.frozen_nil_constructor)
                (constructor_to_string frozen.frozen_next_constructor)
                (field_to_string frozen.frozen_next_child_field)
                (constructor_to_string frozen.frozen_end_constructor)
                (constructor_to_string frozen.frozen_more_constructor)
                (field_to_string frozen.frozen_more_head_field)
                (field_to_string frozen.frozen_more_tail_field))
            (frozen_spine_prerequisite evidence)
      | Revealed
      | Abstract_with_evidence (Incomplete_abstraction_evidence _) ->
          ());
      let print_field indent field =
        let mutability =
          match field.field_mutability with
          | Immutable_field -> "immutable"
          | Mutable_field -> "mutable"
        in
        let uniqueness =
          match field.field_modalities.uniqueness_modality with
          | Preserve_uniqueness -> "preserve"
          | Force_unique -> "force-unique"
          | Force_aliased -> "force-aliased"
        in
        let linearity =
          match field.field_modalities.linearity_modality with
          | Preserve_linearity -> "preserve"
          | Force_once -> "force-once"
          | Force_many -> "force-many"
        in
        line buffer indent
          "field %s : %s mutability=%s uniqueness=%s linearity=%s @ %s"
          (field_to_string field.field_id)
          (string_of_type field.field_type) mutability uniqueness linearity
          (span_to_string field.span)
      in
      match definition.type_kind with
      | Record_definition fields ->
          line buffer 2 "record";
          List.iter (print_field 4) fields
      | Variant_definition constructors ->
          line buffer 2 "variant";
          List.iter
            (fun constructor ->
              line buffer 4 "constructor %s#%d @ %s"
                constructor.constructor_id.constructor_name
                constructor.constructor_id.constructor_index
                (span_to_string constructor.span);
              List.iter (print_field 6) constructor.constructor_fields)
            constructors)
    program.types;
  List.iter
    (fun definition ->
      line buffer 0
        "function %s#%d%s mode=%s recursive=%b result=%s policy=%s @ %s"
        definition.function_id.function_name
        definition.function_id.function_index
        (match definition.type_binders with
        | [] -> ""
        | binders ->
            Printf.sprintf " binders=[%s]"
              (String.concat ", "
                 (List.map Parametric_type.binder_to_string binders)))
        (mode_name definition.mode) definition.recursive
        (string_of_type definition.result_type)
        (policy_name definition.policy)
        (span_to_string definition.span);
      (match function_shared_scalar_transitions definition with
      | first :: rest ->
          line buffer 2
            "mutation-policy bounded-shared-scalar-heap-v1 path=%d \
             record=%s#%d field=%s formals=%s writes=%d entry-epoch=0 \
             final-epoch=%d"
            first.shared_path_id first.shared_record_type.type_name
            first.shared_record_type.type_index
            (field_to_string first.shared_target_field)
            (String.concat ","
               (List.map binding_to_string first.shared_formal_roots))
            (List.length (first :: rest))
            (List.fold_left
               (fun epoch transition ->
                 max epoch transition.shared_successor_epoch)
               first.shared_successor_epoch rest)
      | [] -> ());
      Option.iter
        (fun parameter_index ->
          line buffer 2 "returns-unique-parameter %d" parameter_index)
        definition.returns_unique_parameter;
      List.iter
        (function
          | Value_parameter parameter ->
              line buffer 2 "parameter%s"
                (Option.fold ~none:"" ~some:(fun value -> " " ^ value)
                   parameter.label);
              print_pattern buffer 4 parameter.pattern;
              Option.iter
                (fun default ->
                  line buffer 4 "optional-default";
                  print_pattern buffer 6 default.optional_pattern;
                  print_expression buffer 6 default.optional_expression)
                parameter.optional_default
          | Callback_parameter formal ->
              line buffer 2 "callback-formal%s %s#%d"
                (Option.fold ~none:"" ~some:(fun value -> " " ^ value)
                   formal.label)
                formal.binding.callback_name formal.binding.callback_id)
        definition.parameters;
      let print_predicate heading (clause : predicate_clause) =
        line buffer 2 "%s %d stage=%s @ %s" heading clause.clause_index
          (stage_name clause.predicate.stage)
          (span_to_string clause.span);
        print_expression buffer 4 clause.predicate.expression
      in
      List.iter (print_predicate "requires") definition.contracts.requires;
      List.iter
        (fun clause ->
          line buffer 2 "ensures %d stage=%s @ %s" clause.clause_index
            (stage_name clause.predicate.stage)
            (span_to_string clause.span);
          Option.iter (print_pattern buffer 4) clause.binder;
          print_expression buffer 4 clause.predicate.expression)
        definition.contracts.ensures;
      List.iter (print_predicate "decreases") definition.contracts.decreases;
      List.iter (print_predicate "assertion") definition.contracts.assertions;match definition.body with
      | Checked_exec { body; provenance } ->
          line buffer 2 "body checked-exec stage=%s provenance=%s"
            (stage_name body.stage)
            (match provenance with
            | Authenticated_typedtree { source_file; _ } ->
                "typedtree:" ^ Filename.basename source_file
            | Raw_semantic_body _ -> "raw-semantic");
          print_expression buffer 4 body.expression
      | Spec_definition body ->
          line buffer 2 "body spec-definition stage=%s"
            (stage_name body.stage);
          print_expression buffer 4 body.expression
      | Recursive_spec_definition { body; visibility; provenance } ->
          line buffer 2
            "body recursive-spec-definition stage=%s visibility=%s \
             provenance=%s"
            (stage_name body.stage)
            (match visibility with
            | `Opaque -> "opaque" | `Revealed -> "revealed")
            (match provenance with
            | Authenticated_typedtree { source_file; _ } ->
                "typedtree:" ^ Filename.basename source_file
            | Raw_semantic_body _ -> "raw-semantic");
          print_expression buffer 4 body.expression
      | Proof_body { body; provenance } ->
          line buffer 2 "body proof stage=%s provenance=%s"
            (stage_name body.stage)
            (match provenance with
            | Authenticated_typedtree { source_file; _ } ->
                "typedtree:" ^ Filename.basename source_file
            | Raw_semantic_body _ -> "raw-semantic");
          print_expression buffer 4 body.expression
      | External_specification link
      | Trusted_external_spec_target link -> (
          let role =
            match definition.body with
            | External_specification _ -> "external-specification noncallable"
            | Trusted_external_spec_target _ ->
                "trusted-external-specification target"
            | _ -> assert false
          in
          match link with
          | Same_unit_target
              {
                wrapper;
                target;
                target_span;
                declaration_span;
                witness_span
              } ->
              line buffer 2
                "body %s wrapper=%s#%d target=%s#%d target-span=%s \
                 wrapper-span=%s witness-span=%s requires=%d ensures=%d"
                role wrapper.function_name wrapper.function_index
                target.function_name target.function_index
                (span_to_string target_span)
                (span_to_string declaration_span)
                (span_to_string witness_span)
                (List.length definition.contracts.requires)
                (List.length definition.contracts.ensures)
          | Imported_unverified_target
              {
                wrapper;
                consumer_artifact_digest;
                target_unit;
                target_interface_digest;
                import_crc;
                canonical_path;
                value_uid;
                callable_abi_digest;
                summary_digest;
                target_span;
                declaration_span;
                witness_span;
              } ->
              line buffer 2
                "body %s wrapper=%s#%d target=imported-unverified:%s \
                 target-unit=%s target-interface-digest=%s import-crc=%s uid=%s \
                 consumer=%s abi=%s summary=%s target-span=%s wrapper-span=%s \
                 witness-span=%s requires=%d ensures=%d"
                role wrapper.function_name wrapper.function_index canonical_path
                target_unit target_interface_digest import_crc value_uid
                consumer_artifact_digest callable_abi_digest summary_digest
                (span_to_string target_span)
                (span_to_string declaration_span)
                (span_to_string witness_span)
                (List.length definition.contracts.requires)
                (List.length definition.contracts.ensures)
          | Unresolved_target { target_name; witness_span } ->
              line buffer 2
                "body %s target=unresolved:%s witness-span=%s requires=%d \
                 ensures=%d"
                role target_name (span_to_string witness_span)
                (List.length definition.contracts.requires)
                (List.length definition.contracts.ensures))
      | Trusted_external_body
          (Authenticated_external_body provenance) ->
          line buffer 2
            "body trusted-external-body trust=axiomatic \
             provenance=typedtree:%s declaration-span=%s witness-span=%s \
             requires=%d ensures=%d body=unchecked"
            (Filename.basename provenance.source_file)
            (span_to_string provenance.declaration_span)
            (span_to_string provenance.witness_span)
            (List.length definition.contracts.requires)
            (List.length definition.contracts.ensures)
      | Trusted_external_body (Raw_external_body raw_span) ->
          line buffer 2
            "body trusted-external-body trust=unauthenticated raw-span=%s"
            (span_to_string raw_span)
      | Symbolic_declaration declaration ->
          line buffer 2
            "body symbolic-declaration identity=%s path=%s uid=%s binders=%d \
             parameters=%d result=%s trust=none body=absent"
            (Symbolic_application_private.marker_id declaration)
            (Symbolic_application_private.canonical_path declaration)
            (Symbolic_application_private.value_uid declaration)
            (List.length
               (Symbolic_application_private.type_binders declaration))
            (List.length
               (Symbolic_application_private.parameter_types declaration))
            (string_of_type
               (Symbolic_application_private.declaration_result_type
                  declaration)))
    program.functions;
  Buffer.contents buffer
