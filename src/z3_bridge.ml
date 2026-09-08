type config = {
  timeout_ms : int;
  model : bool;
}

type model_value =
  | Integer of Z.t
  | Boolean of bool
  | Aggregate_identity of Z.t
  | Bit_vector of Bv_value.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type bv_model_binding = {
  projection_identity : string;
  width : Bv_width.t;
  value : Bv_value.t;
}

type inconclusive_reason : value mod contended portable =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive_reason

type bv_query_outcome =
  | Bv_verified
  | Bv_counterexample of bv_model_binding list
  | Bv_inconclusive of inconclusive_reason

type error =
  | Invalid_configuration of string
  | Unsupported_features of Logic_ir.feature list
  | Malformed_logic_ir of string
  | Malformed_vir of string
  | Backend_failure of string

let error_to_string = function
  | Invalid_configuration message ->
      "invalid direct-Z3 configuration: " ^ message
  | Unsupported_features features ->
      Printf.sprintf "unsupported direct-Z3 features: %s"
        (String.concat ", " (List.map Logic_ir.feature_to_string features))
  | Malformed_logic_ir message ->
      "malformed logic IR: " ^ message
  | Malformed_vir message -> "malformed VIR: " ^ message
  | Backend_failure message -> "direct-Z3 backend failure: " ^ message

type controlled : value mod contended portable =
  | Real
  | Force_unknown
  | Force_timeout
  | Force_backend_failure

type bv_model_fault : value mod contended portable =
  | Missing_evaluation
  | Wrong_sort
  | Non_numeral
  | Width_mismatch
  | Residue_text of string

type counters : value mod contended portable = {
  capability_resolutions : int;
  translations : int;
  contexts_created : int;
  solvers_created : int;
  solver_resets : int;
  contexts_cleaned : int;
  contexts_live : int;
  maximum_contexts_live : int;
  selected_logics : string list;
}

type detached_sort : value mod contended portable =
  | Detached_int_sort
  | Detached_bool_sort
  | Detached_bv_sort of string
  | Detached_named_sort of int

type detached_term : value mod contended portable =
  | Detached_integer_term of string
  | Detached_boolean_term of bool
  | Detached_bound_term of int
  | Detached_apply_term of int * detached_term list
  | Detached_add_term of detached_term * detached_term
  | Detached_subtract_term of detached_term * detached_term
  | Detached_negate_term of detached_term
  | Detached_multiply_term of detached_term * detached_term
  | Detached_scale_term of string * detached_term
  | Detached_less_than_term of detached_term * detached_term
  | Detached_less_or_equal_term of detached_term * detached_term
  | Detached_greater_than_term of detached_term * detached_term
  | Detached_greater_or_equal_term of detached_term * detached_term
  | Detached_equal_term of detached_term * detached_term
  | Detached_distinct_term of detached_term * detached_term
  | Detached_not_term of detached_term
  | Detached_and_term of detached_term list
  | Detached_or_term of detached_term list
  | Detached_implies_term of detached_term * detached_term
  | Detached_forall_term of
      (int * string * detached_sort) list
      * detached_term
      * detached_term
      * string
      * string
  | Detached_exists_term of
      (int * string * detached_sort) list * detached_term * string * string
  | Detached_ite_term of detached_term * detached_term * detached_term
  | Detached_bv_literal_term of string * string
  | Detached_bv_eq_term of detached_term * detached_term
  | Detached_bv_distinct_term of detached_term * detached_term
  | Detached_bv_add_mod_term of detached_term * detached_term
  | Detached_bv_sub_mod_term of detached_term * detached_term
  | Detached_bv_not_term of detached_term
  | Detached_bv_and_term of detached_term * detached_term
  | Detached_bv_or_term of detached_term * detached_term
  | Detached_bv_xor_term of detached_term * detached_term
  | Detached_bv_ult_term of detached_term * detached_term
  | Detached_bv_ule_term of detached_term * detached_term
  | Detached_bv_ugt_term of detached_term * detached_term
  | Detached_bv_uge_term of detached_term * detached_term
  | Detached_bv_slt_term of detached_term * detached_term
  | Detached_bv_sle_term of detached_term * detached_term
  | Detached_bv_sgt_term of detached_term * detached_term
  | Detached_bv_sge_term of detached_term * detached_term
  | Detached_bv_to_int_unsigned_term of detached_term
  | Detached_bv_to_int_signed_term of detached_term
  | Detached_int_to_bv_mod_term of string * detached_term

type detached_declaration : value mod contended portable =
  | Detached_sort_declaration of int * string
  | Detached_function_declaration of
      int * string * detached_sort list * detached_sort
  | Detached_datatype_declaration of Z3_datatype_private.declaration

type detached_binder : value mod contended portable =
  int * string * detached_sort

type detached_axiom : value mod contended portable = {
  detached_binders : detached_binder list;
  detached_body : detached_term;
  detached_patterns : detached_term list list;
  detached_qid : string;
  detached_skid : string;
}

type detached_projection_sort : value mod contended portable =
  | Detached_project_integer
  | Detached_project_boolean
  | Detached_project_aggregate

type detached_projection : value mod contended portable =
  | Detached_function_projection of string * int * detached_projection_sort
  | Detached_bv_projection of string * string * detached_term

type detached_plan : value mod contended portable = {
  detached_requirements : Logic_ir.feature list;
  detached_declarations : detached_declaration list;
  detached_axioms : detached_axiom list;
  detached_assertions : detached_term list;
  detached_projections : detached_projection list;
}

type detached_query : value mod contended portable =
  | Detached_query of string

type detached_model_value : value mod contended portable =
  | Detached_integer of string * string
  | Detached_boolean of string * bool
  | Detached_aggregate of string * string
  | Detached_bit_vector of string * string * string

type vir_projection_metadata = {
  projection_symbol : Vir.symbol;
  projection_identity : string;
  projection_order : int;
  projection_width : Bv_width.t option;
  projection_width_reference : string option;
}

type detached_outcome : value mod contended portable =
  | Detached_verified
  | Detached_counterexample of detached_model_value option list
  | Detached_inconclusive of inconclusive_reason

type detached_attempt : value mod contended portable = {
  detached_result : (detached_outcome, string) result;
  detached_telemetry : counters;
}

type detached_preflight_defect =
  | Mixed_bv_add_widths
  | Omitted_bv_capability
  | Wrong_int_to_bv_operand
  | Bv_projection_width_mismatch

type datatype_worker_operations : value mod portable = {
  declare :
    context:Z3.context ->
    resolve_named_sort:(int -> Z3.Sort.sort) ->
    resolve_bv_sort:(string -> Z3.Sort.sort) ->
    Z3_datatype_private.declaration ->
    (Z3_datatype_private.bindings, string) result;
}
[@@unsafe_allow_any_mode_crossing
  "This record contains only operation code. Native Z3 handles are arguments \
   and results scoped to one worker invocation, never captured here."]

let datatype_worker_operations =
  { declare = Z3_datatype_private.declare_detached }

type quantifier_worker_operations : value mod portable = {
  exists :
    Z3.context ->
    Z3.Expr.expr list ->
    Z3.Expr.expr ->
    int option ->
    Z3.Quantifier.Pattern.pattern list ->
    Z3.Expr.expr list ->
    Z3.Symbol.symbol option ->
    Z3.Symbol.symbol option ->
    Z3.Quantifier.quantifier;
}
[@@unsafe_allow_any_mode_crossing
  "This record contains only worker-local reconstruction code. Z3 handles \
   remain arguments and results scoped to one worker invocation."]

let quantifier_worker_operations =
  { exists = Z3.Quantifier.mk_exists_const }

type quantifier_audit_operations : value mod portable = {
  audit :
    universal:bool ->
    expected_names:string list ->
    expected_bound:Z3.Expr.expr list ->
    trigger:Z3.Expr.expr option ->
    Z3.Quantifier.quantifier ->
    unit;
}
[@@unsafe_allow_any_mode_crossing
  "This record contains only structural inspection code. Native Z3 handles \
   remain invocation arguments and are never retained."]

type 'a local_attempt = {
  result : ('a, error) result;
  telemetry : counters;
}

let capability_resolutions = ref 0
let translations = ref 0
let contexts_created = ref 0
let solvers_created = ref 0
let solver_resets = ref 0
let contexts_cleaned = ref 0
let contexts_live = ref 0
let maximum_contexts_live = ref 0
let selected_logics_reversed = ref []

let solver_logic requirements =
  if List.mem Logic_ir.Bit_vectors requirements then "general"
  else if List.mem Logic_ir.Nonlinear_integer_arithmetic requirements then "AUFNIA"
  else "AUFLIA"

let nonlinear_reasoning_enabled = false

type local_counters = {
  mutable local_capability_resolutions : int;
  mutable local_translations : int;
  mutable local_contexts_created : int;
  mutable local_solvers_created : int;
  mutable local_solver_resets : int;
  mutable local_contexts_cleaned : int;
  mutable local_contexts_live : int;
  mutable local_maximum_contexts_live : int;
  mutable local_selected_logics_reversed : string list;
}

type accounting = Global | Local of local_counters

let fresh_local_counters () =
  {
    local_capability_resolutions = 0;
    local_translations = 0;
    local_contexts_created = 0;
    local_solvers_created = 0;
    local_solver_resets = 0;
    local_contexts_cleaned = 0;
    local_contexts_live = 0;
    local_maximum_contexts_live = 0;
    local_selected_logics_reversed = [];
  }

let local_snapshot local =
  {
    capability_resolutions = local.local_capability_resolutions;
    translations = local.local_translations;
    contexts_created = local.local_contexts_created;
    solvers_created = local.local_solvers_created;
    solver_resets = local.local_solver_resets;
    contexts_cleaned = local.local_contexts_cleaned;
    contexts_live = local.local_contexts_live;
    maximum_contexts_live = local.local_maximum_contexts_live;
    selected_logics = List.rev local.local_selected_logics_reversed;
  }

let counters () =
  {
    capability_resolutions = !capability_resolutions;
    translations = !translations;
    contexts_created = !contexts_created;
    solvers_created = !solvers_created;
    solver_resets = !solver_resets;
    contexts_cleaned = !contexts_cleaned;
    contexts_live = !contexts_live;
    maximum_contexts_live = !maximum_contexts_live;
    selected_logics = List.rev !selected_logics_reversed;
  }

let commit_local_counters local =
  capability_resolutions := !capability_resolutions + local.capability_resolutions;
  translations := !translations + local.translations;
  contexts_created := !contexts_created + local.contexts_created;
  solvers_created := !solvers_created + local.solvers_created;
  solver_resets := !solver_resets + local.solver_resets;
  contexts_cleaned := !contexts_cleaned + local.contexts_cleaned;
  contexts_live := !contexts_live + local.contexts_live;
  maximum_contexts_live := max !maximum_contexts_live local.maximum_contexts_live;
  selected_logics_reversed :=
    List.rev_append local.selected_logics !selected_logics_reversed

let reset_counters () =
  if !contexts_live <> 0 then
    invalid_arg "cannot reset direct-Z3 counters while a query context is live";
  capability_resolutions := 0;
  translations := 0;
  contexts_created := 0;
  solvers_created := 0;
  solver_resets := 0;
  contexts_cleaned := 0;
  maximum_contexts_live := 0;
  selected_logics_reversed := []

let is_supported = function
  | Logic_ir.Named_sorts
  | Algebraic_datatypes
  | Uninterpreted_functions
  | Linear_integer_arithmetic
  | Quantifiers
  | Explicit_patterns
  | Quantifier_ids
  | Models
  | Nonlinear_integer_arithmetic ->
      true
  | Bit_vectors | Int_bitvector_conversions -> true

let note_capability_resolution = function
  | Global -> incr capability_resolutions
  | Local local ->
      local.local_capability_resolutions <-
        local.local_capability_resolutions + 1

let note_translation = function
  | Global -> incr translations
  | Local local -> local.local_translations <- local.local_translations + 1

let resolve_capabilities accounting requirements =
  note_capability_resolution accounting;
  match List.filter (fun feature -> not (is_supported feature)) requirements with
  | [] -> Ok ()
  | unsupported -> Error (Unsupported_features unsupported)

let preflight = resolve_capabilities Global

type translated_logic = {
  assertions : Z3.Expr.expr list;
  declaration_snapshot : string list;
  functions : (int * Z3.FuncDecl.func_decl) list;
  projected : (Vir.symbol * Z3.Expr.expr) list;
  bv_projected : (Logic_ir.bv_projection * Z3.Expr.expr) list;
}

type translation_environment = {
  context : Z3.context;
  sorts : (int * Z3.Sort.sort) list ref;
  functions : (int * Z3.FuncDecl.func_decl) list ref;
  binders : (int * Z3.Expr.expr) list;
}

let find_index kind index values =
  match List.assoc_opt index values with
  | Some value -> value
  | None ->
      raise
        (Invalid_argument
           (Printf.sprintf "%s index %d is absent from declaration order" kind
              index))

let decode_detached_width @ portable = fun reference ->
  Bv_width.decode_reference_for_worker reference
  |> Result.fold ~ok:Fun.id ~error:(fun message -> invalid_arg message)

let decimal_double @ portable = fun text ->
  let length = String.length text in
  let output = Bytes.create (length + 1) in
  let carry = ref 0 in
  for index = length - 1 downto 0 do
    let value = ((Char.code text.[index] - Char.code '0') * 2) + !carry in
    Bytes.set output (index + 1) (Char.chr (Char.code '0' + (value mod 10)));
    carry := value / 10
  done;
  if !carry = 0 then Bytes.sub_string output 1 length
  else (
    Bytes.set output 0 (Char.chr (Char.code '0' + !carry));
    Bytes.to_string output)

let decimal_power_of_two @ portable = fun width ->
  let rec loop remaining value =
    if remaining = 0 then value
    else loop (remaining - 1) (decimal_double value)
  in
  loop width "1"

let validate_detached_bv_decimal @ portable = fun width text ->
  let length = String.length text in
  if length > 1234 then Error "BV literal or residue exceeds 1234 bytes"
  else if length = 0 then Error "BV literal or residue is empty"
  else if length > 1 && Char.equal text.[0] '0' then
    Error "BV literal or residue is not canonical decimal"
  else if not (String.for_all (fun c -> c >= '0' && c <= '9') text) then
    Error "BV literal or residue is not unsigned decimal"
  else
    let modulus = decimal_power_of_two (Bv_width.to_int width) in
    if
      length > String.length modulus
      || (length = String.length modulus && String.compare text modulus >= 0)
    then Error "BV literal or residue is outside its width"
    else Ok text

let translate_sort environment = function
  | Logic_ir.Int -> Z3.Arithmetic.Integer.mk_sort environment.context
  | Bool -> Z3.Boolean.mk_sort environment.context
  | Bv width ->
      Z3.BitVector.mk_sort environment.context (Bv_width.to_int width)
  | Named sort ->
      find_index "named sort"
        (Logic_ir.View.named_sort_index sort)
        !(environment.sorts)

let rec native_bound_indices expression =
  match Z3.AST.get_ast_kind (Z3.Expr.ast_of_expr expression) with
  | Z3enums.VAR_AST -> [ Z3.Quantifier.get_index expression ]
  | APP_AST ->
      List.concat_map native_bound_indices (Z3.Expr.get_args expression)
  | NUMERAL_AST | QUANTIFIER_AST | SORT_AST | FUNC_DECL_AST | UNKNOWN_AST -> []

let audit_native_user_quantifier ~universal ~expected_names ~expected_bound
    ~trigger quantifier =
  let require condition message =
    if not condition then invalid_arg ("native user quantifier " ^ message)
  in
  require
    ((universal && Z3.Quantifier.is_universal quantifier)
    || ((not universal) && Z3.Quantifier.is_existential quantifier))
    "kind changed";
  require
    (Z3.Quantifier.get_num_bound quantifier = List.length expected_names)
    "binder count changed";
  let names =
    List.map Z3.Symbol.to_string
      (Z3.Quantifier.get_bound_variable_names quantifier)
  in
  require (names = expected_names) "binder order changed";
  let sorts = Z3.Quantifier.get_bound_variable_sorts quantifier in
  let expected_sorts = List.map Z3.Expr.get_sort expected_bound in
  require
    (List.length sorts = List.length expected_sorts
    && List.for_all2 Z3.Sort.equal sorts expected_sorts)
    "binder sorts changed";
  match (universal, trigger, Z3.Quantifier.get_patterns quantifier) with
  | true, Some expected, [ pattern ] ->
      require (Z3.Quantifier.Pattern.get_num_terms pattern = 1)
        "pattern is not one term";
      let actual = List.hd (Z3.Quantifier.Pattern.get_terms pattern) in
      require
        (Z3.AST.get_ast_kind (Z3.Expr.ast_of_expr actual) = Z3enums.APP_AST)
        "pattern is not application-headed";
      require
        (Z3.FuncDecl.equal (Z3.Expr.get_func_decl actual)
           (Z3.Expr.get_func_decl expected))
        "pattern application head changed";
      require
        (List.sort_uniq Int.compare (native_bound_indices actual)
        = List.init (List.length expected_names) Fun.id)
        "pattern binder coverage changed"
  | false, None, [] -> ()
  | true, _, _ -> invalid_arg "native universal user quantifier pattern changed"
  | false, _, _ ->
      invalid_arg "native existential user quantifier gained a pattern"
[@@unsafe_allow_any_mode_crossing
  "This is operation code over worker-local Z3 handles supplied as arguments; \
   it retains no native handle or process-global solver state."]

let quantifier_audit_operations = { audit = audit_native_user_quantifier }

let rec translate_term environment term =
  let recurse = translate_term environment in
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Integer value ->
      Z3.Arithmetic.Integer.mk_numeral_s environment.context (Z.to_string value)
  | Boolean value -> Z3.Boolean.mk_val environment.context value
  | Bound binder ->
      find_index "binder"
        (Logic_ir.View.binder_index binder)
        environment.binders
  | Apply (function_, arguments) ->
      let declaration =
        find_index "function"
          (Logic_ir.View.function_index function_)
          !(environment.functions)
      in
      Z3.Expr.mk_app environment.context declaration (List.map recurse arguments)
  | Rank_project (_, _, function_, value) ->
      let declaration =
        find_index "rank projection"
          (Logic_ir.View.function_index function_)
          !(environment.functions)
      in
      Z3.Expr.mk_app environment.context declaration [ recurse value ]
  | Add (left, right) ->
      Z3.Arithmetic.mk_add environment.context [ recurse left; recurse right ]
  | Subtract (left, right) ->
      Z3.Arithmetic.mk_sub environment.context [ recurse left; recurse right ]
  | Negate value ->
      Z3.Arithmetic.mk_unary_minus environment.context (recurse value)
  | Multiply (left, right) ->
      Z3.Arithmetic.mk_mul environment.context [ recurse left; recurse right ]
  | Scale (coefficient, value) ->
      Z3.Arithmetic.mk_mul environment.context
        [
          Z3.Arithmetic.Integer.mk_numeral_s environment.context
            (Z.to_string coefficient);
          recurse value;
        ]
  | Less_than (left, right) ->
      Z3.Arithmetic.mk_lt environment.context (recurse left) (recurse right)
  | Less_or_equal (left, right) ->
      Z3.Arithmetic.mk_le environment.context (recurse left) (recurse right)
  | Greater_than (left, right) ->
      Z3.Arithmetic.mk_gt environment.context (recurse left) (recurse right)
  | Greater_or_equal (left, right) ->
      Z3.Arithmetic.mk_ge environment.context (recurse left) (recurse right)
  | Equal (left, right) ->
      Z3.Boolean.mk_eq environment.context (recurse left) (recurse right)
  | Distinct (left, right) ->
      Z3.Boolean.mk_distinct environment.context [ recurse left; recurse right ]
  | Not value -> Z3.Boolean.mk_not environment.context (recurse value)
  | And values ->
      Z3.Boolean.mk_and environment.context (List.map recurse values)
  | Or values -> Z3.Boolean.mk_or environment.context (List.map recurse values)
  | Implies (premise, consequence) ->
      Z3.Boolean.mk_implies environment.context (recurse premise)
        (recurse consequence)
  | Forall_term quantifier ->
      translate_user_quantifier environment true quantifier
  | Exists_term quantifier ->
      translate_user_quantifier environment false quantifier
  | Ite (condition, then_, else_) ->
      Z3.Boolean.mk_ite environment.context (recurse condition) (recurse then_)
        (recurse else_)
  | Bv_literal value ->
      Z3.Expr.mk_numeral_string environment.context
        (Bv_value.canonical_decimal value)
        (Z3.BitVector.mk_sort environment.context
           (Bv_width.to_int value.width))
  | Bv_eq (left, right) ->
      Z3.Boolean.mk_eq environment.context (recurse left) (recurse right)
  | Bv_distinct (left, right) ->
      Z3.Boolean.mk_distinct environment.context [ recurse left; recurse right ]
  | Bv_add_mod (left, right) ->
      Z3.BitVector.mk_add environment.context (recurse left) (recurse right)
  | Bv_sub_mod (left, right) ->
      Z3.BitVector.mk_sub environment.context (recurse left) (recurse right)
  | Bv_not value -> Z3.BitVector.mk_not environment.context (recurse value)
  | Bv_and (left, right) ->
      Z3.BitVector.mk_and environment.context (recurse left) (recurse right)
  | Bv_or (left, right) ->
      Z3.BitVector.mk_or environment.context (recurse left) (recurse right)
  | Bv_xor (left, right) ->
      Z3.BitVector.mk_xor environment.context (recurse left) (recurse right)
  | Bv_ult (left, right) ->
      Z3.BitVector.mk_ult environment.context (recurse left) (recurse right)
  | Bv_ule (left, right) ->
      Z3.BitVector.mk_ule environment.context (recurse left) (recurse right)
  | Bv_ugt (left, right) ->
      Z3.BitVector.mk_ugt environment.context (recurse left) (recurse right)
  | Bv_uge (left, right) ->
      Z3.BitVector.mk_uge environment.context (recurse left) (recurse right)
  | Bv_slt (left, right) ->
      Z3.BitVector.mk_slt environment.context (recurse left) (recurse right)
  | Bv_sle (left, right) ->
      Z3.BitVector.mk_sle environment.context (recurse left) (recurse right)
  | Bv_sgt (left, right) ->
      Z3.BitVector.mk_sgt environment.context (recurse left) (recurse right)
  | Bv_sge (left, right) ->
      Z3.BitVector.mk_sge environment.context (recurse left) (recurse right)
  | Bv_to_int_unsigned value ->
      Z3.BitVector.mk_bv2int environment.context (recurse value) false
  | Bv_to_int_signed value ->
      Z3.BitVector.mk_bv2int environment.context (recurse value) true
  | Int_to_bv_mod (width, value) ->
      Z3.Arithmetic.Integer.mk_int2bv environment.context
        (Bv_width.to_int width) (recurse value)

and translate_user_quantifier environment universal quantifier =
  let binders = Logic_ir.View.user_quantifier_binders quantifier in
  let bound =
    List.map
      (fun binder ->
        let index = Logic_ir.View.binder_index binder in
        ( index,
          Z3.Expr.mk_const_s environment.context
            (Printf.sprintf "verocaml_b%d_%s" index
               (Logic_ir.View.binder_name binder))
            (translate_sort environment (Logic_ir.View.binder_sort binder)) ))
      binders
  in
  let scoped =
    { environment with binders = bound @ environment.binders }
  in
  let body =
    translate_term scoped (Logic_ir.View.user_quantifier_body quantifier)
  in
  let patterns =
    if universal then
      match Logic_ir.View.user_quantifier_trigger quantifier with
      | Some trigger ->
          let trigger = translate_term scoped trigger in
          (Some trigger, [ Z3.Quantifier.mk_pattern environment.context [ trigger ] ])
      | None -> invalid_arg "universal user quantifier has no trigger"
    else
      match Logic_ir.View.user_quantifier_trigger quantifier with
      | None -> (None, [])
      | Some _ -> invalid_arg "existential user quantifier has a trigger"
  in
  let symbol value =
    Some (Z3.Symbol.mk_string environment.context value)
  in
  let quantifier =
    if universal then
      Z3.Quantifier.mk_forall_const environment.context (List.map snd bound) body None
        (snd patterns) []
        (symbol (Logic_ir.View.user_quantifier_qid quantifier))
        (symbol (Logic_ir.View.user_quantifier_skid quantifier))
    else
      quantifier_worker_operations.exists environment.context (List.map snd bound) body
        None (snd patterns) []
        (symbol (Logic_ir.View.user_quantifier_qid quantifier))
        (symbol (Logic_ir.View.user_quantifier_skid quantifier))
  in
  quantifier_audit_operations.audit ~universal
    ~expected_names:
      (List.map
         (fun binder ->
           Printf.sprintf "verocaml_b%d_%s"
             (Logic_ir.View.binder_index binder)
             (Logic_ir.View.binder_name binder))
         binders)
    ~expected_bound:(List.map snd bound)
    ~trigger:(fst patterns) quantifier;
  Z3.Quantifier.expr_of_quantifier quantifier

let translate_axiom environment axiom =
  let binder_expressions =
    Logic_ir.View.axiom_binders axiom
    |> List.map (fun binder ->
           let name =
             Printf.sprintf "verocaml_b%d_%s"
               (Logic_ir.View.binder_index binder)
               (Logic_ir.View.binder_name binder)
           in
           let expression =
             Z3.Expr.mk_const_s environment.context name
               (translate_sort environment (Logic_ir.View.binder_sort binder))
           in
           (Logic_ir.View.binder_index binder, expression))
  in
  let scoped_environment =
    {
      environment with
      binders = binder_expressions @ environment.binders;
    }
  in
  let body =
    translate_term scoped_environment (Logic_ir.View.axiom_body axiom)
  in
  let patterns =
    Logic_ir.View.axiom_patterns axiom
    |> List.map (fun terms ->
           Z3.Quantifier.mk_pattern environment.context
             (List.map (translate_term scoped_environment) terms))
  in
  let symbol value = Some (Z3.Symbol.mk_string environment.context value) in
  Z3.Quantifier.mk_forall_const environment.context
    (List.map snd binder_expressions)
    body None patterns []
    (symbol (Logic_ir.View.axiom_qid axiom))
    (symbol (Logic_ir.View.axiom_skid axiom))
  |> Z3.Quantifier.expr_of_quantifier

let translate_logic_query context query =
  let environment =
    { context; sorts = ref []; functions = ref []; binders = [] }
  in
  let declaration_snapshot = ref [] in
  let datatypes = Logic_ir.View.datatypes query in
  let datatype_for_sort sort =
    List.find_opt
      (fun datatype ->
        Logic_ir.View.named_sort_index
          (Logic_ir.View.datatype_sort datatype)
        = Logic_ir.View.named_sort_index sort)
      datatypes
  in
  let datatype_function index =
    List.exists
      (fun datatype ->
        Logic_ir.View.datatype_constructors datatype
        |> List.exists (fun constructor ->
               Logic_ir.View.function_index
                 (Logic_ir.View.datatype_constructor_symbol constructor)
               = index
               || Logic_ir.View.function_index
                    (Logic_ir.View.datatype_recognizer_symbol constructor)
                  = index
               || Logic_ir.View.datatype_fields constructor
                  |> List.exists (fun field ->
                         Logic_ir.View.function_index
                           (Logic_ir.View.datatype_field_symbol field)
                         = index)))
      datatypes
  in
  Logic_ir.View.declarations query
  |> List.iter (function
       | Logic_ir.View.Sort_declaration sort ->
           let index = Logic_ir.View.named_sort_index sort in
           let name = Logic_ir.View.named_sort_name sort in
           (match datatype_for_sort sort with
           | None ->
               let backend_sort =
                 Z3.Sort.mk_uninterpreted_s context name
               in
               environment.sorts :=
                 (index, backend_sort) :: !(environment.sorts);
               declaration_snapshot :=
                 Printf.sprintf "sort:%d:%s" index name
                 :: !declaration_snapshot
           | Some datatype ->
               let detached = Z3_datatype_private.detach datatype in
               let bindings =
                 datatype_worker_operations.declare ~context
                   ~resolve_named_sort:(fun index ->
                     find_index "named datatype dependency" index
                       !(environment.sorts))
                   ~resolve_bv_sort:(fun reference ->
                     Z3.BitVector.mk_sort context
                       (Bv_width.to_int (decode_detached_width reference)))
                   detached
                 |> Result.fold ~ok:Fun.id
                      ~error:(fun message -> invalid_arg message)
               in
               environment.sorts :=
                 bindings.sorts @ !(environment.sorts);
               environment.functions :=
                 bindings.functions @ !(environment.functions);
               declaration_snapshot :=
                 Printf.sprintf "datatype:%d:%s" detached.sort_index
                   detached.sort_name
                 :: !declaration_snapshot)
       | Function_declaration function_ ->
           let index = Logic_ir.View.function_index function_ in
           if datatype_function index then ()
           else
           let name = Logic_ir.View.function_name function_ in
           let domain =
             Logic_ir.View.function_domain function_
             |> List.map (translate_sort environment)
           in
           let range =
             translate_sort environment
               (Logic_ir.View.function_range function_)
           in
           let declaration =
             Z3.FuncDecl.mk_func_decl_s context name domain range
           in
           environment.functions :=
             (index, declaration) :: !(environment.functions);
           declaration_snapshot :=
             Printf.sprintf "fun:%d:%s:(%s)->%s" index name
               (String.concat ","
                  (List.map Logic_ir.sort_to_string
                     (Logic_ir.View.function_domain function_)))
               (Logic_ir.sort_to_string
                  (Logic_ir.View.function_range function_))
             :: !declaration_snapshot);
  let axioms =
    List.map (translate_axiom environment) (Logic_ir.View.axioms query)
  in
  let assertions =
    List.map (translate_term environment) (Logic_ir.View.assertions query)
  in
  {
    assertions = axioms @ assertions;
    declaration_snapshot = List.rev !declaration_snapshot;
    functions = !(environment.functions);
    projected = [];
    bv_projected =
      List.map
        (fun projection ->
          ( projection,
            translate_term environment
              (Logic_ir.View.bv_projection_term projection) ))
        (Logic_ir.View.bv_projections query);
  }

let detach_width width =
  Bv_width.encode_reference
    (Bv_backend_capability_receipt_private.capability ())
    width
  |> Result.fold ~ok:Fun.id ~error:(fun message -> invalid_arg message)

let detach_sort = function
  | Logic_ir.Int -> Detached_int_sort
  | Bool -> Detached_bool_sort
  | Bv width -> Detached_bv_sort (detach_width width)
  | Named sort ->
      Detached_named_sort (Logic_ir.View.named_sort_index sort)

let rec detach_term term =
  let recurse = detach_term in
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Integer value ->
      Detached_integer_term (Z.to_string value)
  | Boolean value -> Detached_boolean_term value
  | Bound binder ->
      Detached_bound_term (Logic_ir.View.binder_index binder)
  | Apply (function_, arguments) ->
      Detached_apply_term
        ( Logic_ir.View.function_index function_,
          List.map recurse arguments )
  | Rank_project (_, _, function_, value) ->
      Detached_apply_term
        (Logic_ir.View.function_index function_, [ recurse value ])
  | Add (left, right) -> Detached_add_term (recurse left, recurse right)
  | Subtract (left, right) ->
      Detached_subtract_term (recurse left, recurse right)
  | Negate value -> Detached_negate_term (recurse value)
  | Multiply (left, right) ->
      Detached_multiply_term (recurse left, recurse right)
  | Scale (coefficient, value) ->
      Detached_scale_term (Z.to_string coefficient, recurse value)
  | Less_than (left, right) ->
      Detached_less_than_term (recurse left, recurse right)
  | Less_or_equal (left, right) ->
      Detached_less_or_equal_term (recurse left, recurse right)
  | Greater_than (left, right) ->
      Detached_greater_than_term (recurse left, recurse right)
  | Greater_or_equal (left, right) ->
      Detached_greater_or_equal_term (recurse left, recurse right)
  | Equal (left, right) ->
      Detached_equal_term (recurse left, recurse right)
  | Distinct (left, right) ->
      Detached_distinct_term (recurse left, recurse right)
  | Not value -> Detached_not_term (recurse value)
  | And values -> Detached_and_term (List.map recurse values)
  | Or values -> Detached_or_term (List.map recurse values)
  | Implies (premise, consequence) ->
      Detached_implies_term (recurse premise, recurse consequence)
  | Forall_term quantifier ->
      let detached_binders =
        Logic_ir.View.user_quantifier_binders quantifier
        |> List.map (fun binder ->
               ( Logic_ir.View.binder_index binder,
                 Logic_ir.View.binder_name binder,
                 detach_sort (Logic_ir.View.binder_sort binder) ))
      in
      let trigger =
        match Logic_ir.View.user_quantifier_trigger quantifier with
        | Some trigger -> recurse trigger
        | None -> invalid_arg "universal user quantifier has no trigger"
      in
      Detached_forall_term
        ( detached_binders,
          recurse (Logic_ir.View.user_quantifier_body quantifier),
          trigger,
          Logic_ir.View.user_quantifier_qid quantifier,
          Logic_ir.View.user_quantifier_skid quantifier )
  | Exists_term quantifier ->
      let binders =
        Logic_ir.View.user_quantifier_binders quantifier
        |> List.map (fun binder ->
               ( Logic_ir.View.binder_index binder,
                 Logic_ir.View.binder_name binder,
                 detach_sort (Logic_ir.View.binder_sort binder) ))
      in
      (match Logic_ir.View.user_quantifier_trigger quantifier with
      | Some _ -> invalid_arg "existential user quantifier has a trigger"
      | None ->
          Detached_exists_term
            ( binders,
              recurse (Logic_ir.View.user_quantifier_body quantifier),
              Logic_ir.View.user_quantifier_qid quantifier,
              Logic_ir.View.user_quantifier_skid quantifier ))
  | Ite (condition, then_, else_) ->
      Detached_ite_term
        (recurse condition, recurse then_, recurse else_)
  | Bv_literal value ->
      Detached_bv_literal_term
        (detach_width value.Bv_value.width, Bv_value.canonical_decimal value)
  | Bv_eq (left, right) -> Detached_bv_eq_term (recurse left, recurse right)
  | Bv_distinct (left, right) ->
      Detached_bv_distinct_term (recurse left, recurse right)
  | Bv_add_mod (left, right) ->
      Detached_bv_add_mod_term (recurse left, recurse right)
  | Bv_sub_mod (left, right) ->
      Detached_bv_sub_mod_term (recurse left, recurse right)
  | Bv_not value -> Detached_bv_not_term (recurse value)
  | Bv_and (left, right) -> Detached_bv_and_term (recurse left, recurse right)
  | Bv_or (left, right) -> Detached_bv_or_term (recurse left, recurse right)
  | Bv_xor (left, right) -> Detached_bv_xor_term (recurse left, recurse right)
  | Bv_ult (left, right) -> Detached_bv_ult_term (recurse left, recurse right)
  | Bv_ule (left, right) -> Detached_bv_ule_term (recurse left, recurse right)
  | Bv_ugt (left, right) -> Detached_bv_ugt_term (recurse left, recurse right)
  | Bv_uge (left, right) -> Detached_bv_uge_term (recurse left, recurse right)
  | Bv_slt (left, right) -> Detached_bv_slt_term (recurse left, recurse right)
  | Bv_sle (left, right) -> Detached_bv_sle_term (recurse left, recurse right)
  | Bv_sgt (left, right) -> Detached_bv_sgt_term (recurse left, recurse right)
  | Bv_sge (left, right) -> Detached_bv_sge_term (recurse left, recurse right)
  | Bv_to_int_unsigned value ->
      Detached_bv_to_int_unsigned_term (recurse value)
  | Bv_to_int_signed value -> Detached_bv_to_int_signed_term (recurse value)
  | Int_to_bv_mod (width, value) ->
      Detached_int_to_bv_mod_term (detach_width width, recurse value)

let detach_axiom axiom =
  {
    detached_binders =
      Logic_ir.View.axiom_binders axiom
      |> List.map (fun binder ->
             ( Logic_ir.View.binder_index binder,
               Logic_ir.View.binder_name binder,
               detach_sort (Logic_ir.View.binder_sort binder) ));
    detached_body = detach_term (Logic_ir.View.axiom_body axiom);
    detached_patterns =
      List.map (List.map detach_term) (Logic_ir.View.axiom_patterns axiom);
    detached_qid = Logic_ir.View.axiom_qid axiom;
    detached_skid = Logic_ir.View.axiom_skid axiom;
  }

let detach_query_with_projections query projections =
  let datatypes = Logic_ir.View.datatypes query in
  let datatype_for_sort sort =
    List.find_opt
      (fun datatype ->
        Logic_ir.View.named_sort_index
          (Logic_ir.View.datatype_sort datatype)
        = Logic_ir.View.named_sort_index sort)
      datatypes
  in
  let datatype_function index =
    List.exists
      (fun datatype ->
        Logic_ir.View.datatype_constructors datatype
        |> List.exists (fun constructor ->
               Logic_ir.View.function_index
                 (Logic_ir.View.datatype_constructor_symbol constructor)
               = index
               || Logic_ir.View.function_index
                    (Logic_ir.View.datatype_recognizer_symbol constructor)
                  = index
               || Logic_ir.View.datatype_fields constructor
                  |> List.exists (fun field ->
                         Logic_ir.View.function_index
                           (Logic_ir.View.datatype_field_symbol field)
                         = index)))
      datatypes
  in
  let declarations =
    Logic_ir.View.declarations query
    |> List.filter_map (function
         | Logic_ir.View.Sort_declaration sort ->
             Some
               (match datatype_for_sort sort with
               | None ->
                   Detached_sort_declaration
                     ( Logic_ir.View.named_sort_index sort,
                       Logic_ir.View.named_sort_name sort )
               | Some datatype ->
                   Detached_datatype_declaration
                     (Z3_datatype_private.detach datatype))
         | Function_declaration function_ ->
             let index = Logic_ir.View.function_index function_ in
             if datatype_function index then None
             else
               Some
                 (Detached_function_declaration
                    ( index,
                      Logic_ir.View.function_name function_,
                      List.map detach_sort
                        (Logic_ir.View.function_domain function_),
                      detach_sort
                        (Logic_ir.View.function_range function_) )))
  in
  let bv_projections =
    Logic_ir.View.bv_projections query
    |> List.map (fun projection ->
           Detached_bv_projection
             ( Logic_ir.View.bv_projection_identity projection,
               detach_width (Logic_ir.View.bv_projection_width projection),
               detach_term (Logic_ir.View.bv_projection_term projection) ))
  in
  Detached_query
    (Marshal.to_string
       {
         detached_requirements = Logic_ir.requirements query;
         detached_declarations = declarations;
         detached_axioms =
           List.map detach_axiom (Logic_ir.View.axioms query);
         detached_assertions =
           List.map detach_term (Logic_ir.View.assertions query);
         detached_projections = projections @ bv_projections;
       }
       [])

let detach_query query = detach_query_with_projections query []

let detach_bv_query query =
  ( detach_query query,
    Logic_ir.View.bv_projections query
    |> List.map Logic_ir.View.bv_projection_identity )

let malformed_detached_bv_query_for_testing ~width ~other_width defect =
  let reference = detach_width width
  and other_reference = detach_width other_width in
  let literal reference value = Detached_bv_literal_term (reference, value) in
  let plan =
    match defect with
    | Mixed_bv_add_widths ->
        { detached_requirements = [ Logic_ir.Bit_vectors ];
          detached_declarations = [];
          detached_axioms = [];
          detached_assertions =
            [ Detached_bv_eq_term
                ( Detached_bv_add_mod_term
                    (literal reference "0", literal other_reference "0"),
                  literal reference "0" ) ];
          detached_projections = [] }
    | Omitted_bv_capability ->
        { detached_requirements = [];
          detached_declarations = [];
          detached_axioms = [];
          detached_assertions =
            [ Detached_bv_eq_term
                (literal reference "0", literal reference "0") ];
          detached_projections = [] }
    | Wrong_int_to_bv_operand ->
        { detached_requirements =
            [ Logic_ir.Bit_vectors; Logic_ir.Int_bitvector_conversions ];
          detached_declarations = [];
          detached_axioms = [];
          detached_assertions =
            [ Detached_bv_eq_term
                ( Detached_int_to_bv_mod_term
                    (reference, Detached_boolean_term true),
                  literal reference "0" ) ];
          detached_projections = [] }
    | Bv_projection_width_mismatch ->
        { detached_requirements = [ Logic_ir.Bit_vectors; Logic_ir.Models ];
          detached_declarations = [];
          detached_axioms = [];
          detached_assertions = [ Detached_boolean_term true ];
          detached_projections =
            [ Detached_bv_projection
                ("wrong-width", other_reference, literal reference "0") ] }
  in
  Detached_query (Marshal.to_string plan [])

let projection_sort (symbol : Vir.symbol) =
  match symbol.sort with
  | Vir.Integer -> Detached_project_integer
  | Boolean -> Detached_project_boolean
  | Bit_vector _ ->
      invalid_arg "BV symbols require a width-bearing detached projection"
  | Aggregate _ -> Detached_project_aggregate
  | Parametric _ ->
      invalid_arg "parametric symbols must not be projected through aggregate ABI"

let detach_vir ~requires obligation =
  match Vir_logic_ir_translation_private.translate ~requires obligation with
  | Error message -> Error (Malformed_vir message)
  | Ok translation ->
      let projected = Vir_logic_ir_translation_private.projected translation in
      let projections, metadata =
        projected
        |> List.mapi (fun order ((symbol : Vir.symbol), function_) ->
               let identity =
                 Printf.sprintf "vir-model-v1:f%d:s%d:o%d"
                   obligation.Vir.function_ref.function_index symbol.symbol_id
                   order
               in
               let projection, metadata =
                 match symbol.sort with
                 | Vir.Bit_vector width ->
                     let width_reference = detach_width width in
                     ( Detached_bv_projection
                         ( identity,
                           width_reference,
                           Detached_apply_term
                             (Logic_ir.View.function_index function_, []) ),
                       {
                         projection_symbol = symbol;
                         projection_identity = identity;
                         projection_order = order;
                         projection_width = Some width;
                         projection_width_reference = Some width_reference;
                       } )
                 | Vir.Integer | Boolean | Aggregate _ ->
                     ( Detached_function_projection
                         ( identity,
                           Logic_ir.View.function_index function_,
                           projection_sort symbol ),
                       {
                         projection_symbol = symbol;
                         projection_identity = identity;
                         projection_order = order;
                         projection_width = None;
                         projection_width_reference = None;
                       } )
                 | Vir.Parametric _ ->
                     invalid_arg
                       "parametric symbols cannot enter the model manifest"
               in
               [%log.trace "issued detached VIR model projection metadata"
                 ~stage:(Delator.Field.string "vir-model-manifest")
                 ~projection_order:(Delator.Field.int order)
                 ~symbol_id:(Delator.Field.int symbol.symbol_id)
                 ~identity_bytes:(Delator.Field.int (String.length identity))
                 ~width:
                   (Delator.Field.int
                      (match metadata.projection_width with
                      | None -> 0
                      | Some width -> Bv_width.to_int width))
                 ~decision:(Delator.Field.string "issued")];
               (projection, metadata))
        |> List.split
      in
      [%log.debug "completed detached VIR model projection manifest"
        ~stage:(Delator.Field.string "vir-model-manifest")
        ~function_index:
          (Delator.Field.int obligation.function_ref.function_index)
        ~projection_count:(Delator.Field.int (List.length metadata))
        ~decision:(Delator.Field.string "issued")];
      Ok
        ( detach_query_with_projections
            (Vir_logic_ir_translation_private.query translation)
            projections,
          metadata )

let projection_symbol metadata = metadata.projection_symbol
let projection_identity metadata = metadata.projection_identity
let projection_order metadata = metadata.projection_order
let projection_width metadata = metadata.projection_width
let projection_width_reference metadata = metadata.projection_width_reference

type detached_translation_environment = {
  detached_context : Z3.context;
  detached_sorts : (int * Z3.Sort.sort) list ref;
  detached_functions : (int * Z3.FuncDecl.func_decl) list ref;
  detached_bound : (int * Z3.Expr.expr) list;
}

type detached_preflight_sort =
  | Preflight_int
  | Preflight_bool
  | Preflight_bv of string
  | Preflight_named of int

type detached_preflight_environment = {
  preflight_sorts : int list;
  preflight_functions :
    (int * (detached_preflight_sort list * detached_preflight_sort)) list;
  preflight_bound : (int * detached_preflight_sort) list;
  preflight_features : Logic_ir.feature list ref;
}

let preflight_fail @ portable = fun reason ->
  raise (Invalid_argument ("detached typed preflight: " ^ reason))

let preflight_add_feature @ portable = fun environment feature ->
  if not (List.mem feature !(environment.preflight_features)) then
    environment.preflight_features := feature :: !(environment.preflight_features)

let preflight_sort_equal @ portable = fun left right ->
  match (left, right) with
  | Preflight_int, Preflight_int | Preflight_bool, Preflight_bool -> true
  | Preflight_bv left, Preflight_bv right -> String.equal left right
  | Preflight_named left, Preflight_named right -> left = right
  | (Preflight_int | Preflight_bool | Preflight_bv _ | Preflight_named _), _ ->
      false

let preflight_sort_name @ portable = function
  | Preflight_int -> "Int"
  | Preflight_bool -> "Bool"
  | Preflight_bv _ -> "BV"
  | Preflight_named _ -> "named"

let preflight_expect_sort @ portable = fun expected observed operation ->
  if not (preflight_sort_equal expected observed) then
    preflight_fail
      (Printf.sprintf "%s expected %s but found %s" operation
         (preflight_sort_name expected) (preflight_sort_name observed))

let preflight_detached_sort @ portable = fun environment -> function
  | Detached_int_sort -> Preflight_int
  | Detached_bool_sort -> Preflight_bool
  | Detached_bv_sort reference ->
      ignore (decode_detached_width reference);
      preflight_add_feature environment Logic_ir.Bit_vectors;
      Preflight_bv reference
  | Detached_named_sort index ->
      if not (List.mem index environment.preflight_sorts) then
        preflight_fail "named sort is absent from declaration order";
      preflight_add_feature environment Logic_ir.Named_sorts;
      Preflight_named index

let preflight_features_of_sort @ portable = fun environment -> function
  | Preflight_bv _ ->
      preflight_add_feature environment Logic_ir.Bit_vectors
  | Preflight_named _ ->
      preflight_add_feature environment Logic_ir.Named_sorts
  | Preflight_int | Preflight_bool -> ()

let preflight_exact_arguments @ portable = fun expected observed ->
  let rec loop expected observed =
    match (expected, observed) with
    | [], [] -> ()
    | expected :: expected_rest, observed :: observed_rest ->
        preflight_expect_sort expected observed "function application";
        loop expected_rest observed_rest
    | [], _ :: _ | _ :: _, [] ->
        preflight_fail "function application arity changed"
  in
  loop expected observed

let preflight_binders @ portable = fun environment binders ->
  if binders = [] then preflight_fail "quantifier binder vector is empty";
  let rec extend scoped seen_indices seen_names = function
    | [] -> scoped
    | (index, name, sort) :: rest ->
        if List.mem index seen_indices || List.mem name seen_names then
          preflight_fail "quantifier binder vector contains a duplicate";
        let sort = preflight_detached_sort scoped sort in
        extend
          { scoped with
            preflight_bound = (index, sort) :: scoped.preflight_bound }
          (index :: seen_indices) (name :: seen_names) rest
  in
  extend environment [] [] binders

let rec preflight_detached_term @ portable = fun environment term ->
  let infer = preflight_detached_term environment in
  let integer_binary operation feature left right result =
    let left = infer left and right = infer right in
    preflight_expect_sort Preflight_int left operation;
    preflight_expect_sort Preflight_int right operation;
    preflight_add_feature environment feature;
    result
  in
  let boolean_binary operation left right =
    let left = infer left and right = infer right in
    preflight_expect_sort Preflight_bool left operation;
    preflight_expect_sort Preflight_bool right operation;
    Preflight_bool
  in
  let same_binary operation left right result =
    let left = infer left and right = infer right in
    preflight_expect_sort left right operation;
    result left
  in
  let bv_unary operation value result =
    match infer value with
    | Preflight_bv reference ->
        preflight_add_feature environment Logic_ir.Bit_vectors;
        result reference
    | observed ->
        preflight_fail
          (Printf.sprintf "%s expected BV but found %s" operation
             (preflight_sort_name observed))
  in
  let bv_binary operation left right result =
    match (infer left, infer right) with
    | Preflight_bv left, Preflight_bv right when String.equal left right ->
        preflight_add_feature environment Logic_ir.Bit_vectors;
        result left
    | Preflight_bv _, Preflight_bv _ ->
        preflight_fail (operation ^ " operands have unequal authenticated widths")
    | left, right ->
        preflight_fail
          (Printf.sprintf "%s expected two BV operands but found %s and %s"
             operation (preflight_sort_name left) (preflight_sort_name right))
  in
  match term with
  | Detached_integer_term _ -> Preflight_int
  | Detached_boolean_term _ -> Preflight_bool
  | Detached_bound_term index -> (
      match List.assoc_opt index environment.preflight_bound with
      | Some sort ->
          preflight_features_of_sort environment sort;
          sort
      | None -> preflight_fail "bound term is absent from quantifier scope")
  | Detached_apply_term (index, values) -> (
      match List.assoc_opt index environment.preflight_functions with
      | None -> preflight_fail "function is absent from declaration order"
      | Some (domain, range) ->
          let observed = List.map infer values in
          preflight_exact_arguments domain observed;
          preflight_add_feature environment Logic_ir.Uninterpreted_functions;
          List.iter (preflight_features_of_sort environment) (range :: domain);
          range)
  | Detached_add_term (left, right) | Detached_subtract_term (left, right) ->
      integer_binary "integer additive operation"
        Logic_ir.Linear_integer_arithmetic left right Preflight_int
  | Detached_negate_term value ->
      let observed = infer value in
      preflight_expect_sort Preflight_int observed "integer negation";
      preflight_add_feature environment Logic_ir.Linear_integer_arithmetic;
      Preflight_int
  | Detached_multiply_term (left, right) ->
      integer_binary "integer multiplication"
        Logic_ir.Nonlinear_integer_arithmetic left right Preflight_int
  | Detached_scale_term (_, value) ->
      let observed = infer value in
      preflight_expect_sort Preflight_int observed "integer scaling";
      preflight_add_feature environment Logic_ir.Linear_integer_arithmetic;
      Preflight_int
  | Detached_less_than_term (left, right)
  | Detached_less_or_equal_term (left, right)
  | Detached_greater_than_term (left, right)
  | Detached_greater_or_equal_term (left, right) ->
      integer_binary "integer comparison" Logic_ir.Linear_integer_arithmetic
        left right Preflight_bool
  | Detached_equal_term (left, right)
  | Detached_distinct_term (left, right) ->
      same_binary "generic equality" left right (function
        | Preflight_bv _ ->
            preflight_fail "generic equality cannot encode a BV operation"
        | Preflight_int | Preflight_bool | Preflight_named _ -> Preflight_bool)
  | Detached_not_term value ->
      let observed = infer value in
      preflight_expect_sort Preflight_bool observed "Boolean negation";
      Preflight_bool
  | Detached_and_term values | Detached_or_term values ->
      if values = [] then preflight_fail "Boolean connective has no operands";
      List.iter
        (fun value ->
          preflight_expect_sort Preflight_bool (infer value)
            "Boolean connective")
        values;
      Preflight_bool
  | Detached_implies_term (left, right) ->
      boolean_binary "Boolean implication" left right
  | Detached_forall_term (binders, body, trigger, _, _) ->
      let scoped = preflight_binders environment binders in
      preflight_expect_sort Preflight_bool
        (preflight_detached_term scoped body)
        "universal quantifier body";
      ignore (preflight_detached_term scoped trigger);
      preflight_add_feature environment Logic_ir.Quantifiers;
      preflight_add_feature environment Logic_ir.Explicit_patterns;
      preflight_add_feature environment Logic_ir.Quantifier_ids;
      Preflight_bool
  | Detached_exists_term (binders, body, _, _) ->
      let scoped = preflight_binders environment binders in
      preflight_expect_sort Preflight_bool
        (preflight_detached_term scoped body)
        "existential quantifier body";
      preflight_add_feature environment Logic_ir.Quantifiers;
      preflight_add_feature environment Logic_ir.Quantifier_ids;
      Preflight_bool
  | Detached_ite_term (condition, then_, else_) ->
      preflight_expect_sort Preflight_bool (infer condition) "ITE condition";
      let then_sort = infer then_ and else_sort = infer else_ in
      preflight_expect_sort then_sort else_sort "ITE branches";
      then_sort
  | Detached_bv_literal_term (reference, unsigned_bits) ->
      let width = decode_detached_width reference in
      ignore
        (validate_detached_bv_decimal width unsigned_bits
        |> Result.fold ~ok:Fun.id ~error:(fun message -> invalid_arg message));
      preflight_add_feature environment Logic_ir.Bit_vectors;
      Preflight_bv reference
  | Detached_bv_eq_term (left, right)
  | Detached_bv_distinct_term (left, right)
  | Detached_bv_ult_term (left, right)
  | Detached_bv_ule_term (left, right)
  | Detached_bv_ugt_term (left, right)
  | Detached_bv_uge_term (left, right)
  | Detached_bv_slt_term (left, right)
  | Detached_bv_sle_term (left, right)
  | Detached_bv_sgt_term (left, right)
  | Detached_bv_sge_term (left, right) ->
      bv_binary "BV predicate" left right (fun _ -> Preflight_bool)
  | Detached_bv_add_mod_term (left, right)
  | Detached_bv_sub_mod_term (left, right)
  | Detached_bv_and_term (left, right)
  | Detached_bv_or_term (left, right)
  | Detached_bv_xor_term (left, right) ->
      bv_binary "BV binary operation" left right (fun reference ->
          Preflight_bv reference)
  | Detached_bv_not_term value ->
      bv_unary "BV complement" value (fun reference -> Preflight_bv reference)
  | Detached_bv_to_int_unsigned_term value
  | Detached_bv_to_int_signed_term value ->
      bv_unary "BV-to-Int conversion" value (fun _ ->
          preflight_add_feature environment Logic_ir.Int_bitvector_conversions;
          Preflight_int)
  | Detached_int_to_bv_mod_term (reference, value) ->
      ignore (decode_detached_width reference);
      preflight_expect_sort Preflight_int (infer value) "Int-to-BV conversion";
      preflight_add_feature environment Logic_ir.Bit_vectors;
      preflight_add_feature environment Logic_ir.Int_bitvector_conversions;
      Preflight_bv reference

let preflight_datatype_sort @ portable = fun environment self -> function
  | Z3_datatype_private.Int_sort -> Preflight_int
  | Bool_sort -> Preflight_bool
  | Bv_sort reference ->
      ignore (decode_detached_width reference);
      preflight_add_feature environment Logic_ir.Bit_vectors;
      Preflight_bv reference
  | Named_sort index ->
      if not (List.mem index environment.preflight_sorts) then
        preflight_fail "datatype field named sort is absent";
      preflight_add_feature environment Logic_ir.Named_sorts;
      Preflight_named index
  | Recursive_self -> Preflight_named self

let preflight_add_function @ portable = fun environment index signature ->
  if List.mem_assoc index environment.preflight_functions then
    preflight_fail "function declaration identity is duplicated";
  { environment with
    preflight_functions = (index, signature) :: environment.preflight_functions }

let preflight_detached_plan @ portable = fun plan ->
  let features = ref [] in
  let initial =
    { preflight_sorts = []; preflight_functions = []; preflight_bound = [];
      preflight_features = features }
  in
  let environment =
    List.fold_left
      (fun environment declaration ->
        match declaration with
        | Detached_sort_declaration (index, _) ->
            if List.mem index environment.preflight_sorts then
              preflight_fail "sort declaration identity is duplicated";
            preflight_add_feature environment Logic_ir.Named_sorts;
            { environment with preflight_sorts = index :: environment.preflight_sorts }
        | Detached_function_declaration (index, _, domain, range) ->
            let domain =
              List.map (preflight_detached_sort environment) domain
            and range = preflight_detached_sort environment range in
            preflight_add_feature environment Logic_ir.Uninterpreted_functions;
            preflight_add_function environment index (domain, range)
        | Detached_datatype_declaration datatype ->
            let self = datatype.Z3_datatype_private.sort_index in
            if List.mem self environment.preflight_sorts then
              preflight_fail "datatype sort identity is duplicated";
            preflight_add_feature environment Logic_ir.Named_sorts;
            preflight_add_feature environment Logic_ir.Algebraic_datatypes;
            preflight_add_feature environment Logic_ir.Uninterpreted_functions;
            let environment =
              { environment with
                preflight_sorts = self :: environment.preflight_sorts }
            in
            List.fold_left
              (fun environment constructor ->
                let fields =
                  List.map
                    (fun field ->
                      preflight_datatype_sort environment self
                        field.Z3_datatype_private.field_sort)
                    constructor.Z3_datatype_private.fields
                in
                let environment =
                  preflight_add_function environment
                    constructor.Z3_datatype_private.constructor_function_index
                    (fields, Preflight_named self)
                in
                let environment =
                  preflight_add_function environment
                    constructor.Z3_datatype_private.recognizer_function_index
                    ([ Preflight_named self ], Preflight_bool)
                in
                List.fold_left2
                  (fun environment field range ->
                    preflight_add_function environment
                      field.Z3_datatype_private.field_function_index
                      ([ Preflight_named self ], range))
                  environment constructor.Z3_datatype_private.fields fields)
              environment datatype.Z3_datatype_private.constructors)
      initial plan.detached_declarations
  in
  List.iter
    (fun axiom ->
      let scoped = preflight_binders environment axiom.detached_binders in
      preflight_expect_sort Preflight_bool
        (preflight_detached_term scoped axiom.detached_body)
        "axiom body";
      if axiom.detached_patterns = [] then
        preflight_fail "axiom has no explicit pattern";
      List.iter
        (fun pattern ->
          if pattern = [] then preflight_fail "axiom pattern is empty";
          List.iter (fun term -> ignore (preflight_detached_term scoped term)) pattern)
        axiom.detached_patterns;
      preflight_add_feature environment Logic_ir.Quantifiers;
      preflight_add_feature environment Logic_ir.Explicit_patterns;
      preflight_add_feature environment Logic_ir.Quantifier_ids)
    plan.detached_axioms;
  List.iter
    (fun assertion ->
      preflight_expect_sort Preflight_bool
        (preflight_detached_term environment assertion)
        "query assertion")
    plan.detached_assertions;
  List.iter
    (function
      | Detached_function_projection (_, index, expected) -> (
          match List.assoc_opt index environment.preflight_functions with
          | Some ([], range) ->
              let expected =
                match expected with
                | Detached_project_integer -> Preflight_int
                | Detached_project_boolean -> Preflight_bool
                | Detached_project_aggregate -> (
                    match range with
                    | Preflight_named _ | Preflight_int -> range
                    | Preflight_bool | Preflight_bv _ ->
                        preflight_fail
                          "aggregate projection does not have an aggregate-compatible sort")
              in
              preflight_expect_sort expected range "function projection"
          | Some (_ :: _, _) ->
              preflight_fail "projected function is not a constant"
          | None -> preflight_fail "projected function is absent")
      | Detached_bv_projection (_, reference, term) ->
          ignore (decode_detached_width reference);
          let observed = preflight_detached_term environment term in
          preflight_expect_sort (Preflight_bv reference) observed "BV projection";
          preflight_add_feature environment Logic_ir.Models)
    plan.detached_projections;
  let inferred = !features in
  List.iter
    (fun feature ->
      if not (List.mem feature plan.detached_requirements) then
        preflight_fail
          "transported requirements omitted an inferred feature")
    inferred;
  List.fold_left
    (fun validated feature ->
      if List.mem feature validated then validated else feature :: validated)
    inferred plan.detached_requirements

let translate_detached_sort environment = function
  | Detached_int_sort ->
      Z3.Arithmetic.Integer.mk_sort environment.detached_context
  | Detached_bool_sort ->
      Z3.Boolean.mk_sort environment.detached_context
  | Detached_bv_sort reference ->
      Z3.BitVector.mk_sort environment.detached_context
        (Bv_width.to_int (decode_detached_width reference))
  | Detached_named_sort index ->
      find_index "detached named sort" index !(environment.detached_sorts)

let rec translate_detached_term environment = function
  | Detached_integer_term value ->
      Z3.Arithmetic.Integer.mk_numeral_s environment.detached_context value
  | Detached_boolean_term value ->
      Z3.Boolean.mk_val environment.detached_context value
  | Detached_bound_term index ->
      find_index "detached binder" index environment.detached_bound
  | Detached_apply_term (index, arguments) ->
      Z3.Expr.mk_app environment.detached_context
        (find_index "detached function" index
           !(environment.detached_functions))
        (List.map (translate_detached_term environment) arguments)
  | Detached_add_term (left, right) ->
      Z3.Arithmetic.mk_add environment.detached_context
        [
          translate_detached_term environment left;
          translate_detached_term environment right;
        ]
  | Detached_subtract_term (left, right) ->
      Z3.Arithmetic.mk_sub environment.detached_context
        [
          translate_detached_term environment left;
          translate_detached_term environment right;
        ]
  | Detached_negate_term value ->
      Z3.Arithmetic.mk_unary_minus environment.detached_context
        (translate_detached_term environment value)
  | Detached_multiply_term (left, right) ->
      Z3.Arithmetic.mk_mul environment.detached_context
        [
          translate_detached_term environment left;
          translate_detached_term environment right;
        ]
  | Detached_scale_term (coefficient, value) ->
      Z3.Arithmetic.mk_mul environment.detached_context
        [
          Z3.Arithmetic.Integer.mk_numeral_s environment.detached_context
            coefficient;
          translate_detached_term environment value;
        ]
  | Detached_less_than_term (left, right) ->
      Z3.Arithmetic.mk_lt environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_less_or_equal_term (left, right) ->
      Z3.Arithmetic.mk_le environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_greater_than_term (left, right) ->
      Z3.Arithmetic.mk_gt environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_greater_or_equal_term (left, right) ->
      Z3.Arithmetic.mk_ge environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_equal_term (left, right) ->
      Z3.Boolean.mk_eq environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_distinct_term (left, right) ->
      Z3.Boolean.mk_distinct environment.detached_context
        [
          translate_detached_term environment left;
          translate_detached_term environment right;
        ]
  | Detached_not_term value ->
      Z3.Boolean.mk_not environment.detached_context
        (translate_detached_term environment value)
  | Detached_and_term values ->
      Z3.Boolean.mk_and environment.detached_context
        (List.map (translate_detached_term environment) values)
  | Detached_or_term values ->
      Z3.Boolean.mk_or environment.detached_context
        (List.map (translate_detached_term environment) values)
  | Detached_implies_term (premise, consequence) ->
      Z3.Boolean.mk_implies environment.detached_context
        (translate_detached_term environment premise)
        (translate_detached_term environment consequence)
  | Detached_forall_term (binder, body, trigger, qid, skid) ->
      translate_detached_user_quantifier environment true binder body
        (Some trigger) qid skid
  | Detached_exists_term (binder, body, qid, skid) ->
      translate_detached_user_quantifier environment false binder body None
        qid skid
  | Detached_ite_term (condition, then_, else_) ->
      Z3.Boolean.mk_ite environment.detached_context
        (translate_detached_term environment condition)
        (translate_detached_term environment then_)
        (translate_detached_term environment else_)
  | Detached_bv_literal_term (reference, unsigned_bits) ->
      let width = decode_detached_width reference in
      let unsigned_bits =
        validate_detached_bv_decimal width unsigned_bits
        |> Result.fold ~ok:Fun.id ~error:(fun message -> invalid_arg message)
      in
      Z3.Expr.mk_numeral_string environment.detached_context
        unsigned_bits
        (Z3.BitVector.mk_sort environment.detached_context
           (Bv_width.to_int width))
  | Detached_bv_eq_term (left, right) ->
      Z3.Boolean.mk_eq environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_distinct_term (left, right) ->
      Z3.Boolean.mk_distinct environment.detached_context
        [ translate_detached_term environment left;
          translate_detached_term environment right ]
  | Detached_bv_add_mod_term (left, right) ->
      Z3.BitVector.mk_add environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_sub_mod_term (left, right) ->
      Z3.BitVector.mk_sub environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_not_term value ->
      Z3.BitVector.mk_not environment.detached_context
        (translate_detached_term environment value)
  | Detached_bv_and_term (left, right) ->
      Z3.BitVector.mk_and environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_or_term (left, right) ->
      Z3.BitVector.mk_or environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_xor_term (left, right) ->
      Z3.BitVector.mk_xor environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_ult_term (left, right) ->
      Z3.BitVector.mk_ult environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_ule_term (left, right) ->
      Z3.BitVector.mk_ule environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_ugt_term (left, right) ->
      Z3.BitVector.mk_ugt environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_uge_term (left, right) ->
      Z3.BitVector.mk_uge environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_slt_term (left, right) ->
      Z3.BitVector.mk_slt environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_sle_term (left, right) ->
      Z3.BitVector.mk_sle environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_sgt_term (left, right) ->
      Z3.BitVector.mk_sgt environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_sge_term (left, right) ->
      Z3.BitVector.mk_sge environment.detached_context
        (translate_detached_term environment left)
        (translate_detached_term environment right)
  | Detached_bv_to_int_unsigned_term value ->
      Z3.BitVector.mk_bv2int environment.detached_context
        (translate_detached_term environment value) false
  | Detached_bv_to_int_signed_term value ->
      Z3.BitVector.mk_bv2int environment.detached_context
        (translate_detached_term environment value) true
  | Detached_int_to_bv_mod_term (reference, value) ->
      Z3.Arithmetic.Integer.mk_int2bv environment.detached_context
        (Bv_width.to_int (decode_detached_width reference))
        (translate_detached_term environment value)

and translate_detached_user_quantifier environment universal binders body trigger
    qid skid =
  let bound =
    List.map
      (fun (index, name, sort) ->
        ( index,
          Z3.Expr.mk_const_s environment.detached_context
            (Printf.sprintf "verocaml_b%d_%s" index name)
            (translate_detached_sort environment sort) ))
      binders
  in
  let scoped =
    {
      environment with
      detached_bound = bound @ environment.detached_bound;
    }
  in
  let patterns =
    match (universal, trigger) with
    | true, Some trigger ->
        let trigger = translate_detached_term scoped trigger in
        ( Some trigger,
          [ Z3.Quantifier.mk_pattern environment.detached_context [ trigger ] ] )
    | false, None -> (None, [])
    | true, None -> invalid_arg "detached universal has no trigger"
    | false, Some _ -> invalid_arg "detached existential has a trigger"
  in
  let symbol value =
    Some (Z3.Symbol.mk_string environment.detached_context value)
  in
  let body = translate_detached_term scoped body in
  let quantifier =
    if universal then
      Z3.Quantifier.mk_forall_const environment.detached_context
        (List.map snd bound)
        body None (snd patterns) [] (symbol qid) (symbol skid)
    else
      quantifier_worker_operations.exists environment.detached_context
        (List.map snd bound) body None (snd patterns) [] (symbol qid) (symbol skid)
  in
  quantifier_audit_operations.audit ~universal
    ~expected_names:
      (List.map
         (fun (index, name, _) -> Printf.sprintf "verocaml_b%d_%s" index name)
         binders)
    ~expected_bound:(List.map snd bound)
    ~trigger:(fst patterns) quantifier;
  Z3.Quantifier.expr_of_quantifier quantifier

let translate_detached_axiom environment axiom =
  let bound =
    List.map
      (fun (index, name, sort) ->
        ( index,
          Z3.Expr.mk_const_s environment.detached_context
            (Printf.sprintf "verocaml_b%d_%s" index name)
            (translate_detached_sort environment sort) ))
      axiom.detached_binders
  in
  let scoped =
    {
      environment with
      detached_bound = bound @ environment.detached_bound;
    }
  in
  let patterns =
    List.map
      (fun terms ->
        Z3.Quantifier.mk_pattern environment.detached_context
          (List.map (translate_detached_term scoped) terms))
      axiom.detached_patterns
  in
  let symbol value =
    Some (Z3.Symbol.mk_string environment.detached_context value)
  in
  Z3.Quantifier.mk_forall_const environment.detached_context
    (List.map snd bound)
    (translate_detached_term scoped axiom.detached_body)
    None patterns []
    (symbol axiom.detached_qid)
    (symbol axiom.detached_skid)
  |> Z3.Quantifier.expr_of_quantifier

type detached_backend_projection =
  | Detached_backend_function of
      string * detached_projection_sort * Z3.Expr.expr
  | Detached_backend_bv of string * string * Bv_width.t * Z3.Expr.expr

type detached_translated = {
  detached_assertions_backend : Z3.Expr.expr list;
  detached_projected_backend : detached_backend_projection list;
}

let translate_detached_plan context plan =
  let environment =
    {
      detached_context = context;
      detached_sorts = ref [];
      detached_functions = ref [];
      detached_bound = [];
    }
  in
  List.iter
    (function
      | Detached_sort_declaration (index, name) ->
          let sort = Z3.Sort.mk_uninterpreted_s context name in
          environment.detached_sorts :=
            (index, sort) :: !(environment.detached_sorts)
      | Detached_datatype_declaration datatype ->
          let bindings =
            datatype_worker_operations.declare ~context
              ~resolve_named_sort:(fun index ->
                find_index "detached datatype dependency" index
                  !(environment.detached_sorts))
              ~resolve_bv_sort:(fun reference ->
                Z3.BitVector.mk_sort context
                  (Bv_width.to_int (decode_detached_width reference)))
              datatype
            |> Result.fold ~ok:Fun.id
                 ~error:(fun message -> invalid_arg message)
          in
          environment.detached_sorts :=
            bindings.sorts @ !(environment.detached_sorts);
          environment.detached_functions :=
            bindings.functions @ !(environment.detached_functions)
      | Detached_function_declaration (index, name, domain, range) ->
          let declaration =
            Z3.FuncDecl.mk_func_decl_s context name
              (List.map (translate_detached_sort environment) domain)
              (translate_detached_sort environment range)
          in
          environment.detached_functions :=
            (index, declaration) :: !(environment.detached_functions))
    plan.detached_declarations;
  let projected =
    List.map
      (function
        | Detached_function_projection (identity, index, sort) ->
            let declaration =
              find_index "detached projection" index
                !(environment.detached_functions)
            in
            Detached_backend_function
              (identity, sort, Z3.Expr.mk_app context declaration [])
        | Detached_bv_projection (identity, reference, term) ->
            Detached_backend_bv
              ( identity,
                reference,
                decode_detached_width reference,
                translate_detached_term environment term ))
      plan.detached_projections
  in
  {
    detached_assertions_backend =
      List.map (translate_detached_axiom environment) plan.detached_axioms
      @ List.map (translate_detached_term environment)
          plan.detached_assertions;
    detached_projected_backend = projected;
  }

let parameters ?policy context config =
  let parameters = Z3.Params.mk_params context in
  let timeout_ms =
    Option.fold ~none:config.timeout_ms ~some:Solver_policy_private.timeout_ms
      policy
  in
  Z3.Params.add_int parameters (Z3.Symbol.mk_string context "timeout")
    timeout_ms;
  Option.iter
    (fun policy ->
      Z3.Params.add_int parameters (Z3.Symbol.mk_string context "rlimit")
        (Solver_policy_private.rlimit policy))
    policy;
  Z3.Params.add_bool parameters (Z3.Symbol.mk_string context "model")
    config.model;
  Z3.Params.add_bool parameters (Z3.Symbol.mk_string context "arith.nl")
    nonlinear_reasoning_enabled;
  parameters

let note_context_created = function
  | Global ->
      incr contexts_created;
      incr contexts_live;
      maximum_contexts_live := max !maximum_contexts_live !contexts_live
  | Local local ->
      local.local_contexts_created <- local.local_contexts_created + 1;
      local.local_contexts_live <- local.local_contexts_live + 1;
      local.local_maximum_contexts_live <-
        max local.local_maximum_contexts_live local.local_contexts_live

let note_context_cleaned = function
  | Global ->
      decr contexts_live;
      incr contexts_cleaned;
      [%log.trace "completed solver resource cleanup"
        ~stage:(Delator.Field.string "resource-cleanup")
        ~resource:(Delator.Field.string "solver-context")
        ~route:(Delator.Field.string "direct-global")
        ~decision:(Delator.Field.string "released")
        ~contexts_live:(Delator.Field.int !contexts_live)]
  | Local local ->
      local.local_contexts_live <- local.local_contexts_live - 1;
      local.local_contexts_cleaned <- local.local_contexts_cleaned + 1;
      [%log.trace "completed solver resource cleanup"
        ~stage:(Delator.Field.string "resource-cleanup")
        ~resource:(Delator.Field.string "solver-context")
        ~route:(Delator.Field.string "query-local")
        ~decision:(Delator.Field.string "released")
        ~contexts_live:(Delator.Field.int local.local_contexts_live)]

let note_solver_created accounting logic =
  match accounting with
  | Global ->
      incr solvers_created;
      selected_logics_reversed := logic :: !selected_logics_reversed
  | Local local ->
      local.local_solvers_created <- local.local_solvers_created + 1;
      local.local_selected_logics_reversed <-
        logic :: local.local_selected_logics_reversed

let note_solver_reset = function
  | Global -> incr solver_resets
  | Local local -> local.local_solver_resets <- local.local_solver_resets + 1

let with_solver accounting ?policy ~requirements config translate use =
  let logic = solver_logic requirements in
  let context =
    Z3.mk_context
      [
        ("model", string_of_bool config.model);
        ("auto_config", "false");
      ]
  in
  note_context_created accounting;
  let solver = ref None in
  Fun.protect
    ~finally:(fun () ->
      Fun.protect
        ~finally:(fun () ->
          note_context_cleaned accounting)
        (fun () ->
          Option.iter
            (fun solver_ ->
              Z3.Solver.reset solver_;
              note_solver_reset accounting)
            !solver))
    (fun () ->
      let translated = translate context in
      let created =
        if List.mem Logic_ir.Bit_vectors requirements then
          Z3.Solver.mk_solver context None
        else Z3.Solver.mk_solver_s context logic
      in
      solver := Some created;
      note_solver_created accounting logic;
      Z3.Solver.set_parameters created (parameters ?policy context config);
      Z3.Solver.add created translated.assertions;
      [%log.trace "initialized direct Z3 solver"
        ~logic:(Delator.Field.string logic)
        ~bit_vectors:
          (Delator.Field.bool (List.mem Logic_ir.Bit_vectors requirements))
        ~int_bitvector_conversions:
          (Delator.Field.bool
             (List.mem Logic_ir.Int_bitvector_conversions requirements))
        ~nonlinear_terms:(Delator.Field.string "admitted")
        ~nonlinear_reasoning:
          (Delator.Field.string
             (if nonlinear_reasoning_enabled then "enabled" else "disabled"))
        ~assertion_count:
          (Delator.Field.int (List.length translated.assertions))
        ~decision:(Delator.Field.string "initialized")];
      use created translated)
[@@delator.instrument] [@@delator.level trace]

let protect f =
  try f () with
  | Invalid_argument message -> Error (Malformed_logic_ir message)
  | Z3.Error message -> Error (Backend_failure message)
  | exn -> Error (Backend_failure (Printexc.to_string exn))

let resolve_policy ?rlimit config =
  let result =
    match rlimit with
    | None ->
        Solver_policy_private.create_default ~timeout_ms:config.timeout_ms
    | Some rlimit ->
        Solver_policy_private.create ~timeout_ms:config.timeout_ms ~rlimit
  in
  Result.map_error
    (fun error ->
      Invalid_configuration (Solver_policy_private.error_to_string error))
    result

let classify_unknown_reason @ portable = fun reason ->
  match reason with
  | "max. resource limit exceeded" -> Resource_exhausted
  | "timeout" -> Timed_out
  | reason -> Backend_unknown reason

let solve_translated @ portable = fun controlled solver ->
  match controlled with
  | Force_unknown -> Ok (Inconclusive (Backend_unknown "controlled unknown"))
  | Force_timeout -> Ok (Inconclusive Timed_out)
  | Force_backend_failure ->
      raise (Failure "controlled direct-Z3 failure after solver creation")
  | Real -> (
      match Z3.Solver.check solver [] with
      | Z3.Solver.UNSATISFIABLE -> Ok Verified
      | SATISFIABLE -> Ok (Counterexample [])
      | UNKNOWN ->
          let reason = Z3.Solver.get_reason_unknown solver in
          Ok (Inconclusive (classify_unknown_reason reason)))
[@@delator.instrument] [@@delator.level trace]

let solve_query ?(controlled = Real) ?rlimit config query =
  if Logic_ir.View.bv_projections query <> [] then
    Error
      (Malformed_logic_ir
         "BV projections require the structured BV query solver")
  else match resolve_policy ?rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match resolve_capabilities Global (Logic_ir.requirements query) with
      | Error _ as error -> error
      | Ok () ->
          note_translation Global;
          protect (fun () ->
              if List.mem Logic_ir.Bit_vectors (Logic_ir.requirements query) then
                ignore (detach_query query);
              with_solver Global ~policy
                ~requirements:(Logic_ir.requirements query) config
                (fun context -> translate_logic_query context query)
                (fun solver _ -> solve_translated controlled solver)))
[@@delator.instrument] [@@delator.level trace]

let solve_query_with accounting ~controlled ~rlimit config query =
  if Logic_ir.View.bv_projections query <> [] then
    Error
      (Malformed_logic_ir
         "BV projections require the structured BV query solver")
  else match resolve_policy ~rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match resolve_capabilities accounting (Logic_ir.requirements query) with
      | Error _ as error -> error
      | Ok () ->
          note_translation accounting;
          protect (fun () ->
              if List.mem Logic_ir.Bit_vectors (Logic_ir.requirements query) then
                ignore (detach_query query);
              with_solver accounting ~policy
                ~requirements:(Logic_ir.requirements query) config
                (fun context -> translate_logic_query context query)
                (fun solver _ -> solve_translated controlled solver)))

let solve_query_local ~controlled ~rlimit config query =
  let local = fresh_local_counters () in
  let result =
    solve_query_with (Local local) ~controlled ~rlimit config query
  in
  { result; telemetry = local_snapshot local }
[@@delator.instrument] [@@delator.level trace]

let detached_parameters context ~timeout_ms ~rlimit ~model =
  let parameters = Z3.Params.mk_params context in
  Z3.Params.add_int parameters (Z3.Symbol.mk_string context "timeout")
    timeout_ms;
  Z3.Params.add_int parameters (Z3.Symbol.mk_string context "rlimit") rlimit;
  Z3.Params.add_bool parameters (Z3.Symbol.mk_string context "model") model;
  Z3.Params.add_bool parameters (Z3.Symbol.mk_string context "arith.nl")
    nonlinear_reasoning_enabled;
  parameters

let bv_model_failure () reason =
  [%log.error "rejected structured BV model projection"
    ~stage:(Delator.Field.string "bv-model-decode")
    ~reason_class:(Delator.Field.string reason)
    ~decision:(Delator.Field.string "rejected")];
  raise (Failure reason)
[@@delator.instrument] [@@delator.level error]

let bv_model_fault_class @ portable = function
  | Missing_evaluation -> "missing-evaluation"
  | Wrong_sort -> "wrong-sort"
  | Non_numeral -> "non-numeral"
  | Width_mismatch -> "width-mismatch"
  | Residue_text _ -> "residue-text"

(* FIXME(delator): return the bounded class so erased builds retain an ordinary
   use without exporting this private test seam through the log-value ABI. *)
let note_bv_model_fault @ portable = fun fault ->
  let fault_class = bv_model_fault_class fault in
  [%log.debug "injected a test-owned BV model observation fault"
    ~stage:(Delator.Field.string "bv-model-decode-fault")
    ~fault_class:(Delator.Field.string fault_class)
    ~decision:(Delator.Field.string "injected")];
  fault_class
[@@delator.instrument] [@@delator.level debug]

let evaluate_bv_model @ portable = fun fault model expression ->
  match fault with
  | Some Missing_evaluation ->
      ignore (note_bv_model_fault Missing_evaluation);
      None
  | None | Some (Wrong_sort | Non_numeral | Width_mismatch | Residue_text _) ->
      Z3.Model.evaluate model expression true

let decode_bv_model_residue @ portable = fun fault width expression ->
  let sort = Z3.Expr.get_sort expression in
  let sort_kind =
    match fault with
    | Some Wrong_sort ->
        ignore (note_bv_model_fault Wrong_sort);
        Z3enums.INT_SORT
    | None
    | Some
        ( Missing_evaluation | Non_numeral | Width_mismatch | Residue_text _ ) ->
        Z3.Sort.get_sort_kind sort
  in
  if sort_kind <> Z3enums.BV_SORT then
    Error "BV model binding has the wrong backend sort"
  else
    let width_int = Bv_width.to_int width in
    let observed_width =
      match fault with
      | Some Width_mismatch ->
          ignore (note_bv_model_fault Width_mismatch);
          width_int + 1
      | None
      | Some (Missing_evaluation | Wrong_sort | Non_numeral | Residue_text _) ->
          Z3.BitVector.get_size sort
    in
    if observed_width <> width_int then
      Error "BV model binding width differs from its projection"
    else
      let is_numeral =
        match fault with
        | Some Non_numeral ->
            ignore (note_bv_model_fault Non_numeral);
            false
        | None
        | Some
            ( Missing_evaluation | Wrong_sort | Width_mismatch | Residue_text _ ) ->
            Z3.Expr.is_numeral expression
      in
      if not is_numeral then Error "BV model binding is not a numeral"
      else
        let residue =
          match fault with
          | Some (Residue_text text) ->
              ignore (note_bv_model_fault (Residue_text text));
              text
          | None
          | Some
              (Missing_evaluation | Wrong_sort | Non_numeral | Width_mismatch) ->
              Z3.BitVector.numeral_to_string expression
        in
        validate_detached_bv_decimal width residue

let decode_bv_model_value fault width expression =
  match decode_bv_model_residue fault width expression with
  | Error reason -> bv_model_failure () reason
  | Ok residue ->
      let value =
        Bv_value.of_string ~width residue
        |> Result.fold ~ok:Fun.id ~error:(bv_model_failure ())
      in
      [%log.trace "decoded structured BV model projection"
        ~stage:(Delator.Field.string "bv-model-decode")
        ~width:(Delator.Field.int (Bv_width.to_int width))
        ~residue_decimal_bytes:
          (Delator.Field.int
             (String.length (Bv_value.canonical_decimal value)))
        ~decision:(Delator.Field.string "accepted")];
      value

let solve_bv_query_with_model_fault model_fault ?(controlled = Real) ?rlimit
    config query =
  let projections = Logic_ir.View.bv_projections query in
  if projections <> [] && not config.model then
    Error
      (Invalid_configuration
         "BV model projections require model production")
  else
    match resolve_policy ?rlimit config with
    | Error _ as error -> error
    | Ok policy -> (
        match resolve_capabilities Global (Logic_ir.requirements query) with
        | Error _ as error -> error
        | Ok () ->
            note_translation Global;
            protect (fun () ->
                if List.mem Logic_ir.Bit_vectors (Logic_ir.requirements query) then
                  ignore (detach_query query);
                with_solver Global ~policy
                  ~requirements:(Logic_ir.requirements query) config
                  (fun context -> translate_logic_query context query)
                  (fun solver translated ->
                    match solve_translated controlled solver with
                    | Error _ as error -> error
                    | Ok Verified -> Ok Bv_verified
                    | Ok (Inconclusive reason) -> Ok (Bv_inconclusive reason)
                    | Ok (Counterexample _) ->
                        let model =
                          match Z3.Solver.get_model solver with
                          | Some model -> model
                          | None ->
                              raise
                                (Failure
                                   "Z3 returned satisfiable without a BV model")
                        in
                        let bindings =
                          List.map
                            (fun (projection, expression) ->
                              let value =
                                match
                                  evaluate_bv_model model_fault model expression
                                with
                                | Some value ->
                                    decode_bv_model_value model_fault
                                      (Logic_ir.View.bv_projection_width projection)
                                      value
                                | None ->
                                    raise
                                      (Failure
                                         "BV model projection has no backend value")
                              in
                              { projection_identity =
                                  Logic_ir.View.bv_projection_identity projection;
                                width =
                                  Logic_ir.View.bv_projection_width projection;
                                value })
                            translated.bv_projected
                        in
                        Ok (Bv_counterexample bindings))))
[@@delator.instrument] [@@delator.level trace]

let solve_bv_query ?controlled ?rlimit config query =
  solve_bv_query_with_model_fault None ?controlled ?rlimit config query

let detached_model_value model_fault projection expression =
  match projection with
  | Detached_backend_function (identity, Detached_project_integer, _) ->
      Detached_integer (identity, Z3.Expr.to_string expression)
  | Detached_backend_function (identity, Detached_project_boolean, _) -> (
      match Z3.Boolean.get_bool_value expression with
      | Z3enums.L_TRUE -> Detached_boolean (identity, true)
      | L_FALSE -> Detached_boolean (identity, false)
      | L_UNDEF ->
          raise
            (Failure "Boolean model binding has an undefined backend value"))
  | Detached_backend_function (identity, Detached_project_aggregate, _) ->
      Detached_aggregate (identity, Z3.Expr.to_string expression)
  | Detached_backend_bv (identity, reference, width, _) ->
      let residue =
        decode_bv_model_residue model_fault width expression
        |> Result.fold ~ok:Fun.id ~error:(fun reason -> raise (Failure reason))
      in
      Detached_bit_vector (identity, reference, residue)

let detached_note_capability_resolution local =
  local.local_capability_resolutions <-
    local.local_capability_resolutions + 1

let detached_note_translation local =
  local.local_translations <- local.local_translations + 1

let detached_note_context_created local =
  local.local_contexts_created <- local.local_contexts_created + 1;
  local.local_contexts_live <- local.local_contexts_live + 1;
  local.local_maximum_contexts_live <-
    max local.local_maximum_contexts_live local.local_contexts_live

let detached_note_context_cleaned local =
  local.local_contexts_live <- local.local_contexts_live - 1;
  local.local_contexts_cleaned <- local.local_contexts_cleaned + 1;
  [%log.trace "completed solver resource cleanup"
    ~stage:(Delator.Field.string "resource-cleanup")
    ~resource:(Delator.Field.string "solver-context")
    ~route:(Delator.Field.string "detached-query-local")
    ~decision:(Delator.Field.string "released")
    ~contexts_live:(Delator.Field.int local.local_contexts_live)]

let detached_note_solver_created local logic =
  local.local_solvers_created <- local.local_solvers_created + 1;
  local.local_selected_logics_reversed <-
    logic :: local.local_selected_logics_reversed

let detached_note_solver_reset local =
  local.local_solver_resets <- local.local_solver_resets + 1

let detached_feature_to_string @ portable = function
  | Logic_ir.Named_sorts -> "named-sorts"
  | Uninterpreted_functions -> "uninterpreted-functions"
  | Linear_integer_arithmetic -> "linear-integer-arithmetic"
  | Quantifiers -> "quantifiers"
  | Explicit_patterns -> "explicit-patterns"
  | Quantifier_ids -> "quantifier-ids"
  | Models -> "models"
  | Nonlinear_integer_arithmetic -> "nonlinear-integer-arithmetic"
  | Algebraic_datatypes -> "algebraic-datatypes"
  | Bit_vectors -> "bit-vectors"
  | Int_bitvector_conversions -> "int-bitvector-conversions"

let detached_feature_supported @ portable = function
  | Logic_ir.Named_sorts | Uninterpreted_functions | Linear_integer_arithmetic
  | Quantifiers | Explicit_patterns | Quantifier_ids | Models
  | Algebraic_datatypes | Nonlinear_integer_arithmetic | Bit_vectors
  | Int_bitvector_conversions ->
      true

let solve_detached_query_local_with_model_fault model_fault ~controlled
    ~timeout_ms ~rlimit ~model (Detached_query encoded) =
  [%log.trace "begin detached solver query"
    ~timeout_ms:(Delator.Field.int timeout_ms)
    ~rlimit:(Delator.Field.int rlimit)
    ~model:(Delator.Field.bool model)
    ~encoded_bytes:(Delator.Field.int (String.length encoded))];
  let local = fresh_local_counters () in
  detached_note_capability_resolution local;
  detached_note_translation local;
  let detached_result =
    try
      let plan : detached_plan = Marshal.from_string encoded 0 in
      [%log.trace "decoded detached solver plan"
        ~requirements:(Delator.Field.int (List.length plan.detached_requirements))
        ~declarations:(Delator.Field.int (List.length plan.detached_declarations))
        ~axioms:(Delator.Field.int (List.length plan.detached_axioms))
        ~assertions:(Delator.Field.int (List.length plan.detached_assertions))
        ~projections:(Delator.Field.int (List.length plan.detached_projections))];
      let unsupported =
        List.filter (fun feature -> not (detached_feature_supported feature))
          plan.detached_requirements
      in
      if unsupported <> [] then
        Error
          (Printf.sprintf "unsupported direct-Z3 features: %s"
             (String.concat ", "
                (List.map detached_feature_to_string unsupported)))
      else
      let validated_requirements = preflight_detached_plan plan in
      [%log.trace "completed detached typed preflight before solver allocation"
        ~stage:(Delator.Field.string "detached-typed-preflight")
        ~transported_features:
          (Delator.Field.int (List.length plan.detached_requirements))
        ~validated_features:
          (Delator.Field.int (List.length validated_requirements))
        ~decision:(Delator.Field.string "accepted")];
      let logic = solver_logic validated_requirements in
      let context =
        Z3.mk_context
          [ ("model", string_of_bool model); ("auto_config", "false") ]
      in
      detached_note_context_created local;
      [%log.trace "created detached Z3 context"];
      let solver = ref None in
      Fun.protect
        ~finally:(fun () ->
          Fun.protect
            ~finally:(fun () -> detached_note_context_cleaned local)
            (fun () ->
              Option.iter
                (fun solver_ ->
                  Z3.Solver.reset solver_;
                  detached_note_solver_reset local)
                !solver))
        (fun () ->
          let translated = translate_detached_plan context plan in
          let created =
            if List.mem Logic_ir.Bit_vectors validated_requirements then
              Z3.Solver.mk_solver context None
            else Z3.Solver.mk_solver_s context logic
          in
          solver := Some created;
          detached_note_solver_created local logic;
          Z3.Solver.set_parameters created
            (detached_parameters context ~timeout_ms ~rlimit ~model);
          Z3.Solver.add created translated.detached_assertions_backend;
          [%log.trace "initialized detached Z3 solver"
            ~logic:(Delator.Field.string logic)
            ~bit_vectors:
              (Delator.Field.bool
                 (List.mem Logic_ir.Bit_vectors validated_requirements))
            ~int_bitvector_conversions:
              (Delator.Field.bool
                 (List.mem Logic_ir.Int_bitvector_conversions
                    validated_requirements))
            ~nonlinear_terms:(Delator.Field.string "admitted")
            ~nonlinear_reasoning:
              (Delator.Field.string
                 (if nonlinear_reasoning_enabled then "enabled" else "disabled"))
            ~assertions:
              (Delator.Field.int
                 (List.length translated.detached_assertions_backend))
            ~projections:
              (Delator.Field.int
                 (List.length translated.detached_projected_backend))];
          match solve_translated controlled created with
          | Error _ -> Error "direct-Z3 solver check failed"
          | Ok Verified -> Ok Detached_verified
          | Ok (Inconclusive reason) ->
              Ok (Detached_inconclusive reason)
          | Ok (Counterexample _) ->
                  let backend_model =
                    match Z3.Solver.get_model created with
                    | Some backend_model -> backend_model
                    | None ->
                        raise
                          (Failure
                             "Z3 returned satisfiable without a model")
                  in
                  Ok
                    (Detached_counterexample
                       (List.map
                          (fun projection ->
                            let expression =
                              match projection with
                              | Detached_backend_function (_, _, expression)
                              | Detached_backend_bv (_, _, _, expression) ->
                                  expression
                            in
                            let evaluated =
                              match projection with
                              | Detached_backend_bv _ ->
                                  evaluate_bv_model model_fault backend_model
                                    expression
                              | Detached_backend_function _ ->
                                  Z3.Model.evaluate backend_model expression true
                            in
                            match evaluated with
                            | Some value ->
                                Some
                                  (detached_model_value model_fault projection
                                     value)
                            | None -> (
                                match projection with
                                | Detached_backend_bv _ ->
                                    raise
                                      (Failure
                                         "BV model projection has no backend value")
                                | Detached_backend_function _ -> None))
                          translated.detached_projected_backend)))
    with
    | Invalid_argument message ->
        Error ("malformed direct-Z3 logic IR: " ^ message)
    | Z3.Error message ->
        Error ("direct-Z3 backend failure: " ^ message)
    | exn ->
        Error
          ("direct-Z3 backend failure: " ^ Printexc.to_string exn)
  in
  let detached_telemetry = local_snapshot local in
  (match detached_result with
  | Error (reason [@log_value.error]) ->
      [%log.error "detached direct-Z3 query failed"
        ~stage:(Delator.Field.string "detached-direct-z3")
        ~reason_class:(Delator.Field.string (reason [@log_value.error]))
        ~contexts_created:(Delator.Field.int detached_telemetry.contexts_created)
        ~contexts_cleaned:(Delator.Field.int detached_telemetry.contexts_cleaned)
        ~decision:(Delator.Field.string "failed")]
  | Ok _ -> ());
  [%log.trace "completed detached solver query"
    ~outcome:
      (Delator.Field.string
         (match detached_result with
         | Ok Detached_verified -> "verified"
         | Ok (Detached_counterexample _) -> "counterexample"
         | Ok (Detached_inconclusive _) -> "inconclusive"
         | Error _ -> "error"))
    ~contexts_created:(Delator.Field.int detached_telemetry.contexts_created)
    ~solvers_created:(Delator.Field.int detached_telemetry.solvers_created)
    ~solver_resets:(Delator.Field.int detached_telemetry.solver_resets)
    ~contexts_cleaned:(Delator.Field.int detached_telemetry.contexts_cleaned)];
  { detached_result; detached_telemetry }
[@@delator.instrument] [@@delator.level trace]

let solve_detached_query_local ~controlled ~timeout_ms ~rlimit ~model query =
  solve_detached_query_local_with_model_fault None ~controlled ~timeout_ms
    ~rlimit ~model query
[@@unsafe_allow_any_mode_crossing
  "The portable closure captures only Z3 datatype operations; every native \
   handle is created, used, and released inside the invoking worker."]

let render_query config query =
  match resolve_capabilities Global (Logic_ir.requirements query) with
  | Error _ as error -> error
  | Ok () ->
      note_translation Global;
      protect (fun () ->
          with_solver Global ~requirements:(Logic_ir.requirements query) config
            (fun context -> translate_logic_query context query)
            (fun solver translated ->
              Ok
                ( String.concat "\n" translated.declaration_snapshot,
                  Z3.Solver.to_string solver )))

let translate_projected context functions projected =
  List.map
    (fun (symbol, function_) ->
      let declaration =
        find_index "projection function"
          (Logic_ir.View.function_index function_)
          functions
      in
      (symbol, Z3.Expr.mk_app context declaration []))
    projected

let aggregate_model_identity expression =
  let rendered = Z3.Expr.to_string expression in
  match List.rev (String.split_on_char '!' rendered) with
  | index :: "val" :: _ -> (
      try Z.of_string index
      with _ -> Z.of_int (Hashtbl.hash rendered))
  | _ -> Z.of_int (Hashtbl.hash rendered)

let model_value model_fault expected_sort expression =
  match expected_sort with
  | Vir.Integer ->
      Integer (Z3.Arithmetic.Integer.get_big_int expression)
  | Boolean -> (
      match Z3.Boolean.get_bool_value expression with
      | Z3enums.L_TRUE -> Boolean true
      | L_FALSE -> Boolean false
      | L_UNDEF ->
          raise
            (Failure "Boolean model binding has an undefined backend value"))
  | Aggregate _ ->
      Aggregate_identity
        (try Z3.Arithmetic.Integer.get_big_int expression
         with _ -> aggregate_model_identity expression)
  | Bit_vector width ->
      Bit_vector (decode_bv_model_value model_fault width expression)
  | Parametric _ ->
      raise (Failure "parametric symbols are not projected through aggregate model ABI")

let evaluated_model_value model_fault model (symbol : Vir.symbol) expression =
  let evaluated =
    match symbol.sort with
    | Vir.Bit_vector _ -> evaluate_bv_model model_fault model expression
    | Vir.Integer | Boolean | Aggregate _ | Parametric _ ->
        Z3.Model.evaluate model expression true
  in
  match evaluated with
  | Some value -> Some (model_value model_fault symbol.sort value)
  | None -> (
      match symbol.sort with
      | Vir.Bit_vector width ->
          [%log.error "mandatory direct VIR BV projection has no model value"
            ~stage:(Delator.Field.string "vir-bv-model-decode")
            ~symbol_id:(Delator.Field.int symbol.symbol_id)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~decision:(Delator.Field.string "rejected")];
          raise (Failure "BV model projection has no backend value")
      | Vir.Integer | Boolean | Aggregate _ | Parametric _ -> None)

let solve_vir_with_model_fault model_fault ?(controlled = Real) ?rlimit
    ?(requires = []) config obligation =
  match resolve_policy ?rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match
        resolve_capabilities Global
          (Logic_ir.Models :: Logic_ir.Uninterpreted_functions
         :: Logic_ir.Linear_integer_arithmetic
          :: requires)
      with
      | Error _ as error -> error
      | Ok () -> (
          note_translation Global;
          let translated =
            Vir_logic_ir_translation_private.translate ~requires obligation
            |> Result.map_error (fun message -> Malformed_vir message)
          in
          match translated with
          | Error _ as error -> error
          | Ok translated ->
              let query =
                Vir_logic_ir_translation_private.query translated
              in
              protect (fun () ->
                  with_solver Global ~policy
                    ~requirements:(Logic_ir.requirements query) config
                    (fun context ->
                      let logic =
                        translate_logic_query context query
                      in
                      let projected =
                        translate_projected context logic.functions
                          (Vir_logic_ir_translation_private.projected translated)
                      in
                      { logic with projected })
                    (fun solver translated_logic ->
                      match controlled with
                      | Force_unknown ->
                          Ok
                            (Inconclusive
                               (Backend_unknown "controlled unknown"))
                      | Force_timeout -> Ok (Inconclusive Timed_out)
                      | Force_backend_failure ->
                          raise
                            (Failure
                               "controlled direct-Z3 failure after solver creation")
                      | Real -> (
                          match Z3.Solver.check solver [] with
                          | Z3.Solver.UNSATISFIABLE -> Ok Verified
                          | UNKNOWN ->
                              let reason =
                                Z3.Solver.get_reason_unknown solver
                              in
                              Ok
                                (Inconclusive
                                   (classify_unknown_reason reason))
                          | SATISFIABLE ->
                              let model =
                                match Z3.Solver.get_model solver with
                                | Some model -> model
                                | None ->
                                    raise
                                      (Failure
                                         "Z3 returned satisfiable without a model")
                              in
                              let bindings =
                                List.map
                                  (fun (symbol, expression) ->
                                    let value =
                                      evaluated_model_value model_fault model symbol
                                        expression
                                    in
                                    { symbol; value })
                                  translated_logic.projected
                              in
                              Ok (Counterexample bindings))))))
[@@delator.instrument] [@@delator.level trace]

let solve_vir ?controlled ?rlimit ?requires config obligation =
  solve_vir_with_model_fault None ?controlled ?rlimit ?requires config obligation

let solve_vir_with accounting ~model_fault ~controlled ~rlimit ?(requires = [])
    config obligation =
  match resolve_policy ~rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match
        resolve_capabilities accounting
          (Logic_ir.Models :: Logic_ir.Uninterpreted_functions
         :: Logic_ir.Linear_integer_arithmetic
          :: requires)
      with
      | Error _ as error -> error
      | Ok () -> (
          note_translation accounting;
          let translated =
            Vir_logic_ir_translation_private.translate ~requires obligation
            |> Result.map_error (fun message -> Malformed_vir message)
          in
          match translated with
          | Error _ as error -> error
          | Ok translated ->
              let query =
                Vir_logic_ir_translation_private.query translated
              in
              protect (fun () ->
                  with_solver accounting ~policy
                    ~requirements:(Logic_ir.requirements query) config
                    (fun context ->
                      let logic =
                        translate_logic_query context query
                      in
                      let projected =
                        translate_projected context logic.functions
                          (Vir_logic_ir_translation_private.projected translated)
                      in
                      { logic with projected })
                    (fun solver translated_logic ->
                      match solve_translated controlled solver with
                      | Error _ as error -> error
                      | Ok (Verified | Inconclusive _ as outcome) -> Ok outcome
                      | Ok (Counterexample _) ->
                          let model =
                            match Z3.Solver.get_model solver with
                            | Some model -> model
                            | None ->
                                raise
                                  (Failure
                                     "Z3 returned satisfiable without a model")
                          in
                          let bindings =
                            List.map
                              (fun (symbol, expression) ->
                                let value =
                                  evaluated_model_value model_fault model symbol
                                    expression
                                in
                                { symbol; value })
                              translated_logic.projected
                          in
                          Ok (Counterexample bindings)))))
[@@delator.instrument] [@@delator.level trace]

let solve_vir_local_with_model_fault model_fault ~controlled ~rlimit
    ?(requires = []) config obligation =
  let local = fresh_local_counters () in
  let result =
    solve_vir_with (Local local) ~model_fault ~controlled ~rlimit ~requires
      config obligation
  in
  { result; telemetry = local_snapshot local }
[@@delator.instrument] [@@delator.level trace]

let solve_vir_local ~controlled ~rlimit ?requires config obligation =
  solve_vir_local_with_model_fault None ~controlled ~rlimit ?requires config
    obligation

let render_vir ?(requires = []) config obligation =
  match
    resolve_capabilities Global
      (Logic_ir.Models :: Logic_ir.Uninterpreted_functions
     :: Logic_ir.Linear_integer_arithmetic
      :: requires)
  with
  | Error _ as error -> error
  | Ok () ->
      let translated =
        Vir_logic_ir_translation_private.translate ~requires obligation
        |> Result.map_error (fun message -> Malformed_vir message)
      in
      (match translated with
      | Error _ as error -> error
      | Ok translated ->
          render_query config
            (Vir_logic_ir_translation_private.query translated))

let diagnostic_snapshot config query =
  Printf.sprintf "logic=%s timeout-ms=%d model=%b requirements=%s"
    (solver_logic (Logic_ir.requirements query)) config.timeout_ms config.model
    (Logic_ir.requirements query
    |> List.map Logic_ir.feature_to_string
    |> String.concat ",")

let version () =
  (Z3.Version.major, Z3.Version.minor, Z3.Version.build, Z3.Version.full_version)

module For_testing = struct
  type defect = detached_preflight_defect =
    | Mixed_bv_add_widths
    | Omitted_bv_capability
    | Wrong_int_to_bv_operand
    | Bv_projection_width_mismatch

  type model_fault = bv_model_fault =
    | Missing_evaluation
    | Wrong_sort
    | Non_numeral
    | Width_mismatch
    | Residue_text of string

  let malformed_detached_bv_query = malformed_detached_bv_query_for_testing

  let solve_bv_query_with_model_fault ~fault ?controlled ?rlimit config query =
    solve_bv_query_with_model_fault (Some fault) ?controlled ?rlimit config query

  let solve_detached_query_local_with_model_fault ~fault ~controlled ~timeout_ms
      ~rlimit ~model query =
    solve_detached_query_local_with_model_fault (Some fault) ~controlled
      ~timeout_ms ~rlimit ~model query

  let solve_vir_local_with_model_fault ~fault ~controlled ~rlimit ?requires
      config obligation =
    solve_vir_local_with_model_fault (Some fault) ~controlled ~rlimit ?requires
      config obligation
end
