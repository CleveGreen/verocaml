type config = {
  timeout_ms : int;
  model : bool;
}

type model_value =
  | Integer of Z.t
  | Boolean of bool
  | Aggregate_identity of Z.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type inconclusive_reason : value mod contended portable =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive_reason

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
  | Detached_named_sort of int

type detached_term : value mod contended portable =
  | Detached_integer_term of string
  | Detached_boolean_term of bool
  | Detached_bound_term of int
  | Detached_apply_term of int * detached_term list
  | Detached_add_term of detached_term * detached_term
  | Detached_subtract_term of detached_term * detached_term
  | Detached_negate_term of detached_term
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

type detached_plan : value mod contended portable = {
  detached_requirements : Logic_ir.feature list;
  detached_declarations : detached_declaration list;
  detached_axioms : detached_axiom list;
  detached_assertions : detached_term list;
  detached_projections : (int * detached_projection_sort) list;
}

type detached_query : value mod contended portable =
  | Detached_query of string

type detached_model_value : value mod contended portable =
  | Detached_integer of string
  | Detached_boolean of bool
  | Detached_aggregate of string

type detached_outcome : value mod contended portable =
  | Detached_verified
  | Detached_counterexample of detached_model_value option list
  | Detached_inconclusive of inconclusive_reason

type detached_attempt : value mod contended portable = {
  detached_result : (detached_outcome, string) result;
  detached_telemetry : counters;
}

type datatype_worker_operations : value mod portable = {
  declare :
    context:Z3.context ->
    resolve_named_sort:(int -> Z3.Sort.sort) ->
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

let solver_logic = "AUFLIA"

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
  | Models ->
      true
  | Nonlinear_integer_arithmetic -> false

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

let translate_sort environment = function
  | Logic_ir.Int -> Z3.Arithmetic.Integer.mk_sort environment.context
  | Bool -> Z3.Boolean.mk_sort environment.context
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
  }

let detach_sort = function
  | Logic_ir.Int -> Detached_int_sort
  | Bool -> Detached_bool_sort
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
  Detached_query
    (Marshal.to_string
       {
         detached_requirements = Logic_ir.requirements query;
         detached_declarations = declarations;
         detached_axioms =
           List.map detach_axiom (Logic_ir.View.axioms query);
         detached_assertions =
           List.map detach_term (Logic_ir.View.assertions query);
         detached_projections = projections;
       }
       [])

let detach_query query = detach_query_with_projections query []

let projection_sort (symbol : Vir.symbol) =
  match symbol.sort with
  | Vir.Integer -> Detached_project_integer
  | Boolean -> Detached_project_boolean
  | Aggregate _ -> Detached_project_aggregate
  | Parametric _ ->
      invalid_arg "parametric symbols must not be projected through aggregate ABI"

let detach_vir ~requires obligation =
  match Vir_logic_ir_translation_private.translate ~requires obligation with
  | Error message -> Error (Malformed_vir message)
  | Ok translation ->
      let projected = Vir_logic_ir_translation_private.projected translation in
      let projections =
        List.map
          (fun (symbol, function_) ->
            ( Logic_ir.View.function_index function_,
              projection_sort symbol ))
          projected
      in
      Ok
        ( detach_query_with_projections
            (Vir_logic_ir_translation_private.query translation)
            projections,
          List.map fst projected )

type detached_translation_environment = {
  detached_context : Z3.context;
  detached_sorts : (int * Z3.Sort.sort) list ref;
  detached_functions : (int * Z3.FuncDecl.func_decl) list ref;
  detached_bound : (int * Z3.Expr.expr) list;
}

let translate_detached_sort environment = function
  | Detached_int_sort ->
      Z3.Arithmetic.Integer.mk_sort environment.detached_context
  | Detached_bool_sort ->
      Z3.Boolean.mk_sort environment.detached_context
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

type detached_translated = {
  detached_assertions_backend : Z3.Expr.expr list;
  detached_projected_backend :
    (detached_projection_sort * Z3.Expr.expr) list;
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
      (fun (index, sort) ->
        let declaration =
          find_index "detached projection" index
            !(environment.detached_functions)
        in
        (sort, Z3.Expr.mk_app context declaration []))
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
      incr contexts_cleaned
  | Local local ->
      local.local_contexts_live <- local.local_contexts_live - 1;
      local.local_contexts_cleaned <- local.local_contexts_cleaned + 1

let note_solver_created accounting =
  match accounting with
  | Global ->
      incr solvers_created;
      selected_logics_reversed :=
        solver_logic :: !selected_logics_reversed
  | Local local ->
      local.local_solvers_created <- local.local_solvers_created + 1;
      local.local_selected_logics_reversed <-
        solver_logic :: local.local_selected_logics_reversed

let note_solver_reset = function
  | Global -> incr solver_resets
  | Local local -> local.local_solver_resets <- local.local_solver_resets + 1

let with_solver accounting ?policy config translate use =
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
      let created = Z3.Solver.mk_solver_s context solver_logic in
      solver := Some created;
      note_solver_created accounting;
      Z3.Solver.set_parameters created (parameters ?policy context config);
      Z3.Solver.add created translated.assertions;
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
  match resolve_policy ?rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match resolve_capabilities Global (Logic_ir.requirements query) with
      | Error _ as error -> error
      | Ok () ->
          note_translation Global;
          protect (fun () ->
              with_solver Global ~policy config
                (fun context -> translate_logic_query context query)
                (fun solver _ -> solve_translated controlled solver)))
[@@delator.instrument] [@@delator.level trace]

let solve_query_with accounting ~controlled ~rlimit config query =
  match resolve_policy ~rlimit config with
  | Error _ as error -> error
  | Ok policy -> (
      match resolve_capabilities accounting (Logic_ir.requirements query) with
      | Error _ as error -> error
      | Ok () ->
          note_translation accounting;
          protect (fun () ->
              with_solver accounting ~policy config
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
  parameters

let detached_model_value sort expression =
  match sort with
  | Detached_project_integer ->
      Detached_integer (Z3.Expr.to_string expression)
  | Detached_project_boolean -> (
      match Z3.Boolean.get_bool_value expression with
      | Z3enums.L_TRUE -> Detached_boolean true
      | L_FALSE -> Detached_boolean false
      | L_UNDEF ->
          raise
            (Failure "Boolean model binding has an undefined backend value"))
  | Detached_project_aggregate ->
      Detached_aggregate (Z3.Expr.to_string expression)

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
  local.local_contexts_cleaned <- local.local_contexts_cleaned + 1

let detached_note_solver_created local =
  local.local_solvers_created <- local.local_solvers_created + 1;
  local.local_selected_logics_reversed <-
    solver_logic :: local.local_selected_logics_reversed

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

let detached_feature_supported @ portable = function
  | Logic_ir.Nonlinear_integer_arithmetic -> false
  | Named_sorts | Uninterpreted_functions | Linear_integer_arithmetic
  | Quantifiers | Explicit_patterns | Quantifier_ids | Models
  | Algebraic_datatypes ->
      true

let solve_detached_query_local ~controlled ~timeout_ms ~rlimit ~model
    (Detached_query encoded) =
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
          let created = Z3.Solver.mk_solver_s context solver_logic in
          solver := Some created;
          detached_note_solver_created local;
          Z3.Solver.set_parameters created
            (detached_parameters context ~timeout_ms ~rlimit ~model);
          Z3.Solver.add created translated.detached_assertions_backend;
          [%log.trace "initialized detached Z3 solver"
            ~logic:(Delator.Field.string solver_logic)
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
                          (fun (sort, expression) ->
                            Z3.Model.evaluate backend_model expression true
                            |> Option.map (detached_model_value sort))
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
[@@unsafe_allow_any_mode_crossing
  "The portable closure captures only Z3 datatype operations; every native \
   handle is created, used, and released inside the invoking worker."]

let render_query config query =
  match resolve_capabilities Global (Logic_ir.requirements query) with
  | Error _ as error -> error
  | Ok () ->
      note_translation Global;
      protect (fun () ->
          with_solver Global config
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

let model_value expected_sort expression =
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
  | Parametric _ ->
      raise (Failure "parametric symbols are not projected through aggregate model ABI")

let solve_vir ?(controlled = Real) ?rlimit ?(requires = []) config obligation =
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
              protect (fun () ->
                  with_solver Global ~policy config
                    (fun context ->
                      let logic =
                        translate_logic_query context
                          (Vir_logic_ir_translation_private.query translated)
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
                                      Z3.Model.evaluate model expression true
                                      |> Option.map
                                           (model_value symbol.Vir.sort)
                                    in
                                    { symbol; value })
                                  translated_logic.projected
                              in
                              Ok (Counterexample bindings))))))
[@@delator.instrument] [@@delator.level trace]

let solve_vir_with accounting ~controlled ~rlimit ?(requires = []) config
    obligation =
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
              protect (fun () ->
                  with_solver accounting ~policy config
                    (fun context ->
                      let logic =
                        translate_logic_query context
                          (Vir_logic_ir_translation_private.query translated)
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
                                  Z3.Model.evaluate model expression true
                                  |> Option.map (model_value symbol.Vir.sort)
                                in
                                { symbol; value })
                              translated_logic.projected
                          in
                          Ok (Counterexample bindings)))))
[@@delator.instrument] [@@delator.level trace]

let solve_vir_local ~controlled ~rlimit ?(requires = []) config obligation =
  let local = fresh_local_counters () in
  let result =
    solve_vir_with (Local local) ~controlled ~rlimit ~requires config obligation
  in
  { result; telemetry = local_snapshot local }
[@@delator.instrument] [@@delator.level trace]

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
    solver_logic config.timeout_ms config.model
    (Logic_ir.requirements query
    |> List.map Logic_ir.feature_to_string
    |> String.concat ",")

let version () =
  (Z3.Version.major, Z3.Version.minor, Z3.Version.build, Z3.Version.full_version)
