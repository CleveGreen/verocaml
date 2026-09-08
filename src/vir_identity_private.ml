let node schema fields = Numeric_receipt_private.encode ~schema fields
let int = string_of_int
let list f xs = Numeric_receipt_private.list (List.map f xs)
let option f = function None -> node "none" [] | Some x -> node "some" [f x]
let typ = Parametric_type.structural_identity_material
let span (s : Diagnostic.span) =
  node "span" [s.file; int s.start_pos.line; int s.start_pos.column;
    int s.end_pos.line; int s.end_pos.column]
let type_id (t : Sst.type_id) = node "type" [int t.type_index; t.type_name]
let function_id (f : Sst.function_id) = node "function" [int f.function_index; f.function_name]
let function_ref (f : Vir.function_ref) = node "function" [int f.function_index; f.function_name]
let constructor (c : Sst.constructor_id) =
  node "constructor" [type_id c.constructor_type; int c.constructor_index; c.constructor_name]
let field (f : Sst.field_id) =
  let owner = match f.field_owner with
    | Record_owner t -> node "record" [type_id t]
    | Constructor_owner c -> node "variant" [constructor c] in
  node "field" [owner; int f.field_index; f.field_name]
let aggregate_type (t : Vir.aggregate_type) =
  node "aggregate-type" [int t.aggregate_type_index; t.aggregate_type_name; list typ t.aggregate_type_arguments]
let bv_width width =
  node "authenticated-bv-width"
    [ Bv_width.to_string width; Bv_width.profile_full_key width;
      option Fun.id (Bv_width.target_full_key width) ]
let sort = function
  | Vir.Integer -> node "integer" [] | Boolean -> node "boolean" []
  | Bit_vector width -> node "bit-vector" [ bv_width width ]
  | Aggregate t -> node "aggregate" [aggregate_type t]
  | Parametric p -> node "parameter" [typ (Parameter p)]
let constant_instance c =
  node "constant-instance" [Logical_constant_instance_private.identity_material c;
    span (Logical_constant_instance_private.span c)]
let symbol (s : Vir.symbol) =
  let role = match s.role with
    | Input -> node "input" [] | Local -> node "local" [] | Result -> node "result" []
    | Logical_constant c -> node "logical-constant" [constant_instance c] in
  node "symbol" [int s.symbol_id; s.source_name; sort s.sort; role; span s.span]
let selector (s : Vir.selector) =
  node "selector" [aggregate_type s.selector_domain; sort s.selector_range;
    s.selector_namespace; int s.selector_index; s.selector_name; list int s.selector_path]
let comparison = function
  | Vir.Equal -> "eq" | Not_equal -> "ne" | Less_than -> "lt"
  | Less_or_equal -> "le" | Greater_than -> "gt" | Greater_or_equal -> "ge"
let rank_fact = function
  | Vir.Ground_rank_base {constructor = c; rank} -> node "ground" [constructor c; Z.to_string rank]
  | Constructor_rank_nonnegative {constructor = c} -> node "nonnegative" [constructor c]
  | Positive_child_rank_smaller {constructor = c; field = f; child_path; child_type} ->
      node "child" [constructor c; field f; list int child_path; aggregate_type child_type]
let rank r = node "rank-domain" [Vir.rank_domain_id r; Vir.rank_domain_version r;
  Vir.rank_domain_digest r; list aggregate_type (Vir.rank_domain_component r);
  list rank_fact (Vir.rank_domain_facts r)]
let adt_field (f : Parametric_adt.field) = node "adt-field"
  [int f.field_index; f.field_name; f.field_uid; typ f.field_type; string_of_bool f.field_mutable]
let adt_constructor (c : Parametric_adt.constructor) = node "adt-constructor"
  [int c.constructor_index; c.constructor_name; c.constructor_uid; list adt_field c.constructor_fields]
let adt_schema s =
  let d = Logical_adt_schema_private.descriptor s in
  let kind = match Parametric_adt.kind d with
    | Record fs -> node "record" [list adt_field fs]
    | Variant cs -> node "variant" [list adt_constructor cs] in
  let provenance = match Parametric_adt.provenance d with
    | Local {compiler_uid} -> node "local" [compiler_uid]
    | External {compiler_uid; proxy_uid} -> node "external" [compiler_uid; proxy_uid] in
  node "adt-schema" [type_id (Parametric_adt.type_id d);
    typ (Application (Parametric_adt.type_constructor d, []));
    list (fun p -> typ (Parameter p)) (Parametric_adt.binders d); provenance; kind;
    string_of_bool (Parametric_adt.is_optional_carrier d);
    list typ (Logical_adt_schema_private.arguments s);
    Logical_adt_schema_private.application_id s; Logical_adt_schema_private.scc_id s]
let callback (c : Sst.callback_binding) =
  node "callback" [int c.callback_id; c.callback_name;
    node "shape" [list (fun (label,t) -> node "endpoint"
      [option Fun.id (Callback_shape_private.label_to_option label); typ t])
      (Callback_shape_private.endpoints c.callback_shape); typ (Callback_shape_private.result c.callback_shape)];
    Callback_certificate_private.identity_material c.callback_certificate; span c.callback_span]
let quantifier_schema q =
  list (fun b -> node "binder" [Logic_quantifier_private.kind_to_string (Logic_quantifier_private.kind b);
    Logic_quantifier_private.owner b; int (Logic_quantifier_private.binder_index b);
    typ (Logic_quantifier_private.binder_type b); span (Logic_quantifier_private.span b);
    Logic_quantifier_private.qid b; Logic_quantifier_private.skid b])
    (Logic_quantifier_private.vector_binders q)

let rec integer = function
  | Vir.Integer_constant z -> node "integer-constant" [Z.to_string z]
  | Integer_symbol s -> node "integer-symbol" [symbol s]
  | Integer_add (a,b) -> node "add" [integer a; integer b]
  | Integer_subtract (a,b) -> node "subtract" [integer a; integer b]
  | Integer_negate a -> node "negate" [integer a]
  | Integer_multiply (a,b) -> node "multiply" [integer a; integer b]
  | Integer_multiply_constant (z,a) -> node "multiply-constant" [Z.to_string z; integer a]
  | Integer_absolute_value a -> node "absolute" [integer a]
  | Integer_conditional (c,a,b) -> node "integer-if" [boolean c; integer a; integer b]
  | Integer_rank_project (r,a) -> node "rank" [rank r; aggregate a]
  | Aggregate_tag (t,a) -> node "tag" [aggregate_type t; aggregate a]
  | Integer_selector (s,a) -> node "integer-select" [selector s; aggregate a]
  | Integer_recursive_spec_application {callee; type_arguments; arguments; span = location} ->
      node "integer-recursive" [function_id callee; list typ type_arguments; list argument arguments; span location]
  | Integer_symbolic_application a -> node "integer-symbolic" [symbolic a]
  | Integer_bv_to_int_unsigned value ->
      node "integer-bv-to-int-unsigned" [ bit_vector value ]
  | Integer_bv_to_int_signed value ->
      node "integer-bv-to-int-signed" [ bit_vector value ]
and bit_vector (t : Vir.bit_vector_term) =
  let desc =
    match t.bit_vector_desc with
    | Vir.Bv_symbol value -> node "symbol" [ symbol value ]
    | Bv_literal value ->
        node "literal" [ Bv_value.canonical_decimal value ]
    | Bv_int_to_bv_mod { input; source_authority } ->
        node "int-to-bv-mod"
          [ integer input;
            Numeric_bv_projection_evidence_private.full_key source_authority ]
    | Bv_conditional (condition, consequent, alternative) ->
        node "conditional"
          [ boolean condition; bit_vector consequent; bit_vector alternative ]
    | Bv_not value -> node "not" [ bit_vector value ]
    | Bv_binary (operation, left, right) ->
        node (Bv_operation_private.binary_name operation)
          [ bit_vector left; bit_vector right ]
    | Bv_selector (selector_, source) ->
        node "selector" [ selector selector_; aggregate source ]
    | Bv_recursive_spec_application
        { callee; type_arguments; arguments; span = location } ->
        node "recursive"
          [ function_id callee; list typ type_arguments;
            list argument arguments; span location ]
    | Bv_symbolic_application application ->
        node "symbolic" [ symbolic application ]
  in
  node "bit-vector-term" [ bv_width t.bit_vector_width; desc ]
and parametric (t : Vir.parametric_term) =
  let desc = match t.parametric_desc with
    | Parametric_symbol s -> node "symbol" [symbol s]
    | Parametric_selector (s,a) -> node "select" [selector s; aggregate a]
    | Parametric_conditional (c,a,b) -> node "if" [boolean c; parametric a; parametric b]
    | Parametric_symbolic_application a -> node "symbolic" [symbolic a] in
  node "parametric" [typ (Parameter t.parametric_sort); desc]
and aggregate (t : Vir.aggregate_term) =
  let desc = match t.aggregate_desc with
    | Aggregate_symbol s -> node "symbol" [symbol s]
    | Aggregate_imported_model_application a ->
        node "imported" [function_id a.callee; a.callable_path; a.callable_uid;
          a.provider_unit; a.provider_interface; a.provider_source; a.provider_family;
          a.provider_import; a.summary_digest; a.closure_digest; a.call_snapshot;
          a.registration_snapshot; int a.invocation_ordinal;
          Imported_callable.aggregate_application_logical_digest a.application_identity;
          list argument a.arguments; aggregate_type a.result_type; span a.span]
    | Aggregate_selector (s,a) -> node "select" [selector s; aggregate a]
    | Aggregate_constructor {constructor = c; arguments} -> node "construct" [constructor c; list argument arguments]
    | Aggregate_record {record_type; fields} ->
        node "record" [type_id record_type; list (fun (f,a) -> node "field" [field f; argument a]) fields]
    | Aggregate_conditional (c,a,b) -> node "if" [boolean c; aggregate a; aggregate b]
    | Aggregate_recursive_spec_application a ->
        (* Process-local application tokens are not semantic identity. The full
           call, argument, result and source occurrence below are. *)
        node "recursive" [function_id a.callee; list typ a.type_arguments;
          list argument a.arguments; aggregate_type a.result_type; span a.span]
    | Aggregate_symbolic_application a -> node "symbolic" [symbolic a] in
  node "aggregate" [aggregate_type t.aggregate_type; desc]
and argument = function
  | Vir.Recursive_integer_argument a -> node "integer" [integer a]
  | Recursive_boolean_argument a -> node "boolean" [boolean a]
  | Recursive_bv_argument a -> node "bit-vector" [bit_vector a]
  | Recursive_aggregate_argument a -> node "aggregate" [aggregate a]
  | Recursive_parametric_argument a -> node "parametric" [parametric a]
and application = function
  | Vir.Integer_application a -> node "integer" [integer a]
  | Boolean_application a -> node "boolean" [boolean a]
  | Bv_application a -> node "bit-vector" [bit_vector a]
  | Aggregate_application a -> node "aggregate" [aggregate a]
  | Parametric_application a -> node "parametric" [parametric a]
and symbolic a = node "symbolic-application"
  [Symbolic_application_private.identity_material a;
   Symbolic_application_private.declaration_identity_material (Symbolic_application_private.declaration a);
   list argument (Symbolic_application_private.arguments a); span (Symbolic_application_private.span a)]
and callback_application (a : Vir.callback_application) =
  node "callback-application" [callback a.callback; list argument a.arguments; span a.call_span]
and quantifier (q : Vir.boolean_quantifier) =
  node "quantifier" [quantifier_schema q.boolean_quantifier_schema;
    quantifier_schema q.boolean_quantifier_expected_schema;
    list typ q.boolean_quantifier_schema_types;
    list sort q.boolean_quantifier_expected_sorts;
    list symbol q.boolean_quantifier_binders; boolean q.boolean_quantifier_body;
    option application q.boolean_quantifier_trigger]
and boolean = function
  | Vir.Logical_adt_schema ds -> node "adt" [list adt_schema ds]
  | Boolean_constant b -> node "boolean-constant" [string_of_bool b]
  | Boolean_symbol s -> node "boolean-symbol" [symbol s]
  | Boolean_not a -> node "not" [boolean a]
  | Boolean_and (a,b) -> node "and" [boolean a; boolean b]
  | Boolean_or (a,b) -> node "or" [boolean a; boolean b]
  | Forall_term q -> node "forall" [quantifier q]
  | Exists_term q -> node "exists" [quantifier q]
  | Integer_compare (c,a,b) -> node "compare" [comparison c; integer a; integer b]
  | Boolean_equal (a,b) -> node "boolean-equal" [boolean a; boolean b]
  | Boolean_not_equal (a,b) -> node "boolean-not-equal" [boolean a; boolean b]
  | Bv_equal (a,b) -> node "bv-equal" [bit_vector a; bit_vector b]
  | Bv_not_equal (a,b) -> node "bv-not-equal" [bit_vector a; bit_vector b]
  | Bv_compare (comparison, a, b) ->
      node ("bv-" ^ Bv_operation_private.comparison_name comparison)
        [ bit_vector a; bit_vector b ]
  | Boolean_selector (s,a) -> node "boolean-select" [selector s; aggregate a]
  | Aggregate_equal (a,b) -> node "aggregate-equal" [aggregate a; aggregate b]
  | Parametric_equal (a,b) -> node "parametric-equal" [parametric a; parametric b]
  | Boolean_invariant_application {invariant_id; model; predicate; value} ->
      node "invariant" [invariant_id; function_id model; function_id predicate; aggregate value]
  | Boolean_recursive_spec_application {callee; type_arguments; arguments; span = location} ->
      node "boolean-recursive" [function_id callee; list typ type_arguments; list argument arguments; span location]
  | Boolean_specification_application {callee; type_arguments; arguments; span = location} ->
      node "boolean-specification" [function_id callee; list typ type_arguments; list argument arguments; span location]
  | Boolean_symbolic_application a -> node "boolean-symbolic" [symbolic a]
  | Callback_requires a -> node "callback-requires" [callback_application a]
  | Callback_ensures {application; result} -> node "callback-ensures" [callback_application application; argument result]

let operation = function
  | Vir.Add -> "add" | Subtract -> "subtract" | Negate -> "negate"
  | Multiply -> "multiply" | Multiply_constant z -> node "multiply-constant" [Z.to_string z]
  | Successor -> "successor" | Predecessor -> "predecessor" | Absolute_value -> "absolute"
let boundary = function
  | Vir.Constructor_establishment -> node "constructor" []
  | Transition_preservation {transition_kind; root_binding_id; pre_version; successor_version} ->
      node "transition" [(match transition_kind with Direct_root_transition -> "direct" | Nested_transition -> "nested" | Rebase_transition -> "rebase");
        int root_binding_id; int pre_version; int successor_version]
  | Call_argument {callee; argument_index} -> node "argument" [function_ref callee; int argument_index]
  | Call_result {callee} -> node "result" [function_ref callee]
  | Function_return -> node "return" []
  | Shared_invariant_close {entry_epoch; final_epoch} -> node "close" [int entry_epoch; int final_epoch]
  | Terminal_observation {operation; snapshot} -> node "observation" [function_ref operation; string_of_bool snapshot]
let kind = function
  | Vir.Arithmetic_safety {operation = op; mathematical_result; violated_bound} ->
      node "arithmetic-safety" [operation op; integer mathematical_result;
        (match violated_bound with Lower_bound -> "lower" | Upper_bound -> "upper")]
  | Assertion {assertion_ordinal} -> node "assertion" [int assertion_ordinal]
  | Local_assertion {local_assertion_ordinal} -> node "local-assertion" [int local_assertion_ordinal]
  | Postcondition {postcondition_ordinal; declaration_span} -> node "postcondition" [int postcondition_ordinal; span declaration_span]
  | Call_precondition {callee; precondition_ordinal; declaration_span; call_span} ->
      node "precondition" [function_ref callee; int precondition_ordinal; span declaration_span; span call_span]
  | Callback_precondition {callback = c; call_span} -> node "callback-precondition" [callback c; span call_span]
  | Invariant_validity {invariant_id; abstract_type; model; predicate; operation; boundary = b} ->
      node "invariant-validity" [invariant_id; aggregate_type abstract_type; function_ref model;
        function_ref predicate; function_ref operation; boundary b]
  | Entry_measure_nonnegative {declaration_span} -> node "entry-measure" [span declaration_span]
  | Recursive_call_measure_nonnegative {callee; declaration_span; call_span} ->
      node "recursive-measure" [function_ref callee; span declaration_span; span call_span]
  | Recursive_call_strict_descent {callee; declaration_span; call_span} ->
      node "strict-descent" [function_ref callee; span declaration_span; span call_span]

let obligation (o : Vir.obligation) =
  let material = node "verocaml.original-vir-obligation.v1"
    [int o.obligation_index; function_ref o.function_ref; kind o.kind; span o.span;
     list boolean o.assumptions; list boolean o.required_preceding_safety;
     list boolean o.path_condition; boolean o.goal; list symbol o.projection_symbols;
     list constant_instance o.logical_constant_instances;
     list (fun (e : Vir.logical_constant_equation) -> node "constant-equation"
       [constant_instance e.logical_constant_instance; application e.logical_constant_rhs;
        span e.logical_constant_span]) o.logical_constant_equations] in
  [%log.trace "serialized original verification obligation"
    ~function_name:(Delator.Field.string o.function_ref.function_name)
    ~obligation_index:(Delator.Field.int o.obligation_index)
    ~canonical_bytes:(Delator.Field.int (String.length material))];
  material
