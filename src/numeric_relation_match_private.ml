type kind = Bounds | Checked | Partial | Modular | Operation
type t = {
  kind : kind;
  declaration : Numeric_semantics_correlation_private.t;
  view : Sst.function_definition;
  clause : Sst.ensures_clause;
  relation : Sst.expression;
  required_guards : Sst.predicate_clause list;
  material : string;
}
let kind_name = function Bounds -> "bounds" | Checked -> "checked-conversion"
  | Partial -> "partial-conversion" | Modular -> "modular-conversion" | Operation -> "operation"
let parameters (definition : Sst.function_definition) =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | Sst.Value_parameter {label = None; optional_default = None; pattern = {pattern_desc = Bind binding; _}} :: rest -> collect (binding :: acc) rest
    | _ -> None in
  if definition.type_binders = [] && not definition.recursive then collect [] definition.parameters else None
let variable binding expression = match expression.Sst.expression_desc with
  | Variable {binding = actual; _} -> actual = binding
  | _ -> false
let direct definition arguments expression = match expression.Sst.expression_desc with
  | Direct_call {call_form = Specification_call; callee; type_arguments = []; arguments = actual; recursive = false}
    when callee = definition.Sst.function_id && List.length arguments = List.length actual ->
      List.for_all2 (fun accepts -> function Sst.Value_argument {label = None; value} -> accepts value | _ -> false) arguments actual
  | _ -> false
let mathematical_call definition arguments expression =
  if expression.Sst.typ <> Sst.Mathematical_int then false else
  match definition.Sst.result_type, expression.expression_desc with
  | Mathematical_int, _ -> direct definition arguments expression
  | Int, Lift_runtime_int operand -> operand.typ = Sst.Int && direct definition arguments operand
  | _ -> false
let constant expected expression = expression.Sst.typ = Sst.Mathematical_int
  && Option.fold ~none:false ~some:(Z.equal expected)
    (Mathematical_int_rewrite_private.constant_value_with Mathematical_int_rewrite_private.default_config expression)
let rec conjuncts expression = match expression.Sst.expression_desc with
  | Boolean_binary (And, a, b) -> conjuncts a @ conjuncts b
  | _ -> [expression]
let equality left right expression = match expression.Sst.expression_desc with
  | Compare (Equal, a, b) -> (left a && right b) || (left b && right a)
  | _ -> false

let match_relation ~kind:(kind [@delator.skip]) ~base_full_key:(base_full_key [@delator.skip])
    ~target:(target [@delator.skip]) ~semantic_width ~signed ~view:(view [@delator.skip])
    ~bounds:(bounds [@delator.skip]) (declaration [@delator.skip]) =
  let result =
    let proof = declaration.Numeric_semantics_correlation_private.definition in
    let width = semantic_width in
    if
      width <= 0
      || width
         >= Mathematical_int_rewrite_private.default_config.max_constant_bits
      || not
           (Build_target_profile_private.instance_logical_bv_width_permitted
              target (Z.of_int width))
    then None else
    let modulus = Z.shift_left Z.one width and half = Z.shift_left Z.one (width - 1) in
    let lower, upper = if signed then Z.neg half, half else Z.zero, modulus in
    match declaration.callable_definition, parameters proof, declaration.kind,
      declaration.role.numeric_role_callable_shape with
    | Some callable, Some proof_parameters, (Completed_proof_declaration | Explicit_axiom_declaration), Some shape
      when proof.result_type = Sst.Unit && callable.mode = Sst.Spec && not callable.recursive
        && callable.type_binders = [] ->
        let compiler_shape = match kind, shape.Numeric_callable_domain_private.parameters, shape.result with
          | Bounds, [Mathematical base], Boolean
          | (Checked | Partial | Modular), [Mathematical base], Carrier -> String.equal base base_full_key
          | Operation, arguments, Carrier -> arguments <> [] && List.for_all ((=) Numeric_callable_domain_private.Carrier) arguments
          | _ -> false in
        let range binding expression =
          let leaves = conjuncts expression in
          let lower_bound expression = match expression.Sst.expression_desc with
            | Compare (Less_or_equal,a,b) -> constant lower a && variable binding b
            | Compare (Greater_or_equal,a,b) -> variable binding a && constant lower b
            | _ -> false in
          let upper_bound expression = match expression.Sst.expression_desc with
            | Compare (Less_than,a,b) -> variable binding a && constant upper b
            | Compare (Greater_than,a,b) -> constant upper a && variable binding b
            | _ -> false in
          List.length leaves = 2 && List.exists lower_bound leaves && List.exists upper_bound leaves in
        let domain binding expression = range binding expression ||
          Option.fold ~none:false ~some:(fun bounds -> direct bounds [variable binding] expression) bounds in
        let requires_domain definition binding = match definition.Sst.contracts.requires with
          | [clause] -> clause.predicate.stage = Logical && domain binding clause.predicate.expression
          | _ -> false in
        let unconditional = proof.contracts.requires = [] && callable.contracts.requires = [] in
        let find matcher =
          List.find_map (fun (clause : Sst.ensures_clause) ->
            if clause.predicate.stage <> Sst.Logical then None else
            List.find_opt matcher (conjuncts clause.predicate.expression)
            |> Option.map (fun relation ->
              let selected = {clause with predicate = {clause.predicate with expression = relation}} in
              let selected_proof = {proof with contracts = {proof.contracts with ensures = [selected]}} in
              let material = Numeric_receipt_private.encode ~schema:"verocaml.numeric-mathematical-relation.v1"
                [kind_name kind; string_of_int clause.clause_index;
                 Sst.to_string {Sst.policy = proof.policy;
                   types = []; parametric_adts = []; functions = [selected_proof]; logical_constants = []}] in
              {kind; declaration; view; clause; relation; required_guards=proof.contracts.requires; material})) proof.contracts.ensures in
        if not compiler_shape then None else
        (match kind, proof_parameters, parameters callable with
        | Bounds, [parameter], Some [_] when parameter.typ = Sst.Mathematical_int && unconditional ->
            find (equality (direct callable [variable parameter]) (range parameter))
        | (Checked | Partial | Modular), [parameter], Some [_]
          when parameter.typ = Sst.Mathematical_int ->
            let projected expression = mathematical_call view [direct callable [variable parameter]] expression in
            let exact = equality projected (variable parameter) in
            (match kind with
            | Checked when requires_domain proof parameter && callable.contracts.requires=[] -> find exact
            | Partial when unconditional ->
                find (fun expression -> match expression.Sst.expression_desc with
                  | Boolean_binary (Or, {expression_desc = Boolean_not premise; _}, conclusion) -> domain parameter premise && exact conclusion
                  | _ -> false)
            | Modular when unconditional && not signed ->
                find (fun expression -> match expression.Sst.expression_desc with
                  | Exists q when q.quantifier_binder.typ = Sst.Mathematical_int ->
                      let product expression = expression.Sst.typ = Sst.Mathematical_int &&
                        match expression.expression_desc with
                        | Checked_arithmetic (Multiply,[a;b]) ->
                            (constant modulus a && variable q.quantifier_binder b) || (variable q.quantifier_binder a && constant modulus b)
                        | Checked_arithmetic (Multiply_constant coefficient,[argument]) ->
                            Z.equal coefficient modulus && variable q.quantifier_binder argument
                        | _ -> false in
                      let sum expression = expression.Sst.typ = Sst.Mathematical_int &&
                        match expression.expression_desc with
                        | Checked_arithmetic (Add,[a;b]) -> (projected a && product b) || (product a && projected b)
                        | _ -> false in
                      equality (variable parameter) sum q.quantifier_body
                  | _ -> false)
            | _ -> None)
        | Operation, parameters, Some callable_parameters
          when List.length parameters = List.length callable_parameters && unconditional ->
            let projected expression = mathematical_call view [direct callable (List.map variable parameters)] expression in
            let nodes = ref 0 in
            let rec arithmetic expression =
              incr nodes;
              !nodes <= 4096 && expression.Sst.typ = Sst.Mathematical_int &&
              (List.exists (fun parameter -> mathematical_call view [variable parameter] expression) parameters
               || match expression.expression_desc with
                 | Int_constant _ -> true
                 | Checked_arithmetic (_, arguments) -> List.for_all arithmetic arguments
                 | If (condition,a,Some b) -> boolean condition && arithmetic a && arithmetic b
                 | _ -> false)
            and boolean expression =
              incr nodes;
              !nodes <= 4096 && expression.Sst.typ = Sst.Bool &&
              match expression.expression_desc with
              | Bool_constant _ -> true
              | Compare (_,a,b) -> arithmetic a && arithmetic b
              | Boolean_not a -> boolean a
              | Boolean_binary (_,a,b) -> boolean a && boolean b
              | _ -> false in
            find (fun expression -> nodes := 0; equality projected arithmetic expression)
        | _ -> None)
    | _ -> None in
  [%log.debug "matched numeric mathematical relation"
    ~kind:(Delator.Field.string (kind_name kind))
    ~callable_uid:(Delator.Field.string declaration.role.numeric_role_callable_uid)
    ~target_width:(Delator.Field.int target.target_claim.width)
    ~semantic_width:(Delator.Field.int semantic_width)
    ~signed_view:(Delator.Field.bool signed)
    ~matched:(Delator.Field.bool (Option.is_some result))
    ~runtime_refinement:(Delator.Field.bool false)];
  result
[@@delator.instrument] [@@delator.level debug]
