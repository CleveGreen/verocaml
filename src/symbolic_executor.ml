[@@@warning "-8-9"]

type unsupported =
  | Ghost_call
  | Or_pattern
  | Missing_summary of Sst.function_id
  | Recursion_awaits_totality of Sst.function_id
  | Missing_decreases
  | Duplicate_decreases
  | Inapplicable_decreases
  | Non_integer_decreases
  | Malformed_sst of string

type error = {
  function_name : string;
  span : Diagnostic.span;
  unsupported : unsupported;
}

type value = Logical_spec_evaluation_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of Logical_spec_evaluation_private.function_value

type state = {
  environment : (int * value) list;
  assumptions : Vir.boolean_term list;
  local_invariant_assumptions : Vir.boolean_term list;
  closed_invariant_facts : Vir.boolean_term list;
  required_preceding_safety : Vir.boolean_term list;
  path_condition : Vir.boolean_term list;
  projection_symbols : Vir.symbol list;
  trusted_summary_uses : Vir.trusted_summary_use list;
  next_symbol : int;
  next_obligation : int;
  entry_measure : (Termination.decrease_domain * Vir.integer_term) option;
}

type evaluated = {
  value : value;
  state : state;
}

type 'a path_evaluation = {
  obligations : Vir.obligation list;
  paths : 'a list;
}

type contract_clause = {
  ordinal : int;
  span : Diagnostic.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type summary = {
  definition : Sst.function_definition;
  requires : contract_clause list;
  ensures : contract_clause list;
  assertions : contract_clause list;
  executable_body : Sst.expression;
}

type evaluation_context = {
  validated : Sst_validation.validated_program;
  function_ref : Vir.function_ref;
  current_callable : Sst.function_id;
  definitions : (int * Sst_validation.callable_descriptor) list;
  summaries : (int * summary) list;
  termination : Termination.plan;
  type_definitions : Sst.type_definition list;
  entry_environment : (int * value) list;
  logical : bool;
  old_environment : (int * value) list option;
  spec_call_stack : int list;
  spec_expansion_limit : int;
  rank_domains : Vir.rank_domain list;
  invariants : Type_invariant.environment;
  reached_callback_calls : Vir.reached_callback_call list ref;
  callback_environment :
    (Sst.callback_binding * Sst.callback_binding) list;
}

let vir_aggregate_type (type_id : Sst.type_id) : Vir.aggregate_type =
  {
    aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }

let owner_type = function
  | Sst.Record_owner type_id -> type_id
  | Sst.Constructor_owner constructor -> constructor.constructor_type

let owner_namespace = function
  | Sst.Record_owner type_id ->
      Printf.sprintf "t%d_%s_record" type_id.type_index type_id.type_name
  | Sst.Constructor_owner constructor ->
      Printf.sprintf "t%d_%s_c%d_%s_inline"
        constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name

let field_selector (field : Sst.field_id) path selector_range : Vir.selector =
  {
    selector_domain = vir_aggregate_type (owner_type field.field_owner);
    selector_range;
    selector_namespace = owner_namespace field.field_owner;
    selector_index = field.field_index;
    selector_name = field.field_name;
    selector_path = path;
  }

let argument_selector (constructor : Sst.constructor_id) index path selector_range :
    Vir.selector =
  {
    selector_domain = vir_aggregate_type constructor.constructor_type;
    selector_range;
    selector_namespace =
      Printf.sprintf "t%d_%s_c%d_%s" constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name;
    selector_index = index;
    selector_name = Printf.sprintf "$arg%d" index;
    selector_path = path;
  }

let ( let* ) result f = match result with Ok value -> f value | Error _ as e -> e

let error function_name span unsupported =
  Error { function_name; span; unsupported }

let function_ref (definition : Sst.function_definition) : Vir.function_ref =
  {
    function_index = definition.function_id.function_index;
    function_name = definition.function_id.function_name;
  }

let vir_function_ref (id : Sst.function_id) : Vir.function_ref =
  {
    function_index = id.function_index;
    function_name = id.function_name;
  }

let append left right = left @ right

let with_assumptions state assumptions =
  { state with assumptions = append state.assumptions assumptions }

let with_local_invariant_assumption state assumption =
  {
    state with
    local_invariant_assumptions =
      append state.local_invariant_assumptions [ assumption ];
  }

let with_closed_invariant_fact state fact =
  {
    state with
    closed_invariant_facts = append state.closed_invariant_facts [ fact ];
  }

let effective_assumptions state =
  append state.assumptions state.local_invariant_assumptions

let with_path state term =
  { state with path_condition = append state.path_condition [ term ] }

let vir_comparison = function
  | Sst.Equal -> Vir.Equal
  | Sst.Not_equal -> Vir.Not_equal
  | Sst.Less_than -> Vir.Less_than
  | Sst.Less_or_equal -> Vir.Less_or_equal
  | Sst.Greater_than -> Vir.Greater_than
  | Sst.Greater_or_equal -> Vir.Greater_or_equal

let fresh_symbol state ~source_name ~sort ~role ~span ~project =
  let symbol : Vir.symbol =
    {
      symbol_id = state.next_symbol;
      source_name;
      sort;
      role;
      span;
    }
  in
  let state =
    {
      state with
      next_symbol = state.next_symbol + 1;
      projection_symbols =
        if project then append state.projection_symbols [ symbol ]
        else state.projection_symbols;
    }
  in
  (symbol, state)

let rec equality left right =
  match (left, right) with
  | Integer_value left, Integer_value right ->
      Some (Vir.Integer_compare (Equal, left, right))
  | Boolean_value left, Boolean_value right ->
      Some (Vir.Boolean_equal (left, right))
  | Unit_value, Unit_value -> Some (Vir.Boolean_constant true)
  | Aggregate_value left, Aggregate_value right
    when left.aggregate_type = right.aggregate_type ->
      Some (Vir.Aggregate_equal (left, right))
  | Tuple_value left, Tuple_value right when List.length left = List.length right ->
      let equations = List.map2 equality left right in
      if List.for_all Option.is_some equations then
        Some
          (List.fold_left
             (fun combined equation ->
               Vir.Boolean_and (combined, Option.get equation))
             (Vir.Boolean_constant true) equations)
      else None
  | _ -> None

let rec fresh_value state ~source_name ~role ~span ~project = function
  | Sst.Unit -> Ok (Unit_value, state)
  | Sst.Int ->
      let symbol, state =
        fresh_symbol state ~source_name ~sort:Integer ~role ~span ~project
      in
      let term = Vir.Integer_symbol symbol in
      let state = with_assumptions state (Vir.integer_range term) in
      Ok (Integer_value term, state)
  | Sst.Bool ->
      let symbol, state =
        fresh_symbol state ~source_name ~sort:Boolean ~role ~span ~project
      in
      Ok (Boolean_value (Vir.Boolean_symbol symbol), state)
  | Sst.Tuple components ->
      let rec loop index state values = function
        | [] -> Ok (Tuple_value (List.rev values), state)
        | (label, typ) :: rest ->
            let component_name =
              match label with
              | Some label -> source_name ^ "." ^ label
              | None -> Printf.sprintf "%s.%d" source_name index
            in
            let* value, state =
              fresh_value state ~source_name:component_name ~role ~span
                ~project typ
            in
            loop (index + 1) state (value :: values) rest
      in
      loop 0 state [] components
  | Sst.Aggregate type_id ->
      let aggregate_type = vir_aggregate_type type_id in
      let symbol, state =
        fresh_symbol state ~source_name ~sort:(Vir.Aggregate aggregate_type)
          ~role ~span ~project
      in
      Ok
        ( Aggregate_value
            { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol },
          state )

let rec selected_value state aggregate make_selector path = function
  | Sst.Unit -> Ok (Unit_value, state)
  | Sst.Int ->
      let term = Vir.Integer_selector (make_selector path Vir.Integer, aggregate) in
      Ok (Integer_value term, with_assumptions state (Vir.integer_range term))
  | Sst.Bool ->
      Ok
        ( Boolean_value
            (Vir.Boolean_selector (make_selector path Vir.Boolean, aggregate)),
          state )
  | Sst.Aggregate type_id ->
      let aggregate_type = vir_aggregate_type type_id in
      Ok
        ( Aggregate_value
            {
              Vir.aggregate_type;
              aggregate_desc =
                Vir.Aggregate_selector
                  (make_selector path (Vir.Aggregate aggregate_type), aggregate);
            },
          state )
  | Sst.Tuple components ->
      let rec loop index state values = function
        | [] -> Ok (Tuple_value (List.rev values), state)
        | (_, typ) :: rest ->
            let* value, state =
              selected_value state aggregate make_selector (path @ [ index ]) typ
            in
            loop (index + 1) state (value :: values) rest
      in
      loop 0 state [] components

let select_field state aggregate field typ =
  selected_value state aggregate (field_selector field) [] typ

type owned_tree_breadcrumb =
  | Owned_field_parent of {
      aggregate : Vir.aggregate_term;
      field : Sst.field_id;
      fields : Sst.field_definition list;
    }
  | Owned_constructor_parent of {
      aggregate : Vir.aggregate_term;
      constructor : Sst.constructor_id;
    }

let owned_fields context owner =
  let type_id = owner_type owner in
  match
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        definition.type_id = type_id)
      context.type_definitions
  with
  | Some { type_kind = Sst.Record_definition fields; _ } -> (
      match owner with Sst.Record_owner _ -> fields | Sst.Constructor_owner _ -> [])
  | Some { type_kind = Sst.Variant_definition constructors; _ } -> (
      match owner with
      | Sst.Record_owner _ -> []
      | Sst.Constructor_owner constructor ->
          Option.value ~default:[]
            (List.find_map
               (fun (definition : Sst.constructor_definition) ->
                 if definition.constructor_id = constructor then
                   Some definition.constructor_fields
                 else None)
               constructors))
  | None -> []

let fresh_owned_aggregate state root span type_id suffix =
  let aggregate_type = vir_aggregate_type type_id in
  let symbol, state =
    fresh_symbol state ~source_name:(root.Sst.name ^ suffix)
      ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local ~span ~project:true
  in
  ( {
      Vir.aggregate_type;
      aggregate_desc = Vir.Aggregate_symbol symbol;
    },
    state )

let rebuild_owned_fields function_name context span root state old_aggregate
    owner changed_field changed_value =
  let fields = owned_fields context owner in
  if fields = [] then
    error function_name span
      (Malformed_sst "owned-tree reconstruction owner has no fields")
  else
    let type_id = owner_type owner in
    let fresh_aggregate, state =
      fresh_owned_aggregate state root span type_id ".owned-state"
    in
    let rec loop state equations = function
      | [] ->
          Ok
            ( Aggregate_value fresh_aggregate,
              with_assumptions state (List.rev equations) )
      | definition :: rest ->
          let* selected_new, state =
            select_field state fresh_aggregate definition.Sst.field_id
              definition.field_type
          in
          let* rhs, state =
            if definition.field_id = changed_field then
              Ok (changed_value, state)
            else
              select_field state old_aggregate definition.field_id
                definition.field_type
          in
          let* equation =
            match equality selected_new rhs with
            | Some equation -> Ok equation
            | None ->
                error function_name span
                  (Malformed_sst
                     "owned-tree reconstruction field has the wrong type")
          in
          loop state (equation :: equations) rest
    in
    loop state [] fields

let descend_owned_path function_name context span state root_aggregate path =
  let rec loop state value breadcrumbs = function
    | [] -> Ok (value, breadcrumbs, state)
    | Sst.Owned_tree_field field :: rest -> (
        match value with
        | Aggregate_value aggregate ->
            let fields = owned_fields context field.field_owner in
            let* definition =
              match
                List.find_opt
                  (fun (definition : Sst.field_definition) ->
                    definition.field_id = field)
                  fields
              with
              | Some definition -> Ok definition
              | None ->
                  error function_name span
                    (Malformed_sst "owned-tree path field is not registered")
            in
            let* selected, state =
              select_field state aggregate field definition.field_type
            in
            loop state selected
              (Owned_field_parent { aggregate; field; fields } :: breadcrumbs)
              rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree field path crossed a scalar"))
    | Sst.Owned_tree_constructor constructor :: rest -> (
        match value with
        | Aggregate_value aggregate ->
            let payload_type = Sst.Aggregate constructor.constructor_type in
            let* selected, state =
              selected_value state aggregate
                (argument_selector constructor 0) [] payload_type
            in
            loop state selected
              (Owned_constructor_parent { aggregate; constructor } :: breadcrumbs)
              rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree constructor path crossed a scalar"))
  in
  loop state (Aggregate_value root_aggregate) [] path

let rebuild_owned_ancestors function_name context span root state value
    breadcrumbs =
  let rec loop state value = function
    | [] -> Ok (value, state)
    | Owned_field_parent { aggregate; field; fields = _ } :: rest ->
        let* value, state =
          rebuild_owned_fields function_name context span root state aggregate
            field.field_owner field value
        in
        loop state value rest
    | Owned_constructor_parent { aggregate = _; constructor } :: rest -> (
        match value with
        | Aggregate_value payload ->
            let fresh, state =
              fresh_owned_aggregate state root span constructor.constructor_type
                ".owned-constructor"
            in
            let tag_equation =
              Vir.Integer_compare
                ( Vir.Equal,
                  Vir.Aggregate_tag
                    (vir_aggregate_type constructor.constructor_type, fresh),
                  Vir.Integer_constant
                    (Z.of_int constructor.constructor_index) )
            in
            let payload_value =
              Aggregate_value
                {
                  Vir.aggregate_type =
                    vir_aggregate_type constructor.constructor_type;
                  aggregate_desc =
                    Vir.Aggregate_selector
                      ( argument_selector constructor 0 []
                          (Vir.Aggregate
                             (vir_aggregate_type constructor.constructor_type)),
                        fresh );
                }
            in
            let* payload_equation =
              match equality payload_value (Aggregate_value payload) with
              | Some equation -> Ok equation
              | None ->
                  error function_name span
                    (Malformed_sst
                       "owned-tree constructor payload has the wrong type")
            in
            loop
              (with_assumptions state [ tag_equation; payload_equation ])
              (Aggregate_value fresh) rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree constructor payload is not aggregate"))
  in
  loop state value breadcrumbs

let rec selected_value_without_state aggregate make_selector path = function
  | Sst.Unit -> Unit_value
  | Sst.Int ->
      Integer_value (Vir.Integer_selector (make_selector path Vir.Integer, aggregate))
  | Sst.Bool ->
      Boolean_value
        (Vir.Boolean_selector (make_selector path Vir.Boolean, aggregate))
  | Sst.Aggregate type_id ->
      Aggregate_value
        {
          Vir.aggregate_type = vir_aggregate_type type_id;
          aggregate_desc =
            Vir.Aggregate_selector
              ( make_selector path
                  (Vir.Aggregate (vir_aggregate_type type_id)),
                aggregate );
        }
  | Sst.Tuple components ->
      Tuple_value
        (List.mapi
           (fun index (_, typ) ->
             selected_value_without_state aggregate make_selector
               (path @ [ index ]) typ)
           components)

let rec ranges_of_value = function
  | Integer_value term -> Vir.integer_range term
  | Tuple_value values -> List.concat_map ranges_of_value values
  | Unit_value | Boolean_value _ | Aggregate_value _ -> []

let bind_scalar state (binding : Sst.binding) value =
  let sort =
    match binding.typ with
    | Sst.Int -> Some Vir.Integer
    | Sst.Bool -> Some Vir.Boolean
    | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ -> None
  in
  match sort with
  | None ->
      Ok
        {
          state with
          environment = (binding.id, value) :: state.environment;
        }
  | Some sort ->
      let symbol, state =
        fresh_symbol state ~source_name:binding.name ~sort ~role:Vir.Local
          ~span:binding.span ~project:true
      in
      let alias =
        match sort with
        | Vir.Integer -> Integer_value (Vir.Integer_symbol symbol)
        | Vir.Boolean -> Boolean_value (Vir.Boolean_symbol symbol)
        | Vir.Aggregate _ -> assert false
      in
      let* equation =
        match equality alias value with
        | Some equation -> Ok equation
        | None ->
            Error
              {
                function_name = "";
                span = binding.span;
                unsupported = Malformed_sst "binding type/value mismatch";
              }
      in
      let assumptions =
        match alias with
        | Integer_value term -> equation :: Vir.integer_range term
        | Boolean_value _ -> [ equation ]
        | Unit_value | Tuple_value _ | Aggregate_value _ -> assert false
      in
      Ok
        {
          state with
          environment = (binding.id, alias) :: state.environment;
          assumptions = append state.assumptions assumptions;
        }

let rec bind_pattern function_name state (pattern : Sst.pattern) value =
  match pattern.pattern_desc with
  | Sst.Wildcard -> Ok state
  | Sst.Bind binding -> (
      match bind_scalar state binding value with
      | Ok state -> Ok state
      | Error error -> Error { error with function_name })
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as an irrefutable binding")
  | Sst.Unit_pattern -> (
      match value with
      | Unit_value -> Ok state
      | _ ->
          error function_name pattern.span
            (Malformed_sst "unit pattern type/value mismatch"))
  | Sst.Tuple_pattern patterns -> (
      match value with
      | Tuple_value values when List.length patterns = List.length values ->
          List.fold_left2
            (fun result (_, pattern) value ->
              let* state = result in
              bind_pattern function_name state pattern value)
            (Ok state) patterns values
      | _ ->
          error function_name pattern.span
            (Malformed_sst "tuple pattern type/value mismatch"))
  | Sst.Record_pattern fields -> (
      match value with
      | Aggregate_value aggregate ->
          List.fold_left
            (fun result (field, (pattern : Sst.pattern)) ->
              let* state = result in
              let* selected, state =
                select_field state aggregate field pattern.Sst.typ
              in
              bind_pattern function_name state pattern selected)
            (Ok state) fields
      | _ ->
          error function_name pattern.span
            (Malformed_sst "record pattern type/value mismatch"))
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable constructor used as a binding pattern")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable binding pattern reached VIR lowering")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern

let rec bind_pattern_direct function_name environment
    (pattern : Sst.pattern) value =
  match pattern.pattern_desc with
  | Sst.Wildcard -> Ok (environment, [])
  | Sst.Bind binding -> Ok ((binding.id, value) :: environment, [])
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as an irrefutable binding")
  | Sst.Unit_pattern -> (
      match value with
      | Unit_value -> Ok (environment, [])
      | _ ->
          error function_name pattern.span
            (Malformed_sst "unit pattern type/value mismatch"))
  | Sst.Tuple_pattern patterns -> (
      match value with
      | Tuple_value values when List.length patterns = List.length values ->
          List.fold_left2
            (fun result (_, pattern) value ->
              let* environment, ranges = result in
              let* environment, nested_ranges =
                bind_pattern_direct function_name environment pattern value
              in
              Ok (environment, ranges @ nested_ranges))
            (Ok (environment, [])) patterns values
      | _ ->
          error function_name pattern.span
            (Malformed_sst "tuple pattern type/value mismatch"))
  | Sst.Record_pattern fields -> (
      match value with
      | Aggregate_value aggregate ->
          List.fold_left
            (fun result (field, (pattern : Sst.pattern)) ->
              let* environment, ranges = result in
              let selected =
                selected_value_without_state aggregate (field_selector field) []
                  pattern.Sst.typ
              in
              let* environment, nested_ranges =
                bind_pattern_direct function_name environment pattern selected
              in
              Ok
                ( environment,
                  ranges @ ranges_of_value selected @ nested_ranges ))
            (Ok (environment, [])) fields
      | _ ->
          error function_name pattern.span
            (Malformed_sst "record pattern type/value mismatch"))
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable constructor used as an irrefutable binding")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable pattern used as an irrefutable binding")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern

let rec fresh_parameter function_name state index (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Bind binding ->
      let* value, state =
        fresh_value state ~source_name:binding.name ~role:Vir.Input
          ~span:binding.span ~project:true binding.typ
      in
      Ok
        (value, { state with environment = (binding.id, value) :: state.environment })
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as a function parameter")
  | Sst.Wildcard ->
      fresh_value state
        ~source_name:(Printf.sprintf "_parameter%d" index)
        ~role:Vir.Input ~span:pattern.span ~project:false pattern.typ
  | Sst.Unit_pattern -> Ok (Unit_value, state)
  | Sst.Tuple_pattern patterns ->
      let rec loop component state values = function
        | [] -> Ok (Tuple_value (List.rev values), state)
        | (_, pattern) :: rest ->
            let* value, state =
              fresh_parameter function_name state
                ((index * 1000) + component + 1)
                pattern
            in
            loop (component + 1) state (value :: values) rest
      in
      loop 0 state [] patterns
  | Sst.Record_pattern _ ->
      let* value, state =
        fresh_value state
          ~source_name:(Printf.sprintf "_parameter%d" index)
          ~role:Vir.Input ~span:pattern.span ~project:true pattern.typ
      in
      let* state = bind_pattern function_name state pattern value in
      Ok (value, state)
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable constructor function parameter reached VIR lowering")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable function parameter reached VIR lowering")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern

let expect_integer function_name span = function
  | Integer_value term -> Ok term
  | _ -> error function_name span (Malformed_sst "expected integer value")

let expect_boolean function_name span = function
  | Boolean_value term -> Ok term
  | _ -> error function_name span (Malformed_sst "expected Boolean value")

let ranked_measure function_name span rank_domains domain value =
  match (domain, value) with
  | Termination.Integer_height, Integer_value term -> Ok term
  | Termination.Structural_rank certificate, Aggregate_value aggregate ->
      let rank_id = Termination.structural_rank_id certificate in
      (match
         List.find_opt
           (fun domain -> String.equal (Vir.rank_domain_id domain) rank_id)
           rank_domains
       with
      | Some domain ->
          if
            List.exists
              (( = ) aggregate.Vir.aggregate_type)
              (Vir.rank_domain_component domain)
          then Ok (Vir.Integer_rank_project (domain, aggregate))
          else
            error function_name span
              (Malformed_sst
                 "structural measure type is outside its sealed rank domain")
      | None ->
          error function_name span
            (Malformed_sst "sealed structural rank domain is unavailable"))
  | Termination.Frozen_spine_direct_edge frozen, Aggregate_value aggregate ->
      Ok
        (match aggregate.Vir.aggregate_desc with
        | Vir.Aggregate_selector (selector, _)
          when
            selector.selector_index
              = frozen.Sst.frozen_next_child_field.field_index
            && selector.selector_domain.aggregate_type_index
               = frozen.frozen_link.type_index
            && String.equal selector.selector_domain.aggregate_type_name
                 frozen.frozen_link.type_name ->
            Vir.Integer_constant Z.zero
        | _ -> Vir.Integer_constant Z.one)
  | ( Termination.Integer_height
    | Termination.Structural_rank _
    | Termination.Frozen_spine_direct_edge _ ),
    _ ->
      error function_name span
        (Malformed_sst "recursive measure value has the wrong sealed domain")

let evaluate_contexts evaluate contexts =
  let rec loop obligations paths = function
    | [] -> Ok { obligations; paths }
    | context :: rest ->
        let* evaluated = evaluate context in
        loop
          (append obligations evaluated.obligations)
          (append paths evaluated.paths) rest
  in
  loop [] [] contexts

let arithmetic_operation = function
  | Sst.Add -> Vir.Add
  | Sst.Subtract -> Vir.Subtract
  | Sst.Negate -> Vir.Negate
  | Sst.Multiply_constant value -> Vir.Multiply_constant value
  | Sst.Successor -> Vir.Successor
  | Sst.Predecessor -> Vir.Predecessor
  | Sst.Absolute_value -> Vir.Absolute_value

let arithmetic_term function_name span operation arguments =
  let* arguments =
    let rec loop values = function
      | [] -> Ok (List.rev values)
      | value :: rest ->
          let* value = expect_integer function_name span value in
          loop (value :: values) rest
    in
    loop [] arguments
  in
  match (operation, arguments) with
  | Sst.Add, [ left; right ] -> Ok (Vir.Integer_add (left, right))
  | Sst.Subtract, [ left; right ] ->
      Ok (Vir.Integer_subtract (left, right))
  | Sst.Negate, [ value ] -> Ok (Vir.Integer_negate value)
  | Sst.Multiply_constant constant, [ value ] ->
      Ok (Vir.Integer_multiply_constant (constant, value))
  | Sst.Successor, [ value ] ->
      Ok (Vir.Integer_add (value, Vir.Integer_constant Z.one))
  | Sst.Predecessor, [ value ] ->
      Ok (Vir.Integer_subtract (value, Vir.Integer_constant Z.one))
  | Sst.Absolute_value, [ value ] ->
      Ok (Vir.Integer_absolute_value value)
  | _ ->
      error function_name span
        (Malformed_sst "checked arithmetic has an invalid arity")

let emit_checked function_ref span operation mathematical_result state =
  let lower, upper =
    match Vir.integer_range mathematical_result with
    | [ lower; upper ] -> (lower, upper)
    | _ -> assert false
  in
  let make obligation_index violated_bound goal =
    {
      Vir.obligation_index;
      function_ref;
      kind =
        Arithmetic_safety
          {
            operation = arithmetic_operation operation;
            mathematical_result;
            violated_bound;
          };
      span;
      assumptions = effective_assumptions state;
      required_preceding_safety = state.required_preceding_safety;
      path_condition = state.path_condition;
      goal;
      projection_symbols = state.projection_symbols;
    }
  in
  let obligations =
    [
      make state.next_obligation Vir.Lower_bound lower;
      make (state.next_obligation + 1) Vir.Upper_bound upper;
    ]
  in
  (* Neither bound is present in either obligation's assumptions.  Both become
     certified facts only after both ordered obligations have been emitted, so
     a later operation may rely on earlier safety without circularly proving
     the operation that introduced the mathematical result. *)
  let state =
    {
      state with
      assumptions = append state.assumptions [ lower; upper ];
      required_preceding_safety =
        append state.required_preceding_safety [ lower; upper ];
      next_obligation = state.next_obligation + 2;
    }
  in
  { obligations; paths = [ { value = Integer_value mathematical_result; state } ] }

let emit_goal function_ref kind span goal state =
  let obligation =
    {
      Vir.obligation_index = state.next_obligation;
      function_ref;
      kind;
      span;
      assumptions = effective_assumptions state;
      required_preceding_safety = state.required_preceding_safety;
      path_condition = state.path_condition;
      goal;
      projection_symbols = state.projection_symbols;
    }
  in
  let state =
    {
      state with
      assumptions = append state.assumptions [ goal ];
      next_obligation = state.next_obligation + 1;
    }
  in
  (obligation, state)

let closed_invariant_application handle = function
  | Aggregate_value value
    when
      value.Vir.aggregate_type
      = vir_aggregate_type (Type_invariant.abstract_type handle) ->
      Ok
        (Vir.Boolean_invariant_application
           {
             invariant_id = Type_invariant.invariant_id handle;
             model = Type_invariant.model_callable handle;
             predicate = Type_invariant.predicate_callable handle;
             value;
           })
  | Aggregate_value _ | Unit_value | Integer_value _ | Boolean_value _
  | Tuple_value _ ->
      Error "invariant boundary value does not have the exact abstract type"

let emit_closed_invariant_goal ?(verified_assumptions = []) context handle
    boundary operation span value state =
  match closed_invariant_application handle value with
  | Error message ->
      error context.function_ref.function_name span (Malformed_sst message)
  | Ok goal ->
      let kind =
        Vir.Invariant_validity
          {
            invariant_id = Type_invariant.invariant_id handle;
            abstract_type =
              Type_invariant.abstract_type handle |> vir_aggregate_type;
            model = Type_invariant.model_callable handle |> vir_function_ref;
            predicate =
              Type_invariant.predicate_callable handle |> vir_function_ref;
            operation;
            boundary;
          }
      in
      let obligation =
        {
          Vir.obligation_index = state.next_obligation;
          function_ref = context.function_ref;
          kind;
          span;
          assumptions =
            append
              (append (effective_assumptions state)
                 state.closed_invariant_facts)
              verified_assumptions;
          required_preceding_safety = state.required_preceding_safety;
          path_condition = state.path_condition;
          goal;
          projection_symbols = state.projection_symbols;
        }
      in
      let state =
        {
          (with_closed_invariant_fact state goal) with
          next_obligation = state.next_obligation + 1;
        }
      in
      Ok (obligation, state)

let invariant_for_typ environment = function
  | Sst.Aggregate type_id -> Type_invariant.find_for_type environment type_id
  | Sst.Unit | Sst.Int | Sst.Bool | Sst.Tuple _ -> None

let boolean_and terms =
  List.fold_left
    (fun left right -> Vir.Boolean_and (left, right))
    (Vir.Boolean_constant true) terms

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name

let same_constructor_id (left : Sst.constructor_id)
    (right : Sst.constructor_id) =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name

let constrain_immediate_record_fields function_name span type_definitions state
    type_id aggregate =
  match
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        same_type_id definition.type_id type_id)
      type_definitions
  with
  | Some { type_kind = Sst.Record_definition fields; _ } ->
      List.fold_left
        (fun state (field : Sst.field_definition) ->
          let* state = state in
          let* _, state =
            select_field state aggregate field.field_id field.field_type
          in
          Ok state)
        (Ok state) fields
  | Some { type_kind = Sst.Variant_definition _; _ } ->
      error function_name span
        (Malformed_sst "unique returned parameter is not a record")
  | None ->
      error function_name span
        (Malformed_sst "unique returned record type is not registered")

let variant_tag_domain function_name span type_definitions
    (constructor : Sst.constructor_id) (aggregate : Vir.aggregate_term) =
  let aggregate_type = constructor.constructor_type in
  let vir_type = vir_aggregate_type aggregate_type in
  if aggregate.aggregate_type <> vir_type then
    error function_name span
      (Malformed_sst "constructor pattern has the wrong aggregate type")
  else
    match
      List.find_opt
        (fun (definition : Sst.type_definition) ->
          same_type_id definition.type_id aggregate_type)
        type_definitions
    with
    | None ->
        error function_name span
          (Malformed_sst "constructor pattern refers to an unknown variant type")
    | Some { type_kind = Sst.Record_definition _; _ } ->
        error function_name span
          (Malformed_sst "constructor pattern refers to a record type")
    | Some { type_kind = Sst.Variant_definition constructors; _ } ->
        if
          not
            (List.exists
               (fun (definition : Sst.constructor_definition) ->
                 same_constructor_id definition.constructor_id constructor)
               constructors)
        then
          error function_name span
            (Malformed_sst
               "constructor pattern does not belong to its declared variant")
        else
          let tag = Vir.Aggregate_tag (vir_type, aggregate) in
          let alternatives =
            List.mapi
              (fun ordinal (definition : Sst.constructor_definition) ->
                let declared = definition.constructor_id in
                if
                  (not
                     (same_type_id declared.constructor_type aggregate_type))
                  || declared.constructor_index <> ordinal
                then
                  error function_name definition.span
                    (Malformed_sst
                       "variant registry contains a constructor with an invalid \
                        owner or ordinal")
                else
                  Ok
                    (Vir.Integer_compare
                       ( Vir.Equal,
                         tag,
                         Vir.Integer_constant (Z.of_int ordinal) )))
              constructors
          in
          let rec collect reversed = function
            | [] -> Ok (List.rev reversed)
            | Ok alternative :: rest ->
                collect (alternative :: reversed) rest
            | (Error _ as error) :: _ -> error
          in
          let* alternatives = collect [] alternatives in
          (match alternatives with
          | [] ->
              error function_name span
                (Malformed_sst "variant type has no constructors")
          | first :: rest ->
              Ok
                (List.fold_left
                   (fun domain alternative ->
                     Vir.Boolean_or (domain, alternative))
                   first rest))

let rec pattern_condition_and_bindings function_name type_definitions
    (pattern : Sst.pattern) value =
  match (pattern.pattern_desc, value) with
  | Sst.Wildcard, _ -> Ok (Vir.Boolean_constant true, [], [])
  | Sst.Bind binding, value ->
      Ok (Vir.Boolean_constant true, [ (binding.id, value) ], [])
  | Sst.Owned_tree_cursor_pattern _, _ ->
      Ok (Vir.Boolean_constant true, [], [])
  | Sst.Unit_pattern, Unit_value -> Ok (Vir.Boolean_constant true, [], [])
  | Sst.Int_pattern expected, Integer_value actual ->
      Ok
        ( Vir.Integer_compare
            (Vir.Equal, actual, Vir.Integer_constant expected),
          [], [] )
  | Sst.Bool_pattern expected, Boolean_value actual ->
      Ok
        ( Vir.Boolean_equal (actual, Vir.Boolean_constant expected),
          [], [] )
  | Sst.Tuple_pattern patterns, Tuple_value values
    when List.length patterns = List.length values ->
      let rec loop conditions bindings ranges patterns values =
        match (patterns, values) with
        | [], [] ->
            Ok
              ( boolean_and (List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (_, pattern) :: patterns, value :: values ->
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                pattern value
            in
          loop (condition :: conditions)
              (List.rev_append nested bindings)
              (nested_ranges :: ranges) patterns values
        | _ -> assert false
      in
      loop [] [] [] patterns values
  | Sst.Record_pattern fields, Aggregate_value aggregate ->
      let rec loop conditions bindings ranges = function
        | [] ->
            Ok
              ( boolean_and (List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (field, (pattern : Sst.pattern)) :: rest ->
            let selected =
              selected_value_without_state aggregate (field_selector field) []
                pattern.Sst.typ
            in
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                pattern selected
            in
            loop (condition :: conditions)
              (List.rev_append nested bindings)
              (ranges_of_value selected :: nested_ranges :: ranges)
              rest
      in
      loop [] [] [] fields
  | Sst.Constructor_pattern (constructor, arguments),
    Aggregate_value aggregate ->
      let* tag_domain =
        variant_tag_domain function_name pattern.span type_definitions
          constructor aggregate
      in
      let tag =
        Vir.Integer_compare
          ( Vir.Equal,
            Vir.Aggregate_tag
              (vir_aggregate_type constructor.constructor_type, aggregate),
            Vir.Integer_constant (Z.of_int constructor.constructor_index) )
      in
      let rec loop index conditions bindings ranges = function
        | [] ->
            Ok
              ( boolean_and (tag :: List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (pattern : Sst.pattern) :: rest ->
            let selected =
              selected_value_without_state aggregate
                (argument_selector constructor index) [] pattern.Sst.typ
            in
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                pattern selected
            in
            loop (index + 1) (condition :: conditions)
              (List.rev_append nested bindings)
              (ranges_of_value selected :: nested_ranges :: ranges)
              rest
      in
      let* condition, bindings, ranges = loop 0 [] [] [] arguments in
      Ok (condition, bindings, tag_domain :: ranges)
  | Sst.Or_pattern _, _ -> error function_name pattern.span Or_pattern
  | _ ->
      error function_name pattern.span
        (Malformed_sst "pattern type/value mismatch")

let add_pattern_bindings state bindings =
  { state with environment = bindings @ state.environment }

let restore_environment environment state = { state with environment }

let remove_binding binding_id environment =
  List.filter (fun (candidate, _) -> candidate <> binding_id) environment

let replace_binding binding value environment =
  (binding.Sst.id, value) :: remove_binding binding.id environment

let find_summary summaries (id : Sst.function_id) =
  match List.assoc_opt id.function_index summaries with
  | Some summary
    when String.equal summary.definition.function_id.function_name
           id.function_name ->
      Some summary
  | _ -> None

let callback_runtime evaluate context =
  {
    Call_contract_execution_private.evaluate =
      (fun context expression state ->
        evaluate context expression state
        |> Result.map (fun evaluated ->
               {
                 Call_contract_execution_private.obligations =
                   evaluated.obligations;
                 paths = evaluated.paths;
               }));
    value = (fun evaluated -> evaluated.value);
    state = (fun evaluated -> evaluated.state);
    evaluated = (fun value state -> { value; state });
    function_ref = (fun context -> context.function_ref);
    logical = (fun context -> context.logical);
    callback_environment = (fun context -> context.callback_environment);
    find_summary =
      (fun context id -> find_summary context.summaries id);
    definition = (fun summary -> summary.definition);
    requires_clauses =
      (fun summary ->
        List.map
          (fun clause ->
            {
              Call_contract_execution_private.ordinal = clause.ordinal;
              span = clause.span;
              binder = clause.binder;
              payload = clause.payload;
            })
          summary.requires);
    ensures_clauses =
      (fun summary ->
        List.map
          (fun clause ->
            {
              Call_contract_execution_private.ordinal = clause.ordinal;
              span = clause.span;
              binder = clause.binder;
              payload = clause.payload;
            })
          summary.ensures);
    contract_context =
      (fun context definition entry_environment old_environment ->
        {
          context with
          current_callable = definition.Sst.function_id;
          entry_environment;
          logical = true;
          old_environment;
        });
    environment = (fun state -> state.environment);
    with_environment =
      (fun state environment -> { state with environment });
    with_assumptions;
    bind_pattern = bind_pattern_direct;
    fresh_value;
    expect_boolean;
    emit_goal;
    malformed =
      (fun function_name span message ->
        { function_name; span; unsupported = Malformed_sst message });
    record =
      (fun reached ->
        context.reached_callback_calls :=
          reached :: !(context.reached_callback_calls));
  }

let owned_tree_transitions =
  Immutable_aggregate_reconstruction_private.owned_tree_transitions

let rec evaluate context expression state =
  let context =
    if
      (not context.logical)
      &&
      match
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      with
      | Some (Sst.Ghost_instance | Sst.Tracked_instance) -> true
      | Some Sst.Exec_instance | None -> false
    then { context with logical = true }
    else context
  in
  let function_ref = context.function_ref in
  let function_name = function_ref.Vir.function_name in
  match expression.Sst.expression_desc with
  | Sst.Forall _ | Sst.Exists _ ->
      error function_name expression.span
        (Malformed_sst
           "user quantifier leaked into the legacy symbolic evaluator")
  | Sst.Int_constant value ->
      Ok
        {
          obligations = [];
          paths =
            [ { value = Integer_value (Vir.Integer_constant value); state } ];
        }
  | Sst.Bool_constant value ->
      Ok
        {
          obligations = [];
          paths =
            [ { value = Boolean_value (Vir.Boolean_constant value); state } ];
        }
  | Sst.Unit_constant ->
      Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ ->
      if context.logical then
        Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
      else
        error function_name expression.span
          (Malformed_sst "reveal reached runtime evaluation")
  | Sst.Variable { binding; _ } -> (
      match List.assoc_opt binding.id state.environment with
      | Some value ->
          Ok { obligations = []; paths = [ { value; state } ] }
      | None ->
          error function_name expression.span
            (Malformed_sst ("unbound variable " ^ binding.name)))
  | Sst.Tuple_value components ->
      let initial = [ (state, []) ] in
      let rec loop obligations contexts = function
        | [] ->
            Ok
              {
                obligations;
                paths =
                  List.map
                    (fun (state, values) ->
                      { value = Tuple_value (List.rev values); state })
                    contexts;
              }
        | (_, component) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context component state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            loop (append obligations evaluated.obligations) evaluated.paths rest
      in
      loop [] initial components
  | Sst.Record_value { record_type; fields } ->
      let rec evaluate_fields obligations contexts = function
        | [] -> Ok { obligations; paths = contexts }
        | (field, field_expression) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context field_expression state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            ( evaluated.state,
                              (field, field_expression.Sst.typ, evaluated.value)
                              :: values ))
                          result.paths;
                    })
                contexts
            in
            evaluate_fields
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      let* fields = evaluate_fields [] [ (state, []) ] fields in
      let make (state, reversed_fields) =
        let aggregate_type = vir_aggregate_type record_type in
        let symbol, state =
          fresh_symbol state ~source_name:("_" ^ record_type.type_name)
            ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
            ~span:expression.span ~project:false
        in
        let aggregate =
          { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
        in
        let* equations =
          let rec loop equations = function
            | [] -> Ok (List.rev equations)
            | (field, typ, value) :: rest ->
                let selected =
                  selected_value_without_state aggregate (field_selector field) [] typ
                in
                (match equality selected value with
                | Some equation -> loop (equation :: equations) rest
                | None ->
                    error function_name expression.span
                      (Malformed_sst "record field type/value mismatch"))
          in
          loop [] (List.rev reversed_fields)
        in
        Ok
          {
            obligations = [];
            paths =
              [
                {
                  value = Aggregate_value aggregate;
                  state = with_assumptions state equations;
                };
              ];
          }
      in
      let* constructed = evaluate_contexts make fields.paths in
      let instance_mode =
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      in
      let* established =
        match
          ( invariant_for_typ context.invariants (Sst.Aggregate record_type),
            instance_mode )
        with
        | ( Some handle,
            Some (Sst.Exec_instance | Sst.Tracked_instance) ) ->
            evaluate_contexts
              (fun evaluated ->
                prove_invariant_validity context handle
                  Vir.Constructor_establishment context.function_ref
                  expression.span evaluated.value evaluated.state)
              constructed.paths
        | None, _ | Some _, (Some Sst.Ghost_instance | None) ->
            Ok { obligations = []; paths = constructed.paths }
      in
      Ok
        {
          obligations =
            append fields.obligations
              (append constructed.obligations established.obligations);
          paths = established.paths;
        }
  | Sst.Constructor_value { constructor; arguments } ->
      let rec evaluate_arguments obligations contexts = function
        | [] -> Ok { obligations; paths = contexts }
        | argument :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context argument state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, (argument.Sst.typ, evaluated.value) :: values))
                          result.paths;
                    })
                contexts
            in
            evaluate_arguments
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      let* arguments = evaluate_arguments [] [ (state, []) ] arguments in
      let make (state, reversed_arguments) =
        let aggregate_type = vir_aggregate_type constructor.constructor_type in
        let symbol, state =
          fresh_symbol state ~source_name:("_" ^ constructor.constructor_name)
            ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
            ~span:expression.span ~project:false
        in
        let aggregate =
          { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
        in
        let arguments = List.rev reversed_arguments in
        let* selector_equations =
          let rec loop index equations = function
            | [] -> Ok (List.rev equations)
            | (typ, value) :: rest ->
                let selected =
                  selected_value_without_state aggregate
                    (argument_selector constructor index) [] typ
                in
                (match equality selected value with
                | Some equation -> loop (index + 1) (equation :: equations) rest
                | None ->
                    error function_name expression.span
                      (Malformed_sst "constructor argument type/value mismatch"))
          in
          loop 0 [] arguments
        in
        let tag =
          Vir.Integer_compare
            ( Vir.Equal,
              Vir.Aggregate_tag (aggregate_type, aggregate),
              Vir.Integer_constant (Z.of_int constructor.constructor_index) )
        in
        Ok
          {
            obligations = [];
            paths =
              [
                {
                  value = Aggregate_value aggregate;
                  state =
                    with_assumptions state (tag :: selector_equations);
                };
              ];
          }
      in
      let* constructed = evaluate_contexts make arguments.paths in
      let instance_mode =
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      in
      let* established =
        match
          ( invariant_for_typ context.invariants
              (Sst.Aggregate constructor.constructor_type),
            instance_mode )
        with
        | ( Some handle,
            Some (Sst.Exec_instance | Sst.Tracked_instance) ) ->
            evaluate_contexts
              (fun evaluated ->
                prove_invariant_validity context handle
                  Vir.Constructor_establishment context.function_ref
                  expression.span evaluated.value evaluated.state)
              constructed.paths
        | None, _ | Some _, (Some Sst.Ghost_instance | None) ->
            Ok { obligations = []; paths = constructed.paths }
      in
      Ok
        {
          obligations =
            append arguments.obligations
              (append constructed.obligations established.obligations);
          paths = established.paths;
        }
  | Sst.Field_read { record; field } ->
      let* record = evaluate context record state in
      let* projected =
        evaluate_contexts
          (fun evaluated ->
            match evaluated.value with
            | Aggregate_value aggregate ->
                let* value, state =
                  select_field evaluated.state aggregate field expression.typ
                in
                Ok { obligations = []; paths = [ { value; state } ] }
            | _ ->
                error function_name expression.span
                  (Malformed_sst "field read receiver is not an aggregate"))
          record.paths
      in
      Ok
        {
          obligations = append record.obligations projected.obligations;
          paths = projected.paths;
        }
  | Sst.Field_write { provenance; field; value; transition } ->
      let erased_update =
        match
          Sst_validation.expression_instance_mode context.validated
            context.current_callable expression
        with
        | Sst.Ghost_instance | Sst.Tracked_instance -> true
        | Sst.Exec_instance -> false
      in
      if context.logical && not erased_update then
        error function_name expression.span
          (Malformed_sst "mutation is not allowed in a logical expression")
      else if
        (not erased_update)
        &&
        provenance.binding_pattern_uniqueness <> Sst.Definitely_unique
        || not provenance.field_is_local
        || not provenance.field_is_public
        || not provenance.field_is_mutable
      then
        error function_name expression.span
          (Malformed_sst "field write lacks definite unique provenance")
      else
        let root = provenance.root in
        let* fields =
          match root.typ with
          | Sst.Aggregate type_id -> (
              match
                List.find_opt
                  (fun (definition : Sst.type_definition) ->
                    same_type_id definition.type_id type_id)
                  context.type_definitions
              with
              | Some { type_kind = Sst.Record_definition fields; _ } ->
                  if
                    List.exists
                      (fun (definition : Sst.field_definition) ->
                        definition.field_id = field)
                      fields
                  then Ok fields
                  else
                    error function_name expression.span
                      (Malformed_sst
                         "field write does not belong to its root record")
              | Some { type_kind = Sst.Variant_definition _; _ } ->
                  error function_name expression.span
                    (Malformed_sst "direct field write root is not a record")
              | None ->
                  error function_name expression.span
                    (Malformed_sst "field-write root type is not registered"))
          | _ ->
              error function_name expression.span
                (Malformed_sst "field-write root has non-aggregate type")
        in
        let* evaluated_value = evaluate context value state in
        let advance evaluated =
          let* old_aggregate =
            match List.assoc_opt root.id evaluated.state.environment with
            | Some (Aggregate_value aggregate) -> Ok aggregate
            | Some _ | None ->
                error function_name expression.span
                  (Malformed_sst
                     "field-write root changed incompatibly while evaluating \
                      its value")
          in
          let aggregate_type = old_aggregate.Vir.aggregate_type in
          let symbol, state =
            fresh_symbol evaluated.state
              ~source_name:(root.name ^ ".state")
              ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
              ~span:expression.span ~project:true
          in
          let fresh_aggregate =
            {
              Vir.aggregate_type;
              aggregate_desc = Vir.Aggregate_symbol symbol;
            }
          in
          let rec copy state equations = function
            | [] ->
                let state =
                  with_assumptions state (List.rev equations)
                in
                let successor = Aggregate_value fresh_aggregate in
                let* preserved =
                  match transition with
                  | None ->
                      Ok
                        {
                          obligations = [];
                          paths = [ { value = successor; state } ];
                        }
                  | Some transition -> (
                      match
                        invariant_for_typ context.invariants transition.root.typ
                      with
                      | None ->
                          Ok
                            {
                              obligations = [];
                              paths = [ { value = successor; state } ];
                            }
                      | Some handle ->
                          prove_invariant_validity context handle
                            (Vir.Transition_preservation
                               {
                                 transition_kind = Vir.Direct_root_transition;
                                 root_binding_id = transition.root.id;
                                 pre_version = transition.pre_version;
                                 successor_version =
                                   transition.successor_version;
                               })
                            context.function_ref expression.span successor state)
                in
                Ok
                  {
                    obligations = preserved.obligations;
                    paths =
                      List.map
                        (fun preserved ->
                          {
                            value = Unit_value;
                            state =
                              {
                                preserved.state with
                                environment =
                                  replace_binding root preserved.value
                                    preserved.state.environment;
                              };
                          })
                        preserved.paths;
                  }
            | definition :: rest ->
                let* selected_new, state =
                  select_field state fresh_aggregate definition.Sst.field_id
                    definition.field_type
                in
                let* selected_old, state =
                  select_field state old_aggregate definition.field_id
                    definition.field_type
                in
                let rhs =
                  if definition.field_id = field then evaluated.value
                  else selected_old
                in
                let* equation =
                  match equality selected_new rhs with
                  | Some equation -> Ok equation
                  | None ->
                      error function_name expression.span
                        (Malformed_sst
                           "field-write value has the wrong field type")
                in
                copy state (equation :: equations) rest
          in
          copy state [] fields
        in
        let* advanced = evaluate_contexts advance evaluated_value.paths in
        Ok
          {
            obligations =
              append evaluated_value.obligations advanced.obligations;
            paths = advanced.paths;
          }
  | Sst.Shared_scalar_field_write _ ->
      error function_name expression.span
        (Malformed_sst
           "bounded shared-scalar writes require the authenticated private \
            executor")
  | Sst.Owned_tree_nested_write { transition; value } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "owned-tree mutation is not logical")
      else
        let root = transition.root in
        let cursor =
          match transition.cursor with
          | Some cursor -> cursor
          | None -> assert false
        in
        let* evaluated_value = evaluate context value state in
        let advance evaluated =
          let* root_aggregate =
            match List.assoc_opt root.id evaluated.state.environment with
            | Some (Aggregate_value aggregate) -> Ok aggregate
            | Some _ | None ->
                error function_name expression.span
                  (Malformed_sst "owned-tree root is not a live aggregate")
          in
          let* cursor_value, breadcrumbs, state =
            descend_owned_path function_name context expression.span
              evaluated.state root_aggregate cursor.guarded_path
          in
          let* cursor_aggregate =
            match cursor_value with
            | Aggregate_value aggregate -> Ok aggregate
            | _ ->
                error function_name expression.span
                  (Malformed_sst "owned-tree mutation cursor is not aggregate")
          in
          let* changed, state =
            rebuild_owned_fields function_name context expression.span root state
              cursor_aggregate transition.target_field.field_owner
              transition.target_field evaluated.value
          in
          let* successor, state =
            rebuild_owned_ancestors function_name context expression.span root
              state changed breadcrumbs
          in
          let* successor =
            match successor with
            | Aggregate_value aggregate -> Ok (Aggregate_value aggregate)
            | _ ->
                error function_name expression.span
                  (Malformed_sst "owned-tree successor root is not aggregate")
          in
          let* preserved =
            match
              invariant_for_typ context.invariants transition.root.typ
            with
            | None ->
                Ok
                  {
                    obligations = [];
                    paths = [ { value = successor; state } ];
                  }
            | Some handle ->
                prove_invariant_validity context handle
                  (Vir.Transition_preservation
                     {
                       transition_kind = Vir.Nested_transition;
                       root_binding_id = transition.root.id;
                       pre_version = transition.pre_version;
                       successor_version = transition.successor_version;
                     })
                  context.function_ref expression.span successor state
          in
          Ok
            {
              obligations = preserved.obligations;
              paths =
                List.map
                  (fun preserved ->
                    {
                      value = Unit_value;
                      state =
                        {
                          preserved.state with
                          environment =
                            replace_binding root preserved.value
                              preserved.state.environment;
                        };
                    })
                  preserved.paths;
            }
        in
        let* advanced = evaluate_contexts advance evaluated_value.paths in
        Ok
          {
            obligations =
              append evaluated_value.obligations advanced.obligations;
            paths = advanced.paths;
          }
  | Sst.Owned_tree_rebase { transition } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "owned-tree move is not logical")
      else
        let root = transition.root in
        let descendant =
          match transition.rhs_provenance with
          | Sst.Guarded_descendant_move cursor -> cursor
          | Sst.Ground_owned_tree_value -> assert false
        in
        let* root_aggregate =
          match List.assoc_opt root.id state.environment with
          | Some (Aggregate_value aggregate) -> Ok aggregate
          | Some _ | None ->
              error function_name expression.span
                (Malformed_sst "owned-tree move root is not a live aggregate")
        in
        let* descendant_value, _, state =
          descend_owned_path function_name context expression.span state
            root_aggregate descendant.guarded_path
        in
        let* successor, state =
          rebuild_owned_fields function_name context expression.span root state
            root_aggregate transition.target_field.field_owner
            transition.target_field descendant_value
        in
        let* successor =
          match successor with
          | Aggregate_value aggregate -> Ok (Aggregate_value aggregate)
          | _ ->
              error function_name expression.span
                (Malformed_sst "owned-tree move successor is not aggregate")
        in
        let* preserved =
          match
            invariant_for_typ context.invariants transition.root.typ
          with
          | None ->
              Ok
                {
                  obligations = [];
                  paths = [ { value = successor; state } ];
                }
          | Some handle ->
              prove_invariant_validity context handle
                (Vir.Transition_preservation
                   {
                     transition_kind = Vir.Rebase_transition;
                     root_binding_id = transition.root.id;
                     pre_version = transition.pre_version;
                     successor_version = transition.successor_version;
                   })
                context.function_ref expression.span successor state
        in
        Ok
          {
            obligations = preserved.obligations;
            paths =
              List.map
                (fun preserved ->
                  {
                    value = Unit_value;
                    state =
                      {
                        preserved.state with
                        environment =
                          replace_binding root preserved.value
                            preserved.state.environment;
                      };
                  })
                preserved.paths;
          }
  | Sst.Let_mutable (binding, initial, body) ->
      if binding.uniqueness <> Sst.Definitely_unique then
        error function_name expression.span
          (Malformed_sst "mutable local lacks definite unique provenance")
      else
        let* initial = evaluate context initial state in
        let* bodies =
          evaluate_contexts
            (fun evaluated ->
              let* state =
                match bind_scalar evaluated.state binding evaluated.value with
                | Ok state -> Ok state
                | Error error -> Error { error with function_name }
              in
              evaluate context body state)
            initial.paths
        in
        Ok
          {
            obligations = append initial.obligations bodies.obligations;
            paths = bodies.paths;
          }
  | Sst.Mutable_read binding -> (
      match List.assoc_opt binding.id state.environment with
      | Some value ->
          Ok { obligations = []; paths = [ { value; state } ] }
      | None ->
          error function_name expression.span
            (Malformed_sst ("unbound mutable local " ^ binding.name)))
  | Sst.Mutable_write { provenance; value } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "mutation is not allowed in a logical expression")
      else if
        provenance.binding_pattern_uniqueness <> Sst.Definitely_unique
        || not provenance.field_is_local
        || not provenance.field_is_mutable
      then
        error function_name expression.span
          (Malformed_sst "mutable local lacks definite provenance")
      else
        let root = provenance.root in
        let* evaluated = evaluate context value state in
        let assign evaluated =
          let* state =
            match bind_scalar evaluated.state root evaluated.value with
            | Ok state -> Ok state
            | Error error -> Error { error with function_name }
          in
          Ok
            {
              obligations = [];
              paths = [ { value = Unit_value; state } ];
            }
        in
        let* assigned = evaluate_contexts assign evaluated.paths in
        Ok
          {
            obligations = append evaluated.obligations assigned.obligations;
            paths = assigned.paths;
          }
  | Sst.Let (bindings, body) ->
      let rec evaluate_bindings obligations contexts = function
        | [] ->
            let* body_results =
              evaluate_contexts
                (fun (state, values) ->
                  let* state =
                    List.fold_left2
                      (fun state (pattern, _) value ->
                        let* state = state in
                        if context.logical then
                          let* environment, ranges =
                            bind_pattern_direct function_name state.environment
                              pattern value
                          in
                          Ok
                            (with_assumptions { state with environment } ranges)
                        else
                          bind_pattern function_name state pattern value)
                      (Ok state) bindings (List.rev values)
                  in
                  evaluate context body state)
                contexts
            in
            Ok
              {
                obligations = append obligations body_results.obligations;
                paths = body_results.paths;
              }
        | (_, value_expression) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context value_expression state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            evaluate_bindings
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      evaluate_bindings [] [ (state, []) ] bindings
  | Sst.Sequence (first, second) ->
      let* first = evaluate context first state in
      let* second =
        evaluate_contexts
          (fun evaluated -> evaluate context second evaluated.state)
          first.paths
      in
      Ok
        {
          obligations = append first.obligations second.obligations;
          paths = second.paths;
        }
  | Sst.If (condition, consequent, alternative) ->
      let* condition = evaluate context condition state in
      let branch evaluated =
        let* condition_term =
          expect_boolean function_name expression.span evaluated.value
        in
        match condition_term with
        | Vir.Boolean_constant true ->
            evaluate context consequent evaluated.state
        | Vir.Boolean_constant false -> (
            match alternative with
            | Some alternative ->
                evaluate context alternative evaluated.state
            | None ->
                Ok
                  {
                    obligations = [];
                    paths = [ { value = Unit_value; state = evaluated.state } ];
                  })
        | condition_term ->
            let* consequent =
              evaluate context consequent
                (with_path evaluated.state condition_term)
            in
            let* alternative =
              match alternative with
              | Some alternative ->
                  evaluate context alternative
                    (with_path evaluated.state (Vir.Boolean_not condition_term))
              | None ->
                  Ok
                    {
                      obligations = [];
                      paths =
                        [
                          {
                            value = Unit_value;
                            state =
                              with_path evaluated.state
                                (Vir.Boolean_not condition_term);
                          };
                        ];
                    }
            in
            Ok
              {
                obligations =
                  append consequent.obligations alternative.obligations;
                paths = append consequent.paths alternative.paths;
              }
      in
      let* branches = evaluate_contexts branch condition.paths in
      Ok
        {
          obligations = append condition.obligations branches.obligations;
          paths = branches.paths;
        }
  | Sst.Match (scrutinee, cases) ->
      let* scrutinee = evaluate context scrutinee state in
      let choose condition state =
        match condition with
        | Vir.Boolean_constant true -> (Some state, None)
        | Vir.Boolean_constant false -> (None, Some state)
        | condition ->
            ( Some (with_path state condition),
              Some (with_path state (Vir.Boolean_not condition)) )
      in
      let rec execute_cases obligations completed remaining = function
        | [] ->
            (* The authenticated frontend admits only total matches.  A
               syntactically remaining symbolic path after the final case is
               therefore infeasible (for example [not (b = true)] and
               [not (b = false)]) and has no executable exit. *)
            Ok { obligations; paths = completed }
        | case :: rest ->
            let execute_one (state, scrutinee_value) =
              let outer_environment = state.environment in
              let* condition, bindings, ranges =
                pattern_condition_and_bindings function_name
                  context.type_definitions case.Sst.case_pattern scrutinee_value
              in
              let state = with_assumptions state ranges in
              let matched, unmatched = choose condition state in
              let unmatched = Option.to_list unmatched in
              match matched with
              | None ->
                  Ok
                    {
                      obligations = [];
                      paths =
                        [
                          ( [],
                            List.map
                              (fun state -> (state, scrutinee_value))
                              unmatched );
                        ];
                    }
              | Some matched ->
                  let matched = add_pattern_bindings matched bindings in
                  let finish_body body_state =
                    let* body = evaluate context case.case_body body_state in
                    Ok
                      {
                        obligations = body.obligations;
                        paths =
                          [
                            ( List.map
                                (fun evaluated ->
                                  {
                                    evaluated with
                                    state =
                                      restore_environment outer_environment
                                        evaluated.state;
                                  })
                                body.paths,
                              List.map
                                (fun state -> (state, scrutinee_value))
                                unmatched );
                          ];
                      }
                  in
                  (match case.case_guard with
                  | None -> finish_body matched
                  | Some guard ->
                      let* guard = evaluate context guard matched in
                      let* guarded =
                        evaluate_contexts
                          (fun evaluated ->
                            let* guard_term =
                              expect_boolean function_name case.case_span
                                evaluated.value
                            in
                            let selected, rejected =
                              choose guard_term evaluated.state
                            in
                            let* selected =
                              match selected with
                              | None ->
                                  Ok { obligations = []; paths = [] }
                              | Some selected ->
                                  let* body =
                                    evaluate context case.case_body selected
                                  in
                                  Ok
                                    {
                                      body with
                                      paths =
                                        List.map
                                          (fun evaluated ->
                                            {
                                              evaluated with
                                              state =
                                                restore_environment
                                                  outer_environment
                                                  evaluated.state;
                                            })
                                          body.paths;
                                    }
                            in
                            let rejected =
                              Option.to_list rejected
                              |> List.map
                                   (fun state ->
                                     ( restore_environment outer_environment
                                         state,
                                       scrutinee_value ))
                            in
                            Ok
                              {
                                obligations = selected.obligations;
                                paths = [ (selected.paths, rejected) ];
                              })
                          guard.paths
                      in
                      let bodies, rejected =
                        List.fold_left
                          (fun (bodies, rejected) (more_bodies, more_rejected) ->
                            ( append bodies more_bodies,
                              append rejected more_rejected ))
                          ([], []) guarded.paths
                      in
                      Ok
                        {
                          obligations =
                            append guard.obligations guarded.obligations;
                          paths =
                            [
                              ( bodies,
                                append
                                  (List.map
                                     (fun state -> (state, scrutinee_value))
                                     unmatched)
                                  rejected );
                            ];
                        })
            in
            let* evaluated = evaluate_contexts execute_one remaining in
            let bodies, next_remaining =
              List.fold_left
                (fun (bodies, remaining) (more_bodies, more_remaining) ->
                  (append bodies more_bodies, append remaining more_remaining))
                ([], []) evaluated.paths
            in
            execute_cases
              (append obligations evaluated.obligations)
              (append completed bodies) next_remaining rest
      in
      let* matched =
        evaluate_contexts
          (fun evaluated ->
            execute_cases [] [] [ (evaluated.state, evaluated.value) ] cases)
          scrutinee.paths
      in
      Ok
        {
          obligations = append scrutinee.obligations matched.obligations;
          paths = matched.paths;
        }
  | Sst.Checked_arithmetic (operation, arguments) ->
      let rec evaluate_arguments obligations contexts = function
        | [] ->
            let results =
              List.map
                (fun (state, values) ->
                  let* mathematical_result =
                    arithmetic_term function_name expression.span operation
                      (List.rev values)
                  in
                  if context.logical then
                    Ok
                      {
                        obligations = [];
                        paths =
                          [
                            {
                              value = Integer_value mathematical_result;
                              state;
                            };
                          ];
                      }
                  else
                    Ok
                      (emit_checked function_ref expression.span operation
                         mathematical_result state))
                contexts
            in
            let rec collect obligations paths = function
              | [] -> Ok { obligations; paths }
              | result :: rest ->
                  let* result = result in
                  collect
                    (append obligations result.obligations)
                    (append paths result.paths) rest
            in
            collect obligations [] results
        | argument :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context argument state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            evaluate_arguments
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      evaluate_arguments [] [ (state, []) ] arguments
  | Sst.Compare (comparison, left, right) ->
      let* left = evaluate context left state in
      let* evaluated =
        evaluate_contexts
          (fun left ->
            let* right = evaluate context right left.state in
            let rec make_paths paths = function
              | [] -> Ok (List.rev paths)
              | right :: rest ->
                  let* term =
                    match (left.value, right.value) with
                    | Integer_value left, Integer_value right ->
                        Ok
                          (Vir.Integer_compare
                             (vir_comparison comparison, left, right))
                    | Boolean_value left, Boolean_value right -> (
                        match comparison with
                        | Sst.Equal -> Ok (Vir.Boolean_equal (left, right))
                        | Sst.Not_equal ->
                            Ok (Vir.Boolean_not_equal (left, right))
                        | _ ->
                            error function_name expression.span
                              (Malformed_sst
                                 "ordered comparison applied to booleans"))
                    | _ ->
                        error function_name expression.span
                          (Malformed_sst "comparison operand type mismatch")
                  in
                  make_paths
                    ({ value = Boolean_value term; state = right.state } :: paths)
                    rest
            in
            let* paths = make_paths [] right.paths in
            Ok { obligations = right.obligations; paths })
          left.paths
      in
      Ok
        {
          obligations = append left.obligations evaluated.obligations;
          paths = evaluated.paths;
        }
  | Sst.Boolean_not operand ->
      let* operand = evaluate context operand state in
      let* paths =
        let rec loop paths = function
          | [] -> Ok (List.rev paths)
          | evaluated :: rest ->
              let* term =
                expect_boolean function_name expression.span evaluated.value
              in
              loop
                ({ value = Boolean_value (Vir.Boolean_not term); state = evaluated.state }
                :: paths)
                rest
        in
        loop [] operand.paths
      in
      Ok { obligations = operand.obligations; paths }
  | Sst.Boolean_binary (operation, left, right) ->
      let* left = evaluate context left state in
      let* right =
        evaluate_contexts
          (fun left ->
            let* left_term =
              expect_boolean function_name expression.span left.value
            in
            let short_circuit value =
              Ok
                {
                  obligations = [];
                  paths =
                    [
                      {
                        value = Boolean_value (Vir.Boolean_constant value);
                        state = left.state;
                      };
                    ];
                }
            in
            let evaluate_right state =
              let* right = evaluate context right state in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: rest ->
                      let* right_term =
                        expect_boolean function_name expression.span
                          evaluated.value
                      in
                      loop
                        ({ value = Boolean_value right_term; state = evaluated.state }
                        :: paths)
                        rest
                in
                loop [] right.paths
              in
              Ok { obligations = right.obligations; paths }
            in
            match (operation, left_term) with
            | Sst.And, Vir.Boolean_constant false -> short_circuit false
            | Sst.And, Vir.Boolean_constant true -> evaluate_right left.state
            | Sst.Or, Vir.Boolean_constant true -> short_circuit true
            | Sst.Or, Vir.Boolean_constant false -> evaluate_right left.state
            | Sst.And, left_term ->
                let short =
                  {
                    value = Boolean_value (Vir.Boolean_constant false);
                    state = with_path left.state (Vir.Boolean_not left_term);
                  }
                in
                let* evaluated =
                  evaluate_right (with_path left.state left_term)
                in
                Ok { evaluated with paths = short :: evaluated.paths }
            | Sst.Or, left_term ->
                let short =
                  {
                    value = Boolean_value (Vir.Boolean_constant true);
                    state = with_path left.state left_term;
                  }
                in
                let* evaluated =
                  evaluate_right
                    (with_path left.state (Vir.Boolean_not left_term))
                in
                Ok { evaluated with paths = short :: evaluated.paths })
          left.paths
      in
      Ok
        {
          obligations = append left.obligations right.obligations;
          paths = right.paths;
        }
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        arguments;
        recursive = false;
        type_arguments;
      } ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst "specification call reached runtime evaluation")
      else
        let* definition =
          match List.assoc_opt callee.function_index context.definitions with
          | Some descriptor
            when
              String.equal
                (Sst_validation.callable_id descriptor).function_name
                callee.function_name ->
              Ok (Sst_validation.callable_definition descriptor)
          | _ -> error function_name expression.span (Missing_summary callee)
        in
        let* definition_body =
          match (definition.mode, definition.body) with
          | Sst.Spec, Sst.Spec_definition body ->
              (match
                 Spec_definition.authenticated_invariant_domain
                   context.type_definitions definition
               with
              | Some domain -> (
                  match
                    Type_invariant.find_for_type context.invariants domain
                  with
                  | Some handle -> Ok (`Opaque_invariant handle)
                  | None ->
                      error function_name expression.span
                        (Malformed_sst
                           "invariant predicate has no authenticated handle"))
              | None ->
              if List.mem callee.function_index context.spec_call_stack then
                error function_name expression.span
                  (Malformed_sst
                     "cyclic specification expansion reached evaluation")
              else if
                List.length context.spec_call_stack
                >= context.spec_expansion_limit
              then
                error function_name expression.span
                  (Malformed_sst
                     "specification expansion depth exceeded validated graph")
              else Ok (`Expand body.expression))
          | Sst.Spec, Sst.Recursive_spec_definition _ -> Ok `Opaque_recursive
          | _ ->
              error function_name expression.span
                (Malformed_sst
                   "specification call target has no validated spec definition")
        in
        let rec evaluate_arguments obligations contexts = function
          | [] -> Ok { obligations; paths = contexts }
          | argument :: rest ->
              let _, argument = Sst.require_value_argument argument in
              let* evaluated =
                evaluate_contexts
                  (fun (state, values) ->
                    let* argument = evaluate context argument state in
                    Ok
                      {
                        obligations = argument.obligations;
                        paths =
                          List.map
                            (fun evaluated ->
                              (evaluated.state, evaluated.value :: values))
                            argument.paths;
                      })
                  contexts
              in
              evaluate_arguments
                (append obligations evaluated.obligations)
                evaluated.paths rest
        in
        let* actuals = evaluate_arguments [] [ (state, []) ] arguments in
        let expand (caller_state, reversed_actuals) =
          let caller_environment = caller_state.environment in
          let actuals = List.rev reversed_actuals in
          let parameters =
            List.map Sst.require_value_parameter definition.parameters
          in
          if List.length actuals <> List.length parameters then
            error function_name expression.span
              (Malformed_sst "specification-call argument count mismatch")
          else
            match definition_body with
            | `Opaque_invariant handle -> (
                match actuals with
                | [ Aggregate_value value ] ->
                    Ok
                      {
                        obligations = [];
                        paths =
                          [
                            {
                              value =
                                Boolean_value
                                  (Vir.Boolean_invariant_application
                                     {
                                       invariant_id =
                                         Type_invariant.invariant_id handle;
                                       model =
                                         Type_invariant.model_callable handle;
                                       predicate =
                                         Type_invariant.predicate_callable
                                           handle;
                                       value;
                                     });
                              state = caller_state;
                            };
                          ];
                      }
                | _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "invariant predicate received a non-exact value"))
            | `Opaque_recursive ->
                let* arguments =
                  List.fold_left
                    (fun result value ->
                      let* arguments = result in
                      match value with
                      | Integer_value term ->
                          Ok (Vir.Recursive_integer_argument term :: arguments)
                      | Boolean_value term ->
                          Ok (Vir.Recursive_boolean_argument term :: arguments)
                      | Unit_value | Tuple_value _ | Aggregate_value _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "recursive specification argument is not scalar"))
                    (Ok []) actuals
                in
                let arguments = List.rev arguments in
                let* value =
                  match definition.result_type with
                  | Sst.Int ->
                      Ok
                        (Integer_value
                           (Vir.Integer_recursive_spec_application
                              {
                                callee;
                                type_arguments;
                                arguments;
                                span = expression.span;
                              }))
                  | Sst.Bool ->
                      Ok
                        (Boolean_value
                           (Vir.Boolean_recursive_spec_application
                              {
                                callee;
                                type_arguments;
                                arguments;
                                span = expression.span;
                              }))
                  | Sst.Aggregate _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "aggregate recursive specifications require private finite-preservation authority")
                  | Sst.Unit | Sst.Tuple _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "recursive specification result is outside its exact ABI")
                in
                Ok
                  {
                    obligations = [];
                    paths = [ { value; state = caller_state } ];
                  }
            | `Expand definition_body ->
            let* definition_environment, ranges =
              List.fold_left2
                (fun environment parameter actual ->
                  let* environment, ranges = environment in
                  let* environment, nested_ranges =
                    bind_pattern_direct function_name environment
                      parameter.Sst.pattern actual
                  in
                  Ok (environment, ranges @ nested_ranges))
                (Ok ([], [])) parameters actuals
            in
            let spec_context =
              {
                context with
                entry_environment = definition_environment;
                logical = true;
                old_environment = None;
                spec_call_stack =
                  callee.function_index :: context.spec_call_stack;
              }
            in
            let* expanded =
              evaluate spec_context definition_body
                (with_assumptions
                   { caller_state with environment = definition_environment }
                   ranges)
            in
            Ok
              {
                expanded with
                paths =
                  List.map
                    (fun evaluated ->
                      {
                        evaluated with
                        state =
                          restore_environment caller_environment evaluated.state;
                      })
                    expanded.paths;
              }
        in
        let* expanded = evaluate_contexts expand actuals.paths in
        Ok
          {
            obligations = append actuals.obligations expanded.obligations;
            paths = expanded.paths;
          }
  | Sst.Direct_call
      {
        call_form = ((Sst.Exec_call | Sst.Proof_call) as call_form);
        callee;
        arguments;
        recursive;
      } ->
      let* summary =
        match find_summary context.summaries callee with
        | None -> error function_name expression.span (Missing_summary callee)
        | Some summary -> Ok summary
      in
      let* () =
        match (call_form, summary.definition.mode, context.logical) with
        | Sst.Exec_call, Sst.Exec, false
        | Sst.Proof_call, Sst.Proof, true ->
            Ok ()
        | Sst.Exec_call, Sst.Exec, true -> (
            match Sst_validation.find_callable context.validated callee with
            | Some descriptor -> (
                match
                  Sst_validation.result_instance_mode context.validated
                    descriptor
                with
                | Sst.Ghost_instance | Sst.Tracked_instance -> Ok ()
                | Sst.Exec_instance ->
                    error function_name expression.span
                      (Malformed_sst
                         "runtime-result executable call reached logical \
                          evaluation"))
            | None -> error function_name expression.span (Missing_summary callee))
        | _ ->
            error function_name expression.span
              (Malformed_sst "call form reached the wrong evaluation stage")
      in
      let* consumed_root =
        match summary.definition.returns_unique_parameter with
        | None -> Ok None
        | Some parameter_index -> (
            match
              ( List.nth_opt summary.definition.parameters parameter_index,
                List.nth_opt arguments parameter_index )
            with
            | ( Some
                  (Sst.Value_parameter
                    {
                      Sst.pattern =
                        {
                          pattern_desc = Sst.Bind parameter_binding;
                          _;
                        };
                      _;
                    }),
                Some
                  (Sst.Value_argument
                    {
                      value =
                        {
                          Sst.expression_desc =
                            Sst.Variable
                              {
                                binding = argument_binding;
                                use_uniqueness = Sst.Definitely_unique;
                              };
                          _;
                        };
                      _;
                    }) )
              when
                parameter_binding.uniqueness = Sst.Definitely_unique
                && argument_binding.uniqueness = Sst.Definitely_unique ->
                Ok (Some argument_binding)
            | Some _, Some (Sst.Value_argument { value = argument; _ }) -> (
                match argument.Sst.expression_desc with
                | Sst.Record_value _ | Sst.Constructor_value _ -> Ok None
                | _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "unique returned state is not supplied by a definite \
                          unique root"))
            | _ ->
                error function_name expression.span
                  (Malformed_sst
                     "unique return provenance names an invalid parameter"))
      in
      if recursive && context.logical && call_form <> Sst.Proof_call then
        error function_name expression.span (Recursion_awaits_totality callee)
      else
        let callback_environment =
          List.fold_left2
            (fun environment formal argument ->
              match formal, argument with
              | ( Sst.Callback_parameter formal,
                  Sst.Callback_argument { callback; _ } ) ->
                  (formal.binding, callback) :: environment
              | Sst.Value_parameter _, Sst.Value_argument _ ->
                  environment
              | Sst.Callback_parameter _, Sst.Value_argument _
              | Sst.Value_parameter _, Sst.Callback_argument _ ->
                  environment)
            context.callback_environment summary.definition.parameters
            arguments
        in
        let call_arguments = arguments in
        let rec evaluate_arguments obligations contexts = function
          | [] -> Ok { obligations; paths = contexts }
          | Sst.Callback_argument _ :: rest ->
              evaluate_arguments obligations contexts rest
          | Sst.Value_argument { value = argument; _ } :: rest ->
              let* evaluated =
                evaluate_contexts
                  (fun (state, values) ->
                    let* argument = evaluate context argument state in
                    Ok
                      {
                        obligations = argument.obligations;
                        paths =
                          List.map
                            (fun evaluated ->
                              (evaluated.state, evaluated.value :: values))
                            argument.paths;
                      })
                  contexts
              in
              evaluate_arguments
                (append obligations evaluated.obligations)
                evaluated.paths rest
        in
        let* arguments =
          evaluate_arguments [] [ (state, []) ] arguments
        in
        let instantiate (caller_state, reversed_actuals) =
          let actuals = List.rev reversed_actuals in
          let parameters =
            List.filter_map
              (function
                | Sst.Value_parameter parameter -> Some parameter
                | Sst.Callback_parameter _ -> None)
              summary.definition.parameters
          in
          if List.length actuals <> List.length parameters
          then
            error function_name expression.span
              (Malformed_sst "direct-call argument count mismatch")
          else
            let* actual_environment, actual_ranges =
              List.fold_left2
                (fun environment parameter actual ->
                  let* environment, ranges = environment in
                  let* environment, nested_ranges =
                    bind_pattern_direct function_name environment
                      parameter.Sst.pattern actual
                  in
                  Ok (environment, ranges @ nested_ranges))
                (Ok ([], [])) parameters actuals
            in
            let* actual_environment =
              List.fold_left
                (fun environment -> function
                  | Sst.Value_argument _ -> environment
                  | Sst.Callback_argument { callback; _ } ->
                      List.fold_left
                        (fun environment
                             (capture :
                               Callback_certificate_private.capture) ->
                          let* environment = environment in
                          match
                            List.assoc_opt capture.binding_id
                              caller_state.environment
                          with
                          | Some value ->
                              Ok
                                ((capture.binding_id, value)
                                :: environment)
                          | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "callback capture is absent at its direct \
                                    call edge"))
                        environment
                        (Callback_certificate_private.captures
                           callback.callback_certificate))
                (Ok actual_environment) call_arguments
            in
            let caller_state = with_assumptions caller_state actual_ranges in
            (* A call does not mint invariant authority for either side.
               Exact locally-established facts remain in the caller state,
               while entry, trusted, imported, and ordinary call boundaries
               contribute no invariant assumption or receipt. *)
            let argument_obligations = [] in
            let spec_context =
              {
                validated = context.validated;
                function_ref;
                current_callable = context.current_callable;
                definitions = context.definitions;
                summaries = context.summaries;
                termination = context.termination;
                type_definitions = context.type_definitions;
                entry_environment = actual_environment;
                logical = true;
                old_environment = None;
                spec_call_stack = context.spec_call_stack;
                spec_expansion_limit = context.spec_expansion_limit;
                rank_domains = context.rank_domains;
                invariants = context.invariants;
                reached_callback_calls = context.reached_callback_calls;
                callback_environment;
              }
            in
            let rec prove_requires obligations states = function
              | [] -> Ok { obligations; paths = states }
              | clause :: rest ->
                  let* evaluated =
                    evaluate_contexts
                      (fun state ->
                        let caller_environment = state.environment in
                        let spec_state =
                          { state with environment = actual_environment }
                        in
                        let* predicate =
                          evaluate spec_context clause.payload spec_state
                        in
                        let* paths =
                          let rec loop paths = function
                            | [] -> Ok (List.rev paths)
                            | evaluated :: remaining ->
                                let* goal =
                                  expect_boolean function_name clause.span
                                    evaluated.value
                                in
                                let kind =
                                  Vir.Call_precondition
                                    {
                                      callee =
                                        {
                                          Vir.function_index =
                                            summary.definition.function_id
                                              .function_index;
                                          function_name =
                                            summary.definition.function_id
                                              .function_name;
                                        };
                                      precondition_ordinal = clause.ordinal;
                                      declaration_span = clause.span;
                                      call_span = expression.span;
                                    }
                                in
                                let obligation, state =
                                  emit_goal function_ref kind expression.span goal
                                    evaluated.state
                                in
                                loop
                                  ( ( obligation,
                                      restore_environment caller_environment
                                        state )
                                  :: paths )
                                  remaining
                          in
                          loop [] predicate.paths
                        in
                        Ok
                          {
                            obligations = predicate.obligations;
                            paths;
                          })
                      states
                  in
                  let emitted, states =
                    List.split evaluated.paths
                  in
                  prove_requires
                    (append obligations
                       (append evaluated.obligations emitted))
                    states rest
            in
            let* proved =
              prove_requires [] [ caller_state ] summary.requires
            in
            let proved =
              {
                proved with
                obligations =
                  append argument_obligations proved.obligations;
              }
            in
            let* proved =
              if not recursive then Ok proved
              else
                match
                  Termination.find_edge_intent context.termination
                    ~caller:context.current_callable ~callee
                    ~span:expression.span
                with
                | Some edge_intent ->
                    let measure =
                      Sst_validation.decrease_clause
                        (Termination.edge_measure edge_intent)
                      |> fun clause ->
                      {
                        ordinal =
                          Sst_validation.contract_clause_index clause;
                        span = Sst_validation.contract_clause_span clause;
                        binder = Sst_validation.contract_clause_binder clause;
                        payload =
                          Sst_validation.contract_clause_expression clause;
                      }
                    in
                    let domain = Termination.edge_domain edge_intent in
                    let measure_context =
                      {
                        spec_context with
                        logical = false;
                        old_environment = None;
                      }
                    in
                    let* evaluated =
                      evaluate_contexts
                        (fun state ->
                          let caller_environment = state.environment in
                          let* measured =
                            evaluate measure_context measure.payload
                              { state with environment = actual_environment }
                          in
                          let* paths =
                            let rec loop paths = function
                              | [] -> Ok (List.rev paths)
                              | measured :: remaining ->
                                  let* call_measure =
                                    ranked_measure function_name measure.span
                                      context.rank_domains domain measured.value
                                  in
                                  let* () =
                                    match (domain, call_measure) with
                                    | ( Termination.Structural_rank _,
                                        Vir.Integer_rank_project
                                          ( rank_domain,
                                            {
                                              Vir.aggregate_desc =
                                                Vir.Aggregate_selector
                                                  (selector, _);
                                              _;
                                            } ) )
                                      when
                                        Vir.rank_selector_is_positive_child
                                          rank_domain selector ->
                                        Ok ()
                                    | Termination.Structural_rank _, _ ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "structural recursion must select \
                                              an authenticated immediate child")
                                    | Termination.Frozen_spine_direct_edge _,
                                      Vir.Integer_constant value
                                      when Z.equal value Z.zero ->
                                        Ok ()
                                    | Termination.Frozen_spine_direct_edge _, _ ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "frozen-spine recursion must \
                                              select its exact direct child")
                                    | Termination.Integer_height, _ -> Ok ()
                                  in
                                  let* entry_measure =
                                    match measured.state.entry_measure with
                                    | Some (entry_domain, entry_measure)
                                      when entry_domain = domain ->
                                        Ok entry_measure
                                    | Some _ ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "recursive call measure domain \
                                              differs from its entry domain")
                                    | None ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "recursive call reached before \
                                              entry measure evaluation")
                                  in
                                  let measured_state =
                                    restore_environment caller_environment
                                      measured.state
                                  in
                                  let callee_ref =
                                    {
                                      Vir.function_index =
                                        summary.definition.function_id
                                          .function_index;
                                      function_name =
                                        summary.definition.function_id
                                          .function_name;
                                    }
                                  in
                                  let nonnegative =
                                    Vir.Integer_compare
                                      ( Vir.Less_or_equal,
                                        Vir.Integer_constant Z.zero,
                                        call_measure )
                                  in
                                  let nonnegative_obligation, measured_state =
                                    emit_goal function_ref
                                      (Vir.Recursive_call_measure_nonnegative
                                         {
                                           callee = callee_ref;
                                           declaration_span = measure.span;
                                           call_span = expression.span;
                                         })
                                      expression.span nonnegative measured_state
                                  in
                                  let descent =
                                    Vir.Integer_compare
                                      (Vir.Less_than, call_measure, entry_measure)
                                  in
                                  let descent_obligation, measured_state =
                                    emit_goal function_ref
                                      (Vir.Recursive_call_strict_descent
                                         {
                                           callee = callee_ref;
                                           declaration_span = measure.span;
                                           call_span = expression.span;
                                         })
                                      expression.span descent measured_state
                                  in
                                  loop
                                    ( ( [ nonnegative_obligation;
                                          descent_obligation ],
                                        measured_state )
                                    :: paths )
                                    remaining
                            in
                            loop [] measured.paths
                          in
                          let obligations, states = List.split paths in
                          Ok
                            {
                              obligations =
                                append measured.obligations
                                  (List.concat obligations);
                              paths = states;
                            })
                        proved.paths
                    in
                    Ok
                      {
                        obligations =
                          append proved.obligations evaluated.obligations;
                        paths = evaluated.paths;
                      }
                | None ->
                    error function_name expression.span
                      (Malformed_sst
                         "recursive call has no validated termination edge \
                          intent")
            in
            let assume_summary state =
              let state =
                match consumed_root with
                | None -> state
                | Some binding ->
                    {
                      state with
                      environment =
                        remove_binding binding.Sst.id state.environment;
                    }
              in
              let* state =
                match summary.definition.body with
                | Sst.Trusted_external_spec_target
                    (Sst.Same_unit_target
                      {
                        wrapper;
                        target;
                        target_span;
                        declaration_span;
                        witness_span;
                      }) ->
                    Ok
                      {
                        state with
                        trusted_summary_uses =
                          append state.trusted_summary_uses
                            [
                              Vir.Trusted_external_specification_use
                                {
                                  target = vir_function_ref target;
                                  wrapper = vir_function_ref wrapper;
                                  target_span;
                                  wrapper_span = declaration_span;
                                  witness_span;
                                  call_span = expression.span;
                                  requires_count =
                                    List.length summary.requires;
                                  ensures_count =
                                    List.length summary.ensures;
                                };
                            ];
                      }
                | Sst.Trusted_external_spec_target
                    (Sst.Unresolved_target _) ->
                    error function_name expression.span
                      (Malformed_sst
                         "trusted external target has an unresolved linkage")
                | Sst.Trusted_external_body
                    (Sst.Authenticated_external_body
                      { declaration_span; witness_span; _ }) ->
                    Ok
                      {
                        state with
                        trusted_summary_uses =
                          append state.trusted_summary_uses
                            [
                              Vir.Trusted_external_body_use
                                {
                                  function_ref =
                                    vir_function_ref
                                      summary.definition.function_id;
                                  mode = summary.definition.mode;
                                  call_form;
                                  declaration_span;
                                  witness_span;
                                  call_span = expression.span;
                                  requires_count =
                                    List.length summary.requires;
                                  ensures_count =
                                    List.length summary.ensures;
                                };
                            ];
                      }
                | Sst.Trusted_external_body (Sst.Raw_external_body _) ->
                    error function_name expression.span
                      (Malformed_sst
                         "trusted external body has raw provenance")
                | Sst.Checked_exec _ | Sst.Proof_body _ -> Ok state
                | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
                | Sst.External_specification _
                ->
                    error function_name expression.span
                      (Malformed_sst
                         "call summary has a noncallable body disposition")
              in
              let* result, state =
                match call_form with
                | Sst.Proof_call when expression.typ = Sst.Unit ->
                    Ok (Unit_value, state)
                | Sst.Proof_call ->
                    fresh_value state
                      ~source_name:
                        (summary.definition.function_id.function_name
                       ^ ".proof-result")
                      ~role:Vir.Result ~span:expression.span ~project:true
                      expression.typ
                | Sst.Exec_call ->
                    fresh_value state
                      ~source_name:
                        (summary.definition.function_id.function_name ^ ".result")
                      ~role:Vir.Result ~span:expression.span ~project:true
                      expression.typ
                | Sst.Specification_call | Sst.Unclassified_call ->
                    assert false
              in
              let* state =
                match
                  ( summary.definition.returns_unique_parameter,
                    expression.typ,
                    result )
                with
                | ( Some _,
                    Sst.Aggregate type_id,
                    Aggregate_value aggregate ) ->
                    constrain_immediate_record_fields function_name
                      expression.span context.type_definitions state type_id
                      aggregate
                | Some _, _, _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "unique returned parameter has a non-record result")
                | None, _, _ -> Ok state
              in
              (* VERO-035 deliberately does not turn a verified callee result
                 into instance authority.  That receipt protocol remains the
                 unresolved VERO-032 boundary. *)
              let result_boundary, state = ([], state) in
              let actual_entry = actual_environment in
              let rec assume_posts states = function
                | [] ->
                    Ok
                      {
                        obligations = result_boundary;
                        paths =
                          List.map
                            (fun state -> { value = result; state })
                            states;
                      }
                | clause :: rest ->
                    let* evaluated =
                      evaluate_contexts
                        (fun state ->
                          let caller_environment = state.environment in
                          let* spec_environment, binder_ranges =
                            match clause.binder with
                            | None -> Ok (actual_entry, [])
                            | Some binder ->
                                bind_pattern_direct function_name actual_entry
                                  binder result
                          in
                          let post_context =
                            {
                              spec_context with
                              old_environment =
                                Some spec_context.entry_environment;
                            }
                          in
                          let* predicate =
                            evaluate post_context clause.payload
                              (with_assumptions
                                 { state with environment = spec_environment }
                                 binder_ranges)
                          in
                          let* paths =
                            let rec loop paths = function
                              | [] -> Ok (List.rev paths)
                              | evaluated :: remaining ->
                                  let* assumption =
                                    expect_boolean function_name clause.span
                                      evaluated.value
                                  in
                                  let state =
                                    with_assumptions evaluated.state
                                      [ assumption ]
                                    |> restore_environment caller_environment
                                  in
                                  loop (state :: paths) remaining
                            in
                            loop [] predicate.paths
                          in
                          Ok
                            {
                              obligations = predicate.obligations;
                              paths;
                            })
                        states
                    in
                    let* rest_result =
                      assume_posts evaluated.paths rest
                    in
                    Ok
                      {
                        obligations =
                          append evaluated.obligations
                            rest_result.obligations;
                        paths = rest_result.paths;
                      }
              in
              assume_posts [ state ] summary.ensures
            in
            let* summarized =
              evaluate_contexts assume_summary proved.paths
            in
            Ok
              {
                obligations =
                  append proved.obligations summarized.obligations;
                paths = summarized.paths;
              }
        in
        let* instantiated =
          evaluate_contexts instantiate arguments.paths
        in
        Ok
          {
            obligations =
              append arguments.obligations instantiated.obligations;
            paths = instantiated.paths;
          }
  | Sst.Direct_call _ ->
      error function_name expression.span
        (Malformed_sst "unclassified call reached symbolic execution")
  | Sst.Use_type_invariant { value; _ } ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst
             "use_type_invariant reached runtime evaluation")
      else
        let* handle =
          match value.typ with
          | Sst.Aggregate type_id -> (
              match Type_invariant.find_for_type context.invariants type_id with
              | Some handle -> Ok handle
              | None ->
                  error function_name expression.span
                    (Malformed_sst
                       "use_type_invariant has no authenticated exact-type \
                        handle"))
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ ->
              error function_name expression.span
                (Malformed_sst
                   "use_type_invariant value is not an abstract aggregate")
        in
        let* evaluated_value = evaluate context value state in
        let assume evaluated =
          match evaluated.value with
          | Aggregate_value _ ->
              let* closed =
                match closed_invariant_application handle evaluated.value with
                | Ok closed -> Ok closed
                | Error message ->
                    error function_name value.span (Malformed_sst message)
              in
              let* () =
                if List.mem closed evaluated.state.closed_invariant_facts then
                  Ok ()
                else
                  error function_name value.span
                    (Malformed_sst
                       "use_type_invariant requires a closed fact for this exact \
                        Exec/Tracked symbolic instance")
              in
              let* predicate =
                evaluate_invariant_predicate context handle evaluated.value
                  evaluated.state
              in
              let rec instantiate paths = function
                | [] -> Ok (List.rev paths)
                | evaluated :: rest ->
                    let* assumption =
                      expect_boolean function_name expression.span
                        evaluated.value
                    in
                    instantiate
                      ({
                         value = Unit_value;
                         state =
                           with_local_invariant_assumption evaluated.state
                             assumption;
                       }
                      :: paths)
                      rest
              in
              let* paths = instantiate [] predicate.paths in
              Ok { obligations = predicate.obligations; paths }
          | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _ ->
              error function_name expression.span
                (Malformed_sst
                   "use_type_invariant value changed from its abstract type")
        in
        let* assumed =
          evaluate_contexts assume evaluated_value.paths
        in
        Ok
          {
            obligations =
              append evaluated_value.obligations assumed.obligations;
            paths = assumed.paths;
          }
  | Sst.Local_assert _ ->
      error function_name expression.span
        (Malformed_sst
           "local assertions require the private authenticated verifier")
  | Sst.Proof_region proof_body ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "nested proof region reached proof evaluation")
      else
        let proof_context = { context with logical = true } in
        let caller_environment = state.environment in
        let caller_invariant_assumptions =
          state.local_invariant_assumptions
        in
        let* evaluated = evaluate proof_context proof_body state in
        Ok
          {
            evaluated with
            paths =
              List.map
                (fun evaluated ->
                  {
                    value = Unit_value;
                    state =
                      {
                        (restore_environment caller_environment evaluated.state)
                        with
                        local_invariant_assumptions =
                          caller_invariant_assumptions;
                      };
                  })
                evaluated.paths;
          }
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
      Call_contract_execution_private.evaluate_callback
        (callback_runtime evaluate context) context expression state
      |> Result.map
           (fun
             (result :
               (Vir.obligation, evaluated)
               Call_contract_execution_private.evaluation) ->
             ({ obligations = result.obligations; paths = result.paths }
               : evaluated path_evaluation))
  | Sst.Old payload when context.logical -> (
      match context.old_environment with
      | None ->
          error function_name expression.span
            (Malformed_sst "old reached a non-postcondition specification")
      | Some entry_environment ->
          let current_environment = state.environment in
          let* evaluated =
            evaluate context payload { state with environment = entry_environment }
          in
          Ok
            {
              evaluated with
              paths =
                List.map
                  (fun evaluated ->
                    {
                      evaluated with
                      state =
                        restore_environment current_environment evaluated.state;
                    })
                  evaluated.paths;
            })
  | Sst.Old _ ->
      error function_name expression.span
        (Malformed_sst "old reached a runtime expression")

and evaluate_invariant_predicate context handle value state =
  let function_name = context.function_ref.function_name in
  let predicate = Type_invariant.predicate_definition handle in
  let* parameter =
    match predicate.parameters with
    | [ parameter ] -> Ok parameter
    | _ ->
        error function_name predicate.span
          (Malformed_sst "authenticated invariant predicate arity changed")
  in
  let parameter = Sst.require_value_parameter parameter in
  let caller_environment = state.environment in
  let* environment, ranges =
    bind_pattern_direct function_name [] parameter.pattern value
  in
  let predicate_body =
    match predicate.body with
    | Sst.Spec_definition body -> body.expression
    | _ -> assert false
  in
  let predicate_context =
    {
      context with
      entry_environment = environment;
      logical = true;
      old_environment = None;
    }
  in
  let* predicate =
    evaluate predicate_context predicate_body
      (with_assumptions { state with environment } ranges)
  in
  Ok
    {
      predicate with
      paths =
        List.map
          (fun evaluated ->
            {
              evaluated with
              state =
                restore_environment caller_environment evaluated.state;
            })
          predicate.paths;
    }

and prove_invariant_validity context handle boundary operation span value state =
  let boundary_state = state in
  let* predecessor =
    match boundary with
    | Vir.Transition_preservation { root_binding_id; _ } ->
        let* predecessor_value =
          match List.assoc_opt root_binding_id state.environment with
          | Some value -> Ok value
          | None ->
              error context.function_ref.function_name span
                (Malformed_sst
                   "invariant transition predecessor is not a live root")
        in
        let* closed =
          match closed_invariant_application handle predecessor_value with
          | Ok closed -> Ok closed
          | Error message ->
              error context.function_ref.function_name span
                (Malformed_sst message)
        in
        if not (List.mem closed state.closed_invariant_facts) then
          error context.function_ref.function_name span
            (Malformed_sst
               "invariant transition predecessor has no authenticated closed \
                validity fact")
        else
          let* expanded =
            evaluate_invariant_predicate context handle predecessor_value state
          in
          let* paths =
            let rec assume paths = function
              | [] -> Ok (List.rev paths)
              | evaluated :: rest ->
                  let* fact =
                    expect_boolean context.function_ref.function_name span
                      evaluated.value
                  in
                  assume (with_assumptions evaluated.state [ fact ] :: paths)
                    rest
            in
            assume [] expanded.paths
          in
          Ok { obligations = expanded.obligations; paths }
    | Vir.Constructor_establishment | Vir.Call_argument _ | Vir.Call_result _
    | Vir.Function_return | Vir.Shared_invariant_close _
    | Vir.Terminal_observation _ ->
        Ok { obligations = []; paths = [ state ] }
  in
  let* predicate =
    evaluate_contexts
      (fun state -> evaluate_invariant_predicate context handle value state)
      predecessor.paths
  in
  let rec loop obligations paths = function
    | [] -> Ok { obligations = List.rev obligations; paths = List.rev paths }
    | predicate :: rest ->
        let* goal =
          expect_boolean context.function_ref.function_name span predicate.value
        in
        let kind =
          Vir.Invariant_validity
            {
              invariant_id = Type_invariant.invariant_id handle;
              abstract_type =
                Type_invariant.abstract_type handle |> vir_aggregate_type;
              model = Type_invariant.model_callable handle |> vir_function_ref;
              predicate =
                Type_invariant.predicate_callable handle |> vir_function_ref;
              operation;
              boundary;
            }
        in
        let obligation, state =
          emit_goal context.function_ref kind span goal predicate.state
        in
        let* closed =
          match closed_invariant_application handle value with
          | Ok closed -> Ok closed
          | Error message ->
              error context.function_ref.function_name span
                (Malformed_sst message)
        in
        let state =
          match boundary with
          | Vir.Transition_preservation _ ->
              {
                state with
                environment = boundary_state.environment;
                assumptions = boundary_state.assumptions;
                local_invariant_assumptions =
                  boundary_state.local_invariant_assumptions;
                closed_invariant_facts =
                  append boundary_state.closed_invariant_facts [ closed ];
                required_preceding_safety =
                  boundary_state.required_preceding_safety;
                path_condition = boundary_state.path_condition;
                projection_symbols = boundary_state.projection_symbols;
                trusted_summary_uses = boundary_state.trusted_summary_uses;
                entry_measure = boundary_state.entry_measure;
              }
          | Vir.Constructor_establishment | Vir.Call_argument _
          | Vir.Call_result _ | Vir.Function_return
          | Vir.Shared_invariant_close _
          | Vir.Terminal_observation _ ->
              with_closed_invariant_fact state closed
        in
        loop (obligation :: obligations)
          ({ value; state } :: paths) rest
  in
  let* proved = loop [] [] predicate.paths in
  Ok
    {
      obligations =
        append predecessor.obligations
          (append predicate.obligations proved.obligations);
      paths = proved.paths;
    }

let rec materialize_result ?(project = false) state span name value =
  match value with
  | Unit_value -> Ok (Vir.Unit_result, state)
  | Integer_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name ~sort:Vir.Integer ~role:Vir.Result
          ~span ~project
      in
      let term = Vir.Integer_symbol symbol in
      let equation = Vir.Integer_compare (Equal, term, value) in
      let state =
        with_assumptions state (equation :: Vir.integer_range term)
      in
      Ok (Vir.Integer_result symbol, state)
  | Boolean_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name ~sort:Vir.Boolean ~role:Vir.Result
          ~span ~project
      in
      let state =
        with_assumptions state
          [ Vir.Boolean_equal (Vir.Boolean_symbol symbol, value) ]
      in
      Ok (Vir.Boolean_result symbol, state)
  | Tuple_value values ->
      let rec loop index state results = function
        | [] -> Ok (Vir.Tuple_result (List.rev results), state)
        | value :: rest ->
            let* result, state =
              materialize_result ~project state span
                (Printf.sprintf "%s.%d" name index)
                value
            in
            loop (index + 1) state (result :: results) rest
      in
      loop 0 state [] values
  | Aggregate_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name
          ~sort:(Vir.Aggregate value.aggregate_type) ~role:Vir.Result ~span
          ~project
      in
      let term =
        {
          Vir.aggregate_type = value.aggregate_type;
          aggregate_desc = Vir.Aggregate_symbol symbol;
        }
      in
      let state =
        with_assumptions state [ Vir.Aggregate_equal (term, value) ]
      in
      Ok (Vir.Aggregate_result symbol, state)

let rec value_of_result = function
  | Vir.Unit_result -> Unit_value
  | Vir.Integer_result symbol -> Integer_value (Vir.Integer_symbol symbol)
  | Vir.Boolean_result symbol -> Boolean_value (Vir.Boolean_symbol symbol)
  | Vir.Tuple_result components ->
      Tuple_value (List.map value_of_result components)
  | Vir.Aggregate_result symbol ->
      let aggregate_type =
        match symbol.Vir.sort with
        | Vir.Aggregate aggregate_type -> aggregate_type
        | Vir.Integer | Vir.Boolean -> assert false
      in
      Aggregate_value
        { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }

let summary_of_callable descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  let contract = Sst_validation.callable_contract descriptor in
  let contract_clause clause =
    {
      ordinal = Sst_validation.contract_clause_index clause;
      span = Sst_validation.contract_clause_span clause;
      binder = Sst_validation.contract_clause_binder clause;
      payload = Sst_validation.contract_clause_expression clause;
    }
  in
  let requires =
    List.map contract_clause (Sst_validation.contract_requires contract)
  and ensures =
    List.map contract_clause (Sst_validation.contract_ensures contract)
  and assertions =
    List.map contract_clause (Sst_validation.contract_assertions contract)
  in
  match definition.body with
  | Sst.Checked_exec { body = { expression = executable_body; _ }; _ }
  | Sst.Proof_body { body = { expression = executable_body; _ }; _ } ->
      Ok
        {
          definition;
          requires;
          ensures;
          assertions;
          executable_body;
        }
  | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _ ->
      Ok
        {
          definition;
          requires;
          ensures;
          assertions;
          executable_body =
            {
              Sst.expression_desc = Sst.Unit_constant;
              typ = Sst.Unit;
              span = definition.span;
            };
        }
  | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
  | Sst.External_specification _ ->
      error definition.function_id.function_name definition.span
        (Malformed_sst "validated declaration has no checked executable body")

let termination_clause termination summary =
  match
    Termination.find_entry_intent termination
      summary.definition.Sst.function_id
  with
  | None -> None
  | Some intent ->
      let decrease = Termination.entry_measure intent in
      let clause =
        Sst_validation.decrease_clause decrease
      in
      Some
        ( Termination.entry_domain intent,
          {
            ordinal = Sst_validation.contract_clause_index clause;
            span = Sst_validation.contract_clause_span clause;
            binder = Sst_validation.contract_clause_binder clause;
            payload = Sst_validation.contract_clause_expression clause;
          } )

let lower_summary physical_program validated rank_domains invariants
    type_definitions definitions summaries termination summary =
  let definition = summary.definition in
  let termination_clause = termination_clause termination summary in
  let function_ref = function_ref definition in
  let reached_callback_calls = ref [] in
  let* callable =
    match Sst_validation.find_callable validated definition.function_id with
    | Some callable -> Ok callable
    | None ->
        error function_ref.function_name definition.span
          (Malformed_sst "validated callable is absent from mode authority")
  in
  let initial =
    {
      environment = [];
      assumptions = [];
      local_invariant_assumptions = [];
      closed_invariant_facts = [];
      required_preceding_safety = [];
      path_condition = [];
      projection_symbols = [];
      trusted_summary_uses = [];
      next_symbol = 0;
      next_obligation = 0;
      entry_measure = None;
    }
  in
  let rec parameters index state = function
    | [] -> Ok state
    | Sst.Callback_parameter _ :: rest ->
        parameters (index + 1) state rest
    | Sst.Value_parameter parameter :: rest ->
        let* value, state =
          fresh_parameter function_ref.function_name state index
            parameter.Sst.pattern
        in
        let* state =
          match
            ( invariant_for_typ invariants parameter.Sst.pattern.typ,
              Sst_validation.formal_has_closed_invariant_authority validated
                callable index )
          with
          | None, _ | Some _, false -> Ok state
          | Some handle, true -> (
              match closed_invariant_application handle value with
              | Ok closed -> Ok (with_closed_invariant_fact state closed)
              | Error message ->
                  error function_ref.function_name parameter.pattern.span
                    (Malformed_sst message))
        in
        parameters (index + 1) state rest
  in
  let* state = parameters 0 initial definition.parameters in
  let* captures =
    Sst_callback_private.authenticated_captures physical_program definition
    |> Result.map_error (fun (span, message) ->
           {
             function_name = function_ref.function_name;
             span;
             unsupported = Malformed_sst message;
           })
  in
  let* state =
    List.fold_left
      (fun state
           ( (capture : Callback_certificate_private.capture),
             binding ) ->
        let* state = state in
        let* value, state =
          fresh_value state ~source_name:binding.Sst.name ~role:Vir.Input
            ~span:binding.span ~project:true capture.typ
        in
        Ok
          {
            state with
            environment = (binding.id, value) :: state.environment;
          })
      (Ok state) captures
  in
  let entry_environment = state.environment in
  let logical_context =
    {
      validated;
      function_ref;
      current_callable = definition.function_id;
      definitions;
      summaries;
      termination;
      type_definitions;
      entry_environment;
      logical = true;
      old_environment = None;
      spec_call_stack = [];
      spec_expansion_limit = List.length definitions + 1;
      rank_domains;
      invariants;
      reached_callback_calls;
      callback_environment = [];
    }
  in
  let runtime_context =
    { logical_context with logical = false }
  in
  let body_context =
    match definition.mode with
    | Sst.Proof -> logical_context
    | Sst.Exec -> runtime_context
    | Sst.Spec -> assert false
  in
  let rec assume_requires obligations states = function
    | [] -> Ok { obligations; paths = states }
    | clause :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* predicate =
                evaluate logical_context clause.payload state
              in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: remaining ->
                      let* assumption =
                        expect_boolean function_ref.function_name clause.span
                          evaluated.value
                      in
                      let state =
                        with_assumptions evaluated.state [ assumption ]
                        |> restore_environment current_environment
                      in
                      loop (state :: paths) remaining
                in
                loop [] predicate.paths
              in
              Ok { obligations = predicate.obligations; paths })
            states
        in
        assume_requires
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  let* required = assume_requires [] [ state ] summary.requires in
  let* measured =
    match termination_clause with
    | None -> Ok required
    | Some (domain, measure) ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* measured =
                evaluate runtime_context measure.payload
                  { state with environment = entry_environment }
              in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | measured :: remaining ->
                      let* entry_measure =
                        ranked_measure function_ref.function_name measure.span
                          runtime_context.rank_domains domain measured.value
                      in
                      let measured_state =
                        restore_environment current_environment measured.state
                      in
                      let nonnegative =
                        Vir.Integer_compare
                          ( Vir.Less_or_equal,
                            Vir.Integer_constant Z.zero,
                            entry_measure )
                      in
                      let obligation, measured_state =
                        emit_goal function_ref
                          (Vir.Entry_measure_nonnegative
                             { declaration_span = measure.span })
                          measure.span nonnegative measured_state
                      in
                      let measured_state =
                        match domain with
                        | Termination.Structural_rank _ ->
                            with_assumptions measured_state [ nonnegative ]
                        | Termination.Integer_height
                        | Termination.Frozen_spine_direct_edge _ ->
                            measured_state
                      in
                      let measured_state =
                        {
                          measured_state with
                          entry_measure = Some (domain, entry_measure);
                        }
                      in
                      loop
                        ((obligation, measured_state) :: paths)
                        remaining
                in
                loop [] measured.paths
              in
              let obligations, states = List.split paths in
              Ok
                {
                  obligations = append measured.obligations obligations;
                  paths = states;
                })
            required.paths
        in
        Ok
          {
            obligations =
              append required.obligations evaluated.obligations;
            paths = evaluated.paths;
          }
  in
  let rec prove_assertions obligations states = function
    | [] -> Ok { obligations; paths = states }
    | clause :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* predicate =
                evaluate logical_context clause.payload state
              in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: remaining ->
                      let* goal =
                        expect_boolean function_ref.function_name clause.span
                          evaluated.value
                      in
                      let obligation, state =
                        emit_goal function_ref
                          (Vir.Assertion
                             { assertion_ordinal = clause.ordinal })
                          clause.span goal evaluated.state
                      in
                      loop
                        ( ( obligation,
                            restore_environment current_environment state )
                        :: paths )
                        remaining
                in
                loop [] predicate.paths
              in
              let emitted, states = List.split paths in
              Ok
                {
                  obligations =
                    append predicate.obligations emitted;
                  paths = states;
                })
            states
        in
        prove_assertions
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  let* asserted =
    prove_assertions measured.obligations measured.paths summary.assertions
  in
  let* body =
    evaluate_contexts
      (fun state ->
        evaluate body_context summary.executable_body state)
      asserted.paths
  in
  let evaluated_obligations =
    append asserted.obligations body.obligations
  in
  let prove_postconditions evaluated =
    let* return_obligations, state =
      match
        ( invariant_for_typ invariants definition.result_type,
          Sst_validation.result_instance_mode validated callable )
      with
      | None, _ | Some _, Sst.Ghost_instance -> Ok ([], evaluated.state)
      | Some handle, (Sst.Exec_instance | Sst.Tracked_instance) ->
          let* obligation, state =
            emit_closed_invariant_goal logical_context handle
              Vir.Function_return function_ref definition.span evaluated.value
              evaluated.state
          in
          Ok ([ obligation ], state)
    in
    let* result, state =
      materialize_result ~project:(summary.ensures <> []) state
        summary.executable_body.span "result" evaluated.value
    in
    let result_value = value_of_result result in
    let rec prove obligations states = function
      | [] ->
          Ok
            {
              obligations;
              paths =
                List.map
                  (fun state ->
                    {
                      Vir.assumptions = state.assumptions;
                      path_condition = state.path_condition;
                      result;
                      projection_symbols = state.projection_symbols;
                      trusted_summary_uses =
                        state.trusted_summary_uses;
                    })
                  states;
            }
      | clause :: rest ->
          let* evaluated =
            evaluate_contexts
              (fun state ->
                let current_environment = state.environment in
                let* environment, binder_ranges =
                  match clause.binder with
                  | None -> Ok (current_environment, [])
                  | Some binder ->
                      bind_pattern_direct function_ref.function_name
                        current_environment binder result_value
                in
                let post_context =
                  {
                    logical_context with
                    old_environment =
                      Some logical_context.entry_environment;
                  }
                in
                let* predicate =
                  evaluate post_context clause.payload
                    (with_assumptions { state with environment } binder_ranges)
                in
                let* paths =
                  let rec loop paths = function
                    | [] -> Ok (List.rev paths)
                    | evaluated :: remaining ->
                        let* goal =
                          expect_boolean function_ref.function_name clause.span
                            evaluated.value
                        in
                        let obligation, state =
                          emit_goal function_ref
                            (Vir.Postcondition
                               {
                                 postcondition_ordinal = clause.ordinal;
                                 declaration_span = clause.span;
                               })
                            clause.span goal evaluated.state
                        in
                        loop
                          ( ( obligation,
                              restore_environment current_environment state )
                          :: paths )
                          remaining
                  in
                  loop [] predicate.paths
                in
                let emitted, states = List.split paths in
                Ok
                  {
                    obligations =
                      append predicate.obligations emitted;
                    paths = states;
                  })
              states
          in
          prove
            (append obligations evaluated.obligations)
            evaluated.paths rest
    in
    prove return_obligations [ state ] summary.ensures
  in
  let* postconditions =
    evaluate_contexts prove_postconditions body.paths
  in
  (* Branches copy immutable allocator state.  Renumbering the final traversal
     gives every emitted VC a deterministic function-local identity without a
     mutable global counter or branch-order side channel. *)
  let obligations =
    List.mapi
      (fun obligation_index (obligation : Vir.obligation) ->
        { obligation with obligation_index })
      (append evaluated_obligations postconditions.obligations)
  in
  let body_provenance =
    match definition.body with
    | Sst.Checked_exec { provenance; _ } -> provenance
    | Sst.Proof_body { provenance; _ } -> provenance
    | _ -> assert false
  in
  let trusted_summary_uses =
    let add uses use =
      if List.mem use uses then uses else append uses [ use ]
    in
    List.fold_left
      (fun uses (exit : Vir.exit) ->
        List.fold_left add uses exit.trusted_summary_uses)
      [] postconditions.paths
  in
  Ok
    {
      Vir.function_ref;
      mode = definition.mode;
      body_provenance;
      policy = definition.policy;
      trusted_summary_uses;
      reached_callback_calls = List.rev !reached_callback_calls;
      owned_tree_transitions =
        owned_tree_transitions summary.executable_body;
      shared_scalar_heap_reads = [];
      shared_scalar_heap_writes = [];
      obligations;
      exits = postconditions.paths;
    }

let error_of_termination termination_error =
  let function_name =
    Option.fold ~none:"program"
      ~some:(fun id -> id.Sst.function_name)
      termination_error.Termination.function_id
  in
  let unsupported =
    match termination_error.kind with
    | Termination.Missing_measure -> Missing_decreases
    | Termination.Duplicate_measure -> Duplicate_decreases
    | Termination.Inapplicable_measure -> Inapplicable_decreases
    | Termination.Non_integer_measure -> Non_integer_decreases
    | Termination.Recursive_measure
    | Termination.Unsupported_mutual_scc _
    | Termination.Unsupported_recursive_mode _
    | Termination.Raw_analysis_only ->
        Malformed_sst (Termination.error_to_string termination_error)
  in
  error function_name termination_error.span unsupported

let prepare_termination validated =
  match Termination.prepare (Termination.analyze validated) with
  | Ok termination -> Ok termination
  | Error termination -> error_of_termination termination

let prepare_invariants validated =
  match Type_invariant.authenticate validated with
  | Ok invariants -> Ok invariants
  | Error invariant ->
      error "program" invariant.span
        (Malformed_sst (Type_invariant.error_to_string invariant))

let is_legacy_recursive_graph_error = function
  | Sst_validation.Invalid_call detail ->
      String.equal detail "cyclic pure specification call graph"
      || String.equal detail "cyclic proof call graph"
  | _ -> false

let validate_with_raw_termination (program : Sst.program) =
  let raw_termination = Termination.analyze_raw program in
  match Sst_validation.validate program with
  | Ok validated -> Ok validated
  | Error validation when is_legacy_recursive_graph_error validation.kind -> (
      match Termination.precheck raw_termination with
      | Error termination -> error_of_termination termination
      | Ok () ->
          error
            (Option.fold ~none:"program"
               ~some:(fun id -> id.Sst.function_name)
               validation.function_id)
            validation.span
            (Malformed_sst (Sst_validation.error_to_string validation)))
  | Error validation ->
      error
        (Option.fold ~none:"program"
           ~some:(fun id -> id.Sst.function_name)
           validation.function_id)
        validation.span
        (Malformed_sst (Sst_validation.error_to_string validation))

let legacy_parametric_rejection (definition : Sst.function_definition) =
  let rec legacy_type = function
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _ -> true
    | Sst.Tuple components ->
        List.for_all (fun (_, typ) -> legacy_type typ) components
    | Sst.Parameter _ | Sst.Application _ -> false
  in
  if
    definition.type_binders <> []
    || not (legacy_type definition.result_type)
    || List.exists
         (function
           | Sst.Value_parameter parameter ->
               not (legacy_type parameter.pattern.typ)
           | Sst.Callback_parameter _ -> false)
         definition.parameters
  then
    Some
      {
        function_name = definition.function_id.function_name;
        span = definition.span;
        unsupported =
          Malformed_sst
            "the legacy public executor rejects first-class parametric functions";
      }
  else None

let error_of_private (error : Symbolic_executor_private.error) =
  let unsupported =
    match error.unsupported with
    | Symbolic_executor_private.Ghost_call -> Ghost_call
    | Or_pattern -> Or_pattern
    | Missing_summary id -> Missing_summary id
    | Recursion_awaits_totality id -> Recursion_awaits_totality id
    | Missing_decreases -> Missing_decreases
    | Duplicate_decreases -> Duplicate_decreases
    | Inapplicable_decreases -> Inapplicable_decreases
    | Non_integer_decreases -> Non_integer_decreases
    | Malformed_sst message -> Malformed_sst message
  in
  {
    function_name = error.function_name;
    span = error.span;
    unsupported;
  }

let lower_function (definition : Sst.function_definition) =
  if Quantifier_validation_private.definition_has_quantifier definition then
    Symbolic_executor_private.lower_function definition
    |> Result.map_error error_of_private
  else
  match legacy_parametric_rejection definition with
  | Some error -> Error error
  | None ->
  let program =
    {
      Sst.policy = definition.policy;
      parametric_adts = [];
      types = [];
      functions = [ definition ];
    }
  in
  let* validated = validate_with_raw_termination program in
  let* termination = prepare_termination validated in
  let* invariants = prepare_invariants validated in
  let declarations = Sst_validation.callable_descriptors validated in
  let* descriptor =
    match Sst_validation.find_callable validated definition.function_id with
    | Some descriptor -> Ok descriptor
    | None ->
        error definition.function_id.function_name definition.span
          (Malformed_sst "validated callable is absent from environment")
  in
  let* summary = summary_of_callable descriptor in
  let definitions =
    List.map
      (fun descriptor ->
        ((Sst_validation.callable_id descriptor).function_index, descriptor))
      declarations
  in
  let* summaries =
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | descriptor :: rest ->
          let definition = Sst_validation.callable_definition descriptor in
          let* summary = summary_of_callable descriptor in
          loop
            ((definition.Sst.function_id.function_index, summary) :: lowered)
            rest
    in
    loop [] declarations
  in
  lower_summary program validated [] invariants [] definitions summaries
    termination summary

let lower_program (program : Sst.program) =
  if Quantifier_validation_private.program_has_quantifier program then
    Symbolic_executor_private.lower_program program
    |> Result.map_error error_of_private
  else
  match List.find_map legacy_parametric_rejection program.functions with
  | Some _ ->
      Symbolic_executor_private.lower_program program
      |> Result.map_error error_of_private
  | None ->
  let* validated = validate_with_raw_termination program in
  let* termination = prepare_termination validated in
  let* invariants = prepare_invariants validated in
  let descriptor_declarations =
    Sst_validation.callable_descriptors validated
  in
  let declarations =
    List.map Sst_validation.callable_definition descriptor_declarations
  in
  let type_definitions =
    List.map Sst_validation.type_definition
      (Sst_validation.type_descriptors validated)
  in
  let rank_domains = Vir.rank_domains_of_validated validated in
  let definitions =
    List.map
      (fun descriptor ->
        ((Sst_validation.callable_id descriptor).function_index, descriptor))
      descriptor_declarations
  in
  let* summaries =
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | descriptor :: rest ->
          let definition = Sst_validation.callable_definition descriptor in
          (match definition.Sst.body with
          | Sst.Checked_exec _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.External_specification _ ->
              loop lowered rest
          | Sst.Proof_body _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Trusted_external_spec_target _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Trusted_external_body _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest)
    in
    loop [] descriptor_declarations
  in
  let rec loop functions = function
    | [] ->
        let trusted_external_body_declarations =
          List.filter_map
            (fun definition ->
              match definition.Sst.body with
              | Sst.Trusted_external_body
                  (Sst.Authenticated_external_body
                    { declaration_span; witness_span; _ }) ->
                  Some
                    {
                      Vir.function_ref =
                        vir_function_ref definition.function_id;
                      mode = definition.mode;
                      declaration_span;
                      witness_span;
                      requires_count =
                        List.length definition.contracts.requires;
                      ensures_count =
                        List.length definition.contracts.ensures;
                    }
              | Sst.Trusted_external_body (Sst.Raw_external_body _) ->
                  assert false
              | _ -> None)
            declarations
        in
        Ok
          {
            Vir.policy = program.policy;
            rank_domains;
            trusted_external_body_declarations;
            functions = List.rev functions;
          }
    | definition :: rest ->
        (match definition.Sst.body with
        | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
        | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ ->
            loop functions rest
        | (Sst.Checked_exec _ | Sst.Proof_body _) ->
            let* summary =
              match
                List.assoc_opt definition.Sst.function_id.function_index summaries
              with
              | Some summary -> Ok summary
              | None ->
                  error definition.function_id.function_name definition.span
                    (Malformed_sst "validated callable is absent from registry")
            in
            let* execution =
              lower_summary program validated rank_domains invariants
                type_definitions definitions summaries termination summary
            in
            loop (execution :: functions) rest
        | Sst.Trusted_external_body _ -> loop functions rest)
  in
  loop [] declarations

let error_to_string error =
  let unsupported =
    match error.unsupported with
    | Ghost_call -> "frontend contract carrier reached symbolic execution"
    | Or_pattern -> "or-patterns await symbolic pattern lowering"
    | Missing_summary callee ->
        Printf.sprintf "missing same-unit summary for %s#%d"
          callee.function_name callee.function_index
    | Recursion_awaits_totality callee ->
        Printf.sprintf "recursion awaits totality for %s#%d"
          callee.function_name callee.function_index
    | Missing_decreases ->
        "direct recursion requires exactly one decreases measure"
    | Duplicate_decreases ->
        "direct recursion has more than one decreases measure"
    | Inapplicable_decreases ->
        "decreases measure is not allowed without a resolved direct self-call"
    | Non_integer_decreases ->
        "decreases measure must have type int"
    | Malformed_sst message -> "malformed SST: " ^ message
  in
  let span = error.span in
  Printf.sprintf "%s: %s at %s:%d:%d-%d:%d" error.function_name unsupported
    (Filename.basename span.file) span.start_pos.line span.start_pos.column
    span.end_pos.line span.end_pos.column
