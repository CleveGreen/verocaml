type translation = {
  query : Logic_ir.query;
  projected : (Vir.symbol * Logic_ir.function_symbol) list;
}

type state = {
  builder : Logic_ir.builder;
  span : Diagnostic.span;
  function_index : int;
  functions :
    ( string,
      Logic_ir.sort list * Logic_ir.sort * Logic_ir.function_symbol )
    Hashtbl.t;
  rank_domains : (string, Logic_ir.rank_domain) Hashtbl.t;
  rank_axioms : Logic_ir.axiom list ref;
  aggregate_sorts : (Vir.aggregate_type, Logic_ir.sort) Hashtbl.t;
  parametric_sorts : Parametric_logic_private.logic_sort_registry;
  use_named_aggregate_sorts : bool;
  logical_adts : Logical_adt_encoding_private.t option ref;
  bound_symbols : (int * Logic_ir.binder) list;
}
exception Translation_error of string
let query translation = translation.query
let projected translation = translation.projected
let fail format =
  Printf.ksprintf (fun message -> raise (Translation_error message)) format
let logic_or_fail = function
  | Ok value -> value
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

type 'error user_quantifier_services = {
  quantifier_builder : Logic_ir.builder;
  quantifier_sort : Vir.sort -> Logic_ir.sort;
  translate :
    (int * Logic_ir.binder) list ->
    Vir.boolean_term ->
    (Logic_ir.term, 'error) result;
  translate_application :
    (int * Logic_ir.binder) list ->
    Vir.application_term ->
    (Logic_ir.term, 'error) result;
  bind_result :
    (Logic_ir.binder, Logic_ir.error) result ->
    (Logic_ir.binder, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : Diagnostic.span -> string -> 'error;
}

let translate_user_quantifier services ~universal quantifier =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let schema = quantifier.Vir.boolean_quantifier_schema in
  let expected_kind =
    if universal then Logic_quantifier_private.Forall
    else Logic_quantifier_private.Exists
  in
  let span = Logic_quantifier_private.vector_span schema in
  if Logic_quantifier_private.vector_kind schema <> expected_kind then
    Error
      (services.malformed span
         "VIR user quantifier kind disagrees with authenticated metadata")
  else
    let* binders =
      List.fold_left
        (fun result (symbol : Vir.symbol) ->
          let* binders = result in
          let* binder =
            Logic_ir.bind services.quantifier_builder ~name:symbol.source_name
              ~sort:(services.quantifier_sort symbol.sort) ~span:symbol.span
            |> services.bind_result
          in
          Ok ((symbol.symbol_id, binder) :: binders))
        (Ok []) quantifier.boolean_quantifier_binders
      |> Result.map List.rev
    in
    let* body =
      services.translate binders quantifier.boolean_quantifier_body
    in
    let logic_binders = List.map snd binders in
    let qid = Logic_quantifier_private.vector_qid schema
    and skid = Logic_quantifier_private.vector_skid schema in
    match (universal, quantifier.boolean_quantifier_trigger) with
    | true, Some trigger ->
        let* trigger = services.translate_application binders trigger in
        Logic_ir.forall_term services.quantifier_builder ~binders:logic_binders ~body
          ~trigger ~qid ~skid ~span
        |> services.term_result
    | true, None ->
        Error
          (services.malformed span
             "VIR universal quantifier lost its explicit trigger")
    | false, None ->
        Logic_ir.exists_term services.quantifier_builder ~binders:logic_binders ~body ~qid
          ~skid ~span
        |> services.term_result
    | false, Some _ ->
        Error
          (services.malformed span
             "VIR existential quantifier acquired a trigger")

type 'error relation_services = {
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  declare :
    string ->
    Logic_ir.sort list ->
    Logic_ir.sort ->
    Diagnostic.span ->
    (Logic_ir.function_symbol, 'error) result;
  translate_arguments :
    Vir.recursive_spec_argument list -> (Logic_ir.term list, 'error) result;
  apply :
    Diagnostic.span ->
    Logic_ir.function_symbol ->
    Logic_ir.term list ->
    (Logic_ir.term, 'error) result;
}

let translate_relation services ~name ~range ~arguments ~span =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let domain =
    List.map
      (function
        | Vir.Recursive_integer_argument _ -> Logic_ir.Int
        | Vir.Recursive_boolean_argument _ -> Logic_ir.Bool
        | Vir.Recursive_aggregate_argument term ->
            services.aggregate_sort term.Vir.aggregate_type
        | Vir.Recursive_parametric_argument term ->
            services.parametric_sort term.Vir.parametric_sort)
      arguments
  in
  let* function_ = services.declare name domain range span in
  let* arguments = services.translate_arguments arguments in
  services.apply span function_ arguments

type 'error scalar_selector_services = {
  span : Diagnostic.span;
  logic_sort : Vir.sort -> Logic_ir.sort;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  selector_function :
    Vir.selector ->
    Logic_ir.sort ->
    Logic_ir.sort ->
    (Logic_ir.function_symbol, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_aggregate : Vir.aggregate_term -> (Logic_ir.term, 'error) result;
  translate_symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_symbolic_application :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}

let translate_scalar_selector services ~expected ~normalize ~exact selector
    source =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  if selector.Vir.selector_domain <> source.Vir.aggregate_type then
    Error (services.malformed "scalar selector has a mismatched owner")
  else if selector.selector_range <> expected then
    Error (services.malformed "scalar selector has a mismatched result type")
  else
    let* observation =
      normalize selector source |> Result.map_error services.malformed
    in
    Logical_aggregate_term_normalization_private.fold
      ~conditional:(fun condition consequent alternative ->
        let* condition = services.translate_boolean condition in
        let* consequent = consequent in
        let* alternative = alternative in
        Logic_ir.ite ~span:services.span condition ~then_:consequent
          ~else_:alternative
        |> services.term_result)
      ~exact
      ~opaque:(fun (source : Vir.aggregate_term) ->
        let* function_ =
          services.selector_function selector
            (services.aggregate_sort selector.selector_domain)
            (services.logic_sort expected)
        in
        let* source = services.translate_aggregate source in
        Logic_ir.apply ~span:services.span function_ [ source ]
        |> services.term_result)
      observation

let translate_integer_selector services =
  translate_scalar_selector services ~expected:Vir.Integer
    ~normalize:Logical_aggregate_term_normalization_private.integer_selector
    ~exact:services.translate_integer

let translate_boolean_selector services =
  translate_scalar_selector services ~expected:Vir.Boolean
    ~normalize:Logical_aggregate_term_normalization_private.boolean_selector
    ~exact:services.translate_boolean

let translate_parametric services (term : Vir.parametric_term) =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  [%log.debug "translating parametric VIR term"
    ~sort:
      (Delator.Field.string
         (Parametric_logic_private.sort_name term.parametric_sort))];
  Parametric_logic_private.fold term ~symbol:services.translate_symbol
    ~selector:(fun selector source ->
      let* function_ =
        services.selector_function selector
          (services.aggregate_sort source.Vir.aggregate_type)
          (services.logic_sort (Vir.Parametric term.parametric_sort))
      in
      let* source = services.translate_aggregate source in
      Logic_ir.apply ~span:services.span function_ [ source ]
      |> services.term_result)
    ~conditional:(fun condition consequent alternative ->
      let* condition = services.translate_boolean condition in
      let* consequent = consequent in
      let* alternative = alternative in
      Logic_ir.ite ~span:services.span condition ~then_:consequent
        ~else_:alternative
      |> services.term_result)
    ~application:services.translate_symbolic_application

type 'error integer_core_services = {
  span : Diagnostic.span;
  symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_arguments :
    Vir.recursive_spec_argument list -> (Logic_ir.term list, 'error) result;
  recursive_function :
    Sst.function_id ->
    Parametric_type.t list ->
    (Logic_ir.function_symbol, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
}

let translate_integer_core services term =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let binary constructor left right =
    let* left = services.translate_integer left in
    let* right = services.translate_integer right in
    constructor ~span:services.span left right |> services.term_result
  in
  match term with
  | Vir.Integer_constant value ->
      Ok (Some (Logic_ir.int ~span:services.span value))
  | Vir.Integer_symbol symbol ->
      let* term = services.symbol symbol in
      Ok (Some term)
  | Vir.Integer_add (left, right) ->
      let* term = binary Logic_ir.add left right in
      Ok (Some term)
  | Vir.Integer_subtract (left, right) ->
      let* term = binary Logic_ir.subtract left right in
      Ok (Some term)
  | Vir.Integer_negate value ->
      let* value = services.translate_integer value in
      let* term =
        Logic_ir.negate ~span:services.span value |> services.term_result
      in
      Ok (Some term)
  | Vir.Integer_multiply_constant (coefficient, value) ->
      let* value = services.translate_integer value in
      let* term =
        Logic_ir.scale ~span:services.span coefficient value
        |> services.term_result
      in
      Ok (Some term)
  | Vir.Integer_absolute_value value ->
      let* value = services.translate_integer value in
      let zero = Logic_ir.int ~span:services.span Z.zero in
      let* negative =
        Logic_ir.less_than ~span:services.span value zero
        |> services.term_result
      in
      let* negated =
        Logic_ir.negate ~span:services.span value |> services.term_result
      in
      let* term =
        Logic_ir.ite ~span:services.span negative ~then_:negated ~else_:value
        |> services.term_result
      in
      Ok (Some term)
  | Vir.Integer_conditional (condition, consequent, alternative) ->
      let* condition = services.translate_boolean condition in
      let* consequent = services.translate_integer consequent in
      let* alternative = services.translate_integer alternative in
      let* term =
        Logic_ir.ite ~span:services.span condition ~then_:consequent
          ~else_:alternative
        |> services.term_result
      in
      Ok (Some term)
  | Vir.Integer_recursive_spec_application
      { callee; type_arguments; arguments; span } ->
      let* function_ = services.recursive_function callee type_arguments in
      let* arguments = services.translate_arguments arguments in
      let* term =
        Logic_ir.apply ~span function_ arguments |> services.term_result
      in
      Ok (Some term)
  | Vir.Integer_symbolic_application _ ->
      Ok None
  | Vir.Aggregate_tag _ | Vir.Integer_selector _ | Vir.Integer_rank_project _ ->
      Ok None

type 'error boolean_core_services = {
  span : Diagnostic.span;
  symbol : Vir.symbol -> (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_integer : Vir.integer_term -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
}

let translate_boolean_core services term =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let boolean_binary constructor left right =
    let* left = services.translate_boolean left in
    let* right = services.translate_boolean right in
    constructor ~span:services.span [ left; right ] |> services.term_result
  in
  match term with
  | Vir.Boolean_constant value ->
      Ok (Some (Logic_ir.bool ~span:services.span value))
  | Vir.Logical_adt_schema _ ->
      Ok (Some (Logic_ir.bool ~span:services.span true))
  | Vir.Boolean_symbol symbol ->
      let* term = services.symbol symbol in
      Ok (Some term)
  | Vir.Boolean_not value ->
      let* value = services.translate_boolean value in
      let* term =
        Logic_ir.not_ ~span:services.span value |> services.term_result
      in
      Ok (Some term)
  | Vir.Boolean_and (left, right) ->
      let* term = boolean_binary Logic_ir.and_ left right in
      Ok (Some term)
  | Vir.Boolean_or (left, right) ->
      let* term = boolean_binary Logic_ir.or_ left right in
      Ok (Some term)
  | Vir.Integer_compare (comparison, left, right) ->
      let* left = services.translate_integer left in
      let* right = services.translate_integer right in
      let constructor =
        match comparison with
        | Vir.Equal -> Logic_ir.equal
        | Not_equal -> Logic_ir.distinct
        | Less_than -> Logic_ir.less_than
        | Less_or_equal -> Logic_ir.less_or_equal
        | Greater_than -> Logic_ir.greater_than
        | Greater_or_equal -> Logic_ir.greater_or_equal
      in
      let* term =
        constructor ~span:services.span left right |> services.term_result
      in
      Ok (Some term)
  | Vir.Boolean_equal (left, right) | Vir.Boolean_not_equal (left, right) ->
      let* left = services.translate_boolean left in
      let* right = services.translate_boolean right in
      let constructor =
        match term with
        | Vir.Boolean_equal _ -> Logic_ir.equal
        | Vir.Boolean_not_equal _ -> Logic_ir.distinct
        | _ -> assert false
      in
      let* term =
        constructor ~span:services.span left right |> services.term_result
      in
      Ok (Some term)
  | Vir.Forall_term _ | Vir.Exists_term _ | Vir.Parametric_equal _
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ | Vir.Boolean_selector _
  | Vir.Aggregate_equal _ | Vir.Boolean_invariant_application _ ->
      Ok None

type 'error parametric_equality_services = {
  span : Diagnostic.span;
  symbol :
    Parametric_type.binder ->
    Vir.symbol ->
    (Logic_ir.term, 'error) result;
  selector :
    Parametric_type.binder ->
    Vir.selector ->
    Vir.aggregate_term ->
    (Logic_ir.term, 'error) result;
  application :
    Parametric_type.binder ->
    Vir.recursive_spec_argument Symbolic_application_private.t ->
    (Logic_ir.term, 'error) result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}

let translate_parametric_equality services left right =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let conditional condition consequent alternative =
    let* condition = services.translate_boolean condition in
    let* consequent = consequent in
    let* alternative = alternative in
    Logic_ir.ite ~span:services.span condition ~then_:consequent
      ~else_:alternative
    |> services.term_result
  in
  let* left, right =
    Parametric_logic_private.fold_equal ~symbol:services.symbol
      ~selector:services.selector ~conditional ~application:services.application
      left right
    |> Result.map_error services.malformed
  in
  let* left = left in
  let* right = right in
  Logic_ir.equal ~span:services.span left right |> services.term_result

type ('source, 'opaque, 'exact, 'error) normalized_services = {
  span : Diagnostic.span;
  normalize :
    'source ->
    ( ('opaque, 'exact)
      Logical_aggregate_term_normalization_private.observation,
      string )
    result;
  translate_boolean : Vir.boolean_term -> (Logic_ir.term, 'error) result;
  translate_exact : 'exact -> (Logic_ir.term, 'error) result;
  translate_opaque : 'opaque -> (Logic_ir.term, 'error) result;
  term_result :
    (Logic_ir.term, Logic_ir.error) result -> (Logic_ir.term, 'error) result;
  malformed : string -> 'error;
}

let translate_normalized services source =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let* observation =
    services.normalize source |> Result.map_error services.malformed
  in
  Logical_aggregate_term_normalization_private.fold
    ~conditional:(fun condition consequent alternative ->
      let* condition = services.translate_boolean condition in
      let* consequent = consequent in
      let* alternative = alternative in
      Logic_ir.ite ~span:services.span condition ~then_:consequent
        ~else_:alternative
      |> services.term_result)
    ~exact:services.translate_exact ~opaque:services.translate_opaque
    observation

let translate_arguments ~integer ~boolean ~aggregate ~parametric arguments =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let* reversed =
    List.fold_left
      (fun result argument ->
        let* terms = result in
        let* term =
          match argument with
          | Vir.Recursive_integer_argument term -> integer term
          | Vir.Recursive_boolean_argument term -> boolean term
          | Vir.Recursive_aggregate_argument term -> aggregate term
          | Vir.Recursive_parametric_argument term -> parametric term
        in
        Ok (term :: terms))
      (Ok []) arguments
  in
  Ok (List.rev reversed)

let normalize = function
  | Ok observation -> observation
  | Error message -> fail "%s" message
let selector_function_name (selector : Vir.selector) =
  let range =
    match selector.selector_range with
    | Vir.Integer -> "int"
    | Vir.Boolean -> "bool"
    | Vir.Aggregate aggregate ->
        Printf.sprintf "agg%d_%s" aggregate.aggregate_type_index
          aggregate.aggregate_type_name
    | Vir.Parametric binder ->
        Printf.sprintf "param_%d_%d"
          binder.Parametric_type.owner.owner_index binder.ordinal
  in
  Aggregate_logic_symbol_private.selector ~range selector
let compare_projection_symbols (left : Vir.symbol) (right : Vir.symbol) =
  let by_id = Int.compare left.symbol_id right.symbol_id in
  if by_id <> 0 then by_id
  else
    let by_name = String.compare left.source_name right.source_name in
    if by_name <> 0 then by_name else Stdlib.compare left.sort right.sort
let normalized_projection symbols =
  let sorted = List.sort compare_projection_symbols symbols in
  let rec deduplicate previous accumulator = function
    | [] -> List.rev accumulator
    | symbol :: rest ->
        if
          match previous with
          | Some previous ->
              Int.equal previous.Vir.symbol_id symbol.Vir.symbol_id
          | None -> false
        then deduplicate previous accumulator rest
        else deduplicate (Some symbol) (symbol :: accumulator) rest
  in
  deduplicate None [] sorted
let create_state requires (obligation : Vir.obligation) =
  {
    builder = Logic_ir.create ();
    span = obligation.span;
    function_index = obligation.function_ref.function_index;
    functions = Hashtbl.create 32;
    rank_domains = Hashtbl.create 4;
    rank_axioms = ref [];
    aggregate_sorts = Hashtbl.create 8;
    parametric_sorts = Parametric_logic_private.create_logic_sort_registry ();
    use_named_aggregate_sorts = List.mem Logic_ir.Named_sorts requires;
    logical_adts = ref None;
    bound_symbols = [];
  }
let compare_aggregate_types (left : Vir.aggregate_type)
    (right : Vir.aggregate_type) =
  let by_index =
    Int.compare left.Vir.aggregate_type_index right.aggregate_type_index
  in
  if by_index <> 0 then by_index
  else Stdlib.compare left right
let obligation_aggregate_types (obligation : Vir.obligation) =
  Vir.obligation_aggregate_types obligation
  @ (Vir.obligation_rank_domains obligation
    |> List.concat_map Vir.rank_domain_component)
  @ (obligation.projection_symbols
    |> List.filter_map (fun (symbol : Vir.symbol) ->
        match symbol.sort with
        | Vir.Aggregate aggregate -> Some aggregate
        | Integer | Boolean | Parametric _ -> None))
  |> List.sort_uniq compare_aggregate_types
let declare_aggregate_sort state aggregate =
  match Hashtbl.find_opt state.aggregate_sorts aggregate with
  | Some _ -> ()
  | None ->
      let sort =
        Logic_ir.declare_sort state.builder
          ~name:
            (Aggregate_logic_symbol_private.named_sort aggregate)
          ~span:state.span
        |> logic_or_fail
      in
      Hashtbl.add state.aggregate_sorts aggregate sort
let logical_adt_schemas (obligation : Vir.obligation) =
  (obligation.assumptions @ obligation.required_preceding_safety)
  |> List.concat_map (function Vir.Logical_adt_schema schemas -> schemas | _ -> [])
let is_logical_adt schemas aggregate =
  List.exists (fun schema ->
      Logical_adt_encoding_private.aggregate_type schema = aggregate) schemas
let initialize_aggregate_sorts state obligation =
  let schemas = logical_adt_schemas obligation in
  if state.use_named_aggregate_sorts then
    obligation_aggregate_types obligation
    |> List.filter (fun aggregate -> not (is_logical_adt schemas aggregate))
    |> List.iter (declare_aggregate_sort state)
let aggregate_sort state aggregate =
  match
    Option.bind !(state.logical_adts)
      (fun adts -> Logical_adt_encoding_private.sort adts aggregate)
  with
  | Some sort -> sort
  | None ->
      if not state.use_named_aggregate_sorts then Logic_ir.Int
      else
        match Hashtbl.find_opt state.aggregate_sorts aggregate with
        | Some sort -> sort
        | None ->
            fail "missing exact aggregate sort %s"
              (Aggregate_logic_symbol_private.type_label aggregate)
let same_owner aggregate (owner : Sst.type_id) =
  Int.equal aggregate.Vir.aggregate_type_index owner.type_index
let parametric_sort state binder =
  Parametric_logic_private.logic_sort state.parametric_sorts
    ~builder:state.builder ~span:state.span binder
let initialize_logical_adts state obligation =
  let schemas = logical_adt_schemas obligation in
  match schemas with
  | [] -> ()
  | _ ->
      state.logical_adts :=
        Some
          (Logical_adt_encoding_private.declare ~builder:state.builder ~schemas
             ~aggregate_sort:(fun aggregate ->
               match Hashtbl.find_opt state.aggregate_sorts aggregate with
               | Some sort -> sort
               | None ->
                   declare_aggregate_sort state aggregate;
                   Option.get
                     (Hashtbl.find_opt state.aggregate_sorts aggregate))
             ~parametric_sort:(parametric_sort state) ~span:state.span
          |> normalize)
let vir_sort state = function
  | Vir.Integer -> Logic_ir.Int
  | Boolean -> Logic_ir.Bool
  | Aggregate aggregate -> aggregate_sort state aggregate
  | Parametric binder -> parametric_sort state binder
let declare state name domain range =
  match Hashtbl.find_opt state.functions name with
  | Some (existing_domain, existing_range, function_)
    when List.for_all2 Logic_ir.sort_equal existing_domain domain
         && Logic_ir.sort_equal existing_range range ->
      function_
  | Some _ -> fail "backend symbol %s is used with incompatible sorts" name
  | None ->
      let function_ =
        Logic_ir.declare_function state.builder ~name ~domain ~range
          ~span:state.span
        |> logic_or_fail
      in
      Hashtbl.add state.functions name (domain, range, function_);
      function_
let logical_adt_function state lookup =
  Option.bind !(state.logical_adts) lookup
let selector_function state selector domain range =
  match logical_adt_function state
          (fun adts -> Logical_adt_encoding_private.selector adts selector) with
  | Some function_ -> function_
  | None -> declare state (selector_function_name selector) [ domain ] range
let constructor_function state aggregate constructor domain =
  match logical_adt_function state (fun adts ->
          Logical_adt_encoding_private.constructor adts aggregate constructor) with
  | Some function_ -> function_
  | None ->
      declare state (Aggregate_logic_symbol_private.constructor aggregate constructor)
        domain (aggregate_sort state aggregate)
let record_constructor_function state aggregate record_type domain =
  match logical_adt_function state (fun adts ->
          Logical_adt_encoding_private.constructor_at adts aggregate 0) with
  | Some function_ -> function_
  | None ->
      declare state (Aggregate_logic_symbol_private.record_constructor
                       aggregate record_type) domain
        (aggregate_sort state aggregate)
let symbol_function state expected_sort (symbol : Vir.symbol) =
  if symbol.sort <> expected_sort then
    fail "symbol %s#%d has sort %s but is used as %s" symbol.source_name
      symbol.symbol_id
      (match symbol.sort with
      | Vir.Integer -> "integer"
      | Boolean -> "Boolean"
      | Aggregate aggregate -> "aggregate " ^ Aggregate_logic_symbol_private.type_label aggregate
      | Parametric binder ->
          "parameter " ^ Parametric_type.binder_to_string binder)
      (match expected_sort with
      | Vir.Integer -> "an integer"
      | Boolean -> "a Boolean"
      | Aggregate aggregate -> "aggregate " ^ Aggregate_logic_symbol_private.type_label aggregate
      | Parametric binder ->
          "parameter " ^ Parametric_type.binder_to_string binder);
  declare state
    (Printf.sprintf "f%d_s%d" state.function_index symbol.symbol_id)
    []
    (vir_sort state expected_sort)
let symbol_term state expected_sort symbol =
  match List.assoc_opt symbol.Vir.symbol_id state.bound_symbols with
  | Some binder ->
      if not (Logic_ir.sort_equal (vir_sort state expected_sort)
                (Logic_ir.View.binder_sort binder))
      then fail "quantifier binder %s#%d is used at the wrong sort"
          symbol.source_name symbol.symbol_id;
      Logic_ir.bound binder
  | None ->
      Logic_ir.apply ~span:state.span
        (symbol_function state expected_sort symbol)
        []
      |> logic_or_fail
let member_name aggregate =
  Printf.sprintf "type_%d" aggregate.Vir.aggregate_type_index
let owner_type domain (constructor : Sst.constructor_id) =
  Vir.rank_domain_component domain
  |> List.find_opt (fun aggregate ->
         aggregate.Vir.aggregate_type_index
         = constructor.constructor_type.type_index)
  |> Option.value
       ~default:
         {
           Vir.aggregate_type_index = constructor.constructor_type.type_index;
           aggregate_type_name = constructor.constructor_type.type_name;
           aggregate_type_arguments = [];
         }
let rank_project (state : state) declared aggregate value =
  Logic_ir.rank_project ~span:state.span declared
    ~member:(member_name aggregate) value
  |> logic_or_fail
let rank_tag state owner value =
  match logical_adt_function state
          (fun adts -> Logical_adt_encoding_private.recognizers adts owner) with
  | None ->
      let function_ =
        declare state (Aggregate_logic_symbol_private.tag owner)
          [ aggregate_sort state owner ] Logic_ir.Int
      in
      Logic_ir.apply ~span:state.span function_ [ value ] |> logic_or_fail
  | Some [] ->
      fail "logical ADT %s has no constructor"
        (Aggregate_logic_symbol_private.type_label owner)
  | Some recognizers ->
      let branch (index, recognizer) alternative =
        let recognized =
          Logic_ir.apply ~span:state.span recognizer [ value ] |> logic_or_fail
        in
        Logic_ir.ite ~span:state.span recognized
          ~then_:(Logic_ir.int ~span:state.span (Z.of_int index))
          ~else_:alternative
        |> logic_or_fail
      in
      let reversed = List.rev recognizers in
      let last_index, _ = List.hd reversed in
      List.fold_right branch (List.rev (List.tl reversed))
        (Logic_ir.int ~span:state.span (Z.of_int last_index))
let add_rank_nonnegative_axiom state declared digest_prefix aggregate =
  let binder =
    Logic_ir.bind state.builder
      ~name:(Printf.sprintf "rank_value_t%d" aggregate.Vir.aggregate_type_index)
      ~sort:(aggregate_sort state aggregate)
      ~span:state.span
    |> logic_or_fail
  in
  let projected =
    rank_project state declared aggregate (Logic_ir.bound binder)
  in
  let body =
    Logic_ir.greater_or_equal ~span:state.span projected
      (Logic_ir.int ~span:state.span Z.zero)
    |> logic_or_fail
  in
  let qid =
    Printf.sprintf "verocaml.rank.%s.t%d.nonnegative" digest_prefix
      aggregate.aggregate_type_index
  in
  let axiom =
    Logic_ir.forall state.builder ~binders:[ binder ] ~body
      ~patterns:[ [ projected ] ] ~qid ~skid:(qid ^ ".skolem") ~span:state.span
    |> logic_or_fail
  in
  state.rank_axioms := axiom :: !(state.rank_axioms)
let add_constructor_nonnegative_axiom state declared domain digest_prefix
    constructor =
  let owner = owner_type domain constructor in
  let binder =
    Logic_ir.bind state.builder
      ~name:
        (Printf.sprintf "rank_parent_t%d_c%d"
           constructor.Sst.constructor_type.type_index
           constructor.constructor_index)
      ~sort:(aggregate_sort state owner)
      ~span:state.span
    |> logic_or_fail
  in
  let value = Logic_ir.bound binder in
  let tag = rank_tag state owner value in
  let is_constructor =
    Logic_ir.equal ~span:state.span tag
      (Logic_ir.int ~span:state.span (Z.of_int constructor.constructor_index))
    |> logic_or_fail
  in
  let projected = rank_project state declared owner value in
  let nonnegative =
    Logic_ir.greater_or_equal ~span:state.span projected
      (Logic_ir.int ~span:state.span Z.zero)
    |> logic_or_fail
  in
  let body =
    Logic_ir.implies ~span:state.span is_constructor nonnegative
    |> logic_or_fail
  in
  let qid =
    Printf.sprintf "verocaml.rank.%s.t%d.c%d.nonnegative" digest_prefix
      constructor.constructor_type.type_index constructor.constructor_index
  in
  let axiom =
    Logic_ir.forall state.builder ~binders:[ binder ] ~body
      ~patterns:[ [ projected ] ]
      ~qid ~skid:(qid ^ ".skolem") ~span:state.span
    |> logic_or_fail
  in
  state.rank_axioms := axiom :: !(state.rank_axioms)
let child_selector domain constructor field child_path child_type =
  let owner = owner_type domain constructor in
  {
    Vir.selector_domain = owner;
    selector_range = Vir.Aggregate child_type;
    selector_namespace =
      Printf.sprintf "t%d_%s_c%d_%s" constructor.Sst.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name;
    selector_index = field.Sst.field_index;
    selector_name = Printf.sprintf "$arg%d" field.field_index;
    selector_path = child_path;
  }
let add_child_smaller_axiom state declared domain digest_prefix fact_index
    constructor field child_path child_type =
  let owner = owner_type domain constructor in
  let binder =
    Logic_ir.bind state.builder
      ~name:
        (Printf.sprintf "rank_parent_t%d_c%d_f%d_%d"
           constructor.Sst.constructor_type.type_index
           constructor.constructor_index field.Sst.field_index fact_index)
      ~sort:(aggregate_sort state owner)
      ~span:state.span
    |> logic_or_fail
  in
  let value = Logic_ir.bound binder in
  let tag = rank_tag state owner value in
  let is_constructor =
    Logic_ir.equal ~span:state.span tag
      (Logic_ir.int ~span:state.span (Z.of_int constructor.constructor_index))
    |> logic_or_fail
  in
  let selector = child_selector domain constructor field child_path child_type in
  let child_function =
    selector_function state selector (aggregate_sort state owner)
      (aggregate_sort state child_type)
  in
  let child =
    Logic_ir.apply ~span:state.span child_function [ value ] |> logic_or_fail
  in
  let parent_rank = rank_project state declared owner value in
  let child_rank = rank_project state declared child_type child in
  let smaller =
    Logic_ir.less_than ~span:state.span child_rank parent_rank |> logic_or_fail
  in
  let body =
    Logic_ir.implies ~span:state.span is_constructor smaller |> logic_or_fail
  in
  let qid =
    Printf.sprintf "verocaml.rank.%s.t%d.c%d.f%d.%d.child" digest_prefix
      constructor.constructor_type.type_index constructor.constructor_index
      field.field_index fact_index
  in
  let axiom =
    Logic_ir.forall state.builder ~binders:[ binder ] ~body
      ~patterns:[ [ child ] ]
      ~qid ~skid:(qid ^ ".skolem") ~span:state.span
    |> logic_or_fail
  in
  state.rank_axioms := axiom :: !(state.rank_axioms)
let add_rank_fact state declared domain digest_prefix fact_index = function
  | Vir.Ground_rank_base _ -> ()
  | Vir.Constructor_rank_nonnegative { constructor } ->
      add_constructor_nonnegative_axiom state declared domain digest_prefix
        constructor
  | Vir.Positive_child_rank_smaller
      { constructor; field; child_path; child_type } ->
      add_child_smaller_axiom state declared domain digest_prefix fact_index
        constructor field child_path child_type
let ensure_rank_domain state domain =
  let domain_id = Vir.rank_domain_id domain in
  match Hashtbl.find_opt state.rank_domains domain_id with
  | Some declared -> declared
  | None ->
      let members =
        Vir.rank_domain_component domain
        |> List.map (fun aggregate ->
            (member_name aggregate, aggregate_sort state aggregate))
      in
      let declared =
        Logic_ir.declare_rank_domain state.builder ~domain_id ~members
          ~span:state.span
        |> logic_or_fail
      in
      Hashtbl.add state.rank_domains domain_id declared;
      let digest = Vir.rank_domain_digest domain in
      let digest_prefix = String.sub digest 0 (min 8 (String.length digest)) in
      Vir.rank_domain_component domain
      |> List.iter (add_rank_nonnegative_axiom state declared digest_prefix);
      Vir.rank_domain_facts domain
      |> List.iteri (add_rank_fact state declared domain digest_prefix);
      declared
let rec aggregate (state : state) (term : Vir.aggregate_term) =
  match term.aggregate_desc with
  | Vir.Aggregate_symbol symbol ->
      symbol_term state (Vir.Aggregate term.aggregate_type) symbol
  | Aggregate_imported_model_application _ ->
      let name, arguments =
        match Retained_model_application_private.resolve term with
        | Ok application -> application
        | Error message -> fail "%s" message
      in
      let arguments = List.map (recursive_argument state) arguments in
      let function_ =
        declare state name
          (List.map Logic_ir.term_sort arguments)
          (aggregate_sort state term.aggregate_type)
      in
      Logic_ir.apply ~span:state.span function_ arguments |> logic_or_fail
  | Aggregate_selector (selector, source) ->
      if selector.selector_domain <> source.aggregate_type then
        fail "selector %s domain %s applied to aggregate %s"
          selector.selector_name
          (Aggregate_logic_symbol_private.type_label selector.selector_domain)
          (Aggregate_logic_symbol_private.type_label source.aggregate_type);
      if selector.selector_range <> Vir.Aggregate term.aggregate_type then
        fail "selector %s has non-aggregate range" selector.selector_name;
      Logical_aggregate_term_normalization_private.aggregate_selector selector
        source
      |> normalize
      |> Logical_aggregate_term_normalization_private.fold
           ~conditional:(fun condition consequent alternative ->
             Logic_ir.ite ~span:state.span (boolean state condition)
               ~then_:consequent ~else_:alternative
             |> logic_or_fail)
           ~exact:(aggregate state)
           ~opaque:(fun (source : Vir.aggregate_term) ->
             let function_ =
               selector_function state selector
                 (aggregate_sort state source.aggregate_type)
                 (aggregate_sort state term.aggregate_type)
             in
             Logic_ir.apply ~span:state.span function_
               [ aggregate state source ]
             |> logic_or_fail)
  | Aggregate_recursive_spec_application _ ->
      fail
        "aggregate recursive specification application requires a verified \
         definition query"
  | Aggregate_symbolic_application application ->
      symbolic_application state
        (aggregate_sort state term.aggregate_type)
        application
  | Aggregate_constructor { constructor; arguments } ->
      if not (same_owner term.aggregate_type constructor.constructor_type) then
        fail "aggregate constructor has a mismatched result type";
      let arguments = List.map (recursive_argument state) arguments in
      let function_ =
        constructor_function state term.aggregate_type constructor
          (List.map Logic_ir.term_sort arguments)
      in
      Logic_ir.apply ~span:state.span function_ arguments |> logic_or_fail
  | Aggregate_record { record_type; fields } ->
      if not (same_owner term.aggregate_type record_type) then
        fail "aggregate record has a mismatched result type";
      let fields =
        List.map (fun (_, value) -> recursive_argument state value) fields
      in
      let function_ =
        record_constructor_function state term.aggregate_type record_type
          (List.map Logic_ir.term_sort fields)
      in
      Logic_ir.apply ~span:state.span function_ fields |> logic_or_fail
  | Aggregate_conditional (condition, consequent, alternative) ->
      if
        consequent.aggregate_type <> term.aggregate_type
        || alternative.aggregate_type <> term.aggregate_type
      then fail "aggregate conditional crosses exact result types";
      Logic_ir.ite ~span:state.span (boolean state condition)
        ~then_:(aggregate state consequent)
        ~else_:(aggregate state alternative)
      |> logic_or_fail
and integer (state : state) = function
  | Vir.Integer_constant value -> Logic_ir.int ~span:state.span value
  | Integer_symbol symbol -> symbol_term state Vir.Integer symbol
  | Integer_add (left, right) ->
      Logic_ir.add ~span:state.span (integer state left) (integer state right)
      |> logic_or_fail
  | Integer_subtract (left, right) ->
      Logic_ir.subtract ~span:state.span (integer state left)
        (integer state right)
      |> logic_or_fail
  | Integer_negate value ->
      Logic_ir.negate ~span:state.span (integer state value) |> logic_or_fail
  | Integer_multiply_constant (coefficient, value) ->
      Logic_ir.scale ~span:state.span coefficient (integer state value)
      |> logic_or_fail
  | Integer_absolute_value value ->
      let value = integer state value in
      let negative =
        Logic_ir.less_than ~span:state.span value
          (Logic_ir.int ~span:state.span Z.zero)
        |> logic_or_fail
      in
      let negated = Logic_ir.negate ~span:state.span value |> logic_or_fail in
      Logic_ir.ite ~span:state.span negative ~then_:negated ~else_:value
      |> logic_or_fail
  | Integer_conditional (condition, consequent, alternative) as term
    when Vir_integer_conditional_private.authenticate term ->
      Logic_ir.ite ~span:state.span (boolean state condition)
        ~then_:(integer state consequent)
        ~else_:(integer state alternative)
      |> logic_or_fail
  | Integer_conditional _ ->
      fail "integer conditional lacks private authentication"
  | Aggregate_tag (aggregate_type, source) ->
      if aggregate_type <> source.aggregate_type then
        fail "tag domain %s applied to aggregate %s"
          (Aggregate_logic_symbol_private.type_label aggregate_type)
          (Aggregate_logic_symbol_private.type_label source.aggregate_type);
      Logical_aggregate_term_normalization_private.tag aggregate_type source
      |> normalize
      |> Logical_aggregate_term_normalization_private.fold
           ~conditional:(fun condition consequent alternative ->
             Logic_ir.ite ~span:state.span (boolean state condition)
               ~then_:consequent ~else_:alternative
             |> logic_or_fail)
           ~exact:(integer state)
           ~opaque:(fun (source : Vir.aggregate_term) ->
             rank_tag state aggregate_type (aggregate state source))
  | Integer_selector (selector, source) ->
      scalar_selector state Logic_ir.Int Vir.Integer
        Logical_aggregate_term_normalization_private.integer_selector
        (integer state) selector source
  | Integer_rank_project (domain, source) ->
      let declared = ensure_rank_domain state domain in
      if
        not
          (List.exists
             (( = ) source.aggregate_type)
             (Vir.rank_domain_component domain))
      then
        fail "rank domain %s does not contain aggregate %s"
          (Vir.rank_domain_id domain)
          (Aggregate_logic_symbol_private.type_label source.aggregate_type);
      rank_project state declared source.aggregate_type (aggregate state source)
  | Integer_recursive_spec_application _ ->
      fail
        "recursive specification application requires a verified definition \
         query"
  | Integer_symbolic_application application ->
      symbolic_application state Logic_ir.Int application
and scalar_selector : type exact.
    state ->
    Logic_ir.sort ->
    Vir.sort ->
    (Vir.selector ->
    Vir.aggregate_term ->
    ( ( Vir.aggregate_term,
        exact )
      Logical_aggregate_term_normalization_private.observation,
      string )
    result) ->
    (exact -> Logic_ir.term) ->
    Vir.selector ->
    Vir.aggregate_term ->
    Logic_ir.term =
 fun (state : state) result_sort expected_range normalize_selector exact selector
     source ->
  if selector.Vir.selector_domain <> source.Vir.aggregate_type then
    fail "selector %s domain %s applied to aggregate %s" selector.selector_name
      (Aggregate_logic_symbol_private.type_label selector.selector_domain)
      (Aggregate_logic_symbol_private.type_label source.aggregate_type);
  if selector.selector_range <> expected_range then
    fail "selector %s is used at the wrong scalar range" selector.selector_name;
  normalize_selector selector source
  |> normalize
  |> Logical_aggregate_term_normalization_private.fold
       ~conditional:(fun condition consequent alternative ->
         Logic_ir.ite ~span:state.span (boolean state condition)
           ~then_:consequent ~else_:alternative
         |> logic_or_fail)
       ~exact
       ~opaque:(fun (source : Vir.aggregate_term) ->
         let function_ =
           selector_function state selector
             (aggregate_sort state source.aggregate_type)
             result_sort
         in
         Logic_ir.apply ~span:state.span function_ [ aggregate state source ]
         |> logic_or_fail)
and boolean (state : state) = function
  | Forall_term quantifier -> user_quantifier state true quantifier
  | Exists_term quantifier -> user_quantifier state false quantifier
  | Vir.Boolean_constant value -> Logic_ir.bool ~span:state.span value
  | Logical_adt_schema _ -> Logic_ir.bool ~span:state.span true
  | Boolean_symbol symbol -> symbol_term state Vir.Boolean symbol
  | Boolean_not value ->
      Logic_ir.not_ ~span:state.span (boolean state value) |> logic_or_fail
  | Boolean_and (left, right) ->
      Logic_ir.and_ ~span:state.span [ boolean state left; boolean state right ]
      |> logic_or_fail
  | Boolean_or (left, right) ->
      Logic_ir.or_ ~span:state.span [ boolean state left; boolean state right ]
      |> logic_or_fail
  | Integer_compare (comparison, left, right) ->
      let left = integer state left in
      let right = integer state right in
      (match comparison with
        | Vir.Equal -> Logic_ir.equal ~span:state.span left right
        | Not_equal -> Logic_ir.distinct ~span:state.span left right
        | Less_than -> Logic_ir.less_than ~span:state.span left right
        | Less_or_equal -> Logic_ir.less_or_equal ~span:state.span left right
        | Greater_than -> Logic_ir.greater_than ~span:state.span left right
        | Greater_or_equal ->
            Logic_ir.greater_or_equal ~span:state.span left right)
      |> logic_or_fail
  | Boolean_equal (left, right) ->
      Logic_ir.equal ~span:state.span (boolean state left) (boolean state right)
      |> logic_or_fail
  | Boolean_not_equal (left, right) ->
      Logic_ir.distinct ~span:state.span (boolean state left)
        (boolean state right)
      |> logic_or_fail
  | Boolean_selector (selector, source) ->
      scalar_selector state Logic_ir.Bool Vir.Boolean
        Logical_aggregate_term_normalization_private.boolean_selector
        (boolean state) selector source
  | Parametric_equal (left, right) ->
      if
        Parametric_type.compare_binder left.parametric_sort right.parametric_sort
        <> 0
      then fail "parametric equality crosses named sorts";
      let left = parametric state left
      and right = parametric state right in
      Logic_ir.equal ~span:state.span left right |> logic_or_fail
  | Aggregate_equal (left, right) ->
      if left.aggregate_type <> right.aggregate_type then
        fail "aggregate equality crosses types %s and %s"
          (Aggregate_logic_symbol_private.type_label left.aggregate_type)
          (Aggregate_logic_symbol_private.type_label right.aggregate_type);
      Logical_aggregate_term_normalization_private.equal left right
      |> normalize
      |> Logical_aggregate_term_normalization_private.fold
           ~conditional:(fun condition consequent alternative ->
             Logic_ir.ite ~span:state.span (boolean state condition)
               ~then_:consequent ~else_:alternative
             |> logic_or_fail)
           ~exact:(boolean state)
           ~opaque:(fun (left, right) ->
             Logic_ir.equal ~span:state.span (aggregate state left)
               (aggregate state right)
             |> logic_or_fail)
  | Boolean_invariant_application { invariant_id; value; _ } ->
      let function_ =
        declare state
          ("verocaml_invariant_" ^ Digest.to_hex (Digest.string invariant_id))
          [ aggregate_sort state value.aggregate_type ]
          Logic_ir.Bool
      in
      Logic_ir.apply ~span:state.span function_ [ aggregate state value ]
      |> logic_or_fail
  | Boolean_recursive_spec_application _ ->
      fail
        "recursive specification application requires a verified definition \
         query"
  | Boolean_specification_application
      { callee; type_arguments; arguments; span } ->
      let domain = List.map (recursive_argument_sort state) arguments in
      let function_ =
        declare state
          (Vir.specification_application_name callee type_arguments arguments)
          domain Logic_ir.Bool
      in
      Logic_ir.apply ~span function_
        (List.map (recursive_argument state) arguments)
      |> logic_or_fail
  | Boolean_symbolic_application application ->
      symbolic_application state Logic_ir.Bool application
  | Callback_requires application ->
      callback_relation state
        (Sst_callback_private.requires_relation application.callback)
        application.arguments
  | Callback_ensures { application; result } ->
      callback_relation state
        (Sst_callback_private.ensures_relation application.callback)
        (application.arguments @ [ result ])
and user_quantifier state universal quantifier =
  let schema = quantifier.Vir.boolean_quantifier_schema in
  let expected_kind =
    if universal then Logic_quantifier_private.Forall
    else Logic_quantifier_private.Exists
  in
  if Logic_quantifier_private.vector_kind schema <> expected_kind then
    fail "VIR user quantifier kind disagrees with authenticated metadata";
  let symbols = quantifier.boolean_quantifier_binders in
  let binders =
    List.map
      (fun symbol ->
        Logic_ir.bind state.builder ~name:symbol.Vir.source_name
          ~sort:(vir_sort state symbol.sort) ~span:symbol.span
        |> logic_or_fail)
      symbols
  in
  let scoped =
    {
      state with
      bound_symbols =
        List.map2
          (fun symbol binder -> (symbol.Vir.symbol_id, binder))
          symbols binders
        @ state.bound_symbols;
    }
  in
  let body = boolean scoped quantifier.boolean_quantifier_body in
  let qid = Logic_quantifier_private.vector_qid schema
  and skid = Logic_quantifier_private.vector_skid schema in
  if universal then
    match quantifier.boolean_quantifier_trigger with
    | None -> fail "VIR universal quantifier lost its explicit trigger"
    | Some trigger ->
        Logic_ir.forall_term state.builder ~binders ~body
          ~trigger:(application scoped trigger) ~qid ~skid
          ~span:(Logic_quantifier_private.vector_span schema)
        |> logic_or_fail
  else
    match quantifier.boolean_quantifier_trigger with
    | Some _ -> fail "VIR existential quantifier acquired a trigger"
    | None ->
        Logic_ir.exists_term state.builder ~binders ~body ~qid ~skid
          ~span:(Logic_quantifier_private.vector_span schema)
        |> logic_or_fail
and recursive_argument state = function
  | Vir.Recursive_integer_argument term -> integer state term
  | Vir.Recursive_boolean_argument term -> boolean state term
  | Vir.Recursive_aggregate_argument term -> aggregate state term
  | Vir.Recursive_parametric_argument term ->
      parametric state term
and parametric state term =
  Parametric_logic_private.validate term |> normalize;
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbolic_application application ->
      symbolic_application state
        (parametric_sort state term.parametric_sort)
        application
  | Parametric_conditional _
    when Spec_function_type_private.is_function_binder term.parametric_sort ->
      Parametric_logic_private.fold_function term
        ~symbol:(fun symbol -> symbol_term state symbol.sort symbol)
        ~conditional:(fun condition consequent alternative ->
          Logic_ir.ite ~span:state.span (boolean state condition)
            ~then_:consequent ~else_:alternative |> logic_or_fail)
        ~application:
          (symbolic_application state
             (parametric_sort state term.parametric_sort))
  | Parametric_symbol _ | Parametric_selector _ | Parametric_conditional _ ->
      Parametric_logic_private.fold term
        ~symbol:(fun symbol -> symbol_term state symbol.sort symbol)
        ~selector:(fun selector source ->
          let binder = term.parametric_sort in
          let function_ =
            selector_function state selector
              (aggregate_sort state source.aggregate_type)
              (parametric_sort state binder)
          in
          Logic_ir.apply ~span:state.span function_ [ aggregate state source ]
          |> logic_or_fail)
        ~conditional:(fun condition consequent alternative ->
          Logic_ir.ite ~span:state.span (boolean state condition)
            ~then_:consequent ~else_:alternative |> logic_or_fail)
        ~application:(fun binder application ->
          symbolic_application state (parametric_sort state binder) application)
and application state = function
  | Vir.Integer_application term -> integer state term
  | Boolean_application term -> boolean state term
  | Aggregate_application term -> aggregate state term
  | Parametric_application term -> parametric state term
and recursive_argument_sort state = function
  | Vir.Recursive_integer_argument _ -> Logic_ir.Int
  | Vir.Recursive_boolean_argument _ -> Logic_ir.Bool
  | Vir.Recursive_aggregate_argument term -> aggregate_sort state term.aggregate_type
  | Vir.Recursive_parametric_argument term -> parametric_sort state term.parametric_sort
and symbolic_application state range application =
  let arguments =
    Symbolic_application_private.arguments application
  in
  let domain = List.map (recursive_argument_sort state) arguments in
  let function_ =
    declare state
      (Symbolic_application_private.symbol_name application)
      domain range
  in
  Logic_ir.apply
    ~span:(Symbolic_application_private.span application)
    function_ (List.map (recursive_argument state) arguments)
  |> logic_or_fail
and callback_relation state name arguments =
  let domain = List.map (recursive_argument_sort state) arguments in
  let function_ = declare state name domain Logic_ir.Bool in
  Logic_ir.apply ~span:state.span function_ (List.map (recursive_argument state) arguments)
  |> logic_or_fail
let assemble_translation state requires (obligation : Vir.obligation) =
  let assertions =
    List.map (boolean state) obligation.assumptions
    @ List.map (boolean state) obligation.required_preceding_safety
    @ List.map (boolean state) obligation.path_condition
    @ [
        Logic_ir.not_ ~span:state.span (boolean state obligation.goal)
        |> logic_or_fail;
      ]
  in
  let projected =
    normalized_projection obligation.projection_symbols
    |> List.map (fun (symbol : Vir.symbol) ->
        (symbol, symbol_function state symbol.sort symbol))
  in
  let query =
    Logic_ir.query state.builder
      ~axioms:(List.rev !(state.rank_axioms))
      ~assertions
      ~requires:(Logic_ir.Models :: requires)
      ~span:state.span
    |> logic_or_fail
  in
  { query; projected }
let translate ~requires obligation =
  try
    let state = create_state requires obligation in
    initialize_aggregate_sorts state obligation;
    initialize_logical_adts state obligation;
    Ok (assemble_translation state requires obligation)
  with
  | Translation_error message -> Error message
  | exn -> Error (Printexc.to_string exn)
