let sanitize name =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as character -> character
      | _ -> '_')
    name

let sort_name binder =
  Printf.sprintf "Param_%d_%s_%d" binder.Parametric_type.owner.owner_index
    (sanitize binder.owner.owner_name)
    binder.ordinal

let of_symbol (symbol : Vir.symbol) =
  match symbol.sort with
  | Vir.Parametric binder ->
      Ok
        {
          Vir.parametric_sort = binder;
          parametric_desc = Vir.Parametric_symbol symbol;
        }
  | Vir.Integer | Vir.Boolean | Vir.Bit_vector _ | Vir.Aggregate _ ->
      Error "abstract parameter term requires a parametric symbol"

let validate (term : Vir.parametric_term) =
  let expected = term.parametric_sort in
  let rec loop (term : Vir.parametric_term) =
    if Parametric_type.compare_binder expected term.parametric_sort <> 0 then
      Error "nested abstract term changes named parameter sort"
    else
      match term.parametric_desc with
      | Vir.Parametric_symbol symbol -> (
          match symbol.sort with
          | Vir.Parametric binder
            when Parametric_type.compare_binder binder term.parametric_sort = 0
            ->
              Ok ()
          | Vir.Parametric _ ->
              Error "abstract symbol changes named parameter sort"
          | Vir.Integer | Vir.Boolean | Vir.Bit_vector _ | Vir.Aggregate _ ->
              Error "abstract term contains a non-parametric symbol")
      | Vir.Parametric_selector (selector, source) ->
          if selector.selector_range = Vir.Parametric term.parametric_sort
             && selector.selector_domain = source.aggregate_type
          then Ok ()
          else Error "abstract selector changes named parameter sort or aggregate domain"
      | Vir.Parametric_conditional (_, consequent, alternative) ->
          Result.bind (loop consequent) (fun () -> loop alternative)
      | Vir.Parametric_symbolic_application application -> (
          match Symbolic_application_private.result_type application with
          | Parametric_type.Parameter binder
            when Parametric_type.compare_binder binder term.parametric_sort = 0
            ->
              Ok ()
          | (Application _ as arrow)
            when
              Parametric_type.is_spec_function arrow
              && Spec_function_logic_private.is_binder_for arrow
                   term.parametric_sort ->
              Ok ()
          | Parameter _ | Int | Mathematical_int | Bool | Bit_vector _ | Unit
          | Tuple _
          | Aggregate _
          | Application _ ->
              Error "symbolic application changes named parameter sort")
  in
  loop term

let equal (left : Vir.parametric_term) (right : Vir.parametric_term) =
  Result.bind (validate left) (fun () ->
      Result.bind (validate right) (fun () ->
          if
            Parametric_type.compare_binder left.parametric_sort
              right.parametric_sort
            = 0
          then Ok (Vir.Parametric_equal (left, right))
          else Error "abstract equality crosses named parameter sorts"))

let conditional condition (consequent : Vir.parametric_term)
    (alternative : Vir.parametric_term) =
  Result.bind (validate consequent) (fun () ->
      Result.bind (validate alternative) (fun () ->
          if
            Parametric_type.compare_binder consequent.parametric_sort
              alternative.parametric_sort
            = 0
          then
            Ok
              {
                Vir.parametric_sort = consequent.parametric_sort;
                parametric_desc =
                  Vir.Parametric_conditional (condition, consequent, alternative);
              }
          else Error "abstract conditional crosses named parameter sorts"))

let rec conditions term =
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol _ | Vir.Parametric_selector _
  | Vir.Parametric_symbolic_application _ ->
      []
  | Vir.Parametric_conditional (condition, consequent, alternative) ->
      (condition :: conditions consequent) @ conditions alternative

let fold_conditions_result initial visit left right =
  List.fold_left
    (fun result condition ->
      match result with Ok () -> visit condition | Error _ as error -> error)
    initial
    (conditions left @ conditions right)

let for_all_conditions predicate left right =
  List.for_all predicate (conditions left @ conditions right)

type logic_sort_registry = (string, Logic_ir.sort) Hashtbl.t

let create_logic_sort_registry () = Hashtbl.create 4

let logic_sort registry ~builder ~span binder =
  let name = sort_name binder in
  match Hashtbl.find_opt registry name with
  | Some sort -> sort
  | None ->
      let sort =
        match Logic_ir.declare_sort builder ~name ~span with
        | Ok sort -> sort
        | Error error -> invalid_arg error.Logic_ir.message
      in
      Hashtbl.add registry name sort;
      sort

let symbol_string ~span_string (symbol : Vir.symbol) =
  Printf.sprintf "%d:%s:%s:%s:%s" symbol.symbol_id symbol.source_name
    (match symbol.sort with
    | Vir.Integer -> "int"
    | Vir.Boolean -> "bool"
    | Vir.Bit_vector width -> "bv:" ^ Bv_width.to_string width
    | Vir.Aggregate typ ->
        Printf.sprintf "aggregate:%s#%d" typ.aggregate_type_name
          typ.aggregate_type_index
    | Vir.Parametric binder -> "parametric:" ^ sort_name binder)
    (match symbol.role with
    | Vir.Input -> "input"
    | Local -> "local"
    | Result -> "result"
    | Logical_constant _ -> "logical-constant")
    (span_string symbol.span)

let _parametric_desc_name = function
  | Vir.Parametric_symbol _ -> "symbol"
  | Vir.Parametric_selector _ -> "selector"
  | Vir.Parametric_conditional _ -> "conditional"
  | Vir.Parametric_symbolic_application _ -> "symbolic-application"

let fold_equal ~symbol ~selector ~conditional ~application
    (left : Vir.parametric_term) (right : Vir.parametric_term) =
  [%log.debug "folding parametric equality"
    ~left_sort:(Delator.Field.string (sort_name left.parametric_sort))
    ~right_sort:(Delator.Field.string (sort_name right.parametric_sort))
    ~left_kind:(Delator.Field.string (_parametric_desc_name left.parametric_desc))
    ~right_kind:
      (Delator.Field.string (_parametric_desc_name right.parametric_desc))];
  Result.bind (validate left) (fun () ->
      Result.bind (validate right) (fun () ->
          if
            Parametric_type.compare_binder left.Vir.parametric_sort
              right.parametric_sort
            <> 0
          then Error "parametric equality crosses named sorts"
          else
            let rec translate term =
              [%log.trace "folding parametric equality operand"
                ~sort:
                  (Delator.Field.string (sort_name term.Vir.parametric_sort))
                ~kind:
                  (Delator.Field.string
                     (_parametric_desc_name term.parametric_desc))];
              match term.Vir.parametric_desc with
              | Vir.Parametric_symbol value -> symbol term.parametric_sort value
              | Vir.Parametric_selector (field, source) ->
                  selector term.parametric_sort field source
              | Vir.Parametric_conditional (condition, consequent, alternative)
                ->
                  conditional condition (translate consequent)
                    (translate alternative)
              | Vir.Parametric_symbolic_application value ->
                  application term.parametric_sort value
            in
            Ok (translate left, translate right)))

let rec fold ~symbol ~selector ~conditional ~application term =
  [%log.trace "folding parametric term"
    ~sort:(Delator.Field.string (sort_name term.Vir.parametric_sort))
    ~kind:(Delator.Field.string (_parametric_desc_name term.parametric_desc))];
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol value -> symbol value
  | Vir.Parametric_selector (field, source) -> selector field source
  | Vir.Parametric_conditional (condition, consequent, alternative) ->
      conditional condition
        (fold ~symbol ~selector ~conditional ~application consequent)
        (fold ~symbol ~selector ~conditional ~application alternative)
  | Vir.Parametric_symbolic_application value ->
      application term.parametric_sort value

let rec fold_function ~symbol ~conditional ~application term =
  if not (Spec_function_type_private.is_function_binder term.Vir.parametric_sort)
  then invalid_arg "function fold requires a canonical function binder"
  else
    match term.parametric_desc with
    | Vir.Parametric_symbol value -> symbol value
    | Vir.Parametric_conditional (condition, consequent, alternative) ->
        conditional condition
          (fold_function ~symbol ~conditional ~application consequent)
          (fold_function ~symbol ~conditional ~application alternative)
    | Vir.Parametric_symbolic_application value -> application value
    | Vir.Parametric_selector _ ->
        invalid_arg "function value cannot use aggregate selection"
