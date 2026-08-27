type bindings = {
  sorts : (int * Z3.Sort.sort) list;
  functions : (int * Z3.FuncDecl.func_decl) list;
}

type sort_reference =
  | Int_sort
  | Bool_sort
  | Named_sort of int
  | Recursive_self

type field = {
  field_function_index : int;
  field_function_name : string;
  field_sort : sort_reference;
}

type constructor = {
  constructor_function_index : int;
  constructor_function_name : string;
  recognizer_function_index : int;
  recognizer_function_name : string;
  fields : field list;
}

type declaration = {
  sort_index : int;
  sort_name : string;
  constructors : constructor list;
}

let ( let* ) @ portable = fun result continuation ->
  match result with
  | Ok value -> continuation value
  | Error _ as error -> error

let exact_length @ portable = fun what expected values ->
  if List.length values = expected then Ok values
  else
    Error
      (Printf.sprintf "native datatype %s count mismatch: expected %d, got %d"
         what expected (List.length values))

let mk_integer_sort = Z3.Arithmetic.Integer.mk_sort

let mk_boolean_sort = Z3.Boolean.mk_sort

let mk_symbol = Z3.Symbol.mk_string

let mk_constructor = Z3.Datatype.mk_constructor_s

let mk_sort = Z3.Datatype.mk_sort_s

let get_constructors = Z3.Datatype.get_constructors

let get_recognizers = Z3.Datatype.get_recognizers

let get_accessors = Z3.Datatype.get_accessors

let detach_sort = function
  | Logic_ir.Int -> Int_sort
  | Bool -> Bool_sort
  | Named sort -> Named_sort (Logic_ir.View.named_sort_index sort)

let detach datatype =
  let field field =
    let field_sort =
      match Logic_ir.View.datatype_field_sort field with
      | Logic_ir.Field_sort sort -> detach_sort sort
      | Recursive_self -> Recursive_self
    in
    let symbol = Logic_ir.View.datatype_field_symbol field in
    { field_function_index = Logic_ir.View.function_index symbol;
      field_function_name = Logic_ir.View.function_name symbol;
      field_sort }
  in
  let constructor constructor =
    let constructor_symbol =
      Logic_ir.View.datatype_constructor_symbol constructor
    and recognizer_symbol =
      Logic_ir.View.datatype_recognizer_symbol constructor
    in
    { constructor_function_index =
        Logic_ir.View.function_index constructor_symbol;
      constructor_function_name =
        Logic_ir.View.function_name constructor_symbol;
      recognizer_function_index =
        Logic_ir.View.function_index recognizer_symbol;
      recognizer_function_name =
        Logic_ir.View.function_name recognizer_symbol;
      fields = List.map field (Logic_ir.View.datatype_fields constructor) }
  in
  let sort = Logic_ir.View.datatype_sort datatype in
  { sort_index = Logic_ir.View.named_sort_index sort;
    sort_name = Logic_ir.View.named_sort_name sort;
    constructors =
      List.map constructor (Logic_ir.View.datatype_constructors datatype) }

let declare_detached ~context ~resolve_named_sort datatype =
  let constructors = datatype.constructors in
  let make_constructor constructor =
    let fields = constructor.fields in
    let sorts, references =
      List.split
        (List.map
           (fun field ->
             match field.field_sort with
             | Int_sort -> (Some (mk_integer_sort context), 0)
             | Bool_sort -> (Some (mk_boolean_sort context), 0)
             | Named_sort index -> (Some (resolve_named_sort index), 0)
             | Recursive_self -> (None, 0))
           fields)
    in
    mk_constructor context constructor.constructor_function_name
      (mk_symbol context constructor.recognizer_function_name)
      (List.map
         (fun field -> mk_symbol context field.field_function_name)
         fields)
      sorts references
  in
  try
    let native_constructors = List.map make_constructor constructors in
    let sort = mk_sort context datatype.sort_name native_constructors in
    let backend_constructors = get_constructors sort
    and backend_recognizers = get_recognizers sort
    and backend_accessors = get_accessors sort in
    let expected = List.length constructors in
    let* backend_constructors =
      exact_length "constructor" expected backend_constructors
    in
    let* backend_recognizers =
      exact_length "recognizer" expected backend_recognizers
    in
    let* backend_accessors =
      exact_length "accessor family" expected backend_accessors
    in
    let functions =
      List.map2
        (fun constructor declaration ->
          (constructor.constructor_function_index, declaration))
        constructors backend_constructors
      @ List.map2
          (fun constructor declaration ->
            (constructor.recognizer_function_index, declaration))
          constructors backend_recognizers
      @ List.concat
          (List.map2
             (fun constructor accessors ->
               let fields = constructor.fields in
               if List.length fields <> List.length accessors then
                 invalid_arg "native datatype accessor count mismatch";
               List.map2
                 (fun field declaration ->
                   (field.field_function_index, declaration))
                 fields accessors)
             constructors backend_accessors)
    in
    Ok
      { sorts = [ (datatype.sort_index, sort) ];
        functions }
  with Invalid_argument message -> Error message

let declare ~context ~resolve_sort datatype =
  let detached = detach datatype in
  let resolve_named_sort index =
    resolve_sort
      (Logic_ir.Named
         (List.find
            (fun sort -> Logic_ir.View.named_sort_index sort = index)
            (let fields =
               Logic_ir.View.datatype_constructors datatype
               |> List.concat_map Logic_ir.View.datatype_fields
             in
             List.filter_map
               (fun field ->
                 match Logic_ir.View.datatype_field_sort field with
                 | Logic_ir.Field_sort (Logic_ir.Named sort) -> Some sort
                 | Field_sort (Int | Bool) | Recursive_self -> None)
               fields)))
  in
  declare_detached ~context ~resolve_named_sort detached
