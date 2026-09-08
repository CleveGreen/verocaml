type constructor_binding = {
  constructor_index : int;
  constructor_symbol : Logic_ir.function_symbol;
  recognizer_symbol : Logic_ir.function_symbol;
}

type application_binding = {
  aggregate_type : Vir.aggregate_type;
  sort : Logic_ir.sort;
  constructors : constructor_binding list;
  selectors : (Vir.selector * Logic_ir.function_symbol) list;
}

type t = application_binding list

type sort_registry = {
  builder : Logic_ir.builder;
  descriptors : Parametric_adt.t list;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  span : Diagnostic.span;
  aggregate_sorts : (Parametric_type.t, Logic_ir.sort) Hashtbl.t;
}

let ( let* ) result continuation =
  match result with
  | Ok value -> continuation value
  | Error _ as error -> error

let create_sort_registry ~builder ~descriptors ~parametric_sort ~span =
  { builder; descriptors; parametric_sort; span; aggregate_sorts = Hashtbl.create 32 }

let sst_type_of_aggregate descriptors aggregate =
  if aggregate.Vir.aggregate_type_arguments = [] then
    Some
      (Sst.Aggregate
         {
           Sst.type_index = aggregate.aggregate_type_index;
           type_name = aggregate.aggregate_type_name;
         })
  else
    match
      List.find_opt
        (fun descriptor ->
          (Parametric_adt.type_id descriptor).type_index
          = aggregate.aggregate_type_index)
        descriptors
    with
    | Some descriptor ->
        Some
          (Sst.Application
             ( Parametric_adt.type_constructor descriptor,
               aggregate.aggregate_type_arguments ))
    | None -> None

let sst_type registry aggregate =
  match sst_type_of_aggregate registry.descriptors aggregate with
  | Some typ -> typ
  | None -> invalid_arg "unregistered proof aggregate application"

let sort_for_sst registry typ =
  let declare name =
    match Hashtbl.find_opt registry.aggregate_sorts typ with
    | Some sort -> sort
    | None ->
        let sort =
          match
            Logic_ir.declare_sort registry.builder ~name ~span:registry.span
          with
          | Ok sort -> sort
          | Error error -> invalid_arg (Logic_ir.error_to_string error)
        in
        Hashtbl.add registry.aggregate_sorts typ sort;
        sort
  in
  match typ with
  | Sst.Int | Sst.Mathematical_int -> Logic_ir.Int
  | Sst.Bool -> Logic_ir.Bool
  | Sst.Bit_vector width -> Logic_ir.Bv width
  | Sst.Aggregate type_id ->
      declare (Printf.sprintf "RankType_%d_%s" type_id.type_index type_id.type_name)
  | Sst.Parameter binder -> registry.parametric_sort binder
  | Sst.Application _ when Parametric_type.is_spec_function typ ->
      registry.parametric_sort (Spec_function_logic_private.binder typ)
  | Sst.Application (constructor, _) ->
      let descriptor =
        match Parametric_adt.find registry.descriptors constructor with
        | Some descriptor -> descriptor
        | None -> invalid_arg "unregistered parametric ADT sort"
      in
      let digest =
        Parametric_type.to_string typ |> Digest.string |> Digest.to_hex
        |> fun digest -> String.sub digest 0 12
      in
      declare
        (Printf.sprintf "RankType_%d_%s"
           (Parametric_adt.type_id descriptor).type_index digest)
  | Sst.Unit | Sst.Tuple _ ->
      invalid_arg "unsupported recursive specification sort"

let sort_for_aggregate registry aggregate =
  sort_for_sst registry (sst_type registry aggregate)

let parametric_sort registry = registry.parametric_sort

let aggregate_type schema =
  let descriptor = Logical_adt_schema_private.descriptor schema in
  let type_id = Parametric_adt.type_id descriptor in
  let arguments = Logical_adt_schema_private.arguments schema in
  { Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name =
      type_id.type_name ^ "<"
      ^ String.concat "," (List.map Parametric_type.to_string arguments)
      ^ ">";
    aggregate_type_arguments = arguments }

let same_application left right =
  left.Vir.aggregate_type_index = right.Vir.aggregate_type_index
  && left.aggregate_type_arguments = right.aggregate_type_arguments

let sort bindings aggregate =
  List.find_map
    (fun binding ->
      if same_application binding.aggregate_type aggregate then Some binding.sort
      else None)
    bindings

let constructor_binding bindings aggregate index =
  List.find_map
    (fun binding ->
      if not (same_application binding.aggregate_type aggregate) then None
      else
        List.find_opt
          (fun constructor -> constructor.constructor_index = index)
          binding.constructors)
    bindings

let constructor bindings aggregate constructor =
  Option.map (fun binding -> binding.constructor_symbol)
    (constructor_binding bindings aggregate constructor.Sst.constructor_index)

let recognizer bindings aggregate constructor =
  Option.map (fun binding -> binding.recognizer_symbol)
    (constructor_binding bindings aggregate constructor.Sst.constructor_index)

let constructor_at bindings aggregate index =
  Option.map (fun binding -> binding.constructor_symbol)
    (constructor_binding bindings aggregate index)

let recognizers bindings aggregate =
  List.find_map
    (fun binding ->
      if same_application binding.aggregate_type aggregate then
        Some
          (List.map
             (fun constructor ->
               (constructor.constructor_index, constructor.recognizer_symbol))
             binding.constructors)
      else None)
    bindings

let vir_sort schema_for_application typ =
  match typ with
  | Parametric_type.Unit | Bool -> `Primitive Logic_ir.Bool
  | Int | Mathematical_int -> `Primitive Logic_ir.Int
  | Bit_vector width -> `Primitive (Logic_ir.Bv width)
  | Parameter binder -> `Parameter binder
  | Aggregate type_id ->
      `Aggregate
        { Vir.aggregate_type_index = type_id.type_index;
          aggregate_type_name = type_id.type_name;
          aggregate_type_arguments = [] }
  | Application (constructor, arguments) ->
      `Application (schema_for_application constructor arguments)
  | Tuple _ -> `Unsupported

let selector_range schema_for_application typ =
  match vir_sort schema_for_application typ with
  | `Primitive Logic_ir.Int -> Vir.Integer
  | `Primitive Logic_ir.Bool -> Vir.Boolean
  | `Primitive (Logic_ir.Bv width) -> Vir.Bit_vector width
  | `Primitive (Logic_ir.Named _) -> assert false
  | `Parameter binder -> Vir.Parametric binder
  | `Aggregate aggregate -> Vir.Aggregate aggregate
  | `Application (Some schema) -> Vir.Aggregate (aggregate_type schema)
  | `Application None | `Unsupported -> invalid_arg "unsupported ADT field sort"

let selector_name selector =
  let range =
    match selector.Vir.selector_range with
    | Vir.Integer -> "int"
    | Boolean -> "bool"
    | Bit_vector width -> "bv" ^ Bv_width.to_string width
    | Aggregate aggregate ->
        Printf.sprintf "agg%d_%s" aggregate.aggregate_type_index
          aggregate.aggregate_type_name
    | Parametric binder ->
        Printf.sprintf "param_%d_%d" binder.Parametric_type.owner.owner_index
          binder.ordinal
  in
  Aggregate_logic_symbol_private.selector ~range selector

type selector_resolution =
  | Declared_selector of Logic_ir.function_symbol
  | Invalid_declared_selector
  | Unavailable_selector_schema

let selector_equal (left : Vir.selector) (right : Vir.selector) =
  left.selector_domain = right.selector_domain
  && Vir.sort_equal left.selector_range right.selector_range
  && String.equal left.selector_namespace right.selector_namespace
  && Int.equal left.selector_index right.selector_index
  && String.equal left.selector_name right.selector_name
  && left.selector_path = right.selector_path

let resolve_selector bindings selector =
  List.find_map
    (fun binding ->
      if same_application binding.aggregate_type selector.Vir.selector_domain
      then
        Some
          (match
             List.find_opt
               (fun (declared, _) -> selector_equal declared selector)
               binding.selectors
           with
          | Some (_, function_) -> Declared_selector function_
          | None -> Invalid_declared_selector)
      else None)
    bindings
  |> Option.value ~default:Unavailable_selector_schema

let selector bindings selector =
  match resolve_selector bindings selector with
  | Declared_selector function_ -> Some function_
  | Invalid_declared_selector | Unavailable_selector_schema -> None

type record_schema_validation =
  | Validated_record_schema
  | Unavailable_record_schema

let argument_sort = function
  | Vir.Recursive_integer_argument _ -> Vir.Integer
  | Recursive_boolean_argument _ -> Vir.Boolean
  | Recursive_bv_argument term -> Vir.Bit_vector term.bit_vector_width
  | Recursive_aggregate_argument term -> Vir.Aggregate term.aggregate_type
  | Recursive_parametric_argument term -> Vir.Parametric term.parametric_sort

let validate_record_fields ~schemas ~aggregate_type:target_aggregate ~record_type
    ~fields =
  match
    List.find_opt
      (fun schema -> same_application (aggregate_type schema) target_aggregate)
      schemas
  with
  | None -> Ok Unavailable_record_schema
  | Some schema ->
      let declared_aggregate = aggregate_type schema in
      if declared_aggregate <> target_aggregate then
        Error "aggregate record differs from its declared schema identity"
      else
        let descriptor = Logical_adt_schema_private.descriptor schema in
        let declared_type = Parametric_adt.type_id descriptor in
        if declared_type <> record_type then
          Error "aggregate record schema has a mismatched nominal type"
        else (
          match Parametric_adt.kind descriptor with
          | Parametric_adt.Variant _ ->
              Error "aggregate record schema names a variant type"
          | Parametric_adt.Record expected_fields ->
              if List.length fields <> List.length expected_fields then
                Error "aggregate record has an incomplete field vector"
              else
                let arguments = Logical_adt_schema_private.arguments schema in
                List.fold_left2
                  (fun result (field, value) expected ->
                    let* () = result in
                    let expected_id =
                      Sst.
                        { field_owner = Record_owner record_type;
                          field_index = expected.Parametric_adt.field_index;
                          field_name = expected.field_name }
                    in
                    let* expected_type =
                      Parametric_adt.instantiate_field descriptor arguments
                        expected
                    in
                    let expected_sort =
                      selector_range
                        (fun constructor arguments ->
                          List.find_opt
                            (fun candidate ->
                              let nested =
                                Logical_adt_schema_private.descriptor candidate
                              in
                              Parametric_type.compare_constructor
                                (Parametric_adt.type_constructor nested)
                                constructor
                              = 0
                              && List.length
                                   (Logical_adt_schema_private.arguments candidate)
                                 = List.length arguments
                              && List.for_all2 Parametric_type.equal
                                   (Logical_adt_schema_private.arguments candidate)
                                   arguments)
                            schemas)
                        expected_type
                    in
                    if field <> expected_id then
                      Error
                        "aggregate record field metadata/order is not exact"
                    else if
                      not (Vir.sort_equal (argument_sort value) expected_sort)
                    then Error "aggregate record field has a mismatched type/sort"
                    else Ok ())
                  (Ok ()) fields expected_fields
                |> Result.map (fun () -> Validated_record_schema))

type declaration_context = {
  builder : Logic_ir.builder;
  schemas : Logical_adt_schema_private.t list;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  parametric_sort : Parametric_type.binder -> Logic_ir.sort;
  span : Diagnostic.span;
}

let schema_for_application context constructor arguments =
  List.find_opt
    (fun schema ->
      let descriptor = Logical_adt_schema_private.descriptor schema in
      Parametric_type.compare_constructor
        (Parametric_adt.type_constructor descriptor)
        constructor
      = 0
      && List.for_all2 Parametric_type.equal
           (Logical_adt_schema_private.arguments schema)
           arguments)
    context.schemas

let field_sort context bindings schema descriptor arguments field =
  let* typ = Parametric_adt.instantiate_field descriptor arguments field in
  match vir_sort (schema_for_application context) typ with
  | `Primitive sort -> Ok (Logic_ir.Field_sort sort)
  | `Parameter binder ->
      Ok (Logic_ir.Field_sort (context.parametric_sort binder))
  | `Aggregate aggregate ->
      Ok (Logic_ir.Field_sort (context.aggregate_sort aggregate))
  | `Application (Some nested) ->
      if
        Logical_adt_schema_private.application_id nested
        = Logical_adt_schema_private.application_id schema
      then Ok Logic_ir.Recursive_self
      else (
        match sort bindings (aggregate_type nested) with
        | Some sort -> Ok (Logic_ir.Field_sort sort)
        | None -> Error "logical datatype dependency sort is absent")
  | `Application None | `Unsupported ->
      Error "unsupported logical datatype field sort"

let field_spec context bindings schema descriptor arguments aggregate namespace
    name field =
  let* field_sort =
    field_sort context bindings schema descriptor arguments field
  in
  let* typ = Parametric_adt.instantiate_field descriptor arguments field in
  let selector =
    { Vir.selector_domain = aggregate;
      selector_range = selector_range (schema_for_application context) typ;
      selector_namespace = namespace;
      selector_index = field.Parametric_adt.field_index;
      selector_name = name;
      selector_path = [] }
  in
  Ok
    ( { Logic_ir.field_id = field.field_uid;
        field_name = selector_name selector;
        field_sort },
      selector )

let field_specs context bindings schema descriptor arguments aggregate namespace
    name fields =
  let rec collect specs = function
    | [] -> Ok (List.rev specs)
    | field :: rest ->
        let* spec =
          field_spec context bindings schema descriptor arguments aggregate
            namespace (name field) field
        in
        collect (spec :: specs) rest
  in
  collect [] fields

let constructor_spec context bindings schema descriptor arguments aggregate
    type_id (constructor : Parametric_adt.constructor) =
  let index = constructor.constructor_index in
  let name = constructor.constructor_name in
  let namespace =
    Printf.sprintf "t%d_%s_c%d_%s" type_id.Parametric_type.type_index
      type_id.type_name index name
  in
  let* fields =
    field_specs context bindings schema descriptor arguments aggregate namespace
      (fun field -> Printf.sprintf "$arg%d" field.Parametric_adt.field_index)
      constructor.constructor_fields
  in
  let field_specs, selectors = List.split fields in
  let constructor_id =
    { Sst.constructor_type = type_id;
      constructor_index = index;
      constructor_name = name }
  in
  Ok
    ( { Logic_ir.constructor_id = constructor.constructor_uid;
        constructor_name =
          Aggregate_logic_symbol_private.constructor aggregate constructor_id;
        recognizer_id =
          Printf.sprintf "verocaml_is_%s_c%d_%s"
            (Aggregate_logic_symbol_private.suffix aggregate)
            index name;
        fields = field_specs },
      selectors )

let datatype_specs context bindings schema descriptor arguments aggregate =
  let type_id = Parametric_adt.type_id descriptor in
  match Parametric_adt.kind descriptor with
  | Variant constructors ->
      List.fold_left
        (fun result constructor ->
          let* specs = result in
          let* spec =
            constructor_spec context bindings schema descriptor arguments
              aggregate type_id constructor
          in
          Ok (spec :: specs))
        (Ok []) constructors
      |> Result.map List.rev
  | Record fields ->
      let namespace =
        Printf.sprintf "t%d_%s_record" type_id.type_index type_id.type_name
      in
      let* fields =
        field_specs context bindings schema descriptor arguments aggregate
          namespace (fun field -> field.Parametric_adt.field_name) fields
      in
      let field_specs, selectors = List.split fields in
      Ok
        [ ( { Logic_ir.constructor_id =
                "record:" ^ Logical_adt_schema_private.application_id schema;
              constructor_name =
                Aggregate_logic_symbol_private.record_constructor aggregate
                  type_id;
              recognizer_id =
                "verocaml_is_record_"
                ^ Aggregate_logic_symbol_private.suffix aggregate;
              fields = field_specs },
            selectors ) ]

let declare_schema context bindings schema =
  let descriptor = Logical_adt_schema_private.descriptor schema in
  let arguments = Logical_adt_schema_private.arguments schema in
  let aggregate = aggregate_type schema in
  let* specs_and_selectors =
    datatype_specs context bindings schema descriptor arguments aggregate
  in
  let specs, selector_vectors = List.split specs_and_selectors in
  let* datatype =
    Logic_ir.declare_datatype context.builder
      ~datatype_id:(Logical_adt_schema_private.application_id schema)
      ~scc_id:(Logical_adt_schema_private.scc_id schema)
      ~sort_name:(Aggregate_logic_symbol_private.named_sort aggregate)
      ~constructors:specs ~span:context.span
    |> Result.map_error Logic_ir.error_to_string
  in
  let constructors =
    Logic_ir.datatype_constructors datatype
    |> List.mapi (fun index constructor ->
           { constructor_index = index;
             constructor_symbol =
               Logic_ir.datatype_constructor_symbol constructor;
             recognizer_symbol =
               Logic_ir.datatype_recognizer_symbol constructor })
  in
  let selector_symbols =
    Logic_ir.datatype_constructors datatype
    |> List.concat_map (fun constructor ->
           Logic_ir.datatype_fields constructor
           |> List.map Logic_ir.datatype_field_symbol)
  in
  let expected_selectors = List.concat selector_vectors in
  let* selectors =
    if List.length selector_symbols <> List.length expected_selectors then
      Error "logical datatype selector declaration vector is inconsistent"
    else
      List.map2
        (fun selector symbol ->
          if
            String.equal (Logic_ir.View.function_name symbol)
              (selector_name selector)
          then Ok (selector, symbol)
          else Error "logical datatype selector declaration identity changed")
        expected_selectors selector_symbols
      |> List.fold_left
           (fun result item ->
             let* selectors = result in
             let* selector = item in
             Ok (selector :: selectors))
           (Ok [])
      |> Result.map List.rev
  in
  Ok
    ({ aggregate_type = aggregate;
       sort = Logic_ir.datatype_sort datatype;
       constructors;
       selectors }
    :: bindings)

let all_fields descriptor =
  match Parametric_adt.kind descriptor with
  | Parametric_adt.Record fields -> fields
  | Variant constructors ->
      List.concat_map
        (fun constructor -> constructor.Parametric_adt.constructor_fields)
        constructors

let rec ensure context active bindings schema =
  let aggregate = aggregate_type schema in
  match sort bindings aggregate with
  | Some _ -> Ok bindings
  | None ->
      let application_id = Logical_adt_schema_private.application_id schema in
      if List.mem application_id active then
        Error "logical datatype dependency closure is cyclic"
      else
        let descriptor = Logical_adt_schema_private.descriptor schema in
        let arguments = Logical_adt_schema_private.arguments schema in
        let self = Parametric_adt.type_constructor descriptor in
        let ensure_field result field =
          let* bindings = result in
          let* typ =
            Parametric_adt.instantiate_field descriptor arguments field
          in
          match typ with
          | Parametric_type.Application (constructor, nested_arguments)
            when
              Parametric_type.compare_constructor constructor self <> 0
              ||
              (match field.Parametric_adt.field_type with
              | Parametric_type.Application (source_constructor, _) ->
                  Parametric_type.compare_constructor source_constructor self
                  <> 0
              | _ -> true) -> (
              match
                schema_for_application context constructor nested_arguments
              with
              | None ->
                  Error "logical datatype dependency lacks an exact schema"
              | Some nested ->
                  ensure context (application_id :: active) bindings nested)
          | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Parameter _
          | Aggregate _
          | Application _ ->
              Ok bindings
          | Tuple _ -> Error "tuple-valued logical datatype field"
        in
        let* bindings =
          List.fold_left ensure_field (Ok bindings) (all_fields descriptor)
        in
        declare_schema context bindings schema

let declare ~builder ~schemas ~aggregate_sort ~parametric_sort ~span =
  let context = { builder; schemas; aggregate_sort; parametric_sort; span } in
  List.fold_left
    (fun result schema ->
      let* bindings = result in
      ensure context [] bindings schema)
    (Ok []) schemas

let exact_application descriptors = function
  | Parametric_type.Application (constructor, arguments) as application ->
      Option.bind (Parametric_adt.find descriptors constructor) (fun descriptor ->
          if Parametric_adt.same_application descriptor application then
            Some ((Parametric_adt.type_id descriptor).type_index, arguments)
          else None)
  | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _ | Aggregate _
  | Parameter _ ->
      None

let schemas_for_types ~descriptors types =
  Logical_adt_schema_private.instantiate ~descriptors
    ~applications:(List.filter_map (exact_application descriptors) types)
  |> Result.map_error Logical_adt_schema_private.error_to_string

let authenticates_application ~descriptors typ =
  match exact_application descriptors typ with
  | None -> false
  | Some application ->
      Result.is_ok
        (Logical_adt_schema_private.instantiate ~descriptors
           ~applications:[ application ])

let declare_and_install ~builder ~schemas ~aggregate_sort ~parametric_sort
    ~install ~span =
  match schemas with
  | [] -> Ok None
  | _ ->
      let* bindings =
        declare ~builder ~schemas ~aggregate_sort ~parametric_sort ~span
      in
      List.iter
        (fun schema ->
          let aggregate = aggregate_type schema in
          Option.iter (install aggregate) (sort bindings aggregate))
        schemas;
      Ok (Some bindings)

let declare_for_types ~registry ~descriptors ~types ~additional_schemas
    ~aggregate_types =
  let* schemas = schemas_for_types ~descriptors types in
  let schemas = schemas @ additional_schemas in
  List.iter
    (fun aggregate ->
      if
        not
          (List.exists
             (fun schema -> aggregate_type schema = aggregate)
             schemas)
      then
        ignore (sort_for_aggregate registry aggregate))
    aggregate_types;
  declare_and_install ~builder:registry.builder ~schemas
    ~aggregate_sort:(sort_for_aggregate registry)
    ~parametric_sort:registry.parametric_sort
    ~install:(fun aggregate sort ->
      Hashtbl.replace registry.aggregate_sorts (sst_type registry aggregate) sort)
    ~span:registry.span

let schemas (obligation : Vir.obligation) =
  (obligation.assumptions @ obligation.required_preceding_safety)
  |> List.concat_map (function Vir.Logical_adt_schema schemas -> schemas | _ -> [])

let contains schemas aggregate =
  List.exists (fun schema -> aggregate_type schema = aggregate) schemas

let attach_schemas ~descriptors obligation =
  let applications =
    Vir.obligation_aggregate_types obligation
    |> List.filter_map (fun aggregate ->
           match aggregate.Vir.aggregate_type_arguments with
           | [] -> None
           | arguments -> Some (aggregate.aggregate_type_index, arguments))
  in
  Logical_adt_schema_private.instantiate ~descriptors ~applications
  |> Result.map_error Logical_adt_schema_private.error_to_string
  |> Result.map (function
       | [] -> obligation
       | schemas ->
           { obligation with
             Vir.required_preceding_safety =
               Vir.Logical_adt_schema schemas
               :: obligation.required_preceding_safety })

let aggregate_type_of_sst parametric_adts = function
  | Sst.Aggregate type_id ->
      Some
        { Vir.aggregate_type_index = type_id.type_index;
          aggregate_type_name = type_id.type_name;
          aggregate_type_arguments = [] }
  | Sst.Application (constructor, arguments) ->
      Option.map
        (fun descriptor ->
          let type_id = Parametric_adt.type_id descriptor in
          { Vir.aggregate_type_index = type_id.type_index;
            aggregate_type_name =
              Parametric_type.to_string
                (Parametric_type.Application (constructor, arguments));
            aggregate_type_arguments = arguments })
        (Parametric_adt.find parametric_adts constructor)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int
  | Sst.Bit_vector _ | Sst.Tuple _
  | Sst.Parameter _ ->
      None

let vir_sort_of_sst parametric_adts = function
  | Sst.Int | Sst.Mathematical_int -> Some Vir.Integer
  | Sst.Bool -> Some Vir.Boolean
  | Sst.Bit_vector width -> Some (Vir.Bit_vector width)
  | Sst.Parameter binder -> Some (Vir.Parametric binder)
  | (Sst.Aggregate _ | Sst.Application _) as typ ->
      Option.map (fun aggregate -> Vir.Aggregate aggregate)
        (aggregate_type_of_sst parametric_adts typ)
  | Sst.Unit | Sst.Tuple _ -> None

let recognizer_for_sst bindings ~parametric_adts ~owner constructor =
  Option.bind (aggregate_type_of_sst parametric_adts owner) (fun aggregate ->
      recognizer bindings aggregate constructor)

let selector_for_constructor bindings ~parametric_adts ~owner ~constructor
    ~index ~field_name ~field_type =
  Option.bind (aggregate_type_of_sst parametric_adts owner) (fun domain ->
      Option.bind (vir_sort_of_sst parametric_adts field_type) (fun range ->
          selector bindings
            { Vir.selector_domain = domain;
              selector_range = range;
              selector_namespace =
                Printf.sprintf "t%d_%s_c%d_%s"
                  constructor.Sst.constructor_type.type_index
                  constructor.constructor_type.type_name
                  constructor.constructor_index constructor.constructor_name;
              selector_index = index;
              selector_name = field_name;
              selector_path = [] }))

type 'error routing = {
  bindings : t option;
  span : Diagnostic.span;
  aggregate_sort : Vir.aggregate_type -> Logic_ir.sort;
  error : string -> 'error;
  fallback :
    string ->
    Logic_ir.sort list ->
    Logic_ir.sort ->
    Diagnostic.span ->
    (Logic_ir.function_symbol, 'error) result;
}

let routing ~bindings ~span ~aggregate_sort ~error ~fallback =
  { bindings; span; aggregate_sort; error; fallback }

let routed_selector route selector_ domain range =
  let[@log_value.debug] range_identity =
    match selector_.Vir.selector_range with
    | Vir.Bit_vector width -> Bv_width.structural_identity_material width
    | Integer -> "integer"
    | Boolean -> "boolean"
    | Aggregate aggregate ->
        "aggregate:" ^ Aggregate_logic_symbol_private.type_label aggregate
    | Parametric binder ->
        "parametric:" ^ Parametric_type.binder_to_string binder
  in
  match
    Option.map (fun bindings -> resolve_selector bindings selector_) route.bindings
  with
  | Some (Declared_selector function_) ->
      [%log.trace "resolved selector through exact logical datatype schema"
        ~stage:(Delator.Field.string "logical-adt-selector-routing")
        ~selector_index:(Delator.Field.int selector_.Vir.selector_index)
        ~range_identity:
          (Delator.Field.string (range_identity [@log_value.debug]))
        ~decision:(Delator.Field.string "declared")];
      Ok function_
  | Some Invalid_declared_selector ->
      [%log.debug "rejected selector incompatible with declared logical datatype"
        ~stage:(Delator.Field.string "logical-adt-selector-routing")
        ~selector_index:(Delator.Field.int selector_.Vir.selector_index)
        ~range_identity:
          (Delator.Field.string (range_identity [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")];
      Error
        (route.error
           "selector does not match the declared logical datatype schema")
  | None | Some Unavailable_selector_schema ->
      [%log.trace "routed selector through schema-unavailable fallback"
        ~stage:(Delator.Field.string "logical-adt-selector-routing")
        ~selector_index:(Delator.Field.int selector_.Vir.selector_index)
        ~range_identity:
          (Delator.Field.string (range_identity [@log_value.debug]))
        ~decision:(Delator.Field.string "fallback")];
      route.fallback (selector_name selector_) [ domain ] range route.span
[@@delator.instrument] [@@delator.level debug]

let routed_constructor route aggregate constructor_ domain =
  match
    Option.bind route.bindings (fun bindings ->
        constructor bindings aggregate constructor_)
  with
  | Some function_ -> Ok function_
  | None ->
      route.fallback
        (Aggregate_logic_symbol_private.constructor aggregate constructor_)
        domain (route.aggregate_sort aggregate) route.span

let routed_record_constructor route aggregate record_type domain =
  match
    Option.bind route.bindings (fun bindings ->
        constructor_at bindings aggregate 0)
  with
  | Some function_ -> Ok function_
  | None ->
      route.fallback
        (Aggregate_logic_symbol_private.record_constructor aggregate record_type)
        domain (route.aggregate_sort aggregate) route.span

let routed_tag route owner value =
  match Option.bind route.bindings (fun bindings -> recognizers bindings owner) with
  | None ->
      let* function_ =
        route.fallback (Aggregate_logic_symbol_private.tag owner)
          [ route.aggregate_sort owner ] Logic_ir.Int route.span
      in
      Logic_ir.apply ~span:route.span function_ [ value ]
      |> Result.map_error (fun error ->
             route.error (Logic_ir.error_to_string error))
  | Some [] -> Error (route.error "logical datatype has no constructor")
  | Some recognizers ->
      let branch (index, recognizer) alternative =
        let* alternative = alternative in
        let* recognized =
          Logic_ir.apply ~span:route.span recognizer [ value ]
          |> Result.map_error (fun error ->
                 route.error (Logic_ir.error_to_string error))
        in
        Logic_ir.ite ~span:route.span recognized
          ~then_:(Logic_ir.int ~span:route.span (Z.of_int index))
          ~else_:alternative
        |> Result.map_error Logic_ir.error_to_string
        |> Result.map_error route.error
      in
      let reversed = List.rev recognizers in
      let last_index, _ = List.hd reversed in
      List.fold_right branch (List.rev (List.tl reversed))
        (Ok (Logic_ir.int ~span:route.span (Z.of_int last_index)))
