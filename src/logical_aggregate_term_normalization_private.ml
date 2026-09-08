type ('opaque, 'exact) observation =
  | Opaque of 'opaque
  | Exact of 'exact
  | Conditional of
      Vir.boolean_term
      * ('opaque, 'exact) observation
      * ('opaque, 'exact) observation

let rec fold ~opaque ~exact ~conditional = function
  | Opaque value -> opaque value
  | Exact value -> exact value
  | Conditional (condition, consequent, alternative) ->
      conditional condition
        (fold ~opaque ~exact ~conditional consequent)
        (fold ~opaque ~exact ~conditional alternative)

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let constructor_namespace (constructor : Sst.constructor_id) =
  Printf.sprintf "t%d_%s_c%d_%s" constructor.constructor_type.type_index
    constructor.constructor_type.type_name constructor.constructor_index
    constructor.constructor_name

let validate_constructor_result (term : Vir.aggregate_term)
    (constructor : Sst.constructor_id) =
  if
    term.aggregate_type.aggregate_type_index
    = constructor.constructor_type.type_index
  then Ok ()
  else Error "aggregate constructor has a mismatched result type"

let validate_conditional (term : Vir.aggregate_term)
    (consequent : Vir.aggregate_term) (alternative : Vir.aggregate_term) =
  if
    consequent.Vir.aggregate_type = term.aggregate_type
    && alternative.aggregate_type = term.aggregate_type
  then Ok ()
  else Error "aggregate conditional crosses exact result types"

let argument_sort = function
  | Vir.Recursive_integer_argument _ -> Vir.Integer
  | Vir.Recursive_boolean_argument _ -> Vir.Boolean
  | Vir.Recursive_bv_argument term -> Vir.Bit_vector term.bit_vector_width
  | Vir.Recursive_aggregate_argument term -> Vir.Aggregate term.aggregate_type
  | Vir.Recursive_parametric_argument term -> Vir.Parametric term.parametric_sort

let argument_at index arguments =
  if index < 0 then None else List.nth_opt arguments index

let exact_argument selector source constructor arguments =
  let* () = validate_constructor_result source constructor in
  if
    not
      (String.equal selector.Vir.selector_namespace
         (constructor_namespace constructor))
  then Ok (Opaque source)
  else
    let expected_name = Printf.sprintf "$arg%d" selector.selector_index in
    if not (String.equal selector.selector_name expected_name) then
      Error "positional constructor selector has a mismatched name"
    else
      match argument_at selector.selector_index arguments with
      | None ->
          Error "positional constructor selector index is outside the term"
      | Some argument
        when Vir.sort_equal (argument_sort argument) selector.selector_range ->
          Ok (Exact argument)
      | Some _ ->
          Error "positional constructor selector has a mismatched range"

let field_namespace (field : Sst.field_id) =
  match field.field_owner with
  | Sst.Record_owner type_id ->
      Printf.sprintf "t%d_%s_record" type_id.type_index type_id.type_name
  | Sst.Constructor_owner constructor ->
      Printf.sprintf "t%d_%s_c%d_%s_inline"
        constructor.constructor_type.type_index
        constructor.constructor_type.type_name constructor.constructor_index
        constructor.constructor_name

let exact_field selector source fields =
  match
    List.find_opt
      (fun ((field : Sst.field_id), _) ->
        field.field_index = selector.Vir.selector_index
        && String.equal field.field_name selector.selector_name
        && String.equal (field_namespace field) selector.selector_namespace)
      fields
  with
  | None -> Ok (Opaque source)
  | Some (_, argument)
    when Vir.sort_equal (argument_sort argument) selector.selector_range ->
      Ok (Exact argument)
  | Some _ -> Error "record selector has a mismatched range"

let rec tag observed_type (source : Vir.aggregate_term) =
  if observed_type <> source.aggregate_type then
    Error "aggregate tag has a mismatched domain"
  else
    match source.aggregate_desc with
    | Vir.Aggregate_constructor { constructor; _ } ->
        let* () = validate_constructor_result source constructor in
        Ok
          (Exact
             (Vir.Integer_constant (Z.of_int constructor.constructor_index)))
    | Vir.Aggregate_conditional (condition, consequent, alternative) ->
        let* () = validate_conditional source consequent alternative in
        let* consequent = tag observed_type consequent in
        let* alternative = tag observed_type alternative in
        Ok (Conditional (condition, consequent, alternative))
    | Aggregate_symbol _ | Aggregate_imported_model_application _
    | Aggregate_selector _ | Aggregate_record _
    | Aggregate_recursive_spec_application _
    | Aggregate_symbolic_application _ ->
        Ok (Opaque source)

let rec positional_selector selector (source : Vir.aggregate_term) =
  if selector.Vir.selector_domain <> source.aggregate_type then
    Error "positional constructor selector has a mismatched owner"
  else if selector.selector_path <> [] then Ok (Opaque source)
  else
    match source.aggregate_desc with
    | Vir.Aggregate_constructor { constructor; arguments } ->
        exact_argument selector source constructor arguments
    | Vir.Aggregate_conditional (condition, consequent, alternative) ->
        let* () = validate_conditional source consequent alternative in
        let* consequent = positional_selector selector consequent in
        let* alternative = positional_selector selector alternative in
        (match (consequent, alternative) with
        | Opaque _, Opaque _ -> Ok (Opaque source)
        | _ -> Ok (Conditional (condition, consequent, alternative)))
    | Aggregate_record { fields; _ }
      when source.aggregate_type.aggregate_type_arguments <> [] ->
        exact_field selector source fields
    | Aggregate_selector (outer, parent)
      when source.aggregate_type.aggregate_type_arguments <> [] ->
        let* observed = aggregate_selector outer parent in
        let rec project = function
          | Opaque _ -> Ok (Opaque source)
          | Exact aggregate -> positional_selector selector aggregate
          | Conditional (condition, consequent, alternative) ->
              let* consequent = project consequent in
              let* alternative = project alternative in
              Ok (Conditional (condition, consequent, alternative))
        in
        project observed
    | Aggregate_symbol _ | Aggregate_selector _ | Aggregate_record _
    | Aggregate_imported_model_application _
    | Aggregate_recursive_spec_application _
    | Aggregate_symbolic_application _ ->
        Ok (Opaque source)

and aggregate_selector selector source =
  match selector.Vir.selector_range with
  | Vir.Integer | Vir.Boolean | Vir.Bit_vector _ | Vir.Parametric _ ->
      Error "aggregate selector has a non-aggregate range"
  | Vir.Aggregate expected_type ->
      let* observed = positional_selector selector source in
      let rec project = function
        | Opaque source -> Ok (Opaque source)
        | Conditional (condition, consequent, alternative) ->
            let* consequent = project consequent in
            let* alternative = project alternative in
            Ok (Conditional (condition, consequent, alternative))
        | Exact (Vir.Recursive_aggregate_argument term)
          when term.aggregate_type = expected_type ->
            Ok (Exact term)
        | Exact (Vir.Recursive_aggregate_argument _) ->
            Error "aggregate selector reduced to the wrong nominal type"
        | Exact _ -> Error "aggregate selector reduced to a scalar argument"
      in
      project observed

let integer_selector selector source =
  if selector.Vir.selector_range <> Vir.Integer then
    Error "integer selector has a mismatched range"
  else
    let* observed = positional_selector selector source in
    let rec project = function
      | Opaque source -> Ok (Opaque source)
      | Conditional (condition, consequent, alternative) ->
          let* consequent = project consequent in
          let* alternative = project alternative in
          Ok (Conditional (condition, consequent, alternative))
      | Exact (Vir.Recursive_integer_argument term) -> Ok (Exact term)
      | Exact _ -> Error "integer selector reduced to a non-integer argument"
    in
    project observed

let boolean_selector selector source =
  if selector.Vir.selector_range <> Vir.Boolean then
    Error "Boolean selector has a mismatched range"
  else
    let* observed = positional_selector selector source in
    let rec project = function
      | Opaque source -> Ok (Opaque source)
      | Conditional (condition, consequent, alternative) ->
          let* consequent = project consequent in
          let* alternative = project alternative in
          Ok (Conditional (condition, consequent, alternative))
      | Exact (Vir.Recursive_boolean_argument term) -> Ok (Exact term)
      | Exact _ -> Error "Boolean selector reduced to a non-Boolean argument"
    in
    project observed

let bit_vector_selector width selector source =
  if
    not
      (Vir.sort_equal selector.Vir.selector_range (Vir.Bit_vector width))
  then (
    [%log.debug "refused positional BV selector normalization"
      ~selector_index:(Delator.Field.int selector.selector_index)
      ~width:(Delator.Field.int (Bv_width.to_int width))
      ~reason_class:(Delator.Field.string "declared-width-mismatch")
      ~decision:(Delator.Field.string "rejected")];
    Error "bit-vector selector has a mismatched range" )
  else
    let rec project = function
      | Opaque source ->
          [%log.trace "normalized positional BV selector as opaque"
            ~selector_index:(Delator.Field.int selector.selector_index)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~decision:(Delator.Field.string "opaque")];
          Ok (Opaque source)
      | Conditional (condition, consequent, alternative) ->
          [%log.trace "normalizing positional BV selector conditional"
            ~selector_index:(Delator.Field.int selector.selector_index)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~decision:(Delator.Field.string "conditional")];
          let* consequent = project consequent in
          let* alternative = project alternative in
          Ok (Conditional (condition, consequent, alternative))
      | Exact (Vir.Recursive_bv_argument term)
        when Bv_width.equal term.bit_vector_width width ->
          [%log.trace "normalized positional BV selector exactly"
            ~selector_index:(Delator.Field.int selector.selector_index)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~decision:(Delator.Field.string "exact")];
          Ok (Exact term)
      | Exact (Vir.Recursive_bv_argument _) ->
          [%log.debug "refused positional BV selector normalization"
            ~selector_index:(Delator.Field.int selector.selector_index)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~reason_class:(Delator.Field.string "exact-width-mismatch")
            ~decision:(Delator.Field.string "rejected")];
          Error "bit-vector selector reduced to the wrong width"
      | Exact _ ->
          [%log.debug "refused positional BV selector normalization"
            ~selector_index:(Delator.Field.int selector.selector_index)
            ~width:(Delator.Field.int (Bv_width.to_int width))
            ~reason_class:(Delator.Field.string "non-bv-argument")
            ~decision:(Delator.Field.string "rejected")];
          Error "bit-vector selector reduced to a non-bit-vector argument"
    in
    (match positional_selector selector source with
    | Ok observed -> project observed
    | Error message ->
        [%log.debug "refused positional BV selector normalization"
          ~selector_index:(Delator.Field.int selector.selector_index)
          ~width:(Delator.Field.int (Bv_width.to_int width))
          ~reason_class:(Delator.Field.string "positional-owner-refusal")
          ~decision:(Delator.Field.string "rejected")];
        Error message)

let rec boolean_term = function
  | Exact term -> term
  | Opaque (left, right) -> Vir.Aggregate_equal (left, right)
  | Conditional (condition, consequent, alternative) ->
      Vir.Boolean_or
        ( Vir.Boolean_and (condition, boolean_term consequent),
          Vir.Boolean_and
            (Vir.Boolean_not condition, boolean_term alternative) )

let rec argument_equal left right =
  match (left, right) with
  | Vir.Recursive_integer_argument left, Vir.Recursive_integer_argument right ->
      Ok (Vir.Integer_compare (Vir.Equal, left, right))
  | Vir.Recursive_boolean_argument left, Vir.Recursive_boolean_argument right ->
      Ok (Vir.Boolean_equal (left, right))
  | Vir.Recursive_aggregate_argument left, Vir.Recursive_aggregate_argument right ->
      let* equal = equal left right in
      Ok (boolean_term equal)
  | Vir.Recursive_parametric_argument left, Vir.Recursive_parametric_argument right ->
      Parametric_logic_private.equal left right
  | _ -> Error "same-constructor arguments have mismatched sorts"

and equal_arguments left right =
  if List.length left <> List.length right then
    Error "same constructor is applied with inconsistent argument counts"
  else
    let* reversed =
      List.fold_left2
        (fun result left right ->
          let* terms = result in
          let* term = argument_equal left right in
          Ok (term :: terms))
        (Ok []) left right
    in
    Ok
      (List.fold_left
         (fun combined term -> Vir.Boolean_and (combined, term))
         (Vir.Boolean_constant true) (List.rev reversed))

and equal (left : Vir.aggregate_term) (right : Vir.aggregate_term) =
  if left.aggregate_type <> right.aggregate_type then
    Error "aggregate equality crosses exact types"
  else
    match (left.aggregate_desc, right.aggregate_desc) with
    | Aggregate_conditional (condition, consequent, alternative), _ ->
        let* () = validate_conditional left consequent alternative in
        let* consequent = equal consequent right in
        let* alternative = equal alternative right in
        Ok (Conditional (condition, consequent, alternative))
    | _, Aggregate_conditional (condition, consequent, alternative) ->
        let* () = validate_conditional right consequent alternative in
        let* consequent = equal left consequent in
        let* alternative = equal left alternative in
        Ok (Conditional (condition, consequent, alternative))
    | Aggregate_constructor
        { constructor = left_constructor; arguments = left_arguments },
      Aggregate_constructor
        { constructor = right_constructor; arguments = right_arguments } ->
        let* () = validate_constructor_result left left_constructor in
        let* () = validate_constructor_result right right_constructor in
        if
          left_constructor.constructor_index
          <> right_constructor.constructor_index
        then Ok (Exact (Vir.Boolean_constant false))
        else if
          not
            (String.equal left_constructor.constructor_name
               right_constructor.constructor_name)
        then Error "constructor index is paired with conflicting names"
        else
          let* equality = equal_arguments left_arguments right_arguments in
          Ok (Exact equality)
    | _ -> Ok (Opaque (left, right))
