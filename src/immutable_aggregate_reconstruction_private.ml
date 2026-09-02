let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name
let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error
let same_constructor_id (left : Sst.constructor_id) (right : Sst.constructor_id)
    =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name
let same_field_owner left right =
  match (left, right) with
  | Sst.Record_owner left, Sst.Record_owner right -> same_type_id left right
  | Sst.Constructor_owner left, Sst.Constructor_owner right ->
      same_constructor_id left right
  | Sst.Record_owner _, Sst.Constructor_owner _
  | Sst.Constructor_owner _, Sst.Record_owner _ ->
      false
let same_field_id (left : Sst.field_id) (right : Sst.field_id) =
  same_field_owner left.field_owner right.field_owner
  && left.field_index = right.field_index
  && String.equal left.field_name right.field_name
let aggregate_type type_id = Logical_spec_evaluation_private.vir_aggregate_type type_id
let exact_aggregate_type type_id aggregate = aggregate.Vir.aggregate_type = aggregate_type type_id
let definition definitions type_id =
  List.find_opt
    (fun (candidate : Sst.type_definition) ->
      same_type_id candidate.type_id type_id)
    definitions
let deeply_immutable definitions type_id =
  Finite_value_registry.Finite_domain.deeply_immutable_type definitions
    (Sst.Aggregate type_id)

let transparent_schema_embedding descriptors definitions typ =
  let application constructor arguments =
    match Parametric_adt.find descriptors constructor with
    | None -> false
    | Some descriptor ->
        Result.is_ok
          (Logical_adt_schema_private.instantiate ~descriptors
             ~applications:
               [
                 ( (Parametric_adt.type_id descriptor).type_index,
                   arguments );
               ])
  in
  let fields = function
    | Sst.Record_definition fields -> fields
    | Sst.Variant_definition constructors ->
        List.concat_map
          (fun constructor -> constructor.Sst.constructor_fields)
          constructors
  in
  let rec supported visiting = function
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int -> true
    | Sst.Parameter _ -> false
    | Sst.Application (constructor, arguments) ->
        application constructor arguments
    | Sst.Tuple components ->
        List.for_all
          (fun (_, component) -> supported visiting component)
          components
    | Sst.Aggregate type_id when List.exists (same_type_id type_id) visiting ->
        true
    | Sst.Aggregate type_id -> (
        match definition definitions type_id with
        | None -> false
        | Some
            {
              representation =
                Sst.Abstract_with_evidence
                  ( Sst.Incomplete_abstraction_evidence _
                  | Sst.Proposed_same_cmt_abstraction _ );
              _;
            } ->
            false
        | Some definition ->
            fields definition.type_kind
            |> List.for_all (fun field ->
                   field.Sst.field_mutability = Sst.Immutable_field
                   && field.field_modalities.uniqueness_modality
                      <> Sst.Force_aliased
                   && supported (type_id :: visiting) field.field_type))
  in
  supported [] typ

let argument_has_type typ argument =
  match (typ, argument) with
  | Sst.Unit, Vir.Recursive_boolean_argument (Vir.Boolean_constant true) -> true
  | (Sst.Int | Sst.Mathematical_int), Vir.Recursive_integer_argument _ -> true
  | Sst.Bool, Vir.Recursive_boolean_argument _ -> true
  | Sst.Aggregate type_id, Vir.Recursive_aggregate_argument aggregate ->
      exact_aggregate_type type_id aggregate
  | ( Sst.Unit,
      ( Vir.Recursive_integer_argument _ | Vir.Recursive_aggregate_argument _
      | Vir.Recursive_boolean_argument _ ) )
  | ( Sst.Int,
      (Vir.Recursive_boolean_argument _ | Vir.Recursive_aggregate_argument _) )
  | ( Sst.Mathematical_int,
      (Vir.Recursive_boolean_argument _ | Vir.Recursive_aggregate_argument _) )
  | ( Sst.Bool,
      (Vir.Recursive_integer_argument _ | Vir.Recursive_aggregate_argument _) )
  | ( Sst.Aggregate _,
      (Vir.Recursive_integer_argument _ | Vir.Recursive_boolean_argument _) )
  | Sst.Tuple _, _
  | _, Vir.Recursive_parametric_argument _
  | (Sst.Parameter _ | Sst.Application _), _ ->
      false

let selected_argument descriptors aggregate make_selector path = function
  | Sst.Unit ->
      Some (Vir.Recursive_boolean_argument (Vir.Boolean_constant true))
  | Sst.Int | Sst.Mathematical_int ->
      Some
        (Vir.Recursive_integer_argument
           (Vir.Integer_selector (make_selector path Vir.Integer, aggregate)))
  | Sst.Bool ->
      Some
        (Vir.Recursive_boolean_argument
           (Vir.Boolean_selector (make_selector path Vir.Boolean, aggregate)))
  | Sst.Aggregate type_id ->
      let selected_type = aggregate_type type_id in
      Some
        (Vir.Recursive_aggregate_argument
           {
             Vir.aggregate_type = selected_type;
             aggregate_desc =
               Vir.Aggregate_selector
                 (make_selector path (Vir.Aggregate selected_type), aggregate);
           })
  | Sst.Application _ as typ -> (
      match
        Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors
          typ
      with
      | None -> None
      | Some selected_type ->
          Some
            (Vir.Recursive_aggregate_argument
               {
                 Vir.aggregate_type = selected_type;
                 aggregate_desc =
                   Vir.Aggregate_selector
                     ( make_selector path (Vir.Aggregate selected_type),
                       aggregate );
               }))
  | Sst.Tuple _ | Sst.Parameter _ -> None

let record_layout definitions type_id =
  match definition definitions type_id with
  | Some { type_kind = Sst.Record_definition fields; _ } -> Some fields
  | Some { type_kind = Sst.Variant_definition _; _ } | None -> None

let constructor_layout definitions constructor =
  match definition definitions constructor.Sst.constructor_type with
  | Some { type_kind = Sst.Variant_definition constructors; _ } ->
      List.find_opt
        (fun (candidate : Sst.constructor_definition) ->
          same_constructor_id candidate.constructor_id constructor)
        constructors
  | Some { type_kind = Sst.Record_definition _; _ } | None -> None

let record_construction_equality definitions ~record_type ~aggregate ~fields =
  if
    not
      (deeply_immutable definitions record_type
      && exact_aggregate_type record_type aggregate)
  then Ok None
  else
    match record_layout definitions record_type with
    | None -> Error "immutable record construction lost its exact local layout"
    | Some layout ->
        let rec canonical seen ordered = function
          | [] ->
              if List.length seen = List.length fields then
                Ok (List.rev ordered)
              else Error "immutable record construction has extra fields"
          | (field : Sst.field_definition) :: rest -> (
              match
                List.filter
                  (fun (candidate, _) -> same_field_id candidate field.field_id)
                  fields
              with
              | [ (_, value) ] when argument_has_type field.field_type value ->
                  canonical (field.field_id :: seen)
                    ((field.field_id, value) :: ordered)
                    rest
              | [ _ ] ->
                  Error "immutable record construction field changed type"
              | [] ->
                  Error
                    "immutable record construction omitted a canonical field"
              | _ :: _ :: _ ->
                  Error "immutable record construction duplicated a field")
        in
        let* fields = canonical [] [] layout in
        Ok
          (Some
             (Vir.Aggregate_equal
                ( aggregate,
                  {
                    Vir.aggregate_type = aggregate_type record_type;
                    aggregate_desc =
                      Vir.Aggregate_record { record_type; fields };
                  } )))

let constructor_construction_equality definitions ~constructor ~aggregate
    ~arguments =
  let type_id = constructor.Sst.constructor_type in
  if
    not
      (deeply_immutable definitions type_id
      && exact_aggregate_type type_id aggregate)
  then Ok None
  else
    match constructor_layout definitions constructor with
    | None ->
        Error "immutable constructor construction lost its exact local layout"
    | Some layout ->
        if List.length layout.constructor_fields <> List.length arguments then
          Error "immutable constructor construction changed arity"
        else if
          not
            (List.for_all2
               (fun (field : Sst.field_definition) argument ->
                 argument_has_type field.field_type argument)
               layout.constructor_fields arguments)
        then Error "immutable constructor construction argument changed type"
        else
          Ok
            (Some
               (Vir.Aggregate_equal
                  ( aggregate,
                    {
                      Vir.aggregate_type = aggregate_type type_id;
                      aggregate_desc =
                        Vir.Aggregate_constructor { constructor; arguments };
                    } )))

let selected_aggregate aggregate make_selector path type_id =
  let selected_type = aggregate_type type_id in
  {
    Vir.aggregate_type = selected_type;
    aggregate_desc =
      Vir.Aggregate_selector
        (make_selector path (Vir.Aggregate selected_type), aggregate);
  }

let selected_application descriptors aggregate make_selector path typ =
  match
    Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors typ
  with
  | None -> None
  | Some selected_type ->
      Some
        {
          Vir.aggregate_type = selected_type;
          aggregate_desc =
            Vir.Aggregate_selector
              (make_selector path (Vir.Aggregate selected_type), aggregate);
        }

let validate_record_patterns layout patterns =
  let rec loop seen = function
    | [] -> Ok ()
    | (field, (pattern : Sst.pattern)) :: rest -> (
        match
          List.find_opt
            (fun (candidate : Sst.field_definition) ->
              same_field_id candidate.field_id field)
            layout
        with
        | None -> Error "immutable record pattern has a foreign field"
        | Some _ when List.exists (same_field_id field) seen ->
            Error "immutable record pattern duplicated a field"
        | Some definition when definition.field_type <> pattern.typ ->
            Error "immutable record pattern field changed type"
        | Some _ -> loop (field :: seen) rest)
  in
  loop [] patterns

let rec nested_equalities descriptors definitions aggregate make_selector path
    (pattern : Sst.pattern) =
  match pattern.Sst.typ with
  | Sst.Aggregate type_id ->
      reconstruct_aggregate descriptors definitions pattern
        (selected_aggregate aggregate make_selector path type_id)
  | Sst.Application _ as typ -> (
      match selected_application descriptors aggregate make_selector path typ with
      | Some selected ->
          reconstruct_application descriptors definitions pattern selected
      | None -> Error "application reconstruction lost its exact schema")
  | Sst.Tuple components -> (
      match pattern.pattern_desc with
      | Sst.Tuple_pattern patterns
        when List.length components = List.length patterns ->
          let rec loop index facts components patterns =
            match (components, patterns) with
            | [], [] -> Ok (List.rev facts |> List.concat)
            | (_, typ) :: components, (_, (nested : Sst.pattern)) :: patterns
              when typ = nested.Sst.typ ->
                let* nested =
                  nested_equalities descriptors definitions aggregate make_selector
                    (path @ [ index ]) nested
                in
                loop (index + 1) (nested :: facts) components patterns
            | _ -> Error "immutable tuple pattern changed its validated layout"
          in
          loop 0 [] components patterns
      | Sst.Tuple_pattern _ -> Error "immutable tuple pattern changed arity"
      | Sst.Wildcard | Sst.Bind _ -> Ok []
      | Sst.Owned_tree_cursor_pattern _ | Sst.Int_pattern _ | Sst.Bool_pattern _
      | Sst.Unit_pattern | Sst.Record_pattern _ | Sst.Constructor_pattern _
      | Sst.Or_pattern _ ->
          Error "immutable tuple pattern changed shape")
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int -> Ok []
  | Sst.Parameter _ ->
      Error "open reconstruction has no exact schema"

and reconstruct_application descriptors definitions (pattern : Sst.pattern)
    aggregate =
  match pattern.typ with
  | Sst.Application (type_constructor, arguments) as application -> (
      match Parametric_adt.find descriptors type_constructor with
      | Some descriptor
        when Parametric_adt.same_application descriptor application
             && transparent_schema_embedding descriptors definitions application
             && Logical_spec_evaluation_private.vir_aggregate_type_of_sst
                  descriptors application
                = Some aggregate.Vir.aggregate_type -> (
          match (Parametric_adt.kind descriptor, pattern.pattern_desc) with
          | Parametric_adt.Variant constructors,
            Sst.Constructor_pattern (constructor, patterns) -> (
              match
                List.find_opt
                  (fun (candidate : Parametric_adt.constructor) ->
                    candidate.constructor_index = constructor.constructor_index
                    && String.equal candidate.constructor_name
                         constructor.constructor_name
                    && same_type_id constructor.constructor_type
                         (Parametric_adt.type_id descriptor))
                  constructors
              with
              | None ->
                  Error
                    "application constructor pattern lost its exact schema"
              | Some layout ->
                  let* field_types =
                    List.fold_left
                      (fun result field ->
                        let* types = result in
                        let* typ =
                          Parametric_adt.instantiate_field descriptor arguments
                            field
                        in
                        Ok (typ :: types))
                      (Ok []) layout.constructor_fields
                    |> Result.map List.rev
                  in
                  if List.length field_types <> List.length patterns then
                    Error "application constructor pattern changed arity"
                  else if
                    not
                      (List.for_all2
                         (fun typ (nested : Sst.pattern) -> typ = nested.typ)
                         field_types patterns)
                  then Error "application constructor pattern changed type"
                  else
                    let selected =
                      List.mapi
                        (fun index typ ->
                          selected_argument descriptors aggregate
                            (fun path sort ->
                              Logical_spec_evaluation_private.argument_selector
                                constructor index path sort
                              |> fun selector ->
                              {
                                selector with
                                Vir.selector_domain = aggregate.aggregate_type;
                              })
                            [] typ)
                        field_types
                    in
                    if List.exists Option.is_none selected then Ok []
                    else
                      let selected = List.map Option.get selected in
                      let rec nested index facts = function
                        | [] -> Ok (List.rev facts |> List.concat)
                        | nested_pattern :: rest ->
                            let* equalities =
                              nested_equalities descriptors definitions
                                aggregate
                                (fun path sort ->
                                  Logical_spec_evaluation_private.argument_selector
                                    constructor index path sort
                                  |> fun selector ->
                                  {
                                    selector with
                                    Vir.selector_domain =
                                      aggregate.aggregate_type;
                                  })
                                [] nested_pattern
                            in
                            nested (index + 1) (equalities :: facts) rest
                      in
                      let* nested = nested 0 [] patterns in
                      Ok
                        (Vir.Aggregate_equal
                           ( aggregate,
                             {
                               Vir.aggregate_type = aggregate.aggregate_type;
                               aggregate_desc =
                                 Vir.Aggregate_constructor
                                   { constructor; arguments = selected };
                             } )
                        :: nested))
          | Parametric_adt.Record _, Sst.Constructor_pattern _ ->
              Error "application constructor pattern has a foreign owner"
          | Parametric_adt.Record _, Sst.Record_pattern _ -> Ok []
          | (Parametric_adt.Record _ | Parametric_adt.Variant _),
            ( Sst.Wildcard | Sst.Bind _ ) ->
              Ok []
          | (Parametric_adt.Record _ | Parametric_adt.Variant _),
            ( Sst.Owned_tree_cursor_pattern _ | Sst.Int_pattern _
            | Sst.Bool_pattern _ | Sst.Unit_pattern | Sst.Tuple_pattern _
            | Sst.Record_pattern _ | Sst.Or_pattern _ ) ->
              Error "application reconstruction pattern changed shape")
      | Some _ | None ->
          Error "application reconstruction has no exact schema")
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Aggregate _
  | Sst.Parameter _ ->
      Error "application reconstruction received a non-application pattern"

and reconstruct_aggregate descriptors definitions (pattern : Sst.pattern)
    aggregate =
  match pattern.typ with
  | Sst.Aggregate type_id
    when (deeply_immutable definitions type_id
         || transparent_schema_embedding descriptors definitions pattern.typ)
         && exact_aggregate_type type_id aggregate -> (
      match pattern.pattern_desc with
      | Sst.Record_pattern patterns -> (
          match record_layout definitions type_id with
          | None -> Error "immutable record pattern lost its exact local layout"
          | Some layout -> (
              let* () = validate_record_patterns layout patterns in
              let rec fields ordered = function
                | [] -> Ok (Some (List.rev ordered))
                | (field : Sst.field_definition) :: rest -> (
                    match
                      selected_argument descriptors aggregate
                        (Logical_spec_evaluation_private.field_selector
                           field.field_id)
                        [] field.field_type
                    with
                    | None -> Ok None
                    | Some selected ->
                        fields ((field.field_id, selected) :: ordered) rest)
              in
              let* fields = fields [] layout in
              let rec nested facts = function
                | [] -> Ok (List.rev facts |> List.concat)
                | (field : Sst.field_definition) :: rest -> (
                    match
                      List.find_opt
                        (fun (candidate, _) ->
                          same_field_id candidate field.field_id)
                        patterns
                    with
                    | None -> nested facts rest
                    | Some (_, pattern) ->
                        let* equalities =
                          nested_equalities descriptors definitions aggregate
                            (Logical_spec_evaluation_private.field_selector
                               field.field_id)
                            [] pattern
                        in
                        nested (equalities :: facts) rest)
              in
              let* nested = nested [] layout in
              match fields with
              | None -> Ok nested
              | Some fields ->
                  Ok
                    (Vir.Aggregate_equal
                       ( aggregate,
                         {
                           Vir.aggregate_type = aggregate_type type_id;
                           aggregate_desc =
                             Vir.Aggregate_record
                               { record_type = type_id; fields };
                         } )
                    :: nested)))
      | Sst.Constructor_pattern (constructor, patterns)
        when same_type_id constructor.constructor_type type_id -> (
          match constructor_layout definitions constructor with
          | None ->
              Error "immutable constructor pattern lost its exact local layout"
          | Some layout -> (
              if
                match patterns with
                | [ { Sst.pattern_desc = Sst.Record_pattern _; _ } ] -> true
                | _ -> false
              then Ok []
              else if
                List.length layout.constructor_fields <> List.length patterns
              then Error "immutable constructor pattern changed arity"
              else if
                not
                  (List.for_all2
                     (fun (field : Sst.field_definition) (nested : Sst.pattern)
                        -> field.field_type = nested.typ)
                     layout.constructor_fields patterns)
              then Error "immutable constructor pattern argument changed type"
              else
                let rec arguments index ordered = function
                  | [] -> Ok (Some (List.rev ordered))
                  | (field : Sst.field_definition) :: rest -> (
                      match
                        selected_argument descriptors aggregate
                          (Logical_spec_evaluation_private.argument_selector
                             constructor index)
                          [] field.field_type
                      with
                      | None -> Ok None
                      | Some selected ->
                          arguments (index + 1) (selected :: ordered) rest)
                in
                let* arguments = arguments 0 [] layout.constructor_fields in
                let rec nested index facts patterns =
                  match patterns with
                  | [] -> Ok (List.rev facts |> List.concat)
                  | pattern :: rest ->
                      let* equalities =
                        nested_equalities descriptors definitions aggregate
                          (Logical_spec_evaluation_private.argument_selector
                             constructor index)
                          [] pattern
                      in
                      nested (index + 1) (equalities :: facts) rest
                in
                let* nested = nested 0 [] patterns in
                match arguments with
                | None -> Ok nested
                | Some arguments ->
                    Ok
                      (Vir.Aggregate_equal
                         ( aggregate,
                           {
                             Vir.aggregate_type = aggregate_type type_id;
                             aggregate_desc =
                               Vir.Aggregate_constructor
                                 { constructor; arguments };
                           } )
                      :: nested)))
      | Sst.Constructor_pattern _ ->
          Error "immutable constructor pattern has a foreign owner"
      | Sst.Wildcard | Sst.Bind _ -> Ok []
      | Sst.Owned_tree_cursor_pattern _ | Sst.Int_pattern _ | Sst.Bool_pattern _
      | Sst.Unit_pattern | Sst.Tuple_pattern _ | Sst.Or_pattern _ ->
          Error "immutable aggregate pattern changed shape")
  | Sst.Aggregate _ -> Ok []
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Parameter _
  | Sst.Application _ ->
      Error "aggregate reconstruction received a non-aggregate/open pattern"

let pattern_reconstruction_equalities descriptors definitions
    (pattern : Sst.pattern) aggregate =
  match pattern.Sst.typ with
  | Sst.Application _ ->
      reconstruct_application descriptors definitions pattern aggregate
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Aggregate _
  | Sst.Parameter _ ->
      reconstruct_aggregate descriptors definitions pattern aggregate

let rec owned_tree_transitions expression =
  let nested expressions =
    List.concat_map owned_tree_transitions expressions
  in
  match expression.Sst.expression_desc with
  | Sst.Owned_tree_nested_write { transition; value } ->
      transition :: owned_tree_transitions value
  | Sst.Owned_tree_rebase { transition } -> [ transition ]
  | Sst.Lift_runtime_int operand -> owned_tree_transitions operand
  | Sst.Tuple_value components ->
      nested (List.map snd components)
  | Sst.Record_value { fields; _ } -> nested (List.map snd fields)
  | Sst.Constructor_value { arguments; _ } -> nested arguments
  | Sst.Field_read { record; _ } -> owned_tree_transitions record
  | Sst.Field_write { value; transition; _ } ->
      Option.to_list transition @ owned_tree_transitions value
  | Sst.Shared_scalar_field_write { value; _ } ->
      owned_tree_transitions value
  | Sst.Mutable_write { value; _ } -> owned_tree_transitions value
  | Sst.Let_mutable (_, initial, body) ->
      nested [ initial; body ]
  | Sst.Let (bindings, body) ->
      nested (List.map snd bindings @ [ body ])
  | Sst.Sequence (left, right)
  | Sst.Compare (_, left, right)
  | Sst.Boolean_binary (_, left, right) ->
      nested [ left; right ]
  | Sst.If (condition, consequent, alternative) ->
      nested (condition :: consequent :: Option.to_list alternative)
  | Sst.Match (scrutinee, cases) ->
      nested
        (scrutinee
        :: List.concat_map
             (fun case ->
               Option.to_list case.Sst.case_guard @ [ case.case_body ])
             cases)
  | Sst.Checked_arithmetic (_, operands) -> nested operands
  | Sst.Boolean_not operand | Sst.Old operand | Sst.Proof_region operand ->
      owned_tree_transitions operand
  | Sst.Local_assert { predicate; _ } ->
      owned_tree_transitions predicate
  | Sst.Optional_present payload | Sst.Optional_forward payload ->
      owned_tree_transitions payload
  | Sst.Optional_absent -> []
  | Sst.Direct_call { arguments; _ } ->
      nested
        (List.filter_map
           (function
             | Sst.Value_argument { value; _ } -> Some value
             | Sst.Callback_argument _ -> None)
           arguments)
  | Sst.Callback_call { arguments; _ } | Sst.Callback_requires { arguments; _ } ->
      nested (List.map snd arguments)
  | Sst.Callback_ensures { application = { arguments; _ }; result } ->
      nested (List.map snd arguments @ [ result ])
  | Sst.Symbolic_application application ->
      nested (Symbolic_application_private.arguments application)
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ -> []
  | Sst.Use_type_invariant { value; _ } ->
      owned_tree_transitions value
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Forall _ | Sst.Exists _ ->
      []
