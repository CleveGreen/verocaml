let rec aggregate_contains_recursive_specification
    (aggregate : Vir.aggregate_term) =
  match aggregate.aggregate_desc with
  | Vir.Aggregate_recursive_spec_application _ -> true
  | Vir.Aggregate_imported_model_application _ -> false
  | Vir.Aggregate_symbolic_application application ->
      List.exists argument_contains_recursive_specification
        (Symbolic_application_private.arguments application)
  | Vir.Aggregate_selector (_, source) ->
      aggregate_contains_recursive_specification source
  | Vir.Aggregate_constructor { arguments; _ } ->
      List.exists argument_contains_recursive_specification arguments
  | Vir.Aggregate_record { fields; _ } ->
      List.exists
        (fun (_, argument) ->
          argument_contains_recursive_specification argument)
        fields
  | Vir.Aggregate_conditional (_, consequent, alternative) ->
      aggregate_contains_recursive_specification consequent
      || aggregate_contains_recursive_specification alternative
  | Vir.Aggregate_symbol _ -> false
and argument_contains_recursive_specification argument =
  let contains_recursive_specification =
    Vir.recursive_spec_argument_has_recursive_specification argument
  in
  [%log.trace "classified recursive specification argument"
    ~stage:(Delator.Field.string "recursive-term-analysis")
    ~argument_kind:
      (Delator.Field.string
         (match argument with
         | Vir.Recursive_integer_argument _ -> "integer"
         | Vir.Recursive_boolean_argument _ -> "boolean"
         | Vir.Recursive_aggregate_argument _ -> "aggregate"
         | Vir.Recursive_parametric_argument _ -> "parametric"))
    ~contains_recursive_specification:
      (Delator.Field.bool contains_recursive_specification)];
  contains_recursive_specification

let argument_type ~parametric_adts ~expected = function
  | Vir.Recursive_integer_argument _ ->
      let resolved =
        match expected with
        | Some (Sst.Int | Sst.Mathematical_int as typ) -> typ
        | Some
            ( Sst.Unit | Sst.Bool | Sst.Tuple _ | Sst.Aggregate _
            | Sst.Parameter _ | Sst.Application _ )
        | None ->
            Sst.Int
      in
      [%log.trace "resolved recursive integer argument semantic sort"
        ~stage:(Delator.Field.string "recursive-term-analysis")
        ~expected_sort:
          (Delator.Field.string
             (Option.fold ~none:"absent"
                ~some:Parametric_type.to_string expected))
        ~resolved_sort:
          (Delator.Field.string (Parametric_type.to_string resolved))
        ~decision:
          (Delator.Field.string
             (match expected with
             | Some (Sst.Int | Sst.Mathematical_int) ->
                 "preserved-authenticated-integer-sort"
             | Some _ | None -> "runtime-integer-fallback"))];
      resolved
  | Vir.Recursive_boolean_argument _ -> Sst.Bool
  | Vir.Recursive_aggregate_argument term ->
      let aggregate = term.Vir.aggregate_type in
      Option.value
        ~default:
          (Sst.Aggregate
             {
               Sst.type_index = aggregate.aggregate_type_index;
               type_name = aggregate.aggregate_type_name;
             })
        (Logical_adt_encoding_private.sst_type_of_aggregate parametric_adts
           aggregate)
  | Vir.Recursive_parametric_argument term -> (
      match expected with
      | Some typ
        when Parametric_type.is_spec_function typ
             && Spec_function_logic_private.is_binder_for typ
                  term.Vir.parametric_sort ->
          typ
      | Some _ | None -> Sst.Parameter term.Vir.parametric_sort)

let argument_types ~(program : Sst.program) ~callee ~type_arguments arguments =
  let expected =
    List.find_opt
      (fun (definition : Sst.function_definition) ->
        definition.function_id = callee)
      program.functions
    |> Option.map (fun (definition : Sst.function_definition) ->
           let substitute =
             Parametric_type.substitute
               (List.combine definition.type_binders type_arguments)
           in
           List.map
             (fun parameter ->
               substitute (Sst.require_value_parameter parameter).pattern.typ)
             definition.parameters)
  in
  match expected with
  | Some expected when List.length expected = List.length arguments ->
      List.map2
        (fun typ argument ->
          argument_type ~parametric_adts:program.parametric_adts
            ~expected:(Some typ) argument)
        expected arguments
  | Some _ | None ->
      List.map
        (argument_type ~parametric_adts:program.parametric_adts ~expected:None)
        arguments
