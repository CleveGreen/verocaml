open Typedtree

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let label = Parametric_lowering_private.formal_label

let optional_argument label argument =
  match label, argument.Sst.expression_desc with
  | ( Optional _,
      (Sst.Optional_absent | Sst.Optional_present _
      | Sst.Optional_forward _) ) ->
      argument
  | Optional _, _ ->
      { argument with Sst.expression_desc = Sst.Optional_forward argument }
  | (Nolabel | Labelled _ | Position _), _ -> argument

let lower_arguments ~lower_expression ~lower_expression_expected ~adapt_argument
    ~span ~reject ~application ~signature ~result_type arguments =
  let formals = Parametric_signature_private.formals signature in
  if List.length formals <> List.length arguments then
    Error (reject Diagnostic.Higher_order_call)
  else
    let rec lower lowered formals arguments =
      match formals, arguments with
      | [], [] -> Ok (List.rev lowered)
      | (formal : Parametric_signature_private.formal) :: formals,
        (argument_label, Arg (source, _)) :: arguments ->
          if
            (match argument_label with Optional _ -> true | _ -> false)
            && source.exp_loc.loc_start.pos_cnum < 0
          then
            lower ((label argument_label, None) :: lowered) formals arguments
          else
            let* lowered_argument = lower_expression source in
            let* argument =
              adapt_argument source.exp_loc ~expected:formal.typ
                lowered_argument
            in
            lower
              (( label argument_label,
                 Some (optional_argument argument_label argument, source) )
              :: lowered)
              formals arguments
      | (formal : Parametric_signature_private.formal) :: formals,
        (argument_label, Omitted _) :: arguments ->
          (match formal.kind with
          | Parametric_signature_private.Optional_parameter
          | Default_parameter ->
              lower
                ((label argument_label, None) :: lowered)
                formals arguments
          | Positional_parameter | Labelled_parameter ->
              Error (reject Diagnostic.Higher_order_call))
      | [], _ :: _ | _ :: _, [] ->
          Error (reject Diagnostic.Higher_order_call)
    in
    let* lowered = lower [] formals arguments in
    let actual_labels = List.map fst lowered
    and actual_types =
      List.map
        (fun (_, argument) ->
          Option.map
            (fun ((argument : Sst.expression), _) -> argument.typ)
            argument)
        lowered
    in
    let* type_arguments, result_type =
      match
        Parametric_signature_private
        .infer_partial_type_arguments_for_logical_call signature ~actual_types
          ~actual_labels ~actual_result:result_type
      with
      | Ok type_arguments ->
          let* instantiated =
            Parametric_signature_private.instantiate signature type_arguments
            |> Result.map_error (fun _ ->
                   reject Diagnostic.Unsupported_generic_use)
          in
          let* result_type =
            Parametric_lowering_private.reconcile_authenticated_result ~binders:[]
              ~semantic:instantiated.result_type ~compiler:result_type
            |> Result.map_error (fun _ ->
                   reject Diagnostic.Unsupported_generic_use)
          in
          [%log.trace "selected retained generic call inference strategy"
            ~stage:
              (Delator.Field.string "retained-call-argument-reconciliation")
            ~inference_strategy:(Delator.Field.string "semantic-arguments")
            ~decision:(Delator.Field.string "selected")];
          Ok (type_arguments, result_type)
      | Error _ ->
          let* type_arguments =
            Parametric_signature_private.infer_partial_type_arguments signature
              ~actual_types ~actual_labels ~actual_result:result_type
            |> Result.map_error (fun _ ->
                   reject Diagnostic.Unsupported_generic_use)
          in
          [%log.trace "selected retained generic call inference strategy"
            ~stage:
              (Delator.Field.string "retained-call-argument-reconciliation")
            ~inference_strategy:
              (Delator.Field.string "compiler-result-fallback")
            ~decision:(Delator.Field.string "selected")];
          Ok (type_arguments, result_type)
    in
    let* instantiated =
      Parametric_signature_private.instantiate signature type_arguments
      |> Result.map_error (fun _ -> reject Diagnostic.Unsupported_generic_use)
    in
    let* arguments =
      List.fold_left2
        (fun result (argument_label, argument) typ ->
          let* arguments = result in
          match argument with
          | Some (argument, source) ->
              let* initially_adapted =
                adapt_argument application.exp_loc ~expected:typ argument
              in
              let* argument =
                if Parametric_type.equal initially_adapted.Sst.typ typ then
                  Ok initially_adapted
                else lower_expression_expected ~expected:typ source
              in
              Ok ((argument_label, argument) :: arguments)
          | None ->
              Ok
                (( argument_label,
                   { Sst.expression_desc = Sst.Optional_absent;
                     typ;
                     span = span application.exp_loc } )
                :: arguments))
        (Ok []) lowered instantiated.parameter_types
      |> Result.map List.rev
    in
    let[@log_value.trace] integer_adaptation_count =
      List.fold_left2
        (fun count (_, before) (_, (after : Sst.expression)) ->
          match before with
          | Some ((before : Sst.expression), _) ->
              if Parametric_type.equal before.typ after.typ then count
              else count + 1
          | None -> count)
        0 lowered arguments
    in
    [%log.trace "lowered retained generic call arguments"
      ~stage:(Delator.Field.string "retained-call-argument-reconciliation")
      ~argument_count:(Delator.Field.int (List.length arguments))
      ~type_argument_count:(Delator.Field.int (List.length type_arguments))
      ~integer_adaptation_count:
        (Delator.Field.int (integer_adaptation_count [@log_value.trace]))
      ~result_type:
        (Delator.Field.string (Parametric_type.to_string result_type))
      ~decision:(Delator.Field.string "accepted")];
    Ok
      ( type_arguments,
        result_type,
        List.map
          (fun (label, value) -> Sst.Value_argument { label; value })
          arguments )

let lower_direct_call ~lower_expression ~lower_expression_expected ~adapt_argument
    ~span ~reject ~application ~signature ~definition ~result_type arguments =
  let* type_arguments, result_type, arguments =
    lower_arguments ~lower_expression ~lower_expression_expected ~adapt_argument
      ~span ~reject ~application ~signature ~result_type arguments
  in
  Ok
    {
      Sst.expression_desc =
        Sst.Direct_call
          {
            call_form =
              (match definition.Sst.mode with
              | Sst.Exec -> Sst.Exec_call
              | Sst.Proof -> Sst.Proof_call
              | Sst.Spec -> Sst.Specification_call);
            callee = definition.function_id;
            type_arguments;
            arguments;
            recursive = false;
          };
      typ = result_type;
      span = span application.exp_loc;
    }
