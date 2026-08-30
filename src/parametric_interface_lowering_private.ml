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

let lower_arguments ~lower_expression ~span ~reject ~application ~signature
    ~result_type arguments =
  let formals = Parametric_signature_private.formals signature in
  if List.length formals <> List.length arguments then
    Error (reject Diagnostic.Higher_order_call)
  else
    let rec lower lowered formals arguments =
      match formals, arguments with
      | [], [] -> Ok (List.rev lowered)
      | (_ : Parametric_signature_private.formal) :: formals,
        (argument_label, Arg (argument, _)) :: arguments ->
          if
            (match argument_label with Optional _ -> true | _ -> false)
            && argument.exp_loc.loc_start.pos_cnum < 0
          then
            lower ((label argument_label, None) :: lowered) formals arguments
          else
            let* argument = lower_expression argument in
            lower
              ((label argument_label,
                Some (optional_argument argument_label argument))
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
          Option.map (fun (argument : Sst.expression) -> argument.typ) argument)
        lowered
    in
    let* type_arguments =
      Parametric_signature_private.infer_partial_type_arguments signature
        ~actual_types ~actual_labels ~actual_result:result_type
      |> Result.map_error (fun _ -> reject Diagnostic.Unsupported_generic_use)
    in
    let* instantiated =
      Parametric_signature_private.instantiate signature type_arguments
      |> Result.map_error (fun _ -> reject Diagnostic.Unsupported_generic_use)
    in
    let arguments =
      List.map2
        (fun (argument_label, argument) typ ->
          match argument with
          | Some argument -> (argument_label, argument)
          | None ->
              ( argument_label,
                { Sst.expression_desc = Sst.Optional_absent;
                  typ;
                  span = span application.exp_loc } ))
        lowered instantiated.parameter_types
    in
    Ok
      ( type_arguments,
        List.map
          (fun (label, value) -> Sst.Value_argument { label; value })
          arguments )

let lower_direct_call ~lower_expression ~span ~reject ~application ~signature
    ~definition ~result_type arguments =
  let* type_arguments, arguments =
    lower_arguments ~lower_expression ~span ~reject ~application ~signature
      ~result_type arguments
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
