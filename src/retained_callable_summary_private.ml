let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let rec summary_value span typ =
  let expression_desc =
    match typ with
    | Sst.Unit -> Ok Sst.Unit_constant
    | Sst.Bool -> Ok (Sst.Bool_constant false)
    | Sst.Int -> Ok (Sst.Int_constant Z.zero)
    | Sst.Tuple components ->
        let rec loop values = function
          | [] -> Ok (Sst.Tuple_value (List.rev values))
          | (label, typ) :: rest ->
              let* value = summary_value span typ in
              loop ((label, value) :: values) rest
        in
        loop [] components
    | Sst.Aggregate _ ->
        Error
          "[VERO_DEPENDENCY] retained callable ABI rejects aggregate result \
           summaries"
    | Sst.Parameter _ | Sst.Application _ ->
        Error
          "[VERO_DEPENDENCY] retained parametric result requires an opaque \
           summary"
  in
  Result.map
    (fun expression_desc -> { Sst.expression_desc; typ; span })
    expression_desc

let opaque_value ~resolved_path ~opaque_binding_id typ span =
  let binding =
    {
      Sst.id = opaque_binding_id;
      name = resolved_path ^ ".opaque-result";
      typ;
      uniqueness = Sst.Definitely_aliased;
      span;
    }
  in
  {
    Sst.expression_desc =
      Sst.Variable { binding; use_uniqueness = Sst.Definitely_aliased };
    typ;
    span;
  }

let rec requires_opaque_result = function
  | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ -> true
  | Sst.Tuple components ->
      List.exists (fun (_, typ) -> requires_opaque_result typ) components
  | Sst.Unit | Sst.Bool | Sst.Int -> false

let obligation_definition ~resolved_path ~opaque_binding_id definition =
  let* expression =
    if requires_opaque_result definition.Sst.result_type then
      Ok
        (opaque_value ~resolved_path ~opaque_binding_id definition.result_type
           definition.span)
    else summary_value definition.span definition.result_type
  in
  let staged stage = { Sst.stage; expression } in
  let* body =
    match definition.body with
    | Sst.Checked_exec { provenance; _ } ->
        Ok (Sst.Checked_exec { body = staged Sst.Runtime; provenance })
    | Sst.Proof_body { provenance; _ } ->
        Ok (Sst.Proof_body { body = staged Sst.Proof_stage; provenance })
    | Sst.Spec_definition _ -> Ok (Sst.Spec_definition (staged Sst.Logical))
    | Sst.Recursive_spec_definition body ->
        Ok (Sst.Spec_definition (staged body.body.stage))
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ ->
        Error
          "[VERO_DEPENDENCY] retained provider lacks a supported checked \
           first-order body"
    | Sst.Symbolic_declaration _ ->
        Error
          "[VERO_DEPENDENCY] symbolic declarations are same-unit only"
  in
  Ok { definition with Sst.body }
