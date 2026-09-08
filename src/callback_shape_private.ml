type label = Unlabelled | Labelled of string

type endpoint = {
  label : label;
  typ : Parametric_type.t;
}

type t = {
  identity : unit ref;
  endpoints : endpoint list;
  result : Parametric_type.t;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let label_to_option = function
  | Unlabelled -> None
  | Labelled label -> Some label

let valid_label = function
  | Unlabelled -> true
  | Labelled label -> not (String.equal label "")

let authenticate_type typ =
  let rec authenticate = function
    | Parametric_type.Unit | Bool | Int | Mathematical_int | Aggregate _
    | Parameter _ ->
        Ok ()
    | Bit_vector width -> (
        match
          Bv_width.authenticate_bound
            (Bv_backend_capability_receipt_private.capability ())
            width
        with
        | Ok () ->
            [%log.trace "authenticated callback bit-vector endpoint"
              ~stage:(Delator.Field.string "callback-shape-validation")
              ~width:(Delator.Field.int (Bv_width.to_int width))
              ~decision:(Delator.Field.string "accepted")];
            Ok ()
        | Error message ->
            [%log.debug "rejected callback bit-vector endpoint"
              ~stage:(Delator.Field.string "callback-shape-validation")
              ~width:(Delator.Field.int (Bv_width.to_int width))
              ~reason:(Delator.Field.string message)
              ~decision:(Delator.Field.string "rejected")];
            Error
              ("callback bit-vector width is not authenticated: " ^ message))
    | Tuple components ->
        List.fold_left
          (fun result (_, component) ->
            Result.bind result (fun () -> authenticate component))
          (Ok ()) components
    | Application (_, arguments) ->
        List.fold_left
          (fun result argument ->
            Result.bind result (fun () -> authenticate argument))
          (Ok ()) arguments
  in
  authenticate typ

let authenticate_types types =
  List.fold_left
    (fun result typ -> Result.bind result (fun () -> authenticate_type typ))
    (Ok ()) types

let create ~endpoints ~result =
  if endpoints = [] then Error "a callback shape requires at least one endpoint"
  else if not (List.for_all (fun (label, _) -> valid_label label) endpoints) then
    Error "a callback endpoint has an empty compiler label"
  else
    Result.bind
      (Result.bind (authenticate_types (List.map snd endpoints)) (fun () ->
           authenticate_type result))
      (fun () ->
        Ok
          {
            identity = ref ();
            endpoints =
              List.map (fun (label, typ) -> { label; typ }) endpoints;
            result;
          })

let endpoints shape =
  List.map (fun endpoint -> (endpoint.label, endpoint.typ)) shape.endpoints

let result shape = shape.result
let arity shape = List.length shape.endpoints
let labels shape = List.map (fun endpoint -> label_to_option endpoint.label) shape.endpoints
let endpoint_types shape = List.map (fun endpoint -> endpoint.typ) shape.endpoints

let order_arguments shape arguments =
  let rec take label before = function
    | [] -> Error "callback application is missing a compiler endpoint"
    | ((candidate, _) as argument) :: rest ->
        if Option.equal String.equal label candidate then
          Ok (argument, List.rev_append before rest)
        else take label (argument :: before) rest
  in
  let rec order ordered remaining = function
    | [] ->
        if remaining = [] then Ok (List.rev ordered)
        else Error "callback application has an extra compiler endpoint"
    | label :: labels ->
        let* argument, remaining = take label [] remaining in
        order (argument :: ordered) remaining labels
  in
  order [] arguments (labels shape)

let instantiate substitutions shape =
  {
    identity = ref ();
    endpoints =
      List.map
        (fun endpoint ->
          {
            endpoint with
            typ = Parametric_type.substitute substitutions endpoint.typ;
          })
        shape.endpoints;
    result = Parametric_type.substitute substitutions shape.result;
  }

let validate_saturated shape ~labels:actual_labels ~arguments ~result =
  let expected_labels = labels shape in
  let expected_arguments = endpoint_types shape in
  let* () = authenticate_types expected_arguments in
  let* () = authenticate_type shape.result in
  let* () = authenticate_types arguments in
  let* () = authenticate_type result in
  if List.length actual_labels <> List.length expected_labels then
    Error
      (Printf.sprintf "callback expects %d endpoint(s), received %d"
         (List.length expected_labels) (List.length actual_labels))
  else if actual_labels <> expected_labels then
    Error "callback labels or compiler endpoint order do not match"
  else if List.length arguments <> List.length expected_arguments then
    Error "callback application is not fully saturated"
  else if
    not
      (List.for_all2 Parametric_type.equal arguments expected_arguments)
  then Error "callback argument types do not match the canonical endpoints"
  else if not (Parametric_type.equal result shape.result) then
    Error "callback application result does not match the final endpoint result"
  else Ok ()

let equal left right =
  List.length left.endpoints = List.length right.endpoints
  && List.for_all2
       (fun left right ->
         left.label = right.label
         && Parametric_type.equal left.typ right.typ)
       left.endpoints right.endpoints
  && Parametric_type.equal left.result right.result

let same_identity left right = left.identity == right.identity

let to_string shape =
  let endpoints =
    shape.endpoints
    |> List.map (fun endpoint ->
           match endpoint.label with
           | Unlabelled -> Parametric_type.to_string endpoint.typ
           | Labelled label ->
               Printf.sprintf "~%s:%s" label
                 (Parametric_type.to_string endpoint.typ))
    |> String.concat " -> "
  in
  Printf.sprintf "%s -> %s" endpoints (Parametric_type.to_string shape.result)

let rec compiler_callback_signature ~first_order typ =
  match Types.get_desc typ with
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      compiler_callback_signature ~first_order body
  | Types.Tarrow ((label, _, _), argument, result, _) ->
      (match label with
      | Types.Nolabel | Types.Labelled _ -> first_order argument
      | Types.Optional _ | Types.Position _ -> false)
      &&
      (first_order result
      || compiler_callback_signature ~first_order result)
  | Types.Tvar _ | Types.Tunivar _ | Types.Tconstr _ | Types.Ttuple _
  | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _
  | Types.Tnil | Types.Tvariant _ | Types.Tpoly _ | Types.Tpackage _
  | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      false

let rec canonical_type typ =
  match Types.get_desc typ with
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      canonical_type body
  | _ -> typ

let compiler_type_has_path typ expected =
  match Types.get_desc typ with
  | Types.Tconstr (path, [], _) -> Path.same path expected
  | _ -> false

let same_location left right =
  left.Location.loc_start.Lexing.pos_cnum
  = right.Location.loc_start.Lexing.pos_cnum
  && left.Location.loc_end.Lexing.pos_cnum
     = right.Location.loc_end.Lexing.pos_cnum
  && String.equal left.Location.loc_start.Lexing.pos_fname
       right.Location.loc_start.Lexing.pos_fname

let source_parameter_patterns value_binding =
  match value_binding.Typedtree.vb_expr.exp_desc with
  | Typedtree.Texp_function
      { params; body = Typedtree.Tfunction_body _; _ } ->
      let rec collect patterns = function
        | [] -> Some (List.rev patterns)
        | {
            Typedtree.fp_partial = Total;
            fp_kind = Tparam_pat pattern;
            _;
          }
          :: rest ->
            collect (pattern :: patterns) rest
        | {
            fp_partial = Total;
            fp_kind = Tparam_optional_default (pattern, _, _);
            _;
          }
          :: rest ->
            collect (pattern :: patterns) rest
        | _ -> None
      in
      collect [] params
  | _ -> None

let source_signature_types value_binding ~definition_body =
  let parameters =
    Option.value ~default:[] (source_parameter_patterns value_binding)
    |> List.map (fun pattern -> pattern.Typedtree.pat_type)
  in
  parameters
  @ Option.to_list
      (Option.map (fun body -> body.Typedtree.exp_type) definition_body)

let has_immediate_callback_actual ~is_callback parameters arguments =
  List.length parameters = List.length arguments
  && List.exists2
       (fun parameter (_, argument) ->
         let callback =
           match parameter.Typedtree.fp_kind with
           | Typedtree.Tparam_pat pattern
           | Typedtree.Tparam_optional_default (pattern, _, _) ->
               is_callback pattern.pat_type
         in
         callback
         &&
         match argument with
         | Typedtree.Arg
             ({ Typedtree.exp_desc = Typedtree.Texp_function _; _ }, _) ->
             true
         | Typedtree.Arg _ | Typedtree.Omitted _ -> false)
       parameters arguments

let expression_compares_type ~resolves_equality ~type_paths expression =
  let found = ref false in
  let local_type typ =
    match Types.get_desc typ with
    | Types.Tconstr (path, _, _) -> List.exists (Path.same path) type_paths
    | Types.Tvar _ | Types.Tunivar _ | Types.Tarrow _ | Types.Ttuple _
    | Types.Tpoly _ | Types.Tlink _ | Types.Tsubst _
    | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _
    | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _
    | Types.Tsplice _ | Types.Tof_kind _ ->
        false
  in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                [ (Nolabel, Arg (left, _)); (Nolabel, Arg (right, _)) ],
                _,
                _,
                _ )
            when
              resolves_equality path
              && (local_type left.exp_type || local_type right.exp_type) ->
              found := true
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  !found
