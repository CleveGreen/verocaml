type t = Exact_carrier | Different_carrier | Unsupported
type endpoint = Carrier | Mathematical of string | Runtime_integer | Boolean | Unit
type shape = { parameters : endpoint list; result : endpoint }

let rec unwrap typ =
  match Types.get_desc typ with
  | Types.Tpoly (body, []) | Tlink body | Tsubst (body, _) -> unwrap body
  | _ -> typ

let shape ~constructor_uid:(constructor_uid [@delator.skip])
    ~logical_sorts:(logical_sorts [@delator.skip]) ~carrier_uid
    ~carrier:(carrier [@delator.skip]) (typ [@delator.skip]) =
  let endpoint typ = match Types.get_desc (unwrap typ) with
    | Types.Tconstr (path, [], _) ->
        (match constructor_uid path with
        | Some uid when String.equal uid carrier_uid -> Some Carrier
        | Some uid ->
            (match List.filter (fun sort -> String.equal sort.Logical_sort_private.type_uid uid) logical_sorts with
            | [sort] -> Some (Mathematical (Logical_sort_private.canonical_material sort))
            | _ when Path.same path Predef.path_int -> Some Runtime_integer
            | _ when Path.same path Predef.path_bool -> Some Boolean
            | _ when Path.same path Predef.path_unit -> Some Unit
            | _ -> None)
        | None -> None)
    | _ -> None in
  let rec spine parameters typ = match Types.get_desc (unwrap typ) with
    | Types.Tarrow ((Types.Nolabel, _, _), domain, range, _) ->
        Option.bind (endpoint domain) (fun domain -> spine (domain :: parameters) range)
    | _ -> Option.map (fun result -> {parameters = List.rev parameters; result}) (endpoint typ) in
  let result = if carrier.Types.type_params <> []
    || List.exists (fun sort -> String.equal sort.Logical_sort_private.type_uid carrier_uid) logical_sorts then None
  else spine [] typ in
  [%log.trace "reconstructed numeric callable endpoint shape"
    ~carrier_uid:(Delator.Field.string carrier_uid)
    ~supported:(Delator.Field.bool (Option.is_some result))
    ~parameter_count:(Delator.Field.int (Option.fold ~none:0 ~some:(fun shape -> List.length shape.parameters) result))];
  result
[@@delator.instrument] [@@delator.level trace]

let classify ~constructor_uid:(constructor_uid [@delator.skip])
    ~logical_sorts:(logical_sorts [@delator.skip]) ~carrier_uid
    ~carrier:(carrier [@delator.skip]) (typ [@delator.skip]) =
  let result =
    if carrier.Types.type_params <> []
      || List.exists (fun sort -> String.equal sort.Logical_sort_private.type_uid carrier_uid) logical_sorts
    then Unsupported
    else match Types.get_desc (unwrap typ) with
    | Types.Tarrow ((Types.Nolabel, _, _), domain, range, _) -> (
        match Types.get_desc (unwrap domain), Types.get_desc (unwrap range) with
        | Tconstr (path, [], _), Tconstr (_, [], _) -> (
            match constructor_uid path with
            | Some uid when String.equal uid carrier_uid -> Exact_carrier
            | Some _ -> Different_carrier
            | None -> Unsupported)
        | _ -> Unsupported)
    | _ -> Unsupported
  in
  [%log.trace "classified numeric callable carrier domain"
    ~carrier_uid:(Delator.Field.string carrier_uid)
    ~domain_relation:(Delator.Field.string
      (match result with Exact_carrier -> "exact-constructor" | Different_carrier -> "different-constructor" | Unsupported -> "unsupported-shape"))];
  result
[@@delator.instrument] [@@delator.level trace]
