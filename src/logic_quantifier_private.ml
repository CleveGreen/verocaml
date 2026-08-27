type kind = Forall | Exists

type first_order_binder =
  | Integer_binder
  | Boolean_binder
  | Parameter_binder of Parametric_type.binder
  | Application_binder of Parametric_type.t

type t = {
  kind : kind;
  owner : string;
  binder_index : int;
  binder_type : Parametric_type.t;
  span : Diagnostic.span;
  qid : string;
  skid : string;
}

type vector = {
  vector_kind : kind;
  vector_owner : string;
  vector_binders : t list;
  vector_span : Diagnostic.span;
  vector_qid : string;
  vector_skid : string;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let kind_to_string = function Forall -> "forall" | Exists -> "exists"

let stable_id prefix kind owner binder_index binder_type =
  let material =
    String.concat "\000"
      [
        "verocaml-quantifier-v1";
        kind_to_string kind;
        owner;
        string_of_int binder_index;
        Parametric_type.to_string binder_type;
      ]
  in
  prefix ^ Digest.to_hex (Digest.string material)

let create ~kind ~owner ~binder_index ~binder_type ~span =
  {
    kind;
    owner;
    binder_index;
    binder_type;
    span;
    qid = stable_id "vero.q." kind owner binder_index binder_type;
    skid = stable_id "vero.sk." kind owner binder_index binder_type;
  }

let kind metadata = metadata.kind
let owner metadata = metadata.owner
let binder_index metadata = metadata.binder_index
let binder_type metadata = metadata.binder_type
let span metadata = metadata.span
let qid metadata = metadata.qid
let skid metadata = metadata.skid

let rebind ~binder_index ~binder_type metadata =
  create ~kind:metadata.kind ~owner:metadata.owner ~binder_index ~binder_type
    ~span:metadata.span

let seal_import ~offset ~binder_index ~binder_type metadata =
  create ~kind:metadata.kind
    ~owner:(Printf.sprintf "%s@import:%d" metadata.owner offset)
    ~binder_index ~binder_type ~span:metadata.span

let equal left right =
  left.kind = right.kind
  && String.equal left.owner right.owner
  && Int.equal left.binder_index right.binder_index
  && Parametric_type.equal left.binder_type right.binder_type
  && left.span = right.span
  && String.equal left.qid right.qid
  && String.equal left.skid right.skid

let imported_owner_matches ~expected_owner ~binder_index owner =
  let prefix = expected_owner ^ "@import:" in
  let prefix_length = String.length prefix in
  if
    String.length owner <= prefix_length
    || not (String.starts_with ~prefix owner)
  then false
  else
    match
      int_of_string_opt
        (String.sub owner prefix_length (String.length owner - prefix_length))
    with
    | Some offset ->
        offset >= 2_000_000_000 && binder_index >= offset
    | None -> false

let validate_shape ?expected_owner ~kind ~binder_index ~binder_type metadata =
  if metadata.kind <> kind then
    Error "quantifier constructor and metadata kind differ"
  else if not (Int.equal metadata.binder_index binder_index) then
    Error "quantifier metadata binder index differs from its lexical binder"
  else if not (Parametric_type.equal metadata.binder_type binder_type) then
    Error "quantifier metadata binder type differs from its lexical binder"
  else if
    Option.fold ~none:false
      ~some:(fun expected ->
        not
          (String.equal metadata.owner expected
          || imported_owner_matches ~expected_owner:expected ~binder_index
               metadata.owner))
      expected_owner
  then Error "quantifier metadata owner differs from its containing function"
  else Ok ()

let validate_identity metadata =
  let expected =
    create ~kind:metadata.kind ~owner:metadata.owner
      ~binder_index:metadata.binder_index ~binder_type:metadata.binder_type
      ~span:metadata.span
  in
  if String.equal metadata.owner "" || not (equal expected metadata) then
    Error "quantifier metadata identity or deterministic qid/skid is invalid"
  else Ok ()

let duplicate_index binders =
  let rec loop seen = function
    | [] -> None
    | binder :: rest ->
        if List.mem binder.binder_index seen then Some binder.binder_index
        else loop (binder.binder_index :: seen) rest
  in
  loop [] binders

let vector_stable_id prefix kind owner binders =
  let material =
    [
      "verocaml-quantifier-vector-v1";
      kind_to_string kind;
      owner;
    ]
    @ List.concat_map
        (fun binder ->
          [
            string_of_int binder.binder_index;
            Parametric_type.to_string binder.binder_type;
          ])
        binders
    |> String.concat "\000"
  in
  prefix ^ Digest.to_hex (Digest.string material)

let vector binders =
  match binders with
  | [] -> Error "quantifier binder vector must be nonempty"
  | first :: rest ->
      let* () =
        let rec validate = function
          | [] -> Ok ()
          | binder :: rest ->
              if binder.kind <> first.kind then
                Error "quantifier binder vector mixes quantifier kinds"
              else if not (String.equal binder.owner first.owner) then
                Error "quantifier binder vector mixes lexical owners"
              else
                let* () = validate_identity binder in
                validate rest
        in
        let* () = validate_identity first in
        validate rest
      in
      (match duplicate_index binders with
      | Some _ -> Error "quantifier binder vector duplicates a binder index"
      | None ->
          let vector_qid, vector_skid =
            match rest with
            | [] -> (first.qid, first.skid)
            | _ ->
                ( vector_stable_id "vero.qv." first.kind first.owner binders,
                  vector_stable_id "vero.skv." first.kind first.owner binders )
          in
          Ok
            {
              vector_kind = first.kind;
              vector_owner = first.owner;
              vector_binders = binders;
              vector_span = first.span;
              vector_qid;
              vector_skid;
            })

let singleton metadata =
  match vector [ metadata ] with Ok vector -> vector | Error _ -> assert false

let vector_kind vector = vector.vector_kind
let vector_owner vector = vector.vector_owner
let vector_binders vector = vector.vector_binders
let vector_span vector = vector.vector_span
let vector_qid vector = vector.vector_qid
let vector_skid vector = vector.vector_skid

let vector_equal left right =
  left.vector_kind = right.vector_kind
  && String.equal left.vector_owner right.vector_owner
  && List.length left.vector_binders = List.length right.vector_binders
  && List.for_all2 equal left.vector_binders right.vector_binders
  && left.vector_span = right.vector_span
  && String.equal left.vector_qid right.vector_qid
  && String.equal left.vector_skid right.vector_skid

let vector_rebind replacements schema =
  if List.length replacements <> List.length schema.vector_binders then
    Error "quantifier rebind vector arity mismatch"
  else
    List.map2
      (fun (binder_index, binder_type) metadata ->
        rebind ~binder_index ~binder_type metadata)
      replacements schema.vector_binders
    |> vector

let vector_seal_import ~offset replacements schema =
  if List.length replacements <> List.length schema.vector_binders then
    Error "quantifier import vector arity mismatch"
  else
    List.map2
      (fun (binder_index, binder_type) metadata ->
        seal_import ~offset ~binder_index ~binder_type metadata)
      replacements schema.vector_binders
    |> vector

let validate_vector ?expected_owner ~kind ~binder_indices ~binder_types vector =
  let binders = vector.vector_binders in
  if binder_indices = [] then Error "quantifier binder vector must be nonempty"
  else if
    List.length binder_indices <> List.length binder_types
    || List.length binder_indices <> List.length binders
  then Error "quantifier schema, binder, and type vector arities differ"
  else
    let rec validate binders indices types =
      match (binders, indices, types) with
      | [], [], [] -> Ok ()
      | metadata :: binders, binder_index :: indices, binder_type :: types ->
          let* () =
            validate_shape ?expected_owner ~kind ~binder_index ~binder_type
              metadata
          in
          validate binders indices types
      | _ -> assert false
    in
    validate binders binder_indices binder_types

let validate_vector_identity schema =
  let* expected = vector schema.vector_binders in
  if schema.vector_owner = "" || not (vector_equal expected schema) then
    Error "quantifier vector identity or deterministic qid/skid is invalid"
  else Ok ()

let first_order_binder = function
  | Parametric_type.Int -> Ok Integer_binder
  | Parametric_type.Bool -> Ok Boolean_binder
  | Parametric_type.Parameter binder -> Ok (Parameter_binder binder)
  | Parametric_type.Application _ as typ -> Ok (Application_binder typ)
  | Parametric_type.Unit | Parametric_type.Tuple _ | Parametric_type.Aggregate _ ->
      Error "unsupported quantifier binder reached symbolic execution"

let to_string metadata =
  Printf.sprintf "%s[%s,%s](%s:%s)" (kind_to_string metadata.kind)
    metadata.qid metadata.skid metadata.owner
    (Parametric_type.to_string metadata.binder_type)

let vector_to_string vector =
  Printf.sprintf "%s[%s,%s](%s:%s)"
    (kind_to_string vector.vector_kind)
    vector.vector_qid vector.vector_skid vector.vector_owner
    (String.concat ","
       (List.map
          (fun binder -> Parametric_type.to_string binder.binder_type)
          vector.vector_binders))
