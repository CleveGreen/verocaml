type provenance =
  | Local of { compiler_uid : string }
  | External of {
      compiler_uid : string;
      proxy_uid : string;
    }

type field = {
  field_index : int;
  field_name : string;
  field_uid : string;
  field_type : Parametric_type.t;
  field_mutable : bool;
}

type constructor = {
  constructor_index : int;
  constructor_name : string;
  constructor_uid : string;
  constructor_fields : field list;
}

type kind = Record of field list | Variant of constructor list

type t = {
  type_id : Parametric_type.type_id;
  type_constructor : Parametric_type.constructor;
  binders : Parametric_type.binder list;
  provenance : provenance;
  (* Serialized compatibility metadata; optional lowering never treats it as authority. *)
  legacy_optional_carrier : bool;
  kind : kind;
  recursive_fields : (int option * field) list;
}

type scalar_kind = Scalar_bool | Scalar_int | Scalar_bv of Bv_width.t

type option_instance = {
  option_descriptor : t;
  option_arguments : Parametric_type.t list;
  option_payload_type : Parametric_type.t;
  option_absent : constructor;
  option_present : constructor;
}

type optional_carrier_error =
  | Missing_optional_descriptor
  | Invalid_optional_descriptor
  | Optional_payload_mismatch

type error = { descriptor : string; message : string }

let type_id descriptor = descriptor.type_id
let type_constructor descriptor = descriptor.type_constructor
let binders descriptor = descriptor.binders
let provenance descriptor = descriptor.provenance
let kind descriptor = descriptor.kind
let recursive_fields descriptor = descriptor.recursive_fields
let is_optional_carrier descriptor = descriptor.legacy_optional_carrier
let authenticates_recursive_field descriptor ~constructor_index ~field_index =
  List.exists
    (fun (candidate_constructor, field) ->
      candidate_constructor = constructor_index
      && field.field_index = field_index)
    descriptor.recursive_fields

let compiler_uid descriptor =
  match descriptor.provenance with
  | Local { compiler_uid } | External { compiler_uid; _ } -> compiler_uid

let error type_constructor message =
  Error { descriptor = type_constructor.Parametric_type.constructor_path; message }

let dense indices = indices = List.init (List.length indices) Fun.id

let fields = function
  | Record fields -> fields
  | Variant constructors -> List.concat_map (fun constructor -> constructor.constructor_fields) constructors

let direct_recursive type_constructor binders typ =
  match typ with
  | Parametric_type.Application (constructor, arguments)
    when Parametric_type.compare_constructor constructor type_constructor = 0
         && List.length arguments = List.length binders
         && List.for_all2
              (fun argument binder ->
                match argument with
                | Parametric_type.Parameter candidate ->
                    Parametric_type.compare_binder candidate binder = 0
                | _ -> false)
              arguments binders ->
      true
  | _ -> false

let validate_fields type_constructor owner binders candidate_fields =
  let indices = List.map (fun field -> field.field_index) candidate_fields in
  if not (dense indices) then error type_constructor (owner ^ " fields must have dense zero-based indices")
  else if List.exists (fun field -> String.equal field.field_uid "") candidate_fields then
    error type_constructor (owner ^ " field compiler identity is empty")
  else if
    List.exists
      (fun field ->
        List.exists
          (fun parameter ->
            not
              (List.exists
                 (fun binder -> Parametric_type.compare_binder binder parameter = 0)
                 binders))
          (Parametric_type.parameters field.field_type))
      candidate_fields
  then error type_constructor (owner ^ " field template escapes descriptor binders")
  else Ok ()

let binder_matches_type type_id binder =
  binder.Parametric_type.owner.owner_index = type_id.Parametric_type.type_index
  && String.equal binder.owner.owner_name type_id.type_name

let create ~optional_carrier:legacy_optional_carrier ~type_id ~type_constructor ~binders
    ~provenance ~kind =
  let descriptor_name = type_constructor.Parametric_type.constructor_path in
  let fail message = error type_constructor message in
  if type_constructor.constructor_identity = "" then fail "type constructor identity is empty"
  else if
    compiler_uid
      {
        type_id;
        type_constructor;
        binders;
        provenance;
        legacy_optional_carrier;
        kind;
        recursive_fields = [];
      }
    = ""
  then
    fail "compiler type UID is empty"
  else if
    match provenance with
    | External { proxy_uid; _ } -> String.equal proxy_uid ""
    | Local _ -> false
  then fail "external proxy compiler identity is empty"
  else if
    List.mapi (fun ordinal binder -> ordinal = binder.Parametric_type.ordinal) binders
    |> List.exists not
  then fail "binders must have dense declaration order"
  else if List.exists (fun binder -> not (binder_matches_type type_id binder)) binders
  then fail "binders must belong to the descriptor type identity"
  else
    let validate_kind =
      match kind with
      | Record candidate_fields -> validate_fields type_constructor "record" binders candidate_fields
      | Variant constructors ->
          let indices = List.map (fun constructor -> constructor.constructor_index) constructors in
          if constructors = [] then fail "variant has no constructors"
          else if not (dense indices) then fail "constructors must have dense zero-based indices"
          else if List.exists (fun constructor -> String.equal constructor.constructor_uid "") constructors then
            fail "constructor compiler identity is empty"
          else
            List.fold_left
              (fun result constructor ->
                match result with
                | Error _ -> result
                | Ok () -> validate_fields type_constructor ("constructor " ^ constructor.constructor_name) binders constructor.constructor_fields)
              (Ok ()) constructors
    in
    match validate_kind with
    | Error _ as failure -> failure
    | Ok () ->
        let recursive_fields =
          match kind with
          | Record candidate_fields ->
              List.filter_map
                (fun field -> if direct_recursive type_constructor binders field.field_type then Some (None, field) else None)
                candidate_fields
          | Variant constructors ->
              List.concat_map
                (fun constructor ->
                  List.filter_map
                    (fun field ->
                      if direct_recursive type_constructor binders field.field_type then
                        Some (Some constructor.constructor_index, field)
                      else None)
                    constructor.constructor_fields)
                constructors
        in
        let descriptor =
          {
            type_id;
            type_constructor;
            binders;
            provenance;
            legacy_optional_carrier;
            kind;
            recursive_fields;
          }
        in
        let _ = descriptor_name in
        Ok descriptor

let find descriptors constructor =
  List.find_opt
    (fun descriptor ->
      Parametric_type.compare_constructor descriptor.type_constructor constructor = 0)
    descriptors

let option_instance descriptors = function
  | Parametric_type.Application (constructor, arguments) -> (
      match find descriptors constructor with
      | Some
          ({ binders = [ binder ];
             kind =
               Variant
                 [ ({ constructor_index = 0; constructor_fields = []; _ } as absent);
                   ({ constructor_index = 1;
                      constructor_fields =
                        [ { field_index = 0; field_type; field_mutable = false; _ } ];
                      _ } as present) ];
             _ } as descriptor)
        when Parametric_type.compare_constructor constructor
               descriptor.type_constructor
             = 0
             && Parametric_type.equal field_type
                  (Parametric_type.Parameter binder)
             && List.length arguments = 1 -> (
          match Parametric_type.instantiate descriptor.binders arguments
                  (List.hd present.constructor_fields).field_type with
          | Ok option_payload_type ->
              Some
                { option_descriptor = descriptor;
                  option_arguments = arguments;
                  option_payload_type;
                  option_absent = absent;
                  option_present = present }
          | Error _ -> None)
      | Some _ | None -> None)
  | Parametric_type.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _
  | Aggregate _ | Parameter _ ->
      None

let authenticate_optional_carrier descriptors ~carrier ~payload =
  match carrier with
  | Parametric_type.Application (constructor, _) -> (
      match find descriptors constructor with
      | None -> Error Missing_optional_descriptor
      | Some _ -> (
          match option_instance descriptors carrier with
          | None -> Error Invalid_optional_descriptor
          | Some instance
            when Parametric_type.equal instance.option_payload_type payload ->
              Ok instance
          | Some _ -> Error Optional_payload_mismatch))
  | Parametric_type.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _
  | Aggregate _ | Parameter _ ->
      Error Missing_optional_descriptor

let application descriptor arguments =
  if List.length arguments <> List.length descriptor.binders then
    Error
      (Printf.sprintf "type constructor %s expects %d argument(s)"
         descriptor.type_constructor.constructor_path (List.length descriptor.binders))
  else Ok (Parametric_type.Application (descriptor.type_constructor, arguments))

let same_application descriptor = function
  | Parametric_type.Application (constructor, arguments) ->
      Parametric_type.compare_constructor constructor descriptor.type_constructor = 0
      && List.length arguments = List.length descriptor.binders
  | _ -> false

let instantiate_field descriptor arguments field =
  Parametric_type.instantiate descriptor.binders arguments field.field_type

let instantiate_field_by_index descriptor arguments ~constructor_index ~field_index =
  let candidate =
    match (descriptor.kind, constructor_index) with
    | Record fields, None ->
        List.find_opt (fun field -> field.field_index = field_index) fields
    | Variant constructors, Some constructor_index -> (
        match
          List.find_opt
            (fun constructor -> constructor.constructor_index = constructor_index)
            constructors
        with
        | None -> None
        | Some constructor ->
            List.find_opt (fun field -> field.field_index = field_index)
              constructor.constructor_fields)
    | Record _, Some _ | Variant _, None -> None
  in
  match candidate with
  | None -> Error "field index is absent from its descriptor owner"
  | Some field -> instantiate_field descriptor arguments field

let validate_registry descriptors =
  let rec loop seen_ids seen_constructors = function
    | [] -> Ok ()
    | descriptor :: rest ->
        if List.exists (( = ) descriptor.type_id) seen_ids then
          error descriptor.type_constructor "duplicate descriptor type identity"
        else if
          List.exists
            (fun constructor ->
              Parametric_type.compare_constructor constructor descriptor.type_constructor = 0)
            seen_constructors
        then error descriptor.type_constructor "duplicate descriptor constructor identity"
        else
          loop (descriptor.type_id :: seen_ids)
            (descriptor.type_constructor :: seen_constructors) rest
  in
  loop [] [] descriptors

let deeply_immutable_instance descriptors typ =
  let rec immutable visiting = function
    | Parametric_type.Bool | Parametric_type.Int
    | Parametric_type.Mathematical_int ->
        true
    | Parametric_type.Bit_vector _ -> true
    | Parametric_type.Unit -> true
    | Parametric_type.Parameter _ -> true
    | Parametric_type.Tuple components -> List.for_all (fun (_, typ) -> immutable visiting typ) components
    | Parametric_type.Aggregate _ -> false
    | Parametric_type.Application (constructor, arguments) -> (
        match find descriptors constructor with
        | None -> false
        | Some descriptor ->
            let key = constructor.constructor_identity ^ "<" ^ String.concat "," (List.map Parametric_type.to_string arguments) ^ ">" in
            if List.mem key visiting then true
            else if List.exists (fun field -> field.field_mutable) (fields descriptor.kind) then false
            else
              List.for_all (immutable visiting) arguments
              && List.for_all
                   (fun field ->
                     match instantiate_field descriptor arguments field with
                     | Error _ -> false
                     | Ok field_type -> immutable (key :: visiting) field_type)
                   (fields descriptor.kind))
  in
  immutable [] typ

let scalar_kind = function
  | Parametric_type.Bool -> Some Scalar_bool
  | Parametric_type.Int -> Some Scalar_int
  | Parametric_type.Bit_vector width -> Some (Scalar_bv width)
  | Parametric_type.Mathematical_int | Parametric_type.Unit | Tuple _
  | Aggregate _ | Parameter _ | Application _ ->
      None

let exec_scalar_layout descriptors typ =
  match typ with
  | Parametric_type.Application (constructor, arguments) -> (
      match find descriptors constructor with
      | Some descriptor
        when descriptor.recursive_fields = []
             && not (Parametric_type.is_open typ)
             && deeply_immutable_instance descriptors typ -> (
          let eligible_provenance =
            match descriptor.provenance with
            | Local _ | External _ -> true
          in
          match (eligible_provenance, descriptor.kind) with
          | true, Variant constructors ->
              let lower_constructor constructor =
                let rec fields lowered = function
                  | [] -> Some (constructor, List.rev lowered)
                  | field :: rest -> (
                      match instantiate_field descriptor arguments field with
                      | Ok field_type -> (
                          match scalar_kind field_type with
                          | Some kind -> fields ((field, kind) :: lowered) rest
                          | None -> None)
                      | Error _ -> None)
                in
                fields [] constructor.constructor_fields
              in
              let layouts = List.map lower_constructor constructors in
              if List.for_all Option.is_some layouts then
                Some (List.map Option.get layouts)
              else None
          | (false, _ | true, Record _) -> None)
      | Some _ | None -> None)
  | Parametric_type.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _
  | Aggregate _ | Parameter _ ->
      None

let exec_scalar_equality descriptors typ =
  Option.is_some (exec_scalar_layout descriptors typ)

let provenance_name = function
  | Local _ -> "local"
  | External _ -> "external-type-specification"

let to_string descriptor =
  let field field =
    Printf.sprintf "%d:%s:%s%s" field.field_index field.field_name
      (Parametric_type.to_string field.field_type)
      (if field.field_mutable then ":mutable" else "")
  in
  let shape =
    match descriptor.kind with
    | Record candidate_fields -> "record{" ^ String.concat ";" (List.map field candidate_fields) ^ "}"
    | Variant constructors ->
        constructors
        |> List.map (fun constructor ->
               Printf.sprintf "%d:%s(%s)" constructor.constructor_index constructor.constructor_name
                 (String.concat "," (List.map field constructor.constructor_fields)))
        |> String.concat "|" |> Printf.sprintf "variant[%s]"
  in
  Printf.sprintf "adt %s uid=%s provenance=%s binders=%d %s recursive-fields=%d"
    (Parametric_type.to_string (Parametric_type.Application (descriptor.type_constructor, List.map (fun binder -> Parametric_type.Parameter binder) descriptor.binders)))
    (compiler_uid descriptor) (provenance_name descriptor.provenance)
    (List.length descriptor.binders) shape (List.length descriptor.recursive_fields)
