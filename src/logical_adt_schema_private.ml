type t = {
  descriptor : Parametric_adt.t;
  arguments : Parametric_type.t list;
  application_id : string;
  scc_id : string;
}

type error = {
  application : string;
  message : string;
}

let descriptor schema = schema.descriptor
let arguments schema = schema.arguments
let application_id schema = schema.application_id
let scc_id schema = schema.scc_id
let compare left right = String.compare left.application_id right.application_id
let error_to_string error = error.application ^ ": " ^ error.message

let key descriptor arguments =
  let constructor = Parametric_adt.type_constructor descriptor in
  constructor.Parametric_type.constructor_identity
  ^ "<"
  ^ String.concat "," (List.map Parametric_type.to_string arguments)
  ^ ">"

let error application message = Error { application; message }

let descriptor_by_index descriptors index =
  List.find_opt
    (fun descriptor ->
      (Parametric_adt.type_id descriptor).Parametric_type.type_index = index)
    descriptors

let fields = function
  | Parametric_adt.Record fields -> fields
  | Variant constructors ->
      List.concat_map
        (fun constructor -> constructor.Parametric_adt.constructor_fields)
        constructors

let instantiate ~descriptors ~applications =
  let rec add visiting completed descriptor arguments =
    let application_id = key descriptor arguments in
    if List.exists (fun schema -> schema.application_id = application_id) completed
    then Ok completed
    else if List.mem application_id visiting then
      error application_id
        "parameterized datatype applications form a non-self cycle"
    else if
      List.exists (fun field -> field.Parametric_adt.field_mutable)
        (fields (Parametric_adt.kind descriptor))
    then error application_id "logical datatype fields must be immutable"
    else
      let self_constructor = Parametric_adt.type_constructor descriptor in
      let add_type completed source_type typ =
        match typ with
        | Parametric_type.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Parameter _
        | Aggregate _ ->
            Ok completed
        | Tuple _ ->
            error application_id
              "tuple-valued logical datatype fields are outside the first tranche"
        | Application (constructor, nested_arguments) ->
            if
              Parametric_type.compare_constructor constructor self_constructor = 0
            then
              (match source_type with
              | Parametric_type.Application (source_constructor, _)
                when
                  Parametric_type.compare_constructor source_constructor
                    self_constructor
                  = 0 ->
                  if
                    List.for_all2 Parametric_type.equal arguments
                      nested_arguments
                  then Ok completed
                  else
                    error application_id
                      "nonuniform or polymorphic recursion is unsupported"
              | _ ->
                  add (application_id :: visiting) completed descriptor
                    nested_arguments)
            else (
              match Parametric_adt.find descriptors constructor with
              | None ->
                  error application_id
                    "nested datatype application has no authenticated descriptor"
              | Some nested ->
                  add (application_id :: visiting) completed nested
                    nested_arguments)
      in
      let rec add_fields completed = function
        | [] ->
            Ok
              ({ descriptor;
                 arguments;
                 application_id;
                 scc_id = "scc:" ^ application_id }
              :: completed)
        | field :: rest ->
            let result =
              Parametric_adt.instantiate_field descriptor arguments field
            in
            (match result with
            | Error message -> error application_id message
            | Ok typ ->
                let* completed = add_type completed field.field_type typ in
                add_fields completed rest)
      in
      add_fields completed (fields (Parametric_adt.kind descriptor))
  and ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error
  in
  let rec requested completed = function
    | [] -> Ok (List.sort_uniq compare completed)
    | (index, arguments) :: rest -> (
        match descriptor_by_index descriptors index with
        | None ->
            error (string_of_int index)
              "exact application has no authenticated descriptor"
        | Some descriptor ->
            let* completed = add [] completed descriptor arguments in
            requested completed rest)
  in
  requested [] applications
