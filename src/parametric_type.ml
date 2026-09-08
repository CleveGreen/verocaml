type owner = { owner_index : int; owner_name : string }
type binder = { owner : owner; ordinal : int }
type type_id = { type_index : int; type_name : string }
type constructor = { constructor_path : string; constructor_identity : string }

type t =
  | Unit
  | Bool
  | Int
  | Mathematical_int
  | Bit_vector of Bv_width.t
  | Tuple of (string option * t) list
  | Aggregate of type_id
  | Parameter of binder
  | Application of constructor * t list

let owner ~index ~name = { owner_index = index; owner_name = name }
let binder owner ~ordinal = { owner; ordinal }
let binders owner count = List.init count (fun ordinal -> binder owner ~ordinal)

let spec_function_path = "$verocaml.spec-function"
let spec_function_identity_prefix = "verocaml:spec-function/2:"

let spec_function_constructor ~label =
  let encoded_label =
    match label with
    | None -> "0:"
    | Some label -> Printf.sprintf "1:%d:%s" (String.length label) label
  in
  {
    constructor_path = spec_function_path;
    constructor_identity = spec_function_identity_prefix ^ encoded_label;
  }

let spec_function ~label ~domain ~range =
  Application (spec_function_constructor ~label, [ domain; range ])

let spec_function_label constructor =
  if not (String.equal constructor.constructor_path spec_function_path) then None
  else
    let identity = constructor.constructor_identity in
    let prefix_length = String.length spec_function_identity_prefix in
    if
      String.length identity < prefix_length
      || not
           (String.equal
              (String.sub identity 0 prefix_length)
              spec_function_identity_prefix)
    then None
    else
      let encoded =
        String.sub identity prefix_length (String.length identity - prefix_length)
      in
      if String.equal encoded "0:" then Some None
      else
        match String.split_on_char ':' encoded with
        | [ "1"; length; label ]
          when int_of_string_opt length = Some (String.length label) ->
            Some (Some label)
        | _ -> None

let spec_function_view = function
  | Application (constructor, [ domain; range ]) ->
      Option.map
        (fun label -> (label, domain, range))
        (spec_function_label constructor)
  | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _ | Aggregate _ | Parameter _
  | Application _ ->
      None

let is_spec_function typ = Option.is_some (spec_function_view typ)

let compare_owner left right =
  let by_index = Int.compare left.owner_index right.owner_index in
  if by_index <> 0 then by_index
  else String.compare left.owner_name right.owner_name

let compare_binder left right =
  let by_owner = compare_owner left.owner right.owner in
  if by_owner <> 0 then by_owner else Int.compare left.ordinal right.ordinal

let compare_constructor left right =
  let by_identity =
    String.compare left.constructor_identity right.constructor_identity
  in
  if by_identity <> 0 then by_identity
  else String.compare left.constructor_path right.constructor_path

let compare_type_id left right =
  let by_index = Int.compare left.type_index right.type_index in
  if by_index <> 0 then by_index
  else String.compare left.type_name right.type_name

let rec compare left right =
  let rank = function
    | Unit -> 0
    | Bool -> 1
    | Int -> 2
    | Mathematical_int -> 3
    | Bit_vector _ -> 4
    | Tuple _ -> 5
    | Aggregate _ -> 6
    | Parameter _ -> 7
    | Application _ -> 8
  in
  let by_rank = Int.compare (rank left) (rank right) in
  if by_rank <> 0 then by_rank
  else
    match (left, right) with
    | Unit, Unit | Bool, Bool | Int, Int
    | Mathematical_int, Mathematical_int ->
        0
    | Bit_vector left, Bit_vector right -> Bv_width.compare left right
    | Tuple left, Tuple right ->
        List.compare
          (fun (left_label, left_type) (right_label, right_type) ->
            let by_label =
              Option.compare String.compare left_label right_label
            in
            if by_label <> 0 then by_label else compare left_type right_type)
          left right
    | Aggregate left, Aggregate right -> compare_type_id left right
    | Parameter left, Parameter right -> compare_binder left right
    | ( Application (left_constructor, left_arguments),
        Application (right_constructor, right_arguments) ) ->
        let by_constructor =
          compare_constructor left_constructor right_constructor
        in
        if by_constructor <> 0 then by_constructor
        else List.compare compare left_arguments right_arguments
    | _ -> assert false

let equal left right = compare left right = 0

let rec compiler_erasure_compatible ~compiler ~semantic =
  equal compiler semantic
  ||
  match compiler, semantic with
  | Int, Mathematical_int -> true
  | Tuple compiler, Tuple semantic when List.length compiler = List.length semantic ->
      List.for_all2
        (fun (compiler_label, compiler) (semantic_label, semantic) ->
          Option.equal String.equal compiler_label semantic_label
          && compiler_erasure_compatible ~compiler ~semantic)
        compiler semantic
  | ( Application (compiler_constructor, compiler_arguments),
      Application (semantic_constructor, semantic_arguments) )
    when compare_constructor compiler_constructor semantic_constructor = 0
         && List.length compiler_arguments = List.length semantic_arguments ->
      List.for_all2
        (fun compiler semantic ->
          compiler_erasure_compatible ~compiler ~semantic)
        compiler_arguments semantic_arguments
  | ( ( Unit | Bool | Mathematical_int | Bit_vector _ | Aggregate _ | Parameter _
        | Application _ | Tuple _ ),
      _ )
  | Int, _ ->
      false

let application constructor arguments =
  let expected_arity =
    if Option.is_some (spec_function_label constructor) then Some 2
    else None
  in
  match expected_arity with
  | Some expected when List.length arguments <> expected ->
      Error
        (Printf.sprintf "type constructor %s expects %d argument(s)"
           constructor.constructor_path expected)
  | Some _ | None -> Ok (Application (constructor, arguments))

let validate_application constructor arguments =
  if Option.is_none (spec_function_label constructor)
  then
    Error
      (Printf.sprintf "unsupported type constructor identity %s"
         constructor.constructor_path)
  else Result.map (fun _ -> ()) (application constructor arguments)

let rec alpha_equal left right =
  match (left, right) with
  | Unit, Unit | Bool, Bool | Int, Int
  | Mathematical_int, Mathematical_int ->
      true
  | Bit_vector left, Bit_vector right -> Bv_width.equal left right
  | Aggregate left, Aggregate right -> compare_type_id left right = 0
  | Parameter left, Parameter right -> Int.equal left.ordinal right.ordinal
  | Tuple left, Tuple right when List.length left = List.length right ->
      List.for_all2
        (fun (left_label, left_type) (right_label, right_type) ->
          Option.equal String.equal left_label right_label
          && alpha_equal left_type right_type)
        left right
  | ( Application (left_constructor, left_arguments),
      Application (right_constructor, right_arguments) )
    when compare_constructor left_constructor right_constructor = 0
         && List.length left_arguments = List.length right_arguments ->
      List.for_all2 alpha_equal left_arguments right_arguments
  | _ -> false

let rec substitute substitutions = function
  | Parameter binder as original ->
      Option.value ~default:original
        (List.find_map
           (fun (candidate, replacement) ->
             if compare_binder binder candidate = 0 then Some replacement
             else None)
           substitutions)
  | Tuple components ->
      Tuple
        (List.map
           (fun (label, component) ->
             (label, substitute substitutions component))
           components)
  | Application (constructor, arguments) ->
      Application (constructor, List.map (substitute substitutions) arguments)
  | (Unit | Bool | Int | Mathematical_int | Bit_vector _ | Aggregate _) as closed -> closed

let instantiate binders arguments typ =
  if List.length binders <> List.length arguments then
    Error
      (Printf.sprintf "expected %d type argument(s), received %d"
         (List.length binders) (List.length arguments))
  else Ok (substitute (List.combine binders arguments) typ)

let rec is_open = function
  | Parameter _ -> true
  | Tuple components -> List.exists (fun (_, typ) -> is_open typ) components
  | Application (_, arguments) -> List.exists is_open arguments
  | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Aggregate _ -> false

let parameters typ =
  let rec collect found = function
    | Parameter binder ->
        if
          List.exists
            (fun candidate -> compare_binder candidate binder = 0)
            found
        then found
        else binder :: found
    | Tuple components ->
        List.fold_left
          (fun found (_, typ) -> collect found typ)
          found components
    | Application (_, arguments) -> List.fold_left collect found arguments
    | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Aggregate _ -> found
  in
  collect [] typ |> List.sort compare_binder

let owner_to_string owner =
  Printf.sprintf "%s#%d" owner.owner_name owner.owner_index

let binder_to_string binder =
  Printf.sprintf "'%d@%s" binder.ordinal (owner_to_string binder.owner)

let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Mathematical_int -> "Int"
  | Bit_vector width -> Printf.sprintf "BV<%s>" (Bv_width.to_string width)
  | Aggregate type_id ->
      Printf.sprintf "%s#%d" type_id.type_name type_id.type_index
  | Parameter binder -> binder_to_string binder
  | Tuple components ->
      components
      |> List.map (fun (label, typ) ->
          match label with
          | None -> to_string typ
          | Some label -> label ^ ":" ^ to_string typ)
      |> String.concat " * " |> Printf.sprintf "(%s)"
  | Application (constructor, arguments) ->
      Printf.sprintf "%s<%s>" constructor.constructor_path
        (String.concat ", " (List.map to_string arguments))

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let rec structural_identity_material = function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "i"
  | Mathematical_int -> "I"
  | Bit_vector width ->
      framed "B" [ Bv_width.structural_identity_material width ]
  | Aggregate type_id ->
      framed "g"
        [ string_of_int type_id.type_index; type_id.type_name ]
  | Parameter binder ->
      framed "p"
        [
          string_of_int binder.owner.owner_index;
          binder.owner.owner_name;
          string_of_int binder.ordinal;
        ]
  | Tuple components ->
      framed "t"
        (List.map
           (fun (label, typ) ->
             framed "c"
               [
                 Option.value ~default:"" label;
                 structural_identity_material typ;
               ])
           components)
  | Application (constructor, arguments) ->
      framed "a"
        [
          constructor.constructor_identity;
          constructor.constructor_path;
          framed "v" (List.map structural_identity_material arguments);
        ]

let structural_vector_material types =
  framed "V" (List.map structural_identity_material types)

let digest material = Digest.to_hex (Digest.string material)
let structural_identity_digest typ = digest (structural_identity_material typ)
let structural_vector_digest types = digest (structural_vector_material types)

let is_integer = function Int | Mathematical_int -> true | _ -> false

let rec contains_mathematical_int = function
  | Mathematical_int -> true
  | Tuple components ->
      List.exists (fun (_, typ) -> contains_mathematical_int typ) components
  | Application (_, arguments) -> List.exists contains_mathematical_int arguments
  | Unit | Bool | Int | Bit_vector _ | Aggregate _ | Parameter _ -> false

let rec contains_bit_vector = function
  | Bit_vector _ -> true
  | Tuple components ->
      List.exists (fun (_, typ) -> contains_bit_vector typ) components
  | Application (_, arguments) -> List.exists contains_bit_vector arguments
  | Unit | Bool | Int | Mathematical_int | Aggregate _ | Parameter _ -> false
