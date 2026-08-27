type declaration = {
  marker_id : string;
  declaration_index : int;
  declaration_name : string;
  canonical_path : string;
  value_uid : string;
  source_file : string;
  compilation_identity : string;
  declaration_span : Diagnostic.span;
  type_binders : Parametric_type.binder list;
  parameter_types : Parametric_type.t list;
  result_type : Parametric_type.t;
  identity_material : string;
}

type 'argument t = {
  declaration : declaration;
  type_arguments : Parametric_type.t list;
  arguments : 'argument list;
  argument_types : Parametric_type.t list;
  result_type : Parametric_type.t;
  span : Diagnostic.span;
  identity_material : string;
  symbol_name : string;
}

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let span_material span =
  framed "s"
    [
      span.Diagnostic.file;
      string_of_int span.start_pos.line;
      string_of_int span.start_pos.column;
      string_of_int span.end_pos.line;
      string_of_int span.end_pos.column;
    ]

let declaration_material ~marker_id ~declaration_index ~declaration_name
    ~canonical_path ~value_uid ~source_file ~compilation_identity
    ~declaration_span ~type_binders ~parameter_types ~result_type =
  framed "D"
    [
      marker_id;
      string_of_int declaration_index;
      declaration_name;
      canonical_path;
      value_uid;
      source_file;
      compilation_identity;
      span_material declaration_span;
      Parametric_type.structural_vector_material
        (List.map (fun binder -> Parametric_type.Parameter binder) type_binders);
      Parametric_type.structural_vector_material parameter_types;
      Parametric_type.structural_identity_material result_type;
    ]

let supported_type typ =
  let rec supported = function
    | Parametric_type.Unit | Int | Bool | Parameter _ -> true
    | Application (constructor, arguments) ->
        constructor.constructor_path <> ""
        && constructor.constructor_identity <> ""
        && List.for_all supported arguments
    | Tuple _ | Aggregate _ -> false
  in
  supported typ

let declare ~marker_id ~declaration_index ~declaration_name ~canonical_path
    ~value_uid ~source_file ~compilation_identity ~declaration_span
    ~type_binders ~parameter_types ~result_type =
  if String.length marker_id = 0 || String.length declaration_name = 0 then
    Error "symbolic declaration identity is empty"
  else if List.exists (fun binder -> binder.Parametric_type.ordinal < 0) type_binders
  then Error "symbolic declaration has an invalid type binder"
  else if not (List.for_all supported_type parameter_types) then
    Error
      ("symbolic declaration contains an unsupported parameter type: "
      ^ String.concat ","
          (List.map Parametric_type.to_string parameter_types))
  else if not (supported_type result_type) then
    Error
      ("symbolic declaration contains an unsupported result type: "
      ^ Parametric_type.to_string result_type)
  else
    let identity_material =
      declaration_material ~marker_id ~declaration_index ~declaration_name
        ~canonical_path ~value_uid ~source_file ~compilation_identity
        ~declaration_span ~type_binders ~parameter_types ~result_type
    in
    Ok
      {
        marker_id;
        declaration_index;
        declaration_name;
        canonical_path;
        value_uid;
        source_file;
        compilation_identity;
        declaration_span;
        type_binders;
        parameter_types;
        result_type;
        identity_material;
      }

let instantiate declaration type_arguments =
  if List.length declaration.type_binders <> List.length type_arguments then
    Error "symbolic application type-vector arity differs from its declaration"
  else
    let instantiate typ =
      Parametric_type.instantiate declaration.type_binders type_arguments typ
    in
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | typ :: rest ->
          Result.bind (instantiate typ) (fun typ -> loop (typ :: lowered) rest)
    in
    Result.bind (loop [] declaration.parameter_types) (fun parameters ->
        Result.map
          (fun result -> (parameters, result))
          (instantiate declaration.result_type))

let symbol_name_for_application declaration ~type_arguments ~argument_types
    ~result_type =
  Result.bind (instantiate declaration type_arguments)
    (fun (expected_arguments, expected_result) ->
      if
        not (List.equal Parametric_type.equal expected_arguments argument_types)
      then Error "symbolic application argument types are not exact"
      else if not (Parametric_type.equal expected_result result_type) then
        Error "symbolic application result type is not exact"
      else
        let stable_head =
          framed "H"
            [
              declaration.canonical_path;
              Parametric_type.structural_vector_material type_arguments;
            ]
        in
        Ok
          ("vero_symbolic_"
          ^ Digest.to_hex (Digest.string stable_head)))

let create declaration ~type_arguments ~arguments ~argument_types ~result_type
    ~span =
  if List.length arguments <> List.length argument_types then
    Error "symbolic application argument and type vectors differ"
  else
    Result.bind (instantiate declaration type_arguments)
      (fun (expected_arguments, expected_result) ->
        if
          not
            (List.equal Parametric_type.equal expected_arguments argument_types)
        then Error "symbolic application argument types are not exact"
        else if not (Parametric_type.equal expected_result result_type) then
          Error "symbolic application result type is not exact"
        else
          let vector =
            Parametric_type.structural_vector_material type_arguments
          in
          let identity_material =
            framed "A" [ declaration.identity_material; vector ]
          in
          Result.map
            (fun symbol_name ->
            {
              declaration;
              type_arguments;
              arguments;
              argument_types;
              result_type;
              span;
              identity_material;
              symbol_name;
            })
            (symbol_name_for_application declaration ~type_arguments
               ~argument_types ~result_type))

let map_arguments map application =
  { application with arguments = List.map map application.arguments }

let replace_arguments arguments application =
  if List.length arguments <> List.length application.arguments then
    Error "symbolic application replacement argument arity differs"
  else Ok { application with arguments }

let map_types map application =
  create application.declaration
    ~type_arguments:(List.map map application.type_arguments)
    ~arguments:application.arguments
    ~argument_types:(List.map map application.argument_types)
    ~result_type:(map application.result_type) ~span:application.span

let validate ~argument_type application =
  let observed = List.map argument_type application.arguments in
  if not (List.equal Parametric_type.equal observed application.argument_types)
  then Error "symbolic application carried stale argument types"
  else
    Result.map (fun _ -> ())
      (create application.declaration
         ~type_arguments:application.type_arguments
         ~arguments:application.arguments ~argument_types:observed
         ~result_type:application.result_type ~span:application.span)

let declaration application = application.declaration
let declaration_name declaration = declaration.declaration_name
let declaration_index declaration = declaration.declaration_index
let canonical_path declaration = declaration.canonical_path
let value_uid declaration = declaration.value_uid
let source_file declaration = declaration.source_file
let compilation_identity declaration = declaration.compilation_identity
let declaration_span declaration = declaration.declaration_span
let marker_id declaration = declaration.marker_id
let type_binders declaration = declaration.type_binders
let parameter_types declaration = declaration.parameter_types
let declaration_result_type (declaration : declaration) =
  declaration.result_type
let type_arguments application = application.type_arguments
let arguments application = application.arguments
let argument_types application = application.argument_types
let result_type application = application.result_type
let span application = application.span
let identity_material application = application.identity_material
let identity_digest application =
  Digest.to_hex (Digest.string application.identity_material)
let symbol_name application = application.symbol_name

let same_declaration (left : declaration) (right : declaration) =
  String.equal left.identity_material right.identity_material

let same_head left right =
  same_declaration left.declaration right.declaration
  && List.equal Parametric_type.equal left.type_arguments right.type_arguments

let is_nullary application = application.arguments = []

let to_string argument application =
  Printf.sprintf "%s[%s](%s):%s"
    application.declaration.declaration_name
    (String.concat ","
       (List.map Parametric_type.to_string application.type_arguments))
    (String.concat "," (List.map argument application.arguments))
    (Parametric_type.to_string application.result_type)
