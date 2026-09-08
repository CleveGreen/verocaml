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

let declaration_material ~canonical_path ~value_uid =
  framed "symbolic-origin-v1" [ canonical_path; value_uid ]

let rec abi_type_material = function
  | Parametric_type.Unit -> Ok "u"
  | Bool -> Ok "b"
  | Int -> Ok "i"
  | Mathematical_int -> Ok "I"
  | Bit_vector width ->
      Ok
        (framed "B"
           [ Parametric_type.structural_identity_material
               (Parametric_type.Bit_vector width) ])
  | Parameter binder ->
      Ok (framed "p" [ string_of_int binder.Parametric_type.ordinal ])
  | Application (constructor, arguments) ->
      let rec loop lowered = function
        | [] -> Ok (List.rev lowered)
        | argument :: rest ->
            Result.bind (abi_type_material argument) (fun material ->
                loop (material :: lowered) rest)
      in
      Result.map
        (fun arguments ->
          framed "a"
            [
              constructor.Parametric_type.constructor_identity;
              framed "v" arguments;
            ])
        (loop [] arguments)
  | Tuple components ->
      let rec loop lowered = function
        | [] -> Ok (List.rev lowered)
        | (label, component) :: rest ->
            Result.bind (abi_type_material component) (fun material ->
                loop
                  (framed "c"
                     [ Option.value ~default:"" label; material ]
                  :: lowered)
                  rest)
      in
      Result.map (framed "t") (loop [] components)
  | Aggregate _ -> Error "symbolic ABI contains an aggregate identity"

let declaration_abi_material ~canonical_path ~value_uid ~type_binders
    ~parameter_labels ~parameter_types ~result_type =
  if List.length parameter_labels <> List.length parameter_types then
    Error "symbolic ABI label and parameter vectors differ"
  else
    let binder_ordinals =
      List.map (fun binder -> binder.Parametric_type.ordinal) type_binders
    in
    if binder_ordinals <> List.init (List.length binder_ordinals) Fun.id then
      Error "symbolic ABI binders are not canonical and ordered"
    else
      let valid_parameter binder =
        binder.Parametric_type.ordinal >= 0
        && binder.ordinal < List.length type_binders
      in
      let all_types = result_type :: parameter_types in
      if
        List.exists
          (fun typ ->
            List.exists (fun binder -> not (valid_parameter binder))
              (Parametric_type.parameters typ))
          all_types
      then Error "symbolic ABI type escapes its binder vector"
      else
        let rec lower_parameters lowered labels types =
          match (labels, types) with
          | [], [] -> Ok (List.rev lowered)
          | label :: labels, typ :: types ->
              Result.bind (abi_type_material typ) (fun material ->
                  lower_parameters
                    (framed "f" [ label; material ] :: lowered)
                    labels types)
          | [], _ :: _ | _ :: _, [] -> assert false
        in
        Result.bind
          (lower_parameters [] parameter_labels parameter_types)
          (fun parameters ->
            Result.map
              (fun result ->
                framed "symbolic-abi-v1"
                  [
                    canonical_path;
                    value_uid;
                    framed "binders"
                      (List.map string_of_int binder_ordinals);
                    framed "parameters" parameters;
                    result;
                  ])
              (abi_type_material result_type))

let validate_declaration_type typ =
  let rec supported = function
    | Parametric_type.Unit | Int | Mathematical_int | Bool | Parameter _ ->
        Ok ()
    | Bit_vector width ->
        Bv_width.authenticate_bound
          (Bv_backend_capability_receipt_private.capability ())
          width
        |> Result.map_error (fun message ->
               "symbolic declaration bit-vector width is not authenticated: "
               ^ message)
    | Application (constructor, arguments) ->
        if
          constructor.constructor_path = ""
          || constructor.constructor_identity = ""
        then Error "symbolic declaration type constructor identity is empty"
        else
          List.fold_left
            (fun result argument -> Result.bind result (fun () -> supported argument))
            (Ok ()) arguments
    | Tuple _ | Aggregate _ ->
        Error "symbolic declaration contains an unsupported aggregate type"
  in
  supported typ

let validate_declaration_types types =
  List.fold_left
    (fun result typ ->
      Result.bind result (fun () -> validate_declaration_type typ))
    (Ok ()) types

let authenticate_instantiated_type typ =
  let rec authenticate = function
    | Parametric_type.Unit | Int | Mathematical_int | Bool | Parameter _
    | Aggregate _ ->
        Ok ()
    | Bit_vector width -> (
        match
          Bv_width.authenticate_bound
            (Bv_backend_capability_receipt_private.capability ())
            width
        with
        | Ok () ->
            [%log.trace "authenticated symbolic application BV width"
              ~stage:(Delator.Field.string "application-type-authentication")
              ~width:(Delator.Field.int (Bv_width.to_int width))
              ~decision:(Delator.Field.string "accepted")];
            Ok ()
        | Error message ->
            [%log.debug "rejected symbolic application BV width"
              ~stage:(Delator.Field.string "application-type-authentication")
              ~width:(Delator.Field.int (Bv_width.to_int width))
              ~reason:(Delator.Field.string message)
              ~decision:(Delator.Field.string "rejected")];
            Error
              ("symbolic application bit-vector width is not authenticated: "
              ^ message))
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

let authenticate_instantiated_types types =
  List.fold_left
    (fun result typ ->
      Result.bind result (fun () -> authenticate_instantiated_type typ))
    (Ok ()) types

let declare ~marker_id ~declaration_index ~declaration_name ~canonical_path
    ~value_uid ~source_file ~compilation_identity ~declaration_span
    ~type_binders ~parameter_types ~result_type =
  if String.length marker_id = 0 || String.length declaration_name = 0 then
    Error "symbolic declaration identity is empty"
  else if List.exists (fun binder -> binder.Parametric_type.ordinal < 0) type_binders
  then Error "symbolic declaration has an invalid type binder"
  else
    Result.bind
      (Result.bind
         (validate_declaration_types parameter_types)
         (fun () -> validate_declaration_type result_type))
      (fun () ->
        let identity_material =
          declaration_material ~canonical_path ~value_uid
        in
        [%log.trace "constructed semantic symbolic declaration identity"
          ~stage:(Delator.Field.string "declaration-identity")
          ~correlation:
            (Delator.Field.string
               (Digest.to_hex (Digest.string identity_material)))
          ~binder_count:(Delator.Field.int (List.length type_binders))
          ~parameter_count:(Delator.Field.int (List.length parameter_types))
          ~decision:(Delator.Field.string "constructed")];
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
          })

let rebase_declaration (declaration : declaration) ~declaration_index ~declaration_name
    ~canonical_path ~value_uid ~type_binders ~parameter_types ~result_type =
  Result.map
    (fun (rebased : declaration) ->
      { rebased with identity_material = declaration.identity_material })
    (declare ~marker_id:declaration.marker_id ~declaration_index
       ~declaration_name ~canonical_path ~value_uid
       ~source_file:declaration.source_file
       ~compilation_identity:declaration.compilation_identity
       ~declaration_span:declaration.declaration_span ~type_binders
       ~parameter_types ~result_type)

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

let application_material (declaration : declaration) type_arguments =
  framed "symbolic-head-v1"
    [
      declaration.identity_material;
      Parametric_type.structural_vector_material type_arguments;
    ]

let validate_instantiation declaration ~type_arguments ~argument_types
    ~result_type =
  Result.bind (instantiate declaration type_arguments)
    (fun (expected_arguments, expected_result) ->
      if
        not (List.equal Parametric_type.equal expected_arguments argument_types)
      then Error "symbolic application argument types are not exact"
      else if not (Parametric_type.equal expected_result result_type) then
        Error "symbolic application result type is not exact"
      else Ok (application_material declaration type_arguments))

let symbol_name_for_application declaration ~type_arguments ~argument_types
    ~result_type =
  Result.map
    (fun _material ->
      let presentation_material =
        framed "symbolic-presentation-v1"
          [
            declaration.canonical_path;
            Parametric_type.structural_vector_material type_arguments;
          ]
      in
      let default =
        "vero_symbolic_"
        ^ Digest.to_hex (Digest.string presentation_material)
      in
      Symbolic_application_collision_testing_private.select_presentation_name
        ~declaration_name:declaration.declaration_name ~default)
    (validate_instantiation declaration ~type_arguments ~argument_types
       ~result_type)

let create declaration ~type_arguments ~arguments ~argument_types ~result_type
    ~span =
  if List.length arguments <> List.length argument_types then
    Error "symbolic application argument and type vectors differ"
  else
    Result.bind (authenticate_instantiated_types type_arguments) (fun () ->
    Result.bind (authenticate_instantiated_types argument_types) (fun () ->
    Result.bind (authenticate_instantiated_type result_type) (fun () ->
    Result.bind (instantiate declaration type_arguments)
      (fun (expected_arguments, expected_result) ->
        if
          not
            (List.equal Parametric_type.equal expected_arguments argument_types)
        then Error "symbolic application argument types are not exact"
        else if not (Parametric_type.equal expected_result result_type) then
          Error "symbolic application result type is not exact"
        else
          let identity_material =
            application_material declaration type_arguments
          in
          [%log.trace "correlated semantic symbolic application identity"
            ~stage:(Delator.Field.string "application-identity")
            ~correlation:
              (Delator.Field.string
                 (Digest.to_hex (Digest.string identity_material)))
            ~type_arity:(Delator.Field.int (List.length type_arguments))
            ~term_arity:(Delator.Field.int (List.length arguments))
            ~decision:(Delator.Field.string "correlated")];
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
               ~argument_types ~result_type)))))

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
let declaration_identity_material (declaration : declaration) =
  declaration.identity_material
let type_arguments application = application.type_arguments
let arguments application = application.arguments
let argument_types application = application.argument_types
let result_type application = application.result_type
let span application = application.span
let identity_material application = application.identity_material
let identity_digest application =
  Digest.to_hex (Digest.string application.identity_material)
let symbol_name application = application.symbol_name

let backend_head_lock = Mutex.create ()
let backend_head_materials = Hashtbl.create 127

let hex_encode material =
  let encoded = Bytes.create (String.length material * 2) in
  let hex = "0123456789abcdef" in
  String.iteri
    (fun index character ->
      let code = Char.code character in
      Bytes.set encoded (index * 2) hex.[code lsr 4];
      Bytes.set encoded ((index * 2) + 1) hex.[code land 0xf])
    material;
  Bytes.unsafe_to_string encoded

let intern_backend_head material =
  let compact =
    Printf.sprintf "vero_symbolic_v3_%08x" (Hashtbl.hash material)
  in
  Mutex.lock backend_head_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock backend_head_lock)
    (fun () ->
      match Hashtbl.find_opt backend_head_materials compact with
      | None ->
          Hashtbl.add backend_head_materials compact material;
          (compact, "inserted")
      | Some existing when String.equal existing material ->
          (compact, "reused")
      | Some _ ->
          let collision_safe = compact ^ "_" ^ hex_encode material in
          Hashtbl.replace backend_head_materials collision_safe material;
          (collision_safe, "collision-fallback"))

let backend_head_for_instantiation declaration ~type_arguments ~argument_types
    ~result_type =
  Result.map
    (fun material -> fst (intern_backend_head material))
    (validate_instantiation declaration ~type_arguments ~argument_types
       ~result_type)

let backend_head application =
  let backend_head, _interning_decision =
    intern_backend_head application.identity_material
  in
  [%log.trace "selecting authenticated symbolic backend head"
    ~stage:(Delator.Field.string "backend-interning")
    ~declaration_name:
      (Delator.Field.string application.declaration.declaration_name)
    ~correlation:(Delator.Field.string (identity_digest application))
    ~type_arity:(Delator.Field.int (List.length application.type_arguments))
    ~term_arity:(Delator.Field.int (List.length application.arguments))
    ~decision:(Delator.Field.string _interning_decision)];
  Symbolic_application_collision_testing_private.observe_backend_head
    ~declaration_name:application.declaration.declaration_name
    ~identity_digest:(identity_digest application)
    ~presentation_name:application.symbol_name ~backend_head;
  backend_head

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
