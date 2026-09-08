type t = {
  constant_id : Sst.logical_constant_id;
  type_arguments : Parametric_type.t list;
  result_type : Parametric_type.t;
  span : Diagnostic.span;
  identity_material : string;
  identity_digest : string;
  backend_head : string;
}

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let instance_material constant type_arguments =
  let origin = constant.Sst.constant_origin in
  framed "logical-constant-instance-v1"
    [ origin.semantic_class; origin.provider_unit; origin.provider_interface;
      origin.value_uid; origin.declaration_marker; origin.canonical_path;
      Parametric_type.structural_vector_material type_arguments ]

let create ~definition ~type_arguments ~result_type ~span =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let* () =
    List.fold_left
      (fun result argument ->
        let* () = result in
        Logical_constant_private.validate_instance_type argument)
      (Ok ()) type_arguments
  in
  let* expected =
    Parametric_type.instantiate definition.Sst.constant_type_binders
      type_arguments definition.constant_declared_type
  in
  let* () = Logical_constant_private.validate_instance_type expected in
  let* () = Logical_constant_private.validate_instance_type result_type in
  if not (Parametric_type.equal expected result_type) then
    Error "logical constant instance result type differs from its definition"
  else
      let identity_material =
        instance_material definition.constant_id type_arguments
      in
      let identity_digest =
        Digest.string identity_material |> Digest.to_hex
      in
      let backend_head = "vero_logical_constant_v1_" ^ identity_digest in
      [%log.trace "created logical constant instance"
        ~stage:(Delator.Field.string "logical-constant-instance")
        ~constant_name:
          (Delator.Field.string definition.constant_id.constant_name)
        ~type_argument_count:(Delator.Field.int (List.length type_arguments))
        ~result_type:
          (Delator.Field.string (Parametric_type.to_string result_type))
        ~identity_digest:(Delator.Field.string identity_digest)
        ~backend_head:(Delator.Field.string backend_head)
        ~decision:(Delator.Field.string "created")];
      Ok
        {
          constant_id = definition.constant_id;
          type_arguments;
          result_type;
          span;
          identity_material;
          identity_digest;
          backend_head;
        }

let constant_id instance = instance.constant_id
let type_arguments instance = instance.type_arguments
let result_type instance = instance.result_type
let span instance = instance.span
let identity_material instance = instance.identity_material
let identity_digest instance = instance.identity_digest
let backend_head instance = instance.backend_head
let compare left right = String.compare left.identity_material right.identity_material
let equal left right = compare left right = 0
