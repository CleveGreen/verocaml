let () = ignore Logical_adt_schema_prerequisites.ready

let fail format =
  Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span =
  Diagnostic.
    { file = "logical_adt_schema_tool.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 } }

let get = function
  | Ok value -> value
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

let descriptor ~index ~name ~kind =
  let owner = Parametric_type.owner ~index ~name in
  let binder = Parametric_type.binder owner ~ordinal:0 in
  let type_id = { Parametric_type.type_index = index; type_name = name } in
  let type_constructor =
    { Parametric_type.constructor_path = name;
      constructor_identity = "uid:" ^ name }
  in
  let descriptor =
    Parametric_adt.create ~type_id ~type_constructor ~binders:[ binder ]
      ~provenance:(Parametric_adt.Local { compiler_uid = "uid:" ^ name })
      ~kind:(kind binder type_constructor)
    |> function
    | Ok descriptor -> descriptor
    | Error error -> fail "descriptor: %s" error.Parametric_adt.message
  in
  (descriptor, binder, type_id)

let field index name typ =
  { Parametric_adt.field_index = index;
    field_name = name;
    field_uid = Printf.sprintf "field:%s:%d" name index;
    field_type = typ;
    field_mutable = false }

let constructor index name fields =
  { Parametric_adt.constructor_index = index;
    constructor_name = name;
    constructor_uid = Printf.sprintf "constructor:%s:%d" name index;
    constructor_fields = fields }

let choice_descriptor () =
  descriptor ~index:55 ~name:"Choice"
    ~kind:(fun binder _ ->
      Parametric_adt.Variant
        [ constructor 0 "None" [];
          constructor 1 "Some"
            [ field 0 "$arg0" (Parametric_type.Parameter binder) ] ])

let list_descriptor () =
  descriptor ~index:56 ~name:"Chain"
    ~kind:(fun binder self ->
      Parametric_adt.Variant
        [ constructor 0 "Nil" [];
          constructor 1 "Cons"
            [ field 0 "$arg0" (Parametric_type.Parameter binder);
              field 1 "$arg1"
                (Parametric_type.Application
                   (self, [ Parametric_type.Parameter binder ])) ] ])

let schemas descriptors applications =
  match
    Logical_adt_schema_private.instantiate ~descriptors ~applications
  with
  | Ok schemas -> schemas
  | Error error ->
      fail "%s" (Logical_adt_schema_private.error_to_string error)

let declare_sorts builder =
  let declared = Hashtbl.create 4 in
  fun binder ->
    let key = Parametric_type.binder_to_string binder in
    match Hashtbl.find_opt declared key with
    | Some sort -> sort
    | None ->
        let sort =
          Logic_ir.declare_sort builder
            ~name:("Payload_" ^ Digest.to_hex (Digest.string key))
            ~span
          |> get
        in
        Hashtbl.add declared key sort;
        sort

let constant builder name sort =
  let symbol =
    Logic_ir.declare_function builder ~name ~domain:[] ~range:sort ~span
    |> get
  in
  Logic_ir.apply ~span symbol [] |> get

let query builder assertion =
  Logic_ir.query builder ~axioms:[] ~assertions:[ assertion ] ~requires:[]
    ~span
  |> get

let solve label query expected =
  let config : Z3_bridge.config = { timeout_ms = 5000; model = true } in
  let actual =
    match Z3_bridge.solve_query config query with
    | Ok Z3_bridge.Verified -> "verified"
    | Ok (Counterexample _) -> "counterexample"
    | Ok (Inconclusive _) -> "inconclusive"
    | Error error -> fail "%s" (Z3_bridge.error_to_string error)
  in
  if actual <> expected then fail "%s: expected %s, got %s" label expected actual;
  Printf.printf "%s=%s\n" label actual

let choice_query () =
  let descriptor, _, type_id = choice_descriptor () in
  let payload_owner = Parametric_type.owner ~index:90 ~name:"Abstract" in
  let payload = Parametric_type.binder payload_owner ~ordinal:0 in
  let schema =
    schemas [ descriptor ] [ (type_id.type_index, [ Parameter payload ]) ]
  in
  let builder = Logic_ir.create () in
  let payload_sort = declare_sorts builder in
  let encoding =
    Logical_adt_encoding_private.declare ~builder ~schemas:schema
      ~aggregate_sort:(fun _ -> fail "foreign aggregate dependency")
      ~parametric_sort:payload_sort ~span
    |> function Ok value -> value | Error message -> fail "%s" message
  in
  let aggregate =
    Logical_adt_encoding_private.aggregate_type (List.hd schema)
  in
  let owner = { Sst.type_index = type_id.type_index; type_name = type_id.type_name } in
  let none = { Sst.constructor_type = owner; constructor_index = 0; constructor_name = "None" }
  and some = { Sst.constructor_type = owner; constructor_index = 1; constructor_name = "Some" } in
  let none_symbol =
    Option.get (Logical_adt_encoding_private.constructor encoding aggregate none)
  and some_symbol =
    Option.get (Logical_adt_encoding_private.constructor encoding aggregate some)
  in
  let payload_sort = payload_sort payload in
  let left = constant builder "abstract_left" payload_sort
  and right = constant builder "abstract_right" payload_sort in
  let none = Logic_ir.apply ~span none_symbol [] |> get
  and some_left = Logic_ir.apply ~span some_symbol [ left ] |> get
  and some_right = Logic_ir.apply ~span some_symbol [ right ] |> get in
  let injectivity =
    Logic_ir.and_ ~span
      [ Logic_ir.equal ~span some_left some_right |> get;
        Logic_ir.distinct ~span left right |> get ]
    |> get
  in
  let discrimination = Logic_ir.equal ~span some_left none |> get in
  (query builder injectivity, query builder discrimination)

let recursive_query () =
  let descriptor, _, type_id = list_descriptor () in
  let schema = schemas [ descriptor ] [ (type_id.type_index, [ Int ]) ] in
  let builder = Logic_ir.create () in
  let encoding =
    Logical_adt_encoding_private.declare ~builder ~schemas:schema
      ~aggregate_sort:(fun _ -> fail "foreign aggregate dependency")
      ~parametric_sort:(declare_sorts builder) ~span
    |> function Ok value -> value | Error message -> fail "%s" message
  in
  let aggregate =
    Logical_adt_encoding_private.aggregate_type (List.hd schema)
  in
  let nil = Option.get (Logical_adt_encoding_private.constructor_at encoding aggregate 0)
  and cons = Option.get (Logical_adt_encoding_private.constructor_at encoding aggregate 1) in
  let nil_term = Logic_ir.apply ~span nil [] |> get in
  let cons_term =
    Logic_ir.apply ~span cons
      [ Logic_ir.int ~span Z.one; nil_term ]
    |> get
  in
  query builder (Logic_ir.equal ~span nil_term cons_term |> get)

let malformed () =
  let descriptor, _, type_id =
    descriptor ~index:57 ~name:"Changing"
      ~kind:(fun _ self ->
        Parametric_adt.Variant
          [ constructor 0 "Next"
              [ field 0 "$arg0"
                  (Parametric_type.Application (self, [ Int ])) ] ])
  in
  match
    Logical_adt_schema_private.instantiate ~descriptors:[ descriptor ]
      ~applications:[ (type_id.type_index, [ Bool ]) ]
  with
  | Error _ -> print_endline "nonuniform-recursion=rejected"
  | Ok _ -> fail "nonuniform recursion unexpectedly accepted"

let () =
  let injectivity, discrimination = choice_query () in
  solve "abstract-injectivity" injectivity "verified";
  solve "false-supported-identity" discrimination "verified";
  let false_vc =
    let builder = Logic_ir.create () in
    let value = Logic_ir.bool ~span false in
    query builder (Logic_ir.not_ ~span value |> get)
  in
  solve "ordinary-failed-vc" false_vc "counterexample";
  solve "recursive-discrimination" (recursive_query ()) "verified";
  let detached = Z3_bridge.detach_query injectivity in
  let attempt =
    Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
      ~timeout_ms:5000 ~rlimit:200000 ~model:true detached
  in
  (match attempt.detached_result with
  | Ok Z3_bridge.Detached_verified ->
      print_endline "detached-native-reconstruction=verified"
  | Ok _ | Error _ -> fail "detached datatype reconstruction failed");
  malformed ()
