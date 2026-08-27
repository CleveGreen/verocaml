let () = ignore Parametric_adts_prerequisites.ready
let fail fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 3) fmt
let load file =
  match Typedtree_lowering.lower_file file with
  | Ok program -> program
  | Error diagnostic -> fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let descriptor_unit file =
  let owner = Parametric_type.owner ~index:41 ~name:"Local.box" in
  let binder = Parametric_type.binder owner ~ordinal:0 in
  let type_id = { Parametric_type.type_index = 41; type_name = "Local.box" } in
  let type_constructor =
    { Parametric_type.constructor_path = "Local.box";
      constructor_identity = "compiler-uid:local-box" }
  in
  let field ?(index = 0) ?(typ = Parametric_type.Parameter binder) () =
    { Parametric_adt.field_index = index; field_name = "payload";
      field_uid = "field:payload"; field_type = typ; field_mutable = false }
  in
  let constructor ?(index = 0) fields =
    { Parametric_adt.constructor_index = index; constructor_name = "Box";
      constructor_uid = "constructor:Box"; constructor_fields = fields }
  in
  let create ?(id = type_id) ?(ctor = type_constructor) ?(binders = [ binder ])
      ?(provenance = Parametric_adt.Local { compiler_uid = "local-box" }) kind =
    Parametric_adt.create ~type_id:id ~type_constructor:ctor ~binders ~provenance
      ~kind
  in
  let show label = function
    | Ok _ -> Printf.printf "%s accepted\n" label
    | Error error -> Printf.printf "%s rejected: %s\n" label error.Parametric_adt.message
  in
  show "canonical-local"
    (create (Parametric_adt.Variant [ constructor [ field () ] ]));
  let wrong_order = Parametric_type.binder owner ~ordinal:1 in
  show "binder-order"
    (create ~binders:[ wrong_order ]
       (Parametric_adt.Variant [ constructor [ field ~typ:(Parameter wrong_order) () ] ]));
  let foreign_owner = Parametric_type.owner ~index:42 ~name:"Foreign.box" in
  let foreign = Parametric_type.binder foreign_owner ~ordinal:0 in
  show "binder-owner"
    (create ~binders:[ foreign ]
       (Parametric_adt.Variant [ constructor [ field ~typ:(Parameter foreign) () ] ]));
  show "constructor-order"
    (create (Parametric_adt.Variant [ constructor ~index:1 [ field () ] ]));
  show "field-order"
    (create (Parametric_adt.Variant [ constructor [ field ~index:1 () ] ]));
  show "field-template"
    (create (Parametric_adt.Variant [ constructor [ field ~typ:(Parameter foreign) () ] ]));
  show "forged-option"
    (create
       ~id:{ Parametric_type.type_index = -1; type_name = "Stdlib.option" }
       ~provenance:(Parametric_adt.Pinned_option { compiler_uid = "<predef:option>" })
       (Parametric_adt.Variant [ constructor []; constructor ~index:1 [ field () ] ]));
  let backend_before = Solver_backend.For_testing.solver_creation_count () in
  let z3_before = Z3_bridge.counters () in
  let descriptors = (load file).Sst.parametric_adts in
  let standard_descriptor expected =
    List.find
      (fun descriptor -> expected (Parametric_adt.provenance descriptor))
      descriptors
  in
  let canonical_option =
    standard_descriptor (function Parametric_adt.Pinned_option _ -> true | _ -> false)
  in
  let canonical_list =
    standard_descriptor (function Parametric_adt.Pinned_list _ -> true | _ -> false)
  in
  let canonical_result =
    standard_descriptor (function Parametric_adt.Pinned_result _ -> true | _ -> false)
  in
  let recreate ?type_id ?provenance descriptor =
    Parametric_adt.create
      ~type_id:(Option.value type_id ~default:(Parametric_adt.type_id descriptor))
      ~type_constructor:(Parametric_adt.type_constructor descriptor)
      ~binders:(Parametric_adt.binders descriptor)
      ~provenance:
        (Option.value provenance ~default:(Parametric_adt.provenance descriptor))
      ~kind:(Parametric_adt.kind descriptor)
  in
  let show_standard label ~name ~uid result =
    match result with
    | Ok _ -> Printf.printf "%s name=%s uid=%s accepted\n" label name uid
    | Error error ->
        Printf.printf "%s name=%s uid=%s rejected: %s\n" label name uid
          error.Parametric_adt.message
  in
  let canonical label descriptor =
    let type_id = Parametric_adt.type_id descriptor in
    show_standard label ~name:type_id.type_name
      ~uid:(Parametric_adt.compiler_uid descriptor)
      (recreate descriptor)
  in
  canonical "canonical-option" canonical_option;
  canonical "canonical-list" canonical_list;
  canonical "canonical-result" canonical_result;
  let forge_name label forged_name descriptor =
    let canonical_id = Parametric_adt.type_id descriptor in
    let forged_id = { canonical_id with type_name = forged_name } in
    show_standard label ~name:forged_name
      ~uid:(Parametric_adt.compiler_uid descriptor)
      (recreate ~type_id:forged_id descriptor)
  in
  forge_name "forged-option-name" "<forged:option>" canonical_option;
  forge_name "forged-list-name" "<forged:list>" canonical_list;
  forge_name "forged-result-name" "<forged:result-name>" canonical_result;
  let forged_uid = "<forged:result>" in
  show_standard "forged-result" ~name:(Parametric_adt.type_id canonical_result).type_name
    ~uid:forged_uid
    (recreate canonical_result
       ~provenance:(Parametric_adt.Pinned_result { compiler_uid = forged_uid }));
  let backend_after = Solver_backend.For_testing.solver_creation_count () in
  let z3_after = Z3_bridge.counters () in
  if backend_before <> backend_after || z3_before <> z3_after then
    fail "pure descriptor construction reached backend or Z3 work";
  Printf.printf
    "descriptor-boundary=Parametric_adt.create downstream=none backend-delta=0 z3-delta=0\n"

let () =
  match Array.to_list Sys.argv with
  | [_; "sst"; file] -> print_string (Sst.to_string (load file))
  | [_; "vir"; file] ->
      let program = load file in
      (match Symbolic_executor_private.lower_program program with
       | Ok vir -> print_string (Vir.to_string vir)
       | Error error -> fail "%s" (Symbolic_executor_private.error_to_string error))
  | [_; "standards"; file] ->
      let cmt = Cmt_format.read_cmt file in
      let structure =
        match cmt.cmt_annots with
        | Cmt_format.Implementation structure -> structure
        | _ -> fail "implementation required"
      in
      let paths = ref [] in
      let default = Tast_iterator.default_iterator in
      let observe typ =
        match Types.get_desc typ with
        | Types.Tconstr (path, _, _)
          when not (List.exists (Path.same path) !paths) ->
            paths := path :: !paths
        | _ -> ()
      in
      let iterator =
        { default with
          expr =
            (fun self expression ->
              observe expression.Typedtree.exp_type;
              default.expr self expression);
          pat =
            (fun self pattern ->
              observe pattern.Typedtree.pat_type;
              default.pat self pattern) }
      in
      iterator.structure iterator structure;
      (match
         Parametric_adt_lowering_private.standard_sources ~observed_paths:!paths
           structure.Typedtree.str_final_env
       with
       | Ok sources -> Printf.printf "%d\n" (List.length sources)
       | Error message -> fail "%s" message)
  | [_; "env-types"; file] ->
      let cmt = Cmt_format.read_cmt file in
      let structure =
        match cmt.cmt_annots with
        | Cmt_format.Implementation structure -> structure
        | _ -> fail "implementation required"
      in
      let show label prefix =
        Env.fold_types
          (fun name path declaration () ->
            if List.length declaration.Types.type_params = 2 then
              Printf.printf "%s%s %s %s\n" label name
                (Path.name path)
                (Format.asprintf "%a" Types.Uid.print declaration.type_uid))
          prefix structure.Typedtree.str_final_env ()
      in
      show "root:" None;
      (try show "stdlib:" (Some (Longident.Lident "Stdlib"))
       with Not_found -> ())
  | [_; "descriptor-unit"; file] -> descriptor_unit file
  | [_; "descriptors"; file] ->
      let program = load file in
      List.iter (fun descriptor -> print_endline (Parametric_adt.to_string descriptor))
        program.Sst.parametric_adts
  | [_; "paths"; file] ->
      let cmt = Cmt_format.read_cmt file in
      let structure =
        match cmt.cmt_annots with
        | Cmt_format.Implementation structure -> structure
        | _ -> fail "implementation required"
      in
      let iterator =
        { Tast_iterator.default_iterator with
          expr =
            (fun self expression ->
              (match Types.get_desc expression.Typedtree.exp_type with
               | Types.Tconstr (path, arguments, _) ->
                   Printf.printf "%s/%d head=%s persistent=%b\n"
                     (Path.name path) (List.length arguments)
                     (Format.asprintf "%a" Ident.print_with_scope
                        (Path.head path))
                     (Ident.same (Path.head path)
                        (Ident.create_persistent "Stdlib"))
               | _ -> ());
              Tast_iterator.default_iterator.expr self expression) }
      in
      iterator.structure iterator structure
  | _ -> fail "usage"
