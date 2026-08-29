type standard = Option | List | Result

type source = {
  paths : Path.t list;
  type_id : Sst.type_id;
  name : string;
  declaration : Types.type_declaration;
  provenance : Parametric_adt.provenance;
  standard : standard option;
}

type lowered = {
  descriptor : Parametric_adt.t;
  definition : Sst.type_definition;
  paths : Path.t list;
  fields : (Types.Uid.t * Sst.type_id * Sst.field_id) list;
  constructors : (Types.Uid.t * Sst.type_id * Sst.constructor_id) list;
}

let is_stdlib_result_path = function
  | Path.Pdot (Path.Pident root, "result") ->
      Ident.same root (Ident.create_persistent "Stdlib")
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false

let standard_of_path path =
  if Path.same path Predef.path_option then Some Option
  else if Path.same path Predef.path_list then Some List
  else if is_stdlib_result_path path then Some Result
  else None

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid

let local_source ~paths ~type_id declaration =
  let compiler_uid = compiler_uid declaration.Typedtree.typ_type.type_uid in
  {
    paths;
    type_id;
    name = declaration.typ_name.txt;
    declaration = declaration.typ_type;
    provenance = Parametric_adt.Local { compiler_uid };
    standard = None;
  }

let direct_type_parameters parameters arguments =
  List.length parameters = List.length arguments
  && List.for_all2
       (fun (parameter, _) argument ->
         match Types.get_desc argument.Typedtree.ctyp_type with
         | Types.Tvar _ | Types.Tunivar _ ->
             Types.get_id parameter.Typedtree.ctyp_type
             = Types.get_id argument.ctyp_type
         | Types.Tpoly _ | Types.Tlink _ | Types.Tsubst _ | Types.Tconstr _
         | Types.Tarrow _ | Types.Ttuple _ | Types.Tunboxed_tuple _
         | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
         | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
         | Types.Tof_kind _ ->
             false)
       parameters arguments

let direct_compiler_parameters parameters arguments =
  List.length parameters = List.length arguments
  && List.for_all2
       (fun parameter argument ->
         match Types.get_desc argument with
         | Types.Tvar _ | Types.Tunivar _ ->
             Types.get_id parameter = Types.get_id argument
         | Types.Tpoly _ | Types.Tlink _ | Types.Tsubst _ | Types.Tconstr _
         | Types.Tarrow _ | Types.Ttuple _ | Types.Tunboxed_tuple _
         | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
         | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
         | Types.Tof_kind _ ->
             false)
       parameters arguments

let load_path_mutex = Mutex.create ()

let with_load_path ~visible ~hidden action =
  Mutex.lock load_path_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock load_path_mutex)
    (fun () ->
      let saved = Load_path.get_paths () in
      let restore () =
        Load_path.init ~auto_include:Load_path.no_auto_include
          ~visible:saved.visible ~hidden:saved.hidden;
        Envaux.reset_cache ~preserve_persistent_env:false
      in
      Fun.protect ~finally:restore (fun () ->
          let visible =
            Config.standard_library :: visible |> List.sort_uniq String.compare
          in
          Load_path.init ~auto_include:Load_path.no_auto_include ~visible
            ~hidden;
          Envaux.reset_cache ~preserve_persistent_env:false;
          action ()))

let external_source ~paths ~type_id ~load_path_visible ~load_path_hidden _env
    declaration =
  if
    declaration.Typedtree.typ_cstrs <> []
    || declaration.typ_private <> Asttypes.Public
    || declaration.typ_kind <> Typedtree.Ttype_abstract
  then Error "external type specification proxy is not a transparent alias"
  else
    match declaration.typ_manifest with
    | Some
        ({ ctyp_desc = Typedtree.Ttyp_constr (target_path, _, arguments); _ } as
         manifest)
      when direct_type_parameters declaration.typ_params arguments -> (
        let target =
          with_load_path ~visible:load_path_visible ~hidden:load_path_hidden
            (fun () ->
              try
                let hydrated =
                  Envaux.env_of_only_summary ~allow_missing_modules:false
                    manifest.ctyp_env
                in
                let canonical_path =
                  Env.normalize_type_path (Some manifest.ctyp_loc) hydrated
                    target_path
                in
                let target = Env.find_type canonical_path hydrated in
                let rec representation seen paths path declaration =
                  if List.exists (Path.same path) seen then
                    (path, declaration, List.rev paths)
                  else
                    match declaration.Types.type_manifest with
                    | Some manifest_type -> (
                        match Types.get_desc manifest_type with
                        | Types.Tconstr (next, arguments, _)
                          when direct_compiler_parameters
                                 declaration.type_params arguments -> (
                            let next =
                              try
                                Env.normalize_type_path
                                  (Some manifest.ctyp_loc) hydrated next
                              with Env.Error _ -> next
                            in
                            if Path.same path next then
                              (path, declaration, List.rev paths)
                            else
                              try
                                representation (path :: seen) (next :: paths)
                                  next (Env.find_type next hydrated)
                              with Not_found | Env.Error _ ->
                                (path, declaration, List.rev paths))
                        | Types.Tconstr _ | Types.Tvar _ | Types.Tunivar _
                        | Types.Tpoly _
                        | Types.Tlink _ | Types.Tsubst _ | Types.Tarrow _
                        | Types.Ttuple _ | Types.Tunboxed_tuple _
                        | Types.Tobject _ | Types.Tfield _ | Types.Tnil
                        | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _
                        | Types.Tsplice _ | Types.Tof_kind _ ->
                            (path, declaration, List.rev paths))
                    | None -> (path, declaration, List.rev paths)
                in
                let canonical_path, target, representation_paths =
                  representation [] [ canonical_path ] canonical_path target
                in
                Ok (canonical_path, target, representation_paths)
              with Not_found | Env.Error _ | Envaux.Error _ ->
                Error
                  "external type specification target cannot be resolved")
        in
        match target with
        | Error _ as error -> error
        | Ok (canonical_path, target, representation_paths)
          when target.Types.type_private = Asttypes.Public
               && List.length target.Types.type_params
               = List.length declaration.typ_params ->
            let target_uid = compiler_uid target.type_uid in
            let proxy_uid = compiler_uid declaration.typ_type.type_uid in
            let paths =
              canonical_path :: target_path :: representation_paths @ paths
              |> List.sort_uniq Path.compare
            in
            Ok
              {
                paths;
                type_id;
                name = Path.name canonical_path;
                declaration = target;
                provenance =
                  Parametric_adt.External
                    { compiler_uid = target_uid; proxy_uid; prelude = false };
                standard = standard_of_path canonical_path;
              }
        | Ok (_, target, _)
          when target.Types.type_private <> Asttypes.Public ->
            Error "external type specification target is private"
        | Ok _ ->
            Error
              "external type specification target arity differs from its proxy")
    | Some _ | None ->
        Error "external type specification proxy is not a direct type alias"

let covers_path (source : source) path =
  List.exists (fun candidate -> Path.same candidate path) source.paths

let standard_proxy_uid = function
  | Option -> "verocaml-pervasive:option_specification"
  | List -> "verocaml-pervasive:list_specification"
  | Result -> "verocaml-pervasive:result_specification"

let standard_source env standard type_id path name =
  let declaration = Env.find_type path env in
  let compiler_uid = compiler_uid declaration.Types.type_uid in
  let provenance =
    Parametric_adt.External
      {
        compiler_uid;
        proxy_uid = standard_proxy_uid standard;
        prelude = true;
      }
  in
  { paths = [ path ]; type_id; name; declaration; provenance; standard = Some standard }

let standard_sources ?(observed_paths = []) env =
  let predef_env =
    Predef.build_initial_env
      (Env.add_type ~check:false)
      (Env.add_extension ~check:false ~rebind:false)
      Env.empty
  in
  let option =
    standard_source predef_env Option
      { Sst.type_index = -1; type_name = "Stdlib.option" }
      Predef.path_option "Stdlib.option"
      |> fun source -> { source with declaration = Env.find_type Predef.path_option predef_env }
  in
  let list =
    standard_source predef_env List
      { Sst.type_index = -2; type_name = "Stdlib.list" }
      Predef.path_list "Stdlib.list"
      |> fun source -> { source with declaration = Env.find_type Predef.path_list predef_env }
  in
  let observed path = List.exists (Path.same path) observed_paths in
  let result =
    match
      List.find_opt
        is_stdlib_result_path
        observed_paths
    with
    | None -> []
    | Some path ->
        let declaration =
          try Env.find_type path env
          with Not_found | Env.Error _ ->
            let cmi =
              Cmi_format.read_cmi
                (Filename.concat Config.standard_library "stdlib.cmi")
            in
            if
              not
                (Compilation_unit.Name.equal cmi.Cmi_format.cmi_name
                   (Compilation_unit.Name.of_string "Stdlib"))
            then raise Not_found;
            cmi.cmi_sign
            |> List.find_map (function
                 | Types.Sig_type (id, declaration, _, _)
                   when String.equal (Ident.name id) "result" ->
                     Some declaration
                 | _ -> None)
            |> Option.get
        in
        let compiler_uid = compiler_uid declaration.Types.type_uid in
        [
          {
            paths = [ path ];
            type_id = { Sst.type_index = -3; type_name = "Stdlib.result" };
            name = "Stdlib.result";
            declaration;
            provenance =
              Parametric_adt.External
                {
                  compiler_uid;
                  proxy_uid = standard_proxy_uid Result;
                  prelude = true;
                };
            standard = Some Result;
          };
        ]
  in
  Ok
    ((if observed Predef.path_option then [ option ] else [])
    @ (if observed Predef.path_list then [ list ] else [])
    @ result)

let type_constructor source =
  match source.standard with
  | Some Option -> Parametric_type.option_constructor
  | Some List -> Parametric_type.list_constructor
  | Some Result -> Parametric_type.result_constructor
  | None ->
      {
        Parametric_type.constructor_path = source.name;
        constructor_identity = "compiler-uid:" ^ compiler_uid source.declaration.type_uid;
      }

let find_source (sources : source list) path =
  List.find_opt (fun (source : source) -> List.exists (Path.same path) source.paths) sources

let parameter_binders source =
  let owner =
    Parametric_type.owner ~index:source.type_id.type_index ~name:source.type_id.type_name
  in
  List.mapi
    (fun ordinal parameter ->
      (Types.get_id parameter, Parametric_type.binder owner ~ordinal))
    source.declaration.type_params

let lower_type sources aggregate binders typ =
  let rec lower seen typ =
    let id = Types.get_id typ in
    if List.mem id seen then Error "cyclic compiler type expression"
    else
      match Types.get_desc typ with
      | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
          lower (id :: seen) body
      | Types.Tvar _ | Types.Tunivar _ -> (
          match List.assoc_opt id binders with
          | Some binder -> Ok (Parametric_type.Parameter binder)
          | None -> Error "field template contains an unbound type variable")
      | Types.Tconstr (path, [], _) when Path.same path Predef.path_unit -> Ok Parametric_type.Unit
      | Types.Tconstr (path, [], _) when Path.same path Predef.path_bool -> Ok Parametric_type.Bool
      | Types.Tconstr (path, [], _) when Path.same path Predef.path_int -> Ok Parametric_type.Int
      | Types.Ttuple components ->
          let rec components_of acc = function
            | [] -> Ok (Parametric_type.Tuple (List.rev acc))
            | (label, component) :: rest ->
                let* component = lower [] component in
                components_of ((label, component) :: acc) rest
          in
          components_of [] components
      | Types.Tconstr (path, arguments, _) -> (
          match find_source sources path with
          | Some target ->
              let rec arguments_of acc = function
                | [] ->
                    let arguments = List.rev acc in
                    if List.length arguments <> List.length target.declaration.type_params then
                      Error "field template type-application arity mismatch"
                    else Ok (Parametric_type.Application (type_constructor target, arguments))
                | argument :: rest ->
                    let* argument = lower [] argument in
                    arguments_of (argument :: acc) rest
              in
              arguments_of [] arguments
          | None -> (
              match (arguments, aggregate path) with
              | [], Some type_id -> Ok (Parametric_type.Aggregate type_id)
              | _ -> Error "field template references an unsupported type constructor"))
      | Types.Tarrow _ -> Error "field template contains a function"
      | Types.Tpoly (_, _ :: _) -> Error "field template contains explicit polymorphism"
      | Types.Tunboxed_tuple _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
      | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _
      | Types.Tof_kind _ -> Error "field template contains an unsupported type"
  in
  lower [] typ

let diagnostic span location message =
  Diagnostic.make (Diagnostic.Unsupported_construct Diagnostic.Aggregate) (span location)
  |> fun diagnostic -> { diagnostic with Diagnostic.message }

let descriptor_error span location error =
  diagnostic span location (error.Parametric_adt.descriptor ^ ": " ^ error.message)

let lower_source ~span ~aggregate ~modalities sources source =
  let binders = parameter_binders source in
  let descriptor_binders = List.map snd binders in
  let constructor = type_constructor source in
  let lower_field owner index name uid mutability modality typ location =
    match lower_type sources aggregate binders typ with
    | Error message -> Error (diagnostic span location message)
    | Ok field_type ->
        let* field_modalities = modalities location modality in
        let field_id = { Sst.field_owner = owner; field_index = index; field_name = name } in
        let descriptor_field =
          {
            Parametric_adt.field_index = index;
            field_name = name;
            field_uid = uid;
            field_type;
            field_mutable = (match mutability with Types.Immutable -> false | Types.Mutable _ -> true);
          }
        in
        let sst_field =
          {
            Sst.field_id;
            field_type;
            field_mutability =
              (match mutability with Types.Immutable -> Sst.Immutable_field | Types.Mutable _ -> Sst.Mutable_field);
            field_modalities;
            span = span location;
          }
        in
        Ok (descriptor_field, sst_field)
  in
  let lower_labels owner labels =
    let rec loop index descriptor_fields sst_fields registrations = function
      | [] -> Ok (List.rev descriptor_fields, List.rev sst_fields, List.rev registrations)
      | label :: rest ->
          let uid = compiler_uid label.Types.ld_uid in
          let* descriptor_field, sst_field =
            lower_field owner index (Ident.name label.ld_id) uid label.ld_mutable
              label.ld_modalities label.ld_type label.ld_loc
          in
          loop (index + 1) (descriptor_field :: descriptor_fields)
            (sst_field :: sst_fields)
            ((label.ld_uid, source.type_id, sst_field.field_id) :: registrations)
            rest
    in
    loop 0 [] [] [] labels
  in
  match source.declaration.type_kind with
  | Types.Type_record (labels, Types.Record_boxed _, _) ->
      let owner = Sst.Record_owner source.type_id in
      let* descriptor_fields, sst_fields, fields = lower_labels owner labels in
      let* descriptor =
        Parametric_adt.create ~type_id:source.type_id ~type_constructor:constructor
          ~binders:descriptor_binders ~provenance:source.provenance
          ~kind:(Parametric_adt.Record descriptor_fields)
        |> Result.map_error (descriptor_error span source.declaration.type_loc)
      in
      Ok
        {
          descriptor;
          definition =
            { Sst.type_id = source.type_id; type_kind = Record_definition sst_fields;
              representation = Revealed; span = span source.declaration.type_loc };
          paths = source.paths;
          fields;
          constructors = [];
        }
  | Types.Type_variant (constructors, Types.Variant_boxed _, _) ->
      let rec lower_constructors index descriptor_constructors sst_constructors fields registrations = function
        | [] -> Ok (List.rev descriptor_constructors, List.rev sst_constructors, List.rev fields, List.rev registrations)
        | declaration :: rest ->
            if declaration.Types.cd_res <> None then
              Error (diagnostic span declaration.cd_loc "GADT result constraints are unsupported")
            else
              let constructor_id =
                { Sst.constructor_type = source.type_id; constructor_index = index;
                  constructor_name = Ident.name declaration.cd_id }
              in
              let owner = Sst.Constructor_owner constructor_id in
              let* descriptor_fields, sst_fields, new_fields =
                match declaration.cd_args with
                | Types.Cstr_record labels -> lower_labels owner labels
                | Types.Cstr_tuple arguments ->
                    let rec loop field_index descriptor_fields sst_fields = function
                      | [] -> Ok (List.rev descriptor_fields, List.rev sst_fields, [])
                      | (argument : Types.constructor_argument) :: arguments ->
                          let uid = compiler_uid declaration.cd_uid ^ "/field/" ^ string_of_int field_index in
                          let* descriptor_field, sst_field =
                            lower_field owner field_index ("$" ^ string_of_int field_index) uid
                              Types.Immutable Mode.Modality.Const.id argument.ca_type argument.ca_loc
                          in
                          loop (field_index + 1) (descriptor_field :: descriptor_fields)
                            (sst_field :: sst_fields) arguments
                    in
                    loop 0 [] [] arguments
              in
              let descriptor_constructor =
                { Parametric_adt.constructor_index = index;
                  constructor_name = Ident.name declaration.cd_id;
                  constructor_uid = compiler_uid declaration.cd_uid;
                  constructor_fields = descriptor_fields }
              in
              let sst_constructor =
                { Sst.constructor_id; constructor_fields = sst_fields; span = span declaration.cd_loc }
              in
              lower_constructors (index + 1)
                (descriptor_constructor :: descriptor_constructors)
                (sst_constructor :: sst_constructors)
                (List.rev_append new_fields fields)
                ((declaration.cd_uid, source.type_id, constructor_id) :: registrations)
                rest
      in
      let* descriptor_constructors, sst_constructors, fields, constructors =
        lower_constructors 0 [] [] [] [] constructors
      in
      let* descriptor =
        Parametric_adt.create ~type_id:source.type_id ~type_constructor:constructor
          ~binders:descriptor_binders ~provenance:source.provenance
          ~kind:(Parametric_adt.Variant descriptor_constructors)
        |> Result.map_error (descriptor_error span source.declaration.type_loc)
      in
      Ok
        {
          descriptor;
          definition =
            { Sst.type_id = source.type_id; type_kind = Variant_definition sst_constructors;
              representation = Revealed; span = span source.declaration.type_loc };
          paths = source.paths;
          fields;
          constructors;
        }
  | Types.Type_abstract _ ->
      Error (diagnostic span source.declaration.type_loc "abstract parameterized ADT representation is unavailable")
  | Types.Type_record _ | Types.Type_record_unboxed_product _ | Types.Type_variant _ | Types.Type_open ->
      Error (diagnostic span source.declaration.type_loc "unsupported parameterized ADT representation")


let lower ~span ~aggregate ~modalities sources =
  let rec loop acc = function
    | [] ->
        let lowered = List.rev acc in
        let descriptors = List.map (fun item -> item.descriptor) lowered in
        (match Parametric_adt.validate_registry descriptors with
        | Ok () -> Ok lowered
        | Error error -> Error (descriptor_error span Location.none error))
    | source :: rest ->
        let* lowered = lower_source ~span ~aggregate ~modalities sources source in
        loop (lowered :: acc) rest
  in
  loop [] sources


let find_by_path lowered path =
  List.find_opt (fun item -> List.exists (Path.same path) item.paths) lowered

let find_by_constructor lowered constructor =
  List.find_opt
    (fun item ->
      Parametric_type.compare_constructor
        (Parametric_adt.type_constructor item.descriptor) constructor = 0)
    lowered

let application_type_id lowered typ =
  match typ with
  | Sst.Application (constructor, _) ->
      Option.map (fun item -> Parametric_adt.type_id item.descriptor)
        (find_by_constructor lowered constructor)
  | Sst.Aggregate type_id -> Some type_id
  | Unit | Bool | Int | Tuple _ | Parameter _ -> None

let instantiate_field_type lowered ~application field =
  match application with
  | Sst.Application (constructor, arguments) -> (
      match find_by_constructor lowered constructor with
      | None -> Error "field application has no authenticated descriptor"
      | Some item ->
          let candidate =
            match Parametric_adt.kind item.descriptor with
            | Record fields ->
                if field.Sst.field_owner = Sst.Record_owner (Parametric_adt.type_id item.descriptor)
                then List.find_opt (fun candidate -> candidate.Parametric_adt.field_index = field.field_index) fields
                else None
            | Variant constructors ->
                List.find_map
                  (fun constructor_definition ->
                    match field.Sst.field_owner with
                    | Sst.Constructor_owner owner
                      when owner.constructor_type = Parametric_adt.type_id item.descriptor
                           && owner.constructor_index = constructor_definition.Parametric_adt.constructor_index ->
                        List.find_opt
                          (fun candidate -> candidate.Parametric_adt.field_index = field.field_index)
                          constructor_definition.constructor_fields
                    | Record_owner _ | Constructor_owner _ -> None)
                  constructors
          in
          (match candidate with
          | None -> Error "field identity is absent from its descriptor"
          | Some candidate -> Parametric_adt.instantiate_field item.descriptor arguments candidate))
  | _ -> Error "field owner is not a parametric ADT application"

let expression_children (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Tuple_value values -> List.map snd values
  | Sst.Record_value { fields; _ } -> List.map snd fields
  | Sst.Constructor_value { arguments; _ }
  | Sst.Checked_arithmetic (_, arguments) ->
      arguments
  | Sst.Field_read { record; _ } -> [ record ]
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Mutable_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ } ->
      [ value ]
  | Sst.Let_mutable (_, initial, body) -> [ initial; body ]
  | Sst.Let (bindings, body) -> List.map snd bindings @ [ body ]
  | Sst.Sequence (left, right)
  | Sst.Compare (_, left, right)
  | Sst.Boolean_binary (_, left, right) ->
      [ left; right ]
  | Sst.If (condition, consequent, alternative) ->
      condition :: consequent :: Option.to_list alternative
  | Sst.Match (scrutinee, cases) ->
      scrutinee
      :: List.concat_map
           (fun case ->
             Option.to_list case.Sst.case_guard @ [ case.case_body ])
           cases
  | Sst.Boolean_not value
  | Sst.Optional_present value
  | Sst.Optional_forward value
  | Sst.Use_type_invariant { value; _ }
  | Sst.Old value ->
      [ value ]
  | Sst.Direct_call { arguments; _ } ->
      List.map
        (fun argument -> snd (Sst.require_value_argument argument))
        arguments
  | Sst.Proof_region body -> [ body ]
  | Sst.Local_assert { predicate; _ } -> [ predicate ]
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      [ quantifier.quantifier_body ]
  | Sst.Callback_call application | Sst.Callback_requires application ->
      List.map snd application.Sst.arguments
  | Sst.Callback_ensures { application; result } ->
      List.map snd application.Sst.arguments @ [ result ]
  | Sst.Symbolic_application application ->
      Symbolic_application_private.arguments application
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Optional_absent
  | Sst.Owned_tree_rebase _ | Sst.Reveal _ | Sst.Reveal_with_fuel _ ->
      []

let authenticate_direct_recursion ~descriptor
    ~(definition : Sst.function_definition) ~(measure : Sst.expression) =
  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error
  in
  let descriptor_type = Parametric_adt.type_id descriptor in
  let current_binder_arguments =
    List.map (fun binder -> Sst.Parameter binder) definition.type_binders
  in
  let expected_arguments =
    match (definition.type_binders, measure.typ) with
    | [], Sst.Application (_, arguments) -> arguments
    | _ -> current_binder_arguments
  in
  let exact_application = function
    | Sst.Application (constructor, arguments) ->
        Parametric_type.compare_constructor constructor
          (Parametric_adt.type_constructor descriptor)
        = 0
        && List.length arguments = List.length expected_arguments
        && List.for_all2 Parametric_type.equal arguments expected_arguments
    | _ -> false
  in
  let* measure_binding =
    match measure.expression_desc with
    | Sst.Variable { binding; _ } when exact_application measure.typ ->
        Ok binding
    | _ ->
        Error
          "generic recursion measure must be the exact current-binder ADT formal"
  in
  let measured_ordinal =
    definition.parameters
    |> List.mapi (fun ordinal parameter ->
           let parameter = Sst.require_value_parameter parameter in
           match parameter.Sst.pattern.pattern_desc with
           | Sst.Bind binding when binding.id = measure_binding.id ->
               Some ordinal
           | _ -> None)
    |> List.find_map Fun.id
  in
  let* measured_ordinal =
    match measured_ordinal with
    | Some ordinal -> Ok ordinal
    | None ->
        Error "generic recursion measure must name a simple formal parameter"
  in
  let owner_matches owner =
    let owner_type =
      match owner with
      | Sst.Record_owner type_id -> type_id
      | Sst.Constructor_owner constructor -> constructor.constructor_type
    in
    owner_type.type_index = descriptor_type.type_index
  in
  let direct_projection = function
    | {
        Sst.expression_desc =
          Sst.Field_read
            {
              record =
                {
                  expression_desc =
                    Sst.Variable { binding = parent; _ };
                  _;
                };
              field;
            };
        _;
      } ->
        parent.id = measure_binding.id
        && owner_matches field.field_owner
        && Parametric_adt.authenticates_recursive_field descriptor
             ~constructor_index:
               (match field.field_owner with
               | Sst.Record_owner _ -> None
               | Sst.Constructor_owner constructor ->
                   Some constructor.constructor_index)
             ~field_index:field.field_index
    | _ -> false
  in
  let pattern_children children (pattern : Sst.pattern) =
    match pattern.pattern_desc with
    | Sst.Constructor_pattern (constructor, arguments)
      when constructor.constructor_type.type_index = descriptor_type.type_index
      ->
        List.fold_left
          (fun children (index, argument) ->
            if
              Parametric_adt.authenticates_recursive_field descriptor
                ~constructor_index:(Some constructor.constructor_index)
                ~field_index:index
            then
              match argument.Sst.pattern_desc with
              | Sst.Bind binding -> binding.id :: children
              | _ -> children
            else children)
          children (List.mapi (fun index argument -> (index, argument)) arguments)
    | Sst.Record_pattern fields ->
        List.fold_left
          (fun children (field, nested) ->
            if
              owner_matches field.Sst.field_owner
              && Parametric_adt.authenticates_recursive_field descriptor
                   ~constructor_index:None ~field_index:field.field_index
            then
              match nested.Sst.pattern_desc with
              | Sst.Bind binding -> binding.id :: children
              | _ -> children
            else children)
          children fields
    | _ -> children
  in
  let is_measured_binding (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Variable { binding; _ } -> binding.id = measure_binding.id
    | _ -> false
  in
  let rec check children (expression : Sst.expression) =
    let check_all expressions =
      List.fold_left
        (fun result expression ->
          let* () = result in
          check children expression)
        (Ok ()) expressions
    in
    match expression.expression_desc with
    | Sst.Direct_call
        { callee; type_arguments; arguments; recursive = true; _ }
      when
        callee.function_index = definition.function_id.function_index
        && String.equal callee.function_name definition.function_id.function_name
        ->
        let exact_arguments =
          List.length type_arguments = List.length current_binder_arguments
          && List.for_all2 Parametric_type.equal type_arguments
               current_binder_arguments
        in
        let actual =
          List.nth_opt arguments measured_ordinal
          |> Option.map (fun argument -> snd (Sst.require_value_argument argument))
        in
        if not exact_arguments then
          Error "generic recursive call changes current type arguments"
        else (
          match actual with
          | Some
              {
                Sst.expression_desc = Sst.Variable { binding; _ };
                typ;
                _;
              }
            when List.mem binding.id children && exact_application typ ->
              check_all
                (List.map
                   (fun argument -> snd (Sst.require_value_argument argument))
                   arguments)
          | Some actual when direct_projection actual ->
              check_all
                (List.map
                   (fun argument -> snd (Sst.require_value_argument argument))
                   arguments)
          | Some _ | None ->
              Error
                "generic recursive call must descend through an authenticated direct recursive field")
    | Sst.Match (scrutinee, cases) ->
        let direct_match = is_measured_binding scrutinee in
        let* () = check children scrutinee in
        List.fold_left
          (fun result case ->
            let* () = result in
            let case_children =
              if direct_match then pattern_children children case.Sst.case_pattern
              else
                match scrutinee.expression_desc with
                | Sst.Tuple_value _ -> (
                    match
                      Recursive_spec_tuple_match_private.plan scrutinee
                        case.Sst.case_pattern
                    with
                    | Error _ -> children
                    | Ok leaves ->
                        List.fold_left
                          (fun children
                               (leaf :
                                 Recursive_spec_tuple_match_private.leaf) ->
                            if is_measured_binding leaf.expression then
                              pattern_children children leaf.pattern
                            else children)
                          children leaves)
                | _ -> children
            in
            let* () =
              match case.case_guard with
              | None -> Ok ()
              | Some guard -> check case_children guard
            in
            check case_children case.case_body)
          (Ok ()) cases
    | _ -> check_all (expression_children expression)
  in
  match definition.body with
  | Sst.Checked_exec { body; _ }
  | Sst.Proof_body { body; _ }
  | Sst.Recursive_spec_definition { body; _ } ->
      check [] body.expression
  | Sst.Spec_definition _ | Sst.External_specification _
  | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
  | Sst.Symbolic_declaration _ ->
      Error "generic direct recursion has no checked recursive body"
