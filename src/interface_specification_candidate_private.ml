open Interface_specification_environment_private

let strict_candidate_entries = ref 0

let import_key (import : Cmt_input.import) = (import.unit_name, import.crc)

let sorted_imports imports =
  Array.to_list imports |> List.map import_key
  |> List.sort (fun (left_name, left_crc) (right_name, right_crc) ->
         match String.compare left_name right_name with
         | 0 -> Option.compare String.compare left_crc right_crc
         | comparison -> comparison)

let imports_cover ~implementation interface =
  let implementation = sorted_imports implementation in
  sorted_imports interface
  |> List.for_all (fun required -> List.mem required implementation)

let duplicate_import imports =
  let rec loop seen = function
    | [] -> None
    | (import : Cmt_input.import) :: rest ->
        if List.mem import.unit_name seen then Some import.unit_name
        else loop (import.unit_name :: seen) rest
  in
  loop [] (Array.to_list imports)

let argument_after flag arguments =
  let rec loop = function
    | [] | [ _ ] -> []
    | candidate :: value :: rest ->
        if String.equal candidate flag then value :: loop rest
        else loop (value :: rest)
  in
  loop (Array.to_list arguments)

let has_argument argument arguments =
  Array.exists (String.equal argument) arguments

let supported_ppx command =
  match String.split_on_char ' ' command |> List.filter (( <> ) "") with
  | executable :: arguments ->
      let executable =
        String.trim executable
        |> String.map (fun character ->
               if Char.equal character '\'' || Char.equal character '"' then
                 ' '
               else character)
        |> String.trim
      in
      let basename = Filename.basename executable in
      let standalone =
        (String.equal basename "vero_ppx.exe"
        || String.equal basename "verocaml-ppx")
        && List.exists (String.equal "--keep-ghost") arguments
      in
      let ppxlib =
        List.exists
          (fun argument ->
            String.equal argument "-as-ppx"
            || String.equal argument "--as-ppx")
          arguments
        && List.exists (String.equal "--verocaml-retained") arguments
        && not
             (List.exists (String.equal "--verocaml-ordinary") arguments)
      in
      standalone || ppxlib
  | [] -> false

let supported_retained_route ~require_public_interface
    (candidate : Cmt_input.implementation) =
  let interface_issuer issuer =
    (not require_public_interface)
    || candidate.interface_family_issuers = [ issuer ]
  in
  let compiler_ppx = argument_after "-ppx" candidate.compiler_arguments in
  let _route, authenticated =
    match compiler_ppx with
    | [ command ] ->
        ( "standalone-v1",
          supported_ppx command
          && Cmt_input.retained_ppx_artifact candidate
          && candidate.implementation_family_issuers = [ "standalone-v1" ]
          && interface_issuer "standalone-v1" )
    | [] ->
        ( "ppxlib-v1",
          Cmt_input.retained_ppx_artifact candidate
          && candidate.implementation_family_issuers = [ "ppxlib-v1" ]
          && interface_issuer "ppxlib-v1" )
    | _ -> ("unrecognized", false)
  in
  [%log.debug "evaluated retained candidate authentication"
    ~provider:(Delator.Field.string candidate.unit_name)
    ~route:(Delator.Field.string _route)
    ~family:(Delator.Field.string "retained-v1")
    ~stage:(Delator.Field.string "retained-candidate")
    ~decision:
      (Delator.Field.string
         (if authenticated then "accepted" else "rejected"))
    ~interface_receipt_count:
      (Delator.Field.int (List.length candidate.interface_family_markers))
    ~public_interface_required:
      (Delator.Field.bool require_public_interface)];
  authenticated

type canonical_carrier = {
  carrier_unit : string;
  carrier_crc : string;
  required : bool;
}

let canonical_carriers =
  [
    {
      carrier_unit = "CamlinternalFormatBasics";
      carrier_crc = "65d3d91b31a7b67cded55120b45d3d3c";
      required = true;
    };
    {
      carrier_unit = "Stdlib";
      carrier_crc = Trusted_imports.stdlib_crc;
      required = true;
    };
    {
      carrier_unit = "Stdlib__Domain";
      carrier_crc = Trusted_imports.domain_crc;
      required = false;
    };
    {
      carrier_unit = "Stdlib__Effect";
      carrier_crc = Trusted_imports.effect_crc;
      required = false;
    };
    {
      carrier_unit = "Vero_ghost";
      carrier_crc = Trusted_imports.ghost_crc;
      required = false;
    };
  ]

let canonical_carrier unit_name =
  List.find_opt
    (fun carrier -> String.equal carrier.carrier_unit unit_name)
    canonical_carriers

let canonical_import (import : Cmt_input.import) =
  match canonical_carrier import.unit_name with
  | Some carrier -> import.crc = Some carrier.carrier_crc
  | None -> false

let strict_candidate ~require_public_interface
    (candidate : Cmt_input.implementation) =
  incr strict_candidate_entries;
  let unit_name = candidate.unit_name in
  let reject message = error ~unit_name message in
  match candidate.interface_digest with
  | None -> reject "missing self interface digest"
  | Some interface_digest -> (
      match duplicate_import candidate.imports with
      | Some duplicate ->
          reject (Printf.sprintf "duplicate import slot for %s" duplicate)
      | None -> (
          let self_slots =
            Array.to_list candidate.imports
            |> List.filter (fun (import : Cmt_input.import) ->
                   String.equal import.Cmt_input.unit_name unit_name)
          in
          if
            self_slots
            <>
            [
              {
                Cmt_input.unit_name;
                crc = Some interface_digest
              }
            ]
          then reject "self import slot does not match the interface digest"
          else if
            Array.exists
              (fun (import : Cmt_input.import) ->
                Option.is_none import.Cmt_input.crc)
              candidate.imports
          then reject "an import slot is missing its CRC"
          else if
            require_public_interface
            && not
                 (candidate.embedded_interface
                 || candidate.explicit_interface)
          then
            reject
              "dependency implementation CMT has no authenticated public \
               interface"
          else if
            (candidate.explicit_interface
            || candidate.interface_family_markers <> [])
            && candidate.interface_family_markers <> [ "retained-v1" ]
          then
            reject
              "dependency implementation and embedded interface artifact \
               families differ"
          else if Option.is_some (duplicate_import candidate.interface_imports)
          then reject "interface contains a duplicate import slot"
          else if candidate.interface_unit_name <> Some unit_name then
            reject "interface unit identity mismatch"
          else if candidate.interface_implementation_unit_name <> Some unit_name
          then reject "interface implementation identity mismatch"
          else if candidate.interface_parameter_count <> 0 then
            reject "parameterized compilation units are unsupported"
          else if
            candidate.embedded_interface
            && sorted_imports candidate.imports
            <> sorted_imports candidate.interface_imports
          then reject "CMT and embedded interface import slots differ"
          else if
            candidate.explicit_interface
            && not
                 (imports_cover ~implementation:candidate.imports
                    candidate.interface_imports)
          then reject "CMT imports do not cover explicit interface import slots"
          else if Option.is_none candidate.source_digest then
            reject "missing source digest"
          else if not candidate.has_implementation_shape then
            reject "missing implementation shape"
          else if not (has_argument "-bin-annot" candidate.compiler_arguments)
          then reject "CMT was not produced with binary annotations enabled"
          else
            if not (supported_retained_route ~require_public_interface candidate)
            then
              reject "unsupported retained VeroCaml PPX identity"
            else if
              List.exists
                (fun unsafe -> has_argument unsafe candidate.compiler_arguments)
                [
                  "-unsafe";
                  "-nopervasives";
                  "-nostdlib";
                  "-no-check-prims";
                ]
            then reject "unsupported compiler compatibility option"
            else
              let carrier_failure =
                List.find_map
                  (fun carrier ->
                    let slots =
                      Array.to_list candidate.imports
                      |> List.filter (fun (import : Cmt_input.import) ->
                             String.equal import.unit_name carrier.carrier_unit)
                    in
                    match slots with
                    | [] when carrier.required ->
                        Some
                          (Printf.sprintf
                             "missing canonical carrier import slot for %s"
                             carrier.carrier_unit)
                    | [] -> None
                    | [ import ] when import.crc = Some carrier.carrier_crc ->
                        None
                    | [ _ ] ->
                        Some
                          (Printf.sprintf
                             "canonical carrier CRC mismatch for %s"
                             carrier.carrier_unit)
                    | _ -> assert false)
                  canonical_carriers
              in
              match carrier_failure with
              | Some message -> reject message
              | None -> Ok interface_digest))

let custom_imports (candidate : Cmt_input.implementation) =
  Array.to_list candidate.Cmt_input.imports
  |> List.filter (fun (import : Cmt_input.import) ->
      Option.is_some import.Cmt_input.crc
      && not (String.equal import.Cmt_input.unit_name candidate.unit_name)
      && not (canonical_import import))

let find_candidate (candidates : Cmt_input.implementation list) unit_name =
  List.find_opt
    (fun candidate -> String.equal candidate.Cmt_input.unit_name unit_name)
    candidates

let retained_authority_identity_is_exact =
  Interface_specification_environment_private.retained_authority_identity_is_exact

let exact_import = Interface_specification_environment_private.exact_import
let exact_imports = Interface_specification_environment_private.exact_imports

let graph_order (candidates : Cmt_input.implementation list)
    (consumer : Cmt_input.implementation) =
  let reject ?unit_name message = error ?unit_name message in
  let rec duplicate seen = function
    | [] -> None
    | candidate :: rest ->
        if List.mem candidate.Cmt_input.unit_name seen then
          Some candidate.Cmt_input.unit_name
        else duplicate (candidate.Cmt_input.unit_name :: seen) rest
  in
  match duplicate [] candidates with
  | Some unit_name ->
      reject ~unit_name "duplicate dependency implementation candidate"
  | None -> (
      let digest_pairs =
        List.filter_map
          (fun (candidate : Cmt_input.implementation) ->
            Option.map
              (fun digest -> (candidate.Cmt_input.unit_name, digest))
              candidate.interface_digest)
          candidates
      in
      let rec duplicate_digest seen = function
        | [] -> None
        | (unit_name, digest) :: rest -> (
            match List.assoc_opt digest seen with
            | Some other -> Some (other, unit_name, digest)
            | None -> duplicate_digest ((digest, unit_name) :: seen) rest)
      in
      match duplicate_digest [] digest_pairs with
      | Some (left, right, digest) ->
          reject
            (Printf.sprintf
               "ambiguous interface digest %s is claimed by units %s and %s"
               digest left right)
      | None -> (
          let all_nodes = consumer :: candidates in
          let mismatched =
            List.find_map
              (fun (candidate : Cmt_input.implementation) ->
                custom_imports candidate
                |> List.find_map (fun (import : Cmt_input.import) ->
                       match find_candidate candidates import.unit_name with
                       | Some dependency
                         when not (exact_import ~owner:candidate ~dependency import) ->
                           Some (candidate.unit_name, import.unit_name)
                       | Some _ | None -> None))
              all_nodes
          in
          match mismatched with
          | Some (owner, dependency) ->
              reject ~unit_name:owner
                (Printf.sprintf
                   "import CRC does not match the supplied interface for unit %s"
                   dependency)
          | None ->
          let missing =
            List.find_map
              (fun (candidate : Cmt_input.implementation) ->
                custom_imports candidate
                |> List.filter (Cmt_input.retained_authority_import candidate)
                |> List.find_map (fun (import : Cmt_input.import) ->
                       if String.equal import.unit_name consumer.unit_name then
                         Some (candidate.unit_name, import.unit_name)
                       else if
                         Option.is_some
                           (find_candidate candidates import.unit_name)
                       then None
                       else Some (candidate.unit_name, import.unit_name)))
              all_nodes
          in
          (match missing with
          | Some (owner, missing) ->
              reject ~unit_name:owner
                (Printf.sprintf
                   "missing explicit implementation CMT for imported unit %s"
                   missing)
          | None -> (
              let dependencies (candidate : Cmt_input.implementation) =
                custom_imports candidate
                |> List.filter_map (fun (import : Cmt_input.import) ->
                       find_candidate candidates import.unit_name)
              in
              let rec visit temporary permanent ordered candidate =
                    let name = candidate.Cmt_input.unit_name in
                    if List.mem name permanent then Ok (temporary, permanent, ordered)
                    else if List.mem name temporary then
                      reject ~unit_name:name "cyclic explicit dependency graph"
                    else
                      let temporary = name :: temporary in
                      let rec visit_dependencies temporary permanent ordered =
                        function
                        | [] ->
                            Ok
                              ( List.filter (( <> ) name) temporary,
                                name :: permanent,
                                candidate :: ordered )
                        | dependency :: rest -> (
                            match
                              visit temporary permanent ordered dependency
                            with
                            | Error _ as error -> error
                            | Ok (temporary, permanent, ordered) ->
                                visit_dependencies temporary permanent ordered rest)
                      in
                      visit_dependencies temporary permanent ordered
                        (dependencies candidate)
                  in
                  let rec order temporary permanent ordered = function
                    | [] -> Ok (List.rev ordered)
                    | candidate :: rest -> (
                        match visit temporary permanent ordered candidate with
                        | Error _ as error -> error
                        | Ok (temporary, permanent, ordered) ->
                            order temporary permanent ordered rest)
                  in
              order [] [] [] candidates))))

type public_surface = {
  public_type_names : string list;
  public_revealed_type_names : string list;
  public_callable_names : string list;
  public_external_type_constructors : Parametric_type.constructor list;
  public_symbolic_names : string list;
}

let embedded_public_surface ~unit_name
    (candidate : Cmt_input.implementation) =
  let reject message = error ~unit_name message in
  match candidate.embedded_interface_metadata with
  | None -> reject "dependency implementation has no embedded public signature"
  | Some metadata -> (
      try
        let root_signature =
          Subst.Lazy.force_signature metadata.Cmi_format.cmi_sign
        in
        let rec module_signature seen module_types = function
          | Types.Mty_signature signature -> Ok signature
          | Mty_ident (Path.Pident ident) -> (
              if List.exists (Ident.same ident) seen then
                reject "cyclic public module-type identity"
              else
                match
                  List.find_opt
                    (fun (candidate, _) -> Ident.same candidate ident)
                    module_types
                with
                | Some (_, module_type) ->
                    module_signature (ident :: seen) module_types module_type
                | None ->
                    reject
                      "public module signature is not embedded in the \
                       dependency CMT")
          | Mty_strengthen (module_type, _, _) ->
              module_signature seen module_types module_type
          | Mty_ident _ | Mty_functor _ | Mty_alias _ ->
              reject "unsupported public module signature"
        in
        let rec collect_signature prefix inherited_module_types signature =
          let module_types =
            List.fold_left
              (fun module_types item ->
                match item with
                | Types.Sig_modtype (ident, declaration, Types.Exported) -> (
                    match declaration.Types.mtd_type with
                    | Some module_type -> (ident, module_type) :: module_types
                    | None -> module_types)
                | _ -> module_types)
              inherited_module_types signature
          in
          let qualify name =
            if String.equal prefix "" then name else prefix ^ "." ^ name
          in
          let rec collect types revealed_types callables = function
            | [] ->
                Ok
                  ( List.rev types,
                    List.rev revealed_types,
                    List.rev callables )
            | item :: rest -> (
                match item with
                | Types.Sig_type
                    (ident, declaration, _, Types.Exported) ->
                    let name = qualify (Ident.name ident) in
                    let revealed_types =
                      match declaration.Types.type_kind with
                      | Type_record _ | Type_record_unboxed_product _
                      | Type_variant _ | Type_open ->
                          name :: revealed_types
                      | Type_abstract _ -> revealed_types
                    in
                    collect (name :: types) revealed_types callables rest
                | Sig_value (ident, _, Types.Exported) ->
                    let name = qualify (Ident.name ident) in
                    if
                      List.exists
                        (fun (group : Cmt_input.interface_broadcast_group) ->
                          String.equal group.group_path name)
                        candidate.interface_broadcast_groups
                    then (
                      [%log.trace "exclude broadcast group carrier from callable surface"
                        ~unit_name:(Delator.Field.string unit_name)
                        ~path:(Delator.Field.string name)];
                      collect types revealed_types callables rest)
                    else
                      collect types revealed_types (name :: callables) rest
                | Sig_module
                    (ident, _, declaration, _, Types.Exported) -> (
                    match declaration.Types.md_type with
                    | Mty_alias _ ->
                        collect types revealed_types callables rest
                    | _ ->
                    match
                      module_signature [] module_types
                        declaration.Types.md_type
                    with
                    | Error _ as error -> error
                    | Ok nested -> (
                        match
                          collect_signature
                            (qualify (Ident.name ident))
                            module_types nested
                        with
                        | Error _ as error -> error
                        | Ok
                            ( nested_types,
                              nested_revealed_types,
                              nested_callables ) ->
                            collect
                              (List.rev_append nested_types types)
                              (List.rev_append nested_revealed_types
                                 revealed_types)
                              (List.rev_append nested_callables callables)
                              rest))
                | Sig_value (_, _, Types.Hidden)
                | Sig_type (_, _, _, Types.Hidden)
                | Sig_module (_, _, _, _, Types.Hidden)
                | Sig_typext _ | Sig_modtype _ | Sig_class _
                | Sig_class_type _ ->
                    collect types revealed_types callables rest)
          in
          collect [] [] [] signature
        in
        match collect_signature "" [] root_signature with
        | Error _ as error -> error
        | Ok
            ( public_type_names,
              public_revealed_type_names,
              public_callable_names ) -> (
            let prefix = unit_name ^ "." in
            let broadcast_group_names =
              candidate.interface_broadcasts
              |> List.filter_map (fun member ->
                     let identity =
                       member.Retained_broadcast_private.identity
                     in
                     if identity.kind = Retained_broadcast_private.Group then
                       if String.starts_with ~prefix identity.canonical_path then
                         Some
                           (String.sub identity.canonical_path
                              (String.length prefix)
                              (String.length identity.canonical_path
                              - String.length prefix))
                       else
                         raise
                           (Failure
                              "broadcast group interface provider path mismatch")
                     else None)
            in
            let public_callable_names =
              List.filter
                (fun name -> not (List.mem name broadcast_group_names))
                public_callable_names
            in
            let relative_path path =
              if String.starts_with ~prefix path then
                Some
                  (String.sub path (String.length prefix)
                     (String.length path - String.length prefix))
              else None
            in
            let logical_type_names =
              candidate.interface_logical_sorts
              |> List.filter_map (fun descriptor ->
                     relative_path descriptor.Logical_sort_private.type_path)
            and logical_callable_names =
              candidate.interface_logical_sorts
              |> List.filter_map (fun descriptor ->
                     relative_path
                       descriptor.Logical_sort_private.integer_literal_path)
            in
            let public_type_names =
              List.filter
                (fun name -> not (List.mem name logical_type_names))
                public_type_names
            and public_revealed_type_names =
              List.filter
                (fun name -> not (List.mem name logical_type_names))
                public_revealed_type_names
            and public_callable_names =
              List.filter
                (fun name -> not (List.mem name logical_callable_names))
                public_callable_names
            in
            [%log.debug "partitioned descriptor-backed logical declarations from ordinary provider surface"
              ~provider:(Delator.Field.string unit_name)
              ~stage:(Delator.Field.string "embedded-public-surface")
              ~logical_type_count:
                (Delator.Field.int (List.length logical_type_names))
              ~logical_callable_count:
                (Delator.Field.int (List.length logical_callable_names))
              ~decision:(Delator.Field.string "descriptor-owned")];
            let duplicate values =
              let rec loop seen = function
                | [] -> None
                | value :: rest ->
                    if List.mem value seen then Some value
                    else loop (value :: seen) rest
              in
              loop [] values
            in
            match
               (duplicate public_type_names, duplicate public_callable_names)
             with
            | Some name, _ | _, Some name ->
                reject
                  (Printf.sprintf
                     "ambiguous embedded public identity %s" name)
            | None, None ->
                let public_symbolic_names =
                  candidate.interface_symbolic_declarations
                  |> List.map
                       (fun declaration ->
                         let path = declaration.Cmt_input.symbolic_path in
                         if String.starts_with ~prefix path then
                           String.sub path (String.length prefix)
                             (String.length path - String.length prefix)
                         else
                           raise
                             (Failure
                                "symbolic interface provider path mismatch"))
                in
                if
                  List.exists
                    (fun name -> not (List.mem name public_callable_names))
                    public_symbolic_names
                then
                  raise
                    (Failure
                       "symbolic interface marker is not attached to a public value");
                Ok
                  {
                    public_type_names;
                    public_revealed_type_names;
                    public_callable_names;
                    public_external_type_constructors = [];
                    public_symbolic_names;
                  })
      with Cmi_format.Error _ | Invalid_argument _ | Failure _ ->
          reject "malformed embedded public signature")

let surface_type_name (type_id : Sst.type_id) = type_id.Sst.type_name

let surface_has_type surface type_id =
  List.mem (surface_type_name type_id) surface.public_type_names

let surface_reveals_type surface type_id =
  List.mem (surface_type_name type_id) surface.public_revealed_type_names

let public_type_constructor surface constructor =
  List.exists
    (fun candidate ->
      Parametric_type.compare_constructor candidate constructor = 0)
    surface.public_external_type_constructors
  ||
  let path = constructor.Parametric_type.constructor_path in
  not (String.contains path '.') && List.mem path surface.public_type_names

let rec public_typ surface binders = function
  | Sst.Unit | Bool | Int | Mathematical_int -> true
  | Tuple components ->
      List.for_all (fun (_, typ) -> public_typ surface binders typ) components
  | Aggregate type_id -> surface_has_type surface type_id
  | Parameter binder -> List.mem binder binders
  | Application _ as typ when Parametric_type.is_spec_function typ -> (
      match Parametric_type.spec_function_view typ with
      | Some (_, domain, range) ->
          public_typ surface binders domain && public_typ surface binders range
      | None -> false)
  | Application (constructor, arguments) ->
      public_type_constructor surface constructor
      && List.for_all (public_typ surface binders) arguments

let public_binding surface binders (binding : Sst.binding) =
  public_typ surface binders binding.Sst.typ

let public_constructor surface (constructor : Sst.constructor_id) =
  surface_reveals_type surface constructor.Sst.constructor_type

let public_field surface (field : Sst.field_id) =
  match field.Sst.field_owner with
  | Sst.Record_owner owner -> surface_reveals_type surface owner
  | Constructor_owner constructor -> public_constructor surface constructor

let public_path_step surface = function
  | Sst.Owned_tree_field field -> public_field surface field
  | Owned_tree_constructor constructor ->
      public_constructor surface constructor

let public_cursor surface binders (cursor : Sst.owned_tree_cursor) =
  public_binding surface binders cursor.Sst.cursor_binding
  && public_binding surface binders cursor.root
  && List.for_all (public_path_step surface) cursor.guarded_path

let public_reconstruction_layer surface = function
  | Sst.Reconstruct_record { record_type; changed_field; preserved_fields } ->
      surface_reveals_type surface record_type
      && public_field surface changed_field
      && List.for_all (public_field surface) preserved_fields
  | Reconstruct_constructor constructor ->
      public_constructor surface constructor

let public_transition surface binders (transition : Sst.owned_tree_transition) =
  public_binding surface binders transition.Sst.root
  && Option.fold ~none:true ~some:(public_cursor surface binders) transition.cursor
  && public_field surface transition.target_field
  &&
  (match transition.rhs_provenance with
  | Sst.Ground_owned_tree_value -> true
  | Guarded_descendant_move cursor -> public_cursor surface binders cursor)
  && List.for_all
       (public_reconstruction_layer surface)
       transition.reconstruction

let public_shared_transition surface binders
    (transition : Sst.shared_scalar_heap_transition) =
  surface_reveals_type surface transition.shared_record_type
  && List.mem transition.shared_function_name surface.public_callable_names
  && List.for_all
       (public_binding surface binders)
       transition.shared_formal_roots
  && public_binding surface binders transition.shared_target
  && public_binding surface binders transition.shared_canonical_root
  && List.for_all
       (public_binding surface binders)
       transition.shared_alias_chain
  && public_field surface transition.shared_target_field

let rec public_pattern surface binders (pattern : Sst.pattern) =
  public_typ surface binders pattern.Sst.typ
  &&
  match pattern.pattern_desc with
  | Sst.Wildcard | Int_pattern _ | Bool_pattern _ | Unit_pattern -> true
  | Bind binding -> public_binding surface binders binding
  | Owned_tree_cursor_pattern cursor -> public_cursor surface binders cursor
  | Tuple_pattern patterns ->
      List.for_all
        (fun (_, pattern) -> public_pattern surface binders pattern)
        patterns
  | Record_pattern fields ->
      List.for_all
        (fun (field, pattern) ->
          public_field surface field && public_pattern surface binders pattern)
        fields
  | Constructor_pattern (constructor, patterns) ->
      public_constructor surface constructor
      && List.for_all (public_pattern surface binders) patterns
  | Or_pattern (left, right) ->
      public_pattern surface binders left
      && public_pattern surface binders right

let surface_callable_name (function_id : Sst.function_id) =
  function_id.Sst.function_name

let public_function surface function_id =
  Spec_function_sst_private.is_application_id function_id
  || List.mem (surface_callable_name function_id) surface.public_callable_names

let public_canonical_application_constructor surface = function
  | Sst.Application (constructor, _) ->
      public_type_constructor surface constructor
      && not
           (List.mem constructor.Parametric_type.constructor_path
              surface.public_type_names)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Aggregate _
  | Sst.Parameter _ ->
      false

let rec public_expression surface binders (expression : Sst.expression) =
  public_typ surface binders expression.Sst.typ
  &&
  match expression.expression_desc with
  | Sst.Int_constant _ | Bool_constant _ | Unit_constant -> true
  | Lift_runtime_int operand -> public_expression surface binders operand
  | Variable { binding; _ } | Mutable_read binding ->
      public_binding surface binders binding
  | Tuple_value values ->
      List.for_all
        (fun (_, expression) -> public_expression surface binders expression)
        values
  | Record_value { record_type; fields } ->
      surface_reveals_type surface record_type
      && List.for_all
           (fun (field, expression) ->
             public_field surface field
             && public_expression surface binders expression)
           fields
  | Constructor_value { constructor; arguments } ->
      (public_constructor surface constructor
      || public_canonical_application_constructor surface expression.typ)
      && List.for_all (public_expression surface binders) arguments
  | Field_read { record; field } ->
      public_field surface field && public_expression surface binders record
  | Field_write { provenance; field; value; transition } ->
      public_binding surface binders provenance.root
      && public_field surface field
      && public_expression surface binders value
      && Option.fold ~none:true
           ~some:(public_transition surface binders)
           transition
  | Shared_scalar_field_write { provenance; field; value; transition } ->
      public_binding surface binders provenance.root
      && public_field surface field
      && public_expression surface binders value
      && public_shared_transition surface binders transition
  | Owned_tree_nested_write { transition; value } ->
      public_transition surface binders transition
      && public_expression surface binders value
  | Owned_tree_rebase { transition } ->
      public_transition surface binders transition
  | Let_mutable (binding, initial, body) ->
      public_binding surface binders binding
      && public_expression surface binders initial
      && public_expression surface binders body
  | Mutable_write { provenance; value } ->
      public_binding surface binders provenance.root
      && public_expression surface binders value
  | Let (bindings, body) ->
      List.for_all
        (fun (pattern, expression) ->
          public_pattern surface binders pattern
          && public_expression surface binders expression)
        bindings
      && public_expression surface binders body
  | Sequence (left, right) | Boolean_binary (_, left, right)
  | Compare (_, left, right) ->
      public_expression surface binders left
      && public_expression surface binders right
  | If (condition, yes, no) ->
      public_expression surface binders condition
      && public_expression surface binders yes
      && Option.fold ~none:true ~some:(public_expression surface binders) no
  | Match (scrutinee, cases) ->
      public_expression surface binders scrutinee
      && List.for_all
           (fun case ->
             public_pattern surface binders case.Sst.case_pattern
             && Option.fold ~none:true
                  ~some:(public_expression surface binders)
                  case.case_guard
             && public_expression surface binders case.case_body)
           cases
  | Checked_arithmetic (_, arguments) ->
      List.for_all (public_expression surface binders) arguments
  | Boolean_not operand | Proof_region operand | Old operand ->
      public_expression surface binders operand
  | Forall quantifier | Exists quantifier ->
      public_binding surface binders quantifier.quantifier_binder
      && public_expression surface binders quantifier.quantifier_body
      && Option.fold ~none:true
           ~some:(public_expression surface binders)
           quantifier.quantifier_trigger
  | Direct_call { callee; arguments; _ } ->
      public_function surface callee
      && List.for_all
           (fun argument ->
             public_expression surface binders
               (snd (Sst.require_value_argument argument)))
           arguments
  | Symbolic_application application ->
      let declaration =
        Symbolic_application_private.declaration application
      in
      let declaration_binders =
        Symbolic_application_private.type_binders declaration
      in
      List.mem
        (Symbolic_application_private.canonical_path declaration)
        surface.public_symbolic_names
      && public_typ surface declaration_binders
           (Symbolic_application_private.declaration_result_type declaration)
      && List.for_all
           (public_typ surface declaration_binders)
           (Symbolic_application_private.parameter_types declaration)
      && List.for_all
           (public_typ surface binders)
           (Symbolic_application_private.type_arguments application)
      && List.for_all
           (public_typ surface binders)
           (Symbolic_application_private.argument_types application)
      && public_typ surface binders
           (Symbolic_application_private.result_type application)
      && List.for_all
           (public_expression surface binders)
           (Symbolic_application_private.arguments application)
  | Callback_call _ | Callback_requires _ | Callback_ensures _
  | Optional_absent | Optional_present _ | Optional_forward _ | Reveal _ | Reveal_with_fuel _ | Use_type_invariant _
  | Local_assert _ ->
      false

let public_field_definition surface binders field =
  public_field surface field.Sst.field_id
  && public_typ surface binders field.field_type

let surface_type_kind_is_public surface binders = function
  | Sst.Record_definition fields ->
      List.for_all (public_field_definition surface binders) fields
  | Variant_definition constructors ->
      List.for_all
        (fun constructor ->
          public_constructor surface constructor.Sst.constructor_id
          && List.for_all
               (public_field_definition surface binders)
               constructor.constructor_fields)
        constructors

let public_clause surface binders clause =
  Option.fold ~none:true
    ~some:(public_pattern surface binders)
    (Sst_validation.contract_clause_binder clause)
  && public_expression surface binders
       (Sst_validation.contract_clause_expression clause)

let public_callable_descriptor surface descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  let binders = definition.Sst.type_binders in
  let contract = Sst_validation.callable_contract descriptor in
  public_function surface definition.Sst.function_id
  && List.for_all
       (fun parameter ->
         public_pattern surface binders
           (Sst.require_value_parameter parameter).Sst.pattern)
       definition.parameters
  && public_typ surface binders definition.result_type
  && List.for_all
       (public_clause surface binders)
       (Sst_validation.contract_requires contract)
  && List.for_all
       (public_clause surface binders)
       (Sst_validation.contract_ensures contract)
  && List.for_all
       (fun decrease ->
         public_clause surface binders
           (Sst_validation.decrease_clause decrease))
       (Sst_validation.contract_decreases contract)

module For_testing = struct
  let reset_strict_candidate_entries () = strict_candidate_entries := 0
  let strict_candidate_entries () = !strict_candidate_entries
end
