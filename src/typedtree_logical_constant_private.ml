open Typedtree

let attribute_name = "verocaml.internal.logical_constant.definition.v1"

type declaration = {
  binding : value_binding;
  body : expression;
  source_type : core_type;
  ident : Ident.t;
  resolved_paths : Path.t list;
  value_uid : string;
  marker_id : string;
  source_name : string;
  canonical_path : string;
  provider_unit : string;
  provider_interface : string;
  source_file : string;
  compilation_identity : string;
  source_body_digest : string;
  declaration_location : Location.t;
}

type t = {
  declarations : declaration list;
  source_file : string;
  compilation_identity : string;
}

type error = { location : Location.t; message : string }

let empty ~source_file =
  { declarations = []; source_file; compilation_identity = "" }

let fail location message = Error { location; message }

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let offsets loc =
  (loc.Location.loc_start.Lexing.pos_cnum, loc.Location.loc_end.pos_cnum)

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let longident_material longident =
  let rec components = function
    | Longident.Lident name -> [ name ]
    | Ldot (parent, name) -> components parent @ [ name ]
    | Lapply _ -> []
  in
  framed "l" (components longident)

let rec type_material typ =
  let recurse = type_material in
  match typ.ctyp_desc with
  | Ttyp_var (None, _) -> "any"
  | Ttyp_var (Some name, _) -> framed "var" [ name ]
  | Ttyp_arrow (label, argument, result) ->
      let label =
        match label with
        | Types.Nolabel -> ""
        | Labelled name -> "~" ^ name
        | Optional name -> "?" ^ name
        | Position name -> "$" ^ name
      in
      framed "arrow" [ label; recurse argument; recurse result ]
  | Ttyp_tuple components | Ttyp_unboxed_tuple components ->
      framed "tuple"
        (List.map
           (fun (label, component) ->
             framed "component"
               [ Option.value ~default:"" label; recurse component ])
           components)
  | Ttyp_constr (_, name, arguments) ->
      framed "constr"
        (longident_material name.txt :: List.map recurse arguments)
  | Ttyp_alias (typ, name, _) ->
      framed "alias"
        [ recurse typ;
          Option.fold ~none:"" ~some:(fun name -> name.Location.txt) name ]
  | Ttyp_poly (variables, typ) ->
      framed "poly" (List.map fst variables @ [ recurse typ ])
  | Ttyp_object _ | Ttyp_class _ | Ttyp_variant _ | Ttyp_package _
  | Ttyp_open _ | Ttyp_quote _ | Ttyp_splice _ | Ttyp_of_kind _
  | Ttyp_call_pos ->
      ""

let payload_string attribute =
  match attribute.Parsetree.attr_payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (text, _, None)); _ },
                [] );
          _;
        };
      ] ->
      Some text
  | PStr _ | PSig _ | PTyp _ | PPat _ -> None

let parse_attribute binding name source_type attribute =
  if
    not
      (attribute.Parsetree.attr_loc.Location.loc_ghost
      && attribute.attr_name.loc.loc_ghost)
  then
    fail attribute.attr_loc
      "logical constant marker is not retained ghost syntax"
  else
    match Option.map (String.split_on_char '|') (payload_string attribute) with
    | Some
        [ "v2"; marker; payload_name; start_; stop; body_start; body_stop;
          type_digest ] -> (
        match
          ( int_of_string_opt start_,
            int_of_string_opt stop,
            int_of_string_opt body_start,
            int_of_string_opt body_stop )
        with
        | Some start_, Some stop, Some body_start, Some body_stop
          when start_ >= 0 && stop >= start_ && body_start >= start_
               && body_stop >= body_start && body_stop <= stop
               && String.equal payload_name name
               && String.length marker = 49
               && String.starts_with ~prefix:"logical-constant." marker ->
            let binding_start, binding_stop = offsets binding.vb_loc in
            let actual_body_start, actual_body_stop = offsets binding.vb_expr.exp_loc in
            let material = type_material source_type in
            let expected_type_digest = Digest.to_hex (Digest.string material) in
            let expected_marker =
              "logical-constant."
              ^ Digest.to_hex
                  (Digest.string
                     (framed "logical-constant-v2"
                        [ name; string_of_int start_; string_of_int stop;
                          string_of_int body_start; string_of_int body_stop;
                          expected_type_digest ]))
            in
            if binding_start <> start_ || binding_stop <> stop then
              fail attribute.attr_loc
                "logical constant source span is stale or forged"
            else if
              actual_body_start <> body_start || actual_body_stop <> body_stop
            then
              fail attribute.attr_loc
                "logical constant body span is stale or forged"
            else if
              String.equal type_digest expected_type_digest
              && String.equal marker expected_marker
            then Ok (marker, body_start, body_stop)
            else
              fail attribute.attr_loc
                "logical constant type or marker is stale or forged"
        | _ ->
            fail attribute.attr_loc
              "logical constant identity payload is malformed")
    | Some _ | None ->
        fail attribute.attr_loc "logical constant identity payload is malformed"

let binding_identity binding =
  match (binding.vb_pat.pat_desc, binding.vb_pat.pat_extra) with
  | Tpat_var (ident, name, uid, _, _),
    [ (Tpat_constraint source_type, _, []) ] ->
      Some
        ( ident,
          name.txt,
          Format.asprintf "%a" Types.Uid.print uid,
          source_type )
  | Tpat_var _, _ | _ -> None

let attributes binding =
  List.filter
    (fun attribute ->
      String.equal attribute.Parsetree.attr_name.txt attribute_name)
    binding.vb_attributes

type scope = {
  canonical_module_path : Path.t option;
  resolved_module_paths : Path.t list;
}

let root_scope =
  { canonical_module_path = None; resolved_module_paths = [] }

let enter_module scope ident name =
  let canonical_module_path =
    match scope.canonical_module_path with
    | None -> Path.Pident ident
    | Some parent -> Path.Pdot (parent, name)
  in
  let resolved_module_paths =
    Path.Pident ident
    :: List.map
         (fun parent -> Path.Pdot (parent, name))
         scope.resolved_module_paths
    |> List.sort_uniq Path.compare
  in
  { canonical_module_path = Some canonical_module_path; resolved_module_paths }

let declaration_paths scope ident name =
  let direct = Path.Pident ident in
  let canonical =
    match scope.canonical_module_path with
    | None -> direct
    | Some parent -> Path.Pdot (parent, name)
  in
  let resolved =
    direct :: canonical
    :: List.map
         (fun parent -> Path.Pdot (parent, name))
         scope.resolved_module_paths
    |> List.sort_uniq Path.compare
  in
  (canonical, resolved)

let rec rebase_path_prefix ~source ~alias path =
  if Path.same path source then Some alias
  else
    match path with
    | Path.Pdot (parent, name) ->
        Option.map
          (fun parent -> Path.Pdot (parent, name))
          (rebase_path_prefix ~source ~alias parent)
    | Path.Pident _ | Path.Papply _ | Path.Pextra_ty _ -> None

let first_marker visit value =
  let found = ref None in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      attributes =
        (fun self attributes ->
          List.iter
            (fun attribute -> self.Tast_iterator.attribute self attribute)
            attributes);
      attribute =
        (fun self attribute ->
          if
            Option.is_none !found
            && String.equal attribute.Parsetree.attr_name.txt attribute_name
          then found := Some attribute;
          default.attribute self attribute);
    }
  in
  visit iterator value;
  !found

let expression_marker expression =
  first_marker
    (fun iterator expression -> iterator.Tast_iterator.expr iterator expression)
    expression

let module_marker expression =
  first_marker
    (fun iterator expression ->
      iterator.Tast_iterator.module_expr iterator expression)
    expression

let structure_marker structure =
  first_marker
    (fun iterator structure -> iterator.Tast_iterator.structure iterator structure)
    structure

let authenticate_binding ~source_file ~authenticated_source_text
    ~authenticated_source_identity ~compilation_identity ~provider_unit
    ~provider_interface ~scope binding =
  match (binding_identity binding, attributes binding) with
  | Some (ident, name, value_uid, source_type), [ attribute ] ->
      let* marker_id, body_start, body_stop =
        parse_attribute binding name source_type attribute
      in
      let* () =
        match authenticated_source_text with
        | Some source when body_stop > String.length source ->
            fail attribute.attr_loc
              "logical constant body is outside its authenticated source"
        | Some _ | None -> Ok ()
      in
      let source_body_digest =
        Digest.to_hex
          (Digest.string
             (framed "logical-constant-source-body-v3"
                [ authenticated_source_identity;
                  string_of_int body_start;
                  string_of_int body_stop ]))
      in
      let canonical_path, resolved_paths = declaration_paths scope ident name in
      (match binding.vb_expr.exp_desc with
      | Texp_function _ ->
          fail binding.vb_expr.exp_loc
            "logical constant marker is attached to a function"
      | _ ->
          Ok
            (Some
               {
                 binding;
                 body = binding.vb_expr;
                 source_type;
                 ident;
                 resolved_paths;
                 value_uid;
                 marker_id;
                 source_name = name;
                 canonical_path = Path.name canonical_path;
                 provider_unit;
                 provider_interface;
                 source_file;
                 compilation_identity;
                 source_body_digest;
                 declaration_location = binding.vb_loc;
               }))
  | Some _, [] -> Ok None
  | Some _, _ :: _ :: _ ->
      fail binding.vb_loc
        "logical constant has duplicate authentication markers"
  | None, [] -> Ok None
  | None, _ :: _ ->
      fail binding.vb_loc
        "logical constant authentication requires one explicitly typed variable"

let authenticate_structure ~source_file ~authenticated_source_text
    ~authenticated_source_identity ~compilation_identity ~provider_unit
    ~provider_interface structure =
  let reject_nested attribute message =
    fail attribute.Parsetree.attr_loc message
  in
  let reject_expression expression =
    match expression_marker expression with
    | None -> Ok ()
    | Some attribute ->
        reject_nested attribute
          "logical constants cannot be declared inside local expressions"
  in
  let rec scan_structure scope declarations structure =
    scan_items scope declarations structure.str_items
  and scan_items scope declarations = function
    | [] -> Ok declarations
    | item :: rest -> (
        match item.str_desc with
        | Tstr_value (Asttypes.Nonrecursive, bindings) ->
            let* declarations = scan_bindings scope declarations bindings in
            scan_items scope declarations rest
        | Tstr_value (Asttypes.Recursive, bindings) ->
            if List.exists (fun binding -> attributes binding <> []) bindings
            then
              fail (List.hd bindings).vb_loc
                "logical constants cannot be recursive"
            else
              let* () =
                List.fold_left
                  (fun result binding ->
                    let* () = result in
                    reject_expression binding.vb_expr)
                  (Ok ()) bindings
              in
              scan_items scope declarations rest
        | Tstr_module binding ->
            let* declarations = scan_module scope declarations binding in
            scan_items scope declarations rest
        | Tstr_recmodule bindings ->
            let* declarations =
              List.fold_left
                (fun result binding ->
                  let* declarations = result in
                  scan_module scope declarations binding)
                (Ok declarations) bindings
            in
            scan_items scope declarations rest
        | Tstr_include include_ ->
            (match module_marker include_.incl_mod with
            | None -> scan_items scope declarations rest
            | Some attribute ->
                reject_nested attribute
                  "logical constants cannot be declared through an anonymous include; put the declaration in a named module")
        | _ -> (
            match
              first_marker
                (fun iterator item ->
                  iterator.Tast_iterator.structure_item iterator item)
                item
            with
            | None -> scan_items scope declarations rest
            | Some attribute ->
                reject_nested attribute
                  "logical constant marker is attached to an unsupported \
                   structure form"))
  and scan_bindings scope declarations = function
    | [] -> Ok declarations
    | binding :: rest ->
        let* declaration =
          authenticate_binding ~source_file ~authenticated_source_text
            ~authenticated_source_identity ~compilation_identity ~provider_unit
            ~provider_interface ~scope binding
        in
        let* () = reject_expression binding.vb_expr in
        scan_bindings scope
          (Option.fold ~none:declarations
             ~some:(fun declaration -> declaration :: declarations)
             declaration)
          rest
  and scan_module scope declarations binding =
    match (binding.mb_id, binding.mb_name.txt) with
    | Some ident, Some name ->
        scan_module_expression (enter_module scope ident name) declarations
          binding.mb_expr
    | None, _ | _, None -> (
        match module_marker binding.mb_expr with
        | None -> Ok declarations
        | Some attribute ->
            reject_nested attribute
              "logical constants require a named enclosing module")
  and scan_module_expression scope declarations expression =
    match expression.mod_desc with
    | Tmod_structure nested -> scan_structure scope declarations nested
    | Tmod_constraint (nested, _, _, _) ->
        scan_module_expression scope declarations nested
    | Tmod_ident (target, _) -> (
        match scope.canonical_module_path with
        | None -> Ok declarations
        | Some alias ->
            let[@log_value.trace] alias_count =
              List.fold_left
                (fun total declaration ->
                  total
                  + List.length
                      (List.filter_map
                         (rebase_path_prefix ~source:target ~alias)
                         declaration.resolved_paths))
                0 declarations
            in
            let declarations =
              List.map
                (fun declaration ->
                  let aliases =
                    declaration.resolved_paths
                    |> List.filter_map
                         (rebase_path_prefix ~source:target ~alias)
                  in
                  {
                    declaration with
                    resolved_paths =
                      List.sort_uniq Path.compare
                        (aliases @ declaration.resolved_paths);
                  })
                declarations
            in
            [%log.trace "authenticated logical constant module aliases"
              ~stage:(Delator.Field.string "logical-constant-authentication")
              ~alias_path:(Delator.Field.string (Path.name alias))
              ~target_path:(Delator.Field.string (Path.name target))
              ~resolved_value_count:
                (Delator.Field.int (alias_count [@log_value.trace]))
              ~decision:(Delator.Field.string "recorded")];
            Ok declarations)
    | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _ | Tmod_unpack _ -> (
        match module_marker expression with
        | None -> Ok declarations
        | Some attribute ->
            reject_nested attribute
              "logical constants inside functor, applied, or unpacked module \
               expressions are unsupported")
  in
  Result.map List.rev (scan_structure root_scope [] structure)

let authenticate ~source_file ~authenticated_source_text ~artifact
    ~compilation_identity structure =
  if Option.is_none (structure_marker structure) then Ok (empty ~source_file)
  else
    let* compilation_identity =
      match compilation_identity with
      | Some (Ok identity) ->
          Ok
            (Callback_certificate_private.compilation_identity_string identity)
      | Some (Error message) ->
          fail (Location.in_file source_file)
            ("logical constant compilation identity is invalid: " ^ message)
      | None ->
          fail (Location.in_file source_file)
            "logical constants require an authenticated CMT identity"
    in
    let* provider_unit, provider_interface =
      match artifact with
      | Some artifact -> (
          match
            Typedtree_adapter_issuance_private.logical_builtin_provider_identity
              ?authenticated_source_text artifact ~source_file
          with
          | Ok identity -> Ok identity
          | Error message -> fail (Location.in_file source_file) message)
      | None ->
          fail (Location.in_file source_file)
            "logical constants require an authenticated provider identity"
    in
    let* authenticated_source_identity =
      match artifact with
      | Some artifact -> (
          match
            Typedtree_adapter_issuance_private.logical_builtin_source_identity
              artifact ~source_file
          with
          | Ok identity -> Ok identity
          | Error message -> fail (Location.in_file source_file) message)
      | None ->
          fail (Location.in_file source_file)
            "logical constants require an authenticated source identity"
    in
    let* () =
      match artifact with
      | Some artifact
        when
          Typedtree_adapter_issuance_private.authenticate_logical_builtin_artifact
            artifact ~source_file ?authenticated_source_text ->
          Ok ()
      | Some _ | None ->
          fail (Location.in_file source_file)
            "logical constants require the exact retained source/CMT artifact"
    in
    let* declarations =
      authenticate_structure ~source_file ~authenticated_source_text
        ~authenticated_source_identity ~compilation_identity ~provider_unit
        ~provider_interface structure
    in
    [%log.debug "authenticated logical constant declarations"
      ~stage:(Delator.Field.string "logical-constant-authentication")
      ~source_file:(Delator.Field.string (Filename.basename source_file))
      ~source_route:
        (Delator.Field.string
           (if Option.is_some authenticated_source_text then "adjacent-source"
            else "retained-artifact"))
      ~declaration_count:(Delator.Field.int (List.length declarations))
      ~decision:(Delator.Field.string "accepted")];
    Ok { declarations; source_file; compilation_identity }

let declarations scan = scan.declarations
let find_binding scan binding =
  List.find_opt (fun declaration -> declaration.binding == binding)
    scan.declarations
let binding (declaration : declaration) = declaration.binding
let body (declaration : declaration) = declaration.body
let source_type (declaration : declaration) = declaration.source_type
let ident (declaration : declaration) = declaration.ident
let resolved_paths (declaration : declaration) = declaration.resolved_paths
let value_uid (declaration : declaration) = declaration.value_uid
let marker_id (declaration : declaration) = declaration.marker_id
let source_name (declaration : declaration) = declaration.source_name
let canonical_path (declaration : declaration) = declaration.canonical_path
let provider_unit (declaration : declaration) = declaration.provider_unit
let provider_interface (declaration : declaration) =
  declaration.provider_interface
let source_file (declaration : declaration) = declaration.source_file
let compilation_identity (declaration : declaration) =
  declaration.compilation_identity
let source_body_digest (declaration : declaration) =
  declaration.source_body_digest
let declaration_location (declaration : declaration) =
  declaration.declaration_location

let authenticate_use scan declaration ~path ~value_uid ~location =
  if
    not
      (List.exists (fun candidate -> candidate == declaration) scan.declarations)
  then fail location "logical constant belongs to another authenticated scan"
  else if not (List.exists (Path.same path) declaration.resolved_paths) then
    fail location "logical constant use does not resolve to its exact declaration"
  else if not (String.equal value_uid declaration.value_uid) then
    fail location "logical constant use has a stale compiler UID"
  else if
    not
      (String.equal scan.source_file declaration.source_file
      && String.equal scan.compilation_identity declaration.compilation_identity)
  then fail location "logical constant use belongs to another compilation"
  else Ok ()
