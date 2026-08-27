open Typedtree

let attribute_name = "verocaml.internal.symbolic.declaration.v1"

type declaration = {
  binding : value_binding;
  ident : Ident.t;
  resolved_paths : Path.t list;
  value_uid : string;
  marker_id : string;
  source_name : string;
  canonical_path : string;
  source_file : string;
  compilation_identity : string;
  declaration_location : Location.t;
}

type t = {
  declarations : declaration list;
  source_file : string;
  compilation_identity : string;
}

type error = { location : Location.t; message : string }
type use_error_kind = Authentication | Declaration | Application
type use_error = {
  use_error_kind : use_error_kind;
  location : Location.t;
  message : string;
}

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
        [
          recurse typ;
          Option.fold ~none:""
            ~some:(fun name -> name.Location.txt)
            name;
        ]
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
      "symbolic declaration attribute is not retained ghost syntax"
  else
    match Option.map (String.split_on_char '|') (payload_string attribute) with
    | Some [ "v1"; marker; payload_name; start_; stop; type_digest ] -> (
        match (int_of_string_opt start_, int_of_string_opt stop) with
        | Some start_, Some stop
          when start_ >= 0 && stop >= start_
               && String.equal payload_name name
               && String.length marker = 41
               && String.starts_with ~prefix:"symbolic." marker ->
            let binding_start, binding_stop = offsets binding.vb_loc in
            let material = type_material source_type in
            let expected_type_digest =
              Digest.to_hex (Digest.string material)
            in
            let expected_marker =
              "symbolic."
              ^ Digest.to_hex
                  (Digest.string
                     (framed "symbolic-v1"
                        [
                          name;
                          string_of_int start_;
                          string_of_int stop;
                          material;
                        ]))
            in
            if binding_start <> start_ || binding_stop <> stop then
              fail attribute.attr_loc
                "symbolic declaration source span is stale or forged"
            else if
              String.equal type_digest expected_type_digest
              && String.equal marker expected_marker
            then Ok marker
            else
              fail attribute.attr_loc
                "symbolic declaration source type or marker is stale or forged"
        | Some _, Some _ | None, _ | _, None ->
            fail attribute.attr_loc
              "symbolic declaration identity payload is malformed")
    | Some _ | None ->
        fail attribute.attr_loc
          "symbolic declaration identity payload is malformed"

let string_argument = function
  | { exp_desc = Texp_constant (Const_string (text, _, _)); _ } -> Some text
  | _ -> None

let false_assertion = function
  | {
      exp_desc =
        Texp_assert
          ( {
              exp_desc =
                Texp_construct
                  (_, { Types.cstr_name = "false"; cstr_arity = 0; _ }, [], _);
              _;
            },
            _ );
      _;
    } ->
      true
  | _ -> false

let canonical_thunk expression =
  match expression.exp_desc with
  | Texp_function
      {
        params =
          [
            {
              fp_arg_label = Types.Nolabel;
              fp_kind = Tparam_pat { pat_desc = Tpat_any; _ };
              fp_partial = Total;
              _;
            };
          ];
        body = Tfunction_body body;
        _;
      } ->
      false_assertion body
  | _ -> false

let carrier resolves_to_spec_carrier marker expression =
  match expression.exp_desc with
  | Texp_apply
      ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
        [
          (Types.Nolabel, Arg (marker_expression, _));
          (Types.Nolabel, Arg (thunk, _));
        ],
        _,
        _,
        _ )
    when resolves_to_spec_carrier path
         && Option.equal String.equal (string_argument marker_expression)
              (Some marker)
         && canonical_thunk thunk ->
      true
  | _ -> false

let rec terminal expression =
  match expression.exp_desc with
  | Texp_function
      {
        params;
        body = Tfunction_body body;
        _;
      }
    when
      params <> []
      && List.for_all
           (fun parameter ->
             parameter.fp_partial = Total
             &&
             match parameter.fp_kind with
             | Tparam_pat { pat_desc = Tpat_any; _ } -> true
             | Tparam_pat _ -> false
             | Tparam_optional_default _ -> false)
           params ->
      terminal body
  | _ -> expression

let binding_identity binding =
  match (binding.vb_pat.pat_desc, binding.vb_pat.pat_extra) with
  | Tpat_var (ident, name, uid, _, _),
    [ (Tpat_constraint source_type, _, []) ] ->
      Some
        ( ident,
          name.txt,
          Format.asprintf "%a" Types.Uid.print uid,
          source_type )
  | Tpat_var _, _ -> None
  | _ -> None

let attributes binding =
  List.filter
    (fun attribute ->
      String.equal attribute.Parsetree.attr_name.txt attribute_name)
    binding.vb_attributes

let authenticate_binding ~source_file ~compilation_identity
    ~resolves_to_spec_carrier ~canonical_path ~resolved_paths binding =
  match (binding_identity binding, attributes binding) with
  | Some (ident, name, value_uid, source_type), [ attribute ] ->
      let* marker_id = parse_attribute binding name source_type attribute in
      if not (carrier resolves_to_spec_carrier marker_id (terminal binding.vb_expr))
      then
        fail binding.vb_expr.exp_loc
          "symbolic declaration carrier is not canonical"
      else
        Ok
          (Some
             {
               binding;
               ident;
               resolved_paths;
               value_uid;
               marker_id;
               source_name = name;
               canonical_path = Path.name canonical_path;
               source_file;
               compilation_identity;
               declaration_location = binding.vb_loc;
             })
  | Some _, [] -> Ok None
  | Some _, _ :: _ :: _ ->
      fail binding.vb_loc
        "symbolic declaration has duplicate authentication attributes"
  | None, [] -> Ok None
  | None, _ :: _ ->
      fail binding.vb_loc
        "symbolic declaration authentication is attached to a non-value"

type scope = {
  canonical_module_path : Path.t option;
  resolved_module_paths : Path.t list;
}

let root_scope =
  { canonical_module_path = None; resolved_module_paths = [] }

let first_symbolic_attribute visit value =
  let found = ref None in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      attributes =
        (fun self attributes ->
          List.iter (fun attribute -> self.Tast_iterator.attribute self attribute)
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

let structure_attribute structure =
  first_symbolic_attribute
    (fun iterator structure -> iterator.Tast_iterator.structure iterator structure)
    structure

let expression_attribute expression =
  first_symbolic_attribute
    (fun iterator expression -> iterator.Tast_iterator.expr iterator expression)
    expression

let module_expression_attribute expression =
  first_symbolic_attribute
    (fun iterator expression ->
      iterator.Tast_iterator.module_expr iterator expression)
    expression

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
  {
    canonical_module_path = Some canonical_module_path;
    resolved_module_paths;
  }

let declaration_paths scope ident name =
  let direct = Path.Pident ident in
  let canonical =
    match scope.canonical_module_path with
    | None -> direct
    | Some parent -> Path.Pdot (parent, name)
  in
  let resolved =
    direct
    :: canonical
    :: List.map
         (fun parent -> Path.Pdot (parent, name))
         scope.resolved_module_paths
    |> List.sort_uniq Path.compare
  in
  (canonical, resolved)

let authenticate_structure ~source_file ~compilation_identity
    ~resolves_to_spec_carrier structure =
  let reject_nested attribute message =
    fail attribute.Parsetree.attr_loc message
  in
  let reject_expression expression =
    match expression_attribute expression with
    | None -> Ok ()
    | Some attribute ->
        reject_nested attribute
          "symbolic declarations inside local expressions are unsupported"
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
                "symbolic declarations cannot be recursive"
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
            let* declarations =
              scan_module_binding scope declarations binding
            in
            scan_items scope declarations rest
        | Tstr_recmodule bindings ->
            let* declarations =
              List.fold_left
                (fun result binding ->
                  let* declarations = result in
                  scan_module_binding scope declarations binding)
                (Ok declarations) bindings
            in
            scan_items scope declarations rest
        | Tstr_include include_ ->
            let* declarations =
              scan_module_expression scope declarations include_.incl_mod
            in
            scan_items scope declarations rest
        | _ -> (
            match
              first_symbolic_attribute
                (fun iterator item ->
                  iterator.Tast_iterator.structure_item iterator item)
                item
            with
            | None -> scan_items scope declarations rest
            | Some attribute ->
                reject_nested attribute
                  "symbolic declaration is attached to an unsupported \
                   structure form"))
  and scan_bindings scope declarations = function
    | [] -> Ok declarations
    | binding :: rest ->
        let canonical_path, resolved_paths =
          match binding_identity binding with
          | Some (ident, name, _, _) ->
              declaration_paths scope ident name
          | None -> (Predef.path_unit, [])
        in
        let* declaration =
          authenticate_binding ~source_file ~compilation_identity
            ~resolves_to_spec_carrier ~canonical_path ~resolved_paths binding
        in
        let* () = reject_expression binding.vb_expr in
        scan_bindings scope
          (Option.fold ~none:declarations
             ~some:(fun declaration -> declaration :: declarations)
             declaration)
          rest
  and scan_module_binding scope declarations binding =
    match (binding.mb_id, binding.mb_name.txt) with
    | Some ident, Some name ->
        scan_module_expression (enter_module scope ident name) declarations
          binding.mb_expr
    | None, _ | _, None -> (
        match module_expression_attribute binding.mb_expr with
        | None -> Ok declarations
        | Some attribute ->
            reject_nested attribute
              "symbolic declarations require a named enclosing module")
  and scan_module_expression scope declarations expression =
    match expression.mod_desc with
    | Tmod_structure nested -> scan_structure scope declarations nested
    | Tmod_constraint (nested, _, _, _) ->
        scan_module_expression scope declarations nested
    | Tmod_ident _ -> Ok declarations
    | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _ | Tmod_unpack _ -> (
        match module_expression_attribute expression with
        | None -> Ok declarations
        | Some attribute ->
            reject_nested attribute
              "symbolic declarations inside functor, applied, or unpacked \
               module expressions are unsupported")
  in
  Result.map List.rev (scan_structure root_scope [] structure)

let authenticate ~source_file ~artifact ~compilation_identity
    ~resolves_to_spec_carrier structure =
  let has_declarations = Option.is_some (structure_attribute structure) in
  if not has_declarations then
    Ok (empty ~source_file)
  else
  let* compilation_identity =
    match compilation_identity with
    | Some (Ok identity) ->
        Ok
          (Callback_certificate_private.compilation_identity_string identity)
    | Some (Error message) ->
        fail (Location.in_file source_file)
          ("symbolic compilation identity is invalid: " ^ message)
    | None ->
        fail (Location.in_file source_file)
          "symbolic declarations require an authenticated CMT identity"
  in
  let* () =
    match artifact with
    | Some artifact
      when
        Typedtree_adapter_issuance_private.authenticate_logical_builtin_artifact
          artifact ~source_file ->
        Ok ()
    | Some _ | None ->
        fail (Location.in_file source_file)
          "symbolic declarations require the exact retained source/CMT artifact"
  in
  let* declarations =
    authenticate_structure ~source_file ~compilation_identity
      ~resolves_to_spec_carrier structure
  in
  Ok { declarations; source_file; compilation_identity }

let declarations scan = scan.declarations

let find_binding scan binding =
  List.find_opt (fun declaration -> declaration.binding == binding)
    scan.declarations

let binding (declaration : declaration) = declaration.binding
let definition_body declaration = terminal declaration.binding.vb_expr
let ident (declaration : declaration) = declaration.ident
let value_uid (declaration : declaration) = declaration.value_uid
let marker_id (declaration : declaration) = declaration.marker_id
let source_name (declaration : declaration) = declaration.source_name
let canonical_path (declaration : declaration) = declaration.canonical_path
let source_file (declaration : declaration) = declaration.source_file
let compilation_identity (declaration : declaration) =
  declaration.compilation_identity
let declaration_location (declaration : declaration) =
  declaration.declaration_location

let authenticate_use scan declaration ~path ~value_uid ~location =
  let exact_path =
    List.exists (Path.same path) declaration.resolved_paths
  in
  if
    not
      (List.exists (fun candidate -> candidate == declaration) scan.declarations)
  then fail location "symbolic declaration is stale or belongs to another scan"
  else if not exact_path then
    fail location "symbolic use does not resolve to its exact same-unit Ident"
  else if not (String.equal value_uid declaration.value_uid) then
    fail location "symbolic use has a stale or forged compiler UID"
  else if
    not
      (String.equal scan.source_file declaration.source_file
      && String.equal scan.compilation_identity declaration.compilation_identity)
  then fail location "symbolic use belongs to another source/CMT identity"
  else Ok ()

let authenticate_candidate ~logical ~candidates ~path ~value_uid ~location =
  let use_error use_error_kind message =
    Error { use_error_kind; location; message }
  in
  match candidates path with
  | [ (owner, scan, source, Some definition) ] -> (
      match authenticate_use scan source ~path ~value_uid ~location with
      | Error error ->
          Error
            {
              use_error_kind = Authentication;
              location = error.location;
              message = error.message;
            }
      | Ok () ->
          if not logical then
            use_error Application
              "symbolic declarations are unavailable to executable code"
          else
            match definition.Sst.body with
            | Sst.Symbolic_declaration declaration -> Ok (owner, declaration)
            | Sst.Checked_exec _ | Sst.Spec_definition _
            | Sst.Recursive_spec_definition _ | Sst.Proof_body _
            | Sst.External_specification _
            | Sst.Trusted_external_spec_target _
            | Sst.Trusted_external_body _ ->
                use_error Declaration
                  "symbolic source lowered to a nonsymbolic definition")
  | [ (_, _, _, None) ] ->
      use_error Authentication
        "symbolic declaration has no current lowering identity"
  | [] ->
      use_error Authentication
        "symbolic use does not resolve to a local declaration"
  | _ :: _ :: _ ->
      use_error Authentication
        "symbolic use resolves to multiple local declarations"

let identifier_expression declaration ~result_type ~span =
  Parametric_lowering_private.infer_type_arguments ~binders:(Symbolic_application_private.type_binders declaration) ~formals:[] ~actuals:[] ~formal_result:(Symbolic_application_private.declaration_result_type declaration) ~actual_result:result_type
  |> Result.map (fun type_arguments ->
         Symbolic_application_private.create declaration ~type_arguments
           ~arguments:[] ~argument_types:[] ~result_type ~span) |> Result.join
  |> Result.map (fun application ->
         { Sst.expression_desc = Sst.Symbolic_application application;
           typ = result_type;
           span })

let application_expression declaration ~result_type ~span lowered =
  match lowered.Sst.expression_desc with
  | Sst.Direct_call { type_arguments; arguments; _ } ->
      let arguments =
        List.map
          (fun argument -> snd (Sst.require_value_argument argument))
          arguments
      in
      Symbolic_application_private.create declaration ~type_arguments ~arguments
        ~argument_types:(List.map (fun expression -> expression.Sst.typ) arguments)
        ~result_type ~span
      |> Result.map (fun application ->
             {
               Sst.expression_desc = Sst.Symbolic_application application;
               typ = result_type;
               span;
             })
  | _ ->
      Error "symbolic application did not lower as one direct first-order call"
