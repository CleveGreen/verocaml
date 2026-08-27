open Ast_helper
open Asttypes
open Parsetree

let extension_name = "verocaml.symbolic"
let attribute_name = "verocaml.internal.symbolic.declaration.v1"

let ghost loc = { loc with Location.loc_ghost = true }

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let longident_material longident =
  let rec components = function
    | Longident.Lident name -> [ name ]
    | Ldot (parent, name) -> components parent @ [ name ]
    | Lapply _ ->
        Location.raise_errorf
          "symbolic declaration types do not support applied module paths"
  in
  framed "l" (components longident)

let rec type_material typ =
  let recurse = type_material in
  match typ.ptyp_desc with
  | Ptyp_any _ -> "any"
  | Ptyp_var (name, _) -> framed "var" [ name ]
  | Ptyp_arrow (label, argument, result, _, _) ->
      let label =
        match label with
        | Nolabel -> ""
        | Labelled name -> "~" ^ name
        | Optional name -> "?" ^ name
      in
      framed "arrow" [ label; recurse argument; recurse result ]
  | Ptyp_tuple components | Ptyp_unboxed_tuple components ->
      framed "tuple"
        (List.map
           (fun (label, component) ->
             framed "component"
               [ Option.value ~default:"" label; recurse component ])
           components)
  | Ptyp_constr (name, arguments) ->
      framed "constr"
        (longident_material name.txt :: List.map recurse arguments)
  | Ptyp_alias (typ, name, _) ->
      framed "alias"
        [ recurse typ; Option.fold ~none:"" ~some:(fun name -> name.txt) name ]
  | Ptyp_poly (variables, typ) ->
      framed "poly"
        (List.map (fun (variable, _) -> variable.txt) variables @ [ recurse typ ])
  | Ptyp_object _ | Ptyp_class _ | Ptyp_variant _ | Ptyp_package _
  | Ptyp_extension _ | Ptyp_open _ | Ptyp_quote _ | Ptyp_splice _
  | Ptyp_of_kind _ ->
      Location.raise_errorf ~loc:typ.ptyp_loc
        "symbolic declaration type syntax is unsupported"

let offsets loc =
  (loc.Location.loc_start.Lexing.pos_cnum, loc.Location.loc_end.pos_cnum)

let marker_id value =
  let start_, stop = offsets value.pval_loc in
  let material =
    framed "symbolic-v1"
      [ value.pval_name.txt; string_of_int start_; string_of_int stop;
        type_material value.pval_type ]
  in
  "symbolic." ^ Digest.to_hex (Digest.string material)

let string_payload ~loc text =
  let loc = ghost loc in
  PStr [ Str.eval ~loc (Exp.constant ~loc (Pconst_string (text, loc, None))) ]

let declaration_attribute value marker =
  let loc = ghost value.pval_loc in
  let start_, stop = offsets value.pval_loc in
  let payload =
    String.concat "|"
      [ "v1"; marker; value.pval_name.txt; string_of_int start_;
        string_of_int stop;
        Digest.to_hex (Digest.string (type_material value.pval_type)) ]
  in
  {
    attr_name = { txt = attribute_name; loc };
    attr_payload = string_payload ~loc payload;
    attr_loc = loc;
  }

let false_expression ~loc =
  Exp.construct ~loc { txt = Longident.Lident "false"; loc } None

let carrier ~loc marker =
  let loc = ghost loc in
  let unit_parameter =
    {
      pparam_loc = loc;
      pparam_desc = Pparam_val (Nolabel, None, Pat.any ~loc ());
    }
  in
  let constraint_ =
    {
      mode_annotations = [];
      ret_mode_annotations = [];
      ret_type_constraint = None;
    }
  in
  let thunk =
    Exp.function_ ~loc [ unit_parameter ] constraint_
      (Pfunction_body (Exp.assert_ ~loc (false_expression ~loc)))
  in
  Exp.apply ~loc
    (Exp.ident ~loc
       {
         txt =
           Longident.Ldot (Longident.Lident "Vero_ghost", "spec_definition");
         loc;
       })
    [
      (Nolabel, Exp.constant ~loc (Pconst_string (marker, loc, None)));
      (Nolabel, thunk);
    ]

let retained_expression typ marker =
  let rec arrows reversed typ =
    let loc = ghost typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_arrow (label, _, result, _, _) ->
        let parameter =
          {
            pparam_loc = loc;
            pparam_desc = Pparam_val (label, None, Pat.any ~loc ());
          }
        in
        arrows (parameter :: reversed) result
    | _ -> (List.rev reversed, loc)
  in
  match arrows [] typ with
  | [], loc -> carrier ~loc marker
  | parameters, loc ->
      let constraint_ =
        {
          mode_annotations = [];
          ret_mode_annotations = [];
          ret_type_constraint = None;
        }
      in
      Exp.function_ ~loc parameters constraint_
        (Pfunction_body (carrier ~loc marker))

let retained_binding value =
  let loc = ghost value.pval_loc in
  let marker = marker_id value in
  let pattern =
    Pat.constraint_ ~loc
      (Pat.var ~loc { value.pval_name with loc })
      (Some { value.pval_type with ptyp_loc = loc }) []
  in
  Vb.mk ~loc ~attrs:[ declaration_attribute value marker ] pattern
    (retained_expression value.pval_type marker)

let rewrite_item ~keep_ghost item =
  match item.pstr_desc with
  | Pstr_extension (({ txt; _ }, payload), attributes)
    when String.equal txt extension_name -> (
      if attributes <> [] then
        Location.raise_errorf ~loc:item.pstr_loc
          "[%%%%%s] does not accept extension attributes" extension_name;
      match payload with
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim = [] && value.pval_attributes = [] ->
          if keep_ghost then
            [
              Str.value ~loc:(ghost item.pstr_loc) Nonrecursive
                [ retained_binding value ];
            ]
          else []
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim <> [] ->
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] requires val syntax, not an external primitive"
            extension_name
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ] ->
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] declarations do not accept attributes" extension_name
      | PStr _ ->
          Location.raise_errorf ~loc:item.pstr_loc
            "[%%%%%s] requires exactly one val declaration" extension_name
      | PTyp _ | PSig _ | PPat _ ->
          Location.raise_errorf ~loc:item.pstr_loc
            "[%%%%%s] requires a structure val payload" extension_name)
  | _ -> [ item ]

let rewrite_structure ~keep_ghost structure =
  List.concat_map (rewrite_item ~keep_ghost) structure
