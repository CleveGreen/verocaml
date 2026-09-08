open Ast_helper
open Asttypes
open Parsetree

let attribute_name = "verocaml.internal.logical_constant.definition.v1"

let ghost loc = { loc with Location.loc_ghost = true }

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let longident_material longident =
  let rec components = function
    | Longident.Lident name -> [ name ]
    | Ldot (parent, name) -> components parent @ [ name ]
    | Lapply _ ->
        [%log.debug "rejected logical constant type syntax"
          ~stage:(Delator.Field.string "logical-constant-ppx")
          ~construct:(Delator.Field.string "applied-module-path")
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:(Delator.Field.string "unsupported-type-syntax")];
        Location.raise_errorf
          "logical constant types do not support applied module paths"
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
      [%log.debug "rejected logical constant type syntax"
        ~stage:(Delator.Field.string "logical-constant-ppx")
        ~construct:(Delator.Field.string "core-type")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "unsupported-type-syntax")];
      Location.raise_errorf ~loc:typ.ptyp_loc
        "logical constant type syntax is unsupported"

let explicit_type binding =
  match binding.pvb_constraint with
  | Some (Pvc_constraint { locally_abstract_univars = []; typ }) -> Some typ
  | Some (Pvc_constraint { locally_abstract_univars = _ :: _; _ })
  | Some (Pvc_coercion _) | None ->
      None

let offsets loc =
  (loc.Location.loc_start.Lexing.pos_cnum, loc.Location.loc_end.pos_cnum)

let string_payload ~loc text =
  let loc = ghost loc in
  PStr [ Str.eval ~loc (Exp.constant ~loc (Pconst_string (text, loc, None))) ]

let declaration_attribute binding ~name typ =
  let loc = ghost binding.pvb_loc in
  let start_, stop = offsets binding.pvb_loc in
  let body_start, body_stop = offsets binding.pvb_expr.pexp_loc in
  let type_digest = Digest.to_hex (Digest.string (type_material typ)) in
  let marker =
    "logical-constant."
    ^ Digest.to_hex
        (Digest.string
           (framed "logical-constant-v2"
              [ name.txt; string_of_int start_; string_of_int stop;
                string_of_int body_start; string_of_int body_stop; type_digest ]))
  in
  let payload =
    String.concat "|"
      [ "v2"; marker; name.txt; string_of_int start_; string_of_int stop;
        string_of_int body_start; string_of_int body_stop; type_digest ]
  in
  {
    attr_name = { txt = attribute_name; loc };
    attr_payload = string_payload ~loc payload;
    attr_loc = loc;
  }
