open Ast_helper
open Asttypes
open Parsetree

let extension_name = "verocaml.symbolic"
let attribute_name = "verocaml.internal.symbolic.declaration.v1"
let interface_attribute_name = "verocaml.internal.symbolic.interface.v1"
let retained_family_name = "verocaml.internal.artifact_family.retained-v1"

let symbolic_rejection ~route:_route ~stage:_stage
    ~payload_kind:_payload_kind ~reason_class:_reason_class =
  [%log.debug "rejected symbolic declaration syntax"
    ~route:(Delator.Field.string _route)
    ~stage:(Delator.Field.string _stage)
    ~payload_kind:(Delator.Field.string _payload_kind)
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string _reason_class)]

let symbolic_admission ~route:_route ~stage:_stage
    ~payload_kind:_payload_kind ~keep_ghost:_keep_ghost =
  [%log.trace "admitted symbolic declaration"
    ~route:(Delator.Field.string _route)
    ~stage:(Delator.Field.string _stage)
    ~payload_kind:(Delator.Field.string _payload_kind)
    ~keep_ghost:(Delator.Field.bool _keep_ghost)
    ~decision:(Delator.Field.string "accepted")]

let ghost loc = { loc with Location.loc_ghost = true }

let framed tag fields =
  let field value = Printf.sprintf "%d:%s" (String.length value) value in
  tag ^ String.concat "" (List.map field fields)

let longident_material longident =
  let rec components = function
    | Longident.Lident name -> [ name ]
    | Ldot (parent, name) -> components parent @ [ name ]
    | Lapply _ ->
        symbolic_rejection ~route:"syntax" ~stage:"type-material"
          ~payload_kind:"module-path" ~reason_class:"applied-module-path";
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
      symbolic_rejection ~route:"syntax" ~stage:"type-material"
        ~payload_kind:"core-type" ~reason_class:"unsupported-type-syntax";
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

let interface_marker_id value =
  let start_, stop = offsets value.pval_loc in
  let type_digest = Digest.to_hex (Digest.string (type_material value.pval_type)) in
  let material =
    framed "symbolic-interface-v1"
      [ value.pval_name.txt; string_of_int start_; string_of_int stop; type_digest ]
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

let interface_attribute value marker =
  let attribute = declaration_attribute value marker in
  {
    attribute with
    attr_name = { attribute.attr_name with txt = interface_attribute_name };
  }

let retained_family_attribute ~loc ~issuer =
  let loc = ghost loc in
  let family = "retained-v1" in
  let receipt =
    String.concat "|"
      [ "v1"; "issuer=verocaml.ppx"; "route=" ^ issuer;
        "family=" ^ family ]
  in
  {
    attr_name = { txt = retained_family_name; loc };
    attr_payload = string_payload ~loc receipt;
    attr_loc = loc;
  }

let payload_string attribute =
  match attribute.attr_payload with
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

let issued_interface_attribute attribute =
  String.equal attribute.attr_name.txt interface_attribute_name
  && attribute.attr_loc.loc_ghost
  && attribute.attr_name.loc.loc_ghost
  &&
  match Option.map (String.split_on_char '|') (payload_string attribute) with
  | Some [ "v1"; marker; name; start_; stop; type_digest ] ->
      String.starts_with ~prefix:"symbolic." marker
      && String.length marker = 41
      && name <> ""
      && Option.is_some (int_of_string_opt start_)
      && Option.is_some (int_of_string_opt stop)
      && String.length type_digest = 32
  | Some _ | None -> false

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

let retained_binding ~issuer value =
  let loc = ghost value.pval_loc in
  let marker = marker_id value in
  let pattern =
    Pat.constraint_ ~loc
      (Pat.var ~loc { value.pval_name with loc })
      (Some { value.pval_type with ptyp_loc = loc }) []
  in
  Vb.mk ~loc
    ~attrs:
      [ declaration_attribute value marker; retained_family_attribute ~loc ~issuer ]
    pattern
    (retained_expression value.pval_type marker)

let rewrite_item ~keep_ghost ~issuer item =
  match item.pstr_desc with
  | Pstr_extension (({ txt; _ }, payload), attributes)
    when String.equal txt extension_name -> (
      if attributes <> [] then (
        symbolic_rejection ~route:issuer ~stage:"structure-admission"
          ~payload_kind:"extension" ~reason_class:"extension-attributes";
        Location.raise_errorf ~loc:item.pstr_loc
          "[%%%%%s] does not accept extension attributes" extension_name);
      match payload with
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim = [] && value.pval_attributes = [] ->
          symbolic_admission ~route:issuer ~stage:"structure-admission"
            ~payload_kind:"value-declaration" ~keep_ghost;
          if keep_ghost then
            [
              Str.value ~loc:(ghost item.pstr_loc) Nonrecursive
                [ retained_binding ~issuer value ];
            ]
          else []
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim <> [] ->
          symbolic_rejection ~route:issuer ~stage:"structure-admission"
            ~payload_kind:"external-primitive"
            ~reason_class:"external-primitive";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] requires val syntax, not an external primitive"
            extension_name
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ] ->
          symbolic_rejection ~route:issuer ~stage:"structure-admission"
            ~payload_kind:"value-declaration"
            ~reason_class:"declaration-attributes";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] declarations do not accept attributes" extension_name
      | PStr _ ->
          symbolic_rejection ~route:issuer ~stage:"structure-admission"
            ~payload_kind:"structure" ~reason_class:"declaration-cardinality";
          Location.raise_errorf ~loc:item.pstr_loc
            "[%%%%%s] requires exactly one val declaration" extension_name
      | PTyp _ | PSig _ | PPat _ ->
          symbolic_rejection ~route:issuer ~stage:"structure-admission"
            ~payload_kind:"non-structure"
            ~reason_class:"payload-category";
          Location.raise_errorf ~loc:item.pstr_loc
            "[%%%%%s] requires a structure val payload" extension_name)
  | _ -> [ item ]

let rewrite_structure
    ~keep_ghost:(keep_ghost [@delator.field string_of_bool])
    ~issuer:(issuer [@delator.field Fun.id]) (structure [@delator.skip]) =
  let rewritten =
    List.concat_map (rewrite_item ~keep_ghost ~issuer) structure
  in
  [%log.debug "rewrote symbolic implementation structure"
    ~route:(Delator.Field.string issuer)
    ~stage:(Delator.Field.string "structure-admission")
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length structure))
    ~output_items:(Delator.Field.int (List.length rewritten))
    ~decision:(Delator.Field.string "completed")];
  rewritten
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let retained_signature_value value =
  let loc = ghost value.pval_loc in
  let marker = interface_marker_id value in
  {
    value with
    pval_name = { value.pval_name with loc };
    pval_type = { value.pval_type with ptyp_loc = loc };
    pval_attributes = [ interface_attribute value marker ];
    pval_loc = loc;
  }

let rewrite_signature_item ~keep_ghost ~issuer item =
  match item.psig_desc with
  | Psig_extension (({ txt; _ }, payload), attributes)
    when String.equal txt extension_name -> (
      if attributes <> [] then (
        symbolic_rejection ~route:issuer ~stage:"signature-admission"
          ~payload_kind:"extension" ~reason_class:"extension-attributes";
        Location.raise_errorf ~loc:item.psig_loc
          "[%%%%%s] does not accept extension attributes" extension_name);
      match payload with
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim = [] && value.pval_attributes = [] ->
          symbolic_admission ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"structure-value-declaration" ~keep_ghost;
          if keep_ghost then
            [ Sig.value ~loc:(ghost item.psig_loc) (retained_signature_value value) ]
          else []
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ]
        when value.pval_prim <> [] ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"external-primitive"
            ~reason_class:"external-primitive";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] requires val syntax, not an external primitive"
            extension_name
      | PStr [ { pstr_desc = Pstr_primitive value; _ } ] ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"structure-value-declaration"
            ~reason_class:"declaration-attributes";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] declarations do not accept attributes" extension_name
      | PStr _ ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"structure" ~reason_class:"declaration-cardinality";
          Location.raise_errorf ~loc:item.psig_loc
            "[%%%%%s] requires exactly one val declaration" extension_name
      | PSig { psg_items = [ { psig_desc = Psig_value value; _ } ]; _ }
        when value.pval_prim = [] && value.pval_attributes = [] ->
          symbolic_admission ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"signature-value-declaration" ~keep_ghost;
          if keep_ghost then
            [ Sig.value ~loc:(ghost item.psig_loc) (retained_signature_value value) ]
          else []
      | PSig { psg_items = [ { psig_desc = Psig_value value; _ } ]; _ }
        when value.pval_prim <> [] ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"external-primitive"
            ~reason_class:"external-primitive";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] requires val syntax, not an external primitive"
            extension_name
      | PSig { psg_items = [ { psig_desc = Psig_value value; _ } ]; _ } ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"signature-value-declaration"
            ~reason_class:"declaration-attributes";
          Location.raise_errorf ~loc:value.pval_loc
            "[%%%%%s] declarations do not accept attributes" extension_name
      | PSig _ ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"signature" ~reason_class:"declaration-cardinality";
          Location.raise_errorf ~loc:item.psig_loc
            "[%%%%%s] requires exactly one val declaration" extension_name
      | PTyp _ | PPat _ ->
          symbolic_rejection ~route:issuer ~stage:"signature-admission"
            ~payload_kind:"non-signature"
            ~reason_class:"payload-category";
          Location.raise_errorf ~loc:item.psig_loc
            "[%%%%%s] requires a signature val payload" extension_name)
  | _ -> [ item ]

let rewrite_signature
    ~keep_ghost:(keep_ghost [@delator.field string_of_bool])
    ~issuer:(issuer [@delator.field Fun.id]) (signature [@delator.skip]) =
  let names = Hashtbl.create 4 in
  let validated =
    List.map
      (fun source_item ->
        let retained =
          rewrite_signature_item ~keep_ghost:true ~issuer source_item
        in
        List.iter
          (fun item ->
            match item.psig_desc with
            | Psig_value value
              when List.exists issued_interface_attribute
                     value.pval_attributes ->
                if Hashtbl.mem names value.pval_name.txt then (
                  symbolic_rejection ~route:issuer
                    ~stage:"signature-validation"
                    ~payload_kind:"value-declaration"
                    ~reason_class:"duplicate-declaration";
                  Location.raise_errorf ~loc:value.pval_loc
                    "duplicate [%%%%%s] declaration for %s" extension_name
                    value.pval_name.txt);
                Hashtbl.add names value.pval_name.txt ()
            | _ -> ())
          retained;
        (source_item, retained))
      signature.psg_items
  in
  let psg_items =
    List.concat_map
      (fun (source_item, retained) ->
        if keep_ghost then retained
        else rewrite_signature_item ~keep_ghost:false ~issuer source_item)
      validated
  in
  [%log.debug "rewrote symbolic interface signature"
    ~route:(Delator.Field.string issuer)
    ~stage:(Delator.Field.string "signature-admission")
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length signature.psg_items))
    ~output_items:(Delator.Field.int (List.length psg_items))
    ~declaration_count:(Delator.Field.int (Hashtbl.length names))
    ~decision:(Delator.Field.string "completed")];
  { signature with psg_items }
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]
