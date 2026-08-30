open Ast_helper
open Asttypes
open Parsetree

let activate_name = "verocaml.activate"
let group_name = "verocaml.broadcast_group"
let declaration_name = "verocaml.broadcast"
let legacy_lemma_name = "verocaml.broadcast_lemma"
let legacy_axiom_name = "verocaml.broadcast_axiom"
let carrier_attribute_name = "verocaml.internal.broadcast.carrier.v1"
let declaration_attribute_name = "verocaml.internal.broadcast.declaration.v1"
let scope_attribute_name = "verocaml.internal.broadcast.scope.v1"
let interface_declaration_attribute_name =
  "verocaml.internal.broadcast.interface_declaration.v1"
let interface_group_attribute_name =
  "verocaml.internal.broadcast.interface_group.v1"
let retained_family_attribute_name =
  "verocaml.internal.artifact_family.retained-v1"

type target = { path : Longident.t Location.loc }

type group = {
  name : string;
  targets : target list;
  loc : Location.t;
  id : string;
}

let ghost loc = { loc with Location.loc_ghost = true }

let offsets loc =
  (loc.Location.loc_start.Lexing.pos_cnum, loc.Location.loc_end.pos_cnum)

let stable_id kind loc suffix =
  let start_, stop = offsets loc in
  let material =
    Printf.sprintf "broadcast-v1|%s|%d|%d|%s" kind start_ stop suffix
  in
  kind ^ "." ^ Digest.to_hex (Digest.string material)

let validate_empty attribute =
  match attribute.attr_payload with
  | PStr [] -> ()
  | _ ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "[@@%s] does not accept a payload" attribute.attr_name.txt

let payload_expression ~name ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (expression, []); _ } ] -> expression
  | _ ->
      Location.raise_errorf ~loc "%s expects exactly one expression payload"
        name

let target_of_expression expression =
  match expression.pexp_desc with
  | Pexp_ident path ->
      let raw = String.concat "." (Longident.flatten path.txt) in
      if String.equal raw "" then
        Location.raise_errorf ~loc:path.loc "broadcast target path is empty";
      { path }
  | _ ->
      Location.raise_errorf ~loc:expression.pexp_loc
        "broadcast targets must be literal identifier paths"

let target_list ~loc expression =
  let rec loop collected expression =
    match expression.pexp_desc with
    | Pexp_construct ({ txt = Longident.Lident "[]"; _ }, None) ->
        List.rev collected
    | Pexp_construct
        ( { txt = Longident.Lident "::"; _ },
          Some { pexp_desc = Pexp_tuple [ (None, head); (None, tail) ]; _ } ) ->
        loop (target_of_expression head :: collected) tail
    | _ ->
        Location.raise_errorf ~loc
          "broadcast target payload must be a literal identifier list"
  in
  match loop [] expression with
  | [] -> Location.raise_errorf ~loc "broadcast target list must be nonempty"
  | targets -> targets

let structure_targets attribute =
  payload_expression
    ~name:("[@@@" ^ activate_name ^ "]")
    ~loc:attribute.attr_loc attribute.attr_payload
  |> target_list ~loc:attribute.attr_loc

let group_of_attribute loc attribute =
  let expression =
    payload_expression
      ~name:("[@@@" ^ group_name ^ "]")
      ~loc:attribute.attr_loc attribute.attr_payload
  in
  match expression.pexp_desc with
  | Pexp_tuple
      [
        (None, { pexp_desc = Pexp_ident { txt = Longident.Lident name; _ }; _ });
        (None, targets);
      ] ->
      {
        name;
        targets = target_list ~loc:attribute.attr_loc targets;
        loc;
        id = stable_id "group" loc name;
      }
  | _ ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "broadcast_group expects (unqualified_identifier, [literal; targets])"

let string_payload ~loc value =
  PStr
    [
      Str.eval ~loc:(ghost loc)
        (Exp.constant ~loc:(ghost loc) (Pconst_string (value, ghost loc, None)));
    ]

let internal_attribute ~name ~payload ~loc =
  let loc = ghost loc in
  {
    attr_name = { txt = name; loc };
    attr_payload = string_payload ~loc payload;
    attr_loc = loc;
  }

let retained_family_attribute loc =
  let loc = ghost loc in
  {
    attr_name = { txt = retained_family_attribute_name; loc };
    attr_payload = PStr [];
    attr_loc = loc;
  }

let carrier_attribute ~kind ~id ?(source_name = "") loc =
  let start_, stop = offsets loc in
  internal_attribute ~name:carrier_attribute_name ~loc
    ~payload:(Printf.sprintf "%s|%s|%s|%d|%d" kind id source_name start_ stop)

let declaration_attribute ~id loc =
  (* The declaration payload is deliberately role-neutral. *)
  let start_, stop = offsets loc in
  internal_attribute ~name:declaration_attribute_name ~loc
    ~payload:(Printf.sprintf "%s|%d|%d" id start_ stop)

let scope_attribute ~id loc =
  let start_, stop = offsets loc in
  internal_attribute ~name:scope_attribute_name ~loc
    ~payload:(Printf.sprintf "%s|%d|%d" id start_ stop)

let interface_declaration_attribute loc =
  internal_attribute ~name:interface_declaration_attribute_name ~loc
    ~payload:"v1"

let target_name target =
  String.concat "." (Longident.flatten target.path.txt)

let interface_group_attribute group =
  internal_attribute ~name:interface_group_attribute_name ~loc:group.loc
    ~payload:("v1|" ^ String.concat ";" (List.map target_name group.targets))

let marker ~loc text =
  let loc = ghost loc in
  Exp.apply ~loc
    (Exp.ident ~loc
       { txt = Longident.Ldot (Longident.Lident "Vero_ghost", "marker"); loc })
    [ (Nolabel, Exp.constant ~loc (Pconst_string (text, loc, None))) ]

let unit ~loc =
  let loc = ghost loc in
  Exp.construct ~loc { txt = Longident.Lident "()"; loc } None

let target_use target tail =
  let loc = ghost target.path.loc in
  let binding =
    Vb.mk ~loc (Pat.any ~loc ()) (Exp.ident ~loc { target.path with loc })
  in
  Exp.let_ ~loc Immutable Nonrecursive [ binding ] tail

let carrier_expression ~kind ~id ~loc targets =
  let loc = ghost loc in
  let body =
    List.fold_right target_use targets (unit ~loc) |> fun uses ->
    Exp.sequence ~loc
      (marker ~loc
         (Printf.sprintf "verocaml:broadcast:carrier:v1:%s:%s" kind id))
      uses
  in
  let parameter =
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
  Exp.function_ ~loc [ parameter ] constraint_ (Pfunction_body body)

let activation_binding ~kind ~id ~loc targets =
  let loc = ghost loc in
  Vb.mk ~loc
    ~attrs:
      [ carrier_attribute ~kind ~id loc; retained_family_attribute loc ]
    (Pat.any ~loc ())
    (carrier_expression ~kind ~id ~loc targets)

let group_binding group =
  let loc = ghost group.loc in
  Vb.mk ~loc
    ~attrs:
      [
        carrier_attribute ~kind:"group" ~id:group.id ~source_name:group.name
          group.loc;
        retained_family_attribute group.loc;
      ]
    (Pat.var ~loc { txt = group.name; loc })
    (carrier_expression ~kind:"group" ~id:group.id ~loc:group.loc group.targets)

let public_declaration_attributes attributes =
  List.filter
    (fun attribute ->
      List.exists (String.equal attribute.attr_name.txt)
        [ declaration_name; legacy_lemma_name; legacy_axiom_name ])
    attributes

let legacy_declaration_attribute attribute =
  String.equal attribute.attr_name.txt legacy_lemma_name
  || String.equal attribute.attr_name.txt legacy_axiom_name

let rewrite_declaration ~keep_ghost binding =
  match public_declaration_attributes binding.pvb_attributes with
  | [] -> binding
  | attribute :: _ when legacy_declaration_attribute attribute ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "legacy [@@%s] syntax is invalid; use [@@%s]"
        attribute.attr_name.txt declaration_name
  | [ attribute ] ->
      validate_empty attribute;
      let id = stable_id "declaration" binding.pvb_loc "" in
      let remaining =
        List.filter
          (fun candidate ->
            not
              (String.equal candidate.attr_name.txt declaration_name))
          binding.pvb_attributes
      in
      {
        binding with
        pvb_attributes =
          (if keep_ghost then
             declaration_attribute ~id attribute.attr_loc :: remaining
           else remaining);
      }
  | first :: _ ->
      Location.raise_errorf ~loc:first.attr_loc
        "a declaration accepts exactly one [@@%s] marker" declaration_name

let reject_misplaced_attribute attribute =
  if
    String.equal attribute.attr_name.txt declaration_name
    || legacy_declaration_attribute attribute
  then
    Location.raise_errorf ~loc:attribute.attr_loc
      "[@@%s] is only valid on one top-level function binding"
      attribute.attr_name.txt

let collect_groups structure =
  let groups =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_attribute attribute
          when String.equal attribute.attr_name.txt group_name ->
            Some (group_of_attribute item.pstr_loc attribute)
        | _ -> None)
      structure
  in
  let sorted =
    List.sort (fun left right -> String.compare left.name right.name) groups
  in
  let rec duplicate = function
    | left :: (right :: _ as rest) ->
        if String.equal left.name right.name then Some left.name
        else duplicate rest
    | [] | [ _ ] -> None
  in
  (match duplicate sorted with
  | None -> ()
  | Some name ->
      Location.raise_errorf "duplicate broadcast group %S in one structure" name);
  groups

let rewrite_structure ~keep_ghost structure =
  let groups = collect_groups structure in
  let emitted_groups = ref false in
  let group_item loc =
    emitted_groups := true;
    if not keep_ghost then []
    else
      [ Str.value ~loc:(ghost loc) Recursive (List.map group_binding groups) ]
  in
  List.concat_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_attribute attribute
        when String.equal attribute.attr_name.txt group_name ->
          if !emitted_groups then [] else group_item item.pstr_loc
      | Pstr_attribute attribute
        when String.equal attribute.attr_name.txt activate_name ->
          let targets = structure_targets attribute in
          if not keep_ghost then []
          else
            let id = stable_id "structure" item.pstr_loc "" in
            [
              Str.value ~loc:(ghost item.pstr_loc) Nonrecursive
                [
                  activation_binding ~kind:"structure" ~id ~loc:item.pstr_loc
                    targets;
                ];
            ]
      | Pstr_value (flag, bindings) ->
          [
            {
              item with
              pstr_desc =
                Pstr_value
                  (flag, List.map (rewrite_declaration ~keep_ghost) bindings);
            };
          ]
      | _ -> [ item ])
    structure

let rewrite_signature_declaration ~keep_ghost value =
  match public_declaration_attributes value.pval_attributes with
  | [] -> value
  | attribute :: _ when legacy_declaration_attribute attribute ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "legacy [@@%s] syntax is invalid; use [@@%s]"
        attribute.attr_name.txt declaration_name
  | [ attribute ] ->
      validate_empty attribute;
      [%log.trace "rewrite broadcast interface declaration"
        ~name:(Delator.Field.string value.pval_name.txt)
        ~keep_ghost:(Delator.Field.bool keep_ghost)];
      let remaining =
        List.filter
          (fun candidate ->
            not (String.equal candidate.attr_name.txt declaration_name))
          value.pval_attributes
      in
      {
        value with
        pval_attributes =
          (if keep_ghost then
             interface_declaration_attribute attribute.attr_loc :: remaining
           else remaining);
      }
  | first :: _ ->
      Location.raise_errorf ~loc:first.attr_loc
        "a declaration accepts exactly one [@@%s] marker" declaration_name

let group_signature_item group =
  let loc = ghost group.loc in
  let unit_type = Typ.constr ~loc { txt = Longident.Lident "unit"; loc } [] in
  Sig.value ~loc
    (Val.mk ~loc ~attrs:[ interface_group_attribute group ]
       { txt = group.name; loc }
       (Typ.arrow ~loc Nolabel unit_type unit_type [] []))

let rewrite_signature_level ~keep_ghost signature =
  let groups =
    List.filter_map
      (fun item ->
        match item.psig_desc with
        | Psig_attribute attribute
          when String.equal attribute.attr_name.txt group_name ->
            Some (group_of_attribute item.psig_loc attribute)
        | _ -> None)
      signature.psg_items
  in
  let sorted =
    List.sort (fun left right -> String.compare left.name right.name) groups
  in
  let rec reject_duplicate = function
    | left :: (right :: _ as rest) ->
        if String.equal left.name right.name then
          Location.raise_errorf ~loc:left.loc
            "duplicate broadcast group %S in one signature" left.name
        else reject_duplicate rest
    | [] | [ _ ] -> ()
  in
  reject_duplicate sorted;
  List.iter
    (fun _group ->
      [%log.trace "rewrite broadcast interface group"
        ~group:(Delator.Field.string _group.name)
        ~targets:(Delator.Field.int (List.length _group.targets))
        ~keep_ghost:(Delator.Field.bool keep_ghost)])
    groups;
  let emitted_groups = ref false in
  let group_items () =
    emitted_groups := true;
    if keep_ghost then List.map group_signature_item groups else []
  in
  let rewritten =
    {
      signature with
      psg_items =
        List.concat_map
          (fun item ->
            match item.psig_desc with
            | Psig_attribute attribute
              when String.equal attribute.attr_name.txt group_name ->
                if !emitted_groups then [] else group_items ()
            | Psig_value value ->
                [
                  {
                    item with
                    psig_desc =
                      Psig_value
                        (rewrite_signature_declaration ~keep_ghost value);
                  };
                ]
            | _ -> [ item ])
          signature.psg_items;
    }
  in
  [%log.debug "rewrote broadcast interface signature"
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length signature.psg_items))
    ~output_items:(Delator.Field.int (List.length rewritten.psg_items))
    ~groups:(Delator.Field.int (List.length groups))];
  rewritten

let rewrite_signature ~keep_ghost signature =
  let default = Ast_mapper.default_mapper in
  let mapper =
    {
      default with
      signature =
        (fun self signature ->
          rewrite_signature_level ~keep_ghost signature
          |> default.signature self);
    }
  in
  mapper.signature mapper signature

let activation_payload expression payload =
  let payload =
    payload_expression ~name:"[%verocaml.activate]" ~loc:expression.pexp_loc
      payload
  in
  match payload.pexp_desc with
  | Pexp_apply (targets, [ (Nolabel, body) ]) ->
      (target_list ~loc:targets.pexp_loc targets, body)
  | _ ->
      Location.raise_errorf ~loc:expression.pexp_loc
        "%%verocaml.activate expects [literal; targets] followed by one body"

let rewrite_expression ~keep_ghost ~map expression =
  match expression.pexp_desc with
  | Pexp_extension ({ txt; _ }, payload) when String.equal txt activate_name ->
      let targets, body = activation_payload expression payload in
      let body = map body in
      if not keep_ghost then Some body
      else
        let id = stable_id "expression" expression.pexp_loc "" in
        let loc = ghost expression.pexp_loc in
        let binding =
          activation_binding ~kind:"expression" ~id ~loc:expression.pexp_loc
            targets
        in
        let body =
          {
            body with
            pexp_attributes =
              scope_attribute ~id expression.pexp_loc :: body.pexp_attributes;
          }
        in
        Some (Exp.let_ ~loc Immutable Nonrecursive [ binding ] body)
  | _ -> None
