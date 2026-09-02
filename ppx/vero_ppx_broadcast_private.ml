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

let ppx_rejection ~route:_route ~member_kind:_member_kind
    ~reason_class:_reason_class =
  [%log.debug "rejected retained broadcast syntax"
    ~route:(Delator.Field.string _route)
    ~stage:(Delator.Field.string "signature-admission")
    ~member_kind:(Delator.Field.string _member_kind)
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string _reason_class)]

let signature_rejection ~member_kind ~reason_class =
  ppx_rejection ~route:"signature" ~member_kind ~reason_class

let signature_admission ~member_kind:_member_kind ~keep_ghost:_keep_ghost
    ~target_count:_target_count =
  [%log.trace "admitted retained broadcast signature item"
    ~route:(Delator.Field.string "signature")
    ~stage:(Delator.Field.string "signature-admission")
    ~member_kind:(Delator.Field.string _member_kind)
    ~target_count:(Delator.Field.int _target_count)
    ~keep_ghost:(Delator.Field.bool _keep_ghost)
    ~decision:(Delator.Field.string "accepted")]

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
      ppx_rejection ~route:"syntax" ~member_kind:"marker"
        ~reason_class:"nonempty-payload";
      Location.raise_errorf ~loc:attribute.attr_loc
        "[@@%s] does not accept a payload" attribute.attr_name.txt

let payload_expression ~name ~loc = function
  | PStr [ { pstr_desc = Pstr_eval (expression, []); _ } ] -> expression
  | _ ->
      ppx_rejection ~route:"syntax" ~member_kind:"payload"
        ~reason_class:"expression-arity";
      Location.raise_errorf ~loc "%s expects exactly one expression payload"
        name

let target_of_expression expression =
  match expression.pexp_desc with
  | Pexp_ident path ->
      let raw = String.concat "." (Longident.flatten path.txt) in
      if String.equal raw "" then (
        ppx_rejection ~route:"syntax" ~member_kind:"target"
          ~reason_class:"empty-path";
        Location.raise_errorf ~loc:path.loc "broadcast target path is empty";
      );
      { path }
  | _ ->
      ppx_rejection ~route:"syntax" ~member_kind:"target"
        ~reason_class:"non-path-target";
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
        ppx_rejection ~route:"syntax" ~member_kind:"target-set"
          ~reason_class:"nonliteral-list";
        Location.raise_errorf ~loc
          "broadcast target payload must be a literal identifier list"
  in
  match loop [] expression with
  | [] ->
      ppx_rejection ~route:"syntax" ~member_kind:"target-set"
        ~reason_class:"empty-target-set";
      Location.raise_errorf ~loc "broadcast target list must be nonempty"
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
      ppx_rejection ~route:"syntax" ~member_kind:"group"
        ~reason_class:"malformed-group";
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

let framed values =
  values
  |> List.map (fun value -> Printf.sprintf "%d:%s" (String.length value) value)
  |> String.concat ""

let target_path target =
  String.concat "." (Longident.flatten target.path.txt)

let interface_group_attribute group =
  let members =
    group.targets |> List.map target_path |> List.sort String.compare
  in
  internal_attribute ~name:interface_group_attribute_name ~loc:group.loc
    ~payload:("v4|" ^ framed (group.name :: members))

let interface_member_attribute_name =
  "verocaml.internal.broadcast.interface_member.v1"

let interface_member_attribute group ordinal target =
  internal_attribute ~name:interface_member_attribute_name ~loc:target.path.loc
    ~payload:("v1|" ^ framed [ group.name; string_of_int ordinal ])

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

let activation_binding ?family_attribute ~kind ~id ~loc targets =
  let loc = ghost loc in
  let attrs =
    carrier_attribute ~kind ~id loc
    :: Option.to_list (Option.map (fun issue -> issue loc) family_attribute)
  in
  Vb.mk ~loc
    ~attrs
    (Pat.any ~loc ())
    (carrier_expression ~kind ~id ~loc targets)

let group_binding ~family_attribute group =
  let loc = ghost group.loc in
  Vb.mk ~loc
    ~attrs:
      [
        carrier_attribute ~kind:"group" ~id:group.id ~source_name:group.name
          group.loc;
        family_attribute group.loc;
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
      ppx_rejection ~route:"structure" ~member_kind:"declaration"
        ~reason_class:"legacy-marker";
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
      ppx_rejection ~route:"structure" ~member_kind:"declaration"
        ~reason_class:"duplicate-marker";
      Location.raise_errorf ~loc:first.attr_loc
        "a declaration accepts exactly one [@@%s] marker" declaration_name

let reject_misplaced_attribute attribute =
  if
    String.equal attribute.attr_name.txt declaration_name
    || legacy_declaration_attribute attribute
  then
    (ppx_rejection ~route:"structure" ~member_kind:"declaration"
       ~reason_class:"misplaced-marker";
    Location.raise_errorf ~loc:attribute.attr_loc
      "[@@%s] is only valid on one top-level function binding"
      attribute.attr_name.txt)

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
      ppx_rejection ~route:"structure" ~member_kind:"group"
        ~reason_class:"duplicate-group";
      Location.raise_errorf "duplicate broadcast group %S in one structure" name);
  groups

let pattern_names pattern =
  let names = ref [] in
  let default = Ast_iterator.default_iterator in
  let iterator =
    {
      default with
      pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
          | Ppat_var name | Ppat_alias (_, name) -> names := name.txt :: !names
          | _ -> ());
          default.pat self pattern);
    }
  in
  iterator.pat iterator pattern;
  !names

let structure_value_names structure =
  List.concat_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_value (_, bindings) ->
          List.concat_map (fun binding -> pattern_names binding.pvb_pat) bindings
      | Pstr_primitive declaration -> [ declaration.pval_name.txt ]
      | _ -> [])
    structure

let rewrite_structure
    ~keep_ghost:(keep_ghost [@delator.field string_of_bool])
    ~family_attribute:(family_attribute [@delator.skip])
    (structure [@delator.skip]) =
  let groups = collect_groups structure in
  let value_names = structure_value_names structure in
  (match List.find_opt (fun group -> List.mem group.name value_names) groups with
  | None -> ()
  | Some group ->
      ppx_rejection ~route:"structure" ~member_kind:"group"
        ~reason_class:"namespace-collision";
      Location.raise_errorf ~loc:group.loc
        "broadcast group %S collides with an existing implementation value"
        group.name);
  let emitted_groups = ref false in
  let group_item loc =
    emitted_groups := true;
    if not keep_ghost then []
    else
      [
        Str.value ~loc:(ghost loc) Recursive
          (List.map (group_binding ~family_attribute) groups);
      ]
  in
  let rewritten =
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
                    activation_binding ~family_attribute ~kind:"structure" ~id
                      ~loc:item.pstr_loc targets;
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
  in
  [%log.debug "rewrote broadcast implementation structure"
    ~route:(Delator.Field.string "structure")
    ~stage:(Delator.Field.string "structure-admission")
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length structure))
    ~output_items:(Delator.Field.int (List.length rewritten))
    ~group_count:(Delator.Field.int (List.length groups))
    ~decision:(Delator.Field.string "completed")];
  rewritten
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let rewrite_signature_declaration ~keep_ghost value =
  match public_declaration_attributes value.pval_attributes with
  | [] -> value
  | attribute :: _ when legacy_declaration_attribute attribute ->
      signature_rejection ~member_kind:"declaration"
        ~reason_class:"legacy-marker";
      Location.raise_errorf ~loc:attribute.attr_loc
        "legacy [@@%s] syntax is invalid; use [@@%s]"
        attribute.attr_name.txt declaration_name
  | [ attribute ] ->
      (match attribute.attr_payload with
      | PStr [] -> ()
      | _ ->
          signature_rejection ~member_kind:"declaration"
            ~reason_class:"nonempty-marker-payload";
          validate_empty attribute);
      signature_admission ~member_kind:"declaration" ~keep_ghost
        ~target_count:0;
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
      signature_rejection ~member_kind:"declaration"
        ~reason_class:"duplicate-marker";
      Location.raise_errorf ~loc:first.attr_loc
        "a declaration accepts exactly one [@@%s] marker" declaration_name

let group_signature_item group =
  let loc = ghost group.loc in
  let unit_type = Typ.constr ~loc { txt = Longident.Lident "unit"; loc } [] in
  Sig.value ~loc
    (Val.mk ~loc ~attrs:[ interface_group_attribute group ]
       { txt = group.name; loc }
       (Typ.arrow ~loc Nolabel unit_type unit_type [] []))

let group_member_signature_item group ordinal target =
  let loc = ghost target.path.loc in
  let binding =
    Vb.mk ~loc (Pat.any ~loc ())
      (Exp.ident ~loc { target.path with loc })
  in
  let witness = Mod.structure ~loc [ Str.value ~loc Nonrecursive [ binding ] ] in
  Sig.include_ ~loc
    (Incl.mk ~loc
       ~attrs:[ interface_member_attribute group ordinal target ]
       (Mty.typeof_ ~loc witness))

let group_signature_items group =
  List.mapi (group_member_signature_item group) group.targets
  @ [ group_signature_item group ]

let rewrite_signature_level ~keep_ghost signature =
  let signature_group_of_attribute loc attribute =
    try group_of_attribute loc attribute
    with Location.Error _ as exn ->
      signature_rejection ~member_kind:"group"
        ~reason_class:"malformed-group-marker";
      raise exn
  in
  let raw_groups =
    List.filter_map (fun item ->
        match item.psig_desc with
        | Psig_attribute attribute
          when String.equal attribute.attr_name.txt group_name ->
            Some (signature_group_of_attribute item.psig_loc attribute)
        | _ -> None)
      signature.psg_items
  in
  let sorted =
    List.sort (fun left right -> String.compare left.name right.name) raw_groups
  in
  let rec reject_duplicate = function
    | left :: (right :: _ as rest) ->
        if String.equal left.name right.name then
          (signature_rejection ~member_kind:"group"
             ~reason_class:"duplicate-group";
          Location.raise_errorf ~loc:left.loc
            "duplicate broadcast group %S in one signature" left.name)
        else reject_duplicate rest
    | [] | [ _ ] -> ()
  in
  reject_duplicate sorted;
  let declared_names =
    signature.psg_items
    |> List.filter_map (fun item ->
           match item.psig_desc with
           | Psig_value value -> Some value.pval_name.txt
           | Psig_module declaration -> declaration.pmd_name.txt
           | _ -> None)
  in
  (match
     List.find_opt (fun group -> List.mem group.name declared_names) raw_groups
   with
  | None -> ()
  | Some group ->
      signature_rejection ~member_kind:"group"
        ~reason_class:"namespace-collision";
      Location.raise_errorf ~loc:group.loc
        "broadcast group %S collides with an existing signature name" group.name);
  List.iter
    (fun group ->
      signature_admission ~member_kind:"group" ~keep_ghost
        ~target_count:(List.length group.targets))
    raw_groups;
  let rewritten =
    {
      signature with
      psg_items =
        List.concat_map
          (fun item ->
            match item.psig_desc with
            | Psig_attribute attribute
              when String.equal attribute.attr_name.txt group_name ->
                if keep_ghost then
                  signature_group_of_attribute item.psig_loc attribute
                  |> group_signature_items
                else []
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
    ~route:(Delator.Field.string "signature")
    ~stage:(Delator.Field.string "signature-admission")
    ~decision:(Delator.Field.string "completed")
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length signature.psg_items))
    ~output_items:(Delator.Field.int (List.length rewritten.psg_items))
    ~groups:(Delator.Field.int (List.length raw_groups))];
  rewritten

let rewrite_signature
    ~keep_ghost:(keep_ghost [@delator.field string_of_bool])
    (signature [@delator.skip]) =
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
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let activation_payload expression payload =
  let payload =
    payload_expression ~name:"[%verocaml.activate]" ~loc:expression.pexp_loc
      payload
  in
  match payload.pexp_desc with
  | Pexp_apply (targets, [ (Nolabel, body) ]) ->
      (target_list ~loc:targets.pexp_loc targets, body)
  | _ ->
      ppx_rejection ~route:"expression" ~member_kind:"activation"
        ~reason_class:"application-shape";
      Location.raise_errorf ~loc:expression.pexp_loc
        "%%verocaml.activate expects [literal; targets] followed by one body"

let rewrite_expression ~keep_ghost ~map expression =
  match expression.pexp_desc with
  | Pexp_extension ({ txt; _ }, payload) when String.equal txt activate_name ->
      let targets, body = activation_payload expression payload in
      let body = map body in
      [%log.trace "admitted retained broadcast expression activation"
        ~route:(Delator.Field.string "expression")
        ~stage:(Delator.Field.string "activation-admission")
        ~member_kind:(Delator.Field.string "activation")
        ~target_count:(Delator.Field.int (List.length targets))
        ~keep_ghost:(Delator.Field.bool keep_ghost)
        ~decision:(Delator.Field.string "accepted")];
      if not keep_ghost then Some body
      else
        let id = stable_id "expression" expression.pexp_loc "" in
        let loc = ghost expression.pexp_loc in
        let binding =
          activation_binding ~kind:"expression" ~id
            ~loc:expression.pexp_loc targets
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
