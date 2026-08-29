open Ast_helper
open Asttypes
open Parsetree

type contract = Requires | Ensures | Decreases | Assert

let spec_attribute = "verocaml.spec"
let proof_attribute = "verocaml.proof"
let axiom_attribute = "verocaml.axiom"
let type_invariant_attribute = "verocaml.type_invariant"
let external_specification_attribute = "verocaml.external_specification"
let external_type_specification_attribute =
  "verocaml.external_type_specification"
let external_body_attribute = "verocaml.external_body"
let opaque_attribute = "verocaml.opaque"
let revealed_attribute = "verocaml.revealed"
let ghost_attribute = "ghost"
let tracked_attribute = "tracked"
let finite_attribute = "finite"
let verification_scope_attribute = "verocaml.verify"
let internal_prefix = "verocaml.internal."
let internal_mode_prefix = internal_prefix ^ "instance_mode."
let internal_finite_formal_prefix = internal_prefix ^ "finite_formal."
let family_prefix = internal_prefix ^ "artifact_family."
let verification_scope_prefix = internal_prefix ^ "verification_scope."
let explicit_compiler_mode_marker = internal_prefix ^ "compiler_mode_syntax"
let external_type_specification_marker =
  internal_prefix ^ "external_type_specification.v1"

type declaration_role = {
  attribute_name : string;
  role_name : string;
  carrier_name : string;
  preserve_ordinary_body : bool;
}

type instance_mode = Tracked_mode | Ghost_mode

type instance_site =
  | Binding_site
  | Expression_site
  | Pattern_site
  | Formal_site
  | Result_site
  | Field_site
  | Component_type_site

type instance_stage =
  | Exec_stage
  | Spec_stage
  | Proof_stage
  | Recursive_proof_stage

let instance_mode_name = function
  | Tracked_mode -> "tracked"
  | Ghost_mode -> "ghost"

let formal_label_name = function
  | Nolabel -> "-"
  | Labelled label -> "label:" ^ label
  | Optional label -> "optional:" ^ label

let instance_site_name = function
  | Binding_site -> "binding"
  | Expression_site -> "expression"
  | Pattern_site -> "pattern"
  | Formal_site -> "formal"
  | Result_site -> "result"
  | Field_site -> "field"
  | Component_type_site -> "component-type"

let public_instance_mode_attribute attribute =
  match attribute.attr_name.txt with
  | name when String.equal name ghost_attribute -> Some Ghost_mode
  | name when String.equal name tracked_attribute -> Some Tracked_mode
  | _ -> None

let validate_empty_attribute attribute =
  match attribute.attr_payload with
  | PStr [] -> ()
  | _ ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "[@%s] does not accept a payload" attribute.attr_name.txt

let split_instance_mode_attributes attributes =
  let modes, remaining =
    List.partition
      (fun attribute ->
        Option.is_some (public_instance_mode_attribute attribute))
      attributes
  in
  List.iter validate_empty_attribute modes;
  match modes with
  | [] -> (None, remaining)
  | [ attribute ] ->
      (public_instance_mode_attribute attribute, remaining)
  | first :: _ ->
      Location.raise_errorf ~loc:first.attr_loc
        "exactly one [@ghost] or [@tracked] annotation is allowed at this site"

let public_finite_attribute attribute =
  String.equal attribute.attr_name.txt finite_attribute

let split_finite_attributes attributes =
  let finite, remaining =
    List.partition public_finite_attribute attributes
  in
  List.iter validate_empty_attribute finite;
  match finite with
  | [] -> (false, remaining)
  | [ _ ] -> (true, remaining)
  | first :: _ ->
      Location.raise_errorf ~loc:first.attr_loc
        "exactly one [@finite] annotation is allowed on a formal"

let internal_attribute ~loc name =
  {
    attr_name = { txt = name; loc = { loc with Location.loc_ghost = true } };
    attr_payload = PStr [];
    attr_loc = { loc with Location.loc_ghost = true };
  }

let internal_mode_attribute ~site ~mode ~loc =
  internal_attribute ~loc
    (internal_mode_prefix ^ instance_site_name site ^ "."
   ^ instance_mode_name mode)

let internal_formal_mode_attribute ~index ~mode ~loc =
  internal_attribute ~loc
    (Printf.sprintf "%s%s.%d.%s" internal_mode_prefix
       (instance_site_name Formal_site) index (instance_mode_name mode))

let internal_finite_formal_attribute ~index ~loc =
  internal_attribute ~loc
    (Printf.sprintf "%s%d" internal_finite_formal_prefix index)

let family_attribute ~keep_ghost ~loc =
  internal_attribute ~loc
    (family_prefix ^ (if keep_ghost then "retained-v1" else "ordinary-v1"))

let verification_scope_marker ~loc =
  internal_attribute ~loc (verification_scope_prefix ^ "marked-v1")

let explicit_compiler_mode_attribute ~loc =
  internal_attribute ~loc explicit_compiler_mode_marker

let retained_external_type_specification_attribute ~loc =
  internal_attribute ~loc external_type_specification_marker

let public_verification_scope attribute =
  String.equal attribute.attr_name.txt verification_scope_attribute

let split_root_verification_scope structure =
  let markers, structure =
    List.partition
      (fun item ->
        match item.pstr_desc with
        | Pstr_attribute attribute -> public_verification_scope attribute
        | _ -> false)
      structure
  in
  List.iter
    (fun item ->
      match item.pstr_desc with
      | Pstr_attribute attribute -> validate_empty_attribute attribute
      | _ -> assert false)
    markers;
  match markers with
  | [] -> structure
  | [ marker ] ->
      let loc = marker.pstr_loc in
      Str.attribute ~loc (verification_scope_marker ~loc) :: structure
  | duplicate :: _ ->
      Location.raise_errorf ~loc:duplicate.pstr_loc
        "exactly one [@@@%s] implementation-unit marker is allowed"
        verification_scope_attribute

let reject_reserved_attribute attribute =
  if
    String.starts_with ~prefix:internal_prefix attribute.attr_name.txt
    && not
         (attribute.attr_loc.loc_ghost
         && attribute.attr_name.loc.loc_ghost)
  then
    Location.raise_errorf ~loc:attribute.attr_loc
      "reserved VeroCaml internal attribute [@%s] is not source syntax"
      attribute.attr_name.txt

let stage_of_role ~recursive = function
  | None -> Exec_stage
  | Some role when String.equal role.attribute_name spec_attribute -> Spec_stage
  | Some role when String.equal role.attribute_name proof_attribute ->
      if recursive then Recursive_proof_stage else Proof_stage
  | Some role when String.equal role.attribute_name type_invariant_attribute ->
      Spec_stage
  | Some role
    when String.equal role.attribute_name external_specification_attribute ->
      Spec_stage
  | Some role when String.equal role.attribute_name external_body_attribute ->
      Exec_stage
  | Some _ -> assert false

let validate_signature_mode ~stage ~loc = function
  | None -> ()
  | Some Ghost_mode -> (
      match stage with
      | Exec_stage | Spec_stage | Proof_stage -> ()
      | Recursive_proof_stage ->
          Location.raise_errorf ~loc
            "recursive proof formals and results retain the scalar/unit slice \
             and reject mode annotations")
  | Some Tracked_mode -> (
      match stage with
      | Exec_stage | Proof_stage -> ()
      | Spec_stage ->
          Location.raise_errorf ~loc
            "specification formals and results cannot be Tracked"
      | Recursive_proof_stage ->
          Location.raise_errorf ~loc
            "recursive proof formals and results retain the scalar/unit slice \
             and reject mode annotations")

let simple_binding_pattern pattern =
  match pattern.ppat_desc with Ppat_var _ -> true | _ -> false

let mode_on_type typ =
  let mode, attributes = split_instance_mode_attributes typ.ptyp_attributes in
  (mode, { typ with ptyp_attributes = attributes })

let signature_on_type typ =
  let mode, attributes = split_instance_mode_attributes typ.ptyp_attributes in
  let finite, attributes = split_finite_attributes attributes in
  (mode, finite, { typ with ptyp_attributes = attributes })

let mode_on_pattern_constraint pattern =
  match pattern.ppat_desc with
  | Ppat_constraint (nested, Some typ, modes) ->
      let mode, typ = mode_on_type typ in
      ( mode,
        {
          pattern with
          ppat_desc = Ppat_constraint (nested, Some typ, modes);
        } )
  | _ -> (None, pattern)

let signature_on_pattern_constraint pattern =
  match pattern.ppat_desc with
  | Ppat_constraint (nested, Some typ, modes) ->
      let mode, finite, typ = signature_on_type typ in
      ( mode,
        finite,
        {
          pattern with
          ppat_desc = Ppat_constraint (nested, Some typ, modes);
        } )
  | _ -> (None, false, pattern)

let rewrite_signature_formal self ~stage typ =
  let mode, finite, typ = signature_on_type typ in
  validate_signature_mode ~stage ~loc:typ.ptyp_loc mode;
  let typ = Ast_mapper.default_mapper.typ self typ in
  (mode, finite, typ)

let rec rewrite_signature_result self ~keep_ghost ~stage typ =
  match typ.ptyp_desc with
  | Ptyp_arrow
      (label, argument, result, argument_modes, arrow_result_modes) ->
      let formal_mode, finite, argument =
        rewrite_signature_formal self ~stage argument
      in
      let result_modes, finite_formals, result =
        rewrite_signature_result self ~keep_ghost ~stage result
      in
      let result =
        if
          (not keep_ghost)
          &&
          match formal_mode with
          | Some (Ghost_mode | Tracked_mode) -> true
          | None -> false
        then result
        else
          {
            typ with
            ptyp_desc =
              Ptyp_arrow
                ( label,
                  argument,
                  result,
                  argument_modes,
                  arrow_result_modes );
          }
      in
      ( formal_mode :: result_modes,
        (formal_label_name label, finite) :: finite_formals,
        result )
  | _ ->
      let mode, finite, typ = signature_on_type typ in
      if finite then
        Location.raise_errorf ~loc:typ.ptyp_loc
          "finite result contracts are not supported";
      validate_signature_mode ~stage ~loc:typ.ptyp_loc mode;
      let typ = Ast_mapper.default_mapper.typ self typ in
      ([ mode ], [], typ)

let mode_signature_payload_attribute ~loc payload =
  {
    attr_name =
      {
        txt = internal_prefix ^ "mode_signature";
        loc = { loc with Location.loc_ghost = true };
      };
    attr_payload =
      PStr
        [
          Str.eval ~loc:{ loc with Location.loc_ghost = true }
            (Exp.constant ~loc:{ loc with Location.loc_ghost = true }
               (Pconst_string
                  (payload, { loc with Location.loc_ghost = true }, None)));
        ];
    attr_loc = { loc with Location.loc_ghost = true };
  }

let mode_signature_attribute ~loc modes =
  let payload =
    modes
    |> List.mapi (fun index mode ->
           Printf.sprintf "%d=%s" index
             (Option.fold ~none:"default" ~some:instance_mode_name mode))
    |> String.concat ";"
  in
  mode_signature_payload_attribute ~loc payload

let finite_signature_payload_attribute ~loc payload =
  {
    attr_name =
      {
        txt = internal_prefix ^ "finite_signature";
        loc = { loc with Location.loc_ghost = true };
      };
    attr_payload =
      PStr
        [
          Str.eval ~loc:{ loc with Location.loc_ghost = true }
            (Exp.constant ~loc:{ loc with Location.loc_ghost = true }
               (Pconst_string
                  (payload, { loc with Location.loc_ghost = true }, None)));
        ];
    attr_loc = { loc with Location.loc_ghost = true };
  }

let finite_signature_attribute ~loc finite_formals =
  let payload =
    "v1|"
    ^ (finite_formals
      |> List.mapi (fun index (label, finite) ->
             Printf.sprintf "%d:%s=%s" index label
               (if finite then "finite" else "default"))
      |> String.concat ";")
  in
  finite_signature_payload_attribute ~loc payload

let without_finite_signature attributes =
  List.filter
    (fun attribute ->
      not
        (String.equal attribute.attr_name.txt
           (internal_prefix ^ "finite_signature")))
    attributes

let retain_or_issue_finite_signature ~loc ~finite_formals attributes =
  match
    List.filter
      (fun attribute ->
        String.equal attribute.attr_name.txt
          (internal_prefix ^ "finite_signature"))
      attributes
  with
  | [] -> finite_signature_attribute ~loc finite_formals
  | [ attribute ]
    when
      attribute.attr_loc.loc_ghost
      && attribute.attr_name.loc.loc_ghost ->
      attribute
  | [ attribute ] ->
      reject_reserved_attribute attribute;
      assert false
  | attribute :: _ ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "duplicate internal finite signature metadata"

let declaration_expression_modes expression =
  match expression.pexp_desc with
  | Pexp_function (parameters, constraint_, _) ->
      let formals =
        List.filter_map
          (fun parameter ->
            match parameter.pparam_desc with
            | Pparam_newtype _ -> None
            | Pparam_val (_, _, pattern) ->
                let mode, _ = mode_on_pattern_constraint pattern in
                Some mode)
          parameters
      in
      let result =
        match constraint_.ret_type_constraint with
        | Some (Pconstraint typ) -> fst (mode_on_type typ)
        | Some (Pcoerce _) | None -> None
      in
      formals @ [ result ]
  | _ -> [ None ]

let declaration_expression_finite_formals expression =
  match expression.pexp_desc with
  | Pexp_function (parameters, _, _) ->
      List.filter_map
        (fun parameter ->
          match parameter.pparam_desc with
          | Pparam_newtype _ -> None
          | Pparam_val (label, _, pattern) ->
              let _, finite, _ = signature_on_pattern_constraint pattern in
              Some (formal_label_name label, finite))
        parameters
  | _ -> []

let callable_has_explicit_compiler_mode_syntax binding =
  let found = ref (binding.pvb_modes <> []) in
  let inspect modes = if modes <> [] then found := true in
  let default = Ast_iterator.default_iterator in
  let iterator =
    {
      default with
      typ =
        (fun self typ ->
          (match typ.ptyp_desc with
          | Ptyp_arrow (_, _, _, parameter_modes, result_modes) ->
              inspect parameter_modes;
              inspect result_modes
          | _ -> ());
          default.typ self typ);
      pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
          | Ppat_constraint (_, _, modes) -> inspect modes
          | _ -> ());
          default.pat self pattern);
    }
  in
  let inspect_type = iterator.typ iterator in
  iterator.pat iterator binding.pvb_pat;
  Option.iter
    (function
      | Pvc_constraint { typ; _ } -> inspect_type typ
      | Pvc_coercion { ground; coercion } ->
          Option.iter inspect_type ground;
          inspect_type coercion)
    binding.pvb_constraint;
  (match binding.pvb_expr.pexp_desc with
  | Pexp_function (parameters, constraint_, _) ->
      inspect constraint_.mode_annotations;
      inspect constraint_.ret_mode_annotations;
      Option.iter
        (function
          | Pconstraint typ -> inspect_type typ
          | Pcoerce (ground, coercion) ->
              Option.iter inspect_type ground;
              inspect_type coercion)
        constraint_.ret_type_constraint;
      List.iter
        (fun parameter ->
          match parameter.pparam_desc with
          | Pparam_val (_, _, pattern) -> iterator.pat iterator pattern
          | Pparam_newtype _ -> ())
        parameters
  | _ -> ());
  !found

let type_mode_signature_attribute declaration =
  let mode_entry name attributes =
    let mode, _ = split_instance_mode_attributes attributes in
    name ^ "="
    ^ Option.fold ~none:"default" ~some:instance_mode_name mode
  in
  let entries =
    match declaration.ptype_kind with
    | Ptype_record labels ->
        List.map
          (fun label ->
            mode_entry ("record:" ^ label.pld_name.txt) label.pld_attributes)
          labels
    | Ptype_variant constructors ->
        List.concat_map
          (fun constructor ->
            match constructor.pcd_args with
            | Pcstr_tuple arguments ->
                List.mapi
                  (fun index argument ->
                    mode_entry
                      (Printf.sprintf "variant:%s:%d"
                         constructor.pcd_name.txt index)
                      argument.pca_type.ptyp_attributes)
                  arguments
            | Pcstr_record labels ->
                List.map
                  (fun label ->
                    mode_entry
                      ("variant:" ^ constructor.pcd_name.txt ^ ":"
                     ^ label.pld_name.txt)
                      label.pld_attributes)
                  labels)
          constructors
    | Ptype_abstract | Ptype_open | Ptype_record_unboxed_product _ -> []
  in
  mode_signature_payload_attribute ~loc:declaration.ptype_loc
    (String.concat ";" entries)

let declaration_roles =
  [
    {
      attribute_name = spec_attribute;
      role_name = "spec";
      carrier_name = "spec_definition";
      preserve_ordinary_body = false;
    };
    {
      attribute_name = proof_attribute;
      role_name = "proof";
      carrier_name = "proof_definition";
      preserve_ordinary_body = false;
    };
    {
      attribute_name = type_invariant_attribute;
      role_name = "type-invariant";
      carrier_name = "type_invariant_definition";
      preserve_ordinary_body = false;
    };
    {
      attribute_name = external_specification_attribute;
      role_name = "external-specification";
      carrier_name = "external_specification";
      preserve_ordinary_body = false;
    };
  ]

let declaration_role name =
  List.find_opt (fun role -> String.equal role.attribute_name name)
    declaration_roles

let external_body_role =
  {
    attribute_name = external_body_attribute;
    role_name = "external-body";
    carrier_name = "external_body";
    preserve_ordinary_body = true;
  }

let declaration_attribute name =
  String.equal name external_body_attribute
  || Option.is_some (declaration_role name)

let contract_of_name = function
  | "verocaml.requires" -> Some Requires
  | "verocaml.ensures" -> Some Ensures
  | "verocaml.decreases" -> Some Decreases
  | "verocaml.assert" -> Some Assert
  | _ -> None

let ghost_name = function
  | Requires -> "requires"
  | Ensures -> "ensures"
  | Decreases -> "decreases"
  | Assert -> "assert_"

let extension expression =
  match expression.pexp_desc with
  | Pexp_extension ({ txt = name; _ }, payload) -> Some (name, payload)
  | _ -> None

let payload_expression ~name ~loc = function
  | PStr
      [
        {
          pstr_desc = Pstr_eval (expression, []);
          pstr_loc = _;
        };
      ] ->
      expression
  | _ ->
      Location.raise_errorf ~loc
        "%%%s expects exactly one expression payload" name

let ghost_identifier ~loc name =
  Exp.ident ~loc
    { txt = Longident.Ldot (Longident.Lident "Vero_ghost", name); loc }

let apply_ghost ~loc ~attrs name argument =
  Exp.apply ~loc ~attrs (ghost_identifier ~loc name) [ (Nolabel, argument) ]

let string_constant ~loc value =
  Exp.constant ~loc (Pconst_string (value, loc, None))

let unit_pattern ~loc =
  Pat.construct ~loc { txt = Longident.Lident "()"; loc } None

let thunk ~loc body =
  let parameter =
    {
      pparam_loc = loc;
      pparam_desc = Pparam_val (Nolabel, None, unit_pattern ~loc);
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

let location_number location =
  ( location.Location.loc_start.Lexing.pos_cnum,
    location.Location.loc_end.Lexing.pos_cnum )

let role_attributes attributes =
  List.filter
    (fun attribute -> Option.is_some (declaration_role attribute.attr_name.txt))
    attributes

let external_body_attributes attributes =
  List.filter
    (fun attribute ->
      String.equal attribute.attr_name.txt external_body_attribute)
    attributes

let declaration_attributes attributes =
  List.filter
    (fun attribute -> declaration_attribute attribute.attr_name.txt)
    attributes

let validate_role_attribute attribute =
  match attribute.attr_payload with
  | PStr [] -> ()
  | _ ->
      Location.raise_errorf ~loc:attribute.attr_loc
        "[@@%s] does not accept a payload" attribute.attr_name.txt

let normalize_axiom_sugar structure =
  let default = Ast_mapper.default_mapper in
  let normalized_attribute attribute name =
    { attribute with attr_name = { attribute.attr_name with txt = name } }
  in
  let normalize_binding binding =
    let axioms =
      List.filter
        (fun attribute -> String.equal attribute.attr_name.txt axiom_attribute)
        binding.pvb_attributes
    in
    List.iter validate_role_attribute axioms;
    match axioms with
    | [] -> binding
    | [ axiom ] ->
        let attributes =
          List.concat_map
            (fun attribute ->
              if String.equal attribute.attr_name.txt axiom_attribute then
                [
                  normalized_attribute axiom proof_attribute;
                  normalized_attribute axiom external_body_attribute;
                ]
              else [ attribute ])
            binding.pvb_attributes
        in
        { binding with pvb_attributes = attributes }
    | duplicate :: _ ->
        Location.raise_errorf ~loc:duplicate.attr_loc
          "duplicate [@%s] attribute" axiom_attribute
  in
  let mapper =
    {
      default with
      attribute =
        (fun self attribute ->
          if String.equal attribute.attr_name.txt axiom_attribute then
            Location.raise_errorf ~loc:attribute.attr_loc
              "[@@%s] is only valid on one nonrecursive top-level function \
               binding"
              axiom_attribute;
          default.attribute self attribute);
      structure_item =
        (fun self item ->
          match item.pstr_desc with
          | Pstr_value (flag, bindings) ->
              default.structure_item self
                {
                  item with
                  pstr_desc =
                    Pstr_value (flag, List.map normalize_binding bindings);
                }
          | _ -> default.structure_item self item);
    }
  in
  mapper.structure mapper structure

let reject_misplaced_spec_attribute attribute =
  if String.equal attribute.attr_name.txt proof_attribute then
    Location.raise_errorf ~loc:attribute.attr_loc
      "[@@%s] is only valid on one top-level function binding"
      attribute.attr_name.txt
  else if declaration_attribute attribute.attr_name.txt then
    Location.raise_errorf ~loc:attribute.attr_loc
      "[@@%s] is only valid on one nonrecursive top-level function binding"
      attribute.attr_name.txt
  else if
    String.equal attribute.attr_name.txt opaque_attribute
    || String.equal attribute.attr_name.txt revealed_attribute
  then
    Location.raise_errorf ~loc:attribute.attr_loc
      "[@@%s] is valid only on a recursive [@@%s] binding"
      attribute.attr_name.txt spec_attribute

let binding_name role binding =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var name -> name
  | _ ->
      Location.raise_errorf ~loc:binding.pvb_pat.ppat_loc
        "[@@%s] requires a variable function binding" role.attribute_name

let wrap_declaration_body ~(binding_location : Location.t)
    ~(attribute_location : Location.t) ~attribute_name ~role ~carrier name
    ?visibility expression =
  let carrier_loc = { attribute_location with loc_ghost = true } in
  let carrier_id =
    let binding_start, binding_end = location_number binding_location in
    let attribute_start, attribute_end = location_number attribute_location in
    Printf.sprintf "verocaml:%s:1:%d:%d:%d:%d:%s" role binding_start
      binding_end attribute_start attribute_end name.txt
  in
  let wrap body =
    let arguments =
      [ (Nolabel, string_constant ~loc:carrier_loc carrier_id) ]
      @
      match visibility with
      | None -> [ (Nolabel, thunk ~loc:carrier_loc body) ]
      | Some visibility ->
          [
            (Nolabel, string_constant ~loc:carrier_loc visibility);
            (Nolabel, thunk ~loc:carrier_loc body);
          ]
    in
    Exp.apply ~loc:carrier_loc
      (ghost_identifier ~loc:carrier_loc carrier)
      arguments
  in
  match expression.pexp_desc with
  | Pexp_function (parameters, constraint_, Pfunction_body body) ->
      {
        expression with
        pexp_desc =
          Pexp_function
            (parameters, constraint_, Pfunction_body (wrap body));
      }
  | _ ->
      Location.raise_errorf ~loc:expression.pexp_loc
        "[@@%s] requires a function with an expression body" attribute_name

let unit_expression ~loc =
  Exp.construct ~loc { txt = Longident.Lident "()"; loc } None

let visibility_attributes attributes =
  List.filter
    (fun attribute ->
      String.equal attribute.attr_name.txt opaque_attribute
      || String.equal attribute.attr_name.txt revealed_attribute)
    attributes

let visibility_name attribute =
  if String.equal attribute.attr_name.txt opaque_attribute then "opaque"
  else if String.equal attribute.attr_name.txt revealed_attribute then "revealed"
  else assert false

let reveal_literal ~name expression =
  let positive = function
    | {
        pexp_desc = Pexp_constant (Pconst_integer (digits, None));
        _;
      } ->
        Some digits
    | _ -> None
  in
  match positive expression with
  | Some digits -> digits
  | None -> (
      match expression.pexp_desc with
      | Pexp_apply
          ( {
              pexp_desc =
                Pexp_ident { txt = Longident.Lident "~-"; _ };
              _;
            },
            [ (Nolabel, operand) ] ) -> (
          match positive operand with
          | Some digits -> "-" ^ digits
          | None ->
              Location.raise_errorf ~loc:expression.pexp_loc
                "%%%s depth must be an unsuffixed integer literal" name)
      | _ ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "%%%s depth must be an unsuffixed integer literal" name)

let reveal_application ~name ~loc target literal_depth =
  let carrier_loc = { loc with Location.loc_ghost = true } in
  let start_, end_ = location_number loc in
  let carrier_id =
    Printf.sprintf "verocaml:%s:1:%d:%d" name start_ end_
  in
  let arguments =
    [ (Nolabel, string_constant ~loc:carrier_loc carrier_id) ]
    @
    match literal_depth with
    | None -> [ (Nolabel, target) ]
    | Some depth ->
        [
          (Nolabel, string_constant ~loc:carrier_loc depth);
          (Nolabel, target);
        ]
  in
  Exp.apply ~loc:carrier_loc
    (ghost_identifier ~loc:carrier_loc name)
    arguments

let use_type_invariant_application ~loc value =
  let carrier_loc = { loc with Location.loc_ghost = true } in
  let start_, end_ = location_number loc in
  let carrier_id =
    Printf.sprintf "verocaml:use-type-invariant:1:%d:%d" start_ end_
  in
  Exp.apply ~loc:carrier_loc
    (ghost_identifier ~loc:carrier_loc "use_type_invariant")
    [
      (Nolabel, string_constant ~loc:carrier_loc carrier_id);
      (Nolabel, value);
    ]

let validate_external_body_contracts ~attribute_name expression =
    let rec prefix contracts expression =
      match expression.pexp_desc with
      | Pexp_sequence (head, tail) -> (
          match extension head with
          | Some (name, _) -> (
              match contract_of_name name with
              | Some contract -> prefix (contract :: contracts) tail
              | None -> List.rev contracts)
          | None -> List.rev contracts)
      | _ -> List.rev contracts
    in
    let contracts =
      match expression.pexp_desc with
      | Pexp_function (_, _, Pfunction_body body) -> prefix [] body
      | _ -> []
    in
    if List.exists (function Decreases | Assert -> true | _ -> false) contracts
    then
      Location.raise_errorf ~loc:expression.pexp_loc
        "[@@%s] allows only requires and ensures clauses" attribute_name
    else if not (List.mem Ensures contracts) then
      Location.raise_errorf ~loc:expression.pexp_loc
        "[@@%s] requires at least one ensures clause" attribute_name

let pattern_value_bindings =
  Vero_ppx_logical_builtin_private.pattern_value_bindings
let visible_parameter_bindings =
  Vero_ppx_logical_builtin_private.visible_parameter_bindings
let aliased_shadow = Vero_ppx_logical_builtin_private.aliased_shadow
let mentioned_identifiers =
  Vero_ppx_logical_builtin_private.mentioned_identifiers

type proof_source_binding = {
  proof_binding_name : string Location.loc;
  proof_binding_span : Location.t;
}

type proof_callable = {
  proof_callable_name : string;
  proof_callable_binding_span : Location.t;
  proof_callable_body_span : Location.t;
}

let add_proof_binding visible binding =
  List.filter
    (fun existing ->
      not
        (String.equal existing.proof_binding_name.txt
           binding.proof_binding_name.txt))
    visible
  @ [ binding ]

let proof_bindings_of_pattern pattern =
  pattern_value_bindings pattern
  |> List.map (fun name ->
         {
           proof_binding_name = name;
           proof_binding_span = name.loc;
         })

let add_proof_pattern visible pattern =
  List.fold_left add_proof_binding visible (proof_bindings_of_pattern pattern)

let hex_string value =
  let buffer = Buffer.create (String.length value * 2) in
  String.iter
    (fun character ->
      Buffer.add_string buffer (Printf.sprintf "%02x" (Char.code character)))
    value;
  Buffer.contents buffer

type proof_capture_kind =
  | Proof_region_capture
  | Local_assert_capture of int

let proof_capture_manifest callable region predicate_location captures kind =
  let offset_pair location =
    let start_, end_ = location_number location in
    Printf.sprintf "%d,%d" start_ end_
  in
  let slots =
    captures
    |> List.map (fun binding ->
           Printf.sprintf "%s,%s"
             (hex_string binding.proof_binding_name.txt)
             (offset_pair binding.proof_binding_span))
    |> String.concat ";"
  in
  let base =
    Printf.sprintf
      "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=%s|binding=%s|body=%s|region=%s|slots=%s"
      (hex_string callable.proof_callable_name)
      (offset_pair callable.proof_callable_binding_span)
      (offset_pair callable.proof_callable_body_span)
      (offset_pair region) slots
  in
  match kind with
  | Proof_region_capture -> base
  | Local_assert_capture ordinal ->
      Printf.sprintf "%s|kind=local-assert|ordinal=%d|predicate=%s" base
        ordinal (offset_pair predicate_location)

let proof_region_id region =
  let start_, end_ = location_number region in
  Printf.sprintf "verocaml:proof-region:2:%d:%d" start_ end_

let local_assert_id region predicate ordinal =
  let start_, end_ = location_number region in
  let predicate_start, predicate_end = location_number predicate in
  Printf.sprintf "verocaml:local-assert:1:%d:%d:%d:%d:%d" start_ end_
    ordinal predicate_start predicate_end

let retained_proof_region_mapper ~keep_ghost () =
  let default = Ast_mapper.default_mapper in
  let visible = ref [] in
  let callable = ref None in
  let proof_declaration = ref false in
  let proof_body = ref None in
  let prefix_contracts = ref [] in
  let statement_position = ref false in
  let exec_proof_region = ref None in
  let local_assertion_locations = ref [] in
  let with_ref reference value action =
    let saved = !reference in
    reference := value;
    Fun.protect ~finally:(fun () -> reference := saved) action
  in
  let with_patterns patterns action =
    let nested = List.fold_left add_proof_pattern !visible patterns in
    with_ref visible nested action
  in
  let local_assertions_in_source_order expression prefix_locations =
    let locations = ref [] in
    let iterator_default = Ast_iterator.default_iterator in
    let iterator =
      {
        iterator_default with
        expr =
          (fun self expression ->
            (match expression.pexp_desc with
            | ( Pexp_assert _
              | Pexp_extension ({ txt = "verocaml.assert"; _ }, _) )
              when
                not
                  (List.exists
                     (fun location -> location = expression.pexp_loc)
                     prefix_locations) ->
                locations := expression.pexp_loc :: !locations
            | _ -> ());
            iterator_default.expr self expression);
      }
    in
    iterator.expr iterator expression;
    List.sort
      (fun (left : Location.t) (right : Location.t) ->
        let by_start =
          Int.compare left.loc_start.pos_cnum right.loc_start.pos_cnum
        in
        if by_start <> 0 then by_start
        else Int.compare left.loc_end.pos_cnum right.loc_end.pos_cnum)
      !locations
  in
  let free_local_names expression =
    let free = ref [] in
    let bound = ref [] in
    let with_bound patterns action =
      let saved = !bound in
      bound :=
        List.fold_left
          (fun bound pattern ->
            List.fold_left
              (fun bound name ->
                name.txt
                :: List.filter
                     (fun existing -> not (String.equal existing name.txt))
                     bound)
              bound (pattern_value_bindings pattern))
          !bound patterns;
      Fun.protect ~finally:(fun () -> bound := saved) action
    in
    let iterator_default = Ast_iterator.default_iterator in
    let iterator =
      {
        iterator_default with
        expr =
          (fun self expression ->
            (match expression.pexp_desc with
            | Pexp_ident { txt = Longident.Lident name; _ }
              when
                not (List.mem name !bound)
                && List.exists
                     (fun binding ->
                       String.equal binding.proof_binding_name.txt name)
                     !visible ->
                free := name :: !free
            | Pexp_let (_, rec_flag, bindings, body) ->
                let patterns =
                  List.map (fun binding -> binding.pvb_pat) bindings
                in
                (match rec_flag with
                | Nonrecursive ->
                    List.iter
                      (fun binding -> self.expr self binding.pvb_expr)
                      bindings
                | Recursive ->
                    with_bound patterns (fun () ->
                        List.iter
                          (fun binding -> self.expr self binding.pvb_expr)
                          bindings));
                with_bound patterns (fun () -> self.expr self body)
            | Pexp_function (parameters, _, body) ->
                List.iter
                  (fun parameter ->
                    match parameter.pparam_desc with
                    | Pparam_val (_, default_value, _) ->
                        Option.iter (self.expr self) default_value
                    | Pparam_newtype _ -> ())
                  parameters;
                let patterns =
                  List.filter_map
                    (fun parameter ->
                      match parameter.pparam_desc with
                      | Pparam_val (_, _, pattern) -> Some pattern
                      | Pparam_newtype _ -> None)
                    parameters
                in
                with_bound patterns (fun () ->
                    match body with
                    | Pfunction_body body -> self.expr self body
                    | Pfunction_cases (cases, _, _) ->
                        List.iter (self.case self) cases)
            | Pexp_match (scrutinee, cases)
            | Pexp_try (scrutinee, cases) ->
                self.expr self scrutinee;
                List.iter (self.case self) cases
            | Pexp_for (pattern, first, last, _, body) ->
                self.expr self first;
                self.expr self last;
                with_bound [ pattern ] (fun () -> self.expr self body)
            | _ -> iterator_default.expr self expression));
        case =
          (fun self case ->
            with_bound [ case.pc_lhs ] (fun () ->
                Option.iter (self.expr self) case.pc_guard;
                self.expr self case.pc_rhs));
      }
    in
    iterator.expr iterator expression;
    List.sort_uniq String.compare !free
  in
  let retained_carrier ?free_names expression payload kind =
    let callable =
      match !callable with
      | Some callable -> callable
      | None ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "%%verocaml.proof is supported only inside a top-level callable"
    in
    let free =
      Option.value free_names ~default:(free_local_names payload)
    in
    let captures =
      List.filter
        (fun binding -> List.mem binding.proof_binding_name.txt free)
        !visible
    in
    let carrier_loc = { expression.pexp_loc with loc_ghost = true } in
    let manifest =
      proof_capture_manifest callable expression.pexp_loc payload.pexp_loc
        captures kind
    in
    let region_id =
      match kind with
      | Proof_region_capture -> proof_region_id expression.pexp_loc
      | Local_assert_capture ordinal ->
          local_assert_id expression.pexp_loc payload.pexp_loc ordinal
    in
    let marker =
      apply_ghost ~loc:carrier_loc ~attrs:[] "marker"
        (string_constant ~loc:carrier_loc manifest)
    in
    let sidecar_marker =
      apply_ghost ~loc:carrier_loc ~attrs:[] "sidecar"
        (string_constant ~loc:carrier_loc manifest)
    in
    let retained_payload =
      match kind with
      | Proof_region_capture -> payload
      | Local_assert_capture _ ->
          apply_ghost ~loc:carrier_loc ~attrs:[] "assert_"
            (thunk ~loc:carrier_loc payload)
    in
    let proof_call =
      Exp.apply ~loc:carrier_loc
        (ghost_identifier ~loc:carrier_loc "proof_region")
        [
          (Nolabel, string_constant ~loc:carrier_loc region_id);
          (Nolabel, thunk ~loc:carrier_loc retained_payload);
        ]
    in
    let sidecar_body =
      Exp.sequence ~loc:carrier_loc sidecar_marker proof_call
    in
    let shadow_parameters =
      match captures with
      | [] ->
          [
            {
              pparam_loc = carrier_loc;
              pparam_desc =
                Pparam_val (Nolabel, None, unit_pattern ~loc:carrier_loc);
            };
          ]
      | captures ->
          List.map
            (fun binding -> aliased_shadow binding.proof_binding_name)
            captures
    in
    let constraint_ =
      {
        mode_annotations = [];
        ret_mode_annotations = [];
        ret_type_constraint = None;
      }
    in
    let sidecar_function =
      Exp.function_ ~loc:carrier_loc shadow_parameters constraint_
        (Pfunction_body sidecar_body)
    in
    let ignored =
      Exp.apply ~loc:carrier_loc
        (Exp.ident ~loc:carrier_loc
           { txt = Longident.Lident "ignore"; loc = carrier_loc })
        [ (Nolabel, sidecar_function) ]
    in
    let dead_sidecar =
      Exp.ifthenelse ~loc:carrier_loc
        (Exp.construct ~loc:carrier_loc
           { txt = Longident.Lident "false"; loc = carrier_loc }
           None)
        ignored
        (Some
           (Exp.construct ~loc:carrier_loc
              { txt = Longident.Lident "()"; loc = carrier_loc }
              None))
    in
    Exp.sequence ~loc:carrier_loc marker dead_sidecar
  in
  let mapper =
    {
      default with
      structure =
        (fun self structure ->
          List.map
            (fun item ->
              match item.pstr_desc with
              | Pstr_value (rec_flag, bindings) ->
                  let bindings =
                    List.map
                      (fun binding ->
                        let is_proof =
                          role_attributes binding.pvb_attributes
                          |> List.exists (fun attribute ->
                                 String.equal attribute.attr_name.txt
                                   proof_attribute)
                        in
                        let rec prefix_locations expression =
                          match expression.pexp_desc with
                          | Pexp_sequence (head, tail) -> (
                              match extension head with
                              | Some (name, _)
                                when Option.is_some (contract_of_name name) ->
                                  head.pexp_loc :: prefix_locations tail
                              | Some _ | None -> [])
                          | _ -> []
                        in
                        let expression =
                          match
                            ( binding.pvb_pat.ppat_desc,
                              binding.pvb_expr.pexp_desc )
                          with
                          | ( Ppat_var name,
                              Pexp_function
                                (_, _, Pfunction_body body) ) ->
                              let current =
                                {
                                  proof_callable_name = name.txt;
                                  proof_callable_binding_span = binding.pvb_loc;
                                  proof_callable_body_span = body.pexp_loc;
                                }
                              in
                              with_ref callable (Some current) (fun () ->
                                  with_ref visible [] (fun () ->
                                      with_ref proof_declaration is_proof
                                        (fun () ->
                                          with_ref proof_body (Some body)
                                            (fun () ->
                                              with_ref prefix_contracts
                                                (prefix_locations body)
                                                (fun () ->
                                                  with_ref
                                                    local_assertion_locations
                                                    (local_assertions_in_source_order
                                                       body
                                                       (prefix_locations body))
                                                    (fun () ->
                                                      self.expr self
                                                        binding.pvb_expr))))))
                          | _ -> self.expr self binding.pvb_expr
                        in
                        {
                          binding with
                          pvb_pat = self.pat self binding.pvb_pat;
                          pvb_expr = expression;
                        })
                      bindings
                  in
                  { item with pstr_desc = Pstr_value (rec_flag, bindings) }
              | _ -> default.structure_item self item)
            structure);
      expr =
        (fun self expression ->
          match expression.pexp_desc with
          | Pexp_extension
              ({ txt = "verocaml.proof" as name; loc = name_loc }, payload) ->
              let payload =
                payload_expression ~name ~loc:name_loc payload
              in
              if not keep_ghost then unit_expression ~loc:expression.pexp_loc
              else
                let free_names = free_local_names payload in
                let admitted_exec_region =
                  (not !proof_declaration) && !statement_position
                  && Option.is_none !exec_proof_region
                in
                let payload =
                  with_ref exec_proof_region
                    (if admitted_exec_region then Some expression.pexp_loc
                     else None)
                    (fun () ->
                      with_ref statement_position admitted_exec_region (fun () ->
                          self.expr self payload))
                in
                retained_carrier ~free_names expression payload
                  Proof_region_capture
          | Pexp_extension
              ({ txt = "verocaml.assert" as name; loc = name_loc }, payload)
            when
              (!proof_declaration || Option.is_some !exec_proof_region)
              && !statement_position
              && not
                   (List.exists
                      (fun location -> location = expression.pexp_loc)
                      !prefix_contracts) ->
              if not keep_ghost then unit_expression ~loc:expression.pexp_loc
              else
                let payload =
                  with_ref statement_position false (fun () ->
                      payload_expression ~name ~loc:name_loc payload
                      |> self.expr self)
                in
                let ordinal =
                  match
                    List.find_index
                      (fun location -> location = expression.pexp_loc)
                      !local_assertion_locations
                  with
                  | Some ordinal -> ordinal
                  | None ->
                      Location.raise_errorf ~loc:expression.pexp_loc
                        "internal error: local assertion source slot is absent"
                in
                retained_carrier expression payload
                  (Local_assert_capture ordinal)
          | Pexp_sequence (first, second) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_sequence
                    ( with_ref statement_position true (fun () ->
                          self.expr self first),
                      with_ref statement_position admitted (fun () ->
                          self.expr self second) );
              }
          | Pexp_let (mutable_flag, rec_flag, bindings, body) ->
              let admitted = !statement_position in
              let patterns = List.map (fun binding -> binding.pvb_pat) bindings in
              let map_binding binding =
                {
                  binding with
                  pvb_pat = self.pat self binding.pvb_pat;
                  pvb_expr =
                    with_ref statement_position false (fun () ->
                        self.expr self binding.pvb_expr);
                }
              in
              let bindings =
                match rec_flag with
                | Nonrecursive -> List.map map_binding bindings
                | Recursive ->
                    with_patterns patterns (fun () ->
                        List.map map_binding bindings)
              in
              let body =
                with_patterns patterns (fun () ->
                    with_ref statement_position admitted (fun () ->
                        self.expr self body))
              in
              {
                expression with
                pexp_desc = Pexp_let (mutable_flag, rec_flag, bindings, body);
              }
          | Pexp_function (parameters, constraint_, body) ->
              let parameters =
                List.map
                  (fun parameter ->
                    match parameter.pparam_desc with
                    | Pparam_val (label, default_value, pattern) ->
                        {
                          parameter with
                          pparam_desc =
                            Pparam_val
                              ( label,
                                Option.map (self.expr self) default_value,
                                self.pat self pattern );
                        }
                    | Pparam_newtype _ -> parameter)
                  parameters
              in
              let parameter_patterns =
                List.filter_map
                  (fun parameter ->
                    match parameter.pparam_desc with
                    | Pparam_val (_, _, pattern) -> Some pattern
                    | Pparam_newtype _ -> None)
                  parameters
              in
              let body =
                with_patterns parameter_patterns (fun () ->
                    match body with
                    | Pfunction_body body ->
                        let admitted =
                          match !proof_body with
                          | Some root -> root == body && !proof_declaration
                          | None -> false
                        in
                        Pfunction_body
                          (with_ref statement_position admitted (fun () ->
                               self.expr self body))
                    | Pfunction_cases (cases, loc, attrs) ->
                        Pfunction_cases
                          ( with_ref statement_position false (fun () ->
                                List.map (self.case self) cases),
                            loc,
                            attrs ))
              in
              {
                expression with
                pexp_desc = Pexp_function (parameters, constraint_, body);
              }
          | Pexp_match (scrutinee, cases) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_match
                    ( with_ref statement_position false (fun () ->
                          self.expr self scrutinee),
                      with_ref statement_position admitted (fun () ->
                          List.map (self.case self) cases) );
              }
          | Pexp_try (body, cases) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_try
                    ( with_ref statement_position admitted (fun () ->
                          self.expr self body),
                      with_ref statement_position admitted (fun () ->
                          List.map (self.case self) cases) );
              }
          | Pexp_ifthenelse (condition, consequent, alternative) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_ifthenelse
                    ( with_ref statement_position false (fun () ->
                          self.expr self condition),
                      with_ref statement_position admitted (fun () ->
                          self.expr self consequent),
                      Option.map
                        (fun alternative ->
                          with_ref statement_position admitted (fun () ->
                              self.expr self alternative))
                        alternative );
              }
          | Pexp_constraint (payload, typ, modes) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_constraint
                    ( with_ref statement_position admitted (fun () ->
                          self.expr self payload),
                      Option.map (self.typ self) typ,
                      modes );
              }
          | Pexp_for (pattern, first, last, direction, body) ->
              let admitted = !statement_position in
              {
                expression with
                pexp_desc =
                  Pexp_for
                    ( self.pat self pattern,
                      with_ref statement_position false (fun () ->
                          self.expr self first),
                      with_ref statement_position false (fun () ->
                          self.expr self last),
                      direction,
                      with_patterns [ pattern ] (fun () ->
                          with_ref statement_position admitted (fun () ->
                              self.expr self body)) );
              }
          | _ ->
              with_ref statement_position false (fun () ->
                  default.expr self expression));
      case =
        (fun self case ->
          let pattern = self.pat self case.pc_lhs in
          let admitted = !statement_position in
          with_patterns [ case.pc_lhs ] (fun () ->
              {
                pc_lhs = pattern;
                pc_guard =
                  Option.map
                    (fun guard ->
                      with_ref statement_position false (fun () ->
                          self.expr self guard))
                    case.pc_guard;
                pc_rhs =
                  with_ref statement_position admitted (fun () ->
                      self.expr self case.pc_rhs);
              }));
    }
  in
  mapper

let instance_mode_mapper ~keep_ghost =
  let default = Ast_mapper.default_mapper in
  let current_stage = ref Exec_stage in
  let declaration_function = ref false in
  let function_depth = ref 0 in
  let pattern_component = ref false in
  let with_ref reference value action =
    let saved = !reference in
    reference := value;
    Fun.protect ~finally:(fun () -> reference := saved) action
  in
  let validate_local_mode ~loc = function
    | None -> ()
    | Some Tracked_mode when !current_stage = Spec_stage ->
        Location.raise_errorf ~loc
          "specification code cannot create or receive Tracked values"
    | Some (Tracked_mode | Ghost_mode) -> ()
  in
  let rec mapper =
    {
      default with
      attribute =
        (fun self attribute ->
          reject_reserved_attribute attribute;
          if public_verification_scope attribute then
            Location.raise_errorf ~loc:attribute.attr_loc
              "[@@@%s] is only valid at implementation-unit scope"
              verification_scope_attribute;
          if Option.is_some (public_instance_mode_attribute attribute) then
            Location.raise_errorf ~loc:attribute.attr_loc
              "misplaced [@%s] instance-mode annotation"
              attribute.attr_name.txt;
          default.attribute self attribute);
      structure_item =
        (fun self item ->
          match item.pstr_desc with
          | Pstr_value (rec_flag, bindings) ->
              let bindings, erased_results =
                List.map
                  (fun binding ->
                    let declaration_modes =
                      declaration_expression_modes binding.pvb_expr
                    in
                    let declaration_finite_formals =
                      declaration_expression_finite_formals binding.pvb_expr
                    in
                    let role =
                      role_attributes binding.pvb_attributes
                      |> function
                      | [] -> None
                      | attribute :: _ ->
                          declaration_role attribute.attr_name.txt
                    in
                    let explicit_compiler_mode_syntax =
                      match role with
                      | Some role
                        when String.equal role.attribute_name
                               external_specification_attribute ->
                          callable_has_explicit_compiler_mode_syntax binding
                      | Some _ | None -> false
                    in
                    let mode, attributes =
                      split_instance_mode_attributes binding.pvb_attributes
                    in
                    if Option.is_some mode then
                      Location.raise_errorf ~loc:binding.pvb_loc
                        "[@ghost] and [@tracked] local-binding syntax is not \
                         supported on a top-level declaration";
                    let stage =
                      stage_of_role ~recursive:(rec_flag = Recursive) role
                    in
                    let expression =
                      with_ref current_stage stage (fun () ->
                          with_ref declaration_function true (fun () ->
                              self.expr self binding.pvb_expr))
                    in
                    let erased_result =
                      List.exists
                        (fun attribute ->
                          String.starts_with
                            ~prefix:
                              (internal_mode_prefix
                             ^ instance_site_name Result_site ^ ".")
                            attribute.attr_name.txt)
                        expression.pexp_attributes
                    in
                    ( {
                        binding with
                        pvb_pat = self.pat self binding.pvb_pat;
                        pvb_expr = expression;
                        pvb_attributes =
                          family_attribute ~keep_ghost ~loc:binding.pvb_loc
                          ::
                          (if keep_ghost then
                             mode_signature_attribute ~loc:binding.pvb_loc
                               declaration_modes
                             :: finite_signature_attribute ~loc:binding.pvb_loc
                                  declaration_finite_formals
                             ::
                             (if explicit_compiler_mode_syntax then
                                explicit_compiler_mode_attribute
                                  ~loc:binding.pvb_loc
                                :: self.attributes self attributes
                              else self.attributes self attributes)
                           else self.attributes self attributes);
                      },
                      erased_result ))
                  bindings
                |> List.split
              in
              if
                (not keep_ghost)
                && List.exists Fun.id erased_results
              then (
                if List.length bindings <> 1 then
                  Location.raise_errorf ~loc:item.pstr_loc
                    "an erased-result declaration must be a single top-level \
                     binding";
                {
                  item with
                  pstr_desc =
                    Pstr_attribute
                      (family_attribute ~keep_ghost ~loc:item.pstr_loc);
                })
              else { item with pstr_desc = Pstr_value (rec_flag, bindings) }
          | Pstr_type (rec_flag, declarations) ->
              let declarations =
                List.filter_map
                  (rewrite_type_declaration self ~keep_ghost)
                  declarations
              in
              if declarations = [] then
                {
                  item with
                  pstr_desc =
                    Pstr_attribute
                      (family_attribute ~keep_ghost ~loc:item.pstr_loc);
                }
              else { item with pstr_desc = Pstr_type (rec_flag, declarations) }
          | _ -> default.structure_item self item);
      signature_item =
        (fun self item ->
          match item.psig_desc with
          | Psig_value value ->
              let attributes = self.attributes self value.pval_attributes in
              let external_bodies = external_body_attributes attributes in
              if external_bodies <> [] then
                Location.raise_errorf
                  ~loc:(List.hd external_bodies).attr_loc
                  "[@@%s] is only valid on one nonrecursive top-level function \
                   binding"
                  external_body_attribute;
              let roles = role_attributes attributes in
              if List.length roles > 1 then
                Location.raise_errorf ~loc:value.pval_loc
                  "signature value has conflicting VeroCaml declaration roles";
              let role =
                match roles with
                | [] -> None
                | [ attribute ] -> declaration_role attribute.attr_name.txt
                | _ -> assert false
              in
              let stage = stage_of_role ~recursive:false role in
              let modes, finite_formals, typ =
                rewrite_signature_result self ~keep_ghost ~stage
                  value.pval_type
              in
              let erased_declaration =
                (not keep_ghost)
                &&
                (match role with
                | Some role -> not role.preserve_ordinary_body
                | None -> false)
                ||
                ((not keep_ghost)
                &&
                match List.rev modes with
                | Some (Ghost_mode | Tracked_mode) :: _ -> true
                | None :: _ | [] -> false)
              in
              if erased_declaration then
                {
                  item with
                  psig_desc =
                    Psig_attribute
                      (family_attribute ~keep_ghost ~loc:item.psig_loc);
                }
              else
                {
                  item with
                  psig_desc =
                    Psig_value
                      {
                        value with
                        pval_type = typ;
                        pval_attributes =
                          family_attribute ~keep_ghost ~loc:value.pval_loc
                          :: mode_signature_attribute ~loc:value.pval_loc modes
                          ::
                          (if keep_ghost then
                             retain_or_issue_finite_signature
                               ~loc:value.pval_loc ~finite_formals attributes
                             :: without_finite_signature attributes
                           else without_finite_signature attributes);
                      };
                }
          | Psig_type (rec_flag, declarations) ->
              let declarations =
                List.filter_map
                  (rewrite_type_declaration self ~keep_ghost)
                  declarations
              in
              if declarations = [] then
                {
                  item with
                  psig_desc =
                    Psig_attribute
                      (family_attribute ~keep_ghost ~loc:item.psig_loc);
                }
              else { item with psig_desc = Psig_type (rec_flag, declarations) }
          | _ -> default.signature_item self item);
      expr = rewrite_mode_expression;
      pat = rewrite_mode_pattern;
      typ =
        (fun self typ ->
          let mode, _ = split_instance_mode_attributes typ.ptyp_attributes in
          let finite, _ = split_finite_attributes typ.ptyp_attributes in
          match (mode, finite) with
          | Some _, _ ->
              Location.raise_errorf ~loc:typ.ptyp_loc
                "mode annotation is not supported at this type position"
          | None, true ->
              Location.raise_errorf ~loc:typ.ptyp_loc
                "[@finite] is supported only on an outer top-level callable formal"
          | None, false -> default.typ self typ);
    }

  and rewrite_type_declaration self ~keep_ghost declaration =
    List.iter reject_reserved_attribute declaration.ptype_attributes;
    let external_type_specifications, declaration_attributes =
      List.partition
        (fun attribute ->
          String.equal attribute.attr_name.txt
            external_type_specification_attribute)
        declaration.ptype_attributes
    in
    List.iter validate_empty_attribute external_type_specifications;
    let external_type_specification =
      match external_type_specifications with
      | [] -> false
      | [ _ ] -> true
      | duplicate :: _ ->
          Location.raise_errorf ~loc:duplicate.attr_loc
            "duplicate [@@%s] attribute"
            external_type_specification_attribute
    in
    let () =
      if external_type_specification then
        let direct_parameter argument =
          match argument.ptyp_desc with
          | Ptyp_var (name, _) -> Some name
          | _ -> None
        in
        let declared_parameters =
          List.map (fun (parameter, _) -> direct_parameter parameter)
            declaration.ptype_params
        in
        let target_parameters =
          match declaration.ptype_manifest with
          | Some { ptyp_desc = Ptyp_constr (_, arguments); _ } ->
              List.map direct_parameter arguments
          | Some _ | None -> []
        in
        if
          declaration.ptype_kind <> Ptype_abstract
          || declaration.ptype_private <> Public
          || declaration.ptype_cstrs <> []
          || List.exists Option.is_none declared_parameters
          || declared_parameters <> target_parameters
        then
          Location.raise_errorf ~loc:declaration.ptype_loc
            "[@@%s] requires a public transparent type alias whose target uses \
             each declared type parameter once in declaration order"
            external_type_specification_attribute
    in
    let rewrite_label label =
      List.iter reject_reserved_attribute label.pld_attributes;
      let mode, attributes =
        split_instance_mode_attributes label.pld_attributes
      in
      match mode with
      | Some (Ghost_mode | Tracked_mode) when not keep_ghost -> None
      | Some mode ->
          Some
            {
              label with
              pld_type = self.typ self label.pld_type;
              pld_attributes =
                internal_mode_attribute ~site:Field_site ~mode
                  ~loc:label.pld_loc
                :: attributes;
            }
      | None ->
          Some
            {
              label with
              pld_type = self.typ self label.pld_type;
              pld_attributes = self.attributes self attributes;
            }
    in
    let rewrite_constructor constructor =
      let args =
        match constructor.pcd_args with
        | Pcstr_record labels ->
            Pcstr_record (List.filter_map rewrite_label labels)
        | Pcstr_tuple arguments ->
            let arguments =
              List.filter_map
                (fun argument ->
                  let mode, typ = mode_on_type argument.pca_type in
                  match mode with
                  | Some (Ghost_mode | Tracked_mode) when not keep_ghost -> None
                  | Some mode ->
                      Some
                        {
                          argument with
                          pca_type =
                            {
                              (self.typ self typ) with
                              ptyp_attributes =
                                internal_mode_attribute
                                  ~site:Component_type_site ~mode
                                  ~loc:argument.pca_loc
                                :: (self.typ self typ).ptyp_attributes;
                            };
                        }
                  | None ->
                      Some { argument with pca_type = self.typ self typ })
                arguments
            in
            Pcstr_tuple arguments
      in
      { constructor with pcd_args = args }
    in
    let kind =
      match declaration.ptype_kind with
      | Ptype_record labels ->
          let labels = List.filter_map rewrite_label labels in
          if labels = [] then None else Some (Ptype_record labels)
      | Ptype_variant constructors ->
          Some (Ptype_variant (List.map rewrite_constructor constructors))
      | Ptype_abstract | Ptype_open
      | Ptype_record_unboxed_product _ as kind ->
          Some kind
    in
    Option.map
      (fun ptype_kind ->
        {
          declaration with
          ptype_kind;
          ptype_attributes =
            family_attribute ~keep_ghost ~loc:declaration.ptype_loc
            ::
            (if keep_ghost then
               type_mode_signature_attribute declaration
               ::
               (if external_type_specification then
                  retained_external_type_specification_attribute
                    ~loc:declaration.ptype_loc
                  :: self.attributes self declaration_attributes
                else self.attributes self declaration_attributes)
             else self.attributes self declaration_attributes);
        })
      kind

  and rewrite_mode_expression self expression =
    List.iter reject_reserved_attribute expression.pexp_attributes;
    let mode, attributes =
      split_instance_mode_attributes expression.pexp_attributes
    in
    match mode with
    | Some mode ->
        validate_local_mode ~loc:expression.pexp_loc (Some mode);
        if not keep_ghost then unit_expression ~loc:expression.pexp_loc
        else
          let rewritten =
            default.expr self { expression with pexp_attributes = attributes }
          in
          {
            rewritten with
            pexp_attributes =
              internal_mode_attribute ~site:Expression_site ~mode
                ~loc:expression.pexp_loc
              :: rewritten.pexp_attributes;
          }
    | None -> (
        match expression.pexp_desc with
        | Pexp_let (mutable_flag, Nonrecursive, bindings, body) ->
            let marked =
              List.filter_map
                (fun binding ->
                  let mode, _ =
                    split_instance_mode_attributes binding.pvb_attributes
                  in
                  Option.map (fun mode -> (binding, mode)) mode)
                bindings
            in
            if marked <> [] && List.length bindings <> 1 then
              Location.raise_errorf ~loc:expression.pexp_loc
                "a mode-annotated let must contain exactly one binding";
            (match marked with
            | [ binding, binding_mode ] ->
                validate_local_mode ~loc:binding.pvb_loc (Some binding_mode);
                if not (simple_binding_pattern binding.pvb_pat) then
                  Location.raise_errorf ~loc:binding.pvb_pat.ppat_loc
                    "a mode-annotated let requires a variable pattern";
                if not keep_ghost then self.expr self body
                else
                  let _, binding_attributes =
                    split_instance_mode_attributes binding.pvb_attributes
                  in
                  let binding =
                    {
                      binding with
                      pvb_pat = self.pat self binding.pvb_pat;
                      pvb_expr = self.expr self binding.pvb_expr;
                      pvb_attributes =
                        internal_mode_attribute ~site:Binding_site
                          ~mode:binding_mode ~loc:binding.pvb_loc
                        :: binding_attributes;
                    }
                  in
                  {
                    expression with
                      pexp_desc =
                      Pexp_let
                        ( mutable_flag,
                          Nonrecursive,
                          [ binding ],
                          self.expr self body );
                    pexp_attributes = self.attributes self attributes;
                  }
            | [] -> default.expr self expression
            | _ -> assert false)
        | Pexp_let (_, Recursive, bindings, _) ->
            List.iter
              (fun binding ->
                let mode, _ =
                  split_instance_mode_attributes binding.pvb_attributes
                in
                if Option.is_some mode then
                  Location.raise_errorf ~loc:binding.pvb_loc
                    "mode-annotated recursive local bindings are unsupported")
              bindings;
            default.expr self expression
        | Pexp_function (parameters, constraint_, body) ->
            let is_declaration = !declaration_function && !function_depth = 0 in
            let formal_attributes = ref [] in
            let formal_index = ref 0 in
            let parameters =
              List.filter_map
                (fun parameter ->
                  match parameter.pparam_desc with
                  | Pparam_newtype _ -> Some parameter
                  | Pparam_val (label, default_value, pattern) ->
                      let index = !formal_index in
                      incr formal_index;
                      let formal_mode, finite, pattern =
                        signature_on_pattern_constraint pattern
                      in
                      if
                        (Option.is_some formal_mode || finite)
                        && not is_declaration
                      then
                        Location.raise_errorf ~loc:pattern.ppat_loc
                          "mode-bearing and finite formals are supported only \
                           on a top-level declaration";
                      validate_signature_mode ~stage:!current_stage
                        ~loc:pattern.ppat_loc formal_mode;
                      if
                        (not keep_ghost)
                        &&
                        match formal_mode with
                        | Some (Ghost_mode | Tracked_mode) -> true
                        | None -> false
                      then None
                      else
                        let pattern =
                          match (formal_mode, finite) with
                          | None, false -> self.pat self pattern
                          | mode, finite ->
                              Option.iter
                                (fun mode ->
                              formal_attributes :=
                                internal_formal_mode_attribute ~index ~mode
                                  ~loc:pattern.ppat_loc
                                :: !formal_attributes)
                                mode;
                              if keep_ghost && finite then
                                formal_attributes :=
                                  internal_finite_formal_attribute ~index
                                    ~loc:pattern.ppat_loc
                                  :: !formal_attributes;
                              {
                                (self.pat self pattern) with
                                ppat_attributes =
                                  (self.pat self pattern).ppat_attributes;
                              }
                        in
                        Some
                          {
                            parameter with
                            pparam_desc =
                              Pparam_val
                                ( label,
                                  Option.map (self.expr self) default_value,
                                  pattern );
                          })
                parameters
            in
            let result_mode, constraint_ =
              match constraint_.ret_type_constraint with
              | Some (Pconstraint typ) ->
                  let result_mode, finite, typ = signature_on_type typ in
                  if finite then
                    Location.raise_errorf ~loc:typ.ptyp_loc
                      "finite result contracts are not supported";
                  (result_mode, { constraint_ with ret_type_constraint =
                    Some (Pconstraint (self.typ self typ)) })
              | Some (Pcoerce (source, target)) ->
                  ( None,
                    {
                      constraint_ with
                      ret_type_constraint =
                        Some
                          (Pcoerce
                             ( Option.map (self.typ self) source,
                               self.typ self target ));
                    } )
              | None -> (None, constraint_)
            in
            if Option.is_some result_mode && not is_declaration then
              Location.raise_errorf ~loc:expression.pexp_loc
                "mode-bearing results are supported only on a top-level \
                 declaration";
            validate_signature_mode ~stage:!current_stage
              ~loc:expression.pexp_loc result_mode;
            let body =
              with_ref declaration_function false (fun () ->
                  with_ref function_depth (!function_depth + 1) (fun () ->
                      match body with
                      | Pfunction_body body ->
                          Pfunction_body (self.expr self body)
                      | Pfunction_cases (cases, loc, attrs) ->
                          Pfunction_cases
                            (List.map (self.case self) cases, loc, attrs)))
            in
            let attributes =
              match result_mode with
              | None -> List.rev_append !formal_attributes attributes
              | Some mode ->
                  internal_mode_attribute ~site:Result_site ~mode
                    ~loc:expression.pexp_loc
                  :: List.rev_append !formal_attributes attributes
            in
            {
              expression with
              pexp_desc = Pexp_function (parameters, constraint_, body);
              pexp_attributes = attributes;
            }
        | Pexp_apply (callee, arguments) ->
            let arguments =
              List.filter_map
                (fun (label, argument) ->
                  let mode, _ =
                    split_instance_mode_attributes argument.pexp_attributes
                  in
                  if Option.is_some mode && not keep_ghost then None
                  else Some (label, self.expr self argument))
                arguments
            in
            if arguments = [] then self.expr self callee
            else
              {
                expression with
                pexp_desc = Pexp_apply (self.expr self callee, arguments);
                pexp_attributes = self.attributes self attributes;
              }
        | Pexp_record (fields, inherited) ->
            if Option.is_some inherited then
              Location.raise_errorf ~loc:expression.pexp_loc
                "inherited record updates are unsupported with instance modes";
            let fields =
              List.filter_map
                (fun (label, value) ->
                  let mode, _ =
                    split_instance_mode_attributes value.pexp_attributes
                  in
                  if Option.is_some mode && not keep_ghost then None
                  else Some (label, self.expr self value))
                fields
            in
            if fields = [] then unit_expression ~loc:expression.pexp_loc
            else
              {
                expression with
                pexp_desc = Pexp_record (fields, None);
                pexp_attributes = self.attributes self attributes;
              }
        | Pexp_construct (constructor, Some argument) -> (
            match argument.pexp_desc with
            | Pexp_tuple components ->
                let components =
                  List.filter_map
                    (fun (label, component) ->
                      let mode, _ =
                        split_instance_mode_attributes
                          component.pexp_attributes
                      in
                      if Option.is_some mode && not keep_ghost then None
                      else Some (label, self.expr self component))
                    components
                in
                let argument =
                  match components with
                  | [] -> None
                  | [ _, component ] -> Some component
                  | components ->
                      Some { argument with pexp_desc = Pexp_tuple components }
                in
                {
                  expression with
                  pexp_desc = Pexp_construct (constructor, argument);
                  pexp_attributes = self.attributes self attributes;
                }
            | _ -> default.expr self expression)
        | Pexp_tuple components ->
            let components =
              List.filter_map
                (fun (label, component) ->
                  let mode, _ =
                    split_instance_mode_attributes component.pexp_attributes
                  in
                  if Option.is_some mode && not keep_ghost then None
                  else Some (label, self.expr self component))
                components
            in
            (match components with
            | [] -> unit_expression ~loc:expression.pexp_loc
            | [ _, component ] -> component
            | components ->
                {
                  expression with
                  pexp_desc = Pexp_tuple components;
                  pexp_attributes = self.attributes self attributes;
                })
        | _ -> default.expr self expression)

  and rewrite_mode_pattern self pattern =
    List.iter reject_reserved_attribute pattern.ppat_attributes;
    let mode, attributes =
      split_instance_mode_attributes pattern.ppat_attributes
    in
    match mode with
    | Some mode ->
        if not !pattern_component then
          Location.raise_errorf ~loc:pattern.ppat_loc
            "mode annotations on patterns are supported only on erased record \
             or positional-variant components";
        if not keep_ghost then Pat.any ~loc:pattern.ppat_loc ()
        else
          {
            (default.pat self { pattern with ppat_attributes = attributes }) with
            ppat_attributes =
              internal_mode_attribute ~site:Pattern_site ~mode
                ~loc:pattern.ppat_loc
              :: attributes;
          }
    | None -> (
        match pattern.ppat_desc with
        | Ppat_record (fields, closed) ->
            let fields =
              List.filter_map
                (fun (label, field_pattern) ->
                  let mode, _ =
                    split_instance_mode_attributes
                      field_pattern.ppat_attributes
                  in
                  if Option.is_some mode && not keep_ghost then None
                  else
                    Some
                      ( label,
                        with_ref pattern_component true (fun () ->
                            self.pat self field_pattern) ))
                fields
            in
            if fields = [] then Pat.any ~loc:pattern.ppat_loc ()
            else
              {
                pattern with
                ppat_desc = Ppat_record (fields, closed);
                ppat_attributes = self.attributes self attributes;
              }
        | Ppat_construct (constructor, Some (vars, argument)) -> (
            match argument.ppat_desc with
            | Ppat_tuple (components, tuple_closed) ->
                let components =
                  List.filter_map
                    (fun (label, component) ->
                      let mode, _ =
                        split_instance_mode_attributes
                          component.ppat_attributes
                      in
                      if Option.is_some mode && not keep_ghost then None
                      else
                        Some
                          ( label,
                            with_ref pattern_component true (fun () ->
                                self.pat self component) ))
                    components
                in
                let argument =
                  match components with
                  | [] -> None
                  | [ _, component ] -> Some (vars, component)
                  | components ->
                          Some
                        ( vars,
                          {
                            argument with
                            ppat_desc = Ppat_tuple (components, tuple_closed);
                          } )
                in
                {
                  pattern with
                  ppat_desc = Ppat_construct (constructor, argument);
                  ppat_attributes = self.attributes self attributes;
                }
            | _ -> default.pat self pattern)
        | _ -> default.pat self pattern)
  in
  mapper

let rewrite_expression_extensions ~keep_ghost ~logical_scope
    ~lexical_bindings ~function_scopes ~default self expression =
  match
    Vero_ppx_logical_builtin_private.rewrite_in_payload logical_scope
      ~map:(self.Ast_mapper.expr self) expression
  with
  | Some expression -> expression
  | None -> (
      match expression.pexp_desc with
      | Pexp_let (rec_flag, value_constraint, bindings, body) ->
          Vero_ppx_logical_builtin_private.map_let
            ~reject_attributes:(List.iter reject_misplaced_spec_attribute)
            ~map_binding:(default.Ast_mapper.value_binding self)
            ~map_expression:(self.expr self) ~lexical_bindings expression
            rec_flag value_constraint bindings body
      | Pexp_function (parameters, _, Pfunction_body body) ->
          Vero_ppx_logical_builtin_private.map_function
            ~visible_parameters:visible_parameter_bindings ~lexical_bindings
            ~function_scopes ~map:(default.expr self) expression parameters
            body
      | Pexp_extension
          ({ txt = "verocaml.proof" as name; loc = name_loc }, payload) ->
          let payload = payload_expression ~name ~loc:name_loc payload in
          if not keep_ghost then unit_expression ~loc:expression.pexp_loc
          else
            let payload =
              Vero_ppx_logical_builtin_private.with_logical_payload
                logical_scope (fun () -> self.expr self payload)
            in
            let carrier_loc = { expression.pexp_loc with loc_ghost = true } in
            let start_, end_ = location_number expression.pexp_loc in
            let id =
              Printf.sprintf "verocaml:proof-region:1:%d:%d" start_ end_
            in
            Exp.apply ~loc:carrier_loc
              (ghost_identifier ~loc:carrier_loc "proof_region")
              [
                (Nolabel, string_constant ~loc:carrier_loc id);
                (Nolabel, thunk ~loc:carrier_loc payload);
              ]
      | Pexp_extension
          ({ txt = "verocaml.reveal" as name; loc = name_loc }, payload) ->
          let target = payload_expression ~name ~loc:name_loc payload in
          if not keep_ghost then unit_expression ~loc:expression.pexp_loc
          else
            reveal_application ~name:"reveal" ~loc:expression.pexp_loc
              (self.expr self target) None
      | Pexp_extension
          ( { txt = "verocaml.use_type_invariant" as name; loc = name_loc },
            payload ) ->
          let value = payload_expression ~name ~loc:name_loc payload in
          if not keep_ghost then unit_expression ~loc:expression.pexp_loc
          else
            use_type_invariant_application ~loc:expression.pexp_loc
              (self.expr self value)
      | Pexp_extension
          ( { txt = "verocaml.reveal_with_fuel" as name; loc = name_loc },
            payload ) ->
          let payload = payload_expression ~name ~loc:name_loc payload in
          let target, depth =
            match payload.pexp_desc with
            | Pexp_tuple [ (None, target); (None, depth) ] ->
                (target, reveal_literal ~name depth)
            | _ ->
                Location.raise_errorf ~loc:payload.pexp_loc
                  "%%%s expects (function, literal-depth)" name
          in
          if not keep_ghost then unit_expression ~loc:expression.pexp_loc
          else
            reveal_application ~name:"reveal_with_fuel"
              ~loc:expression.pexp_loc (self.expr self target) (Some depth)
      | Pexp_extension
          ({ txt = "verocaml.old" as name; loc = name_loc }, payload) ->
          if
            not
              (Vero_ppx_logical_builtin_private.inside_ensures logical_scope)
          then
            Location.raise_errorf ~loc:expression.pexp_loc
              "%%%s is only valid inside a verocaml.ensures payload" name;
          let payload =
            payload_expression ~name ~loc:name_loc payload |> self.expr self
          in
          apply_ghost ~loc:expression.pexp_loc
            ~attrs:expression.pexp_attributes "old" payload
      | Pexp_extension ({ txt = name; _ }, _)
        when contract_of_name name <> None ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "%%%s is only valid in a contiguous function-body contract prefix"
            name
      | Pexp_extension ({ txt = name; _ }, _)
        when String.starts_with ~prefix:"verocaml." name ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "unsupported verocaml extension %%%s" name
      | _ -> default.expr self expression)

let make arguments =
  let keep_ghost =
    match arguments with
    | [] -> false
    | [ "--keep-ghost" ] -> true
    | _ ->
        invalid_arg
          "verocaml-ppx accepts only the optional --keep-ghost argument"
  in
  let default = Ast_mapper.default_mapper in
  let root_structure = ref true in
  let root_signature = ref true in
  let instance_modes = instance_mode_mapper ~keep_ghost in
  let proof_regions = retained_proof_region_mapper ~keep_ghost () in
  let function_scopes = ref [] in
  let lexical_bindings = ref [] in
  let logical_scope = Vero_ppx_logical_builtin_private.create_logical_scope () in
  let next_clause = ref 0 in

  let rec mapper =
    {
      default with
      attribute =
        (fun self attribute ->
          if public_verification_scope attribute then
            Location.raise_errorf ~loc:attribute.attr_loc
              "[@@@%s] is only valid at implementation-unit scope"
              verification_scope_attribute;
          Vero_ppx_broadcast_private.reject_misplaced_attribute attribute;
          reject_misplaced_spec_attribute attribute;
          default.attribute self attribute);
      structure =
        (fun self structure ->
          let structure =
            if !root_structure then (
              root_structure := false;
              let structure = normalize_axiom_sugar structure in
              let structure = split_root_verification_scope structure in
              let structure =
                instance_modes.structure instance_modes structure
              in
              proof_regions.structure proof_regions structure)
            else structure
          in
          let structure =
            Vero_ppx_symbolic_private.rewrite_structure ~keep_ghost structure
          in
          let structure =
            Vero_ppx_broadcast_private.rewrite_structure ~keep_ghost structure
          in
          List.concat_map (rewrite_structure_item self) structure);
      signature =
        (fun _self signature ->
          if !root_signature then (
            root_signature := false;
            instance_modes.signature instance_modes signature)
          else signature);
      expr =
        (fun self expression ->
          match
            Vero_ppx_logical_builtin_private.rewrite_scoped_body
              ~function_scopes ~lexical_bindings
              ~rewrite:(rewrite_function_body self) expression
          with
          | Some expression -> expression
          | None -> rewrite_expression self expression);
    }

  and rewrite_expression self expression =
    match
      Vero_ppx_broadcast_private.rewrite_expression ~keep_ghost
        ~map:(self.expr self) expression
    with
    | Some expression -> expression
    | None ->
        rewrite_expression_extensions ~keep_ghost ~logical_scope
          ~lexical_bindings ~function_scopes ~default self expression

  and rewrite_structure_item self item =
    match item.pstr_desc with
    | Pstr_value (rec_flag, bindings) ->
        let marked =
          List.filter
            (fun binding -> declaration_attributes binding.pvb_attributes <> [])
            bindings
        in
        if marked = [] then [ default.structure_item self item ]
        else if List.length bindings <> 1 then
          let attribute =
            List.hd
              (declaration_attributes (List.hd marked).pvb_attributes)
          in
          Location.raise_errorf ~loc:item.pstr_loc
            "[@@%s] requires a single top-level binding"
            attribute.attr_name.txt
        else
          let binding = List.hd bindings in
          let attributes = role_attributes binding.pvb_attributes in
          let external_bodies =
            external_body_attributes binding.pvb_attributes
          in
          let visibility = visibility_attributes binding.pvb_attributes in
          List.iter validate_role_attribute (attributes @ external_bodies);
          if List.length external_bodies > 1 then
            Location.raise_errorf ~loc:(List.hd external_bodies).attr_loc
              "duplicate [@%s] attribute" external_body_attribute
          else if List.length attributes > 1 then
            let first = List.hd attributes in
            let second = List.nth attributes 1 in
            if String.equal first.attr_name.txt second.attr_name.txt then
              Location.raise_errorf ~loc:first.attr_loc
                "duplicate [@%s] attribute" first.attr_name.txt
            else
              Location.raise_errorf ~loc:first.attr_loc
                "conflicting VeroCaml declaration roles [@@%s] and [@@%s]"
                first.attr_name.txt second.attr_name.txt
          else if
            external_bodies <> []
            && attributes <> []
            && not
                 (String.equal
                    (List.hd attributes).attr_name.txt proof_attribute)
          then
            Location.raise_errorf ~loc:(List.hd external_bodies).attr_loc
              "conflicting VeroCaml declaration roles [@@%s] and [@@%s]"
              (List.hd attributes).attr_name.txt external_body_attribute
          else if
            rec_flag <> Nonrecursive
            &&
            (external_bodies <> []
            ||
            match attributes with
            | [ attribute ] ->
                not
                  (List.exists (String.equal attribute.attr_name.txt)
                     [ proof_attribute; spec_attribute ])
            | [] -> true
            | _ -> assert false)
          then
            let attribute =
              match external_bodies with
              | attribute :: _ -> attribute
              | [] -> List.hd attributes
            in
            Location.raise_errorf ~loc:binding.pvb_loc
              "[@@%s] does not support recursive bindings"
              attribute.attr_name.txt
          else
            let role, attribute =
              match (attributes, external_bodies) with
              | [], [ attribute ] -> (external_body_role, attribute)
              | [ primary ], [ attribute ]
                when String.equal primary.attr_name.txt proof_attribute ->
                  ( {
                      external_body_role with
                      role_name = "proof-external-body";
                      preserve_ordinary_body = false;
                    },
                    attribute )
              | [ attribute ], [] ->
                  ( Option.get (declaration_role attribute.attr_name.txt),
                    attribute )
              | _ -> assert false
            in
            let name = binding_name role binding in
            let recursive_spec =
              rec_flag = Recursive
              && String.equal role.attribute_name spec_attribute
            in
            let visibility =
              match (recursive_spec, visibility) with
              | true, [ visibility ] ->
                  validate_role_attribute visibility;
                  Some (visibility_name visibility)
              | true, [] ->
                  Location.raise_errorf ~loc:binding.pvb_loc
                    "[@@%s] does not support recursive bindings without exactly \
                     one [@@%s] or [@@%s]"
                    spec_attribute opaque_attribute revealed_attribute
              | true, visibility :: _ :: _ ->
                  Location.raise_errorf ~loc:visibility.attr_loc
                    "recursive [@@%s] has conflicting visibility attributes"
                    spec_attribute
              | false, [] -> None
              | false, visibility :: _ ->
                  Location.raise_errorf ~loc:visibility.attr_loc
                    "[@@%s] is valid only on a recursive [@@%s] binding"
                    visibility.attr_name.txt spec_attribute
            in
            if external_bodies <> [] then
              validate_external_body_contracts
                ~attribute_name:external_body_attribute binding.pvb_expr;
            let expression =
              Vero_ppx_logical_builtin_private.map_declaration_body
                logical_scope ~role:role.role_name ~map:(self.expr self)
                binding.pvb_expr
            in
            let () =
              Vero_ppx_logical_builtin_private.require_function_body
                ~attribute:role.attribute_name expression
            in
            let remaining_attributes =
              List.filter
                (fun candidate ->
                  not (declaration_attribute candidate.attr_name.txt)
                  && not
                       (String.equal candidate.attr_name.txt opaque_attribute
                       || String.equal candidate.attr_name.txt
                            revealed_attribute))
                binding.pvb_attributes
            in
            let rewritten_binding expression =
              {
                binding with
                pvb_pat = self.pat self binding.pvb_pat;
                pvb_expr = expression;
                pvb_attributes = self.attributes self remaining_attributes;
              }
            in
            if not keep_ghost then
              if role.preserve_ordinary_body then
                [
                  {
                    item with
                    pstr_desc =
                      Pstr_value (rec_flag, [ rewritten_binding expression ]);
                  };
                ]
              else []
            else
              let expression =
                wrap_declaration_body ~binding_location:binding.pvb_loc
                  ~attribute_location:attribute.attr_loc
                  ~attribute_name:role.attribute_name ~role:role.role_name
                  ~carrier:
                    (if recursive_spec then "recursive_spec_definition"
                     else role.carrier_name)
                  name ?visibility expression
              in
              let binding = rewritten_binding expression in
              [ { item with pstr_desc = Pstr_value (rec_flag, [ binding ]) } ]
    | _ -> [ default.structure_item self item ]

  and rewrite_function_body self parameters expression =
    match expression.pexp_desc with
    | Pexp_sequence (head, tail) -> (
        match extension head with
        | Some (name, payload) -> (
            match contract_of_name name with
            | Some contract ->
                let rewritten =
                  rewrite_contract self parameters head contract name payload
                in
                let tail = rewrite_function_body self parameters tail in
                (match rewritten with
                | None -> tail
                | Some (marker, sidecar) ->
                    {
                      expression with
                      pexp_desc =
                        Pexp_sequence
                          ( marker,
                            {
                              expression with
                              pexp_desc = Pexp_sequence (sidecar, tail);
                            } );
                    })
            | None -> self.expr self expression)
        | None -> self.expr self expression)
    | _ -> (
        match extension expression with
        | Some (name, _) when contract_of_name name <> None ->
            Location.raise_errorf ~loc:expression.pexp_loc
              "%%%s must be followed by a function body" name
        | _ -> self.expr self expression)

  and rewrite_contract self parameters expression contract name payload =
    let name_loc =
      match expression.pexp_desc with
      | Pexp_extension ({ loc; _ }, _) -> loc
      | _ -> assert false
    in
    let payload = payload_expression ~name ~loc:name_loc payload in
    let payload =
      match contract with
      | Ensures ->
          Vero_ppx_logical_builtin_private.with_logical_payload logical_scope (fun () ->
              Vero_ppx_logical_builtin_private.with_ensures logical_scope (fun () -> self.expr self payload))
      | Requires | Decreases | Assert ->
          Vero_ppx_logical_builtin_private.with_logical_payload logical_scope (fun () -> self.expr self payload)
    in
    if not keep_ghost then None
    else
      let ordinal = !next_clause in
      incr next_clause;
      let kind =
        match contract with
        | Requires -> "requires"
        | Ensures -> "ensures"
        | Decreases -> "decreases"
        | Assert -> "assert"
      in
      let clause_id =
        Printf.sprintf "verocaml:%d:%s:%d"
          expression.pexp_loc.loc_start.pos_cnum kind ordinal
      in
      let id = string_constant ~loc:expression.pexp_loc clause_id in
      let marker =
        apply_ghost ~loc:expression.pexp_loc
          ~attrs:expression.pexp_attributes "marker" id
      in
      let argument =
        match contract with
        | Ensures -> payload
        | Requires | Decreases | Assert ->
            thunk ~loc:expression.pexp_loc payload
      in
      let contract_call =
        apply_ghost ~loc:expression.pexp_loc ~attrs:[]
          (ghost_name contract) argument
      in
      let ghost_loc = { expression.pexp_loc with loc_ghost = true } in
      let sidecar_marker =
        apply_ghost ~loc:ghost_loc ~attrs:[] "sidecar"
          (string_constant ~loc:ghost_loc clause_id)
      in
      let sidecar_body = Exp.sequence ~loc:ghost_loc sidecar_marker contract_call in
      let mentioned = mentioned_identifiers payload in
      let shadows =
        parameters
        |> List.filter (fun parameter -> List.mem parameter.txt mentioned)
        |> List.map aliased_shadow
      in
      let shadows =
        match shadows with
        | [] ->
            [
              {
                pparam_loc = ghost_loc;
                pparam_desc =
                  Pparam_val (Nolabel, None, unit_pattern ~loc:ghost_loc);
              };
            ]
        | shadows -> shadows
      in
      let constraint_ =
        {
          mode_annotations = [];
          ret_mode_annotations = [];
          ret_type_constraint = None;
        }
      in
      let lifted =
        Exp.function_ ~loc:ghost_loc shadows constraint_
          (Pfunction_body sidecar_body)
      in
      let ignored =
        Exp.apply ~loc:ghost_loc
          (Exp.ident ~loc:ghost_loc
             { txt = Longident.Lident "ignore"; loc = ghost_loc })
          [ (Nolabel, lifted) ]
      in
      let sidecar =
        Exp.ifthenelse ~loc:ghost_loc
          (Exp.construct ~loc:ghost_loc
             { txt = Longident.Lident "false"; loc = ghost_loc }
             None)
          ignored (Some (Exp.construct ~loc:ghost_loc
                           { txt = Longident.Lident "()"; loc = ghost_loc }
                           None))
      in
      Some (marker, sidecar)
  in
  mapper
