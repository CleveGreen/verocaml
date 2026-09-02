open Typedtree
let carrier_attribute_name = "verocaml.internal.broadcast.carrier.v1"
let declaration_attribute_name = "verocaml.internal.broadcast.declaration.v1"
let scope_attribute_name = "verocaml.internal.broadcast.scope.v1"

type target = Broadcast_scope_private.target = {
  target_id : string;
  target_group : bool;
  target_path : string;
  target_uid : string;
  target_interface_uid : string option;
}
type group = {
  group_id : string;
  group_uid : string;
  group_interface_uid : string option;
  group_name : string;
  group_path : string;
  group_targets : target list;
  group_span : Diagnostic.span;
}

type expression_scope = {
  expression_scope_id : string;
  expression_scope_span : Diagnostic.span;
  expression_scope_targets : target list;
}

type declaration = {
  declaration_binding : value_binding;
  declaration_id : string;
  declaration_ident : Ident.t;
  declaration_uid : string;
  declaration_body_uid : string;
  declaration_interface_uid : string option;
  declaration_path : Path.t;
  declaration_triggers : Location.t list;
}

type carrier_kind = Group | Structure | Expression

type carrier = {
  carrier_binding : value_binding;
  carrier_kind : carrier_kind;
  carrier_id : string;
  carrier_name : string;
  carrier_targets : target list;
  carrier_span : Diagnostic.span;
}

type expression_wrapper = {
  wrapper_expression : expression;
  wrapper_body : expression;
  wrapper_scope : expression_scope;
}

type binding_scope = {
  scoped_binding : value_binding;
  scoped_targets : target list;
  scoped_expressions : expression_scope list;
}

type t = {
  declarations : declaration list;
  carriers : carrier list;
  groups : group list;
  binding_scopes : binding_scope list;
  wrappers : expression_wrapper list;
}

let empty =
  { declarations = []; carriers = []; groups = []; binding_scopes = []; wrappers = [] }

let append left right =
  {
    declarations = left.declarations @ right.declarations;
    carriers = left.carriers @ right.carriers;
    groups = left.groups @ right.groups;
    binding_scopes = left.binding_scopes @ right.binding_scopes;
    wrappers = left.wrappers @ right.wrappers;
  }

type error = { location : Location.t; message : string }

type carrier_metadata = {
  metadata_kind : carrier_kind;
  metadata_id : string;
  metadata_name : string;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let fail location message =
  [%log.debug "rejected typed broadcast carrier boundary"
    ~route:(Delator.Field.string "typedtree")
    ~stage:(Delator.Field.string "carrier-authentication")
    ~decision:(Delator.Field.string "rejected")
    ~reason_class:(Delator.Field.string "carrier-body-target-graph")];
  Error { location; message }

let offsets loc =
  (loc.Location.loc_start.Lexing.pos_cnum, loc.Location.loc_end.pos_cnum)

let internal_attribute name attributes =
  List.filter
    (fun attribute -> String.equal attribute.Parsetree.attr_name.txt name)
    attributes

let authenticated_attribute attribute =
  attribute.Parsetree.attr_loc.Location.loc_ghost
  && attribute.attr_name.loc.loc_ghost

let parse_offsets start_ stop =
  match (int_of_string_opt start_, int_of_string_opt stop) with
  | Some start_, Some stop when start_ >= 0 && stop >= start_ ->
      Some (start_, stop)
  | _ -> None

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid

let parse_declaration_attribute binding attribute =
  (*
    A retained declaration authenticates only the neutral declaration ID and
    source span.  In particular, no payload field can claim lemma, axiom,
    proved, or trusted status.  The declaration owner derives that status only
    after the corresponding SST body has been authenticated and completed.

    Keeping this parser role-blind also makes every old four-field carrier
    malformed rather than a compatibility spelling.  That distinction is
    security-relevant: malformed retained syntax reaches the same fail-closed
    authentication boundary as a stale ID or source span, before lowering can
    create semantic work or a solver.
    No legacy role field is accepted or ignored.
  *)
  if not (authenticated_attribute attribute) then
    fail attribute.attr_loc
      "broadcast declaration attribute is not retained ghost syntax"
  else
    match
      Option.map (String.split_on_char '|')
        (Broadcast_scope_private.carrier_payload attribute)
    with
    | Some [ id; start_; stop ] -> (
        match parse_offsets start_ stop with
        | None ->
            fail attribute.attr_loc "broadcast declaration span is invalid"
        | Some (payload_start, payload_stop) ->
            let attribute_start, attribute_stop = offsets attribute.attr_loc in
            if
              payload_start <> attribute_start
              || payload_stop <> attribute_stop
              || not
                   (String.equal id
                      (Broadcast_scope_private.carrier_stable_id "declaration"
                         binding.vb_loc ""))
            then
              fail attribute.attr_loc
                "broadcast declaration identity or span is stale or forged"
            else Ok id)
    | Some _ | None ->
        fail attribute.attr_loc "broadcast declaration payload is malformed"

let binding_ident binding =
  match binding.vb_pat.pat_desc with
  | Tpat_var (ident, name, uid, _, _) ->
      Some (ident, name.txt, compiler_uid uid)
  | _ -> None

let function_body expression =
  match expression.exp_desc with
  | Texp_function { body = Tfunction_body body; _ } -> Some body
  | _ -> None

let marker_call resolves_to_marker expression =
  match expression.exp_desc with
  | Texp_apply
      ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
        [
          ( Nolabel,
            Arg ({ exp_desc = Texp_constant (Const_string (text, _, _)); _ }, _)
          );
        ],
        _,
        _,
        _ )
    when resolves_to_marker path ->
      Some text
  | _ -> None

let carrier_body resolves_to_marker metadata binding =
  let expected =
    let kind =
      match metadata.metadata_kind with
      | Group -> "group"
      | Structure -> "structure"
      | Expression -> "expression"
    in
    Printf.sprintf "verocaml:broadcast:carrier:v1:%s:%s" kind
      metadata.metadata_id
  in
  match function_body binding.vb_expr with
  | Some { exp_desc = Texp_sequence (head, _, tail); _ } -> (
      match marker_call resolves_to_marker head with
      | Some text when String.equal text expected -> Ok tail
      | Some _ | None ->
          fail binding.vb_expr.exp_loc "broadcast carrier marker is malformed")
  | Some _ | None ->
      fail binding.vb_expr.exp_loc
        "broadcast carrier is not one canonical thunk"

type target_reference = {
  target_path : Path.t option;
  local_target_path : Path.t;
  target_uid : string;
}

type module_node = {
  module_path : Path.t;
  module_uid : string;
  module_expression : module_expr;
  module_structure : structure option;
}

type module_relation = {
  relation_path : Path.t;
  relation_target : Path.t;
  relation_public_type : Types.module_type;
}

type module_type_node = {
  module_type_path : Path.t;
  module_type_value : Types.module_type;
}

let _uid_correlation uid =
  "compiler-" ^ Digest.to_hex (Digest.string ("broadcast-uid-v1:" ^ uid))

let implementation_module_relations structure =
  let rec inner_ident constrained expression =
    match expression.mod_desc with
    | Tmod_ident (path, _) ->
        Some (constrained, path, expression.mod_type)
    | Tmod_constraint (inner, module_type, _, _) ->
        Option.map
          (fun (_, path, _) -> (true, path, module_type))
          (inner_ident true inner)
    | Tmod_structure _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_unpack _ -> None
  in
  let rec inner_structure expression =
    match expression.mod_desc with
    | Tmod_structure structure -> Some structure
    | Tmod_constraint (inner, _, _, _) -> inner_structure inner
    | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_unpack _ -> None
  in
  let rec collect parent nodes transparent projected structure =
    List.fold_left
      (fun (nodes, transparent, projected) item ->
        match item.str_desc with
        | Tstr_module binding -> (
            match binding.mb_id with
            | None -> (nodes, transparent, projected)
            | Some ident ->
                let path =
                  match parent with
                  | None -> Path.Pident ident
                  | Some parent -> Path.Pdot (parent, Ident.name ident)
                in
                let module_structure = inner_structure binding.mb_expr in
                let node =
                  {
                    module_path = path;
                    module_uid = compiler_uid binding.mb_uid;
                    module_expression = binding.mb_expr;
                    module_structure;
                  }
                in
                let transparent, projected =
                  match binding.mb_expr.mod_desc with
                  | Tmod_constraint
                      ({ mod_desc = Tmod_structure _; _ }, public_type, _, _) ->
                      [%log.trace "recognized constrained structure self projection"
                        ~route:(Delator.Field.string "typed-module-projection")
                        ~stage:(Delator.Field.string "module-relation-correlation")
                        ~member_kind:(Delator.Field.string "module")
                        ~correlation:
                          (Delator.Field.string (_uid_correlation node.module_uid))
                        ~decision:(Delator.Field.string "projected")];
                      ( transparent,
                        {
                          relation_path = path;
                          relation_target = path;
                          relation_public_type = public_type;
                        }
                        :: projected )
                  | _ -> (
                      match inner_ident false binding.mb_expr with
                      | Some (false, target, public_type) ->
                          ( {
                              relation_path = path;
                              relation_target = target;
                              relation_public_type = public_type;
                            }
                            :: transparent,
                            projected )
                      | Some (true, target, public_type) ->
                          ( transparent,
                            {
                              relation_path = path;
                              relation_target = target;
                              relation_public_type = public_type;
                            }
                            :: projected )
                      | None -> (transparent, projected))
                in
                Option.fold ~none:(node :: nodes, transparent, projected)
                  ~some:(collect (Some path) (node :: nodes) transparent projected)
                  module_structure)
        | Tstr_recmodule _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _
        | Tstr_typext _ | Tstr_exception _ | Tstr_modtype _ | Tstr_open _
        | Tstr_class _ | Tstr_class_type _ | Tstr_include _ | Tstr_eval _
        | Tstr_attribute _ ->
            (nodes, transparent, projected))
      (nodes, transparent, projected) structure.str_items
  in
  let nodes, transparent, projected = collect None [] [] [] structure in
  [%log.trace "collected compiler module identity relations"
    ~route:(Delator.Field.string "typed-module-identity")
    ~transparent_count:(Delator.Field.int (List.length transparent))
    ~projected_count:(Delator.Field.int (List.length projected))
    ~module_count:(Delator.Field.int (List.length nodes))
    ~decision:(Delator.Field.string "collected")];
  (nodes, transparent, projected)

let implementation_module_types nodes root =
  let rec collect parent found structure =
    List.fold_left
      (fun found item ->
        match item.str_desc with
        | Tstr_modtype declaration -> (
            match declaration.mtd_type with
            | None -> found
            | Some module_type ->
                let path =
                  match parent with
                  | None -> Path.Pident declaration.mtd_id
                  | Some parent ->
                      Path.Pdot (parent, Ident.name declaration.mtd_id)
                in
                {
                  module_type_path = path;
                  module_type_value = module_type.mty_type;
                }
                :: found)
        | Tstr_module binding -> (
            match binding.mb_id with
            | None -> found
            | Some ident ->
                let path =
                  match parent with
                  | None -> Path.Pident ident
                  | Some parent -> Path.Pdot (parent, Ident.name ident)
                in
                Option.fold ~none:found ~some:(collect (Some path) found)
                  (match
                     List.filter
                       (fun node -> Path.same node.module_path path)
                       nodes
                   with
                  | [ node ] -> node.module_structure
                  | [] | _ :: _ :: _ -> None))
        | Tstr_recmodule _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _
        | Tstr_typext _ | Tstr_exception _ | Tstr_open _ | Tstr_class _
        | Tstr_class_type _ | Tstr_include _ | Tstr_eval _ | Tstr_attribute _ ->
            found)
      found structure.str_items
  in
  collect None [] root

let normalize_local_module_path relations path =
  let rec normalize seen path =
    if List.exists (Path.same path) seen then path
    else
      match
        List.find_opt
          (fun relation -> Path.same relation.relation_path path)
          relations
      with
      | Some relation ->
          [%log.trace "advanced transparent compiler module identity fixpoint"
            ~route:(Delator.Field.string "typed-module-identity")
            ~stage:(Delator.Field.string "transparent-alias-fixpoint")
            ~decision:(Delator.Field.string "continue")];
          normalize (path :: seen) relation.relation_target
      | None ->
      match path with
      | Path.Pdot (parent, name) ->
          let parent = normalize (path :: seen) parent in
          Path.Pdot (parent, name)
      | Pident _ | Papply _ | Pextra_ty _ -> path
  in
  normalize [] path

let compiler_dependency_graph implementation =
  implementation.Cmt_input.metadata.Cmt_format.cmt_declaration_dependencies
  |> List.map (fun (_, left, right) -> (compiler_uid left, compiler_uid right))

let compiler_receipts_correlate graph left right =
  let rec visit seen = function
    | [] -> false
    | current :: _ when String.equal current right -> true
    | current :: rest when List.mem current seen -> visit seen rest
    | current :: rest ->
        let adjacent =
          graph
          |> List.filter_map (fun (edge_left, edge_right) ->
                 if String.equal edge_left current then Some edge_right
                 else if String.equal edge_right current then Some edge_left
                 else None)
        in
        visit (current :: seen) (rest @ adjacent)
  in
  String.equal left right || visit [] [ left ]

let path_components path =
  let rec collect prefixes = function
    | Path.Pident ident ->
        Some ((Path.Pident ident, Ident.name ident) :: prefixes)
    | Pdot (parent, name) ->
        collect ((Path.Pdot (parent, name), name) :: prefixes) parent
    | Papply _ | Pextra_ty _ -> None
  in
  collect [] path

let shape_component shape kind name =
  match (Shape.strip_head_aliases shape).Shape.desc with
  | Shape.Struct components ->
      Shape.Item.Map.find_opt (Shape.Item.make name kind) components
  | Shape.Var _ | Abs _ | App _ | Alias _ | Leaf | Proj _ | Comp_unit _
  | Error _ | Constr _ | Tuple _ | Unboxed_tuple _ | Predef _ | Arrow
  | Poly_variant _ | Mu _ | Rec_var _ | Variant _ | Variant_unboxed _
  | Record _ | Mutrec _ | Proj_decl _ ->
      None

let unique_module_node nodes path =
  match List.filter (fun node -> Path.same node.module_path path) nodes with
  | [ node ] -> Some node
  | [] | _ :: _ :: _ -> None

let exact_structure_path nodes structure =
  nodes
  |> List.filter_map (fun node ->
         match node.module_structure with
         | Some candidate when candidate == structure -> Some node.module_path
         | Some _ | None -> None)
  |> function [ path ] -> Some path | [] | _ :: _ :: _ -> None

let compiler_module_shapes root_shape =
  let rec collect shapes shape =
    let shapes =
      match shape.Shape.uid with
      | Some uid -> (compiler_uid uid, shape) :: shapes
      | None -> shapes
    in
    match shape.Shape.desc with
    | Shape.Alias nested -> collect shapes nested
    | Struct components ->
        Shape.Item.Map.fold (fun _ nested shapes -> collect shapes nested)
          components shapes
    | Var _ | Abs _ | App _ | Leaf | Proj _ | Comp_unit _ | Error _ | Constr _
    | Tuple _ | Unboxed_tuple _ | Predef _ | Arrow | Poly_variant _ | Mu _
    | Rec_var _ | Variant _ | Variant_unboxed _ | Record _ | Mutrec _
    | Proj_decl _ ->
        shapes
  in
  collect [] root_shape

let compiler_module_shape shapes nodes path =
  match unique_module_node nodes path with
  | None -> None
  | Some node -> (
      let candidates =
        List.filter_map
          (fun (uid, shape) ->
            if String.equal uid node.module_uid then Some shape else None)
          shapes
        |> List.fold_left
             (fun unique shape ->
               if List.exists (Shape.equal shape) unique then unique
               else shape :: unique)
             []
      in
      match candidates with
      | [ shape ] -> Some shape
      | [] | _ :: _ :: _ -> None)

let direct_alias_uid shape =
  match shape.Shape.desc with
  | Shape.Alias target -> Option.map compiler_uid target.Shape.uid
  | Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _ | Error _
  | Constr _ | Tuple _ | Unboxed_tuple _ | Predef _ | Arrow | Poly_variant _
  | Mu _ | Rec_var _ | Variant _ | Variant_unboxed _ | Record _ | Mutrec _
  | Proj_decl _ ->
      None

let module_type_signature ~module_types ~modules environment module_type =
  let rec immediate seen = function
    | Types.Mty_signature signature -> Some signature
    | Mty_strengthen (nested, _, _) -> immediate seen nested
    | Mty_ident path ->
        if List.exists (Path.same path) seen then None
        else
          module_types
          |> List.filter (fun node -> Path.same node.module_type_path path)
          |> (function
               | [ node ] -> immediate (path :: seen) node.module_type_value
               | [] | _ :: _ :: _ -> None)
    | Mty_alias path ->
        if List.exists (Path.same path) seen then None
        else
          modules
          |> List.filter (fun node -> Path.same node.module_path path)
          |> (function
               | [ node ] ->
                   immediate (path :: seen) node.module_expression.mod_type
               | [] | _ :: _ :: _ -> None)
    | Mty_functor _ -> None
  in
  match immediate [] module_type with
  | Some _ as signature -> signature
  | None -> (
      try immediate [] (Mtype.scrape environment module_type)
      with Env.Error _ -> None)

let reduced_shape_uid environment shape =
  let rec resolved = function
    | Shape_reduce.Resolved uid -> Some (compiler_uid uid)
    | Resolved_alias (_, nested) -> resolved nested
    | Unresolved _ | Approximated _ | Internal_error_missing_uid -> None
  in
  resolved (Shape_reduce.local_reduce_for_uid environment shape)

type projection_edge = {
  projected_id : string;
  projected_path : Path.t;
  occurrence_uid : string;
  resolved_uid : string;
}

let projection_edges_for_relation ~artifact ~nodes ~module_types relation =
  let implementation =
    artifact.Typedtree_adapter_issuance_private.proof_capture_implementation
  in
  let root_shape = implementation.Cmt_input.metadata.Cmt_format.cmt_impl_shape in
  let shapes = Option.map compiler_module_shapes root_shape in
  let target_node = unique_module_node nodes relation.relation_target in
  let source_shape =
    Option.bind shapes (fun shapes ->
        compiler_module_shape shapes nodes relation.relation_path)
  in
  let target_shape =
    Option.bind shapes (fun shapes ->
        compiler_module_shape shapes nodes relation.relation_target)
  in
  let target_exact =
    match (source_shape, target_node) with
    | Some source_shape, Some target ->
        (match direct_alias_uid source_shape with
        | Some uid -> String.equal uid target.module_uid
        | None -> true)
    | _ -> false
  in
  [%log.trace "evaluated exact compiler module projection endpoints"
    ~route:(Delator.Field.string "typed-module-projection")
    ~stage:(Delator.Field.string "module-endpoint-correlation")
    ~target_exact:(Delator.Field.bool target_exact)
    ~source_shape:(Delator.Field.bool (Option.is_some source_shape))
    ~target_shape:(Delator.Field.bool (Option.is_some target_shape))
    ~decision:
      (Delator.Field.string
         (if target_exact && Option.is_some source_shape
          then "correlated" else "rejected"))];
  match (target_exact, source_shape, target_shape) with
  | true, Some source_shape, _ -> (
      match
        module_type_signature ~module_types ~modules:nodes
          implementation.Cmt_input.structure.str_final_env
          relation.relation_public_type
      with
      | None ->
          [%log.debug "rejected unsupported compiler module projection form"
            ~route:(Delator.Field.string "typed-module-projection")
            ~stage:(Delator.Field.string "public-shape-enumeration")
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "unsupported-module-type")];
          []
      | Some signature ->
          let rec members prefix path shape signature =
            List.concat_map
              (function
                | Types.Sig_value (ident, description, _) -> (
                    let name = Ident.name ident in
                    match
                      shape_component shape Shape.Sig_component_kind.Value name
                    with
                    | None ->
                        [%log.debug "rejected missing compiler value projection"
                          ~route:(Delator.Field.string "typed-module-projection")
                          ~stage:(Delator.Field.string "member-shape-correlation")
                          ~member_kind:(Delator.Field.string "declaration")
                          ~decision:(Delator.Field.string "rejected")
                          ~reason_class:(Delator.Field.string "missing-value-shape")];
                        []
                    | Some value_shape -> (
                        match
                          reduced_shape_uid
                            implementation.Cmt_input.structure.str_final_env
                            value_shape
                        with
                        | None ->
                            [%log.debug "rejected unresolved compiler value projection"
                              ~route:(Delator.Field.string "typed-module-projection")
                              ~stage:(Delator.Field.string "member-shape-correlation")
                              ~member_kind:(Delator.Field.string "declaration")
                              ~decision:(Delator.Field.string "rejected")
                              ~reason_class:
                                (Delator.Field.string "unresolved-value-shape")];
                            []
                        | Some resolved_uid ->
                            let occurrence_uid =
                              compiler_uid description.Types.val_uid
                            in
                            [%log.trace "correlated compiler-projected value identity"
                              ~route:(Delator.Field.string "typed-module-projection")
                              ~stage:(Delator.Field.string "member-shape-correlation")
                              ~member_kind:(Delator.Field.string "declaration")
                              ~occurrence:
                                (Delator.Field.string
                                   (_uid_correlation occurrence_uid))
                              ~definition:
                                (Delator.Field.string
                                   (_uid_correlation resolved_uid))
                              ~decision:(Delator.Field.string "correlated")];
                            [
                              {
                                projected_id =
                                  "broadcast:"
                                  ^ String.concat "." (prefix @ [ name ]);
                                projected_path = Path.Pdot (path, name);
                                occurrence_uid;
                                resolved_uid;
                              };
                            ]))
                | Types.Sig_module (ident, _, declaration, _, _) -> (
                    let name = Ident.name ident in
                    match
                      ( shape_component shape Shape.Sig_component_kind.Module name,
                        module_type_signature ~module_types ~modules:nodes
                          implementation.Cmt_input.structure.str_final_env
                          declaration.Types.md_type )
                    with
                    | Some nested_shape, Some nested_signature ->
                        members (prefix @ [ name ]) (Path.Pdot (path, name))
                          nested_shape nested_signature
                    | _ -> [])
                | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
                | Types.Sig_class _ | Types.Sig_class_type _ ->
                    [])
              signature
          in
          let prefix =
            match path_components relation.relation_path with
            | Some components -> List.map snd components
            | None -> []
          in
          members prefix relation.relation_path source_shape signature)
  | false, _, _ | true, None, _ -> []

let collect_target_references artifact module_relations projection_edges
    resolves_to_marker expression =
  let references = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_ident (path, _, description, _, _)
            when not (resolves_to_marker path) ->
              let local_target_path =
                match
                  List.filter
                    (fun edge ->
                      String.equal edge.resolved_uid
                        (compiler_uid description.Types.val_uid))
                    projection_edges
                with
                | [ edge ] ->
                    [%log.trace "correlated local reference through public module projection"
                      ~route:(Delator.Field.string "typed-module-projection")
                      ~stage:(Delator.Field.string "reference-path-correlation")
                      ~member_kind:(Delator.Field.string "declaration")
                      ~occurrence:
                        (Delator.Field.string
                           (_uid_correlation edge.occurrence_uid))
                      ~definition:
                        (Delator.Field.string
                           (_uid_correlation edge.resolved_uid))
                      ~decision:(Delator.Field.string "correlated")];
                    edge.projected_path
                | [] | _ :: _ :: _ ->
                    normalize_local_module_path module_relations path
              in
              let path =
                match artifact with
                | Some artifact ->
                    Cmt_input.normalize_value_path
                      artifact.Typedtree_adapter_issuance_private.proof_capture_implementation
                      expression.exp_loc expression.exp_env path
                | None -> None
              in
              (match path with
              | Some _ -> ()
              | None ->
                  [%log.debug "rejected unnormalized typed broadcast target path"
                    ~route:(Delator.Field.string "typed-reference")
                    ~stage:(Delator.Field.string "compiler-normalization")
                    ~decision:(Delator.Field.string "rejected")
                    ~reason_class:
                      (Delator.Field.string "compiler-normalization-failure")]);
              let target_uid = compiler_uid description.Types.val_uid in
              [%log.trace "collected broadcast target reference"
                ~route:(Delator.Field.string "typed-reference")
                ~member_kind:(Delator.Field.string "unresolved")
                ~decision:(Delator.Field.string "collected")];
              references :=
                {
                  target_path = path;
                  local_target_path;
                  target_uid;
                }
                :: !references
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  List.rev !references

let trigger_locations ~artifact ~source_file ~resolves_to_marker binding =
  let found = ref [] in
  let failure = ref None in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          match
            Typedtree_logical_builtin_private.authenticate ~artifact
              ~source_file ~canonical_marker:resolves_to_marker expression
          with
          | Error message -> failure := Some (expression.exp_loc, message)
          | Ok (Some logical)
            when match Typedtree_logical_builtin_private.kind logical with
                 | Forall | Exists -> true
                 | Call_requires | Call_ensures -> false ->
              ()
          | Ok (Some _) | Ok None ->
              List.iter
                (fun attribute ->
                  if String.equal attribute.Parsetree.attr_name.txt "trigger"
                  then
                    match attribute.attr_payload with
                    | PStr [] -> found := expression.exp_loc :: !found
                    | _ ->
                        failure :=
                          Some
                            (attribute.attr_loc, "trigger payload must be empty"))
                expression.exp_attributes;
              default.expr self expression);
    }
  in
  iterator.expr iterator binding.vb_expr;
  match !failure with
  | Some (location, message) -> fail location message
  | None -> Ok (List.rev !found)

let target_for_reference ~artifact declarations groups resolve_imported_target
    location reference =
  let* path =
    match reference.target_path with
    | Some path -> Ok path
    | None ->
        fail location
          "broadcast target path cannot be compiler-normalized"
  in
  let imported () =
    match resolve_imported_target path reference.target_uid with
    | Some target ->
        [%log.trace "resolved imported broadcast target"
          ~route:(Delator.Field.string "imported")
          ~member_kind:
            (Delator.Field.string
               (if target.target_group then "group" else "declaration"))
          ~decision:(Delator.Field.string "correlated")];
        Ok target
    | None ->
        fail location
          "broadcast target is not an authenticated local or imported declaration or group"
  in
  let dependency_graph =
    match artifact with
    | None -> []
    | Some artifact ->
        compiler_dependency_graph
          artifact.Typedtree_adapter_issuance_private.proof_capture_implementation
  in
  let _declaration_uid_candidates =
    List.filter
      (fun declaration ->
        String.equal declaration.declaration_uid reference.target_uid)
      declarations
  in
  let _declaration_path_candidates =
    List.filter
      (fun declaration ->
        Path.same declaration.declaration_path reference.local_target_path)
      declarations
  in
  let declaration_candidates =
    List.filter
      (fun declaration ->
        Path.same declaration.declaration_path reference.local_target_path
        &&
        (String.equal declaration.declaration_uid reference.target_uid
        || compiler_receipts_correlate dependency_graph
             declaration.declaration_uid reference.target_uid
        || compiler_receipts_correlate dependency_graph
             declaration.declaration_body_uid reference.target_uid))
      declarations
  in
  let group_candidates =
    List.filter
      (fun (_, _, _, uid, compiler_path, _, _) ->
        Path.same compiler_path reference.local_target_path
        &&
        (String.equal uid reference.target_uid
        || compiler_receipts_correlate dependency_graph uid reference.target_uid))
      groups
  in
  [%log.trace "evaluated exact local broadcast compiler identity"
    ~route:(Delator.Field.string "local-compiler-identity")
    ~stage:(Delator.Field.string "target-correlation")
    ~member_kind:(Delator.Field.string "unresolved")
    ~declaration_candidates:
      (Delator.Field.int (List.length declaration_candidates))
    ~uid_candidates:
      (Delator.Field.int (List.length _declaration_uid_candidates))
    ~path_candidates:
      (Delator.Field.int (List.length _declaration_path_candidates))
    ~dependency_edges:(Delator.Field.int (List.length dependency_graph))
    ~group_candidates:(Delator.Field.int (List.length group_candidates))
    ~correlation:
      (Delator.Field.string (_uid_correlation reference.target_uid))
    ~decision:
      (Delator.Field.string
         (match (declaration_candidates, group_candidates) with
         | [ _ ], [] | [], [ _ ] -> "correlated"
         | [], [] -> "unresolved"
         | _ -> "ambiguous"))];
  match (declaration_candidates, group_candidates) with
  | [ declaration ], [] ->
      [%log.trace "resolved compiler-identified local broadcast target"
        ~route:(Delator.Field.string "local-compiler-identity")
        ~stage:(Delator.Field.string "target-correlation")
        ~member_kind:(Delator.Field.string "declaration")
        ~correlation:
          (Delator.Field.string (_uid_correlation declaration.declaration_uid))
        ~decision:(Delator.Field.string "correlated")];
      Ok
        {
          target_id = declaration.declaration_id;
          target_group = false;
          target_path = Path.name path;
          target_uid = declaration.declaration_uid;
          target_interface_uid = declaration.declaration_interface_uid;
        }
  | [], [ (_, _, id, uid, _, interface_uid, _) ] ->
      [%log.trace "resolved compiler-identified local broadcast target"
        ~route:(Delator.Field.string "local-compiler-identity")
        ~stage:(Delator.Field.string "target-correlation")
        ~member_kind:(Delator.Field.string "group")
        ~correlation:(Delator.Field.string (_uid_correlation uid))
        ~decision:(Delator.Field.string "correlated")];
      Ok
        {
          target_id = id;
          target_group = true;
          target_path = Path.name path;
          target_uid = uid;
          target_interface_uid = interface_uid;
        }
  | [], [] -> imported ()
  | _ ->
      [%log.debug "rejected ambiguous local broadcast compiler identity"
        ~route:(Delator.Field.string "local-compiler-identity")
        ~stage:(Delator.Field.string "target-correlation")
      ~member_kind:(Delator.Field.string "declaration")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "ambiguous-full-compiler-identity")];
      fail location "broadcast target has ambiguous local compiler identity"

let targets_for_carrier declarations groups resolve_imported_target
    artifact module_relations projection_edges resolves_to_marker metadata
    binding =
  let* body = carrier_body resolves_to_marker metadata binding in
  collect_target_references artifact module_relations projection_edges
    resolves_to_marker body
  |> List.fold_left
       (fun result reference ->
         let* targets = result in
         let* target =
           target_for_reference ~artifact declarations groups
             resolve_imported_target binding.vb_loc reference
         in
         Ok (target :: targets))
       (Ok [])
  |> Result.map List.rev

let declaration_of_binding ~artifact ~source_file ~resolves_to_marker
    ~structure_path ~structure_compiler_path binding =
  match internal_attribute declaration_attribute_name binding.vb_attributes with
  | [] -> Ok None
  | [ attribute ] -> (
      let* _carrier_id = parse_declaration_attribute binding attribute in
      match binding_ident binding with
      | None ->
          fail binding.vb_loc
            "broadcast declaration must bind one named function"
      | Some (ident, name, uid) ->
          let* triggers =
            trigger_locations ~artifact ~source_file ~resolves_to_marker binding
          in
          Ok
            (Some
               {
                 declaration_binding = binding;
                 declaration_id =
                   "broadcast:"
                   ^ String.concat "." (structure_path @ [ name ]);
                 declaration_ident = ident;
                 declaration_uid = uid;
                 declaration_body_uid = uid;
                 declaration_interface_uid = None;
                 declaration_path =
                   (match structure_compiler_path with
                   | None -> Path.Pident ident
                   | Some path -> Path.Pdot (path, name));
                 declaration_triggers = triggers;
               }))
  | _ -> fail binding.vb_loc "duplicate broadcast declaration carrier"

let carrier_metadata binding =
  match internal_attribute carrier_attribute_name binding.vb_attributes with
  | [] -> Ok None
  | [ attribute ] -> (
      match
        Broadcast_scope_private.authenticate_carrier_attribute binding
          attribute
      with
      | Error (location, message) -> fail location message
      | Ok metadata ->
          let metadata_kind =
            match metadata.Broadcast_scope_private.carrier_kind with
            | Group_carrier -> Group
            | Structure_carrier -> Structure
            | Expression_carrier -> Expression
          in
          Ok
            (Some
               {
                 metadata_kind;
                 metadata_id = metadata.carrier_id;
                 metadata_name = metadata.carrier_name;
               }))
  | _ -> fail binding.vb_loc "duplicate broadcast carrier"

let activation_wrapper ~source_file carriers expression =
  match expression.exp_desc with
  | Texp_let (Nonrecursive, [ binding ], body) -> (
      match
        List.find_opt
          (fun carrier -> carrier.carrier_binding == binding)
          carriers
      with
      | Some { carrier_kind = Expression; carrier_id; carrier_targets; _ } -> (
          match internal_attribute scope_attribute_name body.exp_attributes with
          | [ attribute ] when authenticated_attribute attribute -> (
              match
                Option.map (String.split_on_char '|')
                  (Broadcast_scope_private.carrier_payload attribute)
              with
              | Some [ scope_id; start_; stop ]
                when String.equal scope_id carrier_id
                     && parse_offsets start_ stop = Some (offsets attribute.attr_loc) ->
                  Some
                    {
                      wrapper_expression = expression;
                      wrapper_body = body;
                      wrapper_scope =
                        {
                          expression_scope_id = scope_id;
                          expression_scope_span =
                            Diagnostic.span_of_location
                              ~fallback_file:source_file body.exp_loc;
                          expression_scope_targets = carrier_targets;
                        };
                    }
              | Some _ | None -> None)
          | [] | _ -> None)
      | Some _ | None -> None)
  | _ -> None

let expression_wrappers ~source_file carriers binding =
  let wrappers = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          match activation_wrapper ~source_file carriers expression
          with
          | Some wrapper ->
              wrappers := wrapper :: !wrappers;
              self.expr self wrapper.wrapper_body
          | None -> default.expr self expression);
    }
  in
  iterator.expr iterator binding.vb_expr;
  List.rev !wrappers

let interface_group_uid artifact group_path group_uid =
  match artifact with
  | None -> Ok None
  | Some artifact ->
      let implementation =
        artifact.Typedtree_adapter_issuance_private.proof_capture_implementation
      in
      let candidates =
        implementation.Cmt_input.interface_broadcasts
        |> List.filter_map (fun member ->
               let identity = member.Retained_broadcast_private.identity in
               if
                 identity.kind = Retained_broadcast_private.Group
                 && String.equal identity.canonical_path
                      (implementation.unit_name ^ "." ^ group_path)
               then Some identity
               else None)
      in
      (match candidates with
      | [] -> Ok None
      | [ identity ] ->
          let correlated =
            compiler_receipts_correlate
              (compiler_dependency_graph implementation)
              group_uid identity.compiler_uid
          in
          [%log.trace "evaluated implementation/interface group UID bridge"
            ~route:(Delator.Field.string "typed-group-identity")
            ~stage:(Delator.Field.string "interface-uid-correlation")
            ~member_kind:(Delator.Field.string "group")
            ~correlation:
              (Delator.Field.string
                 (Retained_broadcast_private.correlation identity))
            ~decision:
              (Delator.Field.string
                 (if correlated then "correlated" else "rejected"))];
          if correlated then Ok (Some identity.compiler_uid)
          else
            Error
              {
                location = Location.none;
                message =
                  "broadcast group interface UID lacks exact compiler correlation";
              }
      | _ :: _ :: _ ->
          Error
            {
              location = Location.none;
              message = "broadcast group interface identity is ambiguous";
            })

let authenticate_carriers ~source_file ~structure_path ~resolves_to_marker
    ~artifact ~structure_compiler_path
    ~module_relations ~projection_edges
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    ~top_level ~bindings ~available_declarations declarations =
  let* group_seeds =
    List.fold_left
      (fun result binding ->
        let* groups = result in
        let* metadata = carrier_metadata binding in
        match (metadata, binding_ident binding) with
        | Some ({ metadata_kind = Group; _ } as metadata), Some (ident, name, uid)
          when top_level binding && String.equal name metadata.metadata_name ->
            let compiler_path =
              match structure_compiler_path with
              | None -> Path.Pident ident
              | Some path -> Path.Pdot (path, name)
            in
            let group_path = String.concat "." (structure_path @ [ name ]) in
            let* interface_uid = interface_group_uid artifact group_path uid in
            Ok
              (( ident,
                 name,
                 metadata.metadata_id,
                 uid,
                 compiler_path,
                 interface_uid,
                 binding )
              :: groups)
        | Some { metadata_kind = Group; _ }, _ ->
            fail binding.vb_loc "broadcast group carrier has the wrong binder"
        | (Some _ | None), _ -> Ok groups)
      (Ok []) bindings
    |> Result.map List.rev
  in
  let* () =
    match
      List.find_opt
        (fun declaration ->
          List.exists
            (fun (_, name, _, _, _, _, _) ->
              String.equal name (Ident.name declaration.declaration_ident))
            group_seeds)
        declarations
    with
    | None -> Ok ()
    | Some declaration ->
        fail declaration.declaration_binding.vb_loc
          "broadcast group collides with a declaration"
  in
  let* carriers =
    List.fold_left
      (fun result binding ->
        let* carriers = result in
        let* metadata = carrier_metadata binding in
        match metadata with
        | None -> Ok carriers
        | Some metadata ->
            let* () =
              if metadata.metadata_kind <> Expression && not (top_level binding)
              then
                fail binding.vb_loc
                  "broadcast group or structure carrier is not direct"
              else Ok ()
            in
            let* targets =
              targets_for_carrier available_declarations group_seeds
                resolve_imported_target artifact module_relations projection_edges
                resolves_to_marker metadata binding
            in
            Ok
              ({
                 carrier_binding = binding;
                 carrier_kind = metadata.metadata_kind;
                 carrier_id = metadata.metadata_id;
                 carrier_name = metadata.metadata_name;
                 carrier_targets = targets;
                 carrier_span =
                   Diagnostic.span_of_location ~fallback_file:source_file
                     binding.vb_loc;
               }
              :: carriers))
      (Ok []) bindings
    |> Result.map List.rev
  in
  let groups =
    List.filter_map
      (fun carrier ->
        match carrier.carrier_kind with
        | Group ->
            let group_uid, group_interface_uid =
              match
                List.find_opt
                  (fun (_, _, _, _, _, _, binding) ->
                    binding == carrier.carrier_binding)
                  group_seeds
              with
              | Some (_, _, _, uid, _, interface_uid, _) ->
                  (uid, interface_uid)
              | None -> ("", None)
            in
            Some
              {
                group_id = carrier.carrier_id;
                group_uid;
                group_interface_uid;
                group_name = carrier.carrier_name;
                group_path =
                  String.concat "." (structure_path @ [ carrier.carrier_name ]);
                group_targets = carrier.carrier_targets;
                group_span = carrier.carrier_span;
              }
        | Structure | Expression -> None)
      carriers
  in
  let* () =
    Broadcast_scope_private.validate_target_graph
      ~declaration_ids:
        (List.map
           (fun declaration -> declaration.declaration_id)
           available_declarations
        @ imported_declaration_ids)
      ~groups:
        (List.map
           (fun group ->
             (group.group_id, group.group_name, group.group_targets))
           groups
        @ List.map (fun id -> (id, id, [])) imported_group_ids)
    |> Result.map_error (fun message ->
           { location = Location.none; message })
  in
  Ok (carriers, groups)

let declarations_for_structure ~source_file ~artifact ~resolves_to_marker ~nodes
    (inventory : Broadcast_scope_private.typedtree_structure) =
  let bindings = inventory.bindings in
  let top_level binding =
    List.exists (fun candidate -> candidate == binding)
      inventory.direct_bindings
  in
  let structure_compiler_path = exact_structure_path nodes inventory.structure in
  List.fold_left
      (fun result binding ->
        let* declarations = result in
        let* () =
          if
            (not (top_level binding))
            && internal_attribute declaration_attribute_name
                 binding.vb_attributes
               <> []
          then
            fail binding.vb_loc
              "broadcast declaration must be a direct structure binding"
          else Ok ()
        in
        let* declaration =
          declaration_of_binding ~artifact ~source_file ~resolves_to_marker
            ~structure_path:inventory.structure_path ~structure_compiler_path
            binding
        in
        Ok
          (Option.fold ~none:declarations
             ~some:(fun value -> value :: declarations)
             declaration))
      (Ok []) bindings
  |> Result.map List.rev

let authenticate_structure ~source_file ~artifact ~resolves_to_marker
    ~nodes ~module_relations ~projection_edges
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    ~available_declarations
    (inventory : Broadcast_scope_private.typedtree_structure) =
  let bindings = inventory.bindings in
  let top_level binding =
    List.exists (fun candidate -> candidate == binding)
      inventory.direct_bindings
  in
  let* declarations =
    declarations_for_structure ~source_file ~artifact ~resolves_to_marker
      ~nodes inventory
  in
  let* carriers, groups =
    authenticate_carriers ~source_file ~resolves_to_marker
      ~artifact
      ~structure_compiler_path:(exact_structure_path nodes inventory.structure)
      ~module_relations ~projection_edges
      ~structure_path:inventory.structure_path
      ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
      ~top_level ~bindings ~available_declarations declarations
  in
  let active = ref [] in
  let binding_scopes = ref [] in
  let wrappers = ref [] in
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_value (_, item_bindings) ->
          List.iter
            (fun binding ->
              match
                List.find_opt
                  (fun carrier -> carrier.carrier_binding == binding)
                  carriers
              with
              | Some { carrier_kind = Structure; carrier_targets; _ } ->
                  active :=
                    Broadcast_scope_private.merge_targets !active carrier_targets
              | Some { carrier_kind = Group | Expression; _ } -> ()
              | None ->
                  let local_wrappers =
                    expression_wrappers ~source_file carriers binding
                  in
                  wrappers := !wrappers @ local_wrappers;
                  binding_scopes :=
                    {
                      scoped_binding = binding;
                      scoped_targets = !active;
                      scoped_expressions =
                        List.map
                          (fun wrapper -> wrapper.wrapper_scope)
                          local_wrappers;
                    }
                    :: !binding_scopes)
            item_bindings
      | _ -> ())
    inventory.structure.str_items;
  let expression_carriers =
    List.filter (fun carrier -> carrier.carrier_kind = Expression) carriers
  in
  let* () =
    if
      List.length carriers <> inventory.carrier_attributes
      || List.length declarations <> inventory.declaration_attributes
      || List.length !wrappers <> inventory.scope_attributes
      || List.length !wrappers <> List.length expression_carriers
    then fail Location.none "broadcast carrier or lexical scope is unconsumed"
    else Ok ()
  in
  Ok
    {
      declarations;
      carriers;
      groups;
      binding_scopes = List.rev !binding_scopes;
      wrappers = !wrappers;
    }

let canonical_projection_edges edges =
  let sorted =
    List.sort
      (fun left right ->
        match String.compare left.projected_id right.projected_id with
        | 0 -> String.compare left.occurrence_uid right.occurrence_uid
        | comparison -> comparison)
      edges
  in
  let rec collect accepted = function
    | [] -> Ok (List.rev accepted)
    | edge :: rest ->
        let same, rest =
          List.partition
            (fun candidate ->
              String.equal candidate.projected_id edge.projected_id)
            rest
        in
        if
          List.for_all
            (fun candidate ->
              String.equal candidate.occurrence_uid edge.occurrence_uid
              && String.equal candidate.resolved_uid edge.resolved_uid
              && Path.same candidate.projected_path edge.projected_path)
            same
        then collect (edge :: accepted) rest
        else (
          [%log.debug "rejected colliding compiler projection identities"
            ~route:(Delator.Field.string "typed-module-projection")
            ~stage:(Delator.Field.string "projection-map-canonicalization")
            ~candidate_count:(Delator.Field.int (List.length same + 1))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "destination-identity-collision")];
          Error
            {
              location = Location.none;
              message = "broadcast module projection has colliding compiler identities";
            })
  in
  collect [] sorted

let project_declaration_fixpoint declarations edges =
  let rec unique_destinations = function
    | [] -> Ok ()
    | declaration :: rest ->
        let collisions =
          List.filter
            (fun candidate ->
              String.equal candidate.declaration_id declaration.declaration_id)
            rest
        in
        if collisions = [] then unique_destinations rest
        else (
          [%log.debug "rejected duplicate implementation declaration identities"
            ~route:(Delator.Field.string "typed-module-projection")
            ~stage:(Delator.Field.string "projection-map-canonicalization")
            ~candidate_count:(Delator.Field.int (List.length collisions + 1))
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "implementation-id-collision")];
          Error
            {
              location = Location.none;
              message = "broadcast implementation declaration identity is ambiguous";
            })
  in
  let source_for edge declarations =
    declarations
    |> List.filter (fun declaration ->
           String.equal declaration.declaration_uid edge.resolved_uid
           || String.equal declaration.declaration_body_uid edge.resolved_uid)
    |> function
    | [] -> Ok None
    | source :: rest
      when List.for_all
             (fun candidate ->
               candidate.declaration_binding == source.declaration_binding
               && String.equal candidate.declaration_body_uid
                    source.declaration_body_uid)
             rest ->
        Ok (Some source)
    | _ ->
        Error
          {
            location = Location.none;
            message =
              "broadcast module projection has ambiguous implementation definitions";
          }
  in
  let add_edge declarations edge =
    let* source = source_for edge declarations in
    match source with
    | None -> Ok (declarations, false)
    | Some source -> (
        let destinations =
          List.filter
            (fun declaration ->
              String.equal declaration.declaration_id edge.projected_id)
            declarations
        in
        match destinations with
        | [] ->
            [%log.trace "advanced compiler identity projection fixpoint"
              ~route:(Delator.Field.string "typed-module-projection")
              ~stage:(Delator.Field.string "projection-fixpoint")
              ~occurrence:
                (Delator.Field.string (_uid_correlation edge.occurrence_uid))
              ~definition:
                (Delator.Field.string
                   (_uid_correlation source.declaration_body_uid))
              ~decision:(Delator.Field.string "projected")];
            Ok
              ( declarations
                @ [
                    {
                      source with
                      declaration_id = edge.projected_id;
                      declaration_uid = edge.occurrence_uid;
                      declaration_interface_uid = None;
                      declaration_path = edge.projected_path;
                    };
                  ],
                true )
        | [ destination ]
          when destination.declaration_binding == source.declaration_binding
               && String.equal destination.declaration_body_uid
                    source.declaration_body_uid
               && String.equal destination.declaration_id edge.projected_id
               && Path.same destination.declaration_path edge.projected_path
               && (String.equal destination.declaration_uid edge.resolved_uid
                  || String.equal destination.declaration_uid
                       destination.declaration_body_uid) ->
            let rekeyed =
              not (String.equal destination.declaration_uid edge.occurrence_uid)
            in
            [%log.trace "correlated constrained structure public declaration identity"
              ~route:(Delator.Field.string "typed-module-projection")
              ~stage:(Delator.Field.string "projection-fixpoint")
              ~member_kind:(Delator.Field.string "declaration")
              ~occurrence:
                (Delator.Field.string (_uid_correlation edge.occurrence_uid))
              ~definition:
                (Delator.Field.string
                   (_uid_correlation destination.declaration_body_uid))
              ~decision:
                (Delator.Field.string (if rekeyed then "rekeyed" else "stable"))];
            Ok
              ( List.map
                  (fun declaration ->
                    if declaration == destination then
                      { declaration with declaration_uid = edge.occurrence_uid }
                    else declaration)
                  declarations,
                rekeyed )
        | [ destination ]
          when destination.declaration_binding == source.declaration_binding
               && String.equal destination.declaration_uid edge.occurrence_uid
               && String.equal destination.declaration_body_uid
                    source.declaration_body_uid
               && Path.same destination.declaration_path edge.projected_path ->
            Ok (declarations, false)
        | _ ->
            [%log.debug "rejected ambiguous projected declaration destination"
              ~route:(Delator.Field.string "typed-module-projection")
              ~stage:(Delator.Field.string "projection-fixpoint")
              ~candidate_count:(Delator.Field.int (List.length destinations))
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "projected-id-collision")];
            Error
              {
                location = Location.none;
                message = "broadcast module projection collides with a declaration";
              })
  in
  let rec iterate iteration declarations =
    let* declarations, changed =
      List.fold_left
        (fun result edge ->
          let* declarations, changed = result in
          let* declarations, edge_changed = add_edge declarations edge in
          Ok (declarations, changed || edge_changed))
        (Ok (declarations, false)) edges
    in
    [%log.trace "completed compiler identity projection fixpoint iteration"
      ~route:(Delator.Field.string "typed-module-projection")
      ~stage:(Delator.Field.string "projection-fixpoint")
      ~iteration:(Delator.Field.int iteration)
      ~edge_count:(Delator.Field.int (List.length edges))
      ~declaration_count:(Delator.Field.int (List.length declarations))
      ~changed:(Delator.Field.bool changed)
      ~decision:(Delator.Field.string (if changed then "continue" else "stable"))];
    if changed then iterate (iteration + 1) declarations else Ok declarations
  in
  let* () = unique_destinations declarations in
  iterate 0 declarations

let bridge_interface_declarations artifact declarations =
  let implementation =
    artifact.Typedtree_adapter_issuance_private.proof_capture_implementation
  in
  let graph = compiler_dependency_graph implementation in
  let interface_declarations =
    implementation.Cmt_input.interface_broadcasts
    |> List.filter (fun member ->
           member.Retained_broadcast_private.identity.kind
           = Retained_broadcast_private.Declaration)
  in
  let prefix = implementation.unit_name ^ "." in
  let bridge declaration =
    let relative =
      let broadcast_prefix = "broadcast:" in
      String.sub declaration.declaration_id (String.length broadcast_prefix)
        (String.length declaration.declaration_id
        - String.length broadcast_prefix)
    in
    let candidates =
      List.filter
        (fun member ->
          String.equal member.Retained_broadcast_private.identity.canonical_path
            (prefix ^ relative))
        interface_declarations
    in
    match candidates with
    | [] -> Ok declaration
    | [ member ] ->
        let identity = member.Retained_broadcast_private.identity in
        let occurrence_matches =
          compiler_receipts_correlate graph declaration.declaration_uid
            identity.compiler_uid
        and definition_matches =
          compiler_receipts_correlate graph declaration.declaration_body_uid
            identity.compiler_uid
        in
        [%log.trace "evaluated implementation/interface compiler UID bridge"
          ~route:(Delator.Field.string "typed-module-projection")
          ~stage:(Delator.Field.string "interface-uid-correlation")
          ~occurrence_matches:(Delator.Field.bool occurrence_matches)
          ~definition_matches:(Delator.Field.bool definition_matches)
          ~dependency_edges:(Delator.Field.int (List.length graph))
          ~correlation:
            (Delator.Field.string
               (Retained_broadcast_private.correlation identity))
          ~decision:
            (Delator.Field.string
               (if occurrence_matches || definition_matches then "correlated"
                else "rejected"))];
        if occurrence_matches || definition_matches then
          Ok
            {
              declaration with
              declaration_interface_uid = Some identity.compiler_uid;
            }
        else
          Error
            {
              location = Location.none;
              message =
                "broadcast declaration interface UID lacks exact compiler correlation";
            }
    | _ :: _ :: _ ->
        Error
          {
            location = Location.none;
            message = "broadcast declaration interface identity is ambiguous";
          }
  in
  let* declarations =
    List.fold_left
      (fun result declaration ->
        let* bridged = result in
        let* declaration = bridge declaration in
        Ok (declaration :: bridged))
      (Ok []) declarations
    |> Result.map List.rev
  in
  let* () =
    List.fold_left
      (fun result member ->
        let* () = result in
        let identity = member.Retained_broadcast_private.identity in
        let declaration_id =
          "broadcast:"
          ^ String.sub identity.canonical_path (String.length prefix)
              (String.length identity.canonical_path - String.length prefix)
        in
        let matches =
          List.filter
            (fun declaration ->
              declaration.declaration_interface_uid
              = Some identity.compiler_uid
              && String.equal declaration.declaration_id declaration_id)
            declarations
        in
        match matches with
        | [ _ ] -> Ok ()
        | [] ->
            Error
              {
                location = Location.none;
                message =
                  "retained broadcast interface declaration has no exact implementation projection";
              }
        | _ :: _ :: _ ->
            Error
              {
                location = Location.none;
                message =
                  "retained broadcast interface declaration has ambiguous implementation projections";
              })
      (Ok ()) interface_declarations
  in
  Ok declarations

let authenticate_internal ~source_file ~artifact ~resolves_to_marker
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    structure =
  let nodes, module_relations, projected_relations =
    implementation_module_relations structure
  in
  let module_types = implementation_module_types nodes structure in
  let inventory = Broadcast_scope_private.typedtree_inventory structure in
  match Broadcast_scope_private.typedtree_syntax inventory with
  | Absent -> Ok empty
  | Raw ->
      fail Location.none
        "raw broadcast syntax was not rewritten by the authenticated PPX"
  | Retained ->
      let* () =
        match artifact with
        | Some artifact
          when Typedtree_adapter_issuance_private
               .authenticate_logical_builtin_artifact artifact ~source_file
               && artifact.proof_capture_implementation.Cmt_input.structure
                  == structure ->
            Ok ()
        | Some _ | None ->
            fail Location.none
              "broadcast syntax requires the exact retained source/CMT artifact"
      in
      let artifact =
        match artifact with
        | Some artifact -> artifact
        | None -> assert false
      in
      let* available_declarations =
        List.fold_left
          (fun result structure_inventory ->
            let* declarations = result in
            let* local =
              declarations_for_structure ~source_file ~artifact:(Some artifact)
                ~resolves_to_marker ~nodes structure_inventory
            in
            Ok (List.rev_append local declarations))
          (Ok []) inventory.structures
        |> Result.map List.rev
      in
      let projection_edges =
        projected_relations
        |> List.concat_map
             (projection_edges_for_relation ~artifact ~nodes ~module_types)
      in
      let* projection_edges = canonical_projection_edges projection_edges in
      let* available_declarations =
        project_declaration_fixpoint available_declarations projection_edges
      in
      let* available_declarations =
        bridge_interface_declarations artifact available_declarations
      in
      [%log.trace "projected compiler-backed local broadcast declarations"
        ~route:(Delator.Field.string "typed-module-projection")
        ~projection_count:(Delator.Field.int (List.length projected_relations))
        ~edge_count:(Delator.Field.int (List.length projection_edges))
        ~declaration_count:
          (Delator.Field.int (List.length available_declarations))
        ~decision:(Delator.Field.string "correlated")];
      List.fold_left
        (fun result structure_inventory ->
          let* combined = result in
          let* local =
            authenticate_structure ~source_file ~artifact:(Some artifact)
              ~resolves_to_marker
              ~nodes ~module_relations ~projection_edges
              ~resolve_imported_target ~imported_declaration_ids
              ~imported_group_ids ~available_declarations structure_inventory
          in
          Ok (append combined local))
        (Ok empty)
        inventory.structures
      |> Result.map (fun scan ->
             { scan with declarations = available_declarations })

let authenticate ~source_file:(source_file [@delator.skip])
    ~artifact:(artifact [@delator.skip])
    ~resolves_to_marker:(resolves_to_marker [@delator.skip])
    ~resolve_imported_target:(resolve_imported_target [@delator.skip])
    ~imported_declaration_ids:(imported_declaration_ids [@delator.skip])
    ~imported_group_ids:(imported_group_ids [@delator.skip])
    (structure [@delator.skip]) =
  let result =
    authenticate_internal ~source_file ~artifact ~resolves_to_marker
      ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
      structure
  in
  (match result with
  | Ok _scan ->
      [%log.debug "authenticated typed broadcast carrier graph"
        ~route:(Delator.Field.string "typedtree")
        ~stage:(Delator.Field.string "carrier-authentication")
        ~declaration_count:(Delator.Field.int (List.length _scan.declarations))
        ~carrier_count:(Delator.Field.int (List.length _scan.carriers))
        ~group_count:(Delator.Field.int (List.length _scan.groups))
        ~scope_count:(Delator.Field.int (List.length _scan.binding_scopes))
        ~decision:(Delator.Field.string "accepted")]
  | Error _ -> ());
  result
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let carrier_binding scan binding =
  List.exists (fun carrier -> carrier.carrier_binding == binding) scan.carriers
let declaration scan binding =
  List.find_opt
    (fun declaration -> declaration.declaration_binding == binding)
    scan.declarations
let declaration_ids scan binding =
  scan.declarations
  |> List.filter_map (fun declaration ->
         if declaration.declaration_binding == binding then
           Some declaration.declaration_id
         else None)
let declaration_identity scan declaration_id =
  match
    List.filter
      (fun declaration ->
        String.equal declaration.declaration_id declaration_id)
      scan.declarations
  with
  | [ declaration ] ->
      Some
        (declaration.declaration_uid, declaration.declaration_interface_uid)
  | [] | _ :: _ :: _ -> None
let declaration_id scan binding =
  Option.map (fun value -> value.declaration_id) (declaration scan binding)
let trigger_locations scan binding =
  Option.fold ~none:[]
    ~some:(fun value -> value.declaration_triggers)
    (declaration scan binding)
let binding_scope scan binding =
  List.find_opt
    (fun scope -> scope.scoped_binding == binding)
    scan.binding_scopes
let active_targets scan binding =
  Option.fold ~none:[] ~some:(fun scope -> scope.scoped_targets)
    (binding_scope scan binding)
let expression_scopes scan binding =
  Option.fold ~none:[] ~some:(fun scope -> scope.scoped_expressions)
    (binding_scope scan binding)
let activation_body scan expression =
  Option.map
    (fun wrapper -> wrapper.wrapper_body)
    (List.find_opt
       (fun wrapper -> wrapper.wrapper_expression == expression)
       scan.wrappers)

let groups scan = scan.groups

let declaration_triggers scan =
  let triggers = List.map
    (fun declaration ->
      (declaration.declaration_id, declaration.declaration_triggers))
    scan.declarations
  in
  [%log.trace "summarized authenticated broadcast declaration triggers"
    ~route:(Delator.Field.string "typed-declaration-summary")
    ~declaration_count:(Delator.Field.int (List.length triggers))
    ~single_trigger_count:
      (Delator.Field.int
         (List.fold_left
            (fun count (_, locations) ->
              if List.length locations = 1 then count + 1 else count)
            0 triggers))
    ~decision:(Delator.Field.string "summarized")];
  triggers
