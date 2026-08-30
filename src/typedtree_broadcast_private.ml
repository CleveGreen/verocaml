open Typedtree
let carrier_attribute_name = "verocaml.internal.broadcast.carrier.v1"
let declaration_attribute_name = "verocaml.internal.broadcast.declaration.v1"
let scope_attribute_name = "verocaml.internal.broadcast.scope.v1"

type target = Broadcast_scope_private.target = {
  target_id : string;
  target_group : bool;
}
type group = {
  group_id : string;
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

let fail location message = Error { location; message }

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
  | Tpat_var (ident, name, _, _, _) -> Some (ident, name.txt)
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
  target_path : Path.t;
  target_uid : string;
}

let compiler_uid uid = Format.asprintf "%a" Types.Uid.print uid

let collect_target_references resolves_to_marker expression =
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
              let target_uid = compiler_uid description.Types.val_uid in
              [%log.trace "collected broadcast target reference"
                ~path:(Delator.Field.string (Path.name path))
                ~value_uid:(Delator.Field.string target_uid)];
              references :=
                {
                  target_path = path;
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

let path_ident = function Path.Pident ident -> Some ident | _ -> None

let target_for_reference declarations groups resolve_imported_target location
    reference =
  let path = reference.target_path in
  let imported () =
    match resolve_imported_target path reference.target_uid with
    | Some target ->
        [%log.trace "resolved imported broadcast target"
          ~path:(Delator.Field.string (Path.name path))
          ~target_id:(Delator.Field.string target.target_id)
          ~group:(Delator.Field.bool target.target_group)];
        Ok target
    | None ->
        fail location
          "broadcast target is not an authenticated local or imported declaration or group"
  in
  match path_ident path with
  | None -> imported ()
  | Some ident -> (
      match
        List.find_opt
          (fun declaration -> Ident.same declaration.declaration_ident ident)
          declarations
      with
      | Some declaration ->
          Ok
            {
              target_id = declaration.declaration_id;
              target_group = false;
            }
      | None -> (
          match
            List.find_opt
              (fun (group_ident, _, _, _) -> Ident.same group_ident ident)
              groups
          with
          | Some (_, _, id, _) -> Ok { target_id = id; target_group = true }
          | None -> imported ()))

let targets_for_carrier declarations groups resolve_imported_target
    resolves_to_marker metadata binding =
  let* body = carrier_body resolves_to_marker metadata binding in
  collect_target_references resolves_to_marker body
  |> List.fold_left
       (fun result reference ->
         let* targets = result in
         let* target =
           target_for_reference declarations groups resolve_imported_target
             binding.vb_loc reference
         in
         Ok (target :: targets))
       (Ok [])
  |> Result.map List.rev

let declaration_of_binding ~artifact ~source_file ~resolves_to_marker
    ~structure_path binding =
  match internal_attribute declaration_attribute_name binding.vb_attributes with
  | [] -> Ok None
  | [ attribute ] -> (
      let* _carrier_id = parse_declaration_attribute binding attribute in
      match binding_ident binding with
      | None ->
          fail binding.vb_loc
            "broadcast declaration must bind one named function"
      | Some (ident, name) ->
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

let authenticate_carriers ~source_file ~structure_path ~resolves_to_marker
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    ~top_level ~bindings declarations =
  let* group_seeds =
    List.fold_left
      (fun result binding ->
        let* groups = result in
        let* metadata = carrier_metadata binding in
        match (metadata, binding_ident binding) with
        | Some ({ metadata_kind = Group; _ } as metadata), Some (ident, name)
          when top_level binding && String.equal name metadata.metadata_name ->
            Ok ((ident, name, metadata.metadata_id, binding) :: groups)
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
            (fun (_, name, _, _) ->
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
              targets_for_carrier declarations group_seeds
                resolve_imported_target resolves_to_marker metadata binding
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
            Some
              {
                group_id = carrier.carrier_id;
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
        (List.map (fun declaration -> declaration.declaration_id) declarations
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

let authenticate_structure ~source_file ~artifact ~resolves_to_marker
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    (inventory : Broadcast_scope_private.typedtree_structure) =
  let bindings = inventory.bindings in
  let top_level binding =
    List.exists (fun candidate -> candidate == binding)
      inventory.direct_bindings
  in
  let* declarations =
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
            ~structure_path:inventory.structure_path binding
        in
        Ok
          (Option.fold ~none:declarations
             ~some:(fun value -> value :: declarations)
             declaration))
      (Ok []) bindings
    |> Result.map List.rev
  in
  let* carriers, groups =
    authenticate_carriers ~source_file ~resolves_to_marker
      ~structure_path:inventory.structure_path
      ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
      ~top_level ~bindings declarations
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

let authenticate ~source_file ~artifact ~resolves_to_marker
    ~resolve_imported_target ~imported_declaration_ids ~imported_group_ids
    structure =
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
      List.fold_left
        (fun result structure_inventory ->
          let* combined = result in
          let* local =
            authenticate_structure ~source_file ~artifact ~resolves_to_marker
              ~resolve_imported_target ~imported_declaration_ids
              ~imported_group_ids structure_inventory
          in
          Ok (append combined local))
        (Ok empty)
        inventory.structures

let carrier_binding scan binding =
  List.exists (fun carrier -> carrier.carrier_binding == binding) scan.carriers
let declaration scan binding =
  List.find_opt
    (fun declaration -> declaration.declaration_binding == binding)
    scan.declarations
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
  List.map
    (fun declaration ->
      (declaration.declaration_id, declaration.declaration_triggers))
    scan.declarations
