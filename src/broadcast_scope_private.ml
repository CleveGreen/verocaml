type declaration_kind = Proved | Trusted

type declaration = {
  declaration_id : string;
  function_id : Sst.function_id;
  kind : declaration_kind;
  declaration_span : Diagnostic.span;
  witness_span : Diagnostic.span option;
}

type target = { target_id : string; target_group : bool }

type group = {
  group_id : string;
  group_name : string;
  targets : target list;
  span : Diagnostic.span;
}

type expression_scope = {
  scope_id : string;
  scope_span : Diagnostic.span;
  targets : target list;
}

type function_scope = {
  function_id : Sst.function_id;
  targets : target list;
  expressions : expression_scope list;
}

type selection = {
  declaration : declaration;
  selecting_paths : string list list;
}

type counters = {
  active_declarations : int;
  trusted_declarations : int;
  proved_declarations : int;
}

type entry = {
  program : Sst.program Weak.t;
  declarations : declaration list;
  groups : group list;
  scopes : function_scope list;
  mutable completed : string list;
}

let entries : entry list ref = ref []

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

type typedtree_structure = {
  structure_path : string list;
  structure : Typedtree.structure;
  direct_bindings : Typedtree.value_binding list;
  bindings : Typedtree.value_binding list;
  carrier_attributes : int;
  declaration_attributes : int;
  scope_attributes : int;
  public_attributes : int;
}

type typedtree_inventory = {
  structures : typedtree_structure list;
  carrier_attributes : int;
  declaration_attributes : int;
  scope_attributes : int;
  public_attributes : int;
}

type typedtree_syntax = Absent | Retained | Raw

type carrier_kind = Group_carrier | Structure_carrier | Expression_carrier

type carrier_metadata = {
  carrier_kind : carrier_kind;
  carrier_id : string;
  carrier_name : string;
}

let typedtree_inventory root =
  let rec module_structure module_expr =
    match module_expr.Typedtree.mod_desc with
    | Tmod_structure structure -> Some structure
    | Tmod_constraint (inner, _, _, _) -> module_structure inner
    | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_unpack _ ->
        None
  in
  let rec collect structure_path structure =
    let direct_bindings =
      structure.Typedtree.str_items
      |> List.concat_map (fun item ->
             match item.Typedtree.str_desc with
             | Tstr_value (_, bindings) -> bindings
             | _ -> [])
    in
    let bindings = ref []
    and carrier_attributes = ref 0
    and declaration_attributes = ref 0
    and scope_attributes = ref 0
    and public_attributes = ref 0 in
    let inspect attributes =
      List.iter
        (fun attribute ->
          match attribute.Parsetree.attr_name.txt with
          | "verocaml.internal.broadcast.carrier.v1" -> incr carrier_attributes
          | "verocaml.internal.broadcast.declaration.v1" ->
              incr declaration_attributes
          | "verocaml.internal.broadcast.scope.v1" -> incr scope_attributes
          | "verocaml.activate" | "verocaml.broadcast_group" | "verocaml.broadcast"
          | "verocaml.broadcast_lemma" | "verocaml.broadcast_axiom" | "verocaml.axiom" ->
              incr public_attributes
          | _ -> ())
        attributes
    in
    let default = Tast_iterator.default_iterator in
    let iterator =
      {
        default with
        value_binding =
          (fun self binding ->
            bindings := binding :: !bindings;
            inspect binding.Typedtree.vb_attributes;
            default.value_binding self binding);
        expr =
          (fun self expression ->
            inspect expression.Typedtree.exp_attributes;
            default.expr self expression);
        module_expr = (fun _ _ -> ());
      }
    in
    List.iter (iterator.value_binding iterator) direct_bindings;
    List.iter
      (fun item ->
        match item.Typedtree.str_desc with
        | Tstr_attribute attribute -> inspect [ attribute ]
        | _ -> ())
      structure.str_items;
    let current =
      {
        structure_path;
        structure;
        direct_bindings;
        bindings = List.rev !bindings;
        carrier_attributes = !carrier_attributes;
        declaration_attributes = !declaration_attributes;
        scope_attributes = !scope_attributes;
        public_attributes = !public_attributes;
      }
    in
    let children =
      structure.str_items
      |> List.concat_map (fun item ->
             match item.Typedtree.str_desc with
             | Tstr_module binding -> (
                 match module_structure binding.Typedtree.mb_expr with
                 | Some child ->
                     let name =
                       Option.value ~default:"<anonymous>" binding.mb_name.txt
                     in
                     collect (structure_path @ [ name ]) child
                 | None -> [])
             | _ -> [])
    in
    current :: children
  in
  let structures = collect [] root in
  let total field =
    List.fold_left (fun count structure -> count + field structure) 0 structures
  in
  {
    structures;
    carrier_attributes = total (fun value -> value.carrier_attributes);
    declaration_attributes = total (fun value -> value.declaration_attributes);
    scope_attributes = total (fun value -> value.scope_attributes);
    public_attributes = total (fun value -> value.public_attributes);
  }

let typedtree_syntax inventory =
  if inventory.public_attributes > 0 then Raw
  else if
    inventory.carrier_attributes = 0
    && inventory.declaration_attributes = 0
    && inventory.scope_attributes = 0
  then Absent
  else Retained

let canonical_marker_path imports path =
  Array.exists
    (fun (import : Cmt_input.import) ->
      String.equal import.unit_name "Vero_ghost"
      && import.crc = Some Trusted_imports.ghost_crc)
    imports
  &&
  match Path.flatten path with
  | `Ok (root, [ component ]) ->
      Ident.is_global_or_predef root
      && String.equal (Ident.name root) "Vero_ghost"
      && String.equal component "marker"
  | `Ok _ | `Contains_apply -> false

let carrier_payload attribute =
  match attribute.Parsetree.attr_payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ },
                [] );
          _;
        };
      ] ->
      Some value
  | _ -> None

let carrier_stable_id kind loc suffix =
  let start_ = loc.Location.loc_start.Lexing.pos_cnum
  and stop = loc.Location.loc_end.Lexing.pos_cnum in
  let material =
    Printf.sprintf "broadcast-v1|%s|%d|%d|%s" kind start_ stop suffix
  in
  kind ^ "." ^ Digest.to_hex (Digest.string material)

let authenticate_carrier_attribute binding attribute =
  let fail message = Error (attribute.Parsetree.attr_loc, message) in
  if
    not
      (attribute.attr_loc.Location.loc_ghost
      && attribute.attr_name.loc.loc_ghost)
  then fail "broadcast carrier attribute is not retained ghost syntax"
  else
    match Option.map (String.split_on_char '|') (carrier_payload attribute) with
    | Some [ kind; id; name; start_; stop ] -> (
        let carrier_kind =
          match kind with
          | "group" -> Some Group_carrier
          | "structure" -> Some Structure_carrier
          | "expression" -> Some Expression_carrier
          | _ -> None
        in
        match
          ( carrier_kind,
            int_of_string_opt start_,
            int_of_string_opt stop )
        with
        | Some carrier_kind, Some start_, Some stop
          when
            start_ = attribute.attr_loc.loc_start.pos_cnum
            && stop = attribute.attr_loc.loc_end.pos_cnum ->
            let suffix = if carrier_kind = Group_carrier then name else "" in
            if
              String.equal id
                (carrier_stable_id kind binding.Typedtree.vb_loc suffix)
            then Ok { carrier_kind; carrier_id = id; carrier_name = name }
            else fail "broadcast carrier identity is stale or forged"
        | _ -> fail "broadcast carrier kind or span is invalid")
    | Some _ | None -> fail "broadcast carrier payload is malformed"

let same_function_id left right =
  left.Sst.function_index = right.Sst.function_index
  && String.equal left.function_name right.function_name

let unique_by id values =
  let rec loop seen = function
    | [] -> Ok ()
    | value :: rest ->
        let key = id value in
        if List.mem key seen then Error key else loop (key :: seen) rest
  in
  loop [] values

let validate_targets declarations groups targets =
  let declaration id =
    List.exists (fun value -> String.equal value.declaration_id id) declarations
  and group id =
    List.exists (fun value -> String.equal value.group_id id) groups
  in
  match
    List.find_opt
      (fun target ->
        if target.target_group then not (group target.target_id)
        else not (declaration target.target_id))
      targets
  with
  | None -> Ok ()
  | Some target -> Error ("unknown broadcast target " ^ target.target_id)

let validate_graph declarations groups =
  let group_name id =
    Option.fold ~none:id ~some:(fun group -> group.group_name)
      (List.find_opt (fun group -> String.equal group.group_id id) groups)
  in
  let rec visit stack done_ group_id =
    if List.mem group_id stack then
      Error
        ("broadcast group cycle: "
        ^ String.concat " -> "
            (List.map group_name (List.rev (group_id :: stack))))
    else if List.mem group_id done_ then Ok done_
    else
      match
        List.find_opt (fun group -> String.equal group.group_id group_id) groups
      with
      | None -> Error ("unknown broadcast group " ^ group_id)
      | Some group ->
          let* () = validate_targets declarations groups group.targets in
          let* done_ =
            List.fold_left
              (fun result target ->
                let* done_ = result in
                if target.target_group then
                  visit (group_id :: stack) done_ target.target_id
                else Ok done_)
              (Ok done_) group.targets
          in
          Ok (group_id :: done_)
  in
  List.fold_left
    (fun result group ->
      let* done_ = result in
      visit [] done_ group.group_id)
    (Ok []) groups
  |> Result.map (fun _ -> ())

let validate_target_graph ~declaration_ids ~groups =
  let placeholder = Diagnostic.file_span "<broadcast-authentication>" in
  let declarations =
    List.mapi
      (fun function_index declaration_id ->
        {
          declaration_id;
          function_id =
            {
              Sst.function_index;
              function_name = "broadcast-authentication";
            };
          kind = Proved;
          declaration_span = placeholder;
          witness_span = None;
        })
      declaration_ids
  in
  let groups =
    List.mapi
      (fun index (group_id, group_name, targets) ->
        {
          group_id;
          group_name;
          targets;
          span =
            { placeholder with start_pos = { line = index; column = 0 } };
        })
      groups
  in
  validate_graph declarations groups

let weak_program program =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some program);
  weak

let live_entries () =
  let live =
    List.filter
      (fun entry -> Option.is_some (Weak.get entry.program 0))
      !entries
  in
  entries := live;
  live

let find program =
  List.find_opt
    (fun entry ->
      Option.fold ~none:false
        ~some:(fun value -> value == program)
        (Weak.get entry.program 0))
    (live_entries ())

let register ~program ~declarations ~groups ~scopes =
  let* () =
    unique_by (fun declaration -> declaration.declaration_id) declarations
    |> Result.map_error (fun id -> "duplicate broadcast declaration " ^ id)
  in
  let* () =
    unique_by (fun group -> group.group_id) groups
    |> Result.map_error (fun id -> "duplicate broadcast group " ^ id)
  in
  let* () = validate_graph declarations groups in
  let* () =
    List.fold_left
      (fun result scope ->
        let* () = result in
        let* () = validate_targets declarations groups scope.targets in
        List.fold_left
          (fun result (expression : expression_scope) ->
            let* () = result in
            validate_targets declarations groups expression.targets)
          (Ok ()) scope.expressions)
      (Ok ()) scopes
  in
  let completed =
    List.filter_map
      (fun declaration ->
        if declaration.kind = Trusted then Some declaration.declaration_id
        else None)
      declarations
  in
  entries :=
    { program = weak_program program; declarations; groups; scopes; completed }
    :: List.filter
         (fun entry ->
           Option.fold ~none:false
             ~some:(fun value -> value != program)
             (Weak.get entry.program 0))
         (live_entries ());
  Ok ()

let position_compare left right =
  match Int.compare left.Diagnostic.line right.Diagnostic.line with
  | 0 -> Int.compare left.column right.column
  | comparison -> comparison

let span_contains outer inner =
  String.equal outer.Diagnostic.file inner.Diagnostic.file
  && position_compare outer.start_pos inner.start_pos <= 0
  && position_compare inner.end_pos outer.end_pos <= 0

let merge_targets left right =
  List.fold_left
    (fun targets target ->
      if
        List.exists
          (fun current -> String.equal current.target_id target.target_id)
          targets
      then targets
      else targets @ [ target ])
    left right

let targets_for entry function_id span =
  match
    List.find_opt
      (fun scope -> same_function_id scope.function_id function_id)
      entry.scopes
  with
  | None -> []
  | Some scope ->
      let is_broadcast =
        List.exists
          (fun (declaration : declaration) ->
            same_function_id declaration.function_id function_id)
          entry.declarations
      in
      if is_broadcast then []
      else
        List.fold_left
          (fun targets (expression : expression_scope) ->
            if span_contains expression.scope_span span then
              merge_targets targets expression.targets
            else targets)
          scope.targets scope.expressions

let add_path declaration path selections =
  match
    List.find_opt
      (fun selection ->
        String.equal selection.declaration.declaration_id
          declaration.declaration_id)
      selections
  with
  | None -> { declaration; selecting_paths = [ path ] } :: selections
  | Some existing ->
      let selecting_paths =
        if List.mem path existing.selecting_paths then existing.selecting_paths
        else path :: existing.selecting_paths
      in
      { existing with selecting_paths }
      :: List.filter (fun candidate -> candidate != existing) selections

let expand entry targets =
  let rec expand_target path selections target_value =
    if target_value.target_group then
      match
        List.find_opt
          (fun group -> String.equal group.group_id target_value.target_id)
          entry.groups
      with
      | None -> Error ("unknown active broadcast group " ^ target_value.target_id)
      | Some group ->
          List.fold_left
            (fun result member ->
              let* selections = result in
              expand_target (path @ [ group.group_name ]) selections member)
            (Ok selections) group.targets
    else
      match
        List.find_opt
          (fun declaration ->
            String.equal declaration.declaration_id target_value.target_id)
          entry.declarations
      with
      | None ->
          Error
            ("unknown active broadcast declaration " ^ target_value.target_id)
      | Some declaration ->
          Ok
            (add_path declaration
               (path @ [ declaration.declaration_id ])
               selections)
  in
  List.fold_left
    (fun result target_value ->
      let* selections = result in
      expand_target [ "activate" ] selections target_value)
    (Ok []) targets
  |> Result.map (fun selections ->
      selections
      |> List.map (fun selection ->
          {
            selection with
            selecting_paths = List.sort compare selection.selecting_paths;
          })
      |> List.sort (fun left right ->
          String.compare left.declaration.declaration_id
            right.declaration.declaration_id))

let active ~program ~function_id ~span =
  match find program with
  | None -> Error "broadcast scope is absent or stale"
  | Some entry -> (
      let* selections = expand entry (targets_for entry function_id span) in
      match
        List.find_opt
          (fun selection ->
            selection.declaration.kind = Proved
            && not
                 (List.mem selection.declaration.declaration_id entry.completed))
          selections
      with
      | None -> Ok selections
      | Some pending ->
          Error
            ("active broadcast lemma is not verified: "
           ^ pending.declaration.declaration_id))

let proved_prerequisites ~program function_id =
  match find program with
  | None -> []
  | Some entry -> (
      let targets =
        match
          List.find_opt
            (fun scope -> same_function_id scope.function_id function_id)
            entry.scopes
        with
        | None -> []
        | Some scope ->
            if
              List.exists
                (fun (declaration : declaration) ->
                  same_function_id declaration.function_id function_id)
                entry.declarations
            then []
            else
              List.fold_left
                (fun targets (expression : expression_scope) ->
                  merge_targets targets expression.targets)
                scope.targets scope.expressions
      in
      match expand entry targets with
      | Error _ -> []
      | Ok selections ->
          List.filter_map
            (fun selection ->
              if selection.declaration.kind = Proved then
                Some selection.declaration.function_id
              else None)
            selections)

let add_proved_dependencies ~program ~definitions dependencies =
  List.fold_left
    (fun dependencies definition ->
      let prerequisites =
        proved_prerequisites ~program definition.Sst.function_id
      in
      if prerequisites = [] then dependencies
      else
        let caller = definition.function_id.function_index in
        let previous =
          Option.value ~default:[] (List.assoc_opt caller dependencies)
        in
        let combined =
          List.fold_left
            (fun ids id ->
              if List.exists (same_function_id id) ids then ids else id :: ids)
            previous prerequisites
        in
        (caller, combined) :: List.remove_assoc caller dependencies)
    dependencies definitions

let mark_completed ~program function_id ~verified =
  match find program with
  | None -> ()
  | Some entry ->
      List.iter
        (fun (declaration : declaration) ->
          if same_function_id declaration.function_id function_id then
            if verified then
              entry.completed <- declaration.declaration_id :: entry.completed
            else
              entry.completed <-
                List.filter
                  (fun id -> not (String.equal id declaration.declaration_id))
                  entry.completed)
        entry.declarations

let counters selections =
  List.fold_left
    (fun counters selection ->
      {
        active_declarations = counters.active_declarations + 1;
        trusted_declarations =
          (counters.trusted_declarations
          + if selection.declaration.kind = Trusted then 1 else 0);
        proved_declarations =
          (counters.proved_declarations
          + if selection.declaration.kind = Proved then 1 else 0);
      })
    {
      active_declarations = 0;
      trusted_declarations = 0;
      proved_declarations = 0;
    }
    selections

let destroy program =
  entries :=
    List.filter
      (fun entry ->
        Option.fold ~none:false
          ~some:(fun value -> value != program)
          (Weak.get entry.program 0))
      (live_entries ())

let registered program = Option.is_some (find program)
