let configure_observability () =
  Delator.init ();
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> Delator.set_default_level Delator.Warn
  | Some _ -> ()

let keep_ghost_of_arguments = function
  | [] -> false
  | [ "--keep-ghost" ] -> true
  | _ ->
      invalid_arg
        "verocaml-ppx accepts only the optional --keep-ghost argument"

let transform_implementation
    ~keep_ghost:(keep_ghost [@delator.field Bool.to_string])
    (rewrite [@delator.skip]) (structure [@delator.skip]) =
  let _ = keep_ghost in
  let rewritten = rewrite structure in
  [%log.info "completed standalone PPX implementation transformation"
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length structure))
    ~output_items:(Delator.Field.int (List.length rewritten))];
  rewritten
[@@delator.instrument] [@@delator.level info]

let transform_interface
    ~keep_ghost:(keep_ghost [@delator.field Bool.to_string])
    (rewrite [@delator.skip]) (signature [@delator.skip]) =
  let _ = keep_ghost in
  let rewritten = rewrite signature in
  [%log.info "completed standalone PPX interface transformation"
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:
      (Delator.Field.int (List.length signature.Parsetree.psg_items))
    ~output_items:
      (Delator.Field.int (List.length rewritten.Parsetree.psg_items))];
  rewritten
[@@delator.instrument] [@@delator.level info]

let mapper (arguments [@delator.skip]) =
  configure_observability ();
  let keep_ghost = keep_ghost_of_arguments arguments in
  let mapper = Vero_ppx_rewriter.make arguments in
  let root_structure = ref true in
  let root_signature = ref true in
  {
    mapper with
    Ast_mapper.structure =
      (fun self structure ->
        if !root_structure then (
          root_structure := false;
          transform_implementation ~keep_ghost
            (mapper.Ast_mapper.structure self)
            structure)
        else mapper.Ast_mapper.structure self structure);
    signature =
      (fun self signature ->
        if !root_signature then (
          root_signature := false;
          transform_interface ~keep_ghost
            (mapper.Ast_mapper.signature self)
            signature)
        else mapper.Ast_mapper.signature self signature);
  }
[@@delator.instrument] [@@delator.level debug]

let () =
  configure_observability ();
  Ast_mapper.register "verocaml" mapper
