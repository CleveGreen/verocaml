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
  let implementation_mapper =
    lazy
      (Vero_ppx_rewriter.make ~entrypoint:Vero_ppx_rewriter.Implementation
         arguments)
  in
  let interface_mapper =
    lazy
      (Vero_ppx_rewriter.make ~entrypoint:Vero_ppx_rewriter.Interface
         arguments)
  in
  {
    Ast_mapper.default_mapper with
    Ast_mapper.structure =
      (fun _self structure ->
        let mapper = Lazy.force implementation_mapper in
        transform_implementation ~keep_ghost
          (mapper.Ast_mapper.structure mapper)
          structure);
    signature =
      (fun _self signature ->
        let mapper = Lazy.force interface_mapper in
        transform_interface ~keep_ghost
          (mapper.Ast_mapper.signature mapper)
          signature);
  }
[@@delator.instrument] [@@delator.level debug]

let () =
  configure_observability ();
  Ast_mapper.register "verocaml" mapper
