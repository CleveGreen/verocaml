let keep_ghost = true

let configure_observability () =
  Delator.init ();
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> Delator.set_default_level Delator.Warn
  | Some _ -> ()

let mapper () = Vero_ppx_rewriter.make [ "--keep-ghost" ]

let implementation (structure [@delator.skip]) =
  let mapper = mapper () in
  let rewritten = mapper.Ast_mapper.structure mapper structure in
  [%log.info "completed public PPX implementation transformation"
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:(Delator.Field.int (List.length structure))
    ~output_items:(Delator.Field.int (List.length rewritten))];
  rewritten
[@@delator.instrument] [@@delator.level info]

let interface (signature [@delator.skip]) =
  let mapper = mapper () in
  let rewritten = mapper.Ast_mapper.signature mapper signature in
  [%log.info "completed public PPX interface transformation"
    ~keep_ghost:(Delator.Field.bool keep_ghost)
    ~input_items:
      (Delator.Field.int (List.length signature.Parsetree.psg_items))
    ~output_items:
      (Delator.Field.int (List.length rewritten.Parsetree.psg_items))];
  rewritten
[@@delator.instrument] [@@delator.level info]

let () =
  configure_observability ();
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast "verocaml"
    ~impl:implementation ~intf:interface
