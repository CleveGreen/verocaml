let mapper () = Vero_ppx_rewriter.make [ "--keep-ghost" ]

let implementation structure =
  let mapper = mapper () in
  mapper.Ast_mapper.structure mapper structure

let interface signature =
  let mapper = mapper () in
  mapper.Ast_mapper.signature mapper signature

let () =
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast "verocaml"
    ~impl:implementation ~intf:interface
