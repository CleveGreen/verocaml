let configure_observability () =
  Delator.init ();
  match Sys.getenv_opt "DELATOR_LOG" with
  | None | Some "" -> Delator.set_default_level Delator.Warn
  | Some _ -> ()

type mode = Ordinary | Retained

let selected_mode = ref None

let default_mode =
  match Sys.getenv_opt "VEROCAML_INTERNAL_PPX_MODE" with
  | Some "retained" -> Retained
  | None | Some _ -> Ordinary

let select_mode mode () =
  match !selected_mode with
  | None -> selected_mode := Some mode
  | Some _ -> raise (Arg.Bad "VeroCaml Ppxlib mode may be selected only once")

let effective_mode () =
  match default_mode with
  | Retained -> Retained
  | Ordinary -> (
      match !selected_mode with Some mode -> mode | None -> Ordinary)

let mode_source () =
  match (default_mode, !selected_mode) with
  | Retained, _ -> "verification-environment"
  | Ordinary, Some _ -> "explicit-argument"
  | Ordinary, None -> "ordinary-default"

let mapper entrypoint =
  Vero_ppx_rewriter.make ~entrypoint
    ("--verocaml-internal-ppxlib-v1"
    ::
    match effective_mode () with
    | Ordinary -> []
    | Retained -> [ "--keep-ghost" ])

let selected_family () =
  match effective_mode () with
  | Ordinary -> (false, "ordinary-v1")
  | Retained -> (true, "retained-v1")

let with_transformation stage (action [@delator.skip]) =
  let keep_ghost, family = selected_family () in
  let[@log_value.debug] mode_source = mode_source () in
  let _ = (stage, keep_ghost, family) in
  [%log.debug "starting official Ppxlib transformation"
    ~route:(Delator.Field.string "ppxlib-v1")
    ~family:(Delator.Field.string family)
    ~mode_source:(Delator.Field.string (mode_source [@log_value.debug]))
    ~stage:(Delator.Field.string stage)
    ~keep_ghost:(Delator.Field.bool keep_ghost)];
  let rewritten =
    Delator.in_span ~level:Delator.Debug ~target:"Vero_ppx_driver"
      ~name:"official Ppxlib transformation"
      ~fields:(fun () ->
        [
          ("route", Delator.Field.string "ppxlib-v1");
          ("family", Delator.Field.string family);
          ("stage", Delator.Field.string stage);
        ])
      action
  in
  rewritten
[@@delator.instrument] [@@delator.level debug]

let implementation (structure [@delator.skip]) =
  with_transformation "implementation-transformation" (fun () ->
      let keep_ghost, _ = selected_family () in
      let _ = keep_ghost in
      let mapper = mapper Vero_ppx_rewriter.Implementation in
      let rewritten = mapper.Ast_mapper.structure mapper structure in
      [%log.info "completed public PPX implementation transformation"
        ~keep_ghost:(Delator.Field.bool keep_ghost)
        ~input_items:(Delator.Field.int (List.length structure))
        ~output_items:(Delator.Field.int (List.length rewritten))];
      rewritten)
[@@delator.instrument] [@@delator.level info]

let interface (signature [@delator.skip]) =
  with_transformation "interface-transformation" (fun () ->
      let keep_ghost, _ = selected_family () in
      let _ = keep_ghost in
      let mapper = mapper Vero_ppx_rewriter.Interface in
      let rewritten = mapper.Ast_mapper.signature mapper signature in
      [%log.info "completed public PPX interface transformation"
        ~keep_ghost:(Delator.Field.bool keep_ghost)
        ~input_items:
          (Delator.Field.int (List.length signature.Parsetree.psg_items))
        ~output_items:
          (Delator.Field.int (List.length rewritten.Parsetree.psg_items))];
      rewritten)
[@@delator.instrument] [@@delator.level info]

let () =
  configure_observability ();
  Ppxlib.Driver.add_arg "--verocaml-retained"
    (Arg.Unit (select_mode Retained))
    ~doc:"retain authenticated VeroCaml ghost artifacts";
  Ppxlib.Driver.add_arg "--verocaml-ordinary"
    (Arg.Unit (select_mode Ordinary))
    ~doc:"erase VeroCaml ghost artifacts (the default)";
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast "verocaml"
    ~impl:implementation ~intf:interface
