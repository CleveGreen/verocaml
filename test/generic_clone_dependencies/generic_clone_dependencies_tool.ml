let () = ignore Generic_clone_dependencies_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let validate program =
  match Sst_validation.validate program with
  | Ok _ -> ()
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let schema_report filename =
  let program = lower (load filename) in
  validate program;
  let generic =
    program.Sst.functions
    |> List.filter (fun definition -> definition.Sst.type_binders <> [])
    |> List.sort (fun left right ->
           Int.compare left.Sst.function_id.function_index
             right.Sst.function_id.function_index)
  in
  if
    List.exists
      (fun definition ->
        String.contains definition.Sst.function_id.function_name '<')
      program.functions
  then fail "closed source clone identity survived schema lowering";
  List.iter
    (fun definition ->
      Printf.printf "schema function=%s#%d binders=%d\n"
        definition.Sst.function_id.function_name
        definition.Sst.function_id.function_index
        (List.length definition.Sst.type_binders))
    generic;
  Printf.printf "schema-count=%d clone-identities=0\n" (List.length generic)

let () =
  match Array.to_list Sys.argv with
  | [ _; "schema"; filename ] -> schema_report filename
  | _ -> fail "usage: generic_clone_dependencies_tool schema <file.cmt>"
