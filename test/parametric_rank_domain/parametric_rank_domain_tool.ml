let fail format = Printf.ksprintf failwith format

let load file =
  match Typedtree_lowering.lower_file file with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let summary file =
  let program = load file in
  let generic =
    List.filter (fun definition -> definition.Sst.type_binders <> [])
      program.Sst.functions
  in
  Printf.printf "parametric-adts=%d generic-functions=%d clones=%d\n"
    (List.length program.parametric_adts) (List.length generic)
    (List.length
       (List.filter
          (fun definition -> String.contains definition.Sst.function_id.function_name '<')
          program.functions))

let repeat file =
  let first = Sst.to_string (load file) in
  let second = Sst.to_string (load file) in
  Printf.printf "repeat-equal=%b bytes=%d\n" (String.equal first second)
    (String.length first)

let () =
  match Array.to_list Sys.argv with
  | [ _; "summary"; file ] -> summary file
  | [ _; "repeat"; file ] -> repeat file
  | _ -> fail "usage: parametric_rank_domain_tool (summary|repeat) FILE"
