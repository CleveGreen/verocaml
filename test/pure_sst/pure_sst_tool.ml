let fail format = Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let print_diagnostic diagnostic =
  let span = diagnostic.Diagnostic.span in
  Printf.printf "%s %s:%d:%d-%d:%d\n" diagnostic.code
    (Filename.basename span.file) span.start_pos.line span.start_pos.column
    span.end_pos.line span.end_pos.column

let () =
  match Array.to_list Sys.argv with
  | [ _; "dump"; filename ] -> (
      match Typedtree_lowering.lower_file filename with
      | Ok program -> print_string (Sst.to_string program)
      | Error diagnostic -> print_diagnostic diagnostic; exit 2)
  | [ _; "classify"; filename ] -> (
      match Typedtree_lowering.lower_file filename with
      | Ok _ -> print_endline "accepted"
      | Error diagnostic -> print_diagnostic diagnostic)
  | _ -> fail "usage: pure_sst_tool (dump|classify) FILE.cmt"
