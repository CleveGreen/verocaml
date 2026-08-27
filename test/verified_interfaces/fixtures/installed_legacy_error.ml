let legacy_style_error : Interface_specification.error =
  { unit_name = None; message = "legacy constructor" }

let () =
  print_endline (Interface_specification.error_to_string legacy_style_error);
  Printf.printf "diagnostic=%s\n"
    (match Interface_specification.error_diagnostic legacy_style_error with
    | None -> "none"
    | Some _ -> "unexpected")
