let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text substring =
  let text_length = String.length text
  and substring_length = String.length substring in
  let rec search offset =
    if offset + substring_length > text_length then false
    else if String.sub text offset substring_length = substring then true
    else search (offset + 1)
  in
  substring_length = 0 || search 0

let require_present path text substring =
  if not (contains text substring) then
    failwith (Printf.sprintf "%s does not contain %S" path substring)

let require_absent path text substring =
  if contains text substring then
    failwith (Printf.sprintf "%s contains %S" path substring)

let check scope_path project_path binary_path =
  let scope = read_file scope_path in
  let project = read_file project_path in
  require_absent project_path project "Verification_scope_private";
  let binary = read_file binary_path in
  require_present binary_path binary "Verifier_service";
  require_absent binary_path binary "Verification_scope_private";
  require_absent scope_path scope "Verocaml_bin";
  print_endline
    "architecture scope-owner=private maxima=inclusive dependency=one-way"

let () =
  match Array.to_list Sys.argv with
  | [ _; scope_path; project_path; binary_path ] ->
      check scope_path project_path binary_path
  | _ ->
      failwith
        (Printf.sprintf "usage: %s SCOPE_SOURCE PROJECT_SOURCE BINARY_SOURCE"
           Sys.argv.(0))
