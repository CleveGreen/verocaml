let () = ignore Finite_formal_counter_prerequisites.ready

let fixture_error message =
  Printf.eprintf "abi-fixture-error: %s\n" message;
  exit 2

let attack_of_fixture fixture_path =
  let channel = open_in_bin fixture_path in
  let contents =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let line =
    match String.split_on_char '\n' contents with
    | [ line ] | [ line; "" ] -> line
    | _ -> fixture_error "expected exactly one attack selector record"
  in
  let prefix = "attack=" in
  if not (String.starts_with ~prefix line) then
    fixture_error "expected attack=<canonical-name>";
  let selector =
    String.sub line (String.length prefix) (String.length line - String.length prefix)
  in
  if selector = "" then fixture_error "attack selector is empty";
  match
    Imported_callable.For_testing.abi_attacks
    |> List.filter (fun attack ->
           String.equal
             (Imported_callable.For_testing.abi_attack_name attack)
             selector)
  with
  | [ attack ] -> attack
  | [] -> fixture_error ("unknown attack selector: " ^ selector)
  | _ :: _ :: _ -> fixture_error ("ambiguous attack selector: " ^ selector)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] ->
      List.iter print_endline
        (Finite_value_registry.For_testing.formal_matrix ())
  | [ _; fixture_path ] ->
      let attack = attack_of_fixture fixture_path in
      print_endline
        (Verification_session.For_testing.retained_abi_attack attack)
  | _ -> invalid_arg "usage: finite_formal_counter_tool.exe [ABI_FIXTURE]"
