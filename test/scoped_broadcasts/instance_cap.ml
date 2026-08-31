let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let type_name index =
  let base = if index mod 2 = 0 then "int" else "bool" in
  base ^ String.concat "" (List.init (index / 2) (Fun.const " option"))

let add_line buffer line =
  Buffer.add_string buffer line;
  Buffer.add_char buffer '\n'

let source () =
  let types = List.init 17 type_name in
  let buffer = Buffer.create 4096 in
  List.iter (add_line buffer)
    [
      "type 'a option_specification = 'a option";
      "[@@verocaml.external_type_specification]";
      "";
      "let observed (_value : 'a) : bool = true [@@verocaml.spec]";
      "";
      "let lemma (value : 'a) : unit =";
      "  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];";
      "  ()";
      "[@@verocaml.proof]";
      "[@@verocaml.external_body]";
      "[@@verocaml.broadcast]";
      "";
      "[@@@verocaml.activate [lemma]]";
      "";
      "let cap";
    ];
  List.iteri
    (fun index typ ->
      add_line buffer (Printf.sprintf "    (v%02d : %s)" index typ))
    types;
  add_line buffer "    : unit =";
  List.iter
    (fun typ ->
      add_line buffer "  [%verocaml.requires";
      add_line buffer (Printf.sprintf "    forall (fun (candidate : %s) ->" typ);
      add_line buffer
        "      ((observed candidate) [@trigger]) || candidate = candidate)];")
    types;
  List.iter (add_line buffer)
    [ "  [%verocaml.assert true];"; "  ()"; "[@@verocaml.proof]" ];
  Buffer.contents buffer

let () =
  let destination =
    match Sys.argv with
    | [| _; destination |] -> destination
    | _ ->
        prerr_endline "usage: instance_cap.exe DESTINATION";
        exit 1
  in
  write_file destination (source ())
