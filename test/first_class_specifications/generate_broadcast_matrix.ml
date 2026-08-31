let increment = "let increment (x:int):int = x + 1 [@@verocaml.spec]\n"

let theorem domain argument =
  String.concat "\n"
    [
      Printf.sprintf "let lemma (f : %s -> %s) (x : %s) : unit =" domain
        domain argument;
      "  [%verocaml.ensures fun _ -> ((f x) [@trigger]) = f x]; ()";
      "[@@verocaml.proof] [@@verocaml.broadcast]";
      "";
    ]

let target =
  String.concat "\n"
    [
      "let target (x:int):int =";
      "  [%verocaml.assert (let f = increment in f x = f x)];";
      "  [%verocaml.ensures fun result -> result = x]; x";
      "";
    ]

let write_file destination name contents =
  let path = Filename.concat destination name in
  let channel = open_out_bin path in
  match output_string channel contents with
  | () -> close_out channel
  | exception error ->
      close_out_noerr channel;
      raise error

let capped count =
  let rows = ref [ "let increment (x:int):int = x + 1 [@@verocaml.spec]"; "" ] in
  for index = 0 to count - 1 do
    rows :=
      !rows
      @ [
          Printf.sprintf "let lemma_%d (f:int->int) (x:int):unit =" index;
          "  [%verocaml.ensures fun _ -> ((f x) [@trigger]) = f x]; ()";
          "[@@verocaml.proof] [@@verocaml.broadcast]";
          "";
        ]
  done;
  rows :=
    !rows
    @ [
        Printf.sprintf "[@@@verocaml.activate [%s]]"
          (String.concat "; "
             (List.init count (fun index -> Printf.sprintf "lemma_%d" index)));
        "let target (x:int):int =";
        "  [%verocaml.assert (let f = increment in f x = f x)];";
        "  [%verocaml.ensures fun result -> result = x]; x";
      ];
  String.concat "\n" !rows ^ "\n"

let generate destination =
  let write name contents = write_file destination name contents in
  let integer_theorem = theorem "int" "int" in
  write "broadcast_exact.ml"
    (increment ^ integer_theorem ^ "[@@@verocaml.activate [lemma]]\n" ^ target);
  write "broadcast_duplicate.ml"
    (increment ^ integer_theorem
   ^ "[@@@verocaml.activate [lemma; lemma]]\n" ^ target);
  write "broadcast_inactive.ml" (increment ^ integer_theorem ^ target);
  write "broadcast_mismatch.ml"
    (increment ^ theorem "bool" "bool"
   ^ "[@@@verocaml.activate [lemma]]\n" ^ target);
  write "broadcast_16.ml" (capped 16);
  write "broadcast_17.ml" (capped 17)

let () =
  match Array.to_list Sys.argv with
  | [ _; destination ] -> generate destination
  | _ ->
      prerr_endline "usage: generate_broadcast_matrix.exe DESTINATION";
      exit 1
