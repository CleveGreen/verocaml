let cases =
  [
    ( "duplicate_binding",
      "let bad x = let[@ghost][@ghost] y = (x [@ghost]) in y" );
    ( "conflicting_binding",
      "let bad x = let[@ghost][@tracked] y = (x [@ghost]) in y" );
    ("duplicate_expression", "let bad x = ((x [@ghost]) [@tracked])");
    ( "multi_binding",
      "let bad x = let[@ghost] y = (x [@ghost]) and z = (x [@ghost]) in y" );
    ( "record_pun",
      "type t = { run : int; ghost : int [@ghost] }\nlet bad p = let { run; ghost } = p in run"
    );
    ( "missing_record_component",
      "type t = { run : int; ghost : int [@ghost] }\nlet bad x = { run = x; ghost = x }"
    );
    ( "missing_positional_component",
      "type t = C of int * (int [@ghost])\nlet bad x = C (x, x)" );
    ( "update_mismatch",
      "type t = { mutable run : int; mutable ghost : int [@ghost] }\nlet bad p x = ((p.ghost <- (x [@tracked])) [@tracked])"
    );
    ( "erased_mutation",
      "let bad x = let cell = ref 0 in let[@ghost] _ = ((cell := x; x) [@ghost]) in x"
    );
    ( "erased_exception",
      "let bad x = let[@ghost] _ = ((raise Exit) [@ghost]) in x" );
    ( "erased_divergence",
      "let rec loop x = loop x\nlet bad x = let[@ghost] _ = (loop x [@ghost]) in x"
    );
    ( "erased_allocation",
      "let bad x = let[@ghost] _ = (ref x [@ghost]) in x" );
    ( "erased_capture",
      "let bad x = let[@ghost] _ = ((fun () -> x) [@ghost]) in x" );
    ( "nested_update_effect",
      "type t = { mutable run : int; mutable ghost : int [@ghost] }\nlet effect x = x + 1\nlet bad p x = ((p.ghost <- (effect x [@ghost])) [@ghost])"
    );
  ]

let write_file path contents =
  let channel = open_out_bin path in
  match output_string channel contents with
  | () -> close_out channel
  | exception error ->
      close_out_noerr channel;
      raise error

let rec create_directory path =
  match Unix.mkdir path 0o777 with
  | () -> ()
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> (
      match (Unix.stat path).st_kind with
      | Unix.S_DIR -> ()
      | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path)))
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      let parent = Filename.dirname path in
      if String.equal parent path then
        raise (Unix.Unix_error (Unix.ENOENT, "mkdir", path));
      create_directory parent;
      create_directory path

let generate destination =
  let destination = if String.equal destination "" then "." else destination in
  create_directory destination;
  List.iter
    (fun (name, source) ->
      write_file (Filename.concat destination (name ^ ".ml")) (source ^ "\n"))
    cases

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: generate_negative_matrix.exe DESTINATION";
    exit 1);
  generate Sys.argv.(1)
