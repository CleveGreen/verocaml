let attacks =
  [
    ("direct", "[%verocaml.use_type_invariant x]; ()");
    ("alias", "let y = x in [%verocaml.use_type_invariant y]; ()");
    ( "pattern",
      "let (y : Box.t) = x in [%verocaml.use_type_invariant y]; ()" );
    ( "branch",
      "let y = if true then x else x in [%verocaml.use_type_invariant y]; ()" );
    ("result", "let y = relay x in [%verocaml.use_type_invariant y]; ()");
  ]

let actuals =
  [
    ("exec", "let actual = source in");
    ("tracked", "let[@tracked] actual = (source [@tracked]) in");
    ("ghost", "let[@ghost] actual = (source [@ghost]) in");
  ]

let read_file path =
  let channel = open_in_bin path in
  match really_input_string channel (in_channel_length channel) with
  | contents ->
      close_in channel;
      contents
  | exception error ->
      close_in_noerr channel;
      raise error

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

let generate destination prelude_path =
  let destination = if String.equal destination "" then "." else destination in
  create_directory destination;
  let prelude = read_file prelude_path in
  let attacks = attacks @ [ ("tracked_escalation", "") ] in
  List.iter
    (fun category ->
      List.iter
        (fun (actual_name, setup) ->
          List.iter
            (fun (attack_name, attack) ->
              let source = Buffer.create 1_024 in
              Buffer.add_string source prelude;
              Buffer.add_string source "\n\n";
              if
                String.equal attack_name "result"
                || String.equal attack_name "tracked_escalation"
              then (
                Buffer.add_string source
                  "let consume (x : Box.t) : Box.t = x\n";
                Printf.bprintf source "[@@verocaml.%s]\n\n" category;
                Buffer.add_string source "let flow (source : Box.t) : unit =\n";
                Printf.bprintf source "  %s\n" setup;
                Buffer.add_string source
                  "  let[@ghost] observed = (consume actual [@ghost]) in\n";
                if String.equal attack_name "result" then
                  Buffer.add_string source
                    "  [%verocaml.proof\n    [%verocaml.use_type_invariant observed]];\n  ()\n"
                else
                  Buffer.add_string source
                    "  let[@tracked] _escalated = (observed [@tracked]) in\n  ()\n")
              else (
                Buffer.add_string source "let consume (x : Box.t) : unit =\n";
                Printf.bprintf source "  %s\n" attack;
                Printf.bprintf source "[@@verocaml.%s]\n\n" category;
                Buffer.add_string source "let flow (source : Box.t) : unit =\n";
                Printf.bprintf source "  %s\n" setup;
                Buffer.add_string source
                  "  let[@ghost] _observed = (consume actual [@ghost]) in\n  ()\n");
              let name =
                String.concat "_" [ category; actual_name; attack_name ] ^ ".ml"
              in
              write_file (Filename.concat destination name)
                (Buffer.contents source))
            attacks)
        actuals)
    [ "spec"; "proof" ]

let () =
  if Array.length Sys.argv < 3 then (
    prerr_endline
      "usage: generate_unannotated_authority_matrix.exe DESTINATION AUTHORITY_PRELUDE";
    exit 1);
  generate Sys.argv.(1) Sys.argv.(2)
