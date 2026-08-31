let axes =
  [
    ("areality", [ "global"; "regional" ]);
    ("forkable", [ "forkable"; "unforkable" ]);
    ("yielding", [ "unyielding"; "yielding" ]);
    ("linearity", [ "many"; "once" ]);
    ("statefulness", [ "stateless"; "observing"; "stateful" ]);
    ("portability", [ "portable"; "shareable"; "nonportable" ]);
    ("uniqueness", [ "unique"; "aliased" ]);
    ("visibility", [ "read_write"; "read"; "immutable" ]);
    ("contention", [ "uncontended"; "shared"; "contended" ]);
    ("staticity", [ "static"; "dynamic" ]);
  ]

let positions = [ "parameter"; "return"; "payload"; "direct"; "retained_import" ]

let source_mode axis state =
  (* Regionality has no source spelling at this compiler pin; a local function
     parameter is regional to its callee. *)
  if String.equal axis "areality" && String.equal state "regional" then "local"
  else state

let returning axis state expression =
  if String.equal axis "areality" && String.equal state "regional" then
    "exclave_ " ^ expression
  else expression

let identifier position axis state kind =
  String.concat "__" [ position; axis; state; kind ]

let add_line buffer line =
  Buffer.add_string buffer line;
  Buffer.add_char buffer '\n'

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
  let manifest = ref [] in
  let source = Buffer.create 24_000 in
  let provider_mli = Buffer.create 4_000 in
  let provider_ml = Buffer.create 4_000 in
  let client_ml = Buffer.create 7_000 in
  add_line provider_ml "[@@@verocaml.verify]";
  add_line client_ml "[@@@verocaml.verify]";
  List.iter
    (fun (axis, states) ->
      List.iter
        (fun state ->
          let mode = source_mode axis state in
          let annotated = "@ " ^ mode in
          let modal = "@@ " ^ mode in
          let argument = "inventory__" ^ axis ^ "__" ^ state in
          let parameter = identifier "parameter" axis state "annotated" in
          let parameter_twin = identifier "parameter" axis state "inferred" in
          add_line source
            (Printf.sprintf "let %s (%s : 'a %s) = %s" parameter argument
               annotated argument);
          add_line source
            (Printf.sprintf "let %s (value : 'a) = value" parameter_twin);
          let returned = identifier "return" axis state "annotated" in
          let returned_twin = identifier "return" axis state "inferred" in
          add_line source
            (Printf.sprintf "let %s (%s : 'a %s) : 'a %s = %s" returned
               argument annotated annotated argument);
          add_line source
            (Printf.sprintf "let %s (value : 'a) = value" returned_twin);
          let payload = identifier "payload" axis state "annotated" in
          let payload_twin = identifier "payload" axis state "inferred" in
          add_line source
            (Printf.sprintf "type 'a %s = %s of 'a %s" payload
               (String.capitalize_ascii payload) modal);
          add_line source
            (Printf.sprintf "type 'a %s = %s of 'a" payload_twin
               (String.capitalize_ascii payload_twin));
          let direct = identifier "direct" axis state "annotated" in
          let direct_twin = identifier "direct" axis state "inferred" in
          add_line source
            (Printf.sprintf "let %s (%s : 'a %s) = %s" direct argument annotated
               (returning axis state (Printf.sprintf "%s %s" parameter argument)));
          add_line source
            (Printf.sprintf "let %s (value : 'a) = %s value" direct_twin
               parameter_twin);
          let imported = identifier "retained_import" axis state "annotated" in
          let imported_twin =
            identifier "retained_import" axis state "inferred"
          in
          add_line provider_mli
            (Printf.sprintf "val %s : int %s -> int" imported annotated);
          add_line provider_mli
            (Printf.sprintf "val %s : int -> int" imported_twin);
          add_line provider_ml
            (Printf.sprintf "let %s (value : int %s) = value" imported annotated);
          add_line provider_ml
            (Printf.sprintf "let %s value = value" imported_twin);
          add_line client_ml
            (Printf.sprintf "let %s (value : int %s) = Mode_provider.%s value"
               imported annotated imported);
          add_line client_ml
            (Printf.sprintf "let %s value = Mode_provider.%s value" imported_twin
               imported_twin);
          List.iter
            (fun position ->
              List.iter
                (fun kind ->
                  manifest := identifier position axis state kind :: !manifest)
                [ "annotated"; "inferred" ])
            positions)
        states)
    axes;
  add_line source "let inferred_areality_local value =";
  add_line source
    "  let local_ inventory__areality__local = (value, value) in";
  add_line source
    "  match inventory__areality__local with _, _ -> value";
  let write name contents =
    write_file (Filename.concat destination name) contents
  in
  write "positions.ml" (Buffer.contents source);
  write "mode_provider.mli" (Buffer.contents provider_mli);
  write "mode_provider.ml" (Buffer.contents provider_ml);
  write "mode_client.ml" (Buffer.contents client_ml);
  write "manifest"
    (String.concat "\n" (List.sort String.compare !manifest) ^ "\n")

let () =
  match Array.to_list Sys.argv with
  | [ _; destination ] -> generate destination
  | _ ->
      prerr_endline "usage: generate_value_mode_matrix.exe DESTINATION";
      exit 1
