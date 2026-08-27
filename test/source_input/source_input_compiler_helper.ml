let argument_after option =
  let rec find = function
    | [] | [ _ ] -> None
    | candidate :: value :: _ when String.equal candidate option -> Some value
    | _ :: rest -> find rest
  in
  find (Array.to_list Sys.argv)

let write_residual_cmt output =
  let filename = Filename.remove_extension output ^ ".cmt" in
  let channel = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel "residual CMT must not be consumed\n")

let controlled_exit () =
  let output = Option.value ~default:"<missing>" (argument_after "-o") in
  let input = Option.value ~default:"<missing>" (argument_after "-impl") in
  Printf.printf "controlled compiler stdout\n";
  Printf.eprintf "controlled compiler stderr\n";
  let temporary_mode =
    (Unix.stat (Filename.dirname output)).st_perm land 0o777
  in
  Printf.printf "temp-mode=%03o output=%s input=%s color=%s\n" temporary_mode
    (Filename.basename output) input
    (Option.value ~default:"unset" (Sys.getenv_opt "OCAML_COLOR"));
  write_residual_cmt output;
  exit 7

let controlled_signal () =
  Printf.eprintf "controlled compiler signal\n";
  flush stderr;
  Unix.kill (Unix.getpid ()) Sys.sigterm;
  exit 125

let () =
  match Sys.getenv_opt "VEROCAML_TEST_COMPILER_OUTCOME" with
  | Some "exit" -> controlled_exit ()
  | Some "signal" -> controlled_signal ()
  | Some outcome ->
      Printf.eprintf "unknown controlled compiler outcome %S\n" outcome;
      exit 64
  | None ->
      Printf.eprintf "VEROCAML_TEST_COMPILER_OUTCOME is required\n";
      exit 64
