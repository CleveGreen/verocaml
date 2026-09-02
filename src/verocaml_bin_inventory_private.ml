type role = Root | Dependency

type entry = {
  role : role;
  cmt : string;
  cmi : string;
  cmti : string option;
  vri : string option;
}

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let read_file filename =
  try
    let channel = open_in_bin filename in
    Ok
      (Fun.protect
         ~finally:(fun () -> close_in_noerr channel)
         (fun () -> really_input_string channel (in_channel_length channel)))
  with Sys_error message -> Error message

let absolute base path =
  if Filename.is_relative path then Filename.concat base path else path

let read (filename [@delator.field Fun.id]) =
  let* contents = read_file filename in
  match String.split_on_char '\n' contents with
  | "verocaml-artifact-inventory-v1" :: lines ->
      let base = Filename.dirname filename in
      let rec entries line_number reversed = function
        | [] -> Ok (List.rev reversed)
        | "" :: rest -> entries (line_number + 1) reversed rest
        | line :: rest -> (
            match String.split_on_char '\t' line with
            | [ role; cmt; cmi; cmti; vri ] ->
                let role =
                  match role with
                  | "root" -> Some Root
                  | "dependency" -> Some Dependency
                  | _ -> None
                in
                let optional path =
                  if String.equal path "-" then None else Some (absolute base path)
                in
                (match role with
                | Some role when cmt <> "" && cmi <> "" ->
                    entries (line_number + 1)
                      ({
                         role;
                         cmt = absolute base cmt;
                         cmi = absolute base cmi;
                         cmti = optional cmti;
                         vri = optional vri;
                       }
                      :: reversed)
                      rest
                | Some _ | None ->
                    Error
                      (Printf.sprintf
                         "invalid artifact inventory entry at line %d"
                         line_number))
            | _ ->
                Error
                  (Printf.sprintf "invalid artifact inventory entry at line %d"
                     line_number))
      in
      let* entries = entries 2 [] lines in
      if entries = [] then Error "artifact inventory is empty"
      else (
        [%log.info "decoded explicit artifact inventory"
          ~stage:(Delator.Field.string "inventory-manifest")
          ~route:(Delator.Field.string "explicit-manifest")
          ~entry_count:(Delator.Field.int (List.length entries))
          ~decision:(Delator.Field.string "accepted")];
        Ok entries)
  | _ ->
      [%log.warn "rejected explicit artifact inventory"
        ~stage:(Delator.Field.string "inventory-manifest")
        ~route:(Delator.Field.string "explicit-manifest")
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "manifest-version")];
      Error "artifact inventory has an unsupported or missing version"
[@@delator.instrument]
