let fail format = Printf.ksprintf failwith format

let self_crc filename =
  let cmi = Cmi_format.read_cmi filename in
  let name = cmi.Cmi_format.cmi_name in
  let matching =
    Array.to_list cmi.Cmi_format.cmi_crcs
    |> List.filter (fun import ->
           Compilation_unit.Name.equal (Import_info.name import) name)
  in
  match matching with
  | [ import ] -> (
      match Import_info.crc import with
      | Some crc -> Digest.to_hex crc
      | None -> fail "%s has no self CRC" filename)
  | matches ->
      fail "%s has %d self CRC entries" filename (List.length matches)

let type_uid filename type_name =
  let cmi = Cmi_format.read_cmi filename in
  match
    List.find_map
      (function
        | Types.Sig_type (id, declaration, _, _)
          when String.equal (Ident.name id) type_name ->
            Some (Format.asprintf "%a" Types.Uid.print declaration.Types.type_uid)
        | _ -> None)
      cmi.Cmi_format.cmi_sign
  with
  | Some uid -> uid
  | None -> fail "%s has no type %s" filename type_name

let () =
  match Array.to_list Sys.argv with
  | [ _; stdlib; effect; domain; ghost; output ] ->
      let channel = open_out output in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () ->
          Printf.fprintf channel
            "let stdlib_crc = %S\nlet stdlib_result_uid = %S\nlet effect_crc = %S\nlet domain_crc = %S\nlet ghost_crc = %S\n"
            (self_crc stdlib) (type_uid stdlib "result") (self_crc effect)
            (self_crc domain) (self_crc ghost))
  | _ ->
      fail
        "usage: trusted_import_generator STDLIB EFFECT DOMAIN GHOST OUTPUT"
