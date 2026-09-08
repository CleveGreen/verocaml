let fail message =
  prerr_endline message;
  exit 2

let rec parse profile logical_bv_width_permission layouts = function
  | [] -> (profile, logical_bv_width_permission, layouts)
  | "--profile" :: value :: rest ->
      if Option.is_some profile || value = "" then
        fail "invalid or duplicate build target profile"
      else parse (Some value) logical_bv_width_permission layouts rest
  | "--logical-bv-widths" :: value :: rest ->
      if Option.is_some logical_bv_width_permission || value <> "positive" then
        fail "invalid or duplicate logical BV width permission"
      else parse profile (Some value) layouts rest
  | "--layout" :: layout_class :: layout_abi :: width :: signed :: rest ->
      if layout_class = "" || layout_abi = "" then
        fail "build target layout identity is empty";
      let width =
        match int_of_string_opt width with
        | Some width when width > 0 -> width
        | Some _ | None -> fail "invalid build target layout width"
      in
      let signed =
        match bool_of_string_opt signed with
        | Some signed -> signed
        | None -> fail "invalid build target layout signedness"
      in
      parse profile logical_bv_width_permission
        ((layout_class, layout_abi, width, signed) :: layouts)
        rest
  | _ -> fail "malformed build target generator arguments"

let () =
  let arguments = Array.to_list Sys.argv |> List.tl in
  let profile, logical_bv_width_permission, layouts =
    parse None None [] arguments
  in
  let profile =
    match profile with
    | Some profile -> profile
    | None -> fail "missing build target profile"
  in
  let layouts = List.sort compare layouts in
  let logical_bv_width_permission =
    match logical_bv_width_permission with
    | Some value -> value
    | None -> fail "missing logical BV width permission"
  in
  if layouts = [] then fail "build target profile has no layouts";
  if List.length layouts <> List.length (List.sort_uniq compare layouts) then
    fail "build target profile has duplicate complete layouts";
  Printf.printf "let producer_schema = %S\n"
    "verocaml.build-produced-target-matrix.v2";
  Printf.printf "let profile_identity = %S\n" profile;
  Printf.printf "let logical_bv_width_permission = %S\n"
    logical_bv_width_permission;
  print_endline "let layouts =";
  print_endline "  [";
  List.iter
    (fun (layout_class, layout_abi, width, signed) ->
      Printf.printf "    (%S, %S, %d, %b);\n" layout_class layout_abi width
        signed)
    layouts;
  print_endline "  ]"
