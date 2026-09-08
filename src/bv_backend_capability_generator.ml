let fail message =
  prerr_endline message;
  exit 2

let rec parse fields = function
  | [] -> fields
  | flag :: value :: rest when String.length flag > 2 && value <> "" ->
      if List.mem_assoc flag fields then fail ("duplicate argument " ^ flag)
      else parse ((flag, value) :: fields) rest
  | _ -> fail "malformed BV backend capability generator arguments"

let required name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> fail ("missing argument " ^ name)

let () =
  let fields = parse [] (Array.to_list Sys.argv |> List.tl) in
  let emit name flag =
    Printf.printf "let %s = %S\n" name (required flag fields)
  in
  emit "issuer_authority" "--issuer";
  emit "build_input_schema" "--build-input-schema";
  emit "z3_package_version" "--z3-package";
  emit "z3_portability_patch_sha256" "--z3-portability-patch-sha256";
  let maximum = required "--maximum-bv-width" fields in
  match int_of_string_opt maximum with
  | Some width when width > 0 ->
      Printf.printf "let maximum_bv_width = %d\n" width
  | Some _ | None -> fail "invalid maximum BV width"
