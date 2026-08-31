let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 1)
    format

let read_lines path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop lines =
        match input_line channel with
        | line -> loop (line :: lines)
        | exception End_of_file -> List.rev lines
      in
      loop [])

let starts_with prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let find_substring ?(from = 0) needle haystack =
  let needle_length = String.length needle in
  let last = String.length haystack - needle_length in
  let rec loop index =
    if index > last then None
    else if String.sub haystack index needle_length = needle then Some index
    else loop (index + 1)
  in
  if from < 0 then None else loop from

let find_row description prefix rows =
  match List.find_opt (starts_with prefix) rows with
  | Some row -> row
  | None -> fail "missing %s row with prefix %S" description prefix

let parse_fields row =
  match String.split_on_char ' ' row |> List.filter (( <> ) "") with
  | [] -> fail "cannot parse an empty structural row"
  | _kind :: fields ->
      List.map
        (fun field ->
          match String.index_opt field '=' with
          | Some separator when separator > 0 ->
              ( String.sub field 0 separator,
                String.sub field (separator + 1)
                  (String.length field - separator - 1) )
          | _ -> fail "malformed structural field %S" field)
        fields

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> fail "missing structural field %S" name

let declaration_block declaration rows =
  let prefix = "function " ^ declaration ^ "#" in
  let rec seek = function
    | [] -> fail "missing SST function block for %S" declaration
    | row :: rows -> if starts_with prefix row then row :: take rows else seek rows
  and take = function
    | row :: _ when starts_with "function " row -> []
    | row :: rows -> row :: take rows
    | [] -> []
  in
  seek rows

let has_authenticated_witness row =
  let provenance =
    "body trusted-external-body trust=axiomatic provenance=typedtree:"
  in
  let witness = " witness-span=" in
  match find_substring provenance row with
  | None -> false
  | Some provenance_start -> (
      let after_provenance = provenance_start + String.length provenance in
      match find_substring ~from:after_provenance witness row with
      | None -> false
      | Some witness_start ->
          let value_start = witness_start + String.length witness in
          value_start < String.length row
          &&
          match row.[value_start] with
          | ' ' | '\t' | '\r' | '\n' -> false
          | _ -> true)

let replace_all ~pattern ~replacement value =
  let pattern_length = String.length pattern in
  if pattern_length = 0 then value
  else
    let buffer = Buffer.create (String.length value) in
    let rec loop offset =
      match find_substring ~from:offset pattern value with
      | None ->
          Buffer.add_substring buffer value offset (String.length value - offset)
      | Some index ->
          Buffer.add_substring buffer value offset (index - offset);
          Buffer.add_string buffer replacement;
          loop (index + pattern_length)
    in
    loop 0;
    Buffer.contents buffer

let () =
  let structural, sst_path, function_name, declaration =
    match Sys.argv with
    | [| _; structural; sst_path; function_name; declaration |] ->
        (structural, sst_path, function_name, declaration)
    | _ ->
        fail
          "usage: trust_oracle.exe STRUCTURAL_OUTPUT SST FUNCTION DECLARATION"
  in
  let structural_rows = read_lines structural in
  let vc_fields =
    find_row "VC" ("vc function=" ^ function_name ^ " ") structural_rows
    |> parse_fields
  in
  let insertion_fields =
    find_row "insertion" ("insert id=broadcast:" ^ declaration ^ " ")
      structural_rows
    |> parse_fields
  in
  let sst_rows = read_lines sst_path in
  if
    not
      (declaration_block declaration sst_rows
      |> List.exists has_authenticated_witness)
  then fail "missing authenticated witness for %S" declaration;
  if
    not
      (String.equal (field "trusted" insertion_fields) "true"
      && not (String.equal (field "witness" insertion_fields) "none"))
  then fail "insertion for %S is not authenticated and trusted" declaration;
  let normalized_paths =
    replace_all
      ~pattern:("broadcast:" ^ declaration)
      ~replacement:"broadcast:<declaration>"
      (field "paths" insertion_fields)
  in
  Printf.printf
    "trust declaration=<declaration> declaration-span=<source-span> role=trusted \
     trusted=%s trusted-declarations=%s trusted-uses=%s inserted=%s ordinal=%s \
     vector=%s paths=%s witness=<source-span> \
     provenance=authenticated-external-proof\n"
    (field "trusted" insertion_fields)
    (field "trusted-declarations" vc_fields)
    (field "trusted-uses" vc_fields)
    (field "inserted" vc_fields)
    (field "ordinal" insertion_fields)
    (field "vector" insertion_fields) normalized_paths
