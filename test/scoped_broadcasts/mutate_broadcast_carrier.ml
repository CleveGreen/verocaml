let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 1)
    format

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path payload =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_bytes channel payload)

let has_literal payload offset literal =
  let literal_length = String.length literal in
  let rec loop index =
    if index = literal_length then true
    else if Bytes.get payload (offset + index) <> literal.[index] then false
    else loop (index + 1)
  in
  offset >= 0
  && offset + literal_length <= Bytes.length payload
  && loop 0

let find_literal ?(from = 0) payload literal =
  let last = Bytes.length payload - String.length literal in
  let rec loop offset =
    if offset > last then None
    else if has_literal payload offset literal then Some offset
    else loop (offset + 1)
  in
  loop from

let is_decimal = function '0' .. '9' -> true | _ -> false

let is_hexadecimal = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let is_identifier_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_identifier_continue = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let skip_while predicate payload offset =
  let rec loop index =
    if index < Bytes.length payload && predicate (Bytes.get payload index) then
      loop (index + 1)
    else index
  in
  loop offset

let has_hexadecimal payload offset length =
  let rec loop index =
    index = length
    ||
    (offset + index < Bytes.length payload
    && is_hexadecimal (Bytes.get payload (offset + index))
    && loop (index + 1))
  in
  loop 0

let decimal_before_bar payload offset =
  if offset >= Bytes.length payload || not (is_decimal (Bytes.get payload offset))
  then None
  else
    let stop = skip_while is_decimal payload offset in
    if stop < Bytes.length payload && Bytes.get payload stop = '|' then
      Some stop
    else None

type carrier = {
  identity_last : int;
  span_stop : int;
}

let find_declaration payload =
  let prefix = "declaration." in
  let rec loop from =
    match find_literal ~from payload prefix with
    | None -> None
    | Some start ->
        let hexadecimal = start + String.length prefix in
        let separator = hexadecimal + 32 in
        if
          has_hexadecimal payload hexadecimal 32
          && separator < Bytes.length payload
          && Bytes.get payload separator = '|'
        then
          let span_start = separator + 1 in
          (match decimal_before_bar payload span_start with
          | Some span_stop
            when span_stop + 1 < Bytes.length payload
                 && is_decimal (Bytes.get payload (span_stop + 1)) ->
              Some
                {
                  identity_last = separator - 1;
                  span_stop;
                }
          | _ -> loop (start + 1))
        else loop (start + 1)
  in
  loop 0

let find_structure payload =
  let prefix = "structure|structure." in
  let rec loop from =
    match find_literal ~from payload prefix with
    | None -> None
    | Some start ->
        let hexadecimal = start + String.length prefix in
        let separator = hexadecimal + 32 in
        if
          has_hexadecimal payload hexadecimal 32
          && separator + 1 < Bytes.length payload
          && Bytes.get payload separator = '|'
          && Bytes.get payload (separator + 1) = '|'
        then
          let span_start = separator + 2 in
          (match decimal_before_bar payload span_start with
          | Some span_stop
            when span_stop + 1 < Bytes.length payload
                 && is_decimal (Bytes.get payload (span_stop + 1)) ->
              Some
                {
                  identity_last = separator - 1;
                  span_stop;
                }
          | _ -> loop (start + 1))
        else loop (start + 1)
  in
  loop 0

let find_group payload =
  let prefix = "group|group." in
  let rec loop from =
    match find_literal ~from payload prefix with
    | None -> None
    | Some start ->
        let hexadecimal = start + String.length prefix in
        let separator = hexadecimal + 32 in
        if
          has_hexadecimal payload hexadecimal 32
          && separator + 1 < Bytes.length payload
          && Bytes.get payload separator = '|'
          && is_identifier_start (Bytes.get payload (separator + 1))
        then
          let name_stop =
            skip_while is_identifier_continue payload (separator + 2)
          in
          if
            name_stop < Bytes.length payload
            && Bytes.get payload name_stop = '|'
          then
            let span_start = name_stop + 1 in
            (match decimal_before_bar payload span_start with
            | Some span_stop
              when span_stop + 1 < Bytes.length payload
                   && is_decimal (Bytes.get payload (span_stop + 1)) ->
                Some
                  {
                    identity_last = separator - 1;
                    span_stop;
                  }
            | _ -> loop (start + 1))
          else loop (start + 1)
        else loop (start + 1)
  in
  loop 0

let find_expression payload =
  let prefix = "expression." in
  let rec loop from =
    match find_literal ~from payload prefix with
    | None -> None
    | Some start ->
        let hexadecimal = start + String.length prefix in
        let separator = hexadecimal + 32 in
        if
          has_hexadecimal payload hexadecimal 32
          && separator < Bytes.length payload
          && Bytes.get payload separator = '|'
        then
          let span_start = separator + 1 in
          (match decimal_before_bar payload span_start with
          | Some span_stop
            when span_stop + 1 < Bytes.length payload
                 && is_decimal (Bytes.get payload (span_stop + 1)) ->
              Some
                {
                  identity_last = separator - 1;
                  span_stop;
                }
          | _ -> loop (start + 1))
        else loop (start + 1)
  in
  loop 0

let toggle_zero payload index =
  Bytes.set payload index (if Bytes.get payload index = '0' then '1' else '0')

let mutate_identity attack label find payload =
  match find payload with
  | None -> fail "%s: no %s carrier found" attack label
  | Some carrier -> toggle_zero payload carrier.identity_last

let mutate_span attack label find payload =
  match find payload with
  | None -> fail "%s: no %s span found" attack label
  | Some carrier -> toggle_zero payload (carrier.span_stop - 1)

let replace_first attack payload old_value new_value absent_message =
  match find_literal payload old_value with
  | None -> fail "%s: %s" attack absent_message
  | Some start ->
      if String.length old_value <> String.length new_value then
        fail "%s: %s" attack absent_message;
      Bytes.blit_string new_value 0 payload start (String.length new_value)

let () =
  let source, target, attack =
    match Sys.argv with
    | [| _; source; target; attack |] -> (source, target, attack)
    | _ -> fail "usage: mutate_broadcast_carrier SOURCE TARGET ATTACK"
  in
  let payload = Bytes.of_string (read_file source) in
  (match attack with
  | "declaration-id" ->
      mutate_identity attack "declaration" find_declaration payload
  | "carrier-id" -> mutate_identity attack "structure" find_structure payload
  | "group-id" -> mutate_identity attack "group" find_group payload
  | "marker" ->
      replace_first attack payload "verocaml:broadcast:carrier:v1:"
        "verocaml:broadcast:carriem:v1:" "marker target is absent"
  | "declaration-span" ->
      mutate_span attack "declaration" find_declaration payload
  | "carrier-span" -> mutate_span attack "structure" find_structure payload
  | "group-span" -> mutate_span attack "group" find_group payload
  | "scope-span" ->
      mutate_span attack "expression scope" find_expression payload
  | "legacy-lemma" ->
      replace_first attack payload "declaration." "lemma|lemma."
        "neutral declaration carrier is absent"
  | "legacy-axiom" ->
      replace_first attack payload "declaration." "axiom|axiom."
        "neutral declaration carrier is absent"
  | _ -> fail "unknown attack %s" attack);
  write_file target payload
