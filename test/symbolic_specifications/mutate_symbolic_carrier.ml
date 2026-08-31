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
  marker_start : int;
  marker_stop : int;
  path_start : int;
  path_stop : int;
  span_start : int;
  span_stop : int;
  type_vector_start : int;
  type_vector_stop : int;
}

let find_carrier payload =
  let prefix = "v1|symbolic." in
  let rec loop from =
    match find_literal ~from payload prefix with
    | None -> None
    | Some start ->
        let marker_start = start + 3 in
        let hexadecimal = start + String.length prefix in
        let marker_stop = hexadecimal + 32 in
        if
          has_hexadecimal payload hexadecimal 32
          && marker_stop + 1 < Bytes.length payload
          && Bytes.get payload marker_stop = '|'
          && is_identifier_start (Bytes.get payload (marker_stop + 1))
        then
          let path_start = marker_stop + 1 in
          let path_stop =
            skip_while is_identifier_continue payload (path_start + 1)
          in
          if path_stop < Bytes.length payload && Bytes.get payload path_stop = '|'
          then
            let span_start = path_stop + 1 in
            match decimal_before_bar payload span_start with
            | None -> loop (start + 1)
            | Some span_stop ->
                let second_span_start = span_stop + 1 in
                (match decimal_before_bar payload second_span_start with
                | None -> loop (start + 1)
                | Some second_span_stop ->
                    let type_vector_start = second_span_stop + 1 in
                    let type_vector_stop = type_vector_start + 32 in
                    if
                      has_hexadecimal payload type_vector_start 32
                    then
                      Some
                        {
                          marker_start;
                          marker_stop;
                          path_start;
                          path_stop;
                          span_start;
                          span_stop;
                          type_vector_start;
                          type_vector_stop;
                        }
                    else loop (start + 1))
          else loop (start + 1)
        else loop (start + 1)
  in
  loop 0

let is_mutable = is_hexadecimal

let replace_group attack label payload start stop =
  let rec find_last index =
    if index < start then None
    else if is_mutable (Bytes.get payload index) then Some index
    else find_last (index - 1)
  in
  match find_last (stop - 1) with
  | None -> fail "%s: %s has no mutable byte" attack label
  | Some index ->
      Bytes.set payload index
        (if Bytes.get payload index = '0' then '1' else '0')

let require_carrier attack label payload =
  match find_carrier payload with
  | Some carrier -> carrier
  | None -> fail "%s: no %s found" attack label

let () =
  let source, target, attack =
    match Sys.argv with
    | [| _; source; target; attack |] -> (source, target, attack)
    | _ -> fail "usage: mutate_symbolic_carrier SOURCE TARGET ATTACK"
  in
  let payload = Bytes.of_string (read_file source) in
  (match attack with
  | "marker" ->
      let carrier = require_carrier attack "marker" payload in
      replace_group attack "marker" payload carrier.marker_start
        carrier.marker_stop
  | "type-vector" ->
      let carrier = require_carrier attack "type vector" payload in
      replace_group attack "type vector" payload carrier.type_vector_start
        carrier.type_vector_stop
  | "span" ->
      let carrier = require_carrier attack "source span" payload in
      replace_group attack "source span" payload carrier.span_start
        carrier.span_stop
  | "path" ->
      let carrier = require_carrier attack "declaration path" payload in
      Bytes.set payload carrier.path_start
        (if Bytes.get payload carrier.path_start = 'z' then 'y' else 'z')
  | "uid" ->
      let carrier = require_carrier attack "declaration UID anchor" payload in
      let name =
        Bytes.sub_string payload carrier.path_start
          (carrier.path_stop - carrier.path_start)
      in
      (match find_literal payload name with
      | Some occurrence when occurrence < carrier.path_start ->
          Bytes.set payload occurrence
            (if name.[0] = 'z' then 'y' else 'z')
      | _ -> fail "%s: no independent declaration UID/name found" attack)
  | _ -> fail "unknown attack %s" attack);
  write_file target payload
