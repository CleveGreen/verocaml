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
    (fun () ->
      Bytes.of_string
        (really_input_string channel (in_channel_length channel)))

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

let skip_while predicate payload offset =
  let rec loop index =
    if index < Bytes.length payload && predicate (Bytes.get payload index) then
      loop (index + 1)
    else index
  in
  loop offset

let consume_literal payload offset literal =
  if has_literal payload offset literal then Some (offset + String.length literal)
  else None

let consume_nonempty predicate payload offset =
  let stop = skip_while predicate payload offset in
  if stop = offset then None else Some stop

let is_decimal = function '0' .. '9' -> true | _ -> false

let is_hexadecimal = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let is_slot_character = function
  | '0' .. '9' | 'a' .. 'f' | ',' | ';' -> true
  | _ -> false

let ( let* ) value continuation =
  match value with Some value -> continuation value | None -> None

let consume_span payload offset =
  let* comma = consume_nonempty is_decimal payload offset in
  let* second = consume_literal payload comma "," in
  consume_nonempty is_decimal payload second

type carrier = {
  start : int;
  stop : int;
}

let parse_local_carrier payload start =
  let* after_prefix =
    consume_literal payload start
      "verocaml:proof-region-capture:1:issuer=ppx-v1"
  in
  let* callable = consume_literal payload after_prefix "|callable=" in
  let* after_callable = consume_nonempty is_hexadecimal payload callable in
  let* binding = consume_literal payload after_callable "|binding=" in
  let* after_binding = consume_span payload binding in
  let* body = consume_literal payload after_binding "|body=" in
  let* after_body = consume_span payload body in
  let* region = consume_literal payload after_body "|region=" in
  let* after_region = consume_span payload region in
  let* slots = consume_literal payload after_region "|slots=" in
  let after_slots = skip_while is_slot_character payload slots in
  let* after_kind =
    consume_literal payload after_slots "|kind=local-assert"
  in
  let* ordinal = consume_literal payload after_kind "|ordinal=" in
  let* after_ordinal = consume_nonempty is_decimal payload ordinal in
  let* predicate = consume_literal payload after_ordinal "|predicate=" in
  let* stop = consume_span payload predicate in
  Some { start; stop }

let find_carriers payload =
  let rec loop offset matches =
    if offset >= Bytes.length payload then List.rev matches
    else
      match parse_local_carrier payload offset with
      | Some carrier -> loop carrier.stop (carrier :: matches)
      | None -> loop (offset + 1) matches
  in
  loop 0 []

let find_fields payload prefix consume_value =
  let rec loop offset matches =
    if offset >= Bytes.length payload then List.rev matches
    else
      match consume_literal payload offset prefix with
      | Some value_start -> (
          match consume_value payload value_start with
          | Some stop -> loop stop ({ start = offset; stop } :: matches)
          | None -> loop (offset + 1) matches)
      | None -> loop (offset + 1) matches
  in
  loop 0 []

let find_literal_fields payload literal =
  find_fields payload literal (fun _ offset -> Some offset)

let find_nonempty_fields payload prefix predicate =
  find_fields payload prefix (consume_nonempty predicate)

let count_occurrences payload literal =
  let literal_length = String.length literal in
  let rec loop offset count =
    if offset + literal_length > Bytes.length payload then count
    else if has_literal payload offset literal then
      loop (offset + literal_length) (count + 1)
    else loop (offset + 1) count
  in
  loop 0 0

let replace_all payload needle replacement =
  let length = String.length needle in
  let rec loop offset =
    if offset + length > Bytes.length payload then ()
    else if has_literal payload offset needle then (
      Bytes.blit_string replacement 0 payload offset length;
      loop (offset + length))
    else loop (offset + 1)
  in
  loop 0

let replace_once attack payload needle replacement =
  let count = count_occurrences payload needle in
  if String.length needle <> String.length replacement || count <> 1 then
    fail "%s: expected one same-length marker, found %d" attack count;
  replace_all payload needle replacement

let mutate_local_carrier attack payload consume_fields =
  let carriers = find_carriers payload in
  let carrier =
    match carriers with
    | [ carrier ] -> carrier
    | carriers ->
        fail "%s: expected one exact local carrier, found %d" attack
          (List.length carriers)
  in
  let original = Bytes.sub payload carrier.start (carrier.stop - carrier.start) in
  let fields = consume_fields original in
  let field =
    match fields with
    | [ field ] -> field
    | fields ->
        fail "%s: expected one exact local-carrier field, found %d" attack
          (List.length fields)
  in
  let mutated = Bytes.copy original in
  let last = field.stop - 1 in
  Bytes.set mutated last (if Bytes.get mutated last = '0' then '1' else '0');
  replace_once attack payload (Bytes.to_string original) (Bytes.to_string mutated)

let () =
  if Array.length Sys.argv < 3 then
    fail "usage: mutate_local_carrier SOURCE TARGET [ATTACK]";
  let source = Sys.argv.(1) in
  let target = Sys.argv.(2) in
  let attack = if Array.length Sys.argv > 3 then Sys.argv.(3) else "ordinal" in
  let payload = read_file source in
  (match attack with
  | "local-id" ->
      replace_once attack payload "verocaml:local-assert:1:"
        "verocaml:local-asserx:1:"
  | "ordinal" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_nonempty_fields carrier "|ordinal=" is_decimal)
  | "kind" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_literal_fields carrier "|kind=local-assert")
  | "predicate-span" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_fields carrier "|predicate=" consume_span)
  | "callable" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_nonempty_fields carrier "|callable=" is_hexadecimal)
  | "body" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_fields carrier "|body=" consume_span)
  | "region" ->
      mutate_local_carrier attack payload (fun carrier ->
          find_fields carrier "|region=" consume_span)
  | _ -> fail "unknown attack %s" attack);
  write_file target payload
