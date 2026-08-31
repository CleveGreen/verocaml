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
  let first_start = offset in
  let* first_stop = consume_nonempty is_decimal payload first_start in
  let* second_start = consume_literal payload first_stop "," in
  let* second_stop = consume_nonempty is_decimal payload second_start in
  Some ((first_start, first_stop), (second_start, second_stop), second_stop)

type manifest = {
  stop : int;
  callable_start : int;
  callable_stop : int;
  binding_start : int;
  binding_stop : int;
  region_start : int;
  region_stop : int;
  slots_start : int;
  slots_stop : int;
}

let parse_manifest payload start =
  let* after_prefix =
    consume_literal payload start
      "verocaml:proof-region-capture:1:issuer=ppx-v1"
  in
  let* callable_start = consume_literal payload after_prefix "|callable=" in
  let* callable_stop =
    consume_nonempty is_hexadecimal payload callable_start
  in
  let* binding_start = consume_literal payload callable_stop "|binding=" in
  let* (binding, _, after_binding) = consume_span payload binding_start in
  let binding_start, binding_stop = binding in
  let* body_start = consume_literal payload after_binding "|body=" in
  let* (_, _, after_body) = consume_span payload body_start in
  let* region_start = consume_literal payload after_body "|region=" in
  let* (region, _, after_region) = consume_span payload region_start in
  let region_start, region_stop = region in
  let* slots_start = consume_literal payload after_region "|slots=" in
  let slots_stop = skip_while is_slot_character payload slots_start in
  Some
    {
      stop = slots_stop;
      callable_start;
      callable_stop;
      binding_start;
      binding_stop;
      region_start;
      region_stop;
      slots_start;
      slots_stop;
    }

let find_manifests payload =
  let rec loop offset matches =
    if offset >= Bytes.length payload then List.rev matches
    else
      match parse_manifest payload offset with
      | Some manifest -> loop manifest.stop (manifest :: matches)
      | None -> loop (offset + 1) matches
  in
  loop 0 []

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
  if String.length needle <> String.length replacement then
    fail "mutation changed serialized string length";
  let count = count_occurrences payload needle in
  if count = 0 then fail "missing mutation target %S" needle;
  let length = String.length needle in
  let rec loop offset =
    if offset + length > Bytes.length payload then ()
    else if has_literal payload offset needle then (
      Bytes.blit_string replacement 0 payload offset length;
      loop (offset + length))
    else loop (offset + 1)
  in
  loop 0

let sub_string payload start stop = Bytes.sub_string payload start (stop - start)

let toggle_first value =
  let changed = Bytes.of_string value in
  Bytes.set changed 0 (if Bytes.get changed 0 = '0' then '1' else '0');
  Bytes.to_string changed

let toggle_last value =
  let changed = Bytes.of_string value in
  let last = Bytes.length changed - 1 in
  Bytes.set changed last (if Bytes.get changed last = '0' then '1' else '0');
  Bytes.to_string changed

let replace_first_semicolon slots =
  let changed = Bytes.of_string slots in
  match Bytes.index_opt changed ';' with
  | Some index ->
      Bytes.set changed index ':';
      Bytes.to_string changed
  | None -> fail "missing-slot needs two slots"

let () =
  if Array.length Sys.argv < 4 then
    fail "usage: mutate_carrier SOURCE TARGET ATTACK";
  let source = Sys.argv.(1) in
  let target = Sys.argv.(2) in
  let attack = Sys.argv.(3) in
  let payload = read_file source in
  let manifests = find_manifests payload in
  let manifest =
    match manifests with
    | [ manifest ] -> manifest
    | manifests ->
        fail "expected one shared manifest, found %d" (List.length manifests)
  in
  let slots_text = sub_string payload manifest.slots_start manifest.slots_stop in
  let slots = if slots_text = "" then [] else String.split_on_char ';' slots_text in
  (match attack with
  | "wrong-callable" ->
      let callable =
        sub_string payload manifest.callable_start manifest.callable_stop
      in
      replace_all payload ("callable=" ^ callable)
        ("callable=" ^ toggle_first callable)
  | "wrong-binding-span" ->
      let start = sub_string payload manifest.binding_start manifest.binding_stop in
      replace_all payload ("binding=" ^ start) ("binding=" ^ toggle_last start)
  | "wrong-region-id" ->
      let start = sub_string payload manifest.region_start manifest.region_stop in
      let region_id = "verocaml:proof-region:2:" ^ start ^ ":" in
      let region_end_start = manifest.region_stop + 1 in
      let region_end_stop = skip_while is_decimal payload region_end_start in
      let region_id =
        region_id ^ sub_string payload region_end_start region_end_stop
      in
      replace_all payload region_id (toggle_last region_id)
  | "missing-slot" ->
      if List.length slots < 2 then fail "missing-slot needs two slots";
      replace_all payload ("slots=" ^ slots_text)
        ("slots=" ^ replace_first_semicolon slots_text)
  | "reordered-slots" -> (
      match slots with
      | [ first; second ] when String.length first = String.length second ->
          replace_all payload ("slots=" ^ slots_text)
            ("slots=" ^ second ^ ";" ^ first)
      | _ -> fail "reordered-slots needs two equal-length slots")
  | "duplicate-slot" -> (
      match slots with
      | [ first; second ] when String.length first = String.length second ->
          replace_all payload ("slots=" ^ slots_text)
            ("slots=" ^ first ^ ";" ^ first)
      | _ -> fail "duplicate-slot needs two equal-length slots")
  | _ -> fail "unknown attack %s" attack);
  write_file target payload
