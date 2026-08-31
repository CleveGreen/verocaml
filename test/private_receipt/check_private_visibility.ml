let normalize_newlines text =
  let length = String.length text in
  let normalized = Buffer.create length in
  let rec copy index =
    if index < length then
      match text.[index] with
      | '\r' ->
          Buffer.add_char normalized '\n';
          copy
            (if index + 1 < length && text.[index + 1] = '\n' then index + 2
             else index + 1)
      | character ->
          Buffer.add_char normalized character;
          copy (index + 1)
  in
  copy 0;
  Buffer.contents normalized

let read_file path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let buffer = Buffer.create 4096 in
      let chunk = Bytes.create 4096 in
      let rec read () =
        match input channel chunk 0 (Bytes.length chunk) with
        | 0 -> normalize_newlines (Buffer.contents buffer)
        | length ->
            Buffer.add_subbytes buffer chunk 0 length;
            read ()
      in
      read ())

let find_from text literal start =
  let text_length = String.length text in
  let literal_length = String.length literal in
  let rec matches text_index literal_index =
    literal_index = literal_length
    ||
    (text.[text_index] = literal.[literal_index]
    && matches (text_index + 1) (literal_index + 1))
  in
  let rec search index =
    if index + literal_length > text_length then None
    else if matches index 0 then Some index
    else search (index + 1)
  in
  search start

let contains text literal = Option.is_some (find_from text literal 0)

let module_blocks text =
  let delimiter = "\n   (module\n" in
  let delimiter_length = String.length delimiter in
  let text_length = String.length text in
  let rec collect block_start =
    match find_from text delimiter block_start with
    | None -> [ String.sub text block_start (text_length - block_start) ]
    | Some delimiter_start ->
        String.sub text block_start (delimiter_start - block_start)
        :: collect (delimiter_start + delimiter_length)
  in
  match find_from text delimiter 0 with
  | None -> []
  | Some delimiter_start -> collect (delimiter_start + delimiter_length)

let fail_not_private authority =
  prerr_endline ("not-private:" ^ authority);
  exit 1

let () =
  let text = read_file Sys.argv.(1) in
  let blocks = module_blocks text in
  for index = 2 to Array.length Sys.argv - 1 do
    let authority = Sys.argv.(index) in
    let path = "(path " ^ authority ^ ")" in
    match List.filter (fun block -> contains block path) blocks with
    | [ block ] when contains block "(visibility private)" -> ()
    | _ -> fail_not_private authority
  done;
  Printf.printf "private-authority-modules=%d\n" (Array.length Sys.argv - 2)
