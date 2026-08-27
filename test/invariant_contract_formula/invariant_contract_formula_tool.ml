let lines filename =
  let input = open_in filename in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let rec read collected =
        match input_line input with
        | line -> read (line :: collected)
        | exception End_of_file -> List.rev collected
      in
      read [])

let starts prefix value = String.starts_with ~prefix value
let normalize_symbols = Str.global_replace (Str.regexp "\\$[0-9]+") "$N"

type obligation = {
  function_name : string;
  index : int;
  kind : string;
  span : string;
  mutable path_lines : string list;
  mutable body_lines : string list;
  mutable in_path : bool;
  mutable has_path : bool;
}

let parse_vir filename =
  let header = Str.regexp "^  vc \\([0-9]+\\) \\([^ ]+\\).* @ \\(.*\\)$" in
  let function_name = ref "<none>" in
  let pending = ref None in
  let obligations = ref [] in
  let finish () =
    Option.iter (fun current -> obligations := current :: !obligations) !pending;
    pending := None
  in
  let source = lines filename in
  List.iter
    (fun line ->
      if starts "function " line then (
        finish ();
        function_name := List.nth (String.split_on_char ' ' line) 1)
      else if starts "  vc " line then (
        finish ();
        if not (Str.string_match header line 0) then
          failwith ("malformed VIR obligation header: " ^ line);
        let index = int_of_string (Str.matched_group 1 line) in
        let kind = Str.matched_group 2 line in
        let span = Str.matched_group 3 line in
        pending := Some { function_name = !function_name; index; kind; span; path_lines = []; body_lines = []; in_path = false; has_path = false }
      ) else
        Option.iter
          (fun current ->
            current.body_lines <- line :: current.body_lines;
            if line = "    path" && not current.has_path then (
              current.in_path <- true;
              current.has_path <- true)
            else if current.in_path && starts "      " line then (
              let term = String.trim line in
              if term <> "(none)" then
                current.path_lines <- term :: current.path_lines)
            else if starts "    " line then current.in_path <- false)
          !pending)
    source;
  finish ();
  (source, List.rev !obligations)

let validate_boundaries ~print_rows filename =
  let _, obligations = parse_vir filename in
  let keys = Hashtbl.create (List.length obligations) in
  let indices = Hashtbl.create (List.length obligations) in
  List.iter
    (fun obligation ->
      let path = String.concat " && " (List.rev obligation.path_lines) in
      let key = String.concat "\000" [ obligation.function_name; obligation.kind; obligation.span; path ] in
      let index_key = Printf.sprintf "%s\000%d" obligation.function_name obligation.index in
      if Hashtbl.mem keys key then failwith "duplicate VIR boundary key";
      if Hashtbl.mem indices index_key then failwith "duplicate VIR function/obligation index";
      Hashtbl.add keys key ();
      Hashtbl.add indices index_key ();
      if print_rows then
        Printf.printf "obligation-index=%d function=%s kind=%s span=%s path=%S\n"
          obligation.index obligation.function_name obligation.kind obligation.span
          (normalize_symbols path))
    obligations;
  Printf.printf "boundaries=%d unique-keys=%d unique-function-indices=%d\n"
    (List.length obligations) (Hashtbl.length keys) (Hashtbl.length indices)

let report_vir filename =
  let source, obligations = parse_vir filename in
  let goals = List.fold_left (fun count line -> if starts "    goal " line then count + 1 else count) 0 source in
  let unique = List.map (fun obligation -> normalize_symbols (String.concat "\n" (List.rev obligation.body_lines))) obligations |> List.sort_uniq String.compare |> List.length in
  let groups = Hashtbl.create 8 in
  List.iter (fun obligation -> Hashtbl.replace groups obligation.function_name (1 + Option.value ~default:0 (Hashtbl.find_opt groups obligation.function_name))) obligations;
  Printf.printf "headers=%d goals=%d normalized-duplicates=%d\n" (List.length obligations) goals (List.length obligations - unique);
  Hashtbl.to_seq groups |> List.of_seq |> List.sort compare |> List.iter (fun (name, count) -> Printf.printf "%s=%d\n" name count);
  validate_boundaries ~print_rows:false filename

let normalize input output =
  let replace regexp replacement = Str.global_replace (Str.regexp regexp) replacement in
  let channel = open_out output in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
      lines input |> List.iter (fun line ->
        let line = replace "body=checked-typedtree:[^ ]+\\|body=checked-raw:[^ ]+" "body=checked-typedtree:<input>" line in
        let line = if starts "authority=retained " line then "authority=retained unit=<input> source=<input> cmt=<input> interface=<provenance>" else line in
        let line = replace "snapshot=[0-9a-f]+" "snapshot=<provenance>" line |> replace "[^ :]+/path_product\\.ml" "path_product.ml" in
        output_string channel line;
        output_char channel '\n'))

let () =
  match Array.to_list Sys.argv with
  | [ _; "vir"; filename ] -> report_vir filename
  | [ _; "boundaries"; filename ] -> validate_boundaries ~print_rows:true filename
  | [ _; "boundary-check"; filename ] -> validate_boundaries ~print_rows:false filename
  | [ _; "normalize"; input; output ] -> normalize input output
  | _ -> failwith "usage: invariant_contract_formula_tool (vir|boundaries|boundary-check|normalize) ..."
