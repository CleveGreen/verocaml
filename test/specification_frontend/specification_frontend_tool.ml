open Typedtree

type call = {
  path : string;
  location : Location.t;
  argument_locations : Location.t list;
}

let fail format = Printf.ksprintf failwith format

let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let substring_offsets source substring =
  let source_length = String.length source in
  let substring_length = String.length substring in
  let rec find_from offset found =
    if offset + substring_length > source_length then found
    else if String.sub source offset substring_length = substring then
      find_from (offset + 1) (offset :: found)
    else find_from (offset + 1) found
  in
  match List.rev (find_from 0 []) with
  | [ start_offset ] -> (start_offset, start_offset + substring_length)
  | [] -> fail "expected substring not found: %s" substring
  | _ -> fail "expected substring is not unique: %s" substring

let location_offsets location =
  ( location.Location.loc_start.Lexing.pos_cnum,
    location.Location.loc_end.Lexing.pos_cnum )

let locations_in expression =
  let locations = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          locations := expression.exp_loc :: !locations;
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  !locations

let argument_expressions arguments =
  List.filter_map
    (function
      | _, Arg (expression, _) -> Some expression
      | _, Omitted _ -> None)
    arguments

let collect_calls structure =
  let calls = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_apply (callee, arguments, _, _, _) -> (
              match callee.exp_desc with
              | Texp_ident (path, _, _, _, _)
                when String.starts_with ~prefix:"Vero_ghost." (Path.name path)
                ->
                  let argument_locations =
                    argument_expressions arguments
                    |> List.concat_map locations_in
                  in
                  calls :=
                    {
                      path = Path.name path;
                      location = expression.exp_loc;
                      argument_locations;
                    }
                    :: !calls
              | _ -> ())
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  List.rev !calls

let find_call calls path =
  match List.filter (fun call -> String.equal call.path path) calls with
  | [ call ] -> call
  | matches -> fail "expected one %s call, found %d" path (List.length matches)

let check_span source description expected location =
  let expected_offsets = substring_offsets source expected in
  let actual_offsets = location_offsets location in
  if actual_offsets <> expected_offsets then
    fail "%s span mismatch: expected %d-%d, found %d-%d" description
      (fst expected_offsets) (snd expected_offsets) (fst actual_offsets)
      (snd actual_offsets)

let check_payload_span source call expected =
  let expected_offsets = substring_offsets source expected in
  if
    not
      (List.exists
         (fun location -> location_offsets location = expected_offsets)
         call.argument_locations)
  then
    fail "%s does not retain payload span %s" call.path expected

let () =
  if Array.length Sys.argv <> 4 then
    fail "usage: specification_frontend_tool (erased|retained) SOURCE CMT";
  let mode = Sys.argv.(1) in
  let source_filename = Sys.argv.(2) in
  let cmt_filename = Sys.argv.(3) in
  let source = read_file source_filename in
  let structure =
    match Cmt_input.load cmt_filename with
    | Ok implementation -> implementation.structure
    | Error diagnostic ->
        fail "CMT admission failed with %s" diagnostic.Diagnostic.code
  in
  let calls = collect_calls structure in
  match mode with
  | "erased" ->
      if calls <> [] then
        fail "erased CMT retained %d ghost calls" (List.length calls);
      print_endline "erased CMT inspection passed"
  | "retained" ->
      let expectations =
        [
          ( "Vero_ghost.requires",
            "[%verocaml.requires req_bool n]",
            "req_bool n" );
          ( "Vero_ghost.ensures",
            "[%verocaml.ensures fun result -> result > [%verocaml.old old_value n]]",
            "fun result -> result > [%verocaml.old old_value n]" );
          ( "Vero_ghost.decreases",
            "[%verocaml.decreases measure n]",
            "measure n" );
          ( "Vero_ghost.assert_",
            "[%verocaml.assert assert_bool n]",
            "assert_bool n" );
          ("Vero_ghost.old", "[%verocaml.old old_value n]", "old_value n");
        ]
      in
      List.iter
        (fun (path, extension_source, payload_source) ->
          let call = find_call calls path in
          check_span source (path ^ " call") extension_source call.location;
          check_payload_span source call payload_source)
        expectations;
      let markers =
        List.filter
          (fun call -> String.equal call.path "Vero_ghost.marker")
          calls
      in
      let sidecars =
        List.filter
          (fun call -> String.equal call.path "Vero_ghost.sidecar")
          calls
      in
      if List.length markers <> 4 || List.length sidecars <> 4 then
        fail "expected four marker/sidecar pairs, found %d/%d"
          (List.length markers) (List.length sidecars);
      print_endline "retained CMT inspection passed"
  | _ -> fail "unknown inspection mode %s" mode
