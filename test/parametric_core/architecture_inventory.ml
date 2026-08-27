open Parsetree

let line_span location =
  let first = location.Location.loc_start.pos_lnum in
  let last = location.Location.loc_end.pos_lnum in
  (first, max first last)

let sanitize value =
  String.map
    (function '\t' | '\n' | '\r' -> ' ' | character -> character)
    value

let pattern_name pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> name.txt
  | _ -> Format.asprintf "%a" Pprintast.pattern pattern |> sanitize

let rec function_expression expression =
  match expression.pexp_desc with
  | Pexp_function _ -> true
  | Pexp_constraint (nested, _, _)
  | Pexp_coerce (nested, _, _)
  | Pexp_poly (nested, _)
  | Pexp_newtype (_, _, nested) ->
      function_expression nested
  | _ -> false

let location_key location =
  (location.Location.loc_start.pos_cnum, location.Location.loc_end.pos_cnum)

let contexts = ref []
let suppressed_function = ref None

let context_name () =
  match !contexts with
  | [] -> "<module>"
  | names -> String.concat "/" (List.rev names)

let emit kind name attributes location =
  let first, last = line_span location in
  Printf.printf "%s\t%s\t%s\t%d\t%d\t%d\n" kind (sanitize name)
    (sanitize (context_name ()))
    (List.length attributes) first last

let iterator =
  let value_binding self binding =
    let name = pattern_name binding.pvb_pat in
    let is_function = function_expression binding.pvb_expr in
    if is_function then
      emit "binding" name binding.pvb_attributes binding.pvb_loc;
    let previous_suppressed = !suppressed_function in
    if is_function then
      suppressed_function := Some (location_key binding.pvb_expr.pexp_loc);
    contexts := name :: !contexts;
    Ast_iterator.default_iterator.value_binding self binding;
    contexts := List.tl !contexts;
    suppressed_function := previous_suppressed
  in
  let expr self expression =
    (match expression.pexp_desc with
    | Pexp_function (_, _, Pfunction_body body)
      when !suppressed_function <> Some (location_key expression.pexp_loc) ->
        emit "fun" "<anonymous>" expression.pexp_attributes
          { expression.pexp_loc with loc_end = body.pexp_loc.loc_end }
    | Pexp_function (_, _, Pfunction_cases (_, location, attributes))
      when !suppressed_function <> Some (location_key expression.pexp_loc) ->
        emit "function" "<anonymous>"
          (expression.pexp_attributes @ attributes)
          location
    | _ -> ());
    let previous_suppressed = !suppressed_function in
    if previous_suppressed = Some (location_key expression.pexp_loc) then
      suppressed_function := None;
    Ast_iterator.default_iterator.expr self expression;
    suppressed_function := previous_suppressed
  in
  { Ast_iterator.default_iterator with value_binding; expr }

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: architecture_inventory SOURCE.ml";
    exit 2);
  let source = Sys.argv.(1) in
  let channel = open_in_bin source in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let lexbuf = Lexing.from_channel channel in
      Location.init lexbuf source;
      let structure = Parse.implementation lexbuf in
      iterator.structure iterator structure)
