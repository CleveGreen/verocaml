open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/mathematical_int/constant_folding_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let process_succeeded outcome =
  Outcome.process_facts outcome
  |> List.exists (function
       | Outcome.Exit_class (Outcome.Exited 0) -> true
       | _ -> false)

let discover_cmt root =
  match
    files_below (Filename.concat root "_build")
    |> List.filter (fun path ->
           String.equal (Filename.basename path) "constant_folding_probe.cmt")
  with
  | [ path ] -> Ok path
  | [] -> mismatch "constant-folding CMT was not produced"
  | _ -> mismatch "constant-folding CMT was ambiguous"

let compile_and_lower source ~environment ~workspace =
  let workspace = Unix.realpath workspace in
  let root = Filename.concat workspace "project" in
  write_file (Filename.concat root "dune-project")
    "(lang dune 3.17)\n(name constant_folding_probe)\n";
  write_file (Filename.concat root "dune")
    {dune|(library
 (name constant_folding_probe)
 (wrapped false)
 (modules Constant_folding_probe)
 (libraries verocaml.ghost)
 (flags (:standard -w -A -alert -all))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|dune};
  write_file (Filename.concat root "constant_folding_probe.ml")
    ("[@@@verocaml.verify]\n" ^ source);
  let* compiled =
    Process_adapter.run ~cwd:root
      {
        program = Project_environment.dune_path environment;
        arguments =
          [
            "build";
            "--root";
            root;
            "--build-dir";
            Filename.concat root "_build";
            "--profile";
            "release";
            "@all";
          ];
        forwarded =
          [
            ("PATH", Project_environment.tool_path environment);
            ("OCAMLPATH", Project_environment.ocaml_path environment);
            ("DUNE_CACHE", "disabled");
            ("DUNE_CONFIG__DISPLAY", "quiet");
            ("HOME", root);
            ("TMPDIR", root);
            ("OCAML_COLOR", "never");
          ];
        cleanup_paths = [];
        adjacency = [];
      }
  in
  let* () =
    if process_succeeded compiled then Ok ()
    else mismatch "constant-folding fixture did not compile"
  in
  let* cmt = discover_cmt root in
  let* implementation =
    match Cmt_input.load cmt with
    | Ok implementation -> Ok implementation
    | Error diagnostic ->
        mismatch "constant-folding CMT load failed with %s"
          diagnostic.Diagnostic.code
  in
  match Typedtree_lowering.lower implementation with
  | Ok program -> Ok program
  | Error diagnostic ->
      mismatch "constant-folding SST lowering failed with %s"
        diagnostic.Diagnostic.code

let specification_body program function_name =
  match
    List.find_opt
      (fun definition ->
        String.equal definition.Sst.function_id.function_name function_name)
      program.Sst.functions
  with
  | Some { body = Sst.Spec_definition body; _ } -> Ok body.expression
  | Some _ -> mismatch "%s was not lowered as a specification" function_name
  | None -> mismatch "%s was absent from lowered SST" function_name

let verified () =
  Outcome.observation ~status:Outcome.Verified () |> Outcome.project

type expected_expression =
  | Expected_int of Z.t
  | Expected_variable of string
  | Expected_add of expected_expression * expected_expression
  | Expected_subtract of expected_expression * expected_expression
  | Expected_multiply of expected_expression * expected_expression
  | Expected_negate of expected_expression

type token =
  | Token_integer of Z.t
  | Token_identifier of string
  | Token_plus
  | Token_minus
  | Token_star
  | Token_left_parenthesis
  | Token_right_parenthesis
  | Token_end

let parse_error source format =
  Printf.ksprintf
    (fun message ->
      invalid_arg
        (Printf.sprintf "cannot parse expected arithmetic `%s`: %s" source
           message))
    format

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_digit character = character >= '0' && character <= '9'

let is_identifier_start character =
  (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')
  || Char.equal character '_'

let is_identifier_continue character =
  is_identifier_start character || is_digit character

let tokenize source =
  let length = String.length source in
  let rec take_while predicate index =
    if index < length && predicate source.[index] then
      take_while predicate (index + 1)
    else index
  in
  let rec loop index reversed =
    if index >= length then Array.of_list (List.rev (Token_end :: reversed))
    else
      let character = source.[index] in
      if is_whitespace character then loop (index + 1) reversed
      else if is_digit character then
        let stop = take_while is_digit (index + 1) in
        let literal = String.sub source index (stop - index) in
        loop stop (Token_integer (Z.of_string literal) :: reversed)
      else if is_identifier_start character then
        let stop = take_while is_identifier_continue (index + 1) in
        let identifier = String.sub source index (stop - index) in
        loop stop (Token_identifier identifier :: reversed)
      else
        match character with
        | '+' -> loop (index + 1) (Token_plus :: reversed)
        | '-' -> loop (index + 1) (Token_minus :: reversed)
        | '*' -> loop (index + 1) (Token_star :: reversed)
        | '(' -> loop (index + 1) (Token_left_parenthesis :: reversed)
        | ')' -> loop (index + 1) (Token_right_parenthesis :: reversed)
        | unexpected ->
            parse_error source "unexpected character %C" unexpected
  in
  loop 0 []

type expected_parser = {
  source : string;
  tokens : token array;
  mutable position : int;
}

let current_token parser =
  if parser.position < Array.length parser.tokens then
    parser.tokens.(parser.position)
  else Token_end

let advance parser = parser.position <- parser.position + 1

let rec parse_additive parser =
  let left = parse_multiplicative parser in
  let rec continue left =
    match current_token parser with
    | Token_plus ->
        advance parser;
        let right = parse_multiplicative parser in
        continue (Expected_add (left, right))
    | Token_minus ->
        advance parser;
        let right = parse_multiplicative parser in
        continue (Expected_subtract (left, right))
    | _ -> left
  in
  continue left

and parse_multiplicative parser =
  let left = parse_unary parser in
  let rec continue left =
    match current_token parser with
    | Token_star ->
        advance parser;
        let right = parse_unary parser in
        continue (Expected_multiply (left, right))
    | _ -> left
  in
  continue left

and parse_unary parser =
  match current_token parser with
  | Token_minus ->
      advance parser;
      Expected_negate (parse_unary parser)
  | _ -> parse_atom parser

and parse_atom parser =
  match current_token parser with
  | Token_integer value ->
      advance parser;
      Expected_int value
  | Token_identifier identifier ->
      advance parser;
      Expected_variable identifier
  | Token_left_parenthesis ->
      advance parser;
      let expression = parse_additive parser in
      (match current_token parser with
      | Token_right_parenthesis -> advance parser
      | _ -> parse_error parser.source "expected a closing parenthesis");
      expression
  | Token_end -> parse_error parser.source "unexpected end of expression"
  | _ -> parse_error parser.source "expected an integer, variable, or parenthesis"

let rec normalize_expected = function
  | Expected_int _ as expression -> expression
  | Expected_variable _ as expression -> expression
  | Expected_negate operand -> (
      match normalize_expected operand with
      | Expected_int value -> Expected_int (Z.neg value)
      | operand -> Expected_negate operand)
  | Expected_add (left, right) -> (
      let left = normalize_expected left in
      let right = normalize_expected right in
      match (left, right) with
      | Expected_int left, Expected_int right ->
          Expected_int (Z.add left right)
      | _ -> Expected_add (left, right))
  | Expected_subtract (left, right) -> (
      let left = normalize_expected left in
      let right = normalize_expected right in
      match (left, right) with
      | Expected_int left, Expected_int right ->
          Expected_int (Z.sub left right)
      | _ -> Expected_subtract (left, right))
  | Expected_multiply (left, right) -> (
      let left = normalize_expected left in
      let right = normalize_expected right in
      match (left, right) with
      | Expected_int left, Expected_int right ->
          Expected_int (Z.mul left right)
      | _ -> Expected_multiply (left, right))

let parse_expected source =
  let parser = { source; tokens = tokenize source; position = 0 } in
  let expression = parse_additive parser in
  match current_token parser with
  | Token_end -> normalize_expected expression
  | _ -> parse_error source "unexpected trailing input"

let probe_operation_matches probe operation arity =
  match probe.Sst.expression_desc with
  | Sst.Checked_arithmetic (probe_operation, operands) ->
      List.length operands = arity && probe_operation = operation
  | _ -> false

let is_zero expression =
  match expression.Sst.expression_desc with
  | Sst.Int_constant value -> Z.equal value Z.zero
  | _ -> false

let variable_identity expression =
  let identity binding use_uniqueness =
    Some (binding.Sst.id, use_uniqueness, binding.typ)
  in
  match expression.Sst.expression_desc with
  | Sst.Variable { binding; use_uniqueness } ->
      identity binding use_uniqueness
  | Sst.Lift_runtime_int
      {
        Sst.expression_desc = Sst.Variable { binding; use_uniqueness };
        _;
      } ->
      identity binding use_uniqueness
  | _ -> None

let bind_variable bindings actual expected_name =
  match variable_identity actual with
  | None -> false
  | Some actual_identity ->
  let by_actual =
    List.find_opt
      (fun (known_actual, _) -> known_actual = actual_identity)
      !bindings
  in
  let by_expected =
    List.find_opt
      (fun (_, known_expected) -> String.equal known_expected expected_name)
      !bindings
  in
  match (by_actual, by_expected) with
  | None, None ->
      bindings := (actual_identity, expected_name) :: !bindings;
      true
  | Some (_, known_expected), Some (known_actual, _) ->
      String.equal known_expected expected_name
      && known_actual = actual_identity
  | _ -> false

let matches_expected ~add_probe ~subtract_probe ~multiply_probe ~negate_probe
    actual expected =
  let bindings = ref [] in
  let rec matches actual expected =
    if actual.Sst.typ <> Sst.Mathematical_int then false
    else
      match (expected, actual.Sst.expression_desc) with
      | Expected_int expected, Sst.Int_constant actual ->
          Z.equal expected actual
      | ( Expected_variable _,
          (Sst.Int_constant _ | Sst.Checked_arithmetic _) ) ->
          false
      | Expected_variable expected_name, _ ->
          bind_variable bindings actual expected_name
      | ( Expected_add (expected_left, expected_right),
          Sst.Checked_arithmetic
            (operation, [ actual_left; actual_right ]) )
        when probe_operation_matches add_probe operation 2 ->
          matches actual_left expected_left
          && matches actual_right expected_right
      | ( Expected_subtract (expected_left, expected_right),
          Sst.Checked_arithmetic
            (operation, [ actual_left; actual_right ]) )
        when probe_operation_matches subtract_probe operation 2 ->
          matches actual_left expected_left
          && matches actual_right expected_right
      | ( Expected_multiply (Expected_int expected_coefficient, expected_operand),
          Sst.Checked_arithmetic
            (Sst.Multiply_constant actual_coefficient, [ actual_operand ]) )
        when Z.equal expected_coefficient actual_coefficient ->
          matches actual_operand expected_operand
      | ( Expected_multiply (expected_operand, Expected_int expected_coefficient),
          Sst.Checked_arithmetic
            (Sst.Multiply_constant actual_coefficient, [ actual_operand ]) )
        when Z.equal expected_coefficient actual_coefficient ->
          matches actual_operand expected_operand
      | ( Expected_multiply
            (Expected_int expected_coefficient, expected_operand),
          Sst.Checked_arithmetic (operation, [ actual_operand ]) )
        when
          Z.equal expected_coefficient (Z.of_int (-1))
          && probe_operation_matches negate_probe operation 1 ->
          matches actual_operand expected_operand
      | ( Expected_multiply
            (expected_operand, Expected_int expected_coefficient),
          Sst.Checked_arithmetic (operation, [ actual_operand ]) )
        when
          Z.equal expected_coefficient (Z.of_int (-1))
          && probe_operation_matches negate_probe operation 1 ->
          matches actual_operand expected_operand
      | ( Expected_multiply (expected_left, expected_right),
          Sst.Checked_arithmetic
            (operation, [ actual_left; actual_right ]) )
        when probe_operation_matches multiply_probe operation 2 ->
          matches actual_left expected_left
          && matches actual_right expected_right
      | ( Expected_negate expected_operand,
          Sst.Checked_arithmetic
            (Sst.Multiply_constant coefficient, [ actual_operand ]) )
        when Z.equal coefficient (Z.of_int (-1)) ->
          matches actual_operand expected_operand
      | ( Expected_negate expected_operand,
          Sst.Checked_arithmetic (operation, [ actual_operand ]) )
        when probe_operation_matches negate_probe operation 1 ->
          matches actual_operand expected_operand
      | ( Expected_negate expected_operand,
          Sst.Checked_arithmetic
            (operation, [ actual_zero; actual_operand ]) )
        when
          probe_operation_matches subtract_probe operation 2
          && is_zero actual_zero ->
          matches actual_operand expected_operand
      | _ -> false
  in
  matches actual expected

let describe_expression ~add_probe ~subtract_probe ~multiply_probe ~negate_probe
    expression =
  let rec describe depth expression =
    if depth <= 0 then "..."
    else
      match expression.Sst.expression_desc with
      | Sst.Int_constant value -> Z.to_string value
      | Sst.Checked_arithmetic
          (Sst.Multiply_constant coefficient, [ operand ]) ->
          Printf.sprintf "(scale %s %s)" (Z.to_string coefficient)
            (describe (depth - 1) operand)
      | Sst.Checked_arithmetic (operation, [ operand ])
        when probe_operation_matches negate_probe operation 1 ->
          Printf.sprintf "(- %s)" (describe (depth - 1) operand)
      | Sst.Checked_arithmetic (operation, [ left; right ])
        when probe_operation_matches add_probe operation 2 ->
          Printf.sprintf "(+ %s %s)" (describe (depth - 1) left)
            (describe (depth - 1) right)
      | Sst.Checked_arithmetic (operation, [ left; right ])
        when probe_operation_matches subtract_probe operation 2 ->
          Printf.sprintf "(- %s %s)" (describe (depth - 1) left)
            (describe (depth - 1) right)
      | Sst.Checked_arithmetic (operation, [ left; right ])
        when probe_operation_matches multiply_probe operation 2 ->
          Printf.sprintf "(* %s %s)" (describe (depth - 1) left)
            (describe (depth - 1) right)
      | Sst.Checked_arithmetic (_, operands) ->
          Printf.sprintf "(arithmetic/%d %s)" (List.length operands)
            (operands
            |> List.map (describe (depth - 1))
            |> String.concat " ")
      | _ -> "<variable>"
  in
  describe 12 expression

type folding_case = {
  number : int;
  variables : string list;
  input : string;
  expected : string;
}

let folding_case number variables input expected =
  { number; variables; input; expected }

let strict_folding_cases =
  [
    folding_case 1 [ "x" ]
      "x + (2 + 3)"
      "x + 5";
    folding_case 2 [ "y" ]
      "(7 - 4) * y"
      "3 * y";
    folding_case 3 [ "z" ]
      "z * (6 * 5)"
      "z * 30";
    folding_case 4 [ "x" ]
      "(8 + 2) - x"
      "10 - x";
    folding_case 5 [ "x" ]
      "x - (9 - 12)"
      "x - (-3)";
    folding_case 6 [ "x" ]
      "(3 - 10) * x"
      "(-7) * x";
    folding_case 7 [ "y" ]
      "(4 * 5) + y"
      "20 + y";
    folding_case 8 [ "a" ]
      "a + (10 - (3 * 2))"
      "a + 4";
    folding_case 9 [ "x" ]
      "(2 + 3) * (x - 4)"
      "5 * (x - 4)";
    folding_case 10 [ "x" ]
      "(x + (8 - 3)) * (2 + 4)"
      "(x + 5) * 6";
    folding_case 11 [ "x" ]
      "((2 + 3) * (4 - 1)) + x"
      "15 + x";
    folding_case 12 [ "x" ]
      "x * ((9 - 5) + (2 * 3))"
      "x * 10";
    folding_case 13 [ "y" ]
      "((7 * 8) - (3 + 5)) * y"
      "48 * y";
    folding_case 14 [ "a" ]
      "a - ((10 - 6) * (3 + 2))"
      "a - 20";
    folding_case 15 [ "b" ]
      "((12 - 4) * (5 - 2)) + b"
      "24 + b";
    folding_case 16 [ "c" ]
      "c * ((2 * 3) + (4 * 5))"
      "c * 26";
    folding_case 17 [ "x" ]
      "(100 - (20 * 3)) + x"
      "40 + x";
    folding_case 18 [ "x" ]
      "x + ((7 + 8) - (4 * 3))"
      "x + 3";
    folding_case 19 [ "y" ]
      "(2 * (3 + 4)) - y"
      "14 - y";
    folding_case 20 [ "y" ]
      "y - (2 * (8 - 3))"
      "y - 10";
    folding_case 21 [ "x" ]
      "(x * (2 + 5)) + (9 - 4)"
      "(x * 7) + 5";
    folding_case 22 [ "a" ]
      "(a - (6 * 3)) * (4 + 1)"
      "(a - 18) * 5";
    folding_case 23 [ "x" ]
      "(2 + 3) * (x + (9 - 7))"
      "5 * (x + 2)";
    folding_case 24 [ "y" ]
      "(8 - 2) * (y - (3 + 1))"
      "6 * (y - 4)";
    folding_case 25 [ "x" ]
      "((5 * 5) - (4 * 3)) + (x * (2 + 1))"
      "13 + (x * 3)";
    folding_case 26 [ "x" ]
      "x - ((2 + 3) * (7 - 4))"
      "x - 15";
    folding_case 27 [ "z" ]
      "((9 + 1) * (6 - 2)) - z"
      "40 - z";
    folding_case 28 [ "z" ]
      "z + ((11 - 3) * (2 + 2))"
      "z + 32";
    folding_case 29 [ "a" ]
      "(a * (3 * 4)) - (10 - 7)"
      "(a * 12) - 3";
    folding_case 30 [ "b" ]
      "((14 - 9) + (6 * 2)) * b"
      "17 * b";
    folding_case 31 [ "x" ]
      "x + ((2 + 3) * ((7 - 2) + (4 * 2)))"
      "x + 65";
    folding_case 32 [ "y" ]
      "((20 - (3 * 4)) * (2 + 5)) - y"
      "56 - y";
    folding_case 33 [ "a" ]
      "(a - ((9 - 3) * (8 - 5))) + (4 * 6)"
      "a + 6";
    folding_case 34 [ "x"; "y" ]
      "((2 * 3) + x) * ((10 - 6) + y)"
      "(6 + x) * (4 + y)";
    folding_case 35 [ "x" ]
      "((5 + 7) - x) * ((9 - 4) * 2)"
      "(12 - x) * 10";
    folding_case 36 [ "x" ]
      "(x + ((8 * 3) - (7 + 5))) * (6 - 1)"
      "(x + 12) * 5";
    folding_case 37 [ "x" ]
      "((3 + 4) * (5 + 6)) - (x * (2 + 2))"
      "77 - (x * 4)";
    folding_case 38 [ "x" ]
      "(x - (15 - (2 * 4))) + (3 * (5 + 1))"
      "x + 11";
    folding_case 39 [ "y" ]
      "((18 - 7) * (4 - 1)) + (y - (6 + 2))"
      "y + 25";
    folding_case 40 [ "z" ]
      "(z * ((3 * 3) - (2 + 1))) - ((8 - 5) * 4)"
      "(z * 6) - 12";
    folding_case 41 [ "x" ]
      "((2 + 8) * ((7 - 3) * (5 - 2))) + x"
      "120 + x";
    folding_case 42 [ "a" ]
      "a * (((9 + 1) - (3 * 2)) * (8 - 6))"
      "a * 8";
    folding_case 43 [ "b" ]
      "((50 - (6 * 7)) + (3 * 4)) - b"
      "20 - b";
    folding_case 44 [ "b" ]
      "b + ((4 * (3 + 2)) - (18 - 7))"
      "b + 9";
    folding_case 45 [ "c" ]
      "((7 + 5) * (9 - 6)) - (c * (4 - 1))"
      "36 - (c * 3)";
    folding_case 46 [ "x" ]
      "(x + ((2 * 5) + (3 * 7))) - ((8 + 4) * 2)"
      "x + 7";
    folding_case 47 [ "y" ]
      "((16 - 9) * (2 + 6)) + (y * ((5 * 2) - 3))"
      "56 + (y * 7)";
    folding_case 48 [ "a" ]
      "(a - ((3 + 5) * (7 - 2))) * ((9 - 4) + 1)"
      "(a - 40) * 6";
    folding_case 49 [ "x" ]
      "((6 * 6) - (4 * 5)) * (x + (10 - 8))"
      "16 * (x + 2)";
    folding_case 50 [ "x" ]
      "(x * ((12 - 5) + (2 * 4))) - ((3 + 7) * (6 - 2))"
      "(x * 15) - 40";
    folding_case 51 [ "x"; "y" ]
      "(x + (2 - 9)) * (y + (4 * 3))"
      "(x + (-7)) * (y + 12)";
    folding_case 52 [ "a"; "b" ]
      "(a - (5 - 13)) + (b * (2 + 6))"
      "(a - (-8)) + (b * 8)";
    folding_case 53 [ "x" ]
      "((0 - 6) * x) + (9 - 9)"
      "(-6) * x";
    folding_case 54 [ "x"; "y" ]
      "(x * (3 - 3)) - (y + (8 - 2))"
      "-(y + 6)";
    folding_case 55 [ "a"; "b" ]
      "(a + (4 * 0)) * (b - (7 - 7))"
      "a * b";
    folding_case 56 [ "x" ]
      "((2 - 5) * (3 - 8)) + x"
      "15 + x";
    folding_case 57 [ "x" ]
      "x - ((4 - 9) * (2 + 3))"
      "x - (-25)";
    folding_case 58 [ "y" ]
      "((1 - 6) + (2 * 2)) * y"
      "(-1) * y";
    folding_case 59 [ "z" ]
      "(z + ((3 - 10) * (2 - 5))) - (4 - 4)"
      "z + 21";
    folding_case 60 [ "x" ]
      "((8 - 12) * (5 - 2)) + (x * (6 - 6))"
      "-12";
  ]

let default_reassociation_cases =
  [
    folding_case 61 [ "x" ]
      "x + 2 + 3"
      "x + 5";
    folding_case 62 [ "x" ]
      "2 + x + 3"
      "x + 5";
    folding_case 63 [ "x" ]
      "x * 2 * 3"
      "6 * x";
    folding_case 64 [ "x" ]
      "2 * x * 3"
      "6 * x";
    folding_case 65 [ "x" ]
      "x - 2 - 3"
      "x + (-5)";
    folding_case 66 [ "x" ]
      "10 - x - 4"
      "(-x) + 6";
    folding_case 67 [ "x" ]
      "(x + 2) + 3"
      "x + 5";
    folding_case 68 [ "x" ]
      "2 + (x + 3)"
      "x + 5";
    folding_case 69 [ "x" ]
      "(x * 2) * 3"
      "6 * x";
    folding_case 70 [ "x" ]
      "2 * (x * 3)"
      "6 * x";
    folding_case 71 [ "x" ]
      "(x - 2) - 3"
      "x + (-5)";
    folding_case 72 [ "x" ]
      "10 - (x - 4)"
      "(-x) + 14";
    folding_case 73 [ "x"; "y" ]
      "(x + y) * 4"
      "(x + y) * 4";
    folding_case 74 [ "x"; "y" ]
      "x + (y * 3)"
      "x + (y * 3)";
    folding_case 75 [ "x"; "y"; "z" ]
      "(x - y) + z"
      "(x - y) + z";
    folding_case 76 [ "x"; "y" ]
      "(x + 2) * (y - 3)"
      "(x + 2) * (y - 3)";
    folding_case 77 [ "x"; "y" ]
      "x * (y + 4)"
      "x * (y + 4)";
    folding_case 78 [ "x"; "y"; "z" ]
      "(x * y) - (z * 5)"
      "(x * y) - (z * 5)";
    folding_case 79 [ "x"; "y"; "z" ]
      "(x + y) - (z - 6)"
      "(x + y) - (z - 6)";
    folding_case 80 [ "x"; "y" ]
      "(x - (y + 2)) * 3"
      "(x - (y + 2)) * 3";
  ]

let algebraic_normalization_cases =
  [
    folding_case 81 [ "x" ]
      "x + 2 + 3"
      "x + 5";
    folding_case 82 [ "x" ]
      "2 + x + 3"
      "x + 5";
    folding_case 83 [ "x" ]
      "x * 2 * 3"
      "6 * x";
    folding_case 84 [ "x" ]
      "2 * x * 3"
      "6 * x";
    folding_case 85 [ "x" ]
      "x - 2 - 3"
      "x + (-5)";
    folding_case 86 [ "x" ]
      "10 - x - 4"
      "(-x) + 6";
    folding_case 87 [ "x" ]
      "x + 0"
      "x";
    folding_case 88 [ "x" ]
      "0 + x"
      "x";
    folding_case 89 [ "x" ]
      "x - 0"
      "x";
    folding_case 90 [ "x" ]
      "x * 1"
      "x";
    folding_case 91 [ "x" ]
      "1 * x"
      "x";
    folding_case 92 [ "x" ]
      "x * 0"
      "0";
    folding_case 93 [ "x" ]
      "0 * (x + 7)"
      "0";
    folding_case 94 [ "x"; "y" ]
      "(x + 2) + (3 + y)"
      "x + y + 5";
    folding_case 95 [ "x"; "y" ]
      "(x - 2) + (y - 3)"
      "(x + y) + (-5)";
    folding_case 96 [ "x"; "y" ]
      "(x + 7) - (y + 2)"
      "(x + (-y)) + 5";
    folding_case 97 [ "x" ]
      "10 - (x - 4)"
      "(-x) + 14";
    folding_case 98 [ "x" ]
      "x - (2 - 5)"
      "x - (-3)";
    folding_case 99 [ "x" ]
      "(x + 3) - (x + 1)"
      "2";
    folding_case 100 [ "x" ]
      "(2 * x) + (3 * x)"
      "5 * x";
    folding_case 101 [ "x" ]
      "(7 * x) - (4 * x)"
      "3 * x";
    folding_case 102 [ "x" ]
      "(2 * x) + x - (5 * x)"
      "(-2) * x";
    folding_case 103 [ "x"; "y" ]
      "(x + y) + (2 * x) - y"
      "3 * x";
    folding_case 104 [ "x"; "y" ]
      "(3 * x) + (2 * y) - x + 5 - 2"
      "(2 * x) + (2 * y) + 3";
    folding_case 105 [ "x" ]
      "(x + 2) * 3"
      "3 * (x + 2)";
    folding_case 106 [ "x" ]
      "4 * (x - 5)"
      "4 * (x - 5)";
    folding_case 107 [ "x"; "y" ]
      "(x + 2) * (y + 3)"
      "(x + 2) * (y + 3)";
    folding_case 108 [ "x"; "y" ]
      "(x - 2) * (y + 4)"
      "(x - 2) * (y + 4)";
    folding_case 109 [ "x"; "y" ]
      "(x - 3) * (y - 5)"
      "(x - 3) * (y - 5)";
    folding_case 110 [ "x" ]
      "((2 * x) + 3) * (x - 4)"
      "((2 * x) + 3) * (x - 4)";
    folding_case 111 [ "x" ]
      "(x + 2) * (x + 3)"
      "(x + 2) * (x + 3)";
    folding_case 112 [ "x" ]
      "(x - 2) * (x + 2)"
      "(x * x) + (-4)";
    folding_case 113 [ "x" ]
      "((3 * x) - 1) * ((2 * x) + 5)"
      "((3 * x) + (-1)) * ((2 * x) + 5)";
    folding_case 114 [ "x"; "y" ]
      "(x + y) * 2 + (x - y) * 3"
      "(5 * x) + (-y)";
    folding_case 115 [ "x"; "y" ]
      "(x + 1) * (y + 2) - (x * y)"
      "(2 * x) + y + 2";
    folding_case 116 [ "x"; "y" ]
      "(x + 2) * (y + 3) - (x + 2) * y"
      "(3 * x) + 6";
    folding_case 117 [ "x" ]
      "((2 * x) + 4) * 3 - (6 * x)"
      "12";
    folding_case 118 [ "x" ]
      "(x - 4) * 5 + ((2 * x) + 3)"
      "(7 * x) + (-17)";
    folding_case 119 [ "x"; "y" ]
      "(x + y + 2) * 3 - (x + y)"
      "(2 * x) + (2 * y) + 6";
    folding_case 120 [ "x"; "y" ]
      "(x - y) * (x + y)"
      "(x * x) + (-(y * y))";
    folding_case 121 [ "x"; "y" ]
      "(x + y) * (x + y)"
      "(x + y) * (x + y)";
    folding_case 122 [ "x"; "y" ]
      "(x - y) * (x - y)"
      "(x - y) * (x - y)";
    folding_case 123 [ "x"; "y" ]
      "((2 * x) - (3 * y)) * (x + y)"
      "((2 * x) - (3 * y)) * (x + y)";
    folding_case 124 [ "x"; "y" ]
      "(x + (2 * y) + 3) * 2"
      "2 * ((x + (2 * y)) + 3)";
    folding_case 125 [ "x" ]
      "((3 * x) + 2) - (x - 5) + (4 - (2 * x))"
      "11";
    folding_case 126 [ "x"; "y" ]
      "(x + 4) * ((2 * y) - 3)"
      "(x + 4) * ((2 * y) - 3)";
    folding_case 127 [ "x"; "y" ]
      "((2 * x) - 5) * ((3 * y) + 4)"
      "((2 * x) - 5) * ((3 * y) + 4)";
    folding_case 128 [ "x" ]
      "(x + 1) * (x - 1) + 1"
      "x * x";
    folding_case 129 [ "x" ]
      "(x + 2) * (x + 2) - ((4 * x) + 4)"
      "x * x";
    folding_case 130 [ "x"; "y" ]
      "(x + y + 1) * 2 - ((2 * x) + (2 * y))"
      "2";
    folding_case 131 [ "x" ]
      "(x + 3) * 4 - ((2 * x) + 5) * 2"
      "2";
    folding_case 132 [ "x"; "y" ]
      "((2 * x) + (3 * y)) - (x + y) + (4 - 1)"
      "x + (2 * y) + 3";
    folding_case 133 [ "x"; "y" ]
      "(x + 2) * (y - 2) + ((2 * x) - 4)"
      "((x * y) + (2 * y)) + (-8)";
    folding_case 134 [ "x"; "y" ]
      "(x - 2) * (y + 2) - ((x * y) - 4)"
      "(2 * x) + ((-2) * y)";
    folding_case 135 [ "x" ]
      "((3 * x) + 1) * 2 - (x - 4) * 5"
      "x + 22";
    folding_case 136 [ "x"; "y" ]
      "(x + y) * 3 - (x - y) * 2"
      "x + (5 * y)";
    folding_case 137 [ "x"; "y" ]
      "((2 * x) - y) * 4 + (y - x) * 3"
      "(5 * x) + (-y)";
    folding_case 138 [ "x" ]
      "(x + 2) * (x - 3) - (x * x)"
      "(-x) + (-6)";
    folding_case 139 [ "x"; "y" ]
      "(x + 5) * (y + 1) - ((x * y) + x)"
      "(5 * y) + 5";
    folding_case 140 [ "x" ]
      "((2 * x) + 1) * ((2 * x) - 1)"
      "4 * (x * x) + (-1)";
  ]

let all_generated_cases =
  strict_folding_cases @ default_reassociation_cases
  @ algebraic_normalization_cases

let function_name test_case =
  Printf.sprintf "constant_folding_case_%03d" test_case.number

let typed_arguments variables =
  variables
  |> List.map (fun variable -> Printf.sprintf "(%s : int)" variable)
  |> String.concat " "

let source_of_case test_case =
  Printf.sprintf "let %s %s = %s [@@verocaml.spec]\n"
    (function_name test_case)
    (typed_arguments test_case.variables)
    test_case.input

let probe_source =
  {ocaml|
let folded (unused : int) = (2 + 3) * (8 - 3) [@@verocaml.spec]
let scaled (value : int) = (2 + 3) * value [@@verocaml.spec]
let general (left : int) (right : int) = left * right [@@verocaml.spec]
let folded_relation () = (2 + 3) < (7 - 1) [@@verocaml.spec]

let constant_folding_operator_add (left : int) (right : int) =
  left + right
  [@@verocaml.spec]

let constant_folding_operator_subtract (left : int) (right : int) =
  left - right
  [@@verocaml.spec]

let constant_folding_operator_multiply (left : int) (right : int) =
  left * right
  [@@verocaml.spec]

let constant_folding_operator_negate (value : int) =
  -value
  [@@verocaml.spec]
|ocaml}

let generated_source =
  probe_source ^ "\n"
  ^ (all_generated_cases |> List.map source_of_case |> String.concat "\n")

let generated_program_cache = ref None

let generated_program ~environment ~workspace =
  match !generated_program_cache with
  | Some program -> Ok program
  | None ->
      let* program =
        compile_and_lower generated_source ~environment ~workspace
      in
      generated_program_cache := Some program;
      Ok program

let operator_probes program =
  let* add_probe =
    specification_body program "constant_folding_operator_add"
  in
  let* subtract_probe =
    specification_body program "constant_folding_operator_subtract"
  in
  let* multiply_probe =
    specification_body program "constant_folding_operator_multiply"
  in
  let* negate_probe =
    specification_body program "constant_folding_operator_negate"
  in
  Ok (add_probe, subtract_probe, multiply_probe, negate_probe)

let structural_case ~name ~function_name accepts =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* program = generated_program ~environment ~workspace in
      let* expression = specification_body program function_name in
      if expression.typ = Sst.Mathematical_int && accepts expression.expression_desc
      then Ok (verified ())
      else mismatch "%s had an unexpected pre-VC SST shape" function_name)

let folded_product =
  structural_case ~name:"closed-product-folds-before-vc" ~function_name:"folded"
    (function Sst.Int_constant value -> Z.equal value (Z.of_int 25) | _ -> false)

let constant_scale =
  structural_case ~name:"constant-expression-becomes-scale-before-vc"
    ~function_name:"scaled"
    (function
      | Sst.Checked_arithmetic (Sst.Multiply_constant coefficient, [ _ ]) ->
          Z.equal coefficient (Z.of_int 5)
      | _ -> false)

let folded_relation =
  Suite.case ~name:"constant-comparison-folds-before-vc"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* program = generated_program ~environment ~workspace in
      let* expression = specification_body program "folded_relation" in
      match expression.expression_desc with
      | Sst.Bool_constant true -> Ok (verified ())
      | _ -> mismatch "folded_relation had an unexpected pre-VC SST shape")

let general_product =
  structural_case ~name:"variable-product-remains-general-before-vc"
    ~function_name:"general"
    (function
      | Sst.Checked_arithmetic (Sst.Multiply, [ _; _ ]) -> true
      | _ -> false)

let generated_structural_case ?config ~prefix test_case =
  let expected = parse_expected test_case.expected in
  Suite.case
    ~name:(Printf.sprintf "%s-%03d" prefix test_case.number)
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let* program = generated_program ~environment ~workspace in
      let* expression =
        specification_body program (function_name test_case)
      in
      let expression =
        match config with
        | None -> expression
        | Some config ->
            Mathematical_int_rewrite_private.simplify_with config expression
      in
      let* probes = operator_probes program in
      let add_probe, subtract_probe, multiply_probe, negate_probe = probes in
      if
        matches_expected ~add_probe ~subtract_probe ~multiply_probe ~negate_probe
          expression expected
      then Ok (verified ())
      else
        mismatch
          "case %03d: `%s` should lower to `%s`; actual pre-VC SST shape was %s"
          test_case.number test_case.input test_case.expected
          (describe_expression ~add_probe ~subtract_probe ~multiply_probe
             ~negate_probe expression))

let generated_cases =
  List.map
    (generated_structural_case
       ~prefix:"default-closed-subexpression-fold")
    strict_folding_cases
  @ List.map
      (generated_structural_case ~prefix:"default-reassociation")
      default_reassociation_cases
  @ List.map
      (generated_structural_case
         ~config:Mathematical_int_rewrite_private.aggressive_config
         ~prefix:"aggressive-algebraic-normalization")
      algebraic_normalization_cases

let () =
  Suite.run_cli ~suite_path ~manifest:Constant_folding_environment.manifest
    ~expected_environment:Constant_folding_environment.expected
    ([ folded_product; constant_scale; general_product; folded_relation ]
    @ generated_cases)
