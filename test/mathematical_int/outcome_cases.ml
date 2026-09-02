open Outcome_test_support

let suite_path = "test/mathematical_int/outcome_cases.ml"

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

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let project_process module_name outcome =
  let process_facts = Outcome.process_facts outcome in
  let succeeded =
    List.exists
      (function Outcome.Exit_class (Outcome.Exited 0) -> true | _ -> false)
      process_facts
  in
  let frontend_codes =
    List.filter_map
      (function Outcome.Stable_code code -> Some code | _ -> None)
      process_facts
  in
  let status = if succeeded then Outcome.Verified else Outcome.Frontend_rejected in
  let unit =
    if succeeded then Outcome.Unit_verified else Outcome.Unit_frontend_rejected
  in
  Outcome.observation ~status ~frontend_codes ~process_facts ()
  |> Outcome.project |> Outcome.with_unit module_name unit

let run source ~environment ~workspace =
  let workspace = Unix.realpath workspace in
  let root = Filename.concat workspace "project" in
  write_file (Filename.concat root "dune-project")
    "(lang dune 3.17)\n(name mathematical_int_fixture)\n";
  write_file (Filename.concat root "dune")
    {dune|(library
 (name mathematical_int)
 (wrapped false)
 (modules Mathematical_int)
 (libraries verocaml.vstd verocaml.ghost)
 (flags (:standard -w -A -alert -all))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))
|dune};
  write_file (Filename.concat root "mathematical_int.ml") source;
  Process_adapter.run ~cwd:root
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; "."; "--threads"; "2"; "--timeout-ms"; "60000" ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("OCAMLPATH", Project_environment.ocaml_path environment);
          ("VEROCAML_DUNE", Project_environment.dune_path environment);
          ("DUNE_CACHE", "disabled");
          ("HOME", root);
          ("TMPDIR", root);
          ("OCAML_COLOR", "never");
        ];
      cleanup_paths = [];
      adjacency = [];
    }
  |> Result.map (project_process "Mathematical_int")

let expectation unit status =
  Expectation.empty |> Expectation.status status
  |> Expectation.require_unit "Mathematical_int" unit

let verified name body =
  Suite.case ~name
    ~expectation:(expectation Outcome.Unit_verified Outcome.Verified)
    (run ("[@@@verocaml.verify]\n" ^ body))

let rejected name body =
  Suite.case ~name
    ~expectation:(expectation Outcome.Unit_frontend_rejected Outcome.Frontend_rejected)
    (run ("[@@@verocaml.verify]\n" ^ body))

let runtime_math_matrix =
  {|
open Vstd
let add_rm (x : int) (y : Int.t) : Int.t = x + y [@@verocaml.spec]
let add_mr (x : Int.t) (y : int) : Int.t = x + y [@@verocaml.spec]
let sub_rm (x : int) (y : Int.t) : Int.t = x - y [@@verocaml.spec]
let sub_mr (x : Int.t) (y : int) : Int.t = x - y [@@verocaml.spec]
let eq_rm (x : int) (y : Int.t) : bool = x = y [@@verocaml.spec]
let ne_mr (x : Int.t) (y : int) : bool = x <> y [@@verocaml.spec]
let lt_rm (x : int) (y : Int.t) : bool = x < y [@@verocaml.spec]
let le_mr (x : Int.t) (y : int) : bool = x <= y [@@verocaml.spec]
let gt_rm (x : int) (y : Int.t) : bool = x > y [@@verocaml.spec]
let ge_mr (x : Int.t) (y : int) : bool = x >= y [@@verocaml.spec]
let neg (x : Int.t) : Int.t = ~-x [@@verocaml.spec]
let succ_m (x : Int.t) : Int.t = succ x [@@verocaml.spec]
let pred_m (x : Int.t) : Int.t = pred x [@@verocaml.spec]
let abs_m (x : Int.t) : Int.t = abs x [@@verocaml.spec]
let add_mm (x : Int.t) (y : Int.t) : Int.t = x + y [@@verocaml.spec]
let add_exec (x : int) (y : int) : int =
  [%verocaml.requires x = 0];
  x + y
let rr_ne (x : int) (y : int) : bool = x <> y [@@verocaml.spec]
let rr_lt (x : int) (y : int) : bool = x < y [@@verocaml.spec]
let rr_le (x : int) (y : int) : bool = x <= y [@@verocaml.spec]
let rr_gt (x : int) (y : int) : bool = x > y [@@verocaml.spec]
let rr_ge (x : int) (y : int) : bool = x >= y [@@verocaml.spec]
let rm_ne (x : int) (y : Int.t) : bool = x <> y [@@verocaml.spec]
let rm_lt (x : int) (y : Int.t) : bool = x < y [@@verocaml.spec]
let rm_le (x : int) (y : Int.t) : bool = x <= y [@@verocaml.spec]
let rm_gt (x : int) (y : Int.t) : bool = x > y [@@verocaml.spec]
let rm_ge (x : int) (y : Int.t) : bool = x >= y [@@verocaml.spec]
let mr_eq (x : Int.t) (y : int) : bool = x = y [@@verocaml.spec]
let mr_lt (x : Int.t) (y : int) : bool = x < y [@@verocaml.spec]
let mr_gt (x : Int.t) (y : int) : bool = x > y [@@verocaml.spec]
let scale_left (x : Int.t) : Int.t = x * 3 [@@verocaml.spec]
let scale_right (x : Int.t) : Int.t = 3 * x [@@verocaml.spec]
let scale_literal (x : Int.t) : Int.t = x * Int.of_string "12" [@@verocaml.spec]
let fold_numerals () : Int.t = 6 * 7 [@@verocaml.spec]
|}

let logical_matrix_cases =
  [ verified "runtime-runtime-add" "open Vstd\nlet f (x:int) (y:int) : Int.t = x + y [@@verocaml.spec]\n";
    verified "runtime-runtime-sub" "open Vstd\nlet f (x:int) (y:int) : Int.t = x - y [@@verocaml.spec]\n";
    verified "runtime-runtime-equal" "let f (x:int) (y:int) : bool = x = y [@@verocaml.spec]\n";
    verified "constant-five-times-five-assertion"
      "let five_squared () = assert (5 * 5 = 25) [@@verocaml.proof]\n";
    verified "runtime-math-and-math-runtime" runtime_math_matrix;
    verified "math-runtime-comparisons" runtime_math_matrix;
    verified "math-unary-operators" runtime_math_matrix;
    verified "logical-int-result-is-math" "open Vstd\nlet f (x:int) : Int.t = x + 1 [@@verocaml.spec]\n";
    verified "logical-add-no-overflow" "open Vstd\nlet f (x:Int.t) : Int.t = x + 1 [@@verocaml.spec]\n";
    verified "same-unit-unannotated-call-result"
      "let increment x = x + 1 [@@verocaml.spec]\nlet twice x = increment x + increment x [@@verocaml.spec]\n";
    verified "unannotated-recursive-logical-result"
      "type 'a node = Empty | Node of 'a * 'a node\nlet rec length node =\n  [%verocaml.decreases node];\n  match node with Empty -> 0 | Node (_, rest) -> 1 + length rest\n[@@verocaml.spec]\n[@@verocaml.revealed]\n";
    verified "same-unit-nested-tuple-call-result"
      "let pair x = (x + 1, true) [@@verocaml.spec]\nlet first x = match pair x with value, _ -> value [@@verocaml.spec]\n" ]

let literal_cases =
  [ verified "literal-zero" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0\" [@@verocaml.spec]\n";
    verified "literal-decimal" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"123456789\" [@@verocaml.spec]\n";
    verified "literal-arbitrary-precision" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"999999999999999999999999999999999999\" [@@verocaml.spec]\n";
    verified "literal-escaped-source" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"\\x31\\x32\" [@@verocaml.spec]\n";
    verified "literal-plus-sign" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"+1\" [@@verocaml.spec]\n";
    verified "literal-leading-zero" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"01\" [@@verocaml.spec]\n";
    verified "literal-negative-zero" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"-0\" [@@verocaml.spec]\n";
    verified "literal-decimal-separators" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"1_000_000\" [@@verocaml.spec]\n";
    verified "literal-hexadecimal" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0xDEAD_BEEF\" [@@verocaml.spec]\n";
    verified "literal-binary" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0b1010_0101\" [@@verocaml.spec]\n";
    verified "literal-octal" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0o755\" [@@verocaml.spec]\n";
    verified "literal-negative-prefixed" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"-0x1_0000_0000_0000_0000\" [@@verocaml.spec]\n";
    rejected "literal-empty" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"\" [@@verocaml.spec]\n";
    rejected "literal-sign-only" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"+\" [@@verocaml.spec]\n";
    rejected "literal-prefix-only" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0x\" [@@verocaml.spec]\n";
    rejected "literal-concatenation-dynamic" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string (\"12\" ^ \"34\") [@@verocaml.spec]\n";
    rejected "literal-whitespace" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \" 1\" [@@verocaml.spec]\n";
    rejected "literal-invalid-digit" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"0b102\" [@@verocaml.spec]\n";
    rejected "literal-suffix" "open Vstd\nlet f (unused : int) : Int.t = Int.of_string \"10L\" [@@verocaml.spec]\n";
    rejected "literal-dynamic" "open Vstd\nlet f s : Int.t = Int.of_string s [@@verocaml.spec]\n" ]

let nonlinear_arithmetic_cases =
  [ verified "math-times-math"
      "open Vstd\nlet f (x : Int.t) (y : Int.t) : Int.t = x * y [@@verocaml.spec]\n";
    verified "runtime-times-runtime-lifts-to-math"
      "open Vstd\nlet f (x : int) (y : int) : Int.t = x * y [@@verocaml.spec]\n";
    verified "runtime-times-math"
      "open Vstd\nlet f (x : int) (y : Int.t) : Int.t = x * y [@@verocaml.spec]\n";
    verified "math-times-runtime"
      "open Vstd\nlet f (x : Int.t) (y : int) : Int.t = x * y [@@verocaml.spec]\n";
    verified "symbolic-times-variable"
      "open Vstd\n[%%verocaml.symbolic val k : int -> int]\nlet f (x : int) : Int.t = x * k x [@@verocaml.spec]\n";
    verified "nonlinear-commutativity-proof"
      "open Vstd\nlet commute (x : Int.t) (y : Int.t) =\n  [%verocaml.ensures fun _ -> x * y = y * x];\n  ()\n[@@verocaml.proof]\n";
    verified "nonlinear-distributivity-proof"
      "open Vstd\nlet distribute (x : Int.t) (y : Int.t) (z : Int.t) =\n  [%verocaml.ensures fun _ -> x * (y + z) = (x * y) + (x * z)];\n  ()\n[@@verocaml.proof]\n";
    verified "nonlinear-broadcast-fact"
      "open Vstd\nlet square (x : Int.t) : Int.t = x * x [@@verocaml.spec]\nlet square_nonnegative (x : Int.t) =\n  [%verocaml.ensures fun _ -> (square x [@trigger]) >= 0];\n  ()\n[@@verocaml.axiom]\n[@@verocaml.broadcast]\nlet use_square_nonnegative (x : Int.t) =\n  [%verocaml.ensures fun _ -> square x >= 0];\n  ()\n[@@verocaml.proof]\n";
    verified "guarded-executable-general-multiply"
      "let f (x : int) (y : int) : int =\n  [%verocaml.requires x = 0];\n  x * y\n";
    rejected "division" "open Vstd\nlet f x = x / 2 [@@verocaml.spec]\n";
    rejected "modulo" "open Vstd\nlet f x = x mod 2 [@@verocaml.spec]\n";
    rejected "bitwise" "open Vstd\nlet f x = x land 2 [@@verocaml.spec]\n";
    rejected "shift" "open Vstd\nlet f x = x lsl 2 [@@verocaml.spec]\n";
    rejected "power" "open Vstd\nlet f x = x ** 2 [@@verocaml.spec]\n";
    rejected "nonstdlib-multiply" "open Vstd\nmodule M = struct let ( * ) x y = x + y end\nlet f x = M.(x * 2) [@@verocaml.spec]\n" ]

let quantifier_cases =
  [ verified "runtime-quantifier-range"
      "let observed (value : 'a) = false [@@verocaml.spec]\nlet f () = forall (fun (x : int) -> ((observed x) [@trigger]) || x = x) [@@verocaml.spec]\n";
    verified "math-quantifier-range"
      "open Vstd\nlet observed (value : 'a) = false [@@verocaml.spec]\nlet f () = forall (fun (x : Int.t) -> ((observed x) [@trigger]) || x = x) [@@verocaml.spec]\n";
    verified "mixed-quantifier-binders"
      "open Vstd\nlet observed (value : 'a) = false [@@verocaml.spec]\nlet f () = forall (fun (x : int) -> ((observed x) [@trigger]) || exists (fun (y : Int.t) -> x = y)) [@@verocaml.spec]\n" ]

let containment_cases =
  [ verified "tuple-containment" "open Vstd\nlet f (x:Int.t) : Int.t = let pair = (x + 1, x) in match pair with first, _ -> first [@@verocaml.spec]\n";
    verified "polymorphic-containment" "open Vstd\ntype 'a box = Box of 'a\nlet f (x:Int.t) : Int.t = match Box x with Box y -> y [@@verocaml.spec]\n";
    rejected "runtime-adt-reverse-flow" "open Vstd\ntype box = Box of int\nlet f (x:Int.t) : Int.t = match Box x with Box y -> y [@@verocaml.spec]\n";
    verified "callback-containment" "open Vstd\nlet f (k:Int.t -> Int.t) (x:Int.t) : Int.t = k (x + 1) [@@verocaml.spec]\n";
    verified "return-containment" "open Vstd\nlet f (x:Int.t) : Int.t = x + 1 [@@verocaml.spec]\n";
    rejected "unannotated-callback-boundary" "let f k x = k (x + 1) [@@verocaml.spec]\n";
    verified "unannotated-logical-arithmetic" "let f x = x + 1 [@@verocaml.spec]\n" ]

let narrowing_identity () =
  match
    Logical_sort_private.create ~provider_origin:"test-provider"
      ~type_path:"Vstd.Int.t" ~type_uid:"int-type-v1"
      ~manifest_path:"int" ~manifest_uid:"int-manifest-v1"
      ~integer_literal_path:"Vstd.Int.of_string"
      ~integer_literal_uid:"decimal-v1" ~issuer:"test-issuer"
  with
  | Ok sort ->
      (match
         Integer_narrowing_private.issue_authenticated
           ~provider_origin:"test-provider" ~callable_path:"Narrow.to_int"
           ~callable_uid:"narrow-v1" ~logical_sort:sort
           ~issuer_authority:"test-authority"
       with
      | Ok identity -> identity
      | Error message -> failwith message)
  | Error message -> failwith message

let narrowing_alternate () =
  let sort =
    match Logical_sort_private.create ~provider_origin:"other-provider"
            ~type_path:"Vstd.Int.t" ~type_uid:"int-type-v1"
            ~manifest_path:"int" ~manifest_uid:"int-manifest-v1"
            ~integer_literal_path:"Vstd.Int.of_string"
            ~integer_literal_uid:"decimal-v1" ~issuer:"test-issuer" with
    | Ok sort -> sort | Error message -> failwith message
  in
  match Integer_narrowing_private.issue_authenticated
          ~provider_origin:"other-provider" ~callable_path:"Narrow.to_int"
          ~callable_uid:"narrow-v1" ~logical_sort:sort
          ~issuer_authority:"test-authority" with
  | Ok identity -> identity | Error message -> failwith message

let narrowing_case name check =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match narrowing_identity () with
      | identity ->
          let result = check identity in
          if result then Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
          else Error (Failure.make Failure.Expectation_mismatch "narrowing hook result") )

let narrowing_cases =
  [ narrowing_case "narrowing-authenticated-success" (fun identity ->
        match Integer_narrowing_private.validate_application
                ~candidates:[ identity ]
                ~reference:(Integer_narrowing_private.For_testing.reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Ok _ -> true | Error _ -> false);
    narrowing_case "narrowing-missing-identity" (fun identity ->
        match Integer_narrowing_private.validate_application ~candidates:[ identity ]
                ~reference:Integer_narrowing_private.Missing_reference
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Error Integer_narrowing_private.Missing_identity -> true | _ -> false);
    narrowing_case "narrowing-malformed-identity" (fun identity ->
        match Integer_narrowing_private.validate_application ~candidates:[ identity ]
                ~reference:(Integer_narrowing_private.For_testing.malformed_reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Error Integer_narrowing_private.Malformed_identity -> true | _ -> false);
    narrowing_case "narrowing-forged-identity" (fun identity ->
        let alternate = narrowing_alternate () in
        match Integer_narrowing_private.validate_application ~candidates:[ alternate ]
                ~reference:(Integer_narrowing_private.For_testing.reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Error Integer_narrowing_private.Forged_identity -> true | _ -> false);
    narrowing_case "narrowing-invalid-boundary" (fun identity ->
        match Integer_narrowing_private.validate_application ~candidates:[ identity ]
                ~reference:(Integer_narrowing_private.For_testing.reference identity)
                ~source:Parametric_type.Int ~target:Parametric_type.Mathematical_int with
        | Error Integer_narrowing_private.Invalid_semantic_boundary -> true | _ -> false);
    narrowing_case "narrowing-ambiguous-identity" (fun identity ->
        let alternate = narrowing_alternate () in
        match Integer_narrowing_private.validate_application ~candidates:[ identity; alternate ]
                ~reference:(Integer_narrowing_private.For_testing.callable_reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Error Integer_narrowing_private.Ambiguous_identity -> true | _ -> false);
    narrowing_case "narrowing-duplicate-coalescence" (fun identity ->
        match Integer_narrowing_private.validate_application ~candidates:[ identity; identity ]
                ~reference:(Integer_narrowing_private.For_testing.reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Ok _ -> true | _ -> false);
    narrowing_case "narrowing-authenticated-disambiguation" (fun identity ->
        match Integer_narrowing_private.validate_application
                ~candidates:[ identity; narrowing_alternate () ]
                ~reference:(Integer_narrowing_private.For_testing.reference identity)
                ~source:Parametric_type.Mathematical_int ~target:Parametric_type.Int with
        | Ok _ -> true | _ -> false) ]

let constant_span =
  {
    Diagnostic.file = suite_path;
    start_pos = { line = 1; column = 0 };
    end_pos = { line = 1; column = 0 };
  }

let mathematical_expression expression_desc =
  {
    Sst.expression_desc;
    typ = Sst.Mathematical_int;
    span = constant_span;
  }

let constant_token value =
  mathematical_expression (Sst.Int_constant (Z.of_int value))

let constant_unary operation argument =
  mathematical_expression (Sst.Checked_arithmetic (operation, [ argument ]))

let constant_binary operation left right =
  mathematical_expression (Sst.Checked_arithmetic (operation, [ left; right ]))

let dynamic_constant =
  mathematical_expression
    (Sst.Lift_runtime_int
       {
         Sst.expression_desc = Sst.Int_constant Z.zero;
         typ = Sst.Int;
         span = constant_span;
       })

let evaluate_constant = Mathematical_int_rewrite_private.constant_value

let constant_case name check =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      if check () then
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      else
        Error
          (Failure.make Failure.Expectation_mismatch
             "mathematical integer constant evaluator result"))

let constant_evaluator_cases =
  let open Mathematical_int_constant_private in
  [ constant_case "constant-literal-zero" (fun () ->
        Option.equal Z.equal (parse_literal "0") (Some Z.zero));
    constant_case "constant-literal-negative-arbitrary-precision" (fun () ->
        Option.equal Z.equal
          (parse_literal "-999999999999999999999999999999999999")
          (Some (Z.of_string "-999999999999999999999999999999999999")));
    constant_case "constant-literal-empty-rejected" (fun () ->
        Option.is_none (parse_literal ""));
    constant_case "constant-literal-plus" (fun () ->
        Option.equal Z.equal (parse_literal "+5") (Some (Z.of_int 5)));
    constant_case "constant-literal-leading-zero" (fun () ->
        Option.equal Z.equal (parse_literal "05") (Some (Z.of_int 5)));
    constant_case "constant-literal-separated-decimal" (fun () ->
        Option.equal Z.equal (parse_literal "1_000_000")
          (Some (Z.of_int 1_000_000)));
    constant_case "constant-literal-hexadecimal" (fun () ->
        Option.equal Z.equal (parse_literal "0xDEAD_BEEF")
          (Some (Z.of_string "0xDEAD_BEEF")));
    constant_case "constant-literal-binary" (fun () ->
        Option.equal Z.equal (parse_literal "0b1010_0101")
          (Some (Z.of_int 0b1010_0101)));
    constant_case "constant-literal-sign-only-rejected" (fun () ->
        Option.is_none (parse_literal "+"));
    constant_case "constant-literal-prefix-only-rejected" (fun () ->
        Option.is_none (parse_literal "0x"));
    constant_case "constant-five-times-five" (fun () ->
        Option.equal Z.equal
          (evaluate_constant
             (constant_binary Sst.Multiply (constant_token 5)
                (constant_token 5)))
          (Some (Z.of_int 25)));
    constant_case "constant-literal-expression" (fun () ->
        match parse_literal "12345678901234567890" with
        | None -> false
        | Some value ->
            Option.equal Z.equal
              (evaluate_constant
                 (mathematical_expression (Sst.Int_constant value)))
              (Some (Z.of_string "12345678901234567890")));
    constant_case "constant-nested-numeral-expression" (fun () ->
        Option.equal Z.equal
          (evaluate_constant
             (constant_binary Sst.Multiply
                (constant_binary Sst.Add (constant_token 2) (constant_token 3))
                (constant_binary Sst.Subtract (constant_token 8)
                   (constant_token 3))))
          (Some (Z.of_int 25)));
    constant_case "constant-unary-expression" (fun () ->
        Option.equal Z.equal
          (evaluate_constant
             (constant_unary Sst.Absolute_value
                (constant_unary Sst.Predecessor (constant_token (-4)))))
          (Some (Z.of_int 5)));
    constant_case "constant-dynamic-expression-rejected" (fun () ->
        Option.is_none
          (evaluate_constant
             (constant_binary Sst.Multiply (constant_token 5)
                dynamic_constant)));
    constant_case "constant-pair-classification" (fun () ->
        match
          classify_multiplication ~coefficient:evaluate_constant
            ~left:(constant_token 5) ~right:(constant_token 5)
        with
        | Constant_pair (left, right) ->
            Z.equal left (Z.of_int 5) && Z.equal right (Z.of_int 5)
        | Constant_left _ | Constant_right _ | Nonlinear -> false);
    constant_case "constant-left-scaling-classification" (fun () ->
        match
          classify_multiplication ~coefficient:evaluate_constant
            ~left:(constant_token 5) ~right:dynamic_constant
        with
        | Constant_left value -> Z.equal value (Z.of_int 5)
        | Constant_pair _ | Constant_right _ | Nonlinear -> false);
    constant_case "constant-right-scaling-classification" (fun () ->
        match
          classify_multiplication ~coefficient:evaluate_constant
            ~left:dynamic_constant ~right:(constant_token 5)
        with
        | Constant_right value -> Z.equal value (Z.of_int 5)
        | Constant_pair _ | Constant_left _ | Nonlinear -> false);
    constant_case "constant-nonlinear-classification" (fun () ->
        match
          classify_multiplication ~coefficient:evaluate_constant
            ~left:dynamic_constant ~right:dynamic_constant
        with
        | Nonlinear -> true
        | Constant_pair _ | Constant_left _ | Constant_right _ -> false);
    constant_case "rewrite-bottom-up-closed-expression" (fun () ->
        let expression =
          constant_binary Sst.Multiply
            (constant_binary Sst.Add (constant_token 2) (constant_token 3))
            (constant_binary Sst.Subtract (constant_token 8)
               (constant_token 3))
        in
        match
          (Mathematical_int_rewrite_private.simplify expression)
            .Sst.expression_desc
        with
        | Sst.Int_constant value -> Z.equal value (Z.of_int 25)
        | _ -> false);
    constant_case "rewrite-preserves-type-and-span" (fun () ->
        let expression =
          constant_binary Sst.Add (constant_token 20) (constant_token 22)
        in
        let rewritten = Mathematical_int_rewrite_private.simplify expression in
        rewritten.typ = expression.typ && rewritten.span = expression.span);
    constant_case "rewrite-does-not-fold-runtime-arithmetic" (fun () ->
        let runtime =
          {
            Sst.expression_desc =
              Sst.Checked_arithmetic
                ( Sst.Add,
                  [
                    {
                      Sst.expression_desc = Sst.Int_constant (Z.of_int 20);
                      typ = Sst.Int;
                      span = constant_span;
                    };
                    {
                      Sst.expression_desc = Sst.Int_constant (Z.of_int 22);
                      typ = Sst.Int;
                      span = constant_span;
                    };
                  ] );
            typ = Sst.Int;
            span = constant_span;
          }
        in
        match
          (Mathematical_int_rewrite_private.simplify runtime)
            .Sst.expression_desc
        with
        | Sst.Checked_arithmetic (Sst.Add, [ _; _ ]) -> true
        | _ -> false);
    constant_case "rewrite-does-not-hide-mixed-sort-scale" (fun () ->
        let runtime_constant =
          {
            Sst.expression_desc = Sst.Int_constant (Z.of_int 5);
            typ = Sst.Int;
            span = constant_span;
          }
        in
        let malformed =
          mathematical_expression
            (Sst.Checked_arithmetic
               (Sst.Multiply, [ runtime_constant; dynamic_constant ]))
        in
        match
          (Mathematical_int_rewrite_private.simplify malformed)
            .Sst.expression_desc
        with
        | Sst.Checked_arithmetic
            ( Sst.Multiply,
              [ { Sst.typ = Sst.Int; _ }; { Sst.typ = Sst.Mathematical_int; _ } ]
            ) ->
            true
        | _ -> false);
    constant_case "rewrite-composes-ordered-rules-bottom-up" (fun () ->
        let replace_zero expression =
          match (expression.Sst.typ, expression.expression_desc) with
          | Sst.Mathematical_int, Sst.Int_constant value when Z.equal value Z.zero ->
              Some (Sst.Int_constant Z.one)
          | _ -> None
        in
        let expression =
          constant_binary Sst.Add (constant_token 0) (constant_token 2)
        in
        let rewritten =
          Sst_expression_rewrite_private.bottom_up
            ~rules:
              [
                replace_zero;
                Mathematical_int_rewrite_private.fold_constant_arithmetic;
              ]
            expression
        in
        match rewritten.expression_desc with
        | Sst.Int_constant value ->
            Z.equal value (Z.of_int 3)
            && rewritten.typ = expression.typ
            && rewritten.span = expression.span
        | _ -> false) ]

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (logical_matrix_cases @ literal_cases @ nonlinear_arithmetic_cases
    @ quantifier_cases @ containment_cases @ narrowing_cases
    @ constant_evaluator_cases
    @ [ rejected "explicit-runtime-result" "open Vstd\nlet f (x:Int.t) : int = x + 1 [@@verocaml.spec]\n" ])
