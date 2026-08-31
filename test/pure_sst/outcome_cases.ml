open Outcome_test_support

let suite_path = "test/pure_sst/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let single_source ~module_name ~source =
  Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]

let rejection_case ~name ~module_name ~code ~source =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture (single_source ~module_name ~source))

let accepted_case ~name ~module_name ~function_name ~source =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified
      |> Expectation.require_named_fact ("function:" ^ function_name)
           (Outcome.Function_exists function_name))
    (run_fixture (single_source ~module_name ~source))

let ordinary_rejection_cases =
  [
    rejection_case ~name:"partial-match-rejected" ~module_name:"Partial_match"
      ~code:"VERO_UNSUPPORTED_PARTIAL_MATCH"
      ~source:"let partial (x : int) = match x with 0 -> 0\n";
    rejection_case ~name:"partial-parameter-rejected"
      ~module_name:"Partial_parameter"
      ~code:"VERO_UNSUPPORTED_PARTIAL_PARAMETER"
      ~source:"let partial (Some (x : int)) = x\n";
    rejection_case ~name:"mutual-recursion-rejected"
      ~module_name:"Mutual_recursion"
      ~code:"VERO_UNSUPPORTED_MUTUAL_RECURSION"
      ~source:
        {|let rec even (n : int) = n = 0 || odd (n - 1)
and odd (n : int) = n <> 0 && even (n - 1)
|};
    rejection_case ~name:"external-call-rejected"
      ~module_name:"External_call" ~code:"VERO_UNSUPPORTED_EXTERNAL_CALL"
      ~source:"let external_call (x : int) = print_int x\n";
    rejection_case ~name:"while-loop-rejected" ~module_name:"While_loop"
      ~code:"VERO_UNSUPPORTED_LOOP"
      ~source:"let loop (x : int) = while false do ignore x done\n";
    rejection_case ~name:"exception-rejected" ~module_name:"Exception_path"
      ~code:"VERO_UNSUPPORTED_EXCEPTION"
      ~source:"let exception_path (x : int) = try x with _ -> 0\n";
    rejection_case ~name:"array-rejected" ~module_name:"Array_value"
      ~code:"VERO_UNSUPPORTED_ARRAY"
      ~source:"let array () = [| 1; 2 |]\n";
    rejection_case ~name:"object-rejected" ~module_name:"Object_value"
      ~code:"VERO_UNSUPPORTED_OBJECT"
      ~source:"let object_value () = object method value = 1 end\n";
    rejection_case ~name:"first-class-module-rejected"
      ~module_name:"First_class_module"
      ~code:"VERO_UNSUPPORTED_FIRST_CLASS_MODULE"
      ~source:
        {|let packed () =
  (module struct
    type t = int
    let compare (left : int) right = Stdlib.compare left right
  end : Set.OrderedType with type t = int)
|};
    rejection_case ~name:"concurrency-rejected" ~module_name:"Concurrency"
      ~code:"VERO_UNSUPPORTED_CONCURRENCY"
      ~source:
        "let concurrent () = Domain.join (Domain.spawn (fun () -> 1))\n";
    rejection_case ~name:"higher-order-rejected" ~module_name:"Higher_order"
      ~code:"VERO_CALLBACK_CONTRACT"
      ~source:"let higher_order (f : int -> int) (x : int) = f x\n";
    rejection_case ~name:"nonlinear-multiplication-rejected"
      ~module_name:"Nonlinear" ~code:"VERO_UNSUPPORTED_NONLINEAR_MULTIPLICATION"
      ~source:"let nonlinear (x : int) (y : int) = x * y\n";
    rejection_case ~name:"aggregate-equality-rejected"
      ~module_name:"Aggregate_equality" ~code:"VERO_UNSUPPORTED_GENERIC_USE"
      ~source:
        "let aggregate_equality (x : int * int) (y : int * int) = x = y\n";
    rejection_case ~name:"left-shift-rejected" ~module_name:"Wrapping"
      ~code:"VERO_UNSUPPORTED_WRAPPING_ARITHMETIC"
      ~source:"let wrapping (x : int) = x lsl 1\n";
    rejection_case ~name:"division-rejected" ~module_name:"Division"
      ~code:"VERO_UNSUPPORTED_WRAPPING_ARITHMETIC"
      ~source:"let division (x : int) = x / 2\n";
    rejection_case ~name:"remainder-rejected" ~module_name:"Remainder"
      ~code:"VERO_UNSUPPORTED_WRAPPING_ARITHMETIC"
      ~source:"let remainder (x : int) = x mod 2\n";
    rejection_case ~name:"bitwise-rejected" ~module_name:"Bitwise"
      ~code:"VERO_UNSUPPORTED_WRAPPING_ARITHMETIC"
      ~source:"let bitwise (x : int) = x land 1\n";
    rejection_case ~name:"mutation-rejected" ~module_name:"Mutation"
      ~code:"VERO_UNSUPPORTED_MUTATION"
      ~source:
        "let mutation (x : int) = let mutable y = x in y <- x + 1; y\n";
    rejection_case ~name:"partial-string-match-rejected"
      ~module_name:"Partial_match_string"
      ~code:"VERO_UNSUPPORTED_PARTIAL_MATCH"
      ~source:
        "let partial_match_string (x : int) = match x with 0 -> \"zero\"\n";
    rejection_case ~name:"external-string-call-rejected"
      ~module_name:"External_string" ~code:"VERO_UNSUPPORTED_EXTERNAL_CALL"
      ~source:"let external_string (x : int) = string_of_int x\n";
    rejection_case ~name:"direct-ghost-call-rejected"
      ~module_name:"Direct_ghost" ~code:"VERO_MALFORMED_GHOST_CALL"
      ~source:
        "let direct_ghost (x : int) = Vero_ghost.requires (fun () -> x >= 0); x\n";
  ]

let accepted_cases =
  [
    accepted_case ~name:"polymorphic-identity-admitted"
      ~module_name:"Polymorphic_identity" ~function_name:"identity"
      ~source:"let identity x = x\n";
    accepted_case ~name:"aggregate-read-admitted" ~module_name:"Aggregate_read"
      ~function_name:"read"
      ~source:
        "type aggregate = { value : int }\nlet read value = value.value\n";
  ]

let effect_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name effect_fixture)\n";
          };
          {
            path = "dune";
            contents =
              "(library\n (name effect_fixture)\n (wrapped false)\n (modules \
               Effect_support Effect_case)\n (libraries verocaml.ghost)\n (flags \
               (:standard -ppx \"verocaml-ppx --keep-ghost\")))\n";
          };
          {
            path = "effect_support.ml";
            contents = "type _ Effect.t += Tick : int Effect.t\n";
          };
          {
            path = "effect_case.ml";
            contents =
              "let effect () : int = Effect.perform Effect_support.Tick\n";
          };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Effect_case" ];
    }

let effect_case =
  Suite.case ~name:"effect-rejected-in-project"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EFFECT"
      |> Expectation.require_unit "Effect_case" Outcome.Unit_frontend_rejected)
    (run_fixture effect_project)

let counterfeit_stdlib_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name counterfeit_stdlib_fixture)\n";
          };
          {
            path = "dune";
            contents =
              "(library\n (name fake_stdlib)\n (wrapped false)\n (modules \
               Stdlib)\n (flags (:standard -nopervasives)))\n\n(library\n \
               (name counterfeit_stdlib_fixture)\n (wrapped false)\n (modules \
               Counterfeit_stdlib_user)\n (libraries fake_stdlib)\n (flags \
               (:standard -nopervasives)))\n";
          };
          {
            path = "stdlib.ml";
            contents =
              "let ( + ) (left : int) (_right : int) = left\n";
          };
          {
            path = "counterfeit_stdlib_user.ml";
            contents =
              "let counterfeit_stdlib (x : int) = Stdlib.( + ) x 1\n";
          };
        ];
      libraries = [];
      targets = [ "@all" ];
      selected_units = [ "Counterfeit_stdlib_user" ];
    }

let counterfeit_stdlib_case =
  Suite.case ~name:"counterfeit-stdlib-identity-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
      |> Expectation.require_unit "Counterfeit_stdlib_user"
           Outcome.Unit_frontend_rejected)
    (run_fixture counterfeit_stdlib_project)

let counterfeit_ghost_project =
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name counterfeit_ghost_fixture)\n";
          };
          {
            path = "dune";
            contents =
              "(library\n (name fake_ghost)\n (wrapped false)\n (modules \
               Vero_ghost))\n\n(library\n (name counterfeit_ghost_fixture)\n \
               (wrapped false)\n (modules Counterfeit_ghost_user)\n (libraries \
               fake_ghost)\n (flags (:standard -ppx \"verocaml-ppx \
               --keep-ghost\")))\n";
          };
          {
            path = "vero_ghost.ml";
            contents =
              "let requires (_ : unit -> bool) = ()\nlet marker (_ : string) = \
               ()\nlet sidecar (_ : string) = ()\n";
          };
          {
            path = "counterfeit_ghost_user.ml";
            contents =
              "let counterfeit_ghost (x : int) =\n  [%verocaml.requires x >= \
               0];\n  x + 1\n";
          };
        ];
      libraries = [];
      targets = [ "@all" ];
      selected_units = [ "Counterfeit_ghost_user" ];
    }

let counterfeit_ghost_case =
  Suite.case ~name:"counterfeit-ghost-identity-rejected"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
      |> Expectation.require_unit "Counterfeit_ghost_user"
           Outcome.Unit_frontend_rejected)
    (run_fixture counterfeit_ghost_project)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (ordinary_rejection_cases @ accepted_cases
   @ [ effect_case; counterfeit_stdlib_case; counterfeit_ghost_case ])
