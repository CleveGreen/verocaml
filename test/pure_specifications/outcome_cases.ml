open Outcome_test_support

let suite_path = "test/pure_specifications/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let positive_source =
  {|
let math_succ (x : int) : int = x + 1 [@@verocaml.spec]

let twice (x : int) : int = math_succ (math_succ x) [@@verocaml.spec]

let growing (x : int) : bool = twice x > x [@@verocaml.spec]

let verified (x : int) : int =
  [%verocaml.requires growing x];
  [%verocaml.assert twice x = x + 2];
  [%verocaml.ensures fun result -> result = x];
  x
|}

let verified_expansion_case =
  let input =
    Fixture.single_source ~module_name:"Verified_expansion"
      ~source:positive_source ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"logical-expansion-verifies"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Verified_expansion" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:verified"
           (Outcome.Function_exists "verified")
      |> Expectation.require_named_fact "obligation-kind:verified"
           (Outcome.Obligation_kind_exists
              { function_name = "verified"; kind = Outcome.Assertion })
      |> Expectation.require_named_fact "obligation-kind:verified"
           (Outcome.Obligation_kind_exists
              { function_name = "verified"; kind = Outcome.Postcondition }))
    (run_fixture input)

let retained_input ~module_name ~source =
  Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]

let rejection_case ~name ~module_name ~code ~source =
  let input = retained_input ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture input)

let pre_vir_case ~name ~module_name ~code ~source =
  let input = retained_input ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code code
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture input)

let no_ppx_project ~module_name ~source =
  let source_name = String.uncapitalize_ascii module_name ^ ".ml" in
  Fixture.dune_project
    {
      files =
        [
          {
            Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name pure_spec_raw_fixture)\n";
          };
          {
            path = "dune";
            contents =
              Printf.sprintf
                "(library\n (name pure_spec_raw_fixture)\n (wrapped false)\n \
                 (modules %s)\n (libraries verocaml.ghost))\n"
                module_name;
          };
          { path = source_name; contents = source };
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ module_name ];
    }

let raw_rejection_case ~name ~module_name ~source =
  let input = no_ppx_project ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture input)

let ordinary_rejection_cases =
  [
    pre_vir_case ~name:"runtime-spec-use-rejected-before-vir"
      ~module_name:"Runtime_spec_use" ~code:"VERO_ERASED_CALL"
      ~source:
        {|let math_succ (x : int) : int = x + 1 [@@verocaml.spec]
let runtime_use (x : int) : int = math_succ x
|};
    pre_vir_case ~name:"spec-to-exec-call-rejected-before-vir"
      ~module_name:"Spec_to_exec_call" ~code:"VERO_INVALID_PROGRAM"
      ~source:
        {|let executable (x : int) : int = x + 1
let bad (x : int) : int = executable x [@@verocaml.spec]
|};
    rejection_case ~name:"external-call-rejected"
      ~module_name:"Spec_external_call" ~code:"VERO_UNSUPPORTED_EXTERNAL_CALL"
      ~source:
        {|let bad (x : int) : int =
  print_int x;
  x
[@@verocaml.spec]
|};
    rejection_case ~name:"contract-on-spec-rejected"
      ~module_name:"Spec_contract" ~code:"VERO_MALFORMED_GHOST_CALL"
      ~source:
        {|let bad (x : int) : int =
  [%verocaml.requires x > 0];
  x + 1
[@@verocaml.spec]
|};
    rejection_case ~name:"assertion-on-spec-rejected"
      ~module_name:"Spec_assertion" ~code:"VERO_MALFORMED_GHOST_CALL"
      ~source:
        {|let bad (x : int) : int =
  [%verocaml.assert x = x];
  x
[@@verocaml.spec]
|};
    rejection_case ~name:"decreases-on-spec-rejected"
      ~module_name:"Spec_decreases" ~code:"VERO_MALFORMED_GHOST_CALL"
      ~source:
        {|let bad (x : int) : int =
  [%verocaml.decreases x];
  x
[@@verocaml.spec]
|};
    rejection_case ~name:"mutation-on-spec-rejected"
      ~module_name:"Spec_mutation" ~code:"VERO_UNSUPPORTED_MUTATION"
      ~source:
        {|let bad (x : int) : int =
  let mutable y = x in
  y <- y + 1;
  y
[@@verocaml.spec]
|};
    raw_rejection_case ~name:"raw-attribute-is-not-authority"
      ~module_name:"Raw_attribute"
      ~source:"let raw (x : int) : int = x + 1 [@@verocaml.spec]\n";
    raw_rejection_case ~name:"counterfeit-ghost-call-is-not-authority"
      ~module_name:"Counterfeit_ghost_call"
      ~source:
        {|let fake (x : int) : int =
  Vero_ghost.spec_definition "verocaml:spec:1:0:0:0:0:fake"
    (fun () -> x + 1)
|};
  ]

let admitted_spec_case ~name ~module_name ~source =
  let input = retained_input ~module_name ~source in
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit module_name Outcome.Unit_verified)
    (run_fixture input)

let admitted_spec_cases =
  [
    admitted_spec_case ~name:"required-labelled-spec-admitted"
      ~module_name:"Labelled_spec"
      ~source:
        "let labelled ~(x : int) : int = x + 1 [@@verocaml.spec]\n";
    admitted_spec_case ~name:"higher-order-spec-admitted"
      ~module_name:"Higher_order_spec"
      ~source:
        "let higher_order f (x : int) : int = f x [@@verocaml.spec]\n";
    admitted_spec_case ~name:"polymorphic-spec-admitted"
      ~module_name:"Polymorphic_spec"
      ~source:"let identity x = x [@@verocaml.spec]\n";
  ]

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (verified_expansion_case :: ordinary_rejection_cases @ admitted_spec_cases)
