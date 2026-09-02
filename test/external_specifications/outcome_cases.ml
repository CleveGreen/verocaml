open Outcome_test_support

let suite_path = "test/external_specifications/outcome_cases.ml"

let input ~module_name ~source =
  Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let malformed_ghost_call_case ~name ~module_name ~source =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture (input ~module_name ~source))

let pre_vir_rejection_case ~name ~module_name ~source =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_INVALID_PROGRAM"
      |> Expectation.require_unit module_name Outcome.Unit_frontend_rejected)
    (run_fixture (input ~module_name ~source))

let malformed_ghost_call_cases =
  [
    malformed_ghost_call_case
      ~name:"malformed-ghost-call-altered-argument"
      ~module_name:"Altered_argument"
      ~source:
        {|let target (value : bool) = value
let wrapper (value : bool) = target (not value)
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-arity"
      ~module_name:"Arity"
      ~source:
        {|let target (x : int) (y : int) = x + y
let wrapper (x : int) (y : int) = target x x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-label"
      ~module_name:"Label"
      ~source:
        {|let target ~(x : int) = x
let wrapper (x : int) = target ~x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-mode"
      ~module_name:"Mode"
      ~source:
        {|let target (x : int @ unique) = x
let wrapper (x : int @ unique) = target x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-aggregate"
      ~module_name:"Aggregate"
      ~source:
        {|let target ((x, y) : int * int) = x + y
let wrapper ((x, y) : int * int) = target (x, y)
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-assertion"
      ~module_name:"Assertion"
      ~source:
        {|let target (x : int) = x
let wrapper (x : int) =
  [%verocaml.assert x = x];
  target x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-decreases"
      ~module_name:"Decreases"
      ~source:
        {|let target (x : int) = x
let wrapper (x : int) =
  [%verocaml.decreases x];
  target x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-recursive-target"
      ~module_name:"Recursive_target"
      ~source:
        {|let rec target (x : int) = if x = 0 then 0 else target (x - 1)
let wrapper (x : int) = target x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-imported-target"
      ~module_name:"Imported_target"
      ~source:
        {|let wrapper (x : int) = Stdlib.abs x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-duplicate-target"
      ~module_name:"Duplicate_target"
      ~source:
        {|let target (x : int) = x
let wrapper1 (x : int) = target x
[@@verocaml.external_specification]
let wrapper2 (x : int) = target x
[@@verocaml.external_specification]
|};
    malformed_ghost_call_case ~name:"malformed-ghost-call-wrapper-as-target"
      ~module_name:"Wrapper_as_target"
      ~source:
        {|let target (x : int) = x
let wrapper1 (x : int) = target x
[@@verocaml.external_specification]
let wrapper2 (x : int) = wrapper1 x
[@@verocaml.external_specification]
|};
  ]

let pre_vir_rejection_cases =
  [
    pre_vir_rejection_case ~name:"pre-vir-rejects-wrapper-invocation"
      ~module_name:"Wrapper_invocation"
      ~source:
        {|let target (x : int) = x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let caller (x : int) = wrapper x
|};
    pre_vir_rejection_case ~name:"pre-vir-rejects-direct-before-spec"
      ~module_name:"Direct_before_spec"
      ~source:
        {|let target (x : int) = x
let premature (x : int) = target x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let later (x : int) = target x
|};
    pre_vir_rejection_case ~name:"pre-vir-rejects-proof-call-to-target"
      ~module_name:"Proof_calls_target"
      ~source:
        {|let target (x : int) = x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let lemma (x : int) : unit =
  let _ = target x in
  ()
[@@verocaml.proof]
|};
  ]

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (malformed_ghost_call_cases @ pre_vir_rejection_cases)
