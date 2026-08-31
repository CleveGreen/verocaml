open Outcome_test_support

let suite_path = "test/rank_backed_specifications/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/rank_backed_specifications" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let source_input name =
  Fixture.single_source ~module_name:(String.capitalize_ascii name)
    ~source:(read_file ("fixtures/" ^ name ^ ".ml"))
    ~libraries:[ "verocaml.ghost" ]

let source_case name fixture expectation =
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (source_input fixture))

let verified unit_name functions =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit unit_name Outcome.Unit_verified)
    functions

let rejected unit_name code =
  Expectation.empty |> Expectation.status Outcome.Frontend_rejected
  |> Expectation.require_frontend_code code
  |> Expectation.require_unit unit_name Outcome.Unit_frontend_rejected

let shape_only_source =
  {|
type 'a node = Empty | Node of 'a * 'a node

let verified (node : int node) =
  [%verocaml.assert node = node];
  ()
|}

let shape_only =
  let input =
    Fixture.single_source ~module_name:"Shape_only" ~source:shape_only_source
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"shape-only-verification"
    ~expectation:(verified "Shape_only" [ "verified" ])
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      source_case "rank-backed-specifications" "positive"
        (verified "Positive" [ "verified" ]);
      source_case "rank-backed-proof" "proof_rank_backed_positive"
        (verified "Proof_rank_backed_positive"
           [ "lemma_push_front_wf"; "lemma_push_front_wf_tracked"; "complete_shape" ]);
      shape_only;
      source_case "profile-mismatch-rejection" "profile_mismatch"
        (rejected "Profile_mismatch" "VERO_UNSUPPORTED_TYPE");
      source_case "reference-carrier-rejection" "reference_carrier"
        (rejected "Reference_carrier" "VERO_UNSUPPORTED_TYPE");
      source_case "function-carrier-rejection" "function_carrier"
        (rejected "Function_carrier" "VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION");
      source_case "object-carrier-rejection" "object_carrier"
        (rejected "Object_carrier" "VERO_UNSUPPORTED_TYPE");
      source_case "foreign-carrier-rejection" "foreign_carrier"
        (rejected "Foreign_carrier" "VERO_UNSUPPORTED_TYPE");
      source_case "abstract-carrier-rejection" "abstract_carrier"
        (rejected "Abstract_carrier" "VERO_UNSUPPORTED_STRUCTURE_ITEM");
      source_case "polymorphic-carrier-accepted" "polymorphic_carrier"
        (verified "Polymorphic_carrier" []);
      source_case "no-finite-alias-rejection" "no_finite_alias"
        (rejected "No_finite_alias" "VERO_MALFORMED_GHOST_CALL");
      source_case "no-finite-rebinding-rejection" "no_finite_rebinding"
        (rejected "No_finite_rebinding" "VERO_MALFORMED_GHOST_CALL");
      source_case "no-finite-branch-rejection" "no_finite_branch_launder"
        (rejected "No_finite_branch_launder" "VERO_MALFORMED_GHOST_CALL");
      source_case "runtime-cycle-rejection" "runtime_cycle"
        (rejected "Runtime_cycle" "VERO_UNSUPPORTED_TOP_LEVEL_BINDING");
      source_case "runtime-indirect-cycle-rejection" "runtime_indirect_cycle"
        (rejected "Runtime_indirect_cycle" "VERO_UNSUPPORTED_MUTUAL_RECURSION");
    ]
