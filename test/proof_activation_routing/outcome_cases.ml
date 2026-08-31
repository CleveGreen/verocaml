open Outcome_test_support

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let base module_name status disposition =
  Expectation.empty |> Expectation.status status
  |> Expectation.require_unit module_name disposition

let require_functions names expectation =
  List.fold_left
    (fun expectation name ->
      Expectation.require_named_fact ("function:" ^ name)
        (Outcome.Function_exists name) expectation)
    expectation names

let require_kinds function_name kinds expectation =
  List.fold_left
    (fun expectation kind ->
      Expectation.require_named_fact ("obligation-kind:" ^ function_name)
        (Outcome.Obligation_kind_exists { function_name; kind }) expectation)
    expectation kinds

let source_case ~name ~module_name ~source expectation =
  let input =
    Fixture.single_source ~module_name ~source ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name ~expectation (run_fixture input)

let fanout_positive_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let rec seeded_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + seeded_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let lemma_empty_stack (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let lemma_empty_stack_with_call (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]

let one_path_control (_dummy : bool) =
  [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]

let multiple_postconditions (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.ensures fun _result ->
    spec_node_len stack.top = stack.length
    && spec_node_len Empty = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let seeded_fanout (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result ->
    seeded_node_len stack.top = stack.length];
  ()
[@@verocaml.proof]

let nested_branch_match (take_left : bool) (node : int node [@finite]) =
  if take_left then
    (match node with
    | Empty ->
        [%verocaml.reveal_with_fuel (spec_node_len, 2)];
        affirm (spec_node_len Empty = 0)
    | Node _ ->
        [%verocaml.reveal_with_fuel (spec_node_len, 2)])
  else
    [%verocaml.reveal_with_fuel (spec_node_len, 1)]
[@@verocaml.proof]
|vero}

let ordinary_contract_match_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let checked_contract (nodes : int node) =
  [%verocaml.ensures fun _ ->
    match nodes with Empty -> true | Node (value, rest) -> Node (value, rest) = nodes];
  ()
[@@verocaml.proof]

let direct_finite_caller (nodes : int node [@finite]) =
  checked_contract nodes
[@@verocaml.proof]

let rec recursive_finite_caller (nodes : int node [@finite]) =
  [%verocaml.decreases nodes];
  checked_contract nodes;
  match nodes with Empty -> () | Node (_, rest) -> recursive_finite_caller rest
[@@verocaml.proof]

let external_contract (nodes : int node) =
  [%verocaml.ensures fun _ ->
    match nodes with Empty -> true | Node (value, rest) -> Node (value, rest) = nodes];
  ()
[@@verocaml.proof]
[@@verocaml.external_body]

let external_finite_caller (nodes : int node [@finite]) =
  external_contract nodes
[@@verocaml.proof]

let rec external_recursive_finite_caller (nodes : int node [@finite]) =
  [%verocaml.decreases nodes];
  external_contract nodes;
  match nodes with Empty -> () | Node (_, rest) -> external_recursive_finite_caller rest
[@@verocaml.proof]

let checked_finite_same_function (nodes : int node [@finite]) =
  [%verocaml.ensures fun _ ->
    match nodes with Empty -> true | Node (value, rest) -> Node (value, rest) = nodes];
  match nodes with Empty -> () | Node (value, rest) ->
    [%verocaml.assert Node (value, rest) = nodes]
[@@verocaml.proof]

let ordinary_body_match (nodes : int node [@finite]) =
  match nodes with Empty -> () | Node _ -> ()
[@@verocaml.proof]

let ordinary_postcondition_match (nodes : int node) =
  [%verocaml.ensures fun _ -> match nodes with Empty -> true | Node _ -> true];
  ()
[@@verocaml.proof]
|vero}

let no_reveal_negative_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let lemma_empty_stack (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  ()
[@@verocaml.proof]
|vero}

let nonleak_negative_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let sibling_nonleak (take_reveal : bool) =
  if take_reveal then
    [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]
|vero}

let later_nonleak_negative_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let later_nonleak (_dummy : bool) =
  affirm (spec_node_len Empty = 0);
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]
|vero}

let cross_callable_negative_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let revealing_callee (_dummy : bool) =
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let cross_callable_nonleak (_dummy : bool) =
  revealing_callee true;
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]
|vero}

let opaque_seed_negative_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec opaque_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + opaque_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let opaque_seed_control (_dummy : bool) =
  affirm (opaque_node_len Empty = 0)
[@@verocaml.proof]
|vero}

let external_finite_rejected_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let lemma_peel_push (nodes : int node [@finite]) =
  [%verocaml.ensures fun _ ->
    match nodes with
    | Empty -> true
    | Node (value, rest) -> Node (value, rest) = nodes];
  ()
[@@verocaml.proof]
[@@verocaml.external_body]

let finite_caller (nodes : int node [@finite]) =
  lemma_peel_push nodes
[@@verocaml.proof]
|vero}

let fanout_positive =
  base "Fanout_positive" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "lemma_empty_stack"; "lemma_empty_stack_with_call"; "seeded_fanout";
         "nested_branch_match" ]
  |> require_kinds "lemma_empty_stack" [ Outcome.Postcondition ]
  |> require_kinds "lemma_empty_stack_with_call"
       [ Outcome.Call_precondition { callee = "affirm" }; Outcome.Postcondition ]
  |> require_kinds "seeded_fanout" [ Outcome.Postcondition ]
  |> source_case ~name:"reached-proof-fanout" ~module_name:"Fanout_positive"
       ~source:fanout_positive_source

let ordinary_contract_matches =
  base "Ordinary_contract_match" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "direct_finite_caller"; "recursive_finite_caller";
         "external_finite_caller"; "external_recursive_finite_caller";
         "checked_finite_same_function"; "ordinary_body_match";
         "ordinary_postcondition_match" ]
  |> require_kinds "checked_finite_same_function" [ Outcome.Local_assertion ]
  |> require_kinds "ordinary_postcondition_match" [ Outcome.Postcondition ]
  |> source_case ~name:"ordinary-contract-matches"
       ~module_name:"Ordinary_contract_match"
       ~source:ordinary_contract_match_source

let failing_call_case name module_name source function_name =
  base module_name Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ function_name ]
  |> Expectation.require_semantic ~function_name
       (Outcome.Call_precondition { callee = "affirm" })
  |> source_case ~name ~module_name ~source

let no_reveal =
  base "No_reveal_negative" Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ "lemma_empty_stack" ]
  |> Expectation.require_semantic ~function_name:"lemma_empty_stack"
       Outcome.Postcondition
  |> source_case ~name:"missing-reveal" ~module_name:"No_reveal_negative"
       ~source:no_reveal_negative_source

let sibling_nonleak =
  failing_call_case "sibling-reveal-does-not-leak" "Nonleak_negative"
    nonleak_negative_source "sibling_nonleak"

let later_nonleak =
  failing_call_case "later-reveal-does-not-retroactivate"
    "Later_nonleak_negative" later_nonleak_negative_source "later_nonleak"

let cross_callable_nonleak =
  failing_call_case "cross-callable-reveal-does-not-leak"
    "Cross_callable_negative" cross_callable_negative_source
    "cross_callable_nonleak"

let opaque_seed =
  failing_call_case "opaque-seed-does-not-activate" "Opaque_seed_negative"
    opaque_seed_negative_source "opaque_seed_control"

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let external_finite_rejected ~environment ~workspace =
  let source = "external_finite_rejected.ml" in
  write_file (Filename.concat workspace source) external_finite_rejected_source;
  Process_adapter.run ~cwd:workspace
    {
      program =
        Filename.concat (Project_environment.binary_root environment) "verocaml";
      arguments = [ "verify"; source ];
      forwarded = [ ("OCAML_COLOR", "never") ];
      cleanup_paths = [];
      adjacency = [];
    }

let external_finite_rejected_case =
  Suite.case ~name:"external-finite-formal-rejected"
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact
           (Outcome.Stable_code "VERO_INVALID_PROGRAM")
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR"))
    external_finite_rejected

let () =
  Suite.run_cli ~suite_path:"test/proof_activation_routing/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ fanout_positive; ordinary_contract_matches; no_reveal; sibling_nonleak;
      later_nonleak; cross_callable_nonleak; opaque_seed;
      external_finite_rejected_case ]
