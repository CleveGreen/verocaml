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

let unique_empty_return_source =
  {vero|type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let stack_wf (stack : stack) : bool = stack.length >= 0
[@@verocaml.spec]

let lemma_empty_stack (stack : stack) : unit =
  [%verocaml.requires stack_wf stack];
  ()
[@@verocaml.proof]

let empty_stack (_ : unit) : stack =
  [%verocaml.ensures fun result -> stack_wf result];
  let empty = { top = Empty; length = 0 } in
  [%verocaml.proof lemma_empty_stack empty];
  empty
[@@verocaml.external_body]
|vero}

let checked_unique_empty_return_source =
  {vero|type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let stack_wf (stack : stack) : bool = stack.length >= 0
[@@verocaml.spec]

let lemma_empty_stack (stack : stack) : unit =
  [%verocaml.requires stack_wf stack];
  ()
[@@verocaml.proof]

let empty_stack (_ : unit) : stack =
  [%verocaml.ensures fun result -> stack_wf result];
  let empty = { top = Empty; length = 0 } in
  [%verocaml.proof lemma_empty_stack empty];
  empty
|vero}

let recursive_control_source =
  {vero|let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.revealed]

let reveal_on_one_path (take_path : bool) : unit =
  if take_path then (
    [%verocaml.reveal_with_fuel (recurse, 2)];
    ())
  else ()
[@@verocaml.proof]
|vero}

let stale_owned_invariant_source =
  {vero|type snapshot = { length : int }

module type STACK = sig
  type t
  val singleton : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val snapshot : t @ read -> snapshot @ immutable
  val zero_head : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let model (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    (model stack).length >= 0
  [@@verocaml.type_invariant]

  let snapshot (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }

  let zero_head (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.value <- 0;
        stack
end

let run value : unit =
  let stack = Stack.singleton value in
  let stack = Stack.zero_head stack in
  [%verocaml.proof [%verocaml.use_type_invariant stack]];
  ()
|vero}

let effectful_payload_source =
  {vero|let bad source =
  [%verocaml.proof print_int source];
  source
|vero}

let source_written_carrier_source =
  {vero|let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let attack value =
  Vero_ghost.marker
    "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=61747461636b|binding=0,0|body=0,0|region=0,0|slots=76616c7565,0,0";
  (if false then
     ignore
       (fun (value : _ @ aliased) ->
         Vero_ghost.sidecar
           "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=61747461636b|binding=0,0|body=0,0|region=0,0|slots=76616c7565,0,0";
         Vero_ghost.proof_region "verocaml:proof-region:2:0:0"
           (fun () -> observe value))
   else ());
  value
|vero}

let old_direct_carrier_source =
  {vero|let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let attack value =
  Vero_ghost.proof_region "verocaml:proof-region:1:0:0"
    (fun () -> observe value);
  value
|vero}

let unique_empty_return =
  base "Unique_empty_return" Outcome.Verified Outcome.Unit_verified
  |> source_case ~name:"trusted-unique-return" ~module_name:"Unique_empty_return"
       ~source:unique_empty_return_source

let checked_unique_empty_return =
  base "Checked_unique_empty_return" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "lemma_empty_stack"; "empty_stack" ]
  |> require_kinds "empty_stack"
       [ Outcome.Call_precondition { callee = "lemma_empty_stack" };
         Outcome.Postcondition ]
  |> source_case ~name:"checked-unique-return"
       ~module_name:"Checked_unique_empty_return"
       ~source:checked_unique_empty_return_source

let recursive_control =
  base "Recursive_control" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "reveal_on_one_path" ]
  |> source_case ~name:"recursive-control" ~module_name:"Recursive_control"
       ~source:recursive_control_source

let stale_owned_invariant =
  base "Stale_owned_invariant" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "run" ]
  |> source_case ~name:"completed-result-invariant-transition"
       ~module_name:"Stale_owned_invariant" ~source:stale_owned_invariant_source

let effectful_payload =
  base "Effectful_payload" Outcome.Frontend_rejected
    Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code "VERO_UNSUPPORTED_EXTERNAL_CALL"
  |> source_case ~name:"effectful-proof-region-payload"
       ~module_name:"Effectful_payload" ~source:effectful_payload_source

let malformed name module_name source =
  base module_name Outcome.Frontend_rejected Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code "VERO_MALFORMED_GHOST_CALL"
  |> source_case ~name ~module_name ~source

let source_written_carrier =
  malformed "source-written-carrier" "Source_written_carrier"
    source_written_carrier_source
let old_direct_carrier =
  malformed "old-direct-carrier" "Old_direct_carrier" old_direct_carrier_source

let () =
  Suite.run_cli ~suite_path:"test/proof_region_modes/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ unique_empty_return; checked_unique_empty_return; recursive_control; stale_owned_invariant; effectful_payload;
      source_written_carrier; old_direct_carrier ]
