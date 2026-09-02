open Outcome_test_support

let ( let* ) = Result.bind

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

let source_input
    ?(libraries = [ "verocaml.vstd"; "verocaml.ghost" ]) module_name source =
  Fixture.single_source ~module_name ~source ~libraries

let source_case
    ?(libraries = [ "verocaml.vstd"; "verocaml.ghost" ]) ~name ~module_name ~source
    expectation =
  Suite.case ~name ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (source_input ~libraries module_name source))

let tagged key value outcome =
  Outcome.merge
    [ outcome;
      Outcome.observation ~status:Outcome.Verified
        ~named_facts:[ (key, Outcome.Function_exists value) ] ()
      |> Outcome.project ]

let parity_or_failure ~except left right =
  match Outcome.semantic_parity ~except left right with
  | Ok () -> Ok ()
  | Error message -> Error (Failure.make Failure.Expectation_mismatch message)

let rec files_below root =
  if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt workspace basename =
  let matches =
    files_below (Filename.concat workspace "project/_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) basename)
  in
  match matches with
  | [ path ] -> Ok path
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_discovery
           ("expected one prepared CMT named " ^ basename))

let prepared_parity ~module_name ~source ~environment ~workspace =
  let source_workspace = Filename.concat workspace "source" in
  let* source_outcome =
    Fixture.run ~environment ~workspace:source_workspace
      (source_input module_name source)
  in
  let basename = String.uncapitalize_ascii module_name ^ ".cmt" in
  let* cmt = discover_cmt source_workspace basename in
  let* prepared =
    match Fixture.prepared_cmt ~declared_dependencies:[ cmt ] cmt with
    | Ok input -> Ok input
    | Error message -> Error (Failure.make Failure.Selected_cmt_load message)
  in
  let* prepared_outcome =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "prepared")
      prepared
  in
  let source_outcome = tagged "input-mode" "dune-source" source_outcome in
  let prepared_outcome = tagged "input-mode" "prepared-cmt" prepared_outcome in
  let* () =
    parity_or_failure ~except:[ "input-mode" ] source_outcome prepared_outcome
  in
  Ok (Outcome.merge [ source_outcome; prepared_outcome ])

let prepared_parity_case ~name ~module_name ~source expectation =
  let expectation =
    expectation
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "dune-source")
    |> Expectation.require_named_fact "input-mode"
         (Outcome.Function_exists "prepared-cmt")
  in
  Suite.case ~name ~expectation
    (prepared_parity ~module_name ~source)

let immutable_pattern_reconstruction_source =
  {vero|let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]

type 'a node =
  | Empty
  | Node of 'a * 'a node

let branch_reconstruction (nodes : int node) =
  [%verocaml.ensures fun _ ->
    match nodes with
    | Empty -> true
    | Node (value, rest) -> Node (value, rest) = nodes];
  ()
[@@verocaml.proof]

let direct_branch_assert (nodes : int node) =
  match nodes with
  | Empty -> ()
  | Node (value, rest) ->
      [%verocaml.assert Node (value, rest) = nodes]
[@@verocaml.proof]

let assumed_branch_assert (nodes : int node) =
  match nodes with
  | Empty -> ()
  | Node (value, rest) ->
      assume (Node (value, rest) = nodes);
      [%verocaml.assert Node (value, rest) = nodes]
[@@verocaml.proof]
|vero}

let positive_matrix_source =
  {vero|type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let pure_let (x : int) =
  let y = x + 1 in
  [%verocaml.assert y = x + 1]
[@@verocaml.proof]

let branch_reveal (take_left : bool) =
  if take_left then (
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0])
  else ()
[@@verocaml.proof]

let both_predecessors (take_left : bool) =
  let empty = Empty in
  if take_left then
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert spec_node_len empty = 0];
       ()];
     ())
  else
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert spec_node_len empty = 0];
       ()];
     ());
  [%verocaml.proof
    [%verocaml.assert spec_node_len empty <= 0];
    ()];
  ()

let tracked_local (source : int [@tracked]) =
  let[@tracked] tracked = (source [@tracked]) in
  [%verocaml.assert (tracked [@tracked]) = (source [@tracked])]
[@@verocaml.proof]
|vero}

let create_stack_with_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let create_stack_with (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

let scalar_after_let (value : int) : int =
  let local = value in
  [%verocaml.proof
    [%verocaml.assert local = value];
    ()];
  local

let two_assertions () : int =
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0];
    [%verocaml.reveal_with_fuel (spec_node_len, 2)];
    [%verocaml.assert spec_node_len Empty = 0];
    ()];
  0

let branch_intersection (choose : bool) (value : int) : int =
  let local = value in
  if choose then
    ([%verocaml.proof
       [%verocaml.assert local = value];
       ()];
     ())
  else
    ([%verocaml.proof
       [%verocaml.assert local = value];
       ()];
     ());
  [%verocaml.proof
    [%verocaml.assert local = value];
    ()];
  local
|vero}

let exec_assertion_removed_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let assertion_removed (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  push_front value stack

|vero}

let exec_reveal_only_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let reveal_only (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  push_front value stack

|vero}

let exec_reveal_after_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let reveal_after (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.assert stack_wf stack];
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  push_front value stack

|vero}

let exec_wrong_predicate_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let wrong_predicate (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len stack.top = 1];
    ()];
  push_front value stack

|vero}

let exec_sibling_reveal_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let sibling_reveal (choose : bool) (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    if choose then
      [%verocaml.reveal_with_fuel (spec_node_len, 1)]
    else ();
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

|vero}

let exec_later_block_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let later_block (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  [%verocaml.proof
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

|vero}

let exec_one_branch_export_source =
  {vero|type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let one_branch_export (choose : bool) (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  if choose then
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert stack_wf stack];
       ()];
     ())
  else ();
  push_front value stack
|vero}

let empty_node_len_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let lemma_empty_node_len (node : int node [@finite]) =
  [%verocaml.requires spec_is_empty node];
  [%verocaml.ensures fun _ -> spec_node_len node = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> [%verocaml.assert spec_node_len node = 0]
  | Node _ -> [%verocaml.assert false]
[@@verocaml.proof]
|vero}

let empty_node_wrong_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let lemma_empty_node_len (node : int node [@finite]) =
  [%verocaml.requires spec_is_empty node];
  [%verocaml.ensures fun _ -> spec_node_len node = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> [%verocaml.assert spec_node_len node = 1]
  | Node _ -> [%verocaml.assert false]
[@@verocaml.proof]
|vero}

let empty_node_false_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let lemma_empty_node_len (node : int node [@finite]) =
  [%verocaml.requires spec_is_empty node];
  [%verocaml.ensures fun _ -> spec_node_len node = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> [%verocaml.assert false]
  | Node _ -> [%verocaml.assert false]
[@@verocaml.proof]
|vero}

let non_nullary_ground_abstains_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_node (node : int node) : bool =
  match node with Empty -> false | Node _ -> true
[@@verocaml.spec]

let lemma_symbolic_node_abstains (node : int node [@finite]) =
  [%verocaml.requires spec_is_node node];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> ()
  | Node _ -> [%verocaml.assert spec_node_len node = 0]
[@@verocaml.proof]
|vero}

let nullary_recursive_symbolic_positive_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp idx rest
[@@verocaml.spec] [@@verocaml.revealed]

let lemma_index_empty (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty];
      ()
  | Node _ -> ()
[@@verocaml.proof]
|vero}

let nullary_wrong_result_source =
  {vero|type 'a node = Empty | Other | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : Vstd.Int.t) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Other -> Other
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let wrong_result (idx : Vstd.Int.t) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Other]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]

|vero}

let nullary_reachable_false_source =
  {vero|type 'a node = Empty | Other | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : Vstd.Int.t) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Other -> Other
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let reachable_false (idx : Vstd.Int.t) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      let _symbolic = idx in
      [%verocaml.assert false]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]

|vero}

let nullary_symbolic_expected_source =
  {vero|type 'a node = Empty | Other | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : Vstd.Int.t) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Other -> Other
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let symbolic_expected
    (idx : Vstd.Int.t)
    (nodes : int node [@finite])
    (expected : int node) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = expected]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]
|vero}

let region_proof_call_source =
  {vero|type int_list =
  | Nil
  | Cons of int * int_list

let rec list_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 1 + list_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec twice_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 2 + twice_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec prove_twice_len (values : int_list [@finite]) =
  [%verocaml.ensures fun _result ->
    twice_len values = list_len values + list_len values];
  [%verocaml.decreases values];
  match values with
  | Nil -> ()
  | Cons (_, tail) -> prove_twice_len tail
[@@verocaml.proof]

let region_proof_call (values : int_list [@finite]) =
  [%verocaml.proof
    (match values with
     | Nil -> ()
     | Cons (_, tail) -> prove_twice_len tail);
    prove_twice_len values;
    [%verocaml.assert true];
    ()];
  [%verocaml.proof
    (match values with Nil -> () | Cons _ -> ());
    [%verocaml.assert
      twice_len values = list_len values + list_len values];
    ()];
  ()
|vero}

let proof_branch_fact_postcondition_source =
  {vero|let proof_if_branch_fact (take_left : bool) =
  [%verocaml.ensures fun _result -> take_left = take_left];
  if take_left then [%verocaml.assert take_left = true] else ()
[@@verocaml.proof]

let proof_match_branch_fact (selector : int) =
  [%verocaml.ensures fun _result -> selector = selector];
  match selector with
  | 0 -> [%verocaml.assert selector = 0]
  | _ -> ()
[@@verocaml.proof]

let proof_both_branch_facts (take_left : bool) =
  if take_left then [%verocaml.assert take_left = take_left]
  else [%verocaml.assert take_left = take_left];
  [%verocaml.assert take_left = take_left]
[@@verocaml.proof]

let spec_bool (value : bool) : bool = value
[@@verocaml.spec]

let require_true (value : bool) =
  [%verocaml.requires spec_bool value];
  ()

let exec_region_inner_branch (take_left : bool) =
  [%verocaml.proof
    if take_left then [%verocaml.assert spec_bool take_left] else ();
    ()];
  require_true take_left
|vero}

let one_predecessor_negative_source =
  {vero|type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let one_predecessor (take_left : bool) =
  if take_left then (
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0])
  else ();
  [%verocaml.assert spec_node_len Empty >= 0]
[@@verocaml.proof]
|vero}

let recursive_summary_source =
  {vero|type int_list = Nil | Cons of int * int_list

let rec list_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 1 + list_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec twice_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 2 + twice_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec prove_twice_len (values : int_list [@finite]) =
  [%verocaml.ensures fun _result ->
    twice_len values = list_len values + list_len values];
  [%verocaml.decreases values];
  match values with
  | Nil -> ()
  | Cons (_, tail) ->
      prove_twice_len tail;
      [%verocaml.assert twice_len tail - list_len tail = list_len tail]
[@@verocaml.proof]
|vero}

let recursive_call_removed_source =
  {vero|type int_list = Nil | Cons of int * int_list

let rec list_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 1 + list_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec twice_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 2 + twice_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let prove_twice_len (values : int_list [@finite]) =
  [%verocaml.ensures fun _result ->
    twice_len values = list_len values + list_len values];
  match values with
  | Nil -> ()
  | Cons (_, tail) ->
      [%verocaml.assert twice_len tail - list_len tail = list_len tail]
[@@verocaml.proof]
|vero}

let reveal_removed_negative_source =
  {vero|type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_removed (x : int) =
  let _marker = x in
  [%verocaml.assert spec_node_len Empty = 0]
[@@verocaml.proof]
|vero}

let reveal_after_negative_source =
  {vero|type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_after (x : int) =
  let _marker = x in
  [%verocaml.assert spec_node_len Empty = 0];
  [%verocaml.reveal spec_node_len]
[@@verocaml.proof]
|vero}

let reveal_sibling_negative_source =
  {vero|type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_sibling (take_left : bool) =
  if take_left then [%verocaml.reveal spec_node_len] else ();
  if take_left then () else [%verocaml.assert spec_node_len Empty = 0]
[@@verocaml.proof]
|vero}

let reveal_other_callable_negative_source =
  {vero|type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let activate_elsewhere (_x : int) =
  [%verocaml.reveal spec_node_len]
[@@verocaml.proof]

let no_activation_here (x : int) =
  let _marker = x in
  [%verocaml.assert spec_node_len Empty = 0]
[@@verocaml.proof]
|vero}

let revealed_default_negative_source =
  {vero|type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let no_reached_activation (x : int) =
  let _marker = x in
  [%verocaml.assert spec_node_len Empty = 0]
[@@verocaml.proof]
|vero}

let simple_instance_source =
  {vero|let local_true (x : int) =
  let y = x + 1 in
  [%verocaml.assert y > x]
[@@verocaml.proof]
|vero}

let local_dependency_source =
  {vero|let local_true (x : int) =
  let y = x + 1 in
  [%verocaml.assert y > x]
[@@verocaml.proof]
|vero}

let local_dependency_interface =
  {vero|val local_true : int -> unit
[@@verocaml.proof]
|vero}

let imported_local_consumer_source =
  {vero|let reuse_verified_proof (x : int) =
  Local_dependency.local_true x
[@@verocaml.proof]
|vero}

let raw_carrier_rejected_source =
  {vero|let raw_carrier (x : int) =
  let y = x + 1 in
  Vero_ghost.marker
    "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=7261775f63617272696572|binding=0,0|body=0,0|region=0,0|slots=|kind=local-assert|ordinal=0|predicate=0,0";
  if false then
    ignore
      (fun () ->
        Vero_ghost.sidecar
          "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=7261775f63617272696572|binding=0,0|body=0,0|region=0,0|slots=|kind=local-assert|ordinal=0|predicate=0,0";
        Vero_ghost.proof_region "verocaml:local-assert:1:0:0:0:0:0"
          (fun () -> Vero_ghost.assert_ (fun () -> y > x)))
  else ()
[@@verocaml.proof]
|vero}

let effectful_rejected_source =
  {vero|let effectful (x : int) =
  let y = x + 1 in
  [%verocaml.assert (print_endline "not proof-pure"; y > x)]
[@@verocaml.proof]
|vero}

let non_boolean_rejected_source =
  {vero|let non_boolean (x : int) =
  let y = x + 1 in
  [%verocaml.assert y]
[@@verocaml.proof]
|vero}

let value_exec_region_rejected_source =
  {vero|let value_exec_region (value : int) =
  let _proof_value =
    [%verocaml.proof
      [%verocaml.assert value = value];
      ()]
  in
  value
|vero}

let non_boolean_exec_region_rejected_source =
  {vero|let non_boolean_exec_region (value : int) =
  [%verocaml.proof
    [%verocaml.assert value];
    ()];
  value
|vero}

let effectful_exec_region_rejected_source =
  {vero|let effectful_exec_region (value : int) =
  [%verocaml.proof
    print_endline "not proof-pure";
    ()];
  value
|vero}

let builtin_assert_positive_matrix_source =
  {vero|let require_equal (left : int) (right : int) : unit =
  [%verocaml.requires left = right];
  ()

let proof_mixed (value : int) : unit =
  assert (value = value);
  [%verocaml.assert value <= value];
  assert (value >= value)
[@@verocaml.proof]

let proof_branch_tail (choose : bool) (value : int) : unit =
  if choose then assert (value = value) else assert (value <= value)
[@@verocaml.proof]

let exec_statement (value : int) : int =
  assert (value = value);
  value

let exec_branch_tail (choose : bool) (value : int) : unit =
  if choose then assert (value = value) else assert (value <= value)

let exec_proof_region (value : int) : int =
  [%verocaml.proof
    assert (value = value);
    [%verocaml.assert value <= value];
    ()];
  value

let exec_fact_export (value : int) : unit =
  let alias = value in
  assert (alias = value);
  require_equal alias value
|vero}

let builtin_assert_false_source =
  {vero|let proof_false (_condition : bool) : unit =
  assert false
[@@verocaml.proof]

let exec_false (_condition : bool) : unit =
  assert false
|vero}

let builtin_assert_effect_rejected_source =
  {vero|let effect_rejected (condition : bool) : unit =
  assert (print_endline "effect"; condition)
|vero}

let builtin_assert_mutation_rejected_source =
  {vero|type cell = { mutable value : int }

let mutation_rejected (cell : cell) : unit =
  assert (cell.value <- cell.value + 1; true)
|vero}

let builtin_assert_lookalike_source =
  {vero|let explicit_assert_failure (condition : bool) : unit =
  if condition then ()
  else raise (Assert_failure ("builtin_assert_lookalike.ml", 3, 7))
|vero}

let builtin_assert_noassert_source =
  {vero|let runtime_guard (condition : bool) : unit =
  [%verocaml.requires condition];
  assert condition

let retained_check (condition : bool) : unit =
  assert (condition = condition)
|vero}

let immutable_pattern_matrix_source =
  {vero|type 'a node = Empty | Node of 'a * 'a node
type tree = Tip of int | Branch of tree * tree
type row = { first : int; second : int; tail : int node }
let allocation_without_ensures (value : int) (rest : int node) =
  let built = Node (value, rest) in
  [%verocaml.assert built = Node (value, rest)]
[@@verocaml.proof]
let alias_positive (nodes : int node) =
  let alias = nodes in
  match alias with
  | Empty -> ()
  | Node (value, rest) -> [%verocaml.assert Node (value, rest) = nodes]
[@@verocaml.proof]
let guard_positive (nodes : int node) =
  match nodes with
  | Node (value, rest) when value = value ->
      [%verocaml.assert Node (value, rest) = nodes]
  | Empty -> ()
  | Node _ -> ()
[@@verocaml.proof]
let nested_positive (tree : tree) =
  match tree with
  | Branch (Tip value, rest) ->
      [%verocaml.assert Branch (Tip value, rest) = tree]
  | Tip _ -> ()
  | Branch _ -> ()
[@@verocaml.proof]
let generic_alias_positive (nodes : int node) =
  let local_alias = nodes in
  match local_alias with
  | Empty -> ()
  | Node (value, rest) -> [%verocaml.assert Node (value, rest) = nodes]
[@@verocaml.proof]
let complete_record_positive (row : row) =
  match row with
  | { first; second; tail } ->
      [%verocaml.assert { first; second; tail } = row]
[@@verocaml.proof]
let subset_record_positive (row : row) =
  match row with
  | { first } ->
      [%verocaml.assert
        { first; second = row.second; tail = row.tail } = row]
  | _ -> ()
[@@verocaml.proof]
let wrong_constructor (nodes : int node) =
  match nodes with
  | Empty -> [%verocaml.assert Node (0, Empty) = nodes]
  | Node _ -> ()
[@@verocaml.proof]
let wrong_payload (nodes : int node) =
  match nodes with
  | Empty -> ()
  | Node (value, rest) ->
      [%verocaml.assert Node (value + 1, rest) = nodes]
[@@verocaml.proof]
let wrong_nested_payload (tree : tree) =
  match tree with
  | Branch (Tip value, rest) ->
      [%verocaml.assert Branch (Tip (value + 1), rest) = tree]
  | Tip _ -> ()
  | Branch _ -> ()
[@@verocaml.proof]
let wrong_field (row : row) =
  match row with
  | { first; second; tail } ->
      [%verocaml.assert { first = first + 1; second; tail } = row]
[@@verocaml.proof]
|vero}

let immutable_reconstruction_cohabitation_source =
  {vero|type snapshot = End | More of int * snapshot
type local_node = LEmpty | LNode of int * local_node
module type STACK = sig
  type t
  val model : t @ read -> snapshot @ immutable
  val singleton : int -> t @ unique
  val keep : t @ unique -> t @ unique
  val length : t @ read -> int
end
module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }
  let rec contents_node (node : node) : snapshot =
    [%verocaml.decreases node];
    match node with
    | Empty -> End
    | Node { value; next } -> More (value, contents_node next)
  [@@verocaml.spec] [@@verocaml.opaque]
  let model (stack : t @ read) : snapshot @ immutable =
    contents_node stack.top
  [@@verocaml.spec]
  let singleton value : t @ unique =
    [%verocaml.ensures fun result -> model result = More (value, End)];
    { top = Node { value; next = Empty }; length = 1 }
  let keep (stack : t @ unique) : t @ unique = stack
  let length (stack : t @ read) = stack.length
end
let unrelated_immutable_reconstruction (nodes : local_node) =
  match nodes with
  | LEmpty -> ()
  | LNode (value, rest) ->
      [%verocaml.assert LNode (value, rest) = nodes]
[@@verocaml.proof]
let unrelated_construction_exactly_once value rest =
  let built = LNode (value, rest) in
  [%verocaml.assert built = built]
[@@verocaml.proof]
let wrong_value (nodes : local_node) (other : local_node) =
  match nodes with
  | LEmpty -> ()
  | LNode (value, rest) ->
      [%verocaml.assert LNode (value, rest) = other]
[@@verocaml.proof]
let wrong_payload (nodes : local_node) =
  match nodes with
  | LEmpty -> ()
  | LNode (value, rest) ->
      [%verocaml.assert LNode (value + 1, rest) = nodes]
[@@verocaml.proof]
|vero}

let reconstruction_parity =
  base "Immutable_pattern_reconstruction" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "branch_reconstruction"; "direct_branch_assert";
         "assumed_branch_assert" ]
  |> require_kinds "branch_reconstruction" [ Outcome.Postcondition ]
  |> require_kinds "direct_branch_assert" [ Outcome.Local_assertion ]
  |> require_kinds "assumed_branch_assert" [ Outcome.Local_assertion ]
  |> prepared_parity_case ~name:"immutable-reconstruction-source-cmt-parity"
       ~module_name:"Immutable_pattern_reconstruction"
       ~source:immutable_pattern_reconstruction_source

let reconstruction_matrix =
  base "Immutable_pattern_matrix" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions
       [ "allocation_without_ensures"; "alias_positive"; "guard_positive";
         "nested_positive"; "generic_alias_positive";
         "complete_record_positive"; "subset_record_positive";
         "wrong_constructor"; "wrong_payload"; "wrong_nested_payload";
         "wrong_field" ]
  |> require_kinds "allocation_without_ensures" [ Outcome.Local_assertion ]
  |> require_kinds "subset_record_positive" [ Outcome.Local_assertion ]
  |> Expectation.require_semantic ~function_name:"wrong_constructor"
       Outcome.Local_assertion
  |> Expectation.require_semantic ~function_name:"wrong_payload"
       Outcome.Local_assertion
  |> Expectation.require_semantic ~function_name:"wrong_nested_payload"
       Outcome.Local_assertion
  |> Expectation.require_semantic ~function_name:"wrong_field"
       Outcome.Local_assertion
  |> source_case ~name:"immutable-reconstruction-matrix"
       ~module_name:"Immutable_pattern_matrix"
       ~source:immutable_pattern_matrix_source

let reconstruction_cohabitation =
  base "Immutable_reconstruction_cohabitation" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions
       [ "unrelated_immutable_reconstruction";
         "unrelated_construction_exactly_once"; "wrong_value"; "wrong_payload" ]
  |> require_kinds "unrelated_immutable_reconstruction"
       [ Outcome.Local_assertion ]
  |> Expectation.require_semantic ~function_name:"wrong_value"
       Outcome.Local_assertion
  |> Expectation.require_semantic ~function_name:"wrong_payload"
       Outcome.Local_assertion
  |> source_case ~name:"immutable-reconstruction-cohabitation"
       ~module_name:"Immutable_reconstruction_cohabitation"
       ~source:immutable_reconstruction_cohabitation_source

let positive_matrix =
  base "Positive_matrix" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "pure_let"; "branch_reveal"; "both_predecessors"; "tracked_local" ]
  |> require_kinds "pure_let" [ Outcome.Local_assertion ]
  |> require_kinds "branch_reveal" [ Outcome.Local_assertion ]
  |> require_kinds "both_predecessors" [ Outcome.Local_assertion ]
  |> require_kinds "tracked_local" [ Outcome.Local_assertion ]
  |> source_case ~name:"proof-local-assertion-matrix"
       ~module_name:"Positive_matrix" ~source:positive_matrix_source

let create_stack =
  base "Create_stack_with" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "create_stack_with"; "scalar_after_let"; "two_assertions";
         "branch_intersection" ]
  |> require_kinds "create_stack_with"
       [ Outcome.Local_assertion;
         Outcome.Call_precondition { callee = "push_front" } ]
  |> source_case ~name:"exec-proof-region-fact-export"
       ~module_name:"Create_stack_with" ~source:create_stack_with_source

let failing_exec_region name module_name source function_name kind =
  base module_name Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ function_name ]
  |> Expectation.require_semantic ~function_name kind
  |> source_case ~name ~module_name ~source

let exec_assertion_removed =
  failing_exec_region "exec-region-assertion-removed" "Exec_assertion_removed"
    exec_assertion_removed_source "assertion_removed"
    (Outcome.Call_precondition { callee = "push_front" })

let exec_reveal_only =
  failing_exec_region "exec-region-reveal-only" "Exec_reveal_only"
    exec_reveal_only_source "reveal_only"
    (Outcome.Call_precondition { callee = "push_front" })

let exec_reveal_after =
  failing_exec_region "exec-region-reveal-after" "Exec_reveal_after"
    exec_reveal_after_source "reveal_after" Outcome.Local_assertion

let exec_wrong_predicate =
  base "Exec_wrong_predicate" Outcome.Inconclusive Outcome.Unit_inconclusive
  |> require_functions [ "wrong_predicate" ]
  |> Expectation.require_semantic ~function_name:"wrong_predicate"
       Outcome.Local_assertion
  |> source_case ~name:"exec-region-wrong-predicate"
       ~module_name:"Exec_wrong_predicate" ~source:exec_wrong_predicate_source

let exec_sibling_reveal =
  failing_exec_region "exec-region-sibling-reveal" "Exec_sibling_reveal"
    exec_sibling_reveal_source "sibling_reveal" Outcome.Local_assertion

let exec_later_block =
  failing_exec_region "exec-region-later-block" "Exec_later_block"
    exec_later_block_source "later_block" Outcome.Local_assertion

let exec_one_branch_export =
  failing_exec_region "exec-region-one-branch-export" "Exec_one_branch_export"
    exec_one_branch_export_source "one_branch_export"
    (Outcome.Call_precondition { callee = "push_front" })

let empty_node_len =
  base "Empty_node_len" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "lemma_empty_node_len" ]
  |> require_kinds "lemma_empty_node_len" [ Outcome.Local_assertion ]
  |> source_case ~name:"finite-empty-node-assertion" ~module_name:"Empty_node_len"
       ~source:empty_node_len_source

let empty_node_wrong =
  base "Empty_node_wrong" Outcome.Inconclusive Outcome.Unit_inconclusive
  |> require_functions [ "lemma_empty_node_len" ]
  |> Expectation.require_semantic ~function_name:"lemma_empty_node_len"
       Outcome.Local_assertion
  |> source_case ~name:"finite-empty-node-wrong" ~module_name:"Empty_node_wrong"
       ~source:empty_node_wrong_source

let empty_node_false =
  base "Empty_node_false" Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ "lemma_empty_node_len" ]
  |> Expectation.require_semantic ~function_name:"lemma_empty_node_len"
       Outcome.Local_assertion
  |> source_case ~name:"finite-empty-node-false" ~module_name:"Empty_node_false"
       ~source:empty_node_false_source

let non_nullary_ground =
  base "Non_nullary_ground_abstains" Outcome.Inconclusive
    Outcome.Unit_inconclusive
  |> require_functions [ "lemma_symbolic_node_abstains" ]
  |> Expectation.require_semantic ~function_name:"lemma_symbolic_node_abstains"
       Outcome.Local_assertion
  |> source_case ~name:"non-nullary-ground-abstains"
       ~module_name:"Non_nullary_ground_abstains"
       ~source:non_nullary_ground_abstains_source

let nullary_positive =
  base "Nullary_recursive_symbolic_positive" Outcome.Verified
    Outcome.Unit_verified
  |> require_functions [ "lemma_index_empty" ]
  |> require_kinds "lemma_index_empty" [ Outcome.Local_assertion ]
  |> source_case ~name:"nullary-recursive-symbolic-positive"
       ~module_name:"Nullary_recursive_symbolic_positive"
       ~source:nullary_recursive_symbolic_positive_source

let nullary_wrong_result =
  base "Nullary_wrong_result" Outcome.Inconclusive Outcome.Unit_inconclusive
  |> require_functions [ "wrong_result" ]
  |> Expectation.require_semantic ~function_name:"wrong_result"
       Outcome.Local_assertion
  |> source_case ~name:"nullary-recursive-wrong-result"
       ~module_name:"Nullary_wrong_result" ~source:nullary_wrong_result_source

let nullary_reachable_false =
  base "Nullary_reachable_false" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions [ "reachable_false" ]
  |> Expectation.require_semantic ~function_name:"reachable_false"
       Outcome.Local_assertion
  |> source_case ~name:"nullary-recursive-reachable-false"
       ~module_name:"Nullary_reachable_false"
       ~source:nullary_reachable_false_source

let nullary_symbolic_expected =
  base "Nullary_symbolic_expected" Outcome.Inconclusive Outcome.Unit_inconclusive
  |> require_functions [ "symbolic_expected" ]
  |> Expectation.require_semantic ~function_name:"symbolic_expected"
       Outcome.Local_assertion
  |> source_case ~name:"nullary-recursive-symbolic-expected"
       ~module_name:"Nullary_symbolic_expected"
       ~source:nullary_symbolic_expected_source

let region_proof_call =
  base "Region_proof_call" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "prove_twice_len"; "region_proof_call" ]
  |> require_kinds "region_proof_call" [ Outcome.Local_assertion ]
  |> source_case ~name:"exec-region-recursive-proof-call"
       ~module_name:"Region_proof_call" ~source:region_proof_call_source

let branch_facts =
  base "Proof_branch_fact_postcondition" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions
       [ "proof_if_branch_fact"; "proof_match_branch_fact";
         "proof_both_branch_facts"; "exec_region_inner_branch" ]
  |> require_kinds "proof_if_branch_fact"
       [ Outcome.Local_assertion; Outcome.Postcondition ]
  |> Expectation.require_semantic ~function_name:"exec_region_inner_branch"
       (Outcome.Call_precondition { callee = "require_true" })
  |> source_case ~name:"branch-local-fact-scoping"
       ~module_name:"Proof_branch_fact_postcondition"
       ~source:proof_branch_fact_postcondition_source

let one_predecessor =
  base "One_predecessor_negative" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions [ "one_predecessor" ]
  |> Expectation.require_semantic ~function_name:"one_predecessor"
       Outcome.Local_assertion
  |> source_case ~name:"one-predecessor-does-not-prove-join"
       ~module_name:"One_predecessor_negative"
       ~source:one_predecessor_negative_source

let recursive_summary =
  base "Recursive_summary" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "prove_twice_len" ]
  |> require_kinds "prove_twice_len"
       [ Outcome.Entry_measure_nonnegative;
         Outcome.Recursive_call_measure_nonnegative { callee = "prove_twice_len" };
         Outcome.Recursive_call_strict_descent { callee = "prove_twice_len" };
         Outcome.Local_assertion ]
  |> source_case ~name:"recursive-proof-summary" ~module_name:"Recursive_summary"
       ~source:recursive_summary_source

let recursive_call_removed =
  base "Recursive_call_removed" Outcome.Inconclusive Outcome.Unit_inconclusive
  |> require_functions [ "prove_twice_len" ]
  |> Expectation.require_semantic ~function_name:"prove_twice_len"
       Outcome.Local_assertion
  |> source_case ~name:"removed-recursive-summary"
       ~module_name:"Recursive_call_removed"
       ~source:recursive_call_removed_source

let failing_local name module_name source function_name =
  base module_name Outcome.Counterexample Outcome.Unit_counterexample
  |> require_functions [ function_name ]
  |> Expectation.require_semantic ~function_name Outcome.Local_assertion
  |> source_case ~name ~module_name ~source

let reveal_removed =
  failing_local "reveal-removed" "Reveal_removed_negative"
    reveal_removed_negative_source "reveal_removed"
let reveal_after =
  failing_local "reveal-after" "Reveal_after_negative"
    reveal_after_negative_source "reveal_after"
let reveal_sibling =
  failing_local "reveal-sibling" "Reveal_sibling_negative"
    reveal_sibling_negative_source "reveal_sibling"
let reveal_other_callable =
  failing_local "reveal-other-callable" "Reveal_other_callable_negative"
    reveal_other_callable_negative_source "no_activation_here"
let revealed_default =
  base "Revealed_default_negative" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "no_reached_activation" ]
  |> require_kinds "no_reached_activation" [ Outcome.Local_assertion ]
  |> source_case ~name:"revealed-default"
       ~module_name:"Revealed_default_negative"
       ~source:revealed_default_negative_source

let simple_instance =
  base "Simple_instance" Outcome.Verified Outcome.Unit_verified
  |> require_functions [ "local_true" ]
  |> require_kinds "local_true" [ Outcome.Local_assertion ]
  |> source_case ~name:"simple-local-assertion" ~module_name:"Simple_instance"
       ~source:simple_instance_source

let imported_project =
  Fixture.dune_project
    { files =
        [ { Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name imported_local_assertion)\n" };
          { path = "dune";
            contents =
              "(library\n (name imported_local_assertion)\n (wrapped false)\n \
               (modules Local_dependency Imported_local_consumer)\n (libraries \
               verocaml.ghost)\n (flags (:standard -ppx \
               \"verocaml-ppx --keep-ghost\")))\n" };
          { path = "local_dependency.ml"; contents = local_dependency_source };
          { path = "local_dependency.mli"; contents = local_dependency_interface };
          { path = "imported_local_consumer.ml";
            contents = imported_local_consumer_source } ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Local_dependency"; "Imported_local_consumer" ] }

let imported_local_assertion =
  let expectation =
    base "Local_dependency" Outcome.Verified Outcome.Unit_verified
    |> Expectation.require_unit "Imported_local_consumer" Outcome.Unit_verified
    |> require_functions [ "local_true"; "reuse_verified_proof" ]
  in
  Suite.case ~name:"imported-local-assertion-is-not-authority" ~expectation
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace imported_project)

let frontend_case name module_name source code =
  base module_name Outcome.Frontend_rejected Outcome.Unit_frontend_rejected
  |> Expectation.require_frontend_code code
  |> source_case ~name ~module_name ~source

let raw_carrier =
  frontend_case "raw-local-carrier" "Raw_carrier_rejected"
    raw_carrier_rejected_source "VERO_MALFORMED_GHOST_CALL"
let effectful =
  frontend_case "effectful-local-assertion" "Effectful_rejected"
    effectful_rejected_source "VERO_UNSUPPORTED_EXTERNAL_CALL"
let non_boolean =
  frontend_case "non-boolean-local-assertion" "Non_boolean_rejected"
    non_boolean_rejected_source "VERO_MALFORMED_GHOST_CALL"
let value_exec_region =
  frontend_case "value-position-exec-region" "Value_exec_region_rejected"
    value_exec_region_rejected_source "VERO_MALFORMED_GHOST_CALL"
let non_boolean_exec_region =
  frontend_case "non-boolean-exec-region" "Non_boolean_exec_region_rejected"
    non_boolean_exec_region_rejected_source "VERO_MALFORMED_GHOST_CALL"
let effectful_exec_region =
  frontend_case "effectful-exec-region" "Effectful_exec_region_rejected"
    effectful_exec_region_rejected_source "VERO_UNSUPPORTED_EXTERNAL_CALL"

let builtin_parity =
  base "Builtin_assert_positive_matrix" Outcome.Verified Outcome.Unit_verified
  |> require_functions
       [ "proof_mixed"; "proof_branch_tail"; "exec_statement";
         "exec_branch_tail"; "exec_proof_region"; "exec_fact_export" ]
  |> require_kinds "proof_mixed" [ Outcome.Local_assertion ]
  |> require_kinds "exec_statement" [ Outcome.Local_assertion ]
  |> require_kinds "exec_fact_export"
       [ Outcome.Local_assertion;
         Outcome.Call_precondition { callee = "require_equal" } ]
  |> prepared_parity_case ~name:"builtin-assert-source-cmt-parity"
       ~module_name:"Builtin_assert_positive_matrix"
       ~source:builtin_assert_positive_matrix_source

let builtin_false =
  base "Builtin_assert_false" Outcome.Counterexample
    Outcome.Unit_counterexample
  |> require_functions [ "proof_false"; "exec_false" ]
  |> Expectation.require_semantic ~function_name:"proof_false"
       Outcome.Local_assertion
  |> Expectation.require_semantic ~function_name:"exec_false"
       Outcome.Local_assertion
  |> source_case ~name:"builtin-assert-false" ~module_name:"Builtin_assert_false"
       ~source:builtin_assert_false_source

let builtin_effect =
  frontend_case "builtin-assert-effect" "Builtin_assert_effect_rejected"
    builtin_assert_effect_rejected_source "VERO_UNSUPPORTED_EXTERNAL_CALL"
let builtin_mutation =
  frontend_case "builtin-assert-mutation" "Builtin_assert_mutation_rejected"
    builtin_assert_mutation_rejected_source "VERO_UNSUPPORTED_MUTATION"
let builtin_lookalike =
  frontend_case "builtin-assert-lookalike" "Builtin_assert_lookalike"
    builtin_assert_lookalike_source "VERO_UNSUPPORTED_EXTERNAL_CALL"

let noassert_project =
  Fixture.dune_project
    { files =
        [ { Fixture.path = "dune-project";
            contents = "(lang dune 3.17)\n(name noassert_parity)\n" };
          { path = "dune";
            contents =
              "(library\n (name noassert_parity)\n (wrapped false)\n \
               (modules Builtin_assert_noassert)\n (libraries verocaml.ghost)\n \
               (flags (:standard -noassert -ppx \
               \"verocaml-ppx --keep-ghost\")))\n" };
          { path = "builtin_assert_noassert.ml";
            contents = builtin_assert_noassert_source } ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Builtin_assert_noassert" ] }

let noassert_parity ~environment ~workspace =
  let* ordinary =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "ordinary")
      (source_input "Builtin_assert_noassert" builtin_assert_noassert_source)
  in
  let* elided =
    Fixture.run ~environment ~workspace:(Filename.concat workspace "elided")
      noassert_project
  in
  let ordinary = tagged "compile-mode" "ordinary" ordinary in
  let elided = tagged "compile-mode" "noassert" elided in
  let* () = parity_or_failure ~except:[ "compile-mode" ] ordinary elided in
  Ok (Outcome.merge [ ordinary; elided ])

let noassert_parity_case =
  let expectation =
    base "Builtin_assert_noassert" Outcome.Verified Outcome.Unit_verified
    |> require_functions [ "runtime_guard"; "retained_check" ]
    |> require_kinds "runtime_guard" [ Outcome.Local_assertion ]
    |> require_kinds "retained_check" [ Outcome.Local_assertion ]
    |> Expectation.require_named_fact "compile-mode"
         (Outcome.Function_exists "ordinary")
    |> Expectation.require_named_fact "compile-mode"
         (Outcome.Function_exists "noassert")
  in
  Suite.case ~name:"builtin-assert-noassert-semantic-parity" ~expectation
    noassert_parity

let () =
  Suite.run_cli ~suite_path:"test/proof_body_assertions/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ reconstruction_parity; reconstruction_matrix; reconstruction_cohabitation;
      positive_matrix; create_stack; exec_assertion_removed; exec_reveal_only;
      exec_reveal_after; exec_wrong_predicate; exec_sibling_reveal;
      exec_later_block; exec_one_branch_export; empty_node_len; empty_node_wrong;
      empty_node_false; non_nullary_ground;
      nullary_positive; nullary_wrong_result; nullary_reachable_false;
      nullary_symbolic_expected; region_proof_call; branch_facts;
      one_predecessor; recursive_summary; recursive_call_removed;
      reveal_removed; reveal_after; reveal_sibling; reveal_other_callable;
      revealed_default; simple_instance; imported_local_assertion; raw_carrier;
      effectful; non_boolean; value_exec_region; non_boolean_exec_region;
      effectful_exec_region; builtin_parity; builtin_false; builtin_effect;
      builtin_mutation; builtin_lookalike; noassert_parity_case ]
