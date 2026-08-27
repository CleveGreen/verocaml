let admit () =
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
