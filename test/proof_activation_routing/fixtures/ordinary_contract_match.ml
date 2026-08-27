type 'a node = Empty | Node of 'a * 'a node

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
