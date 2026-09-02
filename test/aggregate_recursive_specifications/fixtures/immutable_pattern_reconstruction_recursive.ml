type 'a node = Empty | Node of 'a * 'a node

let rec rebuild (nodes : Vstd.Int.t node) : Vstd.Int.t node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if value = value then Node (value, rest) else rebuild rest
[@@verocaml.spec] [@@verocaml.opaque]

let recursive_reconstruction (nodes : Vstd.Int.t node [@finite]) =
  [%verocaml.reveal_with_fuel (rebuild, 2)];
  match nodes with
  | Empty -> [%verocaml.assert rebuild nodes = nodes]
  | Node (value, rest) ->
      [%verocaml.assert rebuild nodes = Node (value, rest)]
[@@verocaml.proof]

let recursive_wrong_payload (nodes : Vstd.Int.t node [@finite]) =
  [%verocaml.reveal_with_fuel (rebuild, 2)];
  match nodes with
  | Empty -> ()
  | Node (value, rest) ->
      [%verocaml.assert rebuild nodes = Node (value + 1, rest)]
[@@verocaml.proof]
