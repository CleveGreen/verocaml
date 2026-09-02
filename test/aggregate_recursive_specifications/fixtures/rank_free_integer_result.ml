type 'a node = Empty | Node of 'a * 'a node

type 'a option = None | Some of 'a

let spec_node_value (nodes : int node) : int option =
  match nodes with Empty -> None | Node (value, _) -> Some value
[@@verocaml.spec]

let rec spec_index (idx : Vstd.Int.t) (nodes : int node) : int option =
  [%verocaml.decreases idx];
  if idx <= 0 then spec_node_value nodes
  else
    match nodes with
    | Empty -> None
    | Node (_, rest) -> spec_index (idx - 1) rest
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_has_second (expected : int) (nodes : int node) : bool =
  match nodes with
  | Empty -> false
  | Node (_, rest) ->
      (match rest with
      | Empty -> false
      | Node (value, _) -> value = expected)
[@@verocaml.spec]

let observe_recursive_index
    (expected : int)
    (nodes : int node [@finite]) : unit =
  [%verocaml.requires spec_has_second expected nodes];
  [%verocaml.ensures fun _result ->
    match spec_index 1 nodes with
    | None -> false
    | Some value -> value = expected];
  ()
