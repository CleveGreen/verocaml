type 'a node = Empty | Node of 'a * 'a node

type 'a option = None | Some of 'a

let rec spec_index_alt (idx : int) (nodes : int node) : int option =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> None
  | Node (value, rest) ->
      if idx <= 0 then Some value
      else spec_index_alt (idx - 1) rest
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

let observe_recursive_index_alt
    (expected : int)
    (nodes : int node [@finite]) : unit =
  [%verocaml.requires spec_has_second expected nodes];
  [%verocaml.ensures fun _result ->
    match spec_index_alt 1 nodes with
    | None -> false
    | Some value -> value = expected];
  ()
