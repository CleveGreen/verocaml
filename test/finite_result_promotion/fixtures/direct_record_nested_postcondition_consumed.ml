type node = Empty | Node of int * node
type holder = { top : node }

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let rec build count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.ensures fun result ->
    spec_node_len result = spec_node_len xs + count + 1];
  [%verocaml.decreases count];
  if count = 0 then Node (0, xs)
  else
    let nested = { top = build (count - 1) xs } in
    Node (count, nested.top)

let consume (_node : node [@finite]) : unit = ()

let use count (xs : node [@finite]) : unit =
  [%verocaml.requires count >= 0];
  consume (build count xs)
