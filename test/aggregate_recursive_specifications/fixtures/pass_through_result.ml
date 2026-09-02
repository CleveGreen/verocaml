type node = Empty | Node of int * node

let rec spec_pass_through (n : Vstd.Int.t) (node : node) : node =
  [%verocaml.decreases n];
  if n <= 0 then node
  else spec_pass_through (n - 1) node
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_size (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_size next
[@@verocaml.spec]
[@@verocaml.revealed]

let consume_pass_through (n : int) (node : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    let expected = spec_pass_through n node in
    spec_size expected = spec_size expected];
  ()
