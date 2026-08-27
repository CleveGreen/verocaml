type node = Empty | Node of int * node
type pair = Pair of node * node

let rec spec_len (xs : node) : int =
  [%verocaml.decreases xs];
  match xs with Empty -> 0 | Node (_, tail) -> 1 + spec_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let consume_node (xs : node [@finite]) : unit =
  [%verocaml.ensures fun _ -> spec_len xs = spec_len xs]; ()
[@@verocaml.proof]

let consume_pair (_xs : pair [@finite]) : unit = ()

let rec push_n_nodes value count (nodes : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then nodes
  else push_n_nodes value (count - 1) (Node (value, nodes))

let rec make_n_nodes value count : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then Empty
  else Node (value, make_n_nodes value (count - 1))

let rec two_smaller n (xs : node [@finite]) : node =
  [%verocaml.requires n >= 0]; [%verocaml.decreases n];
  if n = 0 then xs
  else
    let left = two_smaller (n - 1) xs in
    let right = two_smaller (n - 1) xs in
    if n > 1 then Node (n, left) else right

let duplicate (xs : node [@finite]) : pair = Pair (xs, xs)

let all_branches choose (xs : node [@finite]) : node =
  let alias = xs in if choose then alias else Node (0, alias)

let external_use n choose (xs : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  let pushed = push_n_nodes n n xs in
  let copied = pushed in
  let rebuilt = make_n_nodes n n in
  let twice = two_smaller n xs in
  let joined = all_branches choose xs in
  let paired = duplicate xs in
  [%verocaml.proof consume_node copied];
  [%verocaml.proof consume_node pushed];
  [%verocaml.proof consume_node rebuilt];
  [%verocaml.proof consume_node twice];
  [%verocaml.proof consume_node joined];
  consume_pair paired
