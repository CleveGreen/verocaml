type seed = Zero | Succ of seed
type 'a chain = Empty | Link of 'a * 'a chain

let rec local_rank_actual (xs : seed chain) : int =
  [%verocaml.decreases xs];
  match xs with Empty -> 0 | Link (_, tail) -> 1 + local_rank_actual tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec nested_rank_actual (xs : int chain chain) : int =
  [%verocaml.decreases xs];
  match xs with Empty -> 0 | Link (_, tail) -> 1 + nested_rank_actual tail
[@@verocaml.spec] [@@verocaml.revealed]
