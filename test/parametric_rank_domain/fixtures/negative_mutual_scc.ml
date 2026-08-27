type 'a left = L0 | L1 of 'a * 'a right
and 'a right = R0 | R1 of 'a * 'a left
let rec depth (xs : int left) =
  [%verocaml.decreases xs];
  match xs with L0 -> 0 | L1 (_, _) -> 1
[@@verocaml.spec] [@@verocaml.revealed]
