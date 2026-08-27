let rec imported (xs : Structural_dependency.chain) : int =
  [%verocaml.decreases xs];
  match xs with
  | Structural_dependency.Empty -> 0
  | Structural_dependency.Link (_, tail) -> imported tail
[@@verocaml.spec] [@@verocaml.revealed]
