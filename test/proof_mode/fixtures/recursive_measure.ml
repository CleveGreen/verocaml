let rec recursive_measure (n : int) : unit =
  [%verocaml.decreases (recursive_measure n; n)];
  recursive_measure n
[@@verocaml.proof]
