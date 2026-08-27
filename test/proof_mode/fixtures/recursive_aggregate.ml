let rec bad (pair : int * int) : unit =
  [%verocaml.decreases 0];
  bad pair
[@@verocaml.proof]
