let rec bad value : unit =
  [%verocaml.decreases 0];
  bad value
[@@verocaml.proof]
