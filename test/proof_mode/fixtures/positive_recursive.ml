let positive (x : int) : unit =
  [%verocaml.requires x > 0];
  ()
[@@verocaml.proof]

let rec induct (n : int) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> n >= 0];
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    induct (n - 1);
    positive n)
[@@verocaml.proof]

let use_induct (n : int) : unit =
  [%verocaml.requires n >= 0];
  induct n
[@@verocaml.proof]

let run_recursive (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.proof use_induct n];
  n
