let inc (x:int) = x + 1 [@@verocaml.spec]
let lemma (x:int) : unit =
  [%verocaml.requires x >= 0];
  [%verocaml.assert inc x > x];
  [%verocaml.ensures fun result -> inc x > x];
  ()
[@@verocaml.proof]
let lemma2 (x:int) : unit =
  [%verocaml.requires x >= 0];
  [%verocaml.assert inc x > x];
  lemma x;
  ()
[@@verocaml.proof]
let run (x:int) =
  [%verocaml.requires x >= 0];
  [%verocaml.proof lemma2 x];
  x
