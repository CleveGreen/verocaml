let same left right = left = right [@@verocaml.spec]

let false_identity (left : 'a) (right : 'a) : unit =
  [%verocaml.assert same left right];
  ()
[@@verocaml.proof]
