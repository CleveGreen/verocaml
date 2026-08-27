type 'a choice = None | Some of 'a

let same left right = left = right [@@verocaml.spec]

let reflexive (value : 'a choice) : unit =
  [%verocaml.assert same value value];
  ()
[@@verocaml.proof]
