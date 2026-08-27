type 'a choice = None | Some of 'a

let same left right = left = right [@@verocaml.spec]

let false_identity (value : 'a choice) : unit =
  [%verocaml.assert same value None];
  ()
[@@verocaml.proof]
