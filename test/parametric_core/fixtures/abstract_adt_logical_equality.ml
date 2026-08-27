type 'a choice = Neither | This of 'a | That of 'a

let same left right = left = right [@@verocaml.spec]

let reflexive (value : 'a choice) : unit =
  [%verocaml.assert same value value];
  ()
[@@verocaml.proof]

let rebuild (value : 'a choice) =
  match value with
  | Neither -> Neither
  | This payload -> This payload
  | That payload -> That payload
[@@verocaml.spec]

let reconstruct (value : 'a choice) : unit =
  [%verocaml.assert same value (rebuild value)];
  ()
[@@verocaml.proof]
