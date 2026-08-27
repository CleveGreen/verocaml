type 'a choice = None | Some of 'a

let different value = Some value <> None [@@verocaml.spec]

let prove value : unit =
  [%verocaml.assert different value];
  ()
[@@verocaml.proof]
