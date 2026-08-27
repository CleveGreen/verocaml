let target (x : int) = x
let wrapper (x : int) =
  Vero_ghost.external_specification
    "verocaml:external-specification:1:0:1:2:3:wrapper"
    (fun () -> target x)
