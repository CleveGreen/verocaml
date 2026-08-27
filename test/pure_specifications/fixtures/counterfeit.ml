let fake (x : int) : int =
  Vero_ghost.spec_definition "verocaml:spec:1:0:0:0:0:fake" (fun () -> x + 1)
