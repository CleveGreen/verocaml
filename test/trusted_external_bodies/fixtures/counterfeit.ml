let trusted x =
  Vero_ghost.external_body "verocaml:external-body:1:0:0:0:0:trusted"
    (fun () -> x)
