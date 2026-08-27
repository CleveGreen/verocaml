let rec fake (n : int) : unit =
  Vero_ghost.proof_definition "verocaml:proof:1:0:0:0:0:fake" (fun () ->
      if n = 0 then () else fake (n - 1))
