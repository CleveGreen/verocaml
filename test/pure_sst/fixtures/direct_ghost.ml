let direct_ghost (x : int) = Vero_ghost.requires (fun () -> x >= 0); x
