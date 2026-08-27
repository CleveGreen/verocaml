let concurrent () = Domain.join (Domain.spawn (fun () -> 1))
