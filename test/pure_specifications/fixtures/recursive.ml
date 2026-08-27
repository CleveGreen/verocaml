let rec bad (x : int) = if x = 0 then 0 else bad (x - 1) [@@verocaml.spec]
