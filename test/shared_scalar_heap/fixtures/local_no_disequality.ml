type box = { mutable value : int }

let local_no_disequality
    (x : box @ aliased) (y : box @ aliased) @ local =
  [%verocaml.ensures fun _ ->
    y.value = [%verocaml.old y.value]];
  x.value <- 1
