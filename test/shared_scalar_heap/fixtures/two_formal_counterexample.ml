type box = { mutable value : int }

let write_x_claim_y_unchanged
    (x : box @ aliased) (y : box @ aliased) : unit =
  [%verocaml.ensures fun _ ->
    y.value = [%verocaml.old y.value]];
  x.value <- 1
