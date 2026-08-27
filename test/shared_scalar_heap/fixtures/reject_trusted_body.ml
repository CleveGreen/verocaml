type box = { mutable value : int }

let reject_trusted_body (cell : box @ aliased) : unit =
  [%verocaml.ensures fun _ -> cell.value = 1];
  cell.value <- 1
[@@verocaml.external_body]
