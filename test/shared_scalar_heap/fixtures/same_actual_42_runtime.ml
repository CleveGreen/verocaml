type box = { mutable value : int }

let update (x : box @ aliased) (y : box @ aliased) =
  [%verocaml.requires true];
  [%verocaml.ensures fun _ -> true];
  x.value <- x.value + y.value;
  y.value <- y.value + 2

let () =
  let cell = { value = 42 } in
  update cell cell;
  Printf.printf "%d\n" cell.value
