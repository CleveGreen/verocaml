type cell = { mutable value : int }

let increment (cell : cell @ aliased) =
  let before = cell.value in
  cell.value <- -1;
  cell.value <- before + 1

let () =
  let cell = { value = 41 } in
  increment cell;
  Printf.printf "%d\n" cell.value
