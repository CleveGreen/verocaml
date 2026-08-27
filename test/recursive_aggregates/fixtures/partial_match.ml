type choice = Left of int | Right of int
let left (choice : choice) =
  match choice with Left value -> value
