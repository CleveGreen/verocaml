type box = { value : int }

let observe_int (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let observe_box (box : box) : unit =
  let _ = box in
  ()
[@@verocaml.proof]

let unique_return value : box @ unique =
  let result = { value } in
  [%verocaml.proof observe_box result];
  result

let mode_locals value =
  let ordinary = value + 1 in
  [%verocaml.proof observe_int ordinary];
  ordinary

let global_only () =
  [%verocaml.proof observe_int 0];
  ()
