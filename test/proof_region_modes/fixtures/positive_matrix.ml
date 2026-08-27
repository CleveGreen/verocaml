type box = { mutable value : int }

let observe_int (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let observe_box (box : box) : unit =
  [%verocaml.requires box.value >= 0];
  ()
[@@verocaml.proof]

let global_only () =
  [%verocaml.proof observe_int 0];
  ()

let unique_return (value : int) : box @ unique =
  let result = { value } in
  [%verocaml.proof observe_box result];
  result

let unique_mutation (box : box @ unique) : box @ unique =
  [%verocaml.requires box.value >= 0];
  [%verocaml.proof observe_box box];
  box.value <- box.value + 1;
  box

let mode_locals source =
  let ordinary = source + 1 in
  let[@tracked] tracked = (source [@tracked]) in
  [%verocaml.proof
    observe_int ordinary;
    observe_int tracked];
  let[@tracked] _still_tracked = (tracked [@tracked]) in
  ordinary

let multiple first second =
  let third = first + second in
  [%verocaml.proof
    observe_int first;
    observe_int second;
    observe_int third];
  third

let destructured ((left, right) : int * int) =
  let sum = left + right in
  [%verocaml.proof
    observe_int left;
    observe_int right;
    observe_int sum];
  sum

let branch choose source =
  match choose with
  | true ->
      let branch_value = source + 1 in
      [%verocaml.proof observe_int branch_value];
      branch_value
  | false ->
      let branch_value = source + 2 in
      [%verocaml.proof observe_int branch_value];
      branch_value

let nested_shadow source =
  let shadow = source + 1 in
  let outer = shadow in
  let shadow = shadow + 1 in
  let nested =
    let inside = shadow + outer in
    [%verocaml.proof
      observe_int outer;
      observe_int shadow;
      observe_int inside];
    inside
  in
  nested
