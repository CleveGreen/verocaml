type left =
  | Left_end
  | Left_next of left

type right =
  | Right_end
  | Right_next of right

let rec left_size value =
  [%verocaml.decreases value];
  match value with
  | Left_end -> 0
  | Left_next rest -> 1 + left_size rest
[@@verocaml.spec]
[@@verocaml.revealed]

let rec right_size value =
  [%verocaml.decreases value];
  match value with
  | Right_end -> 0
  | Right_next rest -> 1 + right_size rest
[@@verocaml.spec]
[@@verocaml.revealed]

let two_domains (left : left [@finite]) (right : right [@finite]) =
  let _ = left in
  let _ = right in
  ()
[@@verocaml.proof]
