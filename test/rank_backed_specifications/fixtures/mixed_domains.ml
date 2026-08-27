type left = Left_empty | Left of left
type right = Right_empty | Right of right

let mixed (left : left) (right : right) : left * right =
  (left, right)
[@@verocaml.spec]
