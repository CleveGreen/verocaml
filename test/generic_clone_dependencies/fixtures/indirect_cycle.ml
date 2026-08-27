type 'a box = Box of 'a

let rec rotate : 'a 'b. 'a box -> 'b box -> bool =
 fun left right ->
  [%verocaml.decreases left];
  rotate right left
[@@verocaml.spec] [@@verocaml.revealed]

let control (left : int box) (right : bool box) : bool =
  rotate left right
[@@verocaml.spec]
