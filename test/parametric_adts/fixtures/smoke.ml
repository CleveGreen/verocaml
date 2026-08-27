type 'a box = Box of 'a

type 'a pair = { left : 'a; right : 'a }

let unbox box = match box with Box value -> value
let make_pair value = { left = value; right = value }

let spec_unbox (box : 'a box) : 'a = match box with Box value -> value [@@verocaml.spec]
let spec_left (pair : 'a pair) : 'a = pair.left [@@verocaml.spec]

let verify value : unit =
  [%verocaml.assert spec_unbox (Box value) = value];
  [%verocaml.assert spec_left { left = value; right = value } = value];
  ()
[@@verocaml.proof]
