let math_succ (x : int) : int = x + 1 [@@verocaml.spec]

let twice (x : int) : int = math_succ (math_succ x) [@@verocaml.spec]

let shifted (x : int) : int = x + 4_611_686_018_427_387_903 [@@verocaml.spec]

let growing (x : int) : bool =
  let y = twice x in
  if y > x then true else false
[@@verocaml.spec]

let verified (x : int) : int =
  [%verocaml.requires growing x];
  [%verocaml.assert twice x = x + 2];
  [%verocaml.ensures fun result -> result = x && shifted x > x];
  x
