let guarded_add (x : int) =
  if x < 4_611_686_018_427_387_903 then x + 1 else x

let guarded_subtract (x : int) =
  if -4_611_686_018_427_387_904 < x then x - 1 else x

let unguarded_add (x : int) = x + 1
let unguarded_subtract (x : int) = x - 1
let arithmetic_branches (x : int) = if x >= 0 then x + 1 else x - 1
let intermediate (x : int) = (x + 1) + 1
let negate (x : int) = -x
let scale (x : int) = x * 3
let standard (x : int) = succ x + pred (abs x)
