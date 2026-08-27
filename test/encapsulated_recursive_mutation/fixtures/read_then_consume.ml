type t = { mutable value : int }
let snapshot (x : t @ read) = x.value
let step (x : t @ unique) : t @ unique = x
let bad (x : t @ unique) =
  let view = snapshot x in
  let x = step x in
  (view, x)
