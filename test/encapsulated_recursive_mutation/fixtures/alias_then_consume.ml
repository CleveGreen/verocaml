type t = { mutable value : int }
let step (x : t @ unique) : t @ unique = x
let bad (x : t @ unique) =
  let alias = x in
  let _ = step x in
  alias.value
