type box = { mutable value : int }
type outer = { mutable inner : box }
let rejected (outer : outer @ unique) : outer @ unique =
  outer.inner.value <- 1;
  outer
