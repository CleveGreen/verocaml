type aggregate = { value : int }

let arbitrary_equality
    (_stack : Generic_family_provider.Stack.t @ read)
    left
    right =
  (left : aggregate) = (right : aggregate)
