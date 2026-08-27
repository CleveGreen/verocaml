let id (value : 'alpha) : 'alpha = value
let choose flag (left : 'beta) (right : 'beta) : 'beta =
  if flag then left else right
let use (value : int) = choose true (id value) value
