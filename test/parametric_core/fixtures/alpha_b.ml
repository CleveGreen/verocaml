let id (value : 'renamed) : 'renamed = value
let choose flag (left : 'other) (right : 'other) : 'other =
  if flag then left else right
let use (value : int) = choose true (id value) value
