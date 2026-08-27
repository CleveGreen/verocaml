let mutable_read (value : int) =
  let mutable local = value in
  local <- local + 1;
  local
