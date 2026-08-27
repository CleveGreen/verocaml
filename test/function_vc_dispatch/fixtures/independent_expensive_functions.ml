let independent_alpha (value : int) : int =
  assert (value = value);
  assert (value <= value);
  value

let independent_beta (value : int) : int =
  assert (value >= value);
  assert (value = value);
  value

let independent_gamma (choose : bool) (value : int) : int =
  if choose then assert (value = value) else assert (value <= value);
  value
