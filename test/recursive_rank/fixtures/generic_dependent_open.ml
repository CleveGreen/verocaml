type 'a nonempty =
  | One of 'a
  | More of 'a * 'a nonempty
