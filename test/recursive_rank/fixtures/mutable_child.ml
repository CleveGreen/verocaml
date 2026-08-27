type t =
  | Ground
  | Next of { mutable child : t }
