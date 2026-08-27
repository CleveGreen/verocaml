type 'a mutable_wrapper =
  | Ground
  | Cell of { mutable value : 'a }
