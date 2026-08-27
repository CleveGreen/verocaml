type 'a higher =
  | Higher of { apply : 'b. 'b -> 'a }
