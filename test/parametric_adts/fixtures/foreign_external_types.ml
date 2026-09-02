type 'a box = Empty | Box of 'a

type ('a, 'error) outcome =
  | Good of 'a
  | Bad of 'error
