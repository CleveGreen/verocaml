type 'a box =
  | Empty
  | Box of ('a * int) * 'a box
