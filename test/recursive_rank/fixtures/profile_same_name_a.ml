type 'a box =
  | Empty
  | Box of 'a * 'a box
