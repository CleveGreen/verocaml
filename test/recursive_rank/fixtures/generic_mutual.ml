type 'a left =
  | Left_ground
  | Left_next of 'a right

and 'a right =
  | Right_ground
  | Right_next of 'a left
