type 'a chain =
  | Empty
  | Link of 'a * 'a chain
