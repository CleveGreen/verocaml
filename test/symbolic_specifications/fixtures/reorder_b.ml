[%%verocaml.symbolic val second : bool -> bool]
[%%verocaml.symbolic val first : int -> int]

let use (number : int) (flag : bool) : int =
  [%verocaml.ensures fun _ ->
    first number = first number && second flag = second flag];
  number
