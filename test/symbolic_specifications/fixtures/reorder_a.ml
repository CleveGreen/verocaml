[%%verocaml.symbolic val first : int -> int]
[%%verocaml.symbolic val second : bool -> bool]

let use (number : int) (flag : bool) : int =
  [%verocaml.ensures fun _ ->
    first number = first number && second flag = second flag];
  number
