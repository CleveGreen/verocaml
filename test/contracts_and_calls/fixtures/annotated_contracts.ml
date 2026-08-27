let increment (x : int) =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result = [%verocaml.old x] + 1];
  x + 1

let call_increment (x : int) =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result -> result > x];
  [%verocaml.assert x < 4_611_686_018_427_387_903];
  increment x

let classify (x : int) =
  [%verocaml.ensures fun result -> result >= 0];
  match x with
  | 0 -> 0
  | n when n > 0 -> 1
  | _ -> 2
