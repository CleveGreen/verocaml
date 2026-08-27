type 'a witness =
  | Int : int witness
  | Pair : 'a witness * 'b witness -> ('a * 'b) witness
