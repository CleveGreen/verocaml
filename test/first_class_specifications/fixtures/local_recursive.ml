let use (value : int) : int =
  [%verocaml.assert
    let rec loop (argument : int) : int =
      if argument = 0 then value else loop (argument - 1)
    in
    loop value = value];
  value
