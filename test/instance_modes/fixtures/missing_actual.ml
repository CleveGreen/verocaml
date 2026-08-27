let bad x =
  let y = (x [@ghost]) in
  y
