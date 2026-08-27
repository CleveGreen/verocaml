type t = { run : int; ghost : int [@ghost] }
let bad value = { value with run = 1; ghost = (2 [@ghost]) }
