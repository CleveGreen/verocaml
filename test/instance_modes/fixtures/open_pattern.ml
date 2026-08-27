type t = { run : int; ghost : int [@ghost] }
let bad value = match value with { run; _ } -> run
