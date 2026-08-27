type node = Empty | Node of node

let checked (_value : node [@finite]) : unit = ()

let run () : unit =
  checked (Node Empty)
