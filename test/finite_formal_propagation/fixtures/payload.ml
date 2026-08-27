type node = Empty | Node of node
let checked (value : node [@finite true]) = ()
