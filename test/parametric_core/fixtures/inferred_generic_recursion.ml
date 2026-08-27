let rec loop value = if true then value else loop value
let use (value : int) = loop value
