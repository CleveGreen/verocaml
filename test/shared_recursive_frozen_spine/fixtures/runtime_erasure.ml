type seq = End | More of int * seq
type node = { mutable value : int; next : node option }

let set_head node value = node.value <- value

let () =
  let tail = { value = 2; next = None } in
  let root = { value = 1; next = Some tail } in
  set_head root 42;
  Printf.printf "%d\n" root.value
