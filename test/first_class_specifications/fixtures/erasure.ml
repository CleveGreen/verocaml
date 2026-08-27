let adder (value : int) : int -> int = fun argument -> value + argument
[@@verocaml.spec]

let runtime_value = 42
let () = Printf.printf "runtime=%d\n" runtime_value
