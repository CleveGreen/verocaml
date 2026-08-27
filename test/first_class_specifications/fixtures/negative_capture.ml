let captured_reference (value : int) : int -> int =
  let cell = ref value in
  fun argument -> !cell + argument
[@@verocaml.spec]
