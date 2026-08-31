type packet = { run : int; mutable ghost : int [@ghost] }

let effects = ref 0
let receiver packet = incr effects; packet
let rhs () = incr effects; 41

let run () =
  let packet = { run = 0; ghost = (0 [@ghost]) } in
  (((receiver packet).ghost <- (rhs () [@ghost])) [@ghost]);
  !effects

let () = if run () <> 0 then failwith "erased update evaluated its effects"
