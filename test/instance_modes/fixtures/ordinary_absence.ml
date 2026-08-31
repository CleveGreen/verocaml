let effects = ref 0
let effect () = incr effects; 41
let run () =
  let[@ghost] _g = (effect () [@ghost]) in
  let[@tracked] _t = (effect () [@tracked]) in
  !effects
let () = if run () <> 0 then failwith "erased bindings evaluated their effects"
