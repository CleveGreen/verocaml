let effects = ref 0
let effect () = incr effects; 41
let run () =
  let[@ghost] _g = (effect () [@ghost]) in
  let[@tracked] _t = (effect () [@tracked]) in
  !effects
let () = Printf.printf "effects=%d\n" (run ())
