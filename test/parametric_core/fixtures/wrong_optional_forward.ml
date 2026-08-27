let pick ?value () = value
let bad () = pick ?value:3 ()
