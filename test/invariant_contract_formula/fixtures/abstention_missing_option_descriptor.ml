let choose ?(value = 0) () = value

let missing_option_descriptor (value : int) = choose ~value ()
