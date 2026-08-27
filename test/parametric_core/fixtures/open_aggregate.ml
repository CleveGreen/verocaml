type 'a box = Box of 'a
let unwrap (Box value) = value
let bad value = unwrap (Box value)
