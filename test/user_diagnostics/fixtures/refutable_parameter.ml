type 'a box = Box of 'a
let unwrap (Box value) = value
let use (value : int box) = unwrap value
