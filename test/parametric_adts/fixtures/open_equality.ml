type 'a local_option = LNone | LSome of 'a
let equal (left : 'a local_option) right = left = right
