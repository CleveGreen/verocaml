type 'a local_option = LNone | LSome of 'a
let equal (left : int array local_option) right = left = right
