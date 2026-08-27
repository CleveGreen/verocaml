type box = { value : int }

let make_box value : box @ unique = { value }
let consume_unique (box : box @ unique) = box.value

let () = Printf.printf "%d\n" (consume_unique (make_box Sys.int_size))
