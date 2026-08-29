type 'a box = Empty | Box of 'a
type ('a, 'error) outcome = Good of 'a | Bad of 'error
type 'a cell = { value : 'a; flag : bool }
type 'a higher_order = Higher_order of ('a -> 'a)
type 'a private_box = private Private_box of 'a
