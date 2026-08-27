type 'a box = Box of 'a
let copy (box : 'a box) = match box with Box value -> Box value
