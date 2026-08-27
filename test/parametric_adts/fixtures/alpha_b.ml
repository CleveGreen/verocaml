type 'payload box = Box of 'payload
let copy (box : 'payload box) = match box with Box value -> Box value
