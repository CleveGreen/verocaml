type 'a scalar = Empty | Payload of unit
let equal (left : int scalar) right = left = right
