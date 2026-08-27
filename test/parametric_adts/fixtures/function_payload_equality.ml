type 'a scalar = Empty | Payload of (int -> int)
let equal (left : int scalar) right = left = right
