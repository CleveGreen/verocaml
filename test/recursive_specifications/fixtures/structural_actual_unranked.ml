type 'a box = Box of 'a

let round_trip (value : int) : int =
  let boxed : int box = Box value in
  match boxed with Box payload -> payload
