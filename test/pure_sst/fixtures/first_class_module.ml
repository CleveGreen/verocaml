let packed () =
  (module struct
    type t = int
    let compare (left : int) right = Stdlib.compare left right
  end : Set.OrderedType with type t = int)
