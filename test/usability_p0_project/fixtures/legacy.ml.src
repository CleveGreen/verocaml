let promised value =
  try value + 1 with _ -> value

let select ~first ?(value = first) () = value

module Unsupported_target_body = struct
  class ['a] state (initial : 'a) = object
    val mutable value = initial
    method get = value
  end
end
