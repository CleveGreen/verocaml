module type STACK = sig type t end
module Stack : STACK = struct type t = int end
let packed = (module Stack : STACK)
