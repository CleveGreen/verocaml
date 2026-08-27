module type STACK = sig type t val make : int -> t end
module Stack = struct type t = { value : int } let make value = { value } end
