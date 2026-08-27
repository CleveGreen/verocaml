module type STACK = sig type t end
module Make () : STACK = struct type t = int end
