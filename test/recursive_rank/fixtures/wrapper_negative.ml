type 'a wrapper = W of ('a -> int)
type t = Ground | K of t wrapper
