type 'a seq = Nil | Cons of 'a * 'a seq

let identity (xs : 'a seq) = xs

let instantiate (xs : int seq) = identity xs
