type 'a seq = Nil | Cons of 'a * 'a seq

val same_shape : 'a seq -> bool
[@@verocaml.spec]
