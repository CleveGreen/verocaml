type 'a seq = Nil | Cons of 'a * 'a seq
let rec finite_refl (xs : 'a seq [@finite]) : unit =
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> finite_refl tail
[@@verocaml.proof]
let use_int value = finite_refl (Cons (value, Nil)) [@@verocaml.proof]
