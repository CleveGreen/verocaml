type 'a box = Box of 'a
type 'a seq = Nil | Cons of 'a * 'a seq

[%%verocaml.symbolic val option_image : 'a option -> 'a option]
[%%verocaml.symbolic val seq_image : 'a seq -> 'a seq]
[%%verocaml.symbolic val combine : 'a box -> 'a option -> 'a box]
[%%verocaml.symbolic val closed_image : int option -> bool option]

let option_identity (value : int option) : int option =
  [%verocaml.ensures fun _ -> option_image value = option_image value];
  value

let sequence_identity (value : bool seq) : bool seq =
  [%verocaml.ensures fun _ -> seq_image value = seq_image value];
  value

let multiargument_identity
    (box : int box)
    (optional : int option) : int box =
  [%verocaml.ensures fun _ ->
    combine box optional = combine box optional];
  box

let closed_identity (value : int option) : int option =
  [%verocaml.ensures fun _ -> closed_image value = closed_image value];
  value
