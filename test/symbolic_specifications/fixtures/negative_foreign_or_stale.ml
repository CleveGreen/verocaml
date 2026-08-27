[%%verocaml.symbolic val local_image : int -> int]

let local_identity (value : int) : int =
  [%verocaml.ensures fun _ -> local_image value = local_image value];
  value
