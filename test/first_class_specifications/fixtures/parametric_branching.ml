[%%verocaml.symbolic val project : 'a -> 'a]
[%%verocaml.symbolic val image : (int -> int) -> int -> int]

let generic_project (value : 'a) : 'a = project value [@@verocaml.spec]

let choose (index : int) : int =
  if index = 4 then 42 else index
[@@verocaml.spec]

let image_axiom (f : int -> int) (index : int) : unit =
  [%verocaml.ensures fun _ -> (image f index) [@trigger] = f index];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

[@@@verocaml.activate [image_axiom]]

let verify (value : int) : int =
  [%verocaml.assert generic_project value = project value];
  [%verocaml.assert image (fun index -> choose index) 4 = 42];
  [%verocaml.assert image (fun index -> choose index) 3 = 3];
  [%verocaml.ensures fun result -> result = value];
  value
