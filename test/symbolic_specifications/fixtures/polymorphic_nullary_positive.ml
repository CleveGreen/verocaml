type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a box = Box of 'a

[%%verocaml.symbolic val empty_option : 'a option]
[%%verocaml.symbolic val empty_box : 'a box]
[%%verocaml.symbolic val arbitrary : 'a]
[%%verocaml.symbolic val unit_value : unit]
[%%verocaml.symbolic val observe_unit : unit -> int]
[%%verocaml.symbolic val produce_unit : int -> unit]

let integer_option (value : int) : int =
  [%verocaml.ensures fun _ ->
    (empty_option : int option) = (empty_option : int option)];
  value

let boolean_option (value : bool) : bool =
  [%verocaml.ensures fun _ ->
    (empty_option : bool option) = (empty_option : bool option)];
  value

let integer_box (value : int) : int =
  [%verocaml.ensures fun _ ->
    (empty_box : int box) = (empty_box : int box)];
  value

let boolean_box (value : bool) : bool =
  [%verocaml.ensures fun _ ->
    (empty_box : bool box) = (empty_box : bool box)];
  value

let arbitrary_integer (value : int) : int =
  [%verocaml.ensures fun _ ->
    (arbitrary : int) = (arbitrary : int)];
  value

let arbitrary_boolean (value : bool) : bool =
  [%verocaml.ensures fun _ ->
    (arbitrary : bool) = (arbitrary : bool)];
  value

let arbitrary_unit () : unit =
  [%verocaml.ensures fun _ ->
    (arbitrary : unit) = (arbitrary : unit) && unit_value = ()];
  ()

let symbolic_unit_argument (value : int) : int =
  [%verocaml.ensures fun _ -> observe_unit () = observe_unit ()];
  value

let symbolic_unit_result (value : int) : int =
  [%verocaml.ensures fun _ -> produce_unit value = ()];
  value
