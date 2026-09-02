[@@@verocaml.verify]

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a lookalike_specification = 'a Optional_carrier_shapes.lookalike
[@@verocaml.external_type_specification]

type 'a box_specification = 'a Optional_carrier_shapes.box
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

let choose fallback ?(value = fallback) () =
  [%verocaml.ensures fun result -> result = value];
  value

let omitted () =
  [%verocaml.ensures fun result -> result = 7];
  choose 7 ()

let provided () =
  [%verocaml.ensures fun result -> result = 9];
  choose 7 ~value:9 ()

let forwarded (carrier : int option) = choose 7 ?value:carrier ()

let choose_nested
    ?(value =
      (Ok (Optional_carrier_shapes.Box (Some 1)) :
        (int option Optional_carrier_shapes.box, bool) result))
    () = value

let nested_forwarded
    (carrier :
      (int option Optional_carrier_shapes.box, bool) result option) =
  choose_nested ?value:carrier ()
