[@@@verocaml.verify]

let direct value = (Provider.make_record value).Provider.value

let bound value =
  let result = Provider.make_record value in
  result.Provider.value

let unique value =
  let result = Provider.make_unique_record value in
  result.Provider.value

let variant value =
  match Provider.make_variant value with
  | Provider.Missing -> 0
  | Provider.Present payload -> payload

let generic_int value = (Provider.make_box value).Provider.box_value
let generic_bool value = (Provider.make_box value).Provider.box_value

let generic_open item =
  let box = Provider.pass_box (Provider.make_box item) in
  box.Provider.box_value

let tree item =
  match Provider.pass_tree (Provider.make_tree item) with
  | Provider.Leaf -> item
  | Provider.Node (payload, _, _) -> payload

let list values =
  match Provider.pass_box (Provider.make_box values) with
  | { Provider.box_value = [] } -> 0
  | { Provider.box_value = _ :: _ } -> 1

let chain count =
  [%verocaml.requires count >= 0];
  match Provider.make_chain count with
  | Provider.End -> 0
  | Provider.Link (head, _) -> head

let tuple value =
  let scalar, record, box = Provider.make_tuple value in
  if scalar = record.Provider.value then box.Provider.box_value else scalar

let pass value = Provider.pass_record (Provider.make_record value)
let return value = Provider.make_record value

let postcondition value =
  [%verocaml.ensures fun result -> result = value];
  (Provider.make_record value).Provider.value

let pattern_fact value =
  [%verocaml.ensures fun result -> result >= 0];
  match Provider.make_variant value with
  | Provider.Missing -> 0
  | Provider.Present _ -> 1
