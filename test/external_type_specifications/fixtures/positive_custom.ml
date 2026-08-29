type 'a box_specification = 'a External_types.box
[@@verocaml.external_type_specification]

type ('a, 'error) outcome_specification =
  ('a, 'error) External_types.outcome
[@@verocaml.external_type_specification]

type 'a cell_specification = 'a External_types.cell
[@@verocaml.external_type_specification]

let has_box (box : 'a External_types.box) : bool =
  match box with Empty -> false | Box _ -> true
[@@verocaml.spec]

let is_good (outcome : ('a, 'error) External_types.outcome) : bool =
  match outcome with Good _ -> true | Bad _ -> false
[@@verocaml.spec]

let cell_flag (cell : 'a External_types.cell) : bool =
  cell.flag
[@@verocaml.spec]

let make_box (value : int) : int External_types.box =
  [%verocaml.ensures fun result -> has_box result];
  Box value

let make_good (value : int) : (int, bool) External_types.outcome =
  [%verocaml.ensures fun result -> is_good result];
  Good value

let make_cell (value : int) : int External_types.cell =
  [%verocaml.ensures fun result -> cell_flag result];
  { value; flag = true }
