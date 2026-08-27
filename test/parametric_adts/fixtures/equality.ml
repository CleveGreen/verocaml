type 'a local_option = LNone | LSome of 'a

type 'a scalar = Empty | Flag of bool | Count of int | Mixed of bool * int

let local_matrix () =
  [%verocaml.ensures fun result -> result];
  LNone = (LNone : int local_option)
  && LNone <> LSome 1
  && LSome 1 = LSome 1
  && LSome 1 <> LSome 2

let stdlib_matrix () =
  [%verocaml.ensures fun result -> result];
  None = (None : int option)
  && None <> Some 1
  && Some 1 = Some 1
  && Some 1 <> Some 2

let local_reflexive (value : int local_option) =
  [%verocaml.ensures fun result -> result];
  let alias = value in
  value = alias && not (value <> alias)

let local_conditional (flag : bool) (value : int local_option) =
  [%verocaml.ensures fun result -> result];
  value = (if flag then value else value)

let local_match (value : int local_option) =
  [%verocaml.ensures fun result -> result];
  match value with
  | LNone -> value = LNone
  | LSome payload -> value = LSome payload

let stdlib_reflexive (value : int option) =
  [%verocaml.ensures fun result -> result];
  let alias = value in
  value = alias && not (value <> alias)

let scalar_matrix () =
  [%verocaml.ensures fun result -> result];
  let empty : int scalar = Empty in
  let flag_true : int scalar = Flag true in
  let flag_false : int scalar = Flag false in
  let count_one : int scalar = Count 1 in
  let count_two : int scalar = Count 2 in
  let mixed : int scalar = Mixed (true, 1) in
  let mixed_bool : int scalar = Mixed (false, 1) in
  let mixed_int : int scalar = Mixed (true, 2) in
  empty = empty
  && empty <> flag_true
  && flag_true = flag_true
  && flag_true <> flag_false
  && count_one = count_one
  && count_one <> count_two
  && mixed = mixed
  && mixed <> mixed_bool
  && mixed <> mixed_int
