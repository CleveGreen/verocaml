let rec toggle (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  if i = 0 then
    toggle 1
  else
    toggle 0


let rec shadowed_toggle (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  let next =
    let i = 1 - i in
    i
  in
  shadowed_toggle next

let rec recursive_in_argument (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  if i = 0 then
    recursive_in_argument (recursive_in_argument 1)
  else
    recursive_in_argument 0

let rec recursive_in_let_rhs (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  let x =
    if i = 0 then recursive_in_let_rhs 1
    else recursive_in_let_rhs 0
  in
  x

let rec recursive_in_condition (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  if recursive_in_condition (1 - i) = 0 then
    0
  else
    0

let rec state_confusion (i : int) : int =
  [%verocaml.requires i = 0 || i = 1];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];

  let i = 1 - i in
  state_confusion i
