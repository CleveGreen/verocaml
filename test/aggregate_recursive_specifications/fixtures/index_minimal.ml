type 'a node =
  | Empty
  | Node of 'a * 'a node

type 'a option =
  | None
  | Some of 'a

let spec_is_none (a : int option) : bool =
  match a with
    | None -> true
    | _ -> false
[@@verocaml.spec]

let spec_opt_eq (a : int option) (b : int option) : bool =
  match a with
    | None -> spec_is_none b
    | Some av -> (match b with
      | None -> false
      | Some bv -> av = bv)
[@@verocaml.spec]

let rec spec_index_alt_imp (idx : Vstd.Int.t) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
    | Empty -> Empty
    | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_index (idx : int) (nodes : int node) : int option =
  match spec_index_alt_imp idx nodes with
    | Empty -> None
    | Node (value, _) -> Some value
[@@verocaml.spec]

let rec index_imp (idx : int) (nodes : int node [@finite]) : int node =
  [%verocaml.requires idx >= 0];
  [%verocaml.ensures fun result -> result = spec_index_alt_imp idx nodes];
  [%verocaml.decreases nodes];
  match nodes with
    | Empty -> Empty
    | Node (value, rest) ->
      if idx = 0 then Node (value, Empty)
      else index_imp (idx - 1) rest

let index (idx : int) (nodes : int node [@finite]) : int option =
  [%verocaml.requires idx >= 0];
  [%verocaml.ensures fun result -> spec_opt_eq result (spec_index idx nodes)];
  match index_imp idx nodes with
    | Empty -> None
    | Node (value, _) -> Some value
