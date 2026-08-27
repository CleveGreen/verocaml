type 'a node = Empty | Node of 'a * 'a node
type 'a option = None | Some of 'a

let spec_is_none (a : int option) : bool =
  match a with None -> true | _ -> false
[@@verocaml.spec]

let spec_opt_eq (a : int option) (b : int option) : bool =
  match a with
  | None -> spec_is_none b
  | Some av -> (match b with None -> false | Some bv -> av = bv)
[@@verocaml.spec]

let spec_node_value (node : int node) : int option =
  match node with Empty -> None | Node (value, _) -> Some value
[@@verocaml.spec]

let direct (nodes : int node) : int option =
  [%verocaml.ensures fun result -> spec_opt_eq result (spec_node_value nodes)];
  match nodes with Empty -> None | Node (value, _) -> Some value

let identity_node (nodes : int node) : int node =
  [%verocaml.ensures fun result -> result = nodes];
  nodes

let through_helper (nodes : int node) : int option =
  [%verocaml.ensures fun result -> spec_opt_eq result (spec_node_value nodes)];
  match identity_node nodes with Empty -> None | Node (value, _) -> Some value
