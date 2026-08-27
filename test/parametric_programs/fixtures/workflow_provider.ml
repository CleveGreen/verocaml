[@@@verocaml.verify]

type 'a batch = { ready : 'a list; deferred : 'a list }
type ('a, 'error) outcome = Accepted of 'a | Rejected of 'error
type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

let identity value = [%verocaml.ensures fun result -> result = value] ; value

let choose flag left right =
  [%verocaml.ensures fun result -> result = left || result = right]
  ;
  if flag then left else right

let copy_option (value : 'a option) =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with None -> None | Some payload -> Some payload

let copy_result (value : ('a, 'error) result) =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with Ok payload -> Ok payload | Error error -> Error error

let copy_outcome (value : ('a, 'error) outcome) =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with
  | Accepted payload -> Accepted payload
  | Rejected error -> Rejected error

let rec append left right =
  [%verocaml.decreases left];
  match left with [] -> right | value :: rest -> value :: append rest right

let rec reverse_into remaining accumulated =
  [%verocaml.decreases remaining];
  match remaining with
  | [] -> accumulated
  | value :: rest -> reverse_into rest (value :: accumulated)

let reverse values = reverse_into values []

let safe_succ value =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if value = 4611686018427387903 then value else value + 1

let rec length values =
  [%verocaml.ensures fun result -> result >= 0]
  ;
  [%verocaml.decreases values] ;
  match values with [] -> 0 | _ :: rest -> safe_succ (length rest)

let make_batch ready deferred = { ready; deferred }

let normalize_batch batch =
  { ready = append batch.ready (reverse batch.deferred); deferred = [] }

let rec tree_size tree =
  [%verocaml.ensures fun result -> result >= 0]
  ;
  [%verocaml.decreases tree] ;
  match tree with
  | Leaf -> 0
  | Branch (_, left, right) ->
      let left_size = tree_size left in
      let right_size = tree_size right in
      let total =
        if left_size > 4611686018427387903 - right_size then 4611686018427387903
        else left_size + right_size
      in
      safe_succ total

let rec mirror tree =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> Leaf
  | Branch (value, left, right) -> Branch (value, mirror right, mirror left)
