[@@@verocaml.verify]

type 'a chain = End | Link of 'a * 'a chain
type 'a queue = { front : 'a chain; back : 'a chain }
type 'a queue_step = Queue_empty | Queue_item of 'a * 'a queue
type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree
type ('a, 'b) choice = First of 'a | Second of 'b

let identity value = [%verocaml.ensures fun result -> result = value] ; value

let constant value _ignored =
  [%verocaml.ensures fun result -> result = value] ; value

let choose flag left right =
  [%verocaml.ensures fun result -> result = left || result = right]
  ;
  if flag then left else right

let copy_option (value : 'a option) : 'a option =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with None -> None | Some payload -> Some payload

let copy_result (value : ('a, 'b) result) : ('a, 'b) result =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with Ok payload -> Ok payload | Error error -> Error error

let copy_choice (value : ('a, 'b) choice) : ('a, 'b) choice =
  [%verocaml.ensures fun result -> result = value]
  ;
  match value with
  | First payload -> First payload
  | Second payload -> Second payload

let safe_succ value =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if value = 4611686018427387903 then value else value + 1

let empty_queue () = { front = End; back = End }
let singleton value = { front = Link (value, End); back = End }

let enqueue value queue =
  { front = queue.front; back = Link (value, queue.back) }

let rec reverse_into remaining accumulated =
  [%verocaml.decreases remaining];
  match remaining with
  | End -> accumulated
  | Link (value, rest) -> reverse_into rest (Link (value, accumulated))

let reverse values = reverse_into values End

let normalize queue =
  match queue.front with
  | Link _ -> queue
  | End -> { front = reverse queue.back; back = End }

let dequeue queue =
  let normalized = normalize queue in
  match normalized.front with
  | End -> Queue_empty
  | Link (value, rest) ->
      Queue_item (value, { front = rest; back = normalized.back })

let rec append left right =
  [%verocaml.decreases left];
  match left with
  | End -> right
  | Link (value, rest) -> Link (value, append rest right)

let rec length values =
  [%verocaml.ensures fun result -> result >= 0]
  ;
  [%verocaml.decreases values] ;
  match values with End -> 0 | Link (_, rest) -> safe_succ (length rest)

let queue_length queue =
  let front_length = length queue.front in
  let back_length = length queue.back in
  if front_length > 4611686018427387903 - back_length then 4611686018427387903
  else front_length + back_length

let rec nth index values =
  [%verocaml.decreases values];
  match values with
  | End -> None
  | Link (value, rest) ->
      if index <= 0 then Some value else nth (index - 1) rest

let rec take count values =
  [%verocaml.decreases values];
  if count <= 0 then End
  else
    match values with
    | End -> End
    | Link (value, rest) -> Link (value, take (count - 1) rest)

let rec drop count values =
  [%verocaml.decreases values];
  if count <= 0 then values
  else match values with End -> End | Link (_, rest) -> drop (count - 1) rest

let rec tree_size tree =
  [%verocaml.ensures fun result -> result >= 0]
  ;
  [%verocaml.decreases tree] ;
  match tree with
  | Leaf -> 0
  | Branch (_, left, right) ->
      let left_size = tree_size left in
      let right_size = tree_size right in
      let children =
        if left_size > 4611686018427387903 - right_size then 4611686018427387903
        else left_size + right_size
      in
      safe_succ children

let max_nonnegative (left : int) (right : int) : int =
  [%verocaml.requires left >= 0];
  [%verocaml.requires right >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if left >= right then left else right

let rec tree_height tree =
  [%verocaml.ensures fun result -> result >= 0]
  ;
  [%verocaml.decreases tree] ;
  match tree with
  | Leaf -> 0
  | Branch (_, left, right) ->
      safe_succ (max_nonnegative (tree_height left) (tree_height right))

let rec mirror tree =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> Leaf
  | Branch (value, left, right) -> Branch (value, mirror right, mirror left)

let rec leftmost tree =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> None
  | Branch (value, Leaf, _) -> Some value
  | Branch (_, left, _) -> leftmost left

let rec preorder_into tree accumulated =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> accumulated
  | Branch (value, left, right) ->
      Link (value, preorder_into left (preorder_into right accumulated))

let preorder tree = preorder_into tree End

let rec same_shape (left : 'a tree) (right : 'a tree) =
  [%verocaml.decreases left];
  match (left, right) with
  | Leaf, Leaf -> true
  | Branch (_, left_a, right_a), Branch (_, left_b, right_b) ->
      same_shape left_a left_b && same_shape right_a right_b
  | _, _ -> false
