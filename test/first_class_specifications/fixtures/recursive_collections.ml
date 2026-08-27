type 'a chain = Empty | Link of 'a * 'a chain

let rec map (f : 'a -> 'b) (xs : 'a chain) : 'b chain =
  [%verocaml.decreases xs];
  match xs with Empty -> Empty | Link (head, tail) -> Link (f head, map f tail)
[@@verocaml.spec] [@@verocaml.revealed]

let increment (x : int) : int = x + 1 [@@verocaml.spec]

let rec all (predicate : 'a -> bool) (xs : 'a chain) : bool =
  [%verocaml.decreases xs];
  match xs with
  | Empty -> true
  | Link (head, tail) -> predicate head && all predicate tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec any (predicate : 'a -> bool) (xs : 'a chain) : bool =
  [%verocaml.decreases xs];
  match xs with
  | Empty -> false
  | Link (head, tail) -> predicate head || any predicate tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec fold_left (combine : 'b -> 'a -> 'b) (accumulator : 'b) (xs : 'a chain)
    : 'b =
  [%verocaml.decreases xs];
  match xs with
  | Empty -> accumulator
  | Link (head, tail) -> fold_left combine (combine accumulator head) tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec fold_right (combine : 'a -> 'b -> 'b) (xs : 'a chain) (accumulator : 'b)
    : 'b =
  [%verocaml.decreases xs];
  match xs with
  | Empty -> accumulator
  | Link (head, tail) -> combine head (fold_right combine tail accumulator)
[@@verocaml.spec] [@@verocaml.revealed]

let positive (x : int) : bool = x > 0 [@@verocaml.spec]
let add (left : int) (right : int) : int = left + right [@@verocaml.spec]

let verify_map (x : int) : int =
  [%verocaml.assert map increment (Link (x, Empty)) = Link (x + 1, Empty)];
  [%verocaml.assert all positive (Link (x, Empty)) = (x > 0)];
  [%verocaml.assert any positive (Link (x, Empty)) = (x > 0)];
  [%verocaml.assert fold_left add x (Link (2, Empty)) = x + 2];
  [%verocaml.assert fold_right add (Link (2, Empty)) x = x + 2];
  [%verocaml.ensures fun result -> result = x];
  x
