type snapshot = { length : int }

module type STACK = sig
  type t
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val snapshot : t @ read -> snapshot @ immutable
  val two_nodes : int -> int -> t @ unique
  val cut : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let model (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    (model stack).length >= 0
  [@@verocaml.type_invariant]

  let snapshot (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }

  let two_nodes first second : t @ unique =
    {
      top =
        Node
          {
            value = first;
            next = Node { value = second; next = Empty };
          };
      length = 2;
    }

  let cut (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.next <- Empty;
        stack.length <- 1;
        stack
    | _ -> stack

end

let nested_cut first second =
  let stack = Stack.two_nodes first second in
  let stack = Stack.cut stack in
  let _view = Stack.snapshot stack in
  stack
