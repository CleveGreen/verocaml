type seq = End | More of int * seq

let tail (xs : seq) : seq =
  match xs with End -> End | More (_, rest) -> rest
[@@verocaml.spec]

module type LIST = sig
  type t
  val make_two : int -> int -> t @ unique
  val contents : (t [@finite]) @ read -> seq @ immutable
  val invariant : (t [@finite]) @ read -> bool
  val set_head : (t [@finite]) @ aliased -> int -> unit
  val head : (t [@finite]) @ read -> int
end

module List : LIST = struct
  type node = { mutable value : int; next : link }
  and link = Nil | Next of node
  type t = node

  let rec contents_node (node : (node [@finite]) @ read) : seq =
    [%verocaml.decreases node];
    let rest =
      match node.next with
      | Nil -> End
      | Next next -> contents_node next
    in
    More (node.value, rest)
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let contents (node : (t [@finite]) @ read) : seq @ immutable =
    contents_node node
  [@@verocaml.spec]

  let invariant (node : (t [@finite]) @ read) : bool =
    match contents node with End -> false | More _ -> true
  [@@verocaml.type_invariant]

  let make_two a b : t @ unique =
    [%verocaml.ensures fun result ->
      contents result = More (a, More (b, End))];
    { value = a; next = Next { value = b; next = Nil } }

  let set_head (node : (t [@finite]) @ aliased) value : unit =
    [%verocaml.ensures fun _ ->
      contents node = More (value, tail ([%verocaml.old (contents node)]))];
    node.value <- value

  let head (node : (t [@finite]) @ read) = node.value
end

let replace_head_and_read
    (xs : (List.t [@finite]) @ aliased) value : int =
  [%verocaml.requires value <> List.head xs];
  [%verocaml.ensures fun result ->
    result = value
    && List.contents xs =
         More (value, tail ([%verocaml.old (List.contents xs)]))];
  let peer = xs in
  List.set_head peer value;
  List.head xs
