module type BOX = sig
  type t
  val make : int -> t
  val get : t @ read -> int
end

module Box : BOX = struct
  type t = { value : int }

  let make value = { value }

  let get (box : t @ read) =
    [%verocaml.ensures fun result -> result = box.value];
    box.value
end
