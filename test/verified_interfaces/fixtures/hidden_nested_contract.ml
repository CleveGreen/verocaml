module type BOX = sig
  type t
  val make : int -> t
  val get : t @ read -> int
end

module Box : BOX = struct
  type t = { value : int }

  let make value = { value }

  let get (box : t @ read) =
    [%verocaml.ensures
      fun result ->
        let hidden =
          if result >= 0 then box.value else box.value
        in
        result = hidden];
    box.value
end
