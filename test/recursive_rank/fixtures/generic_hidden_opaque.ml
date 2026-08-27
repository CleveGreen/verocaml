module type BOX = sig
  type t
end

module Box : BOX = struct
  type 'a hidden =
    | Hidden_ground
    | Hidden_value of 'a

  type t = int
end
