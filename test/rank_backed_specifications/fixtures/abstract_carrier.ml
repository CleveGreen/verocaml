module type DOMAIN = sig
  type t
end

module Domain : DOMAIN = struct
  type t = Empty | Next of t
end

let identity (value : Domain.t) : Domain.t = value
[@@verocaml.spec]
