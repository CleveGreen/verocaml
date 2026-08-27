type t =
  | Ground
  | Proxy of < child : t >
