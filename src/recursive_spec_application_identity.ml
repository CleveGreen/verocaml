type t = {
  issuer : unit ref;
  token : unit ref;
  ordinal : int;
}

let private_issuer = ref ()
let next_ordinal = ref 0

let issue () =
  let ordinal = !next_ordinal in
  incr next_ordinal;
  { issuer = private_issuer; token = ref (); ordinal }

let authenticate identity =
  identity.issuer == private_issuer
  && identity.token != private_issuer
  && identity.ordinal >= 0

let same left right =
  authenticate left && authenticate right && left.token == right.token
  && left.ordinal = right.ordinal

let ordinal identity = if authenticate identity then identity.ordinal else -1

module For_testing = struct
  let forged () = { issuer = ref (); token = ref (); ordinal = 0 }
end
