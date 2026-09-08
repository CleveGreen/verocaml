[@@@verocaml.verify]
type t = int

let unsigned (x : t) : Int.t =
  if x >= 0 then x + 0 else x + (65_536 * 65_536 * 65_536 * 65_536)
[@@verocaml.spec]

let unsigned_range (x : t) =
  [%verocaml.ensures fun _ -> 0 <= unsigned x && unsigned x < (65_536 * 65_536 * 65_536 * 65_536)];
  ()
[@@verocaml.proof]

let signed (x : t) : Int.t = x + 0
[@@verocaml.spec]

let signed_relation (x : t) =
  [%verocaml.ensures fun _ -> signed x =
    (if unsigned x < (32_768 * 65_536 * 65_536 * 65_536) then unsigned x
     else unsigned x - (65_536 * 65_536 * 65_536 * 65_536))];
  ()
[@@verocaml.proof]

let fits_signed (x : Int.t) =
  -(32_768 * 65_536 * 65_536 * 65_536) <= x && x < (32_768 * 65_536 * 65_536 * 65_536)
[@@verocaml.spec]

let bounds_relation (x : Int.t) =
  [%verocaml.ensures fun _ -> fits_signed x =
    (-(32_768 * 65_536 * 65_536 * 65_536) <= x && x < (32_768 * 65_536 * 65_536 * 65_536))];
  ()
[@@verocaml.proof]

let identity (x : t) =
  [%verocaml.ensures fun result -> result = signed x];
  x
