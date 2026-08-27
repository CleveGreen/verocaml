type shade = Light | Dark

type sample = {
  amount : int;
  shade : shade;
}

let make_sample (amount : int) : sample = { amount; shade = Light }
[@@verocaml.spec]

let amount (sample : sample) : int = sample.amount [@@verocaml.spec]

let classify (sample : sample) : int =
  match sample.shade with
  | Light -> sample.amount
  | Dark -> 0
[@@verocaml.spec]

let pair (sample : sample) : sample * int = (sample, sample.amount)
[@@verocaml.spec]

let sum_pair (pair : sample * int) : int =
  let sample, extra = pair in
  sample.amount + extra
[@@verocaml.spec]

let verified (n : int) : int =
  [%verocaml.requires n >= 0];
  [%verocaml.assert classify (make_sample n) = n];
  [%verocaml.assert sum_pair (pair (make_sample n)) = n + n];
  [%verocaml.ensures fun result -> result = n];
  n
