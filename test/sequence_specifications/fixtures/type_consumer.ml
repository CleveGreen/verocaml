let identity (sequence : 'a Seq.t) : 'a Seq.t = sequence

let keep_two (integers : int Seq.t) (booleans : bool Seq.t) =
  (identity integers, identity booleans)
