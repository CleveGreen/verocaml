let use value =
  let[@ghost] _observed =
    (Signatures.observe (value [@ghost]) [@ghost])
  in
  let[@tracked] _tracked =
    (Signatures.tracked_id (value [@tracked]) [@tracked])
  in
  let[@ghost] _erased =
    (Signatures.erased_exec (value [@ghost]) [@ghost])
  in
  let packet : Signatures.packet =
    { run = value; ghost = (value [@ghost]) }
  in
  let { Signatures.run; ghost = (_ [@ghost]) } = packet in
  let choice : Signatures.choice =
    Signatures.C (run, (value [@tracked]))
  in
  match choice with
  | Signatures.C (runtime, (_ [@tracked])) -> runtime
