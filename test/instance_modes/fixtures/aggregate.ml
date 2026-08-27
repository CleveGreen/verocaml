type packet = { run : int; mutable ghost : int [@ghost]; mutable tracked : int [@tracked] }
type choice = C of int * (int [@ghost]) * (int [@tracked])
let make x = { run = x; ghost = (x [@ghost]); tracked = (x [@tracked]) }
let read p =
  let { run; ghost = (g [@ghost]); tracked = (t [@tracked]) } = p in
  let[@ghost] _g = (p.ghost [@ghost]) in
  let[@tracked] _t = (p.tracked [@tracked]) in
  ((p.ghost <- (((g [@ghost]) + 1) [@ghost])) [@ghost]);
  ((p.tracked <- (((t [@tracked]) + 1) [@tracked])) [@tracked]);
  run
let make_c x = C (x, (x [@ghost]), (x [@tracked]))
let read_c c = match c with C (x, (g [@ghost]), (t [@tracked])) ->
  let[@ghost] _gg = (g [@ghost]) in let[@tracked] _tt = (t [@tracked]) in x
