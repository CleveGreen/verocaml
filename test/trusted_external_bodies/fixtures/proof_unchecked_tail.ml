let trusted (flag : bool) : unit =
  [%verocaml.ensures fun _ -> false];
  let mutable counter = 0 in
  counter <- counter + 1;
  print_int counter;
  if flag then raise Exit;
  let rec loop () = loop () in
  loop ()
[@@verocaml.external_body]
[@@verocaml.proof]

let caller (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  trusted false
[@@verocaml.proof]
