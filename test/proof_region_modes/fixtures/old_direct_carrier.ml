let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let attack value =
  Vero_ghost.proof_region "verocaml:proof-region:1:0:0"
    (fun () -> observe value);
  value
