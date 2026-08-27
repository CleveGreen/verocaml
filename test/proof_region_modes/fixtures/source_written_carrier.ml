let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let attack value =
  Vero_ghost.marker
    "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=61747461636b|binding=0,0|body=0,0|region=0,0|slots=76616c7565,0,0";
  (if false then
     ignore
       (fun (value : _ @ aliased) ->
         Vero_ghost.sidecar
           "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=61747461636b|binding=0,0|body=0,0|region=0,0|slots=76616c7565,0,0";
         Vero_ghost.proof_region "verocaml:proof-region:2:0:0"
           (fun () -> observe value))
   else ());
  value
