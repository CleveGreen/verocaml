let raw_carrier (x : int) =
  let y = x + 1 in
  Vero_ghost.marker
    "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=7261775f63617272696572|binding=0,0|body=0,0|region=0,0|slots=|kind=local-assert|ordinal=0|predicate=0,0";
  if false then
    ignore
      (fun () ->
        Vero_ghost.sidecar
          "verocaml:proof-region-capture:1:issuer=ppx-v1|callable=7261775f63617272696572|binding=0,0|body=0,0|region=0,0|slots=|kind=local-assert|ordinal=0|predicate=0,0";
        Vero_ghost.proof_region "verocaml:local-assert:1:0:0:0:0:0"
          (fun () -> Vero_ghost.assert_ (fun () -> y > x)))
  else ()
[@@verocaml.proof]
