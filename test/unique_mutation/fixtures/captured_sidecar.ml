let captured (x : int) =
  let live = x in
  Vero_ghost.marker "captured";
  (if false then
     ignore
       (fun (x : _ @ aliased) ->
         Vero_ghost.sidecar "captured";
         Vero_ghost.requires (fun () -> live >= x))
   else ());
  x
