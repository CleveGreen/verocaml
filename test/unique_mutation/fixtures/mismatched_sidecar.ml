let mismatched (x : int) =
  Vero_ghost.marker "mismatched";
  (if false then
     ignore
       (fun (x : bool @ aliased) ->
         Vero_ghost.sidecar "mismatched";
         Vero_ghost.requires (fun () -> x))
   else ());
  x
