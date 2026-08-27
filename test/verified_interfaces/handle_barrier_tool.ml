let fail format = Printf.ksprintf failwith format

let () =
  if Array.length Sys.argv < 3 then
    fail "usage: %s CONSUMER.cmt DEPENDENCY.cmt..." Sys.argv.(0);
  let constructions = ref 0 in
  let build_handle staged direct transitive =
    incr constructions;
    Instrumented_interface_specification.construct_handle staged direct
      transitive
  in
  match
    Instrumented_interface_specification.authenticate_with_handle_builder
      ~build_handle ~timeout_ms:5000
      ~dependency_files:
        (Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)))
      ~consumer_file:Sys.argv.(1)
  with
  | Ok _ -> fail "expected dependency verification to fail"
  | Error _ ->
      if !constructions <> 0 then
        fail "constructed %d handle(s) before graph-wide success"
          !constructions;
      print_endline "failed authentication constructed 0 handles"
