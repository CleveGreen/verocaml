let () =
  let mutable_session_state = ref 0 in
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    Parallel_kernel.for_
      parallel
      ~start:0
      ~stop:1
      ~f:(fun _parallel _index -> incr mutable_session_state));
  Parallel_scheduler.stop scheduler
