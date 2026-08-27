let () =
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    Parallel_kernel.for_
      parallel
      ~start:0
      ~stop:1
      ~f:(fun _parallel _index ->
        Z3.set_global_param "model" "true"));
  Parallel_scheduler.stop scheduler
