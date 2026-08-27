let () =
  let context = Z3.mk_context [] in
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    Parallel_kernel.for_
      parallel
      ~start:0
      ~stop:1
      ~f:(fun _parallel _index ->
        let solver = Z3.Solver.mk_solver_s context "QF_LIA" in
        Z3.Solver.reset solver));
  Parallel_scheduler.stop scheduler
