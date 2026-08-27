(* Every invocation creates its own context; the tuple returned by [solve_one]
   contains only portable primitive data.  No Z3 handle is captured or returned. *)
let solve_one () : bool * string =
  let context = Z3.mk_context [ "model", "true" ] in
  let solver = Z3.Solver.mk_solver_s context "QF_LIA" in
  Fun.protect
    ~finally:(fun () -> Z3.Solver.reset solver)
    (fun () ->
      let params = Z3.Params.mk_params context in
      Z3.Params.add_bool params (Z3.Symbol.mk_string context "model") true;
      Z3.Params.add_int params (Z3.Symbol.mk_string context "timeout") 1000;
      Z3.Solver.set_parameters solver params;
      let zero = Z3.Arithmetic.Integer.mk_numeral_s context "0" in
      let one = Z3.Arithmetic.Integer.mk_numeral_s context "1" in
      let assertion = Z3.Arithmetic.mk_lt context zero one in
      Z3.Solver.add solver [ assertion ];
      let sat = Z3.Solver.check solver [] = Z3.Solver.SATISFIABLE in
      sat, Z3.Expr.to_string assertion)

let record_domain seen domain =
  let bit = 1 lsl domain in
  let rec loop () =
    let old = Atomic.get seen in
    if not (Atomic.compare_and_set seen old (old lor bit)) then loop ()
  in
  loop ()

let () =
  let successes = Atomic.make 0 in
  let worker_domains = Atomic.make 0 in
  let scheduler = Parallel_scheduler.create ~max_domains:3 () in
  Fun.protect
    ~finally:(fun () -> Parallel_scheduler.stop scheduler)
    (fun () ->
      Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
        Parallel_kernel.for_
          parallel
          ~start:0
          ~stop:256
          ~f:(fun _parallel _index ->
            let sat, rendered = solve_one () in
            if not sat || rendered <> "(< 0 1)" then failwith "local Z3 solve";
            if _index land 31 = 0 then Gc.minor ();
            let domain = Multicore.current_domain () in
            if domain <> 0 then record_domain worker_domains domain;
            ignore (Atomic.fetch_and_add successes 1))));
  let workers = Atomic.get worker_domains in
  if Atomic.get successes <> 256 then failwith "lost worker result";
  (* ids 1 and 2 are the two worker domains in this three-domain scheduler *)
  if workers land 0b110 <> 0b110 then failwith "two worker domains did not solve";
  Gc.full_major ();
  Gc.full_major ();
  Printf.printf "portable-z3-contexts=ok solves=%d worker_domain_mask=%d results=primitive\n%!"
    (Atomic.get successes) workers
