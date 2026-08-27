let stop_once scheduler stop_count =
  incr stop_count;
  Parallel_scheduler.stop scheduler

let run_normal () =
  let total = Atomic.make 0 in
  let nonzero_domain = Atomic.make false in
  let immutable_payload = (20, 22) in
  let stop_count = ref 0 in
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  Fun.protect
    ~finally:(fun () -> stop_once scheduler stop_count)
    (fun () ->
      Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
        Parallel_kernel.for_
          parallel
          ~start:0
          ~stop:100_000
          ~f:(fun _parallel index ->
            if Multicore.current_domain () <> 0
            then Atomic.set nonzero_domain true;
            let left, right = immutable_payload in
            if left + right <> 42 then failwith "immutable payload changed";
            ignore (Atomic.fetch_and_add total (index land 1)))));
  if not (Parallel_scheduler.is_stopped scheduler)
  then failwith "normal scheduler did not stop";
  if !stop_count <> 1 then failwith "normal scheduler stop count";
  if Atomic.get total <> 50_000 then failwith "normal work did not fully join";
  if not (Atomic.get nonzero_domain)
  then failwith "normal work never ran on a nonzero domain";
  !stop_count

exception Worker_failure

let run_worker_exception () =
  let completed = Atomic.make 0 in
  let nonzero_domain = Atomic.make false in
  let inject_failure = Atomic.make true in
  let stop_count = ref 0 in
  let scheduler = Parallel_scheduler.create ~max_domains:2 () in
  let observed =
    match
      Fun.protect
        ~finally:(fun () -> stop_once scheduler stop_count)
        (fun () ->
          Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
            Parallel_kernel.for_
              parallel
              ~start:0
              ~stop:100_000
              ~f:(fun _parallel _index ->
                if Multicore.current_domain () <> 0
                then (
                  Atomic.set nonzero_domain true;
                  if Atomic.compare_and_set inject_failure true false
                  then raise Worker_failure);
                ignore (Atomic.fetch_and_add completed 1))))
    with
    | () -> false
    | exception Worker_failure -> true
  in
  if not observed then failwith "worker exception was not reraised";
  if not (Parallel_scheduler.is_stopped scheduler)
  then failwith "exception scheduler did not stop";
  if !stop_count <> 1 then failwith "exception scheduler stop count";
  if Atomic.get completed <= 0 then failwith "exception work did not run";
  if not (Atomic.get nonzero_domain)
  then failwith "exception work never ran on a nonzero domain";
  !stop_count

let () =
  let max_domains = Multicore.max_domains () in
  if max_domains < 2 then failwith "multidomain runtime did not expose two domains";
  let normal_stop_count = run_normal () in
  let exception_stop_count = run_worker_exception () in
  Printf.printf
    "lifecycle=ok max_domains=%d nonzero_domain=true normal_stop_count=%d \
     exception_stop_count=%d worker_exception=observed\n%!"
    max_domains
    normal_stop_count
    exception_stop_count
