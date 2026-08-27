external production_default_threads : int -> int
  = "verocaml_default_threads"

module Core_pairs = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let validate_with_max ~max_domains threads =
  if threads <= 0 then Error "threads must be a positive integer"
  else if threads > max_domains then
    Error
      (Printf.sprintf "threads=%d exceeds runtime maximum %d" threads
         max_domains)
  else Ok ()

let validate_threads threads =
  validate_with_max ~max_domains:(Multicore.max_domains ()) threads

let default_threads_with ~max_domains ~affinity ~topology =
  let fallback = max 1 max_domains in
  if max_domains <= 0 then 1
  else
    match affinity () with
    | Error _ -> min max_domains fallback
    | Ok [] -> min max_domains fallback
    | Ok cpus ->
        let rec collect pairs = function
          | [] ->
              let count = Core_pairs.cardinal pairs in
              if count <= 0 then min max_domains fallback
              else min max_domains count
          | cpu :: rest -> (
              match topology cpu with
              | Error _ -> min max_domains fallback
              | Ok pair -> collect (Core_pairs.add pair pairs) rest)
        in
        collect Core_pairs.empty cpus

let default_threads () =
  production_default_threads (Multicore.max_domains ())

module For_testing = struct
  let default_threads_with = default_threads_with
end
