type t = {
  timeout_ms : int;
  rlimit : int;
}

type error =
  | Nonpositive_timeout of int
  | Nonpositive_rlimit of int

let default_rlimit = 3_000_000

let create ~timeout_ms ~rlimit =
  if timeout_ms <= 0 then Error (Nonpositive_timeout timeout_ms)
  else if rlimit <= 0 then Error (Nonpositive_rlimit rlimit)
  else Ok { timeout_ms; rlimit }

let create_default ~timeout_ms =
  create ~timeout_ms ~rlimit:default_rlimit

let timeout_ms policy = policy.timeout_ms
let rlimit policy = policy.rlimit

let error_to_string = function
  | Nonpositive_timeout timeout_ms ->
      Printf.sprintf "solver timeout must be positive, found %d" timeout_ms
  | Nonpositive_rlimit rlimit ->
      Printf.sprintf "solver rlimit must be positive, found %d" rlimit
