type contribution = {
  solvers_created : int;
  solver_resets : int;
}

let solver_creations = ref 0
let resets = ref 0

let contribution (telemetry : Z3_bridge.counters) =
  {
    solvers_created = telemetry.solvers_created;
    solver_resets = telemetry.solver_resets;
  }

let commit contribution =
  solver_creations := !solver_creations + contribution.solvers_created;
  resets := !resets + contribution.solver_resets

let account_facade_delta ~(before : Z3_bridge.counters)
    ~(after : Z3_bridge.counters) =
  commit
    {
      solvers_created = after.solvers_created - before.solvers_created;
      solver_resets = after.solver_resets - before.solver_resets;
    }

let solver_creation_count () = !solver_creations
let reset_count () = !resets
let reset_solver_creation_count () = solver_creations := 0
