type command = {
  program : string;
  arguments : string list;
  forwarded : (string * string) list;
  cleanup_paths : string list;
  adjacency : (string * string * string) list;
}

val run : cwd:string -> command -> (Outcome.t, Failure.t) result
