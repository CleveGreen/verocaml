val validate_threads : int -> (unit, string) result
val default_threads : unit -> int

module For_testing : sig
  val default_threads_with :
    max_domains:int ->
    affinity:(unit -> (int list, string) result) ->
    topology:(int -> (int * int, string) result) ->
    int
end
