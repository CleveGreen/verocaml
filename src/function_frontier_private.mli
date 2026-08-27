type 'a candidate

type 'a selection = {
  ready : 'a candidate list;
  blocked : 'a candidate list;
  waiting : 'a candidate list;
}

val candidate :
  source_ordinal:int -> dependency_ordinals:int list -> 'a -> 'a candidate

val select :
  completed_ordinals:int list ->
  is_blocked:('a -> bool) ->
  is_waiting:('a -> bool) ->
  'a candidate list ->
  ('a selection, string) result

val value : 'a candidate -> 'a
val source_ordinal : 'a candidate -> int

module For_testing : sig
  val dependency_ordinals : 'a candidate -> int list
end
