type leaf = {
  expression : Sst.expression;
  pattern : Sst.pattern;
}

type error

val plan : Sst.expression -> Sst.pattern -> (leaf list, error) result
val error_to_string : error -> string
