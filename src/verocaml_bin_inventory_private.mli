type role = Root | Dependency

type entry = {
  role : role;
  cmt : string;
  cmi : string;
  cmti : string option;
  vri : string option;
}

val read : string -> (entry list, string) result
