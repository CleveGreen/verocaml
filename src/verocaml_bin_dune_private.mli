type artifact = {
  source : string;
  cmt : string;
  cmi : string option;
  cmti : string option;
  vri : string option;
  requested : bool;
}

type project = {
  root : string;
  requested_directory : string;
  artifacts : artifact list;
}

type error =
  | Cli_error of string
  | Dependency_error of {
      provider : string option;
      reason_class : string;
    }

val build_and_describe : string -> (project, error) result
