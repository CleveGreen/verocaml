type artifact = {
  source : string;
  cmt : string;
}

type project = {
  root : string;
  requested_directory : string;
  artifacts : artifact list;
}

val build_and_describe : string -> (project, string) result
