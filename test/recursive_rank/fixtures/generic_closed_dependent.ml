type 'a dependent_container = Only of 'a

type closed_dependent =
  | Closed_base of int dependent_container
  | Closed_next of closed_dependent
