type 'a delayed_recursive =
  | Delay of (unit -> 'a delayed_recursive)
