open Generic_family_provider

let select_sibling (value : bool snapshot) =
  match value with
  | End -> false
  | More (head, _) -> head
