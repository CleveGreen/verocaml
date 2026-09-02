type observation = {
  identity_digest : string;
  presentation_name : string;
  backend_head : string;
}

val with_forced_presentation_name :
  declaration_names:string list ->
  presentation_name:string ->
  (unit -> 'a) ->
  'a * observation list

val select_presentation_name :
  declaration_name:string -> default:string -> string

val observe_backend_head :
  declaration_name:string ->
  identity_digest:string ->
  presentation_name:string ->
  backend_head:string ->
  unit
