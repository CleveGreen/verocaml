type classification = Ordinary | Authenticated | Malformed

let public_name = "verocaml.external_type_specification"
let retained_name = "verocaml.internal.external_type_specification.v1"

let classify attributes =
  let relevant =
    List.filter
      (fun attribute ->
        String.equal attribute.Parsetree.attr_name.txt public_name
        || String.equal attribute.attr_name.txt retained_name)
      attributes
  in
  match relevant with
  | [] -> Ordinary
  | [ attribute ]
    when String.equal attribute.attr_name.txt retained_name
         && attribute.attr_loc.loc_ghost
         && attribute.attr_name.loc.loc_ghost
         && attribute.attr_payload = Parsetree.PStr [] ->
      Authenticated
  | [ _ ] | _ :: _ :: _ -> Malformed
