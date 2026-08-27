type 'a option = None | Some of 'a

type token = Stop | Amount of int

type holder = { token : token }

let spec_is_none (value : int option) : bool =
  match value with None -> true | Some _ -> false
[@@verocaml.spec]

let spec_opt_eq (left : int option) (right : int option) : bool =
  match left with
  | None -> spec_is_none right
  | Some left_value ->
      (match right with
       | None -> false
       | Some right_value -> left_value = right_value)
[@@verocaml.spec]

let spec_index (_idx : int) (_nodes : int list) : int option = None
[@@verocaml.spec]

let token_value (value : token) : int =
  match value with Stop -> 0 | Amount amount -> amount
[@@verocaml.spec]

let rec lemma_index_oob (idx : int) (nodes : int list) : unit =
  [%verocaml.decreases nodes];
  match nodes with
  | [] -> ()
  | _value :: rest ->
      lemma_index_oob idx rest;
      [%verocaml.assert
        spec_opt_eq (spec_index (idx - 1) rest) None];
      let direct = Amount idx in
      let copied = direct in
      let record = { token = copied } in
      let projected = record.token in
      (match projected with
       | Stop -> [%verocaml.assert token_value Stop = 0]
       | Amount amount -> [%verocaml.assert token_value (Amount amount) = amount])
[@@verocaml.proof]
