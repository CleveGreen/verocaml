open Vstd

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type 'a option = None | Some of 'a

type token = Stop | Amount of Int.t

type holder = { token : token }

let spec_is_none (value : Int.t option) : bool =
  match value with None -> true | Some _ -> false
[@@verocaml.spec]

let spec_opt_eq (left : Int.t option) (right : Int.t option) : bool =
  match left with
  | None -> spec_is_none right
  | Some left_value ->
      (match right with
       | None -> false
       | Some right_value -> left_value = right_value)
[@@verocaml.spec]

let spec_index (_idx : Int.t) (_nodes : Int.t list) : Int.t option = None
[@@verocaml.spec]

let token_value (value : token) : Int.t =
  match value with Stop -> 0 | Amount amount -> amount
[@@verocaml.spec]

let rec lemma_index_oob (idx : Int.t) (nodes : Int.t list) : unit =
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
