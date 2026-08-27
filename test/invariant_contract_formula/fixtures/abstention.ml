[%%verocaml.symbolic val observed : int -> int]

let quantified () =
  [%verocaml.ensures fun _ ->
    forall (fun (candidate : int) ->
      ((observed candidate) [@trigger]) = observed candidate)];
  ()

type cell = { mutable value : int }

let old_value (cell : cell @ unique) : cell @ unique =
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old cell.value]];
  cell

let old_integer (value : int) =
  [%verocaml.ensures fun result -> result = [%verocaml.old value]];
  value

let callback (callback : int -> int) (value : int) =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.ensures fun result -> call_ensures (callback value) result];
  callback value

let proof_region (value : int) =
  [%verocaml.requires observed value = observed value]; [%verocaml.ensures fun result -> result = value];
  [%verocaml.proof [%verocaml.assert value = value]];
  value
