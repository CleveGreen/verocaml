let int_id (value : int) = Provider.id value
let bool_id (value : bool) = Provider.id value

let labels (value : bool) =
  Provider.labelled ~second:false ~flag:true ~first:value

let omitted () = Provider.pick 7 ()
let supplied () = Provider.pick 7 ~value:9 ()
let forwarded (value : int option) = Provider.pick 7 ?value ()
let option (value : int option) = Provider.copy_option value
let result (value : (bool, int) result) = Provider.copy_result value
let list_length (values : int list) = Provider.length values
let list_append (values : int list) = Provider.append values []
let box value =
  let box = Provider.pass_box { Provider.value = value } in
  box.Provider.value

let tree value =
  let constructed =
    Provider.Node (value, Provider.Leaf, Provider.Leaf)
  in
  match Provider.pass_tree constructed with
  | Provider.Leaf -> 0
  | Provider.Node (_, _, _) -> 1

let tree_size (value : int Provider.tree) = Provider.tree_size value

let logical (value : int) = Provider.same value
[@@verocaml.spec]

let proof (value : int) = Provider.observe value
[@@verocaml.proof]
