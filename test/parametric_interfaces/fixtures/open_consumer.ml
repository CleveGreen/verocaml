let relay value = Provider.id value
let relay_tree tree = Provider.pass_tree tree

let relay_box box = (Provider.pass_box box).Provider.value

let wrap_tree value tree =
  Provider.pass_tree (Provider.Node (value, tree, Provider.Leaf))

let classify_tree tree =
  match tree with Provider.Leaf -> false | Provider.Node _ -> true
