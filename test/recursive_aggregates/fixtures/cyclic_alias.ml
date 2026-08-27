type tree = Tree of alias
and alias = tree

let make tree = Tree tree
