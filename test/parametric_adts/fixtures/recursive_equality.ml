type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let equal (left : int tree) right = left = right
