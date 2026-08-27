let () =
  let open Provider in
  Printf.printf "%d %b %d %d %d %d\n"
    (id 7) (labelled ~second:false ~flag:true ~first:true)
    (pick 7 ()) (pick 7 ~value:9 ())
    (length [ 1; 2; 3 ])
    (tree_size (Node (1, Node (2, Leaf, Leaf), Leaf)))
