type mutable_box = { mutable value : int }

let expose (box : mutable_box) = box [@@verocaml.spec]
