type wrapper = W of int [@@unboxed]

let unwrap (W value) = value
