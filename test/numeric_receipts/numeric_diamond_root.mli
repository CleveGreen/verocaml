module Right = Numeric_diamond_right
type right_carrier = Right.carrier

type carrier = Numeric_diamond_left.carrier
[@@verocaml.numeric_carrier
  { profile = "diamond-root-profile";
    representation = "immediate";
    compatibility = [] }]
