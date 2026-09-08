type carrier = int
[@@verocaml.numeric_carrier
  { profile = "diamond-profile";
    representation = "immediate";
    compatibility = [] }]

val law : carrier -> int
[@@verocaml.spec]
