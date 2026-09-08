type carrier = int
[@@verocaml.numeric_carrier
  { profile = "collision-import-profile";
    representation = "immediate";
    compatibility = [] }]

val law : carrier -> int
[@@verocaml.spec]
