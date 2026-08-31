The solver-neutral schema and worker-local native datatype reconstruction obey
ordinary sorted equality, injectivity, discrimination, and recursion.
This direct solver and private-schema probe is the
`logical-adt-schema-specialist-check` architecture exception.

  $ ./logical_adt_schema_tool.exe
  abstract-injectivity=verified
  false-supported-identity=verified
  ordinary-failed-vc=counterexample
  recursive-discrimination=verified
  detached-native-reconstruction=verified
  nonuniform-recursion=rejected
