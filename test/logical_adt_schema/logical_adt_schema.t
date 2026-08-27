The solver-neutral schema and worker-local native datatype reconstruction obey
ordinary sorted equality, injectivity, discrimination, and recursion.

  $ ./logical_adt_schema_tool.exe
  abstract-injectivity=verified
  false-supported-identity=verified
  ordinary-failed-vc=counterexample
  recursive-discrimination=verified
  detached-native-reconstruction=verified
  nonuniform-recursion=rejected

The accepted source forms verify without enumerating closed members.  A false
but supported identity reaches an ordinary failed VC.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/abstract_payload.ml --threads 1 --timeout-ms 5000
  verocaml: verified file=fixtures/abstract_payload.ml functions=1 obligations=1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/nullary_discrimination.ml --threads 2 --timeout-ms 5000
  verocaml: verified file=fixtures/nullary_discrimination.ml functions=1 obligations=1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/recursive_list.ml --threads 1 --timeout-ms 5000
  verocaml: verified file=fixtures/recursive_list.ml functions=1 obligations=1
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/false_identity.ml --threads 1 --timeout-ms 5000 > false.out 2>&1 || code=$?; test "$code" = 1; grep -o 'counterexample' false.out | head -1
  counterexample

  $ root="${PWD%%/_build/*}"; python3 architecture_check.py "$root"
  private-pairs=present loc-caps=pass
  backend-handles=worker-local
