Verified product output retains the intentional success colour when colour is
forced. Semantic verification is covered by the grouped release outcome suite.

  $ OCAML_COLOR=always ../../../src/verocaml.exe verify ../../release_verification/fixtures/pure_verified.ml --timeout-ms 60000 | od -An -tx1 -v | tr -d ' \n' | grep -q '1b5b313b33326d'; echo $?
  0
