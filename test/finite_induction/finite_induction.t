The structural manifest measures the actual private finite owners, keeps every
finite module below 800 lines and every substantially rewritten finite
function below 200 lines, authenticates one shared bounded aggregate
observation normalizer at both private solver boundaries, and checks the
private ephemeral tuple planner and body validator without a public unit.

  $ python3 structural_manifest.py "${DUNE_SOURCEROOT:-../..}" | grep -v '^measured-function-ranges='
  finite-owners=finite-domain=finite_value_registry.ml:1-143,direct-candidate=finite_value_registry.ml:145-472,finite-expression=recursive_spec_preservation.ml:1-450,finite-induction=finite_induction_private.ml:1-98,direct-recursion-induction=symbolic_executor_private.ml:2068-2276,finite-result-integration=symbolic_executor_private.ml:2277-2417,immutable-fact-integration=symbolic_executor_private.ml:2473-2709
  legacy-lines=symbolic_executor_private.ml:16651->14528,verification_session.ml:11133->11147,finite_value_registry.ml:4541->2068,aggregate:32325->27743
  measured-functions=385 ranges-sha256=a2ec1541fb13c9afbbe4edd9557dc66935ace58ca8700a6042e134a396c695f0
  shared-checker=ordinary+immutable-recursive-spec frozen-spine=separate
  candidate-lifecycle=singular/all-exits/all-obligations mutual=unsupported
  evaluate=thin-finite-dispatch finite-compiled-units=1 logical-private-units=18 aggregate-normalizer=shared-direct+recursive tuple-match=ephemeral-shared reconstruction=immutable-only fact-relevance=pure-two-consumer retry-demand=shared-exact-view
  installed-manifests=installed-paths.manifest:4be24f090dc87fa4774121dcbe5bfeee2d2b61722f04bc56187fc3cf11cd0a65,installed-interfaces.manifest:7e60ba2fd5c04c15848dea3e3f9e5ce0082bc38353d155da8bac5e982e51904f,installed-public-modules.manifest:c7cd2560be752ccd8dd0f769e8188c153799c982a9000cb6fb37c636de8a11e8

Dedicated integer and structural recursive builders, two smaller calls,
duplicate immutable uses, aliases, copies, projections, and all-finite branches
verify through the shared checker.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/finite_induction_positive.cmo fixtures/finite_induction_positive.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/finite_induction_positive.cmt --timeout-ms 60000 | sed -E 's/functions=[0-9]+ obligations=[0-9]+/functions=N obligations=N/'
  verocaml: verified file=artifacts/finite_induction_positive.cmt functions=N obligations=N

A demanded immutable constructor with one nonfinite child reaches the shared
construction judgment and rejects before result evidence or publication.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonfinite_constructor_child.cmo fixtures/nonfinite_constructor_child.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonfinite_constructor_child.cmt --timeout-ms 60000 > constructor.err 2>&1; test $? = 2; grep -F "finite-expression immutable construction child rejected: one immutable constructor/record child has no exact finite fact" constructor.err >/dev/null; echo shared-constructor=reject-before-result/publication
  shared-constructor=reject-before-result/publication

A feasible result join with one nonfinite branch reaches the same shared owner
and rejects before result evidence or publication.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonfinite_result_branch.cmo fixtures/nonfinite_result_branch.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonfinite_result_branch.cmt --timeout-ms 60000 > branch.err 2>&1; test $? = 2; grep -F "finite-expression feasible result branch rejected: one feasible normal result branch has no finite fact" branch.err >/dev/null; echo shared-branch=reject-before-result/publication
  shared-branch=reject-before-result/publication

Ordinary exact finite results retain the accepted milestone materialization and
mode-transfer behavior for direct return and immutable record/constructor
composition.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/ordinary_direct_return.cmo fixtures/ordinary_direct_return.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/ordinary_direct_return.cmt --timeout-ms 60000 | sed -E 's/functions=[0-9]+ obligations=[0-9]+/functions=N obligations=N/'
  verocaml: verified file=artifacts/ordinary_direct_return.cmt functions=N obligations=N

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/ordinary_push_front.cmo fixtures/ordinary_push_front.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/ordinary_push_front.cmt --timeout-ms 60000 | sed -E 's/functions=[0-9]+ obligations=[0-9]+/functions=N obligations=N/'
  verocaml: verified file=artifacts/ordinary_push_front.cmt functions=N obligations=N
