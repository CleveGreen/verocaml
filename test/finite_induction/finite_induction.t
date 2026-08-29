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
