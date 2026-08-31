These malformed-SST barriers are explicit specialist outcome-gap exceptions.
VERO-113 reports them as verifier failures rather than projected outcomes, so
the exact rejection phase remains in Cram and cannot block
finite-induction-outcome-check.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonfinite_constructor_child.cmo fixtures/nonfinite_constructor_child.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonfinite_constructor_child.cmt --timeout-ms 60000 > constructor.err 2>&1; test $? = 2; grep -F "finite-expression immutable construction child rejected: one immutable constructor/record child has no exact finite fact" constructor.err >/dev/null; echo shared-constructor=reject-before-result/publication
  shared-constructor=reject-before-result/publication

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nonfinite_result_branch.cmo fixtures/nonfinite_result_branch.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/nonfinite_result_branch.cmt --timeout-ms 60000 > branch.err 2>&1; test $? = 2; grep -F "finite-expression feasible result branch rejected: one feasible normal result branch has no finite fact" branch.err >/dev/null; echo shared-branch=reject-before-result/publication
  shared-branch=reject-before-result/publication
