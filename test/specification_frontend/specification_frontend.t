Default PPX compilation erases the contiguous specification prefix. The
rewritten source remains stable and contains no executable annotation code.
This is the local `ppx-erasure-render-ratchet` specialist exception; it is not
part of `specification-frontend-outcome-check`.

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/rewrite.ml > artifacts/rewrite.stdout 2> artifacts/rewrite.actual
  $ test ! -s artifacts/rewrite.stdout
  $ diff -u golden/rewrite.expected artifacts/rewrite.actual

Erased specifications neither execute nor require the ghost runtime. The
resulting CMT contains no ghost calls.
This is the local `erased-cmt-runtime-ratchet` specialist exception.

  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe fixtures/runtime_and_cmt.ml -o artifacts/runtime_erased.exe
  $ artifacts/runtime_erased.exe
  $ ./specification_frontend_tool.exe erased fixtures/runtime_and_cmt.ml fixtures/runtime_and_cmt.cmt
  erased CMT inspection passed

The explicit verification flag retains typed proof sidecars. Their resolved
ghost calls, original extension spans, and payload spans survive in the CMT,
while runtime execution remains a no-op.
This is the local `retained-sidecar-authentication-ratchet` specialist
exception. Stable verification behavior is covered separately by
`specification-frontend-outcome-check`.

  $ ocamlc -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" ../../runtime/vero_ghost.cma fixtures/runtime_and_cmt.ml -o artifacts/runtime_retained.exe
  $ artifacts/runtime_retained.exe
  $ ./specification_frontend_tool.exe retained fixtures/runtime_and_cmt.ml fixtures/runtime_and_cmt.cmt
  retained CMT inspection passed
