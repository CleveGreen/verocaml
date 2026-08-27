Default PPX compilation erases the contiguous specification prefix. The
rewritten source remains stable and contains no executable annotation code.

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/rewrite.ml > artifacts/rewrite.stdout 2> artifacts/rewrite.actual
  $ test ! -s artifacts/rewrite.stdout
  $ diff -u golden/rewrite.expected artifacts/rewrite.actual

Erased specifications neither execute nor require the ghost runtime. The
resulting CMT contains no ghost calls.

  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe fixtures/runtime_and_cmt.ml -o artifacts/runtime_erased.exe
  $ artifacts/runtime_erased.exe
  $ ./specification_frontend_tool.exe erased fixtures/runtime_and_cmt.ml fixtures/runtime_and_cmt.cmt
  erased CMT inspection passed

The explicit verification flag retains typed proof sidecars. Their resolved
ghost calls, original extension spans, and payload spans survive in the CMT,
while runtime execution remains a no-op.

  $ ocamlc -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" ../../runtime/vero_ghost.cma fixtures/runtime_and_cmt.ml -o artifacts/runtime_retained.exe
  $ artifacts/runtime_retained.exe
  $ ./specification_frontend_tool.exe retained fixtures/runtime_and_cmt.ml fixtures/runtime_and_cmt.cmt
  retained CMT inspection passed

Malformed payloads and misplaced or unsupported annotations fail at their
source spans. Color is disabled only on each compiler command whose diagnostic
text is asserted.

  $ ppx_failure () { name=$1; fragment=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o "artifacts/$name.cmo" "fixtures/$name.ml" > "artifacts/$name.error" 2>&1; then echo "unexpected PPX success: $name"; return 1; fi; grep -F 'File "' "artifacts/$name.error" >/dev/null && grep -F ', line ' "artifacts/$name.error" >/dev/null && grep -F ', characters ' "artifacts/$name.error" >/dev/null && grep -F "$fragment" "artifacts/$name.error" >/dev/null; }
  $ ppx_failure invalid_empty "%verocaml.requires expects exactly one expression payload"
  $ ppx_failure invalid_pattern "%verocaml.assert expects exactly one expression payload"
  $ ppx_failure misplaced_top_level "%verocaml.requires is only valid"
  $ ppx_failure misplaced_noncontiguous "%verocaml.ensures is only valid"
  $ ppx_failure missing_body "%verocaml.decreases must be followed"
  $ ppx_failure old_outside_ensures "%verocaml.old is only valid inside"
  $ ppx_failure old_in_requires "%verocaml.old is only valid inside"
  $ ppx_failure may_diverge "unsupported verocaml extension %verocaml.may_diverge"
  $ ppx_failure assert_top_level "%verocaml.assert is only valid"
  $ ppx_failure assert_noncontiguous "%verocaml.assert is only valid"
