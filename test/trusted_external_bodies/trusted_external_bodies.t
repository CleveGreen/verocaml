Ordinary trusted-body outcomes live in outcome_cases.ml and selected diagnostic
wording lives in test/ui_goldens/trusted_external_bodies. This specialist target
retains only private provenance, unchecked-body, replay, suppression, and
ordinary-erasure contracts from test/support/migrations/w09.md.

Raw semantic/proof provenance and wrong-stage private call forms are rejected.

  $ ./trusted_external_bodies_tool.exe structural >/dev/null
  $ echo trusted-body-private-shapes=checked
  trusted-body-private-shapes=checked

Ordinary and retained artifacts preserve the implementation while assigning
trust metadata only to the retained family.

  $ mkdir artifacts
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erased.cmo fixtures/positive.ml
  $ ./trusted_external_bodies_tool.exe inspect erased artifacts/erased.cmt >/dev/null
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive.cmo fixtures/positive.ml
  $ ./trusted_external_bodies_tool.exe inspect retained artifacts/positive.cmt >/dev/null

The unchecked trusted tail creates no declaration execution or retained
receipt/recursive authority. Exact resource totals are intentionally not a
Cram contract.

  $ retained_proof () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_proof proof_unchecked_tail
  $ ./trusted_external_bodies_tool.exe observe artifacts/proof_unchecked_tail.cmt >/dev/null
  $ retained_proof proof_replay
  $ ./trusted_external_bodies_tool.exe replay artifacts/proof_replay.cmt >/dev/null
  $ retained_proof proof_positive
  $ ./trusted_external_bodies_tool.exe suppress-postconditions artifacts/proof_positive.cmt >/dev/null
  $ echo trusted-body-authority=checked
  trusted-body-authority=checked

Generic trusted Proof bodies retain their type binders and verify as explicit
axioms rather than being rejected as unsupported polymorphism.

  $ retained_proof proof_generic
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/proof_generic.cmt > artifacts/proof-generic.out
  $ grep -q '^verocaml: trusted external body declaration trust=axiomatic mode=proof function=trusted#' artifacts/proof-generic.out
  $ grep -q '^verocaml: verified-with-trusted-axioms ' artifacts/proof-generic.out
  $ echo generic-trusted-proof=verified
  generic-trusted-proof=verified

Trusted Proof bodies can be exported by an ordinary library, but consumer
verification preserves and reports the explicit axiom trust.

  $ mkdir artifacts/import
  $ (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I ../../../../runtime/.vero_ghost.objs/byte -ppx "../../../../ppx/vero_ppx.exe --keep-ghost" -c -o proof_provider.cmo ../../fixtures/proof_provider.ml)
  $ (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I . -I ../../../../runtime/.vero_ghost.objs/byte -ppx "../../../../ppx/vero_ppx.exe --keep-ghost" -c -o proof_consumer.cmo ../../fixtures/proof_consumer.ml)
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/import/proof_consumer.cmt --dependency artifacts/import/proof_provider.cmt > artifacts/import/verified 2>&1
  $ test "$(grep -c '^verocaml: trusted external body .*function=Proof_provider.admit' artifacts/import/verified)" = 1
  $ grep -q '^verocaml: verified-with-trusted-axioms ' artifacts/import/verified
  $ echo imported-trusted-proof=reported-once verification=trusted
  imported-trusted-proof=reported-once verification=trusted

Ordinary Proof compilation erases the combined declaration, calls, interface,
and runtime carrier.

  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/proof_exact_pfc.ml > artifacts/proof-o 2> artifacts/proof-source
  $ test ! -s artifacts/proof-o
  $ if grep -E '^let |Vero_ghost|external_body|proof_definition' artifacts/proof-source >/dev/null; then false; fi
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/proof_erased.cmo fixtures/proof_exact_pfc.ml
  $ ./trusted_external_bodies_tool.exe inspect proof-erased artifacts/proof_erased.cmt >/dev/null
  $ ocamlc -i -ppx ../../ppx/vero_ppx.exe fixtures/proof_exact_pfc.ml > artifacts/proof-interface
  $ if grep '^val ' artifacts/proof-interface >/dev/null; then false; fi
  $ if strings artifacts/proof_erased.cmo | grep -E 'Vero_ghost|external_body|proof_definition|admit|assume' >/dev/null; then false; fi
  $ ocamlopt -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/proof_erased.cmx fixtures/proof_exact_pfc.ml
  $ ocamlc -o artifacts/proof_erased.byte artifacts/proof_erased.cmo
  $ ocamlopt -o artifacts/proof_erased.native artifacts/proof_erased.cmx
  $ artifacts/proof_erased.byte
  $ artifacts/proof_erased.native
