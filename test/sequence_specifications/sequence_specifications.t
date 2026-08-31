This manual specialist check covers the direct-source CLI boundary and ordinary
abstract sequence interface. Semantic verification lives in the grouped Dune
fixtures, while public cross-unit use through verocaml.vstd and Vstd.Seq is
covered separately by vstd_integration.

  $ mkdir -p artifacts/ordinary
  $ cat ../../library/seq.ml fixtures/positive_client.ml > artifacts/seq.ml
  $ for threads in 1 2; do timeout --foreground --signal=TERM --kill-after=5s 60s env OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/seq.ml --threads "$threads" --timeout-ms 20000 >/dev/null 2>&1 || exit 1; done
  $ test ! -e artifacts/seq.cmi && test ! -e artifacts/seq.cmt

Ordinary clients can use the abstract sequence type but cannot construct a
value. Compiler prose and inferred-interface rendering are not pinned.

  $ timeout --foreground --signal=TERM --kill-after=5s 60s ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/seq.cmi ../../library/seq.mli
  $ timeout --foreground --signal=TERM --kill-after=5s 60s ocamlc -w -A -alert -all -bin-annot -I artifacts/ordinary -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/seq.cmo ../../library/seq.ml
  $ ocamlc -w -A -alert -all -I artifacts/ordinary -c -o artifacts/ordinary/type_consumer.cmo fixtures/type_consumer.ml
  $ if ocamlc -w -A -alert -all -I artifacts/ordinary -c -o artifacts/ordinary/private_token.cmo fixtures/private_token_consumer.ml >/dev/null 2>&1; then exit 1; fi
  $ test ! -e artifacts/ordinary/private_token.cmo
