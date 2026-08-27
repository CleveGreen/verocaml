The schema-rank owner derives concrete int/bool and independently ranked nominal
views lazily while preserving one generic function body and recursive child path.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ for n in positive_structural_decreases positive_finite_formal positive_recursive_child positive_retained_provider; do retained "$n"; done
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive_retained_consumer.cmo fixtures/positive_retained_consumer.ml

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_structural_decreases.cmt --threads 1 --timeout-ms 10000
  verocaml: verified file=artifacts/positive_structural_decreases.cmt functions=0 obligations=3
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_finite_formal.cmt --threads 1 --timeout-ms 10000 --dump-sst artifacts/finite.sst --dump-vir artifacts/finite-1.vir
  verocaml: verified file=artifacts/positive_finite_formal.cmt functions=3 obligations=4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_finite_formal.cmt --threads 2 --timeout-ms 10000 --dump-vir artifacts/finite-2.vir >/dev/null
  $ cmp artifacts/finite-1.vir artifacts/finite-2.vir
  $ grep '^function finite_refl#' artifacts/finite.sst | sed -E 's/ @ .*//'
  function finite_refl#0 binders=['0@finite_refl#0] mode=proof recursive=true result=unit policy=default-linear/default-z3
  $ test "$(grep -c '^function finite_refl' artifacts/finite.sst)" = 1
  $ ./parametric_rank_domain_tool.exe summary artifacts/positive_finite_formal.cmt
  parametric-adts=1 generic-functions=3 clones=0
  $ ./parametric_rank_domain_tool.exe repeat artifacts/positive_finite_formal.cmt | sed -E 's/ bytes=[0-9]+/ bytes=N/'
  repeat-equal=true bytes=N
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_recursive_child.cmt --timeout-ms 10000
  verocaml: verified file=artifacts/positive_recursive_child.cmt functions=1 obligations=3

Retained data carries portable snapshots rather than live rank capabilities; the
consumer reconstructs and authenticates its own exact program-local domain.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_retained_provider.cmt --timeout-ms 10000
  verocaml: verified file=artifacts/positive_retained_provider.cmt functions=2 obligations=3
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_retained_consumer.cmt --dependency artifacts/positive_retained_provider.cmt --timeout-ms 10000 | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Positive_retained_provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/positive_retained_consumer.cmt functions=1 obligations=0
  $ if strings artifacts/positive_retained_provider.cmt | grep -E 'validated_rank_domain|schema_capability|finite receipt'; then exit 1; fi

Tuple actuals and grouped mutable/function/ref/array/object/abstract/GADT,
nonuniform, mutual, and rebound shapes reject before SST/VIR or receipt work.

  $ for n in negative_abstract_foreign negative_cyclic_gadt_private negative_function_reference_array negative_mutable negative_mutual_scc negative_nonuniform_recursion negative_stale_forged_rebound negative_tuple_actual negative_unknown_payload; do retained "$n"; done
  $ reject () { n=$1; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >"artifacts/$n.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$n"; grep -o 'error\[VERO_[A-Z_]*\]' "artifacts/$n.out"; test "$(grep -c 'private-receipt ' "artifacts/$n.out")" = 0; test ! -e "artifacts/$n.sst"; test ! -e "artifacts/$n.vir"; }
  $ for n in negative_abstract_foreign negative_cyclic_gadt_private negative_function_reference_array negative_mutable negative_mutual_scc negative_nonuniform_recursion negative_stale_forged_rebound negative_tuple_actual negative_unknown_payload; do reject "$n"; done
  negative_abstract_foreign: error[VERO_UNSUPPORTED_STRUCTURE_ITEM]
  negative_cyclic_gadt_private: error[VERO_UNSUPPORTED_AGGREGATE]
  negative_function_reference_array: error[VERO_UNSUPPORTED_TYPE]
  negative_mutable: error[VERO_UNSUPPORTED_AGGREGATE]
  negative_mutual_scc: error[VERO_DEPENDENCY]
  negative_nonuniform_recursion: error[VERO_UNSUPPORTED_AGGREGATE]
  negative_stale_forged_rebound: error[VERO_UNSUPPORTED_TYPE]
  negative_tuple_actual: error[VERO_DEPENDENCY]
  negative_unknown_payload: error[VERO_UNSUPPORTED_TYPE]

The architecture gate enforces the sole owner and concentration limits.

  $ python3 architecture_check.py ../..
  owner-lines=650 interface-lines=81 single-owner=true
