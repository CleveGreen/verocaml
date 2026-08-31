The schema-owner summary, repeatability probe, and retained-payload scan are
explicit architecture/resource exceptions.  They guard one generic body with
no clone expansion and absence of process-private capability strings; ordinary
verification status and functions live in the grouped outcome host.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained positive_finite_formal
  $ ./parametric_rank_domain_tool.exe summary artifacts/positive_finite_formal.cmt
  parametric-adts=1 generic-functions=3 clones=0
  $ ./parametric_rank_domain_tool.exe repeat artifacts/positive_finite_formal.cmt | sed -E 's/ bytes=[0-9]+/ bytes=N/'
  repeat-equal=true bytes=N

  $ retained positive_retained_provider
  $ if strings artifacts/positive_retained_provider.cmt | grep -E 'validated_rank_domain|schema_capability|finite receipt'; then exit 1; fi

Mutual schemas and tuple actuals are explicit specialist outcome-gap
exceptions.  The CLI supplies VERO_INVALID_PROGRAM while the direct VERO-113
verifier seam reports an unprojected verifier failure; these checks also retain
the pre-session/no-dump invariant and do not block parametric-rank-domain-outcome-check.

  $ for n in negative_mutual_scc negative_tuple_actual; do retained "$n"; done
  $ reject () { n=$1; expected=$2; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >"artifacts/$n.out" 2>&1 || code=$?; test "$code" = 2; actual=$(grep -Eo 'VERO_[A-Z_]+' "artifacts/$n.out" | head -1); test "$actual" = "$expected"; printf '%s: code=%s\n' "$n" "$actual"; test "$(grep -c 'Verification_session: private receipt ' "artifacts/$n.out")" = 0; test ! -e "artifacts/$n.sst"; test ! -e "artifacts/$n.vir"; }
  $ for spec in 'negative_mutual_scc VERO_INVALID_PROGRAM' 'negative_tuple_actual VERO_INVALID_PROGRAM'; do set -- $spec; reject "$1" "$2"; done
  negative_mutual_scc: code=VERO_INVALID_PROGRAM
  negative_tuple_actual: code=VERO_INVALID_PROGRAM
