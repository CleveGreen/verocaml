The verification client is concatenated with the provider because symbolic
declarations are same-unit-only.  The retained artifact and direct source
routes therefore verify the same complete Seq unit.

  $ mkdir -p artifacts/source artifacts/cmt artifacts/negative artifacts/ordinary artifacts/tmp
  $ cat ../../library/seq.ml fixtures/positive_client.ml > artifacts/source/seq.ml
  $ cp artifacts/source/seq.ml artifacts/cmt/seq.ml
  $ cat ../../library/seq.ml fixtures/negative_invalid_client.ml > artifacts/negative/seq.ml
  $ retained () { source=$1; output=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "$output.cmo" "$source"; }
  $ retained artifacts/cmt/seq.ml artifacts/cmt/seq
  $ retained artifacts/negative/seq.ml artifacts/negative/seq

Both routes, both thread counts, and repeated runs preserve exact VIR and
output.  Retained-CMT SST is byte-stable.  Direct-source SST is compared after
normalizing private compilation identities, interface digests, and mode
snapshot digests; all other source SST content is stable.

  $ verify_positive () { route=$1; input=$2; threads=$3; repeat=$4; prefix="artifacts/$route.$threads.$repeat"; timeout --foreground --signal=TERM --kill-after=2s 45s env TMPDIR="$PWD/artifacts/tmp" OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 20000 --dump-sst "$prefix.sst" --dump-vir "$prefix.vir" >"$prefix.out" 2>"$prefix.err"; }
  $ normalize_source_sst () { sed -E 's/ identity=[0-9a-f]+//g; s/interface=[0-9a-f]+/interface=<digest>/; s/snapshot=[0-9a-f]+/snapshot=<digest>/g' "$1"; }
  $ for route in source cmt; do if test "$route" = source; then input=artifacts/source/seq.ml; else input=artifacts/cmt/seq.cmt; fi; for threads in 1 2; do for repeat in 1 2; do verify_positive "$route" "$input" "$threads" "$repeat"; normalize_source_sst "artifacts/$route.$threads.$repeat.sst" > "artifacts/$route.$threads.$repeat.norm.sst"; done; cmp "artifacts/$route.$threads.1.norm.sst" "artifacts/$route.$threads.2.norm.sst"; cmp "artifacts/$route.$threads.1.vir" "artifacts/$route.$threads.2.vir"; cmp "artifacts/$route.$threads.1.out" "artifacts/$route.$threads.2.out"; cmp "artifacts/$route.$threads.1.err" "artifacts/$route.$threads.2.err"; done; cmp "artifacts/$route.1.1.norm.sst" "artifacts/$route.2.1.norm.sst"; cmp "artifacts/$route.1.1.vir" "artifacts/$route.2.1.vir"; cmp "artifacts/$route.1.1.out" "artifacts/$route.2.1.out"; cmp "artifacts/$route.1.1.err" "artifacts/$route.2.1.err"; done
  $ cmp artifacts/cmt.1.1.sst artifacts/cmt.1.2.sst && cmp artifacts/cmt.1.1.sst artifacts/cmt.2.1.sst
  $ test -z "$(find artifacts/tmp -mindepth 1 -print -quit)"
  $ test ! -e artifacts/source/seq.cmi && test ! -e artifacts/source/seq.cmt
  $ echo 'positive source+cmt threads=1/2 repeats=stable source-sst=normalized cmt-sst=exact vir=exact-per-route'
  positive source+cmt threads=1/2 repeats=stable source-sst=normalized cmt-sst=exact vir=exact-per-route
  $ tail -1 artifacts/source.1.1.out | sed -E 's#file=[^ ]+#file=<source>#'
  verocaml: verified-with-trusted-axioms file=<source> functions=21 obligations=118 trusted-external-bodies=15 trusted-external-body-uses=3 trusted-external-spec-uses=0
  $ tail -1 artifacts/cmt.1.1.out | sed -E 's#file=[^ ]+#file=<retained-cmt>#'
  verocaml: verified-with-trusted-axioms file=<retained-cmt> functions=21 obligations=118 trusted-external-bodies=15 trusted-external-body-uses=3 trusted-external-spec-uses=0

The stable retained dumps pin the complete logical surface, proof/VC totals,
trusted law declarations, and the three explicit extensionality calls.  The
group itself has fourteen members and intentionally omits extensionality.

  $ sst=artifacts/cmt.1.1.sst; vir=artifacts/cmt.1.1.vir; printf 'sst functions=%s symbolic=%s axiomatic=%s extensionality-calls=%s\n' "$(grep -c '^function ' "$sst")" "$(grep -c 'body symbolic-declaration ' "$sst")" "$(grep -c 'body trusted-external-body trust=axiomatic' "$sst")" "$(grep -c 'proof-call axiom_extensionality#' "$sst")"; printf 'vir proofs=%s obligations=%s\n' "$(grep -c '^function ' "$vir")" "$(grep -c '^  vc ' "$vir")"
  sst functions=74 symbolic=8 axiomatic=15 extensionality-calls=3
  vir proofs=21 obligations=118
  $ awk '/broadcast_group \(group_seq_axioms/{inside=1} inside{print} inside && /^\]\)\]$/{exit}' ../../library/seq.ml > artifacts/group.txt
  $ printf 'group laws=%s extensionality-members=%s explicit-calls=%s\n' "$(grep -c '^  axiom_' artifacts/group.txt)" "$(grep -c 'axiom_extensionality' artifacts/group.txt)" "$(grep -c 'proof-call axiom_extensionality#' "$sst")"
  group laws=14 extensionality-members=0 explicit-calls=3

Invalid init lengths, empty and upper-bound gets, updates, and subranges remain
unconstrained beyond the universal length domain.  Each deliberately false
claim has a concrete model rather than verifying or becoming inconclusive, and
the retained negative route is repeat-stable.

  $ for repeat in 1 2; do prefix="artifacts/negative.$repeat"; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/negative/seq.cmt --threads 1 --timeout-ms 10000 --dump-sst "$prefix.sst" --dump-vir "$prefix.vir" >"$prefix.out" 2>&1 || code=$?; test "$code" = 1; done
  $ cmp artifacts/negative.1.sst artifacts/negative.2.sst && cmp artifacts/negative.1.vir artifacts/negative.2.vir && cmp artifacts/negative.1.out artifacts/negative.2.out
  $ test "$(grep -c '^verocaml: counterexample function=' artifacts/negative.1.out)" = 5
  $ ! grep -q 'inconclusive' artifacts/negative.1.out
  $ grep '^verocaml: counterexample function=' artifacts/negative.1.out | sed -E 's/#[0-9]+/#ID/; s/ span=.* result=/ result=/'
  verocaml: counterexample function=invalid_init_length_is_not_constrained#ID vc=local-assertion[0] result=counterexample
  verocaml: counterexample function=invalid_empty_get_is_not_constrained#ID vc=local-assertion[0] result=counterexample
  verocaml: counterexample function=invalid_upper_get_is_not_constrained#ID vc=local-assertion[0] result=counterexample
  verocaml: counterexample function=invalid_update_is_not_constrained#ID vc=local-assertion[0] result=counterexample
  verocaml: counterexample function=invalid_subrange_is_not_constrained#ID vc=local-assertion[0] result=counterexample

Ordinary PPX output exposes only the covariant private token carrier.  It is
not a list representation: content and length remain entirely symbolic.
External OCaml code can name and pass [Seq.t], but cannot fabricate its token.

  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/seq.cmo ../../library/seq.ml
  $ ocamlc -w -A -alert -all -I artifacts/ordinary -c -o artifacts/ordinary/type_consumer.cmo fixtures/type_consumer.ml
  $ ocamlc -i -I artifacts/ordinary fixtures/type_consumer.ml
  val identity : 'a Seq.t -> 'a Seq.t
  val keep_two : int Seq.t -> bool Seq.t -> int Seq.t * bool Seq.t
  $ ocamlc -i -w -A -alert -all -ppx ../../ppx/vero_ppx.exe ../../library/seq.ml
  type +'a t = private Sequence_token of int
  $ code=0; ocamlc -w -A -alert -all -I artifacts/ordinary -c -o artifacts/ordinary/private_token.cmo fixtures/private_token_consumer.ml >artifacts/ordinary/private_token.out 2>&1 || code=$?; test "$code" = 2; grep -q 'Cannot create values of the private type "int Seq.t"' artifacts/ordinary/private_token.out
  $ test ! -e artifacts/ordinary/private_token.cmo
  $ echo 'ordinary consumers type-use=accepted carrier=private symbolic-content=true'
  ordinary consumers type-use=accepted carrier=private symbolic-content=true
