The accepted quantifier tranche stays within its fixed resource class and
retains exact scalar, abstract, and canonical generic-ADT structure.

  $ mkdir artifacts
  $ retained () { retained_name=$1; retained_source=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$retained_name.cmo" "$retained_source"; }
  $ extract_case () { awk -v name="$2" '/\(\* COMMON BEGIN \*\)/ { common=1; next } /\(\* COMMON END \*\)/ { common=0; next } common { print } $0 == "(* CASE " name " BEGIN *)" { selected=1; next } $0 == "(* CASE " name " END *)" { selected=0; next } selected { print }' "$1" > "$3"; }
  $ retained quantifiers fixtures/quantifiers.ml
  $ retained negative_quantifier_semantic fixtures/negative_quantifier_semantic.ml
  $ timeout 10 ../../src/verocaml.exe verify fixtures/quantifiers.ml --threads 1 --timeout-ms 5000 --rlimit 100000
  verocaml: verified file=fixtures/quantifiers.ml functions=3 obligations=10
  $ timeout 10 ./verified_callback_traversals_tool.exe structural artifacts/quantifiers.cmt
  sst forall=13 exists=9 triggers=13 binders=int:11,bool:2,param:3,option:2,seq:2,tree:2 outer-free=1 qids=22 skids=22
  vir-logic forall=32 exists=24 triggers=32 outer-free=3 queries=7 portable=7 function-sort=0
  status=verified functions=3 obligations=10 policy=100000/5000 cap=12
  $ timeout 10 ./verified_callback_traversals_tool.exe parity artifacts/quantifiers.cmt
  parity=repeat/threads status=verified functions=3 obligations=10 resources=100000 timeout-ms=5000
  $ timeout 10 ./verified_callback_traversals_tool.exe resource-evidence artifacts/quantifiers.cmt
  resources obligations=10 contexts=10/10 live=0 solvers=10/10 max-live=1 policy=100000/5000
  $ timeout 10 ./verified_callback_traversals_tool.exe semantic-negative artifacts/negative_quantifier_semantic.cmt
  semantic-negative=counterexample functions=7 obligations=7 queries=7 contexts=7/7 solvers=7/7 policy=100000/5000

The positive semantic matrix goes beyond retention tautologies: universal
preconditions instantiate symbolic applications at concrete values, existential
goals construct witnesses, and quantified Spec bodies accept symbolic and named
first-class function values. It covers scalar, parametric, option, immutable
record, recursive ADT, multiargument, direct Spec, nested, postcondition, proof,
and function-sorted binders.

  $ retained positive_symbolic_quantifiers fixtures/positive_symbolic_quantifiers.ml
  $ timeout 20 env OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/positive_symbolic_quantifiers.ml --threads 1 --timeout-ms 5000 --rlimit 100000
  verocaml: verified file=fixtures/positive_symbolic_quantifiers.ml functions=12 obligations=40
  $ timeout 20 ./verified_callback_traversals_tool.exe parity artifacts/positive_symbolic_quantifiers.cmt
  parity=repeat/threads status=verified functions=12 obligations=40 resources=100000 timeout-ms=5000
  $ timeout 20 ./verified_callback_traversals_tool.exe resource-evidence artifacts/positive_symbolic_quantifiers.cmt
  resources obligations=40 contexts=40/40 live=0 solvers=40/40 max-live=1 policy=100000/5000
  $ for route in fixtures/positive_symbolic_quantifiers.ml artifacts/positive_symbolic_quantifiers.cmt; do tag=$(basename "$route"); timeout 20 env OCAML_COLOR=never ../../src/verocaml.exe verify "$route" --threads 1 --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$tag.sst" --dump-vir "artifacts/$tag.vir" >"artifacts/$tag.out"; done
  $ sed -E 's/file=[^ ]+/file=<fixture>/' artifacts/positive_symbolic_quantifiers.ml.out > artifacts/positive_symbolic_quantifiers.source.out
  $ sed -E 's/file=[^ ]+/file=<fixture>/' artifacts/positive_symbolic_quantifiers.cmt.out > artifacts/positive_symbolic_quantifiers.retained.out
  $ cmp artifacts/positive_symbolic_quantifiers.source.out artifacts/positive_symbolic_quantifiers.retained.out
  $ cmp artifacts/positive_symbolic_quantifiers.ml.vir artifacts/positive_symbolic_quantifiers.cmt.vir
  $ echo 'routes source/cmt=true vir=true'
  routes source/cmt=true vir=true

Every source universal owns one explicit trigger, every translated universal
owns one pattern, and every translated quantifier retains deterministic solver
metadata. The matrix also pins symbolic declarations and first-class Spec apply
heads rather than merely proving the fixture as a black box.

  $ sst=artifacts/positive_symbolic_quantifiers.cmt.sst; printf 'sst forall=%s exists=%s triggers=%s symbolic=%s spec-apply=%s\n' "$(grep -Ec '^[[:space:]]+forall binder=' "$sst")" "$(grep -Ec '^[[:space:]]+exists binder=' "$sst")" "$(grep -Ec '^[[:space:]]+trigger$' "$sst")" "$(grep -c 'body symbolic-declaration ' "$sst")" "$(grep -c 'specification-call \$verocaml.spec-apply:' "$sst")"
  sst forall=24 exists=12 triggers=24 symbolic=10 spec-apply=22
  $ printf 'binders int=%s bool=%s parametric=%s option=%s box=%s sequence=%s pair=%s function=%s\n' "$(grep -Ec 'binder=[^ ]+:int qid=' "$sst")" "$(grep -Ec 'binder=[^ ]+:bool qid=' "$sst")" "$(grep -Ec "binder=[^ ]+:'[0-9]+@" "$sst")" "$(grep -Ec 'binder=[^ ]+:Stdlib.option<' "$sst")" "$(grep -Ec 'binder=[^ ]+:box<' "$sst")" "$(grep -Ec 'binder=[^ ]+:sequence<' "$sst")" "$(grep -Ec 'binder=[^ ]+:pair<' "$sst")" "$(grep -Ec 'binder=[^ ]+:\$verocaml.spec-function<' "$sst")"
  binders int=14 bool=2 parametric=4 option=2 box=3 sequence=2 pair=2 function=7
  $ test "$(grep -o 'qid=vero.q.[^ ]*' "$sst" | wc -l)" -eq 36; test "$(grep -o 'qid=vero.q.[^ ]*' "$sst" | sort -u | wc -l)" -eq 36; test "$(grep -o 'skid=vero.sk.[^ ]*' "$sst" | sort -u | wc -l)" -eq 36; echo 'source-identities qids=36 skids=36 unique=true'
  source-identities qids=36 skids=36 unique=true
  $ grep -q 'specification-call maps_to_self' "$sst"; grep -q 'specification-call has_value_witness' "$sst"; grep -q 'specification-call named_integer_image' "$sst"; grep -q 'specification-call named_box_image' "$sst"; grep -q 'symbolic-application pair_image' "$sst"; grep -q 'symbolic-application combined_image' "$sst"; grep -q 'symbolic-application symbolic_apply' "$sst"
  $ vir=artifacts/positive_symbolic_quantifiers.cmt.vir; printf 'vir forall=%s exists=%s patterns=%s qids=%s skids=%s\n' "$(grep -o '(forall ' "$vir" | wc -l)" "$(grep -o '(exists ' "$vir" | wc -l)" "$(grep -o ':pattern ' "$vir" | wc -l)" "$(grep -o ':qid ' "$vir" | wc -l)" "$(grep -o ':skolemid ' "$vir" | wc -l)"
  vir forall=132 exists=45 patterns=132 qids=177 skids=177
  $ grep -q ':pattern ((trigger-spec.*named_integer_image' "$vir"; grep -q ':pattern ((trigger-spec.*named_box_image' "$vir"; grep -q ':pattern (pair_image' "$vir"; grep -q ':pattern (symbolic_apply\[int,int\](f' "$vir"; grep -q ':pattern (\$verocaml.spec-apply:' "$vir"; echo 'heads direct-spec/symbolic/spec-function=true'
  heads direct-spec/symbolic/spec-function=true

Thirty-two direct statement cases keep the ordinary surface obvious: eight
proof assertions for each quantifier kind and eight executable assertions for
each kind, spanning scalar, parametric, option, record, recursive ADT, and
function binders.

  $ retained positive_quantifier_statements fixtures/positive_quantifier_statements.ml
  $ timeout 30 env OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/positive_quantifier_statements.ml --threads 1 --timeout-ms 5000 --rlimit 100000
  verocaml: verified file=fixtures/positive_quantifier_statements.ml functions=32 obligations=48
  $ timeout 30 ./verified_callback_traversals_tool.exe parity artifacts/positive_quantifier_statements.cmt
  parity=repeat/threads status=verified functions=32 obligations=48 resources=100000 timeout-ms=5000
  $ timeout 30 ./verified_callback_traversals_tool.exe resource-evidence artifacts/positive_quantifier_statements.cmt
  resources obligations=48 contexts=48/48 live=0 solvers=48/48 max-live=1 policy=100000/5000
  $ timeout 30 env OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_quantifier_statements.cmt --threads 2 --timeout-ms 5000 --rlimit 100000 --dump-sst artifacts/positive_quantifier_statements.sst --dump-vir artifacts/positive_quantifier_statements.vir >/dev/null
  $ sst=artifacts/positive_quantifier_statements.sst; vir=artifacts/positive_quantifier_statements.vir; printf 'statements proofs=%s exec=%s forall=%s exists=%s triggers=%s symbolic=%s\n' "$(grep -c '^function proof_' "$sst")" "$(grep -c '^function assert_' "$sst")" "$(grep -Ec '^[[:space:]]+forall binder=' "$sst")" "$(grep -Ec '^[[:space:]]+exists binder=' "$sst")" "$(grep -Ec '^[[:space:]]+trigger$' "$sst")" "$(grep -c 'body symbolic-declaration ' "$sst")"
  statements proofs=16 exec=16 forall=16 exists=16 triggers=16 symbolic=7
  $ printf 'unit-proof-parameters literal=%s named=%s\n' "$(grep -c 'pattern unit : unit' "$sst")" "$(grep -c 'pattern bind _argument#[0-9]*:unit : unit' "$sst")"
  unit-proof-parameters literal=2 named=1
  $ printf 'statement-vir forall=%s exists=%s patterns=%s qids=%s skids=%s\n' "$(grep -o '(forall ' "$vir" | wc -l)" "$(grep -o '(exists ' "$vir" | wc -l)" "$(grep -o ':pattern ' "$vir" | wc -l)" "$(grep -o ':qid ' "$vir" | wc -l)" "$(grep -o ':skolemid ' "$vir" | wc -l)"
  statement-vir forall=40 exists=40 patterns=40 qids=80 skids=80

Carrier issuance and metadata attacks reject before semantic SST, VIR,
obligation, backend, solver, or query work.

  $ ./verified_callback_traversals_tool.exe carrier-attacks artifacts/quantifiers.cmt artifacts/negative_quantifier_semantic.cmt
  carrier-auth authenticated=22 raw=22 forged=22 stale=22 wrong-program=22 copied-marker=22 sst=0 vir=0 vc=0 backend=0 solver=0 query=0
  $ ./verified_callback_traversals_tool.exe metadata-attacks artifacts/quantifiers.cmt
  metadata-auth owner=reject stale=reject type=reject kind=reject sst=0 vir=0 vc=0 backend=0 solver=0 query=0

Qids and skids ignore alpha spelling and observation order while remaining
unique and deterministic.

  $ sed 's/candidate/alternate/g; s/witness/renamed_/g' fixtures/quantifiers.ml > artifacts/alpha_quantifiers.ml
  $ retained alpha_quantifiers artifacts/alpha_quantifiers.ml
  $ ./verified_callback_traversals_tool.exe compare-identities artifacts/quantifiers.cmt artifacts/alpha_quantifiers.cmt
  identity-parity qids=22 skids=22 stable=true reorder-observation=true

Every consolidated authentication subcase is compiled independently and the
counter-owning tool proves zero semantic work.

  $ for name in raw shadowed qualified function-binder reference-binder array-binder object-binder mutable-binder cyclic-binder open-binder grouped-binder nested-wrong-owner; do extract_case fixtures/negative_quantifier_authentication.ml "$name" "artifacts/auth-$name.ml"; retained "auth-$name" "artifacts/auth-$name.ml"; timeout 5 ./verified_callback_traversals_tool.exe reject-zero-work "artifacts/auth-$name.cmt" >/dev/null || exit $?; echo "authentication:$name zero-work"; done
  authentication:raw zero-work
  authentication:shadowed zero-work
  authentication:qualified zero-work
  authentication:function-binder zero-work
  authentication:reference-binder zero-work
  authentication:array-binder zero-work
  authentication:object-binder zero-work
  authentication:mutable-binder zero-work
  authentication:cyclic-binder zero-work
  authentication:open-binder zero-work
  authentication:grouped-binder zero-work
  authentication:nested-wrong-owner zero-work

Every explicit-trigger policy rejection is likewise independent and
pre-semantic.

  $ for name in missing duplicate grouped-payload misplaced-pattern misplaced-quantifier nonapplication equality selector nullary incomplete wrong-binder nested-owner existential-trigger; do extract_case fixtures/negative_trigger.ml "$name" "artifacts/trigger-$name.ml"; retained "trigger-$name" "artifacts/trigger-$name.ml"; timeout 5 ./verified_callback_traversals_tool.exe reject-zero-work "artifacts/trigger-$name.cmt" >/dev/null || exit $?; echo "trigger:$name zero-work"; done
  trigger:missing zero-work
  trigger:duplicate zero-work
  trigger:grouped-payload zero-work
  trigger:misplaced-pattern zero-work
  trigger:misplaced-quantifier zero-work
  trigger:nonapplication zero-work
  trigger:equality zero-work
  trigger:selector zero-work
  trigger:nullary zero-work
  trigger:incomplete zero-work
  trigger:wrong-binder zero-work
  trigger:nested-owner zero-work
  trigger:existential-trigger zero-work
