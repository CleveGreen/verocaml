Scoped broadcasts retain one generic theorem, one explicit trigger, and one
query-local multi-binder quantifier under the fixed resource policy.

  $ mkdir artifacts
  $ retained () { name=$1; source=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ for name in parametric_positive group_positive scopes_positive trusted_axiom inactive_control reorder_a reorder_b negative_declaration negative_trigger negative_group_cycle negative_wrong_actual negative_e_matching; do retained "$name" "fixtures/$name.ml"; done
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/negative_wrong_actual.cmt --threads 1 --timeout-ms 5000 --rlimit 100000 | sed -E 's,file=[^ ]+,file=function-valued-trigger.cmt,'
  verocaml: verified file=function-valued-trigger.cmt functions=1 obligations=1
  $ for input in fixtures/parametric_positive.ml artifacts/parametric_positive.cmt; do timeout 10 env OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 --rlimit 100000 | sed -E 's/file=[^ ]+/file=<source-or-cmt>/'; done
  verocaml: verified file=<source-or-cmt> functions=8 obligations=12
  verocaml: verified file=<source-or-cmt> functions=8 obligations=12
  $ for threads in 1 2; do timeout 10 env OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/parametric_positive.cmt --threads "$threads" --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/thread-$threads.sst" --dump-vir "artifacts/thread-$threads.vir" >/dev/null; done
  $ cmp artifacts/thread-1.sst artifacts/thread-2.sst && cmp artifacts/thread-1.vir artifacts/thread-2.vir && echo "thread-parity=1/2 stable=true"
  thread-parity=1/2 stable=true
  $ timeout 10 ./scoped_broadcasts_tool.exe structural artifacts/parametric_positive.cmt > artifacts/parametric.first
  $ timeout 10 ./scoped_broadcasts_tool.exe structural artifacts/parametric_positive.cmt > artifacts/parametric.second
  $ cmp artifacts/parametric.first artifacts/parametric.second
  $ sed -n '1p;$p' artifacts/parametric.first
  status=verified functions=8 obligations=12 destroyed=true
  resources backend=12 contexts=12 solvers=12 resets=12 cleaned=12 live=0
  $ awk '/^insert / { n++; if ($0 ~ /binders=1 /) b1++; if ($0 ~ /binders=2 /) b2++; if ($0 ~ /binders=3 /) b3++; if ($0 ~ /portable=1$/) portable++; if ($0 ~ /vector=\[bool\]/) vb++; if ($0 ~ /vector=\[int\]/) vi++; if ($0 ~ /vector=\[.0@use_abstract#8\]/) va++ } END { printf "insertions=%d binders=1:%d,2:%d,3:%d vectors=abstract:%d,int:%d,bool:%d portable=%d qid-skid=stable\n", n,b1,b2,b3,va,vi,vb,portable }' artifacts/parametric.first
  insertions=11 binders=1:1,2:2,3:8 vectors=abstract:1,int:6,bool:2 portable=11 qid-skid=stable
  $ ./scoped_broadcasts_tool.exe backend-shape
  native arity=2 quantifiers=1 binders=[verocaml_b0_left,verocaml_b1_flag] patterns=1 terms=1 trigger=application direct=true detached=true parity=true lifecycle=1/1/1/1/0
  native arity=3 quantifiers=1 binders=[verocaml_b0_left,verocaml_b1_flag,verocaml_b2_right] patterns=1 terms=1 trigger=application direct=true detached=true parity=true lifecycle=1/1/1/1/0

Groups expand idempotently in semantic-ID order, retain every selecting path,
and leave their own proofs with an empty active set.

  $ ./scoped_broadcasts_tool.exe structural artifacts/group_positive.cmt > artifacts/group.out
  $ grep -E '^(status=|vc function=grouped|resources )' artifacts/group.out
  status=verified functions=3 obligations=3 destroyed=true
  vc function=grouped index=0 active=2 trusted-declarations=0 trusted-uses=0 inserted=2
  resources backend=3 contexts=3 solvers=3 resets=3 cleaned=3 live=0
  $ grep '^insert ' artifacts/group.out | sed -E 's/qid=[^ ]+ skid=[^ ]+/qid=<stable> skid=<stable>/'
  insert id=broadcast:lemma_a vector=[] binders=1 qid=<stable> skid=<stable> ordinal=0 trusted=false paths=activate/outer/broadcast:lemma_a,activate/outer/inner/broadcast:lemma_a witness=none logic=4 portable=1
  insert id=broadcast:lemma_b vector=[] binders=1 qid=<stable> skid=<stable> ordinal=1 trusted=false paths=activate/broadcast:lemma_b,activate/outer/inner/broadcast:lemma_b witness=none logic=4 portable=1

Structure, expression, and nested-expression activation are lexical and
additive. Earlier siblings and the called proof retain their empty declaration
snapshot.

  $ ./scoped_broadcasts_tool.exe structural artifacts/scopes_positive.cmt > artifacts/scopes.out
  $ grep -E '^(status=|vc function=|resources )' artifacts/scopes.out
  status=verified functions=11 obligations=10 destroyed=true
  vc function=Nested.nested_lemma index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=Nested.nested_after index=0 active=1 trusted-declarations=0 trusted-uses=0 inserted=1
  vc function=lemma_scope index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=expression_scope index=0 active=1 trusted-declarations=0 trusted-uses=0 inserted=1
  vc function=parent_after_nested index=0 active=1 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=structure_scope index=0 active=1 trusted-declarations=0 trusted-uses=0 inserted=1
  vc function=before_activation index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=Nested.make index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=Nested.make index=1 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  vc function=Nested.nested_before index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  resources backend=10 contexts=10 solvers=10 resets=10 cleaned=10 live=0
  $ grep '^insert id=broadcast:Nested' artifacts/scopes.out | sed -E 's/qid=[^ ]+ skid=[^ ]+/qid=<stable> skid=<stable>/'
  insert id=broadcast:Nested.nested_lemma vector=[] binders=1 qid=<stable> skid=<stable> ordinal=0 trusted=false paths=activate/broadcast:Nested.nested_lemma witness=none logic=1 portable=1

The axiom sugar and explicit long form independently retain authenticated
trusted-proof provenance and normalize to the same role, counters, insertion,
type vector, ordinal, and selecting path.  An ordinary nonbroadcast axiom
retains its trusted proof body without creating broadcast state.

  $ ./scoped_broadcasts_tool.exe structural artifacts/trusted_axiom.cmt > artifacts/trusted.out
  $ grep -E '^(status=|vc function=|insert |resources )' artifacts/trusted.out | sed -E 's/qid=[^ ]+ skid=[^ ]+/qid=<stable> skid=<stable>/'
  status=verified functions=3 obligations=3 destroyed=true
  vc function=sugar_use index=0 active=1 trusted-declarations=1 trusted-uses=1 inserted=1
  insert id=broadcast:axiom_sugar vector=[] binders=1 qid=<stable> skid=<stable> ordinal=0 trusted=true paths=activate/broadcast:axiom_sugar witness=trusted_axiom.ml:6:0-6:18 logic=2 portable=1
  vc function=long_use index=0 active=1 trusted-declarations=1 trusted-uses=1 inserted=1
  insert id=broadcast:axiom_long vector=[] binders=1 qid=<stable> skid=<stable> ordinal=0 trusted=true paths=activate/broadcast:axiom_long witness=trusted_axiom.ml:11:19-11:45 logic=2 portable=1
  vc function=ordinary_control index=0 active=0 trusted-declarations=0 trusted-uses=0 inserted=0
  resources backend=3 contexts=3 solvers=3 resets=3 cleaned=3 live=0
  $ ../../src/verocaml.exe verify artifacts/trusted_axiom.cmt --dump-sst artifacts/trusted.sst >/dev/null
  $ cat > artifacts/trust_oracle.py <<'PY'
  > import re
  > import sys
  > from pathlib import Path
  > structural, sst_path, function, declaration = sys.argv[1:]
  > rows = Path(structural).read_text().splitlines()
  > vc = next(row for row in rows if row.startswith(f"vc function={function} "))
  > insertion = next(row for row in rows if row.startswith(f"insert id=broadcast:{declaration} "))
  > fields = dict(field.split("=", 1) for field in insertion.split()[1:])
  > vc_fields = dict(field.split("=", 1) for field in vc.split()[1:])
  > block = re.search(
  >     rf"^function {declaration}#[^\n]*\n.*?(?=^function |\Z)",
  >     Path(sst_path).read_text(),
  >     re.MULTILINE | re.DOTALL,
  > )
  > body = re.search(
  >     r"body trusted-external-body trust=axiomatic provenance=typedtree:"
  >     r"[^\n]* witness-span=([^\s]+)",
  >     block.group(0) if block else "",
  > )
  > assert body and body.group(1)
  > assert fields["trusted"] == "true" and fields["witness"] != "none"
  > normalized_paths = fields["paths"].replace(
  >     f"broadcast:{declaration}", "broadcast:<declaration>"
  > )
  > print(
  >     "trust declaration=<declaration> declaration-span=<source-span> role=trusted "
  >     f"trusted={fields['trusted']} trusted-declarations={vc_fields['trusted-declarations']} "
  >     f"trusted-uses={vc_fields['trusted-uses']} inserted={vc_fields['inserted']} "
  >     f"ordinal={fields['ordinal']} vector={fields['vector']} paths={normalized_paths} "
  >     "witness=<source-span> provenance=authenticated-external-proof"
  > )
  > PY
  $ python3 artifacts/trust_oracle.py artifacts/trusted.out artifacts/trusted.sst sugar_use axiom_sugar > artifacts/sugar.oracle
  $ python3 artifacts/trust_oracle.py artifacts/trusted.out artifacts/trusted.sst long_use axiom_long > artifacts/long.oracle
  $ cmp artifacts/sugar.oracle artifacts/long.oracle && cat artifacts/sugar.oracle
  trust declaration=<declaration> declaration-span=<source-span> role=trusted trusted=true trusted-declarations=1 trusted-uses=1 inserted=1 ordinal=0 vector=[] paths=activate/broadcast:<declaration> witness=<source-span> provenance=authenticated-external-proof
  $ awk '/^function ordinary_axiom#/{seen=1; next} seen && /^function /{exit} seen{print}' artifacts/trusted.sst | grep -q 'body trusted-external-body trust=axiomatic provenance=typedtree:.* witness-span='
  $ test $(grep -c '^insert id=broadcast:ordinary_axiom ' artifacts/trusted.out) -eq 0
  $ grep '^vc function=ordinary_control ' artifacts/trusted.out | sed -E 's/^vc function=ordinary_control index=0 /ordinary role=trusted-proof-body broadcast=false /; s/$/ witness=present provenance=authenticated-external-proof/'
  ordinary role=trusted-proof-body broadcast=false active=0 trusted-declarations=0 trusted-uses=0 inserted=0 witness=present provenance=authenticated-external-proof

The recursive early prepass removes public axiom sugar before retained source
and CMT observation and emits exactly one role-neutral declaration carrier.

  $ cat > artifacts/one_sugar.ml <<'EOF'
  > module Nested = struct
  >   let observed (_value : int) = true [@@verocaml.spec]
  >   let one_sugar (value : int) : unit =
  >     [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  >   [@@verocaml.axiom] [@@verocaml.broadcast]
  > end
  > EOF
  $ retained one_sugar artifacts/one_sugar.ml
  $ strings artifacts/one_sugar.cmt > artifacts/one_sugar.strings
  $ test $(grep -Ec '^[[:space:]]*verocaml\.(axiom|broadcast|broadcast_lemma|broadcast_axiom)$' artifacts/one_sugar.strings) -eq 0
  $ test $(grep -c 'verocaml.internal.broadcast.declaration.v1' artifacts/one_sugar.strings) -eq 1
  $ grep -Eo 'declaration\.[0-9a-f]{32}\|[0-9]+\|[0-9]+' artifacts/one_sugar.strings | sed -E 's/declaration\.[0-9a-f]{32}\|[0-9]+\|[0-9]+/declaration.<id>|<start>|<stop>/'
  declaration.<id>|<start>|<stop>
  $ ocamlc -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" artifacts/one_sugar.ml > artifacts/one_sugar.stdout 2> artifacts/one_sugar.source
  $ test ! -s artifacts/one_sugar.stdout
  $ grep -c 'verocaml.internal.broadcast.declaration.v1' artifacts/one_sugar.source
  1
  $ test $(grep -Ec 'verocaml\.axiom|verocaml\.broadcast(_lemma|_axiom)?([^a-z_]|$)' artifacts/one_sugar.source) -eq 0

Declaration order, activation spelling, alpha spelling, repeated execution,
and solver thread count do not change insertion order or identities.

  $ ./scoped_broadcasts_tool.exe structural artifacts/reorder_a.cmt > artifacts/reorder-a.out
  $ ./scoped_broadcasts_tool.exe structural artifacts/reorder_b.cmt > artifacts/reorder-b.out
  $ grep '^insert ' artifacts/reorder-a.out > artifacts/reorder-a.insert
  $ grep '^insert ' artifacts/reorder-b.out > artifacts/reorder-b.insert
  $ cmp artifacts/reorder-a.insert artifacts/reorder-b.insert && echo "reorder-alpha-parity insertions=$(wc -l < artifacts/reorder-a.insert) stable=true"
  reorder-alpha-parity insertions=4 stable=true

Without retained carriers the syntax erases completely and preserves runtime
behaviour.

  $ cp fixtures/runtime_erasure.ml artifacts/runtime_erasure.ml
  $ ocamlc -w -A -alert -all -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime_erasure.cmo artifacts/runtime_erasure.ml
  $ ocamlc ../../runtime/vero_ghost.cma artifacts/runtime_erasure.cmo -o artifacts/runtime_erasure.exe
  $ artifacts/runtime_erasure.exe
  runtime=42

Malformed PPX surface forms reject before a CMT or any verification work.

  $ cat > artifacts/dual_role.ml <<'EOF'
  > let bad x = x [@@verocaml.broadcast] [@@verocaml.broadcast]
  > EOF
  $ cat > artifacts/empty_activation.ml <<'EOF'
  > [@@@verocaml.activate []]
  > let value = 1
  > EOF
  $ cat > artifacts/computed_group.ml <<'EOF'
  > [@@@verocaml.broadcast_group (String.uppercase_ascii "g", [value])]
  > let value = 1
  > EOF
  $ cat > artifacts/string_group.ml <<'EOF'
  > [@@@verocaml.broadcast_group ("g", [value])]
  > let value = 1
  > EOF
  $ cat > artifacts/qualified_group.ml <<'EOF'
  > [@@@verocaml.broadcast_group (Module.group, [value])]
  > let value = 1
  > EOF
  $ cat > artifacts/recursive_axiom.ml <<'EOF'
  > let observed (_value : int) = true [@@verocaml.spec]
  > let rec bad value =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.axiom] [@@verocaml.broadcast]
  > EOF
  $ cat > artifacts/legacy_lemma.ml <<'EOF'
  > let bad x = x [@@verocaml.broadcast_lemma]
  > EOF
  $ cat > artifacts/legacy_axiom.ml <<'EOF'
  > let bad x = x [@@verocaml.broadcast_axiom]
  > EOF
  $ cat > artifacts/axiom_conflict.ml <<'EOF'
  > let bad () = () [@@verocaml.axiom] [@@verocaml.proof]
  > EOF
  $ cat > artifacts/axiom_duplicate.ml <<'EOF'
  > let bad () = () [@@verocaml.axiom] [@@verocaml.axiom]
  > EOF
  $ cat > artifacts/axiom_duplicate_external.ml <<'EOF'
  > let bad () = () [@@verocaml.axiom] [@@verocaml.external_body]
  > EOF
  $ cat > artifacts/axiom_payload.ml <<'EOF'
  > let bad () = () [@@verocaml.axiom "bad"]
  > EOF
  $ cat > artifacts/axiom_misplaced.ml <<'EOF'
  > let bad = ((fun () -> ()) [@verocaml.axiom])
  > EOF
  $ cat > artifacts/broadcast_payload.ml <<'EOF'
  > let bad () = () [@@verocaml.broadcast "bad"]
  > EOF
  $ cat > artifacts/broadcast_misplaced.ml <<'EOF'
  > let bad = ((fun () -> ()) [@verocaml.broadcast])
  > EOF
  $ cat > artifacts/axiom_malformed_contract.ml <<'EOF'
  > let bad () = [%verocaml.assert true]; () [@@verocaml.axiom]
  > EOF
  $ for case in negative_scope dual_role empty_activation computed_group string_group qualified_group recursive_axiom legacy_lemma legacy_axiom axiom_conflict axiom_duplicate axiom_duplicate_external axiom_payload axiom_misplaced broadcast_payload broadcast_misplaced axiom_malformed_contract; do source="artifacts/$case.ml"; test "$case" = negative_scope && source=fixtures/negative_scope.ml; if retained "ppx-$case" "$source" >/dev/null 2>&1; then exit 1; else echo "ppx:$case zero-work"; fi; done
  ppx:negative_scope zero-work
  ppx:dual_role zero-work
  ppx:empty_activation zero-work
  ppx:computed_group zero-work
  ppx:string_group zero-work
  ppx:qualified_group zero-work
  ppx:recursive_axiom zero-work
  ppx:legacy_lemma zero-work
  ppx:legacy_axiom zero-work
  ppx:axiom_conflict zero-work
  ppx:axiom_duplicate zero-work
  ppx:axiom_duplicate_external zero-work
  ppx:axiom_payload zero-work
  ppx:axiom_misplaced zero-work
  ppx:broadcast_payload zero-work
  ppx:broadcast_misplaced zero-work
  ppx:axiom_malformed_contract zero-work

The adapter independently rejects declaration, trigger, group, and binder
matrices before SST/VIR/VC/backend/solver/query work.

  $ cat > artifacts/generate_negatives.py <<'PY'
  > from pathlib import Path
  > p = Path("artifacts")
  > common = "let observed (_value : int) : bool = true [@@verocaml.spec]\n"
  > cases = {
  > "missing": common + """let bad (value:int) : unit =
  >   [%verocaml.ensures fun _ -> observed value || value = value]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "duplicate": common + """let bad (value:int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])];
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "nonapplication": """let bad (value:bool) : unit =
  >   [%verocaml.ensures fun _ -> (value [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "equality": """let bad (value:int) : unit =
  >   [%verocaml.ensures fun _ -> ((value = value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "incomplete": """let observed_pair (_left:int) (_right:bool) = true [@@verocaml.spec]
  > let bad (left:int) (right:bool) : unit =
  >   [%verocaml.ensures fun _ -> ((observed_pair left true) [@trigger]) || right]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "nonproof-external": common + """let bad (value:int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.external_body] [@@verocaml.broadcast]
  > """,
  > "ordinary": common + """let bad (value:int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.broadcast]
  > """,
  > "axiom-nonunit": common + """let bad (value:int) : int =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; value
  > [@@verocaml.axiom] [@@verocaml.broadcast]
  > """,
  > "self-cycle": common + """let lemma (value:int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > [@@@verocaml.broadcast_group (self, [self; lemma])]
  > let untouched value = value
  > """,
  > "ordinary-group": """let ordinary value = value
  > [@@@verocaml.broadcast_group (bad, [ordinary])]
  > let untouched value = value
  > """,
  > "reference": """let observed (_value : int ref) = true [@@verocaml.spec]
  > let bad (value:int ref) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "array": """let observed (_value : int array) = true [@@verocaml.spec]
  > let bad (value:int array) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > "object": """let observed (_value : < get : int >) = true [@@verocaml.spec]
  > let bad (value:< get : int >) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """,
  > }
  > for name, source in cases.items():
  >     (p / f"{name}.ml").write_text(source)
  > PY
  $ python3 artifacts/generate_negatives.py
  $ for case in missing duplicate nonapplication equality incomplete nonproof-external ordinary axiom-nonunit self-cycle ordinary-group reference array object; do retained "negative-$case" "artifacts/$case.ml"; timeout 5 ./scoped_broadcasts_tool.exe diagnostic "artifacts/negative-$case.cmt" | grep -q 'solver=0 z3=0/0' || exit 1; echo "adapter:$case zero-work"; done
  adapter:missing zero-work
  adapter:duplicate zero-work
  adapter:nonapplication zero-work
  adapter:equality zero-work
  adapter:incomplete zero-work
  adapter:nonproof-external zero-work
  adapter:ordinary zero-work
  adapter:axiom-nonunit zero-work
  adapter:self-cycle zero-work
  adapter:ordinary-group zero-work
  adapter:reference zero-work
  adapter:array zero-work
  adapter:object zero-work
  $ for case in negative_declaration negative_trigger negative_group_cycle; do ./scoped_broadcasts_tool.exe diagnostic "artifacts/$case.cmt" | grep -q 'solver=0 z3=0/0' || exit 1; echo "fixture:$case zero-work"; done
  fixture:negative_declaration zero-work
  fixture:negative_trigger zero-work
  fixture:negative_group_cycle zero-work

Raw syntax, clean-build source forgeries/copies/rebindings, and sampled
byte-level malformed carrier metadata reject at the authenticated adapter
boundary.  Self-consistent hand-edited derived CMT state is outside this
source/PPX trust boundary.

  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/raw.cmo fixtures/negative_foreign_or_stale.ml
  $ ./scoped_broadcasts_tool.exe diagnostic artifacts/raw.cmt
  rejected=adapter code=VERO_BROADCAST_AUTHENTICATION solver=0 z3=0/0 detail=raw broadcast syntax was not rewritten by the authenticated PPX
  $ cat > artifacts/raw-legacy-axiom.ml <<'EOF'
  > let raw_broadcast value = value [@@verocaml.broadcast_axiom]
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/raw-legacy-axiom.cmo artifacts/raw-legacy-axiom.ml
  $ ./scoped_broadcasts_tool.exe diagnostic artifacts/raw-legacy-axiom.cmt
  rejected=adapter code=VERO_BROADCAST_AUTHENTICATION solver=0 z3=0/0 detail=raw broadcast syntax was not rewritten by the authenticated PPX
  $ cat > artifacts/source-forged-declaration.ml <<'EOF'
  > let forged (value : int) : unit =
  >   [%verocaml.assert value = value]; ()
  > [@@verocaml.proof]
  > [@@verocaml.internal.broadcast.declaration.v1
  >   "declaration.00000000000000000000000000000000|0|0"]
  > EOF
  $ cat > artifacts/source-forged-carrier.ml <<'EOF'
  > let forged_carrier () =
  >   Vero_ghost.marker
  >     "verocaml:broadcast:carrier:v1:structure:structure.00000000000000000000000000000000";
  >   ()
  > [@@verocaml.internal.broadcast.carrier.v1
  >   "structure|structure.00000000000000000000000000000000||0|0"]
  > EOF
  $ cat > artifacts/source-copied-target.ml <<'EOF'
  > let observed (_value : int) = true [@@verocaml.spec]
  > let lemma (value : int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > let copied = lemma
  > [@@@verocaml.activate [copied]]
  > let after value = value
  > EOF
  $ cat > artifacts/source-rebound-marker.ml <<'EOF'
  > let observed (_value : int) = true [@@verocaml.spec]
  > let lemma (value : int) : unit =
  >   [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > module Vero_ghost = struct let marker (_ : string) = () end
  > [@@@verocaml.activate [lemma]]
  > let after value = value
  > EOF
  $ for case in source-forged-declaration source-forged-carrier; do if retained "$case" "artifacts/$case.ml" >/dev/null 2>&1; then exit 1; else echo "ppx-source-auth:$case zero-work"; fi; done
  ppx-source-auth:source-forged-declaration zero-work
  ppx-source-auth:source-forged-carrier zero-work
  $ for case in source-copied-target source-rebound-marker; do retained "$case" "artifacts/$case.ml"; ./scoped_broadcasts_tool.exe diagnostic "artifacts/$case.cmt" | grep -q 'solver=0 z3=0/0' || exit 1; echo "source-auth:$case zero-work"; done
  source-auth:source-copied-target zero-work
  source-auth:source-rebound-marker zero-work
  $ for attack in declaration-id carrier-id group-id marker declaration-span carrier-span group-span legacy-lemma legacy-axiom; do python3 mutate_broadcast_carrier.py artifacts/group_positive.cmt "artifacts/$attack.cmt" "$attack"; ./scoped_broadcasts_tool.exe diagnostic "artifacts/$attack.cmt" | grep -q 'solver=0 z3=0/0' || exit 1; echo "carrier:$attack zero-work"; done
  carrier:declaration-id zero-work
  carrier:carrier-id zero-work
  carrier:group-id zero-work
  carrier:marker zero-work
  carrier:declaration-span zero-work
  carrier:carrier-span zero-work
  carrier:group-span zero-work
  carrier:legacy-lemma zero-work
  carrier:legacy-axiom zero-work
  $ python3 mutate_broadcast_carrier.py artifacts/scopes_positive.cmt artifacts/scope-span.cmt scope-span
  $ ./scoped_broadcasts_tool.exe diagnostic artifacts/scope-span.cmt | grep -q 'solver=0 z3=0/0' && echo "carrier:scope-span zero-work"
  carrier:scope-span zero-work
  $ ./scoped_broadcasts_tool.exe wrong-artifact artifacts/group_positive.cmt artifacts/reorder_a.cmt
  wrong-artifact code=VERO_BROADCAST_AUTHENTICATION solver=0 z3=0/0

False but authenticated claims enter bounded solver work rather than becoming
unsupported, while inactive declarations remain inert.

  $ ./scoped_broadcasts_tool.exe semantic artifacts/inactive_control.cmt
  status=counterexample functions=1 obligations=1
  $ ./scoped_broadcasts_tool.exe engine-negative artifacts/negative_e_matching.cmt
  result=status:counterexample solver=1 z3=1/1 live=0

The fixed 16-instance ceiling fires before solver creation for the affected
VC.  Vector construction rejects empty, duplicate, reordered, wrong-owner,
wrong-kind, wrong-type, and incomplete-trigger forms.

  $ cat > artifacts/instance_cap.py <<'PY'
  > from pathlib import Path
  > types = [("int" if i % 2 == 0 else "bool") + " option" * (i // 2) for i in range(17)]
  > lines = [
  > "let observed (_value : 'a) : bool = true [@@verocaml.spec]", "",
  > "let lemma (value : 'a) : unit =",
  > "  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];",
  > "  ()", "[@@verocaml.proof]", "[@@verocaml.external_body]",
  > "[@@verocaml.broadcast]", "", "[@@@verocaml.activate [lemma]]", "",
  > "let cap",
  > ]
  > lines += [f"    (v{i:02d} : {typ})" for i, typ in enumerate(types)]
  > lines += ["    : unit ="]
  > for typ in types:
  >     lines += ["  [%verocaml.requires", f"    forall (fun (candidate : {typ}) ->", "      ((observed candidate) [@trigger]) || candidate = candidate)];"]
  > lines += ["  [%verocaml.assert true];", "  ()", "[@@verocaml.proof]"]
  > Path("artifacts/instance_cap.ml").write_text("\n".join(lines) + "\n")
  > PY
  $ python3 artifacts/instance_cap.py
  $ retained instance_cap artifacts/instance_cap.ml
  $ ./scoped_broadcasts_tool.exe engine-negative artifacts/instance_cap.cmt | sed -E 's/ at [^ ]+ solver=/ solver=/'
  result=engine:cap: malformed SST: broadcast instance cap exceeded: 17 > 16 solver=0 z3=0/0 live=0
  $ ./scoped_broadcasts_tool.exe vector-unit
  vector rejects=8 accepted=1 binders=2 trigger=1
