Rank-backed nonrecursive specifications admit one exact local immutable rank
domain through outer aggregates, tuples, patterns, construction, matching,
projection, and typed equality.  The ordinary acyclic classifier remains
closed and publishes no logical descriptor for either recursive node or stack.

  $ mkdir artifacts
  $ retained () { name=$1; shift; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" "$@" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained positive
  $ ./rank_backed_specifications_tool.exe sst artifacts/positive.cmt > artifacts/positive.first.sst
  $ ./rank_backed_specifications_tool.exe sst artifacts/positive.cmt > artifacts/positive.second.sst
  $ cmp artifacts/positive.first.sst artifacts/positive.second.sst
  $ grep -E '^(type (stack|node<int>)|function (identity_node|identity_stack|spec_push_front|spec_front|node_equal|nested_identity))' artifacts/positive.first.sst
  type stack#1 representation=revealed @ positive.ml:3:0-6:1
  function identity_node#0 mode=spec recursive=false result=node<int> policy=default-linear/default-z3 @ positive.ml:8:0-9:17
  function identity_stack#1 mode=spec recursive=false result=stack#1 policy=default-linear/default-z3 @ positive.ml:11:0-12:17
  function spec_push_front#2 mode=spec recursive=false result=stack#1 policy=default-linear/default-z3 @ positive.ml:14:0-16:17
  function spec_front#3 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:18:0-22:17
  function node_equal#5 mode=spec recursive=false result=bool policy=default-linear/default-z3 @ positive.ml:35:0-37:17
  function nested_identity#6 mode=spec recursive=false result=(stack#1 * node<int>) policy=default-linear/default-z3 @ positive.ml:39:0-42:17
  $ ./rank_backed_specifications_tool.exe descriptors artifacts/positive.cmt | sed -E 's/compiler-uid=[^ ]+/compiler-uid=<uid>/'
  ordinary-logical-descriptor stack#1=none
  parametric-rank-schema node<'0@node#0> compiler-uid=<uid> visibility=private

One nonrecursive standalone Proof now uses the same private rank-backed shape
judgment for its complete body.  The human finite-stack shape covers a formal,
projection, Spec-valued let, assertions, and a Proof call; its Tracked variant
uses the same bare occurrences.  Construction and record/variant patterns
share that selected domain.  Repeated SST and VIR dumps are deterministic.

  $ retained proof_rank_backed_positive
  $ ../../src/verocaml.exe verify artifacts/proof_rank_backed_positive.cmt --timeout-ms 60000 --dump-sst artifacts/proof.first.sst --dump-vir artifacts/proof.first.vir 2>&1 | sed -E 's,file=[^ ]+,file=proof.cmt,'
  verocaml: verified file=proof.cmt functions=4 obligations=7
  $ ../../src/verocaml.exe verify artifacts/proof_rank_backed_positive.cmt --timeout-ms 60000 --dump-sst artifacts/proof.second.sst --dump-vir artifacts/proof.second.vir >/dev/null
  $ cmp artifacts/proof.first.sst artifacts/proof.second.sst
  $ cmp artifacts/proof.first.vir artifacts/proof.second.vir
  $ grep -E '^function (lemma_push_front_wf|lemma_push_front_wf_tracked|complete_shape)' artifacts/proof.first.sst | sed -E 's/ @ .*//'
  function lemma_push_front_wf#4 mode=proof recursive=false result=unit policy=default-linear/default-z3
  function lemma_push_front_wf_tracked#5 mode=proof recursive=false result=unit policy=default-linear/default-z3
  function complete_shape#6 mode=proof recursive=false result=unit policy=default-linear/default-z3

A complete Proof may select only one authenticated local rank domain.  The
negative rejects with zero finite operations, recursive lowering, backend,
context, or solver work.

  $ retained proof_mixed_domains
  $ ./rank_backed_specifications_tool.exe reject artifacts/proof_mixed_domains.cmt
  semantic rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0

The useful proof lowers only ordinary nominal constructor, tag, and selector
equations.  It creates no recursive-spec application/equation, rank projection
or strict-rank obligation, induction/reveal/fuel authority, or closed invariant
fact.  The production private session also issues and consumes no
completed-callee receipt.  These checks traverse structured SST/VIR and
recursive-semantics results; recursive lowering comes from a separate private
production observation trace.

  $ ./rank_backed_specifications_tool.exe vir artifacts/positive.cmt > artifacts/positive.first.vir
  $ ./rank_backed_specifications_tool.exe vir artifacts/positive.cmt > artifacts/positive.second.vir
  $ cmp artifacts/positive.first.vir artifacts/positive.second.vir
  $ ./rank_backed_specifications_tool.exe authority artifacts/positive.cmt
  ordinary tags=22 selectors=62
  forbidden recursive-applications=0 recursive-equations=0 rank-projections=0 strict-rank-obligations=0 induction=0 reveals=0 fuel=0 invariant-facts=0
  $ cat > artifacts/shape_only_pipeline.ml <<'EOF'
  > type 'a node = Empty | Node of 'a * 'a node
  > let verified (node : int node) =
  >   [%verocaml.assert node = node];
  >   ()
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/shape_only_pipeline.cmo artifacts/shape_only_pipeline.ml
  $ ./rank_backed_specifications_tool.exe pipeline artifacts/shape_only_pipeline.cmt
  production-authority recursive-lowering=0
  pipeline status=verified functions=1 obligations=1 completed-callee-issued=0 completed-callee-consumed=0 finite-witnesses=0 finite-parents=0 finite-children=0 finite-result-witnesses=0 finite-finalizations=0 finite-consumptions=0 dependent-lowerings=0 dependent-backends=0 dependent-solvers=0 session-destroyed=true

Finite operations now have real private counters.  The dedicated finite-value
receipt corpus supplies their nonzero positive controls; this arbitrary-entry
shape corpus remains a zero-control.  The former frozen product-diff absence
oracle is intentionally gone.  Public SST/VIR/backend terms remain unchanged,
and exact manifests continue to bind every installed path and interface.

  $ profile=$(realpath "$(dirname "$(readlink -f rank_backed_specifications_tool.exe)")/../.."); root=$(dirname "$(dirname "$profile")")
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ test "$(git -C "$root" rev-parse --show-toplevel)" = "$root"
  $ find "$install_root" -mindepth 1 -printf '%y %P\n' | LC_ALL=C sort > artifacts/installed-paths.manifest
  $ cmp manifests/installed-paths.manifest artifacts/installed-paths.manifest
  $ find "$install_root" \( -type f -o -type l \) \( -name '*.cmi' -o -name '*.mli' \) -printf '%P\n' | LC_ALL=C sort > artifacts/installed-interfaces.manifest
  $ cmp manifests/installed-interfaces.manifest artifacts/installed-interfaces.manifest
  $ find "$install_root" \( -type f -o -type l \) -name '*.cmi' ! -path '*/.private/*' -printf '%f\n' | sed 's/\.cmi$//' | LC_ALL=C sort > artifacts/installed-public-modules.manifest
  $ cmp manifests/installed-public-modules.manifest artifacts/installed-public-modules.manifest
  $ printf 'installed manifests paths=%s interfaces=%s public-modules=%s\n' "$(wc -l < artifacts/installed-paths.manifest)" "$(wc -l < artifacts/installed-interfaces.manifest)" "$(wc -l < artifacts/installed-public-modules.manifest)"
  installed manifests paths=920 interfaces=222 public-modules=21

An installed cold verification is snapshotted before and after under the
declared isolated working directory and HOME/XDG/TMP roots.  This check proves
that invocation creates no entry or mutation within those roots; it does not
claim to observe arbitrary filesystem locations outside that isolated scope.

  $ cold="$PWD/artifacts/cold"; mkdir -p "$cold/home" "$cold/cache" "$cold/state" "$cold/config" "$cold/tmp"
  $ snapshot () { { find . -type d -printf 'd %p\n'; find . -type f -print0 | sort -z | xargs -0 sha256sum; find . -type l -printf 'l %p -> %l\n'; } | sort; }
  $ before=$(mktemp); after=$(mktemp)
  $ snapshot > "$before"
  $ HOME="$cold/home" XDG_CACHE_HOME="$cold/cache" XDG_STATE_HOME="$cold/state" XDG_CONFIG_HOME="$cold/config" TMPDIR="$cold/tmp" OCAML_COLOR=never "$install_root/bin/verocaml" verify artifacts/positive.cmt --timeout-ms 60000 >/dev/null
  $ snapshot > "$after"
  $ cmp "$before" "$after"
  $ rm -f "$before" "$after"
  $ echo "cold installed verification filesystem unchanged"
  cold installed verification filesystem unchanged

Bare Exec, Tracked, and Ghost stack actuals retain the exact
one-way-forgetting trace and do not synthesize a mode descriptor.

  $ ./rank_backed_specifications_tool.exe modes artifacts/positive.cmt | grep '^forgetting-edge .*caller=verified#4' | sed -E 's/.*incoming=([^ ]+).*formal-mode=([^ ]+).*boundary-result-mode=([^ ]+).*edges=([0-9]+).*synthetic-ghost=([0-9]+).*synthetic-tracked=([0-9]+).*/incoming=\1 formal=\2 result=\3 edges=\4 synthetic-ghost=\5 synthetic-tracked=\6/'
  incoming=Exec formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
  incoming=Tracked formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
  incoming=Ghost formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0

Mixed rank domains and mutable/shared outer carriers reject in semantic
validation.  Generic profile/actual mismatch, references, functions, objects,
foreign carriers, and an abstract carrier reject at the authenticated adapter
boundary. A first-order parametric specification remains accepted without
acquiring finite/rank authority or entering legacy clone lowering. A readable
retained CMT whose recursive
type and trusted external body come from another unit supplies no local rank
authority.

  $ for n in mixed_domains mutable_carrier shared_carrier; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        3 semantic rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  $ for n in profile_mismatch reference_carrier function_carrier object_carrier foreign_carrier abstract_carrier; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        6 adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  $ retained polymorphic_carrier
  $ ./rank_backed_specifications_tool.exe parametric-no-rank artifacts/polymorphic_carrier.cmt
  parametric accepted binders=1 clones=0 finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  $ retained imported_provider
  $ retained imported_client
  $ ./rank_backed_specifications_tool.exe reject artifacts/imported_client.cmt
  adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0

Raw copies, forged record-shaped snapshots, stale snapshots, and same-name
cross-unit replay cannot reuse the process-private issued domain.

  $ retained same_name_a
  $ retained same_name_b
  $ ./rank_backed_specifications_tool.exe raw-attacks artifacts/same_name_a.cmt artifacts/same_name_b.cmt
  raw-copy rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  forged-rank-record rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  stale-snapshot rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  cross-unit-same-name rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0

Shape admission is not finite-value authority.  The exact stack.top call,
aliases, rebinding, branch joins, and nonrecursive result laundering all reject
before recursive/rank lowering, backend construction, or solver creation.
Direct and indirect immutable runtime cycles are rejected at the existing
frontend boundary and never become rank facts.

  $ for n in no_finite_call no_finite_alias no_finite_launder no_finite_rebinding no_finite_branch_launder; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        3 adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
        2 lowering rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  $ for n in runtime_cycle runtime_indirect_cycle; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        2 adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
