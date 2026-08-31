Private descriptor ownership is an explicit architecture exception.  Generated
compiler identities are normalized; ordinary verified status and functions are
owned by the grouped outcome host.

  $ mkdir artifacts
  $ retained () { name=$1; shift; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" "$@" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained positive
  $ ./rank_backed_specifications_tool.exe descriptors artifacts/positive.cmt | sed -E "s/compiler-uid=[^ ]+/compiler-uid=<uid>/; s/stack#[0-9]+/stack#N/g; s/node<'[0-9]+@node#[0-9]+>/node<BINDER>/g"
  ordinary-logical-descriptor stack#N=none
  parametric-rank-schema node<BINDER> compiler-uid=<uid> visibility=private

Mixed proof domains are an explicit malformed-SST outcome-gap and private
zero-resource exception; VERO-113 returns an unprojected verifier failure.

  $ retained proof_mixed_domains
  $ ./rank_backed_specifications_tool.exe reject artifacts/proof_mixed_domains.cmt
  semantic rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0

The structured authority snapshot and shape-only pipeline are specialist
resource exceptions.  They retain exact absence of recursive/rank authority
without moving SST/VIR renders or exact function/VC totals into ordinary
expectations.

  $ ./rank_backed_specifications_tool.exe authority artifacts/positive.cmt
  ordinary tags=22 selectors=62
  forbidden recursive-applications=0 recursive-equations=0 rank-projections=0 strict-rank-obligations=0 induction=0 reveals=0 fuel=0 invariant-facts=0
  $ cat > artifacts/shape_only_pipeline.ml <<'SHAPE'
  > type 'a node = Empty | Node of 'a * 'a node
  > let verified (node : int node) =
  >   [%verocaml.assert node = node];
  >   ()
  > SHAPE
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/shape_only_pipeline.cmo artifacts/shape_only_pipeline.ml
  $ ./rank_backed_specifications_tool.exe pipeline artifacts/shape_only_pipeline.cmt
  production-authority recursive-lowering=0
  pipeline status=verified functions=1 obligations=1 completed-callee-issued=0 completed-callee-consumed=0 finite-witnesses=0 finite-parents=0 finite-children=0 finite-result-witnesses=0 finite-finalizations=0 finite-consumptions=0 dependent-lowerings=0 dependent-backends=0 dependent-solvers=0 session-destroyed=true

Installed-surface manifests are explicit architecture/packaging ratchets.  They
remain specialist checks and are excluded from rank-backed-specifications-outcome-check.

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
  installed manifests paths=1000 interfaces=236 public-modules=27

Cold installed verification is a specialist filesystem-side-effect check.

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

The private one-way-forgetting trace is an explicit mode-authority exception.

  $ ./rank_backed_specifications_tool.exe modes artifacts/positive.cmt | grep '^forgetting-edge .*caller=verified#4' | sed -E 's/.*incoming=([^ ]+).*formal-mode=([^ ]+).*boundary-result-mode=([^ ]+).*edges=([0-9]+).*synthetic-ghost=([0-9]+).*synthetic-tracked=([0-9]+).*/incoming=\1 formal=\2 result=\3 edges=\4 synthetic-ghost=\5 synthetic-tracked=\6/'
  incoming=Exec formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
  incoming=Tracked formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
  incoming=Ghost formal=Ghost result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0

Private semantic/adapter barrier and zero-resource checks are specialist
exceptions.  Stable frontend codes that project cleanly are duplicated only in
the grouped host; malformed-SST and imported barriers remain here because they
have no VERO-113 Outcome.

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

The process-private domain replay matrix is an explicit authority exception.

  $ retained same_name_a
  $ retained same_name_b
  $ ./rank_backed_specifications_tool.exe raw-attacks artifacts/same_name_a.cmt artifacts/same_name_b.cmt
  raw-copy rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  forged-rank-record rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  stale-snapshot rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  cross-unit-same-name rejected; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0

Finite-authority and runtime-cycle zero-resource barriers are specialist
exceptions; the projected frontend subset also has canonical code cases.

  $ for n in no_finite_call no_finite_alias no_finite_launder no_finite_rebinding no_finite_branch_launder; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        3 adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
        2 lowering rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
  $ for n in runtime_cycle runtime_indirect_cycle; do retained "$n"; ./rank_backed_specifications_tool.exe reject "artifacts/$n.cmt"; done | sort | uniq -c
        2 adapter rejection; finite=0/0/0/0/0/0 recursive-lowering=0 backend=0 contexts=0 solvers=0
