The pure frontier owner uses validated source ordinals, delays dependencies,
classifies blocked functions, and rejects ambiguous ordinals.

  $ ./function_vc_dispatch_tool.exe frontier
  frontier=ordered ready/waiting/blocked successor=later duplicate=rejected

The Linux default seam counts distinct allowed physical package/core pairs,
caps to the runtime, and takes the frozen fallback for every failed input.

  $ ./function_vc_dispatch_tool.exe physical
  physical=affinity/topology distinct=3 cap=2 fallbacks=affinity/empty/malformed/invalid

One function worker checks VCs serially and stops at its first nonverified VC.

  $ ./function_vc_dispatch_tool.exe worker
  worker=vcs-serial indices=0,1,2 first-nonverified=cutoff-3 correspondence=authenticated

Independent function callbacks overlap while the scheduler bound and full join
are preserved.

  $ ./function_vc_dispatch_tool.exe parallel
  parallel=overlap peak=2 bound=2 functions=4 slots=4 join=complete wall=recorded oversubscription=none

The real production frontier is serial at one thread, overlaps at two, and
preserves the complete canonical transcript across serial/default/2/higher.

  $ mkdir -p artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/independent_expensive_functions.cmo fixtures/independent_expensive_functions.ml
  $ ./function_vc_dispatch_tool.exe production artifacts/independent_expensive_functions.cmt
  production=parity status=verified transcript=byte-equivalent semantic-sst=lazy/explicit-memoized frontier-peak=1@1,2@2 scheduler=0@1,1/1@2,1/1@higher default=parity cleanup=zero

Retained recursive and receipt routes preserve raw-resource transcripts across
the same modes. Producer authority commits before dependent admission; failure
blocks the dependent, and every joined path cleans its scheduler and contexts.

  $ retained () { source=$1; name=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ retained ../recursive_specifications/fixtures/structural_positive.ml structural_positive
  $ retained ../private_receipt/fixtures/local_positive.ml local_positive
  $ retained ../private_receipt/fixtures/failing_callee.ml failing_callee
  $ ./function_vc_dispatch_tool.exe parity recursive-route artifacts/structural_positive.cmt
  parity=recursive-route status=verified serial/default/2/higher=identical rlimit=100000 scheduler/context=clean
  $ ./function_vc_dispatch_tool.exe parity receipt-verified artifacts/local_positive.cmt
  parity=receipt-verified status=verified serial/default/2/higher=identical rlimit=100000 scheduler/context=clean
  $ ./function_vc_dispatch_tool.exe parity receipt-failed artifacts/failing_callee.cmt
  parity=receipt-failed status=counterexample serial/default/2/higher=identical rlimit=100000 scheduler/context=clean
  $ ./function_vc_dispatch_tool.exe receipts artifacts/local_positive.cmt artifacts/failing_callee.cmt
  receipts=producer-commit-before-dependent failed-producer=blocked scheduler/context=clean

Lower injected failures win deterministically. A higher failure permits only
the already canonical lower commits; no later function effect is committed.

  $ ./function_vc_dispatch_tool.exe errors artifacts/independent_expensive_functions.cmt
  errors=materialization/preparation/worker lower=first higher=prior-canonical-commits no-later-effects scheduler/context=clean
