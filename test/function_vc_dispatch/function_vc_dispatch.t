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

The worker wire remains private, modal, and free of coordinator/native authority.

  $ grep -E 'Sst|Vir|Z[.]t|Verification_session|receipt|manifest|callback|Z3[.](context|solver|model)|ref' ../../src/function_vc_worker_private.mli && exit 1 || :
  $ grep -E 'Parallel|scheduler|Atomic_array|Vc_solver_job_private' ../../src/function_vc_worker_private.ml && exit 1 || :
  $ grep -E 'Private_for_cli|resolve_threads' ../../src/interface_specification.mli && exit 1 || :
  $ grep -R 'verocaml_default_threads' ../../src/verocaml_bin.ml ../../src/physical_core_count_private.ml ../../src/physical_core_count_stubs.c | wc -l
  3
  $ grep -E 'parallel[.](kernel|scheduler)' ../../src/dune | wc -l
  2
  $ test "$(wc -l < ../../src/verocaml.ml)" -le 3
  $ grep -R -n 'exit (' ../../src/verocaml.ml
  1:let () = exit (Verocaml_bin.main Sys.argv)
  $ grep -E 'Interface_specification_loaded_private|Verification_driver_private|Solver_policy_private|Unix|Sys|open_(in|out)|exit' ../../src/verifier_service.mli && exit 1 || :
  $ test "$(wc -l < ../../src/verocaml_bin.ml)" -le 650
  $ test "$(wc -l < ../../src/verocaml_bin_render.ml)" -le 350

The decoded route has one static, acyclic three-stage owner graph.  It has no
caller edge, registration indirection, callback cell or parameter, registry,
functor, or module initializer.

  $ owners="../../src/interface_specification_environment_private.ml ../../src/interface_specification_candidate_private.ml ../../src/interface_specification_loaded_private.ml"
  $ interfaces="../../src/interface_specification_environment_private.mli ../../src/interface_specification_candidate_private.mli ../../src/interface_specification_loaded_private.mli"
  $ route_sources="$owners $interfaces ../../src/interface_specification.ml ../../src/verifier_service.ml"
  $ ocamldep -modules $owners ../../src/interface_specification.ml ../../src/verifier_service.ml ../../src/verocaml_bin.ml ../../src/verocaml_bin_render.ml > artifacts/vero075.modules
  $ edge () { grep "^$1:" artifacts/vero075.modules | grep -q " $2\\( \\|$\\)"; }
  $ no_edge () { ! grep "^$1:" artifacts/vero075.modules | grep -q " $2\\( \\|$\\)"; }
  $ edge ../../src/interface_specification_candidate_private.ml Interface_specification_environment_private
  $ edge ../../src/interface_specification_loaded_private.ml Interface_specification_candidate_private
  $ edge ../../src/interface_specification_loaded_private.ml Interface_specification_environment_private
  $ edge ../../src/interface_specification.ml Interface_specification_loaded_private
  $ edge ../../src/interface_specification.ml Interface_specification_environment_private
  $ edge ../../src/verifier_service.ml Interface_specification_loaded_private
  $ no_edge ../../src/interface_specification_environment_private.ml Interface_specification_candidate_private
  $ no_edge ../../src/interface_specification_environment_private.ml Interface_specification_loaded_private
  $ no_edge ../../src/interface_specification_candidate_private.ml Interface_specification_loaded_private
  $ no_edge ../../src/interface_specification.ml Interface_specification_candidate_private
  $ no_edge ../../src/verifier_service.ml Interface_specification
  $ no_edge ../../src/verifier_service.ml Interface_specification_environment_private
  $ no_edge ../../src/verifier_service.ml Interface_specification_candidate_private
  $ grep -E ' (Interface_specification|Verifier_service|Verocaml_bin|Verocaml_bin_render)( |$)' artifacts/vero075.modules | grep '_private[.]ml:' && exit 1 || :
  $ grep -E -i '\\b(Atomic|register|registration|callback|registry|registries)' $route_sources && exit 1 || :
  $ grep -E '\\bfunctor\\b' $route_sources | grep -v 'Mty_functor' && exit 1 || :
  $ grep -E '^[[:space:]]*let[[:space:]]+(_|[(][)])' $route_sources && exit 1 || :
  $ edge ../../src/verocaml_bin.ml Verifier_service
  $ private_modules="$(awk '/^[[:space:]]*[(]private_modules$/ { inside=1; next } inside { gsub(/[[:space:]()]/, ""); module=toupper(substr($0,1,1)) substr($0,2); print module; if (module == "Verification_driver_private") exit }' ../../src/dune)"
  $ for bin_module in ../../src/verocaml_bin.ml ../../src/verocaml_bin_render.ml; do dependencies="$(grep "^$bin_module:" artifacts/vero075.modules)"; for private_module in $private_modules; do if printf '%s\n' "$dependencies" | grep -q " $private_module\\( \\|$\\)"; then exit 1; fi; done; done
  $ echo "private-route-graph=facade/service->loaded->candidate/environment candidate->environment callbacks/registries/functors/initializers=absent"
  private-route-graph=facade/service->loaded->candidate/environment candidate->environment callbacks/registries/functors/initializers=absent

All new owners remain bounded, and the compiler parser measures every named
function binding rather than relying on textual declaration gaps.

  $ for file in $owners; do test "$(wc -l < "$file")" -lt 800; done
  $ ./vero075_structure_tool.exe $owners ../../src/verocaml_bin.ml ../../src/verocaml_bin_render.ml ../../src/verifier_service.ml ../../src/interface_specification.ml
  interface_specification_environment_private.ml=provider_of_root:185
  interface_specification_candidate_private.ml=embedded_public_surface:134
  interface_specification_loaded_private.ml=authenticate_loaded_with_policy:134
  verocaml_bin.ml=compile_source:88
  verocaml_bin_render.ml=verification_stderr_lines:75
  verifier_service.ml=verify_scope:68
  interface_specification.ml=load_inputs:16
