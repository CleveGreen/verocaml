Both local bridge entries return exact immutable per-attempt telemetry without
touching the legacy process-global observations, including typed result,
capability/translation error, and post-context exception cleanup paths.

  $ ./prepared_vc_jobs_tool.exe local-bridge
  local-bridge vir=verified/counterexample/unknown/error query=verified/counterexample/unknown/error global=unchanged cleanup=balanced

Ordinary, structural-rank, and logical-aggregate jobs share one opaque owner.
Only ordinary returns one additive backend contribution; malformed VIR is
rejected before a job exists and equal-policy jobs retain independent
telemetry.

  $ ./prepared_vc_jobs_tool.exe prepared-job
  prepared ordinary=projected/contribution-once direct=structural/logical/no-contribution indices=authenticated equal-policy=isolated malformed=zero-job

Resource exhaustion retires exactly the attempted local context and solver.

  $ ./prepared_vc_jobs_tool.exe resource
  resource outcome=resource-exhausted contexts/solvers/resets/cleaned=1/1/1/1 live=0

The recursive initial query, copied exact retry control, and bounded ground
classifier remain one job. Changing the process-global control after snapshot
does not change the job, both native attempts stay local, ground creates no
third solver, and no ordinary contribution is returned.

  $ mkdir -p artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/nullary_recursive_symbolic_positive.cmo ../proof_body_assertions/fixtures/nullary_recursive_symbolic_positive.ml
  $ ./prepared_vc_jobs_tool.exe recursive-local artifacts/nullary_recursive_symbolic_positive.cmt
  recursive routes=initial/retry/ground controls=copied telemetry=2/2/2/2 global=unchanged contribution=zero

The same recursive initial/retry/ground sequence traverses the production
coordinator. Initial and retry telemetry are committed separately in attempt
order; the ground classifier contributes no third native commit.

  $ ./prepared_vc_jobs_tool.exe production-recursive artifacts/nullary_recursive_symbolic_positive.cmt
  production-recursive status=inconclusive commits=0.0,0.1 native=2 ground-attempts=1/zero-commit session-destroyed=true

The production coordinator authenticates and commits a multi-obligation
sequence in canonical order. The first counterexample prevents the later job
and telemetry commit; the dynamic commit observer sees one local commit for
each actual ordinary attempt.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/builtin_assert_positive_matrix.cmo ../proof_body_assertions/fixtures/builtin_assert_positive_matrix.ml
  $ ./prepared_vc_jobs_tool.exe production-coordinator artifacts/builtin_assert_positive_matrix.cmt
  production-coordinator results=40:verified,41:counterexample cutoff=42-uncommitted telemetry-commits=2 order=canonical backend=2

A failed receipt producer dynamically traverses the production pipeline and
coordinator, destroys its session, issues no receipt, and schedules no
dependent lowering, backend, or solver work.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/failing_callee.cmo ../private_receipt/fixtures/failing_callee.ml
  $ ./prepared_vc_jobs_tool.exe production-failed-producer artifacts/failing_callee.cmt
  production-failed-producer status=counterexample receipts=0/0 dependent=0/0/0 session-destroyed=true

No coordinator authority, callback, native Z3 value, mutable reference, or
scheduler surface crosses the private job interface.

  $ grep -E 'Verification_session|proof_activation|callback|Z3[.]|Smtml[.]|ref|Domain|Parallel|Thread|scheduler' ../../src/vc_solver_job_private.mli && exit 1 || :
  $ sed -n '/^type ground_payload =/,/^type prepared_query =/p' ../../src/recursive_spec_encoding.ml | grep -E 'Logic_ir[.]query|ref|authentication_token' && exit 1 || :
  $ sed -n '/^type prepared_job =/,/^let direct_requirements/p' ../../src/vc_solver_job_private.ml | grep -E 'Logic_ir[.]query|ref|Verification_session|callback|Z3[.]context|Z3[.]Solver' && exit 1 || :
