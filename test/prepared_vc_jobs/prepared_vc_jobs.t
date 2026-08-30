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

The CLI rejects every malformed, missing, duplicate, nonpositive, and
overflowing explicit thread count before opening the input.

  $ for value in 0 -1 nope 999999999999999999999999999; do OCAML_COLOR=never ../../src/verocaml.exe verify /definitely/missing.cmt --threads "$value" 2>&1; echo exit=$?; done
  verocaml: error[VERO_CLI] --threads requires a positive integer
  exit=2
  verocaml: error[VERO_CLI] --threads requires a positive integer
  exit=2
  verocaml: error[VERO_CLI] --threads requires a positive integer
  exit=2
  verocaml: error[VERO_CLI] --threads requires a positive integer
  exit=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify /definitely/missing.cmt --threads 1 --threads 2 2>&1; echo exit=$?
  verocaml: error[VERO_CLI] --threads may be specified only once
  exit=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify /definitely/missing.cmt --threads 2>&1; echo exit=$?
  verocaml: error[VERO_CLI] unknown or incomplete option --threads
  exit=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify /definitely/missing.cmt --threads 999999 2>&1 | sed -E 's/runtime maximum [0-9]+/runtime maximum N/'; echo exit=${PIPESTATUS[0]}
  verocaml: error[VERO_CLI] threads=999999 exceeds runtime maximum N
  exit=2
