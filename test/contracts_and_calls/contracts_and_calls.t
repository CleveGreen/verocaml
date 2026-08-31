  $ ./contracts_and_calls_tool.exe unit
  contracts: valid and false assertions/postconditions, logical arithmetic, entry-old
  calls: preconditions before summaries, fresh ranges, argument safety, contractless nondeterminism
  matches: ordered integer/Boolean/tuple cases, scoped guards, and runtime guard safety
  errors: missing summaries and missing decreases are explicit

Compile an ordinary annotated OxCaml implementation through the authoritative
PPX and no-op ghost runtime, then consume its real CMT twice.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/annotated_contracts.cmo fixtures/annotated_contracts.ml
  $ ./contracts_and_calls_tool.exe dump artifacts/annotated_contracts.cmt > artifacts/first.vir
  $ ./contracts_and_calls_tool.exe dump artifacts/annotated_contracts.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir

The authenticated ghost prefix becomes functional VCs, while runtime
arithmetic and call-argument arithmetic retain checked-safety VCs. Contract
arithmetic is logical and therefore adds no extra safety pair.
