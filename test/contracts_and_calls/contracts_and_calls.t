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

  $ grep -E '^function |^  vc .* (assertion|postcondition|call-precondition|arithmetic-)' artifacts/first.vir
  function increment#0 mode=exec body=checked-typedtree:annotated_contracts.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=add @ annotated_contracts.ml:5:2-5:7
    vc 1 arithmetic-upper operation=add @ annotated_contracts.ml:5:2-5:7
    vc 2 postcondition ordinal=0 declaration=annotated_contracts.ml:3:2-4:35 @ annotated_contracts.ml:3:2-4:35
  function call_increment#1 mode=exec body=checked-typedtree:annotated_contracts.ml policy=default-linear/default-z3
    vc 0 assertion ordinal=0 @ annotated_contracts.ml:10:2-10:50
    vc 1 call-precondition callee=increment#0 ordinal=0 declaration=annotated_contracts.ml:2:2-2:52 call=annotated_contracts.ml:11:2-11:13 @ annotated_contracts.ml:11:2-11:13
    vc 2 postcondition ordinal=0 declaration=annotated_contracts.ml:9:2-9:46 @ annotated_contracts.ml:9:2-9:46
  function classify#2 mode=exec body=checked-typedtree:annotated_contracts.ml policy=default-linear/default-z3
    vc 0 postcondition ordinal=0 declaration=annotated_contracts.ml:14:2-14:47 @ annotated_contracts.ml:14:2-14:47
    vc 1 postcondition ordinal=0 declaration=annotated_contracts.ml:14:2-14:47 @ annotated_contracts.ml:14:2-14:47
    vc 2 postcondition ordinal=0 declaration=annotated_contracts.ml:14:2-14:47 @ annotated_contracts.ml:14:2-14:47

  $ ./contracts_and_calls_tool.exe solve artifacts/annotated_contracts.cmt
  increment: verified (3 obligations, 1 exits)
  call_increment: verified (3 obligations, 1 exits)
  classify: verified (3 obligations, 3 exits)
