The release fixtures are compiled with retained, type-checked ghost sidecars.
Ordinary execution remains unaffected because this flag is verifier-only.

  $ mkdir artifacts
  $ compile_keep () { ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_plain () { ocamlc -w -A -alert -all -bin-annot -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_keep stack_demo
  $ compile_keep pure_verified
  $ compile_keep false_postcondition
  $ compile_keep wrong_mutation_postcondition
  $ compile_keep push_without_upper_bound
  $ compile_keep drop_without_nonemptiness
  $ compile_keep drain_without_decrement
  $ compile_plain aliased_write
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/input_signature.cmi fixtures/input_signature.mli
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/stack_demo_erased.cmo fixtures/stack_demo.ml
  $ test -f artifacts/stack_demo_erased.cmt

The real recursive mutable stack verifies push, drop, and drain. Recursive
aggregate types remain opaque, each state operation returns the unique
parameter, drain has the complete direct-recursion obligations, and a wrapper
consumes its returned unique state through the verified summary.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000
  verocaml: trusted external body trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 call=fixtures/stack_demo.ml:47:10-47:27 requires=1 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 requires=1 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/stack_demo.cmt functions=5 obligations=92 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/pure_verified.cmt --solver z3 --timeout-ms 60000
  verocaml: verified file=artifacts/pure_verified.cmt functions=2 obligations=12
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/pure_verified.cmt --solver z3 --timeout-ms 60000 --rlimit 3000000
  verocaml: verified file=artifacts/pure_verified.cmt functions=2 obligations=12
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/pure_verified.cmt --timeout-ms 60000 --rlimit 1 > artifacts/low.out 2>&1; low_status=$?; test "$low_status" = 3; grep -F 'result=unknown reason=resource-exhausted rlimit=1 timeout-ms=60000' artifacts/low.out >/dev/null; echo explicit-low-budget=resource-exhausted/exit-3
  explicit-low-budget=resource-exhausted/exit-3
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-sst artifacts/stack.first.sst --dump-vir artifacts/stack.first.vir
  verocaml: trusted external body trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 call=fixtures/stack_demo.ml:47:10-47:27 requires=1 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 requires=1 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/stack_demo.cmt functions=5 obligations=92 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-sst artifacts/stack.second.sst --dump-vir artifacts/stack.second.vir
  verocaml: trusted external body trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 call=fixtures/stack_demo.ml:47:10-47:27 requires=1 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic function=keep_only_first#1 declaration-span=fixtures/stack_demo.ml:23:0-40:26 witness-span=fixtures/stack_demo.ml:40:0-40:26 requires=1 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/stack_demo.cmt functions=5 obligations=92 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ cmp artifacts/stack.first.sst artifacts/stack.second.sst
  $ cmp artifacts/stack.first.vir artifacts/stack.second.vir
  $ awk '/^type (node|stack)/ {print; next} /^function / {show = $2 ~ /^(push|keep_only_first|drop|drain|drain_and_read)#/; if (show) print; next} show && /^  (returns-unique-parameter|body trusted-external-body)/ {print}' artifacts/stack.first.sst
  type node#0 representation=revealed @ stack_demo.ml:1:0-6:5
  type stack#1 representation=revealed @ stack_demo.ml:8:0-11:1
  function push#0 mode=exec recursive=false result=stack#1 policy=default-linear/default-z3 @ stack_demo.ml:13:0-21:3
    returns-unique-parameter 0
  function keep_only_first#1 mode=exec recursive=false result=stack#1 policy=default-linear/default-z3 @ stack_demo.ml:23:0-40:26
    returns-unique-parameter 0
    body trusted-external-body trust=axiomatic provenance=typedtree:stack_demo.ml declaration-span=stack_demo.ml:23:0-40:26 witness-span=stack_demo.ml:40:0-40:26 requires=1 ensures=1 body=unchecked
  function drop#3 mode=exec recursive=false result=stack#1 policy=default-linear/default-z3 @ stack_demo.ml:50:0-61:7
    returns-unique-parameter 0
  function drain#4 mode=exec recursive=true result=stack#1 policy=default-linear/default-z3 @ stack_demo.ml:63:0-81:15
    returns-unique-parameter 0
  function drain_and_read#5 mode=exec recursive=false result=int policy=default-linear/default-z3 @ stack_demo.ml:83:0-87:10
  $ grep -E '^function drain|entry-measure|recursive-call-(measure-nonnegative|strict-descent)' artifacts/stack.first.vir
  function drain#4 mode=exec body=checked-typedtree:stack_demo.ml policy=default-linear/default-z3
    vc 0 entry-measure-nonnegative declaration=stack_demo.ml:69:2-69:32 @ stack_demo.ml:69:2-69:32
    vc 4 recursive-call-measure-nonnegative callee=drain#4 declaration=stack_demo.ml:69:2-69:32 call=stack_demo.ml:81:8-81:15 @ stack_demo.ml:81:8-81:15
    vc 5 recursive-call-strict-descent callee=drain#4 declaration=stack_demo.ml:69:2-69:32 call=stack_demo.ml:81:8-81:15 @ stack_demo.ml:81:8-81:15
  function drain_and_read#5 mode=exec body=checked-typedtree:stack_demo.ml policy=default-linear/default-z3
  $ awk '/^function drain_and_read/{show=1} show && (/^function/ || /^  vc/) {print}' artifacts/stack.first.vir
  function drain_and_read#5 mode=exec body=checked-typedtree:stack_demo.ml policy=default-linear/default-z3
    vc 0 call-precondition callee=drain#4 ordinal=0 declaration=stack_demo.ml:64:2-64:36 call=stack_demo.ml:86:10-86:17 @ stack_demo.ml:86:10-86:17
    vc 1 postcondition ordinal=0 declaration=stack_demo.ml:85:2-85:46 @ stack_demo.ml:85:2-85:46
    vc 2 postcondition ordinal=0 declaration=stack_demo.ml:85:2-85:46 @ stack_demo.ml:85:2-85:46
    vc 3 postcondition ordinal=0 declaration=stack_demo.ml:85:2-85:46 @ stack_demo.ml:85:2-85:46

Counterexamples are exit 1 and retain the first failing VC per function. Models
project source bindings, and checked overflow names the operation, mathematical
result, and exact violated 63-bit bound.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/false_postcondition.cmt --timeout-ms 60000 2>&1
  verocaml: counterexample function=false_postcondition#0 vc=postcondition[0] span=fixtures/false_postcondition.ml:2:2-2:50 result=counterexample
    model: x#0=0
    model: result#1=0
  [1]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/wrong_mutation_postcondition.cmt --timeout-ms 60000 2>&1 | grep -E 'counterexample|model:'
  verocaml: counterexample function=wrong_mutation#0 vc=postcondition[0] span=fixtures/wrong_mutation_postcondition.ml:5:2-6:49 result=counterexample
    model: box#0=aggregate#2
    model: box.state#1=aggregate#4
    model: result#2=aggregate#4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/wrong_mutation_postcondition.cmt --timeout-ms 60000 >/dev/null 2>&1; echo $?
  1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/push_without_upper_bound.cmt --timeout-ms 60000 2>&1 | grep -E 'counterexample|overflow:|model: s.length'
  verocaml: counterexample function=push_without_upper_bound#0 vc=arithmetic-safety-upper span=fixtures/push_without_upper_bound.ml:9:14-9:26 result=counterexample
    overflow: operation=add mathematical-result=(+ (t1_stack_record.length#1 s.state$4) 1) violated-bound=upper maximum=4611686018427387903
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/push_without_upper_bound.cmt --timeout-ms 60000 >/dev/null 2>&1; echo $?
  1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/drop_without_nonemptiness.cmt --timeout-ms 60000 2>&1 | grep -E 'counterexample|model:'
  verocaml: counterexample function=call_drop_without_nonemptiness#1 vc=call-precondition[drop#0,0] span=fixtures/drop_without_nonemptiness.ml:21:10-21:16 result=counterexample
    model: s#0=aggregate#2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/drain_without_decrement.cmt --timeout-ms 60000 2>&1 | grep -E 'counterexample|model:'
  verocaml: counterexample function=drain_without_decrement#0 vc=recursive-call-strict-descent[drain_without_decrement#0] span=fixtures/drain_without_decrement.ml:8:30-8:55 result=counterexample
    model: s#0=aggregate#2

Known unsupported input and exact command-line shape errors are exit 2. These
remain distinct from counterexamples and supported-path internal failures.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/aliased_write.cmt --timeout-ms 60000 2>&1
  File "fixtures/aliased_write.ml", line 2, characters 42-56:
  Error: [VERO_UNSUPPORTED_MUTATION] mutation is reserved for unique-state lowering
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/input_signature.cmti --timeout-ms 60000 2>&1
  File "fixtures/input_signature.mli", line 1, characters 0-15:
  Error: [VERO_UNSUPPORTED_INTERFACE] interface typed trees are not supported
  [2]
  $ printf 'not a cmt\n' > artifacts/malformed.cmt
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/malformed.cmt --timeout-ms 60000 2>&1
  File "artifacts/malformed.cmt", line 1, characters 0-0:
  Error: [VERO_MALFORMED_INPUT] input is not a complete typed-tree artifact
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --solver cvc5 2>&1
  verocaml: error[VERO_CLI] unsupported solver "cvc5"; expected z3
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 0 2>&1
  verocaml: error[VERO_CLI] --timeout-ms requires a positive integer
  [2]
  $ test ! -e artifacts/does-not-exist.cmt
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/does-not-exist.cmt --timeout-ms 60000 --rlimit 2 --rlimit 3 2>&1
  verocaml: error[VERO_CLI] --rlimit may be specified only once
  [2]
  $ for value in 0x10 0b10 0o10 +1 -1 1_000 '' nope 0 999999999999999999999999999999999999; do OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/does-not-exist.cmt --timeout-ms 60000 --rlimit "$value" 2>&1; test $? = 2; done | sort | uniq -c
       10 verocaml: error[VERO_CLI] --rlimit requires a positive integer
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/does-not-exist.cmt --timeout-ms 60000 --rlimit 2>&1
  verocaml: error[VERO_CLI] unknown or incomplete option --rlimit
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/does-not-exist.cmt --timeout-ms 0x10 2>&1
  File "artifacts/does-not-exist.cmt", line 1, characters 0-0:
  Error: [VERO_INPUT_IO] input could not be read
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --unknown 2>&1
  verocaml: error[VERO_CLI] unknown or incomplete option --unknown
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-sst --dump-vir 2>&1
  verocaml: error[VERO_CLI] --dump-sst requires FILE
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-vir --dump-sst 2>&1
  verocaml: error[VERO_CLI] --dump-vir requires FILE
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-sst artifacts/one.sst --dump-sst artifacts/two.sst 2>&1
  verocaml: error[VERO_CLI] --dump-sst may be specified only once
  [2]
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stack_demo.cmt --timeout-ms 60000 --dump-vir artifacts/one.vir --dump-vir artifacts/two.vir 2>&1
  verocaml: error[VERO_CLI] --dump-vir may be specified only once
  [2]
  $ test ! -e ./--dump-sst && test ! -e ./--dump-vir
  $ test ! -e artifacts/one.sst && test ! -e artifacts/two.sst
  $ test ! -e artifacts/one.vir && test ! -e artifacts/two.vir
  $ OCAML_COLOR=never ../../src/verocaml.exe 2>&1
  verocaml: error[VERO_CLI] usage: verocaml verify FILE.ml|FILE.cmt|DIRECTORY [--solver z3] [--timeout-ms N] [--rlimit N] [--threads N] [--dependency FILE.cmt]... [--dump-sst FILE] [--dump-vir FILE]
         verocaml verify-project --root FILE.cmt FILE.cmi [--root FILE.cmt FILE.cmi]... [--dependency FILE.cmt FILE.cmi]... [--solver z3] [--timeout-ms N] [--rlimit N] [--threads N]
  [2]

Unknown is exercised deterministically only through the existing controlled
solver seam; production exposes no hidden switch and maps a backend Unknown to
exit 3 when no concrete counterexample exists.

  $ ./release_verification_tool.exe controlled-inconclusive
  controlled-inconclusive: result=unknown exit=3

Product output is styled by default. Only the individual diagnostic snapshot
processes above disabled color; no build or runtime-wide setting does so.

  $ OCAML_COLOR=always ../../src/verocaml.exe verify artifacts/pure_verified.cmt --timeout-ms 60000 | od -An -tx1 -v | tr -d ' \n' | grep -q '1b5b313b33326d'; echo $?
  0
