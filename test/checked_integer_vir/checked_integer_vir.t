  $ ./checked_integer_vir_tool.exe unit
  checked integer VIR unit checks: bounds, raw terms, ordering, handoff, provenance, ranges

The public VIR term boundary has no general symbolic multiplication or
division constructor. Wrapping, remainder, shifts, and bitwise expressions
are likewise absent after the SST firewall.

  $ if OCAML_COLOR=never ocamlfind ocamlc -package zarith -I ../../src/.verocaml_core.objs/byte -c fixtures/general_symbolic_multiply_term.ml > multiply.error 2>&1; then exit 1; fi
  $ tr -d '"' < multiply.error | grep -F 'Error: Unbound constructor Vir.Integer_multiply' > /dev/null && echo 'general symbolic multiply constructor absent'
  general symbolic multiply constructor absent
  $ if OCAML_COLOR=never ocamlfind ocamlc -package zarith -I ../../src/.verocaml_core.objs/byte -c fixtures/division_term.ml > division.error 2>&1; then exit 1; fi
  $ tr -d '"' < division.error | grep -F 'Error: Unbound constructor Vir.Integer_divide' > /dev/null && echo 'division constructor absent'
  division constructor absent

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/arithmetic_paths.cmo fixtures/arithmetic_paths.ml
  $ ./checked_integer_vir_tool.exe dump artifacts/arithmetic_paths.cmt > artifacts/first.vir
  $ ./checked_integer_vir_tool.exe dump artifacts/arithmetic_paths.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir

The real-CMT dump contains source-spanned lower/upper obligations, range
assumptions, and model projections. Guard facts remain path conditions rather
than being promoted to unconditional assumptions.

  $ grep -E '^function |^  vc |^    path$|^      \(< x\$0 4611686018427387903\)|^      \(< -4611686018427387904 x\$0\)|^      \(>= x\$0 0\)|^      \(not \(>= x\$0 0\)\)|^    mathematical-result ' artifacts/first.vir
  function guarded_add#0 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=add @ arithmetic_paths.ml:2:40-2:45
      mathematical-result (+ x$0 1)
      path
        (< x$0 4611686018427387903)
    vc 1 arithmetic-upper operation=add @ arithmetic_paths.ml:2:40-2:45
      mathematical-result (+ x$0 1)
      path
        (< x$0 4611686018427387903)
      path
        (< x$0 4611686018427387903)
      path
  function guarded_subtract#1 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=subtract @ arithmetic_paths.ml:5:41-5:46
      mathematical-result (- x$0 1)
      path
        (< -4611686018427387904 x$0)
    vc 1 arithmetic-upper operation=subtract @ arithmetic_paths.ml:5:41-5:46
      mathematical-result (- x$0 1)
      path
        (< -4611686018427387904 x$0)
      path
        (< -4611686018427387904 x$0)
      path
  function unguarded_add#2 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=add @ arithmetic_paths.ml:7:30-7:35
      mathematical-result (+ x$0 1)
      path
    vc 1 arithmetic-upper operation=add @ arithmetic_paths.ml:7:30-7:35
      mathematical-result (+ x$0 1)
      path
      path
  function unguarded_subtract#3 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=subtract @ arithmetic_paths.ml:8:35-8:40
      mathematical-result (- x$0 1)
      path
    vc 1 arithmetic-upper operation=subtract @ arithmetic_paths.ml:8:35-8:40
      mathematical-result (- x$0 1)
      path
      path
  function arithmetic_branches#4 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=add @ arithmetic_paths.ml:9:51-9:56
      mathematical-result (+ x$0 1)
      path
        (>= x$0 0)
    vc 1 arithmetic-upper operation=add @ arithmetic_paths.ml:9:51-9:56
      mathematical-result (+ x$0 1)
      path
        (>= x$0 0)
    vc 2 arithmetic-lower operation=subtract @ arithmetic_paths.ml:9:62-9:67
      mathematical-result (- x$0 1)
      path
        (not (>= x$0 0))
    vc 3 arithmetic-upper operation=subtract @ arithmetic_paths.ml:9:62-9:67
      mathematical-result (- x$0 1)
      path
        (not (>= x$0 0))
      path
        (>= x$0 0)
      path
        (not (>= x$0 0))
  function intermediate#5 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=add @ arithmetic_paths.ml:10:29-10:36
      mathematical-result (+ x$0 1)
      path
    vc 1 arithmetic-upper operation=add @ arithmetic_paths.ml:10:29-10:36
      mathematical-result (+ x$0 1)
      path
    vc 2 arithmetic-lower operation=add @ arithmetic_paths.ml:10:29-10:40
      mathematical-result (+ (+ x$0 1) 1)
      path
    vc 3 arithmetic-upper operation=add @ arithmetic_paths.ml:10:29-10:40
      mathematical-result (+ (+ x$0 1) 1)
      path
      path
  function negate#6 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=negate @ arithmetic_paths.ml:11:23-11:25
      mathematical-result (- x$0)
      path
    vc 1 arithmetic-upper operation=negate @ arithmetic_paths.ml:11:23-11:25
      mathematical-result (- x$0)
      path
      path
  function scale#7 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=multiply-constant(3) @ arithmetic_paths.ml:12:22-12:27
      mathematical-result (* 3 x$0)
      path
    vc 1 arithmetic-upper operation=multiply-constant(3) @ arithmetic_paths.ml:12:22-12:27
      mathematical-result (* 3 x$0)
      path
      path
  function standard#8 mode=exec body=checked-typedtree:arithmetic_paths.ml policy=default-linear/default-z3
    vc 0 arithmetic-lower operation=successor @ arithmetic_paths.ml:13:25-13:31
      mathematical-result (+ x$0 1)
      path
    vc 1 arithmetic-upper operation=successor @ arithmetic_paths.ml:13:25-13:31
      mathematical-result (+ x$0 1)
      path
    vc 2 arithmetic-lower operation=absolute-value @ arithmetic_paths.ml:13:39-13:46
      mathematical-result (abs x$0)
      path
    vc 3 arithmetic-upper operation=absolute-value @ arithmetic_paths.ml:13:39-13:46
      mathematical-result (abs x$0)
      path
    vc 4 arithmetic-lower operation=predecessor @ arithmetic_paths.ml:13:34-13:46
      mathematical-result (- (abs x$0) 1)
      path
    vc 5 arithmetic-upper operation=predecessor @ arithmetic_paths.ml:13:34-13:46
      mathematical-result (- (abs x$0) 1)
      path
    vc 6 arithmetic-lower operation=add @ arithmetic_paths.ml:13:25-13:46
      mathematical-result (+ (+ x$0 1) (- (abs x$0) 1))
      path
    vc 7 arithmetic-upper operation=add @ arithmetic_paths.ml:13:25-13:46
      mathematical-result (+ (+ x$0 1) (- (abs x$0) 1))
      path
      path
