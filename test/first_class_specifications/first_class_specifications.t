First-class specification functions use one canonical ghost arrow and remain
stable across retained source, CMT, repetition, and worker count.

  $ mkdir artifacts
  $ retained () { name=$1; source=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ for name in core recursive_collections quantified broadcast_symbolic; do retained "$name" "fixtures/$name.ml"; done
  $ for name in core recursive_collections quantified; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 1 --timeout-ms 5000 --rlimit 100000 | sed -E 's/file=[^ ]+/file=<fixture>/'; done
  verocaml: verified file=<fixture> functions=6 obligations=38
  verocaml: verified file=<fixture> functions=1 obligations=19
  verocaml: verified file=<fixture> functions=1 obligations=1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/broadcast_symbolic.cmt --threads 1 --timeout-ms 5000 --rlimit 100000 2>&1 | grep -E '^(verocaml: trusted|verocaml: verified)' | sed -E 's/file=[^ ]+/file=<fixture>/; s/(declaration|witness)-span=[^ ]+/\1-span=<span>/g'
  verocaml: trusted external body trust=axiomatic mode=proof call-form=broadcast function=image_axiom#5 declaration-span=<span> witness-span=<span> call=fixtures/broadcast_symbolic.ml:17:0-24:6 requires=0 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic mode=proof function=image_axiom#5 declaration-span=<span> witness-span=<span> requires=0 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=<fixture> functions=2 obligations=5 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ for route in fixtures/core.ml artifacts/core.cmt; do tag=$(basename "$route"); for threads in 1 2; do OCAML_COLOR=never ../../src/verocaml.exe verify "$route" --threads "$threads" --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$tag.$threads.sst" --dump-vir "artifacts/$tag.$threads.vir" >"artifacts/$tag.$threads.out"; done; cmp "artifacts/$tag.1.out" "artifacts/$tag.2.out"; cmp "artifacts/$tag.1.sst" "artifacts/$tag.2.sst"; cmp "artifacts/$tag.1.vir" "artifacts/$tag.2.vir"; done
  $ sed '/^instance-modes$/,$d' artifacts/core.ml.1.sst > artifacts/source.semantic.sst
  $ sed '/^instance-modes$/,$d' artifacts/core.cmt.1.sst > artifacts/cmt.semantic.sst
  $ cmp artifacts/source.semantic.sst artifacts/cmt.semantic.sst
  $ cmp artifacts/core.ml.1.vir artifacts/core.cmt.1.vir
  $ echo "source-cmt semantic-sst=true vir=true threads=1/2 repeat=true"
  source-cmt semantic-sst=true vir=true threads=1/2 repeat=true

Concrete calls instantiate generic Spec bodies before logical evaluation, and
integer branches passed to symbolic operations remain authenticated formula
terms. Both routes produce identical VIR across source, retained CMT, and workers.

  $ cat > artifacts/parametric_branching.ml <<'EOF'
  > [%%verocaml.symbolic val project : 'a -> 'a]
  > [%%verocaml.symbolic val image : (int -> int) -> int -> int]
  > let generic_project (value : 'a) : 'a = project value [@@verocaml.spec]
  > let choose (index : int) : int = if index = 4 then 42 else index [@@verocaml.spec]
  > let image_axiom (f : int -> int) (index : int) : unit =
  >   [%verocaml.ensures fun _ -> (image f index) [@trigger] = f index]; ()
  > [@@verocaml.axiom] [@@verocaml.broadcast]
  > [@@@verocaml.activate [image_axiom]]
  > let verify (value : int) : int =
  >   [%verocaml.assert generic_project value = project value];
  >   [%verocaml.assert image (fun index -> choose index) 4 = 42];
  >   [%verocaml.assert image (fun index -> choose index) 3 = 3];
  >   [%verocaml.ensures fun result -> result = value]; value
  > EOF
  $ retained parametric_branching artifacts/parametric_branching.ml
  $ for route in artifacts/parametric_branching.ml artifacts/parametric_branching.cmt; do tag=$(basename "$route"); for threads in 1 2; do OCAML_COLOR=never ../../src/verocaml.exe verify "$route" --threads "$threads" --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$tag.$threads.sst" --dump-vir "artifacts/$tag.$threads.vir" >"artifacts/$tag.$threads.out"; done; cmp "artifacts/$tag.1.out" "artifacts/$tag.2.out"; cmp "artifacts/$tag.1.vir" "artifacts/$tag.2.vir"; done
  $ cmp artifacts/parametric_branching.ml.1.vir artifacts/parametric_branching.cmt.1.vir
  $ grep -q 'specification-call generic_project#.*type-arguments=\[int\] : int' artifacts/parametric_branching.ml.1.sst
  $ grep -q 'project\[int\](value\$0):int' artifacts/parametric_branching.ml.1.vir
  $ grep -Eq '\(ite \(= \$spec_lambda_arg\$[0-9]+ 4\) 42 \$spec_lambda_arg\$[0-9]+\)' artifacts/parametric_branching.ml.1.vir
  $ ! grep -Eq 'Param_.*generic_project' artifacts/parametric_branching.ml.1.vir
  $ grep '^verocaml: verified' artifacts/parametric_branching.ml.1.out | sed -E 's/file=[^ ]+/file=<fixture>/'
  verocaml: verified-with-trusted-axioms file=<fixture> functions=1 obligations=4 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ echo "generic-body=int formula-ite=authenticated source-cmt=true threads=1/2"
  generic-body=int formula-ite=authenticated source-cmt=true threads=1/2

The portable SST is visibly staged: returned lambdas and named values are
carriers, while every logical application is the typed f-x Spec_apply head.

  $ ./first_class_specifications_tool.exe sst artifacts/core.cmt > artifacts/core.sst
  $ printf "arrows=%s lambdas=%s applies=%s named-values=%s\n" "$(grep -c '\$verocaml.spec-function<' artifacts/core.sst)" "$(grep -c '\$verocaml.spec-lambda:' artifacts/core.sst)" "$(grep -c 'specification-call \$verocaml.spec-apply:' artifacts/core.sst)" "$(grep -Ec 'specification-call (increment|adder|plus|labelled_plus).*: \$verocaml.spec-function<' artifacts/core.sst)"
  arrows=99 lambdas=8 applies=35 named-values=17
  $ ./first_class_specifications_tool.exe callback-abi
  relation=true result=true

Recursive map/predicates/folds, function quantifiers, and application-headed
patterns retain semantically necessary function applications.

  $ ./first_class_specifications_tool.exe sst artifacts/recursive_collections.cmt > artifacts/recursive.sst
  $ ./first_class_specifications_tool.exe sst artifacts/quantified.cmt > artifacts/quantified.sst
  $ printf "recursive apply=%s calls=%s quantifiers=%s trigger-apply=%s\n" "$(grep -c 'specification-call \$verocaml.spec-apply:' artifacts/recursive.sst)" "$(grep -Ec 'specification-call (map|all|any|fold_left|fold_right)' artifacts/recursive.sst)" "$(grep -Ec '^    (forall|exists) ' artifacts/quantified.sst)" "$(grep -c 'specification-call \$verocaml.spec-apply:' artifacts/quantified.sst)"
  recursive apply=7 calls=10 quantifiers=2 trigger-apply=5

Polymorphic proved and trusted broadcasts match the structural arrow pattern
against literal f-x occurrences and share deterministic insertion order.

  $ ./first_class_specifications_tool.exe structural artifacts/broadcast_symbolic.cmt > artifacts/broadcast.first
  $ ./first_class_specifications_tool.exe structural artifacts/broadcast_symbolic.cmt > artifacts/broadcast.second
  $ cmp artifacts/broadcast.first artifacts/broadcast.second
  $ sed -E 's/qid=[^ ]+ skid=[^ ]+/qid=<stable> skid=<stable>/' artifacts/broadcast.first | awk '/^status=|^resources /{print} /^vc function=symbolic_functions/{v++; inserted+=$NF} /^insert /{if ($0 ~ /trusted=false/) proved++; if ($0 ~ /trusted=true/) trusted++; if (index($0,"vector=[int,int]")) concrete++} END{printf "symbolic-vcs=%d proved=%d trusted=%d concrete=%d\n",v,proved,trusted,concrete}'
  status=verified functions=2 obligations=5 destroyed=true
  resources backend=5 contexts=5 solvers=5 resets=5 cleaned=5 live=0
  symbolic-vcs=4 proved=4 trusted=4 concrete=8
  $ grep -o 'f value \[@trigger\]' fixtures/broadcast_symbolic.ml | sed 's/^/trigger-source: /'
  trigger-source: f value [@trigger]
  trigger-source: f value [@trigger]
  $ grep -c 'specification-call \$verocaml.spec-apply:' artifacts/broadcast_symbolic.cmt >/dev/null 2>&1; test $? -eq 1

The broadcast matrix covers inactive, mismatched, duplicate, exact, and the
shared sixteen-instance cap without a second retry or solver policy.

  $ cat > artifacts/generate_broadcast_matrix.py <<'PY'
  > from pathlib import Path
  > p = Path("artifacts")
  > theorem = """let lemma (f : int -> int) (x : int) : unit =
  >   [%verocaml.ensures fun _ -> ((f x) [@trigger]) = f x]; ()
  > [@@verocaml.proof] [@@verocaml.broadcast]
  > """
  > increment = "let increment (x:int):int = x + 1 [@@verocaml.spec]\n"
  > target = """let target (x:int):int =
  >   [%verocaml.assert (let f = increment in f x = f x)];
  >   [%verocaml.ensures fun result -> result = x]; x
  > """
  > (p/"broadcast_exact.ml").write_text(increment + theorem + "[@@@verocaml.activate [lemma]]\n" + target)
  > (p/"broadcast_duplicate.ml").write_text(increment + theorem + "[@@@verocaml.activate [lemma; lemma]]\n" + target)
  > (p/"broadcast_inactive.ml").write_text(increment + theorem + target)
  > mismatch = theorem.replace("int -> int", "bool -> bool").replace("(x : int)", "(x : bool)")
  > (p/"broadcast_mismatch.ml").write_text(increment + mismatch + "[@@@verocaml.activate [lemma]]\n" + target)
  > def capped(count):
  >   rows = ["let increment (x:int):int = x + 1 [@@verocaml.spec]", ""]
  >   for i in range(count):
  >     rows += [f"let lemma_{i} (f:int->int) (x:int):unit =",
  >              "  [%verocaml.ensures fun _ -> ((f x) [@trigger]) = f x]; ()",
  >              "[@@verocaml.proof] [@@verocaml.broadcast]", ""]
  >   rows += ["[@@@verocaml.activate [" + "; ".join(f"lemma_{i}" for i in range(count)) + "]]",
  >            "let target (x:int):int =",
  >            "  [%verocaml.assert (let f = increment in f x = f x)];",
  >            "  [%verocaml.ensures fun result -> result = x]; x"]
  >   return "\n".join(rows) + "\n"
  > (p/"broadcast_16.ml").write_text(capped(16))
  > (p/"broadcast_17.ml").write_text(capped(17))
  > PY
  $ python3 artifacts/generate_broadcast_matrix.py
  $ for name in broadcast_exact broadcast_duplicate broadcast_inactive broadcast_mismatch broadcast_16 broadcast_17; do retained "$name" "artifacts/$name.ml"; done
  $ for name in broadcast_exact broadcast_duplicate broadcast_inactive broadcast_mismatch broadcast_16; do ./first_class_specifications_tool.exe structural "artifacts/$name.cmt" >"artifacts/$name.out"; done
  $ for name in broadcast_exact broadcast_duplicate broadcast_inactive broadcast_mismatch broadcast_16; do printf "%s active/inserted=" "$name"; awk '/^vc function=target/{a=$0; sub(/^.*active=/,"",a); sub(/ inserted=/,"/",a); print a}' "artifacts/$name.out" | sort -u; done
  broadcast_exact active/inserted=1/1
  broadcast_duplicate active/inserted=1/1
  broadcast_inactive active/inserted=0/0
  broadcast_mismatch active/inserted=1/0
  broadcast_16 active/inserted=16/16
  $ set +e; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/broadcast_17.cmt --threads 1 --timeout-ms 5000 --rlimit 100000 >artifacts/broadcast_17.out 2>&1; cap_status=$?; set -e; test "$cap_status" -eq 2
  $ grep -o 'broadcast instance cap exceeded: 17 > 16' artifacts/broadcast_17.out
  broadcast instance cap exceeded: 17 > 16

Function equality is identity equality only. Independent pointwise-equal
closures and same-site closures with unconstrained captures reach the solver
and fail ordinarily; neither is rejected as policy nor proved by injectivity.

  $ cat > artifacts/false_controls.ml <<'EOF'
  > let factory (x:int) : int -> int = fun _ -> x [@@verocaml.spec]
  > let pointwise (x:int) : int =
  >   [%verocaml.assert (fun y -> y + 1) = (fun y -> 1 + y)];
  >   [%verocaml.ensures fun result -> result = x]; x
  > let captures (x:int) (y:int) : int =
  >   [%verocaml.assert factory x <> factory y];
  >   [%verocaml.ensures fun result -> result = x]; x
  > EOF
  $ retained false_controls artifacts/false_controls.ml
  $ set +e; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/false_controls.cmt --threads 1 --timeout-ms 5000 --rlimit 100000 >artifacts/false.out 2>&1; false_status=$?; set -e; test "$false_status" -eq 1
  $ grep '^verocaml: counterexample' artifacts/false.out | sed -E 's/#[0-9]+/#ID/; s/ span=.*$/ result=counterexample/'
  verocaml: counterexample function=pointwise#ID vc=assertion[0] result=counterexample
  verocaml: counterexample function=captures#ID vc=assertion[0] result=counterexample

Structurally excluded cases reject before backend or solver creation. The
matrix includes mutable capture, optional stages, closure triggers, local
recursion, function storage, rank-2 storage, and callback coercion.

  $ for name in negative_capture negative_policy negative_trigger; do retained "$name" "fixtures/$name.ml"; ./first_class_specifications_tool.exe diagnostic "artifacts/$name.cmt" | sed -E 's/code=[^ ]+/code=<closed>/'; done
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0
  stage=sst code=<closed> backend=0 contexts=0 solvers=0 live=0
  stage=engine code=<closed> backend=0 contexts=0 solvers=0 live=0
  $ cat > artifacts/storage.ml <<'EOF'
  > let increment (x:int):int = x + 1 [@@verocaml.spec]
  > let use (x:int):int =
  >   [%verocaml.assert let pair = (increment, increment) in
  >     let (f, _g) = pair in f x = x + 1]; x
  > EOF
  $ cat > artifacts/local_recursive.ml <<'EOF'
  > let use (x:int):int =
  >   [%verocaml.assert let rec loop (y:int):int =
  >     if y = 0 then x else loop (y - 1) in loop x = x]; x
  > EOF
  $ cat > artifacts/rank2.ml <<'EOF'
  > type rank2 = { run : 'a. 'a -> 'a }
  > let use (x:int):int =
  >   [%verocaml.assert let stored = { run = fun value -> value } in
  >     stored.run x = x]; x
  > EOF
  $ for name in storage local_recursive rank2; do retained "$name" "artifacts/$name.ml"; ./first_class_specifications_tool.exe diagnostic "artifacts/$name.cmt" | sed -E 's/code=[^ ]+/code=<closed>/'; done
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0
  $ cat > artifacts/callback_coercion.ml <<'EOF'
  > let invoke (f : int -> int) (x : int) = f x
  > let increment (x:int):int = x + 1 [@@verocaml.spec]
  > let bad (x:int) = invoke increment x
  > EOF
  $ retained callback_coercion artifacts/callback_coercion.ml
  $ ./first_class_specifications_tool.exe diagnostic artifacts/callback_coercion.cmt | sed -E 's/code=[^ ]+/code=<closed>/'
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0

Specification-function values are same-unit-only. A retained provider exposes
no callable value to a separately typed consumer.

  $ retained negative_cross_unit fixtures/negative_cross_unit.ml
  $ cat > artifacts/cross_unit_consumer.ml <<'EOF'
  > let use (x:int):int =
  >   [%verocaml.assert Negative_cross_unit.increment x = x + 1]; x
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/cross_unit_consumer.cmo artifacts/cross_unit_consumer.ml
  $ ./first_class_specifications_tool.exe diagnostic artifacts/cross_unit_consumer.cmt | sed -E 's/code=[^ ]+/code=<closed>/'
  stage=adapter code=<closed> backend=0 contexts=0 solvers=0 live=0

Ordinary PPX output erases lambdas, carriers, apply heads, callback
certificates, and helper names from bytecode and native artifacts.

  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erasure.cmo fixtures/erasure.ml
  $ ocamlc artifacts/erasure.cmo -o artifacts/erasure.byte
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erasure.cmx fixtures/erasure.ml
  $ ocamlopt artifacts/erasure.cmx -o artifacts/erasure.native
  $ artifacts/erasure.byte; artifacts/erasure.native
  runtime=42
  runtime=42
  $ ocamlc -i -ppx ../../ppx/vero_ppx.exe fixtures/erasure.ml
  val runtime_value : int
  $ if strings artifacts/erasure.cmo artifacts/erasure.cmx artifacts/erasure.o | grep -E 'spec-function|spec-lambda|spec-apply|callback.certificate|adder' >/dev/null; then exit 1; fi
  $ echo "erasure byte/native helpers=0 runtime-api=0"
  erasure byte/native helpers=0 runtime-api=0
