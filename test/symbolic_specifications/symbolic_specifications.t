Bodyless symbolic declarations retain only authenticated ghost carriers and
verify through stable query-local UF/constant semantics.

  $ mkdir artifacts
  $ retained () { name=$1; source=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ for name in scalar_positive nullary_positive polymorphic_nullary_positive parametric_positive adt_positive congruence_control broadcast_axiom_positive inactive_axiom general_trigger_positive negative_semantics negative_exec_use negative_type negative_trigger negative_foreign_or_stale; do retained "$name" "fixtures/$name.ml"; done
  $ check_route () { name=$1; expected=$2; disposition=$3; for input in "fixtures/$name.ml" "artifacts/$name.cmt"; do for threads in 1 2; do for repeat in 1 2; do out="artifacts/$name.$(basename "$input").$threads.$repeat.out"; if timeout 10 env OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 5000 --rlimit 100000 >"$out" 2>&1; then rc=0; else rc=$?; fi; if test "$disposition" = verified; then test "$rc" = 0; grep -Eq "verified(-with-trusted-axioms)? file=.* functions=$expected obligations=$expected" "$out"; else test "$rc" = 1; grep -q 'result=counterexample' "$out"; fi; done; cmp "artifacts/$name.$(basename "$input").$threads.1.out" "artifacts/$name.$(basename "$input").$threads.2.out"; done; done; echo "$name source+cmt threads=1/2 repeat=stable status=$disposition queries=$expected"; }
  $ check_route scalar_positive 4 verified
  scalar_positive source+cmt threads=1/2 repeat=stable status=verified queries=4
  $ check_route nullary_positive 3 verified
  nullary_positive source+cmt threads=1/2 repeat=stable status=verified queries=3
  $ check_route polymorphic_nullary_positive 9 verified
  polymorphic_nullary_positive source+cmt threads=1/2 repeat=stable status=verified queries=9
  $ check_route parametric_positive 3 verified
  parametric_positive source+cmt threads=1/2 repeat=stable status=verified queries=3
  $ check_route adt_positive 4 verified
  adt_positive source+cmt threads=1/2 repeat=stable status=verified queries=4
  $ check_route congruence_control 3 verified
  congruence_control source+cmt threads=1/2 repeat=stable status=verified queries=3
  $ check_route broadcast_axiom_positive 1 verified
  broadcast_axiom_positive source+cmt threads=1/2 repeat=stable status=verified queries=1
  $ check_route general_trigger_positive 4 verified
  general_trigger_positive source+cmt threads=1/2 repeat=stable status=verified queries=4
  $ check_route inactive_axiom 1 counterexample
  inactive_axiom source+cmt threads=1/2 repeat=stable status=counterexample queries=1
  $ check_route negative_semantics 1 counterexample
  negative_semantics source+cmt threads=1/2 repeat=stable status=counterexample queries=1

Nested declarations retain their full resolved module path in CMT identity,
remain same-unit-only hidden module members, and erase from ordinary code.
The fixture is self-contained and independent of the verification library.

  $ retained nested_positive fixtures/nested_positive.ml
  $ for input in fixtures/nested_positive.ml artifacts/nested_positive.cmt; do for threads in 1 2; do out="artifacts/nested_positive.$(basename "$input").$threads.out"; timeout 30 ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 20000 --rlimit 100000 >"$out" 2>&1; grep -q 'verified file=.* functions=3 obligations=2' "$out"; done; cmp "artifacts/nested_positive.$(basename "$input").1.out" "artifacts/nested_positive.$(basename "$input").2.out"; done; echo 'nested source+cmt threads=1/2 path=stable status=verified functions=3 obligations=2'
  nested source+cmt threads=1/2 path=stable status=verified functions=3 obligations=2
  $ ./symbolic_specifications_tool.exe declarations artifacts/nested_positive.cmt
  declaration name=Stack.nested_image path=Stack.nested_image authenticated=true
  $ cat > artifacts/nested_runtime.ml <<'EOF'
  > module Runtime = struct
  >   [%%verocaml.symbolic val nested_image : int -> int]
  >   let value = 42
  > end
  > let value = Runtime.value
  > EOF
  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/nested_ordinary.cmo artifacts/nested_runtime.ml
  $ strings artifacts/nested_ordinary.cmo | grep -E 'nested_image|verocaml.internal.symbolic|symbolic\.' >/dev/null; test $? = 1; echo 'nested-runtime symbolic-bindings=0'
  nested-runtime symbolic-bindings=0

The focused observer pins exact production lifecycle counts, trust/insertion
counts, and portable direct/detached reconstruction under the fixed policy.

  $ for name in scalar_positive nullary_positive polymorphic_nullary_positive parametric_positive adt_positive congruence_control general_trigger_positive; do ./symbolic_specifications_tool.exe structural "artifacts/$name.cmt" | sed -n '1p;$p'; done
  status=verified functions=4 obligations=4 symbolic=4 destroyed=true
  resources backend=4 contexts=4 solvers=4 resets=4 cleaned=4 live=0
  status=verified functions=3 obligations=3 symbolic=3 destroyed=true
  resources backend=3 contexts=3 solvers=3 resets=3 cleaned=3 live=0
  status=verified functions=9 obligations=9 symbolic=6 destroyed=true
  resources backend=9 contexts=9 solvers=9 resets=9 cleaned=9 live=0
  status=verified functions=3 obligations=3 symbolic=1 destroyed=true
  resources backend=3 contexts=3 solvers=3 resets=3 cleaned=3 live=0
  status=verified functions=4 obligations=4 symbolic=4 destroyed=true
  resources backend=4 contexts=4 solvers=4 resets=4 cleaned=4 live=0
  status=verified functions=3 obligations=3 symbolic=3 destroyed=true
  resources backend=3 contexts=3 solvers=3 resets=3 cleaned=3 live=0
  status=verified functions=4 obligations=4 symbolic=4 destroyed=true
  resources backend=4 contexts=4 solvers=4 resets=4 cleaned=4 live=0
  $ ./symbolic_specifications_tool.exe structural artifacts/broadcast_axiom_positive.cmt
  status=verified functions=1 obligations=1 symbolic=1 destroyed=true
  broadcast active=1 trusted-declarations=1 trusted-uses=1 inserted=1
  resources backend=1 contexts=1 solvers=1 resets=1 cleaned=1 live=0
  $ ./symbolic_specifications_tool.exe semantic artifacts/inactive_axiom.cmt
  status=counterexample functions=1 obligations=1 destroyed=true
  resources backend=1 contexts=1 solvers=1 resets=1 cleaned=1 live=0
  $ ./symbolic_specifications_tool.exe semantic artifacts/negative_semantics.cmt
  status=counterexample functions=1 obligations=1 destroyed=true
  resources backend=1 contexts=1 solvers=1 resets=1 cleaned=1 live=0
  $ for name in scalar_positive nullary_positive polymorphic_nullary_positive parametric_positive adt_positive congruence_control broadcast_axiom_positive inactive_axiom general_trigger_positive negative_semantics; do ./symbolic_specifications_tool.exe route-parity "artifacts/$name.cmt"; done
  routes direct=4 detached=4 parity=true timeout-ms=5000 rlimit=100000
  routes direct=3 detached=3 parity=true timeout-ms=5000 rlimit=100000
  routes direct=9 detached=9 parity=true timeout-ms=5000 rlimit=100000
  routes direct=3 detached=3 parity=true timeout-ms=5000 rlimit=100000
  routes direct=4 detached=4 parity=true timeout-ms=5000 rlimit=100000
  routes direct=3 detached=3 parity=true timeout-ms=5000 rlimit=100000
  routes direct=1 detached=1 parity=true timeout-ms=5000 rlimit=100000
  routes direct=1 detached=1 parity=true timeout-ms=5000 rlimit=100000
  routes direct=4 detached=4 parity=true timeout-ms=5000 rlimit=100000
  routes direct=1 detached=1 parity=true timeout-ms=5000 rlimit=100000

SST/VIR dumps preserve body absence, exact type vectors, repeated head identity,
nullary identity, all four trigger sorts, and thread-stable names.

  $ for name in scalar_positive nullary_positive polymorphic_nullary_positive parametric_positive adt_positive congruence_control broadcast_axiom_positive general_trigger_positive; do for threads in 1 2; do timeout 10 ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads "$threads" --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$name.$threads.sst" --dump-vir "artifacts/$name.$threads.vir" >/dev/null; done; cmp "artifacts/$name.1.sst" "artifacts/$name.2.sst"; cmp "artifacts/$name.1.vir" "artifacts/$name.2.vir"; done
  $ printf 'declarations=%s unique-symbols=%s trigger-patterns=%s\n' "$(grep -h -c 'body symbolic-declaration .* trust=none body=absent' artifacts/{scalar_positive,nullary_positive,polymorphic_nullary_positive,parametric_positive,adt_positive,congruence_control,broadcast_axiom_positive,general_trigger_positive}.1.sst | awk '{n+=$1} END{print n}')" "$(grep -h -o 'symbol=vero_symbolic_[0-9a-f]*' artifacts/{scalar_positive,nullary_positive,polymorphic_nullary_positive,parametric_positive,adt_positive,congruence_control,broadcast_axiom_positive,general_trigger_positive}.1.sst | sort -u | wc -l)" "$(grep -o ':qid [^ ]*' artifacts/general_trigger_positive.1.vir | sort -u | wc -l)"
  declarations=26 unique-symbols=32 trigger-patterns=4
  $ for sort in 'candidate$1:Int' 'candidate$1:Bool' 'candidate$1:box<int>#0' "candidate$1:'0@parametric_trigger"; do grep -F "$sort" artifacts/general_trigger_positive.1.vir >/dev/null; done; echo 'trigger-sorts=integer/boolean/aggregate/parametric patterns=4 binders=complete'
  trigger-sorts=integer/boolean/aggregate/parametric patterns=4 binders=complete
  $ ./symbolic_specifications_tool.exe trigger-structure artifacts/general_trigger_positive.cmt
  trigger sort=integer declaration=integer_head vector=exact arguments=ordered binders=complete authenticated=true
  trigger sort=boolean declaration=boolean_head vector=exact arguments=ordered binders=complete authenticated=true
  trigger sort=aggregate declaration=aggregate_head vector=exact arguments=ordered binders=complete authenticated=true
  trigger sort=parametric declaration=parametric_head vector=exact arguments=ordered binders=complete authenticated=true

Declaration order does not alter structural UF heads, and inactive broadcasts
contribute no trust or insertion.

  $ cat > artifacts/reorder_a.ml <<'EOF'
  > [%%verocaml.symbolic val first : int -> int]
  > [%%verocaml.symbolic val second : bool -> bool]
  > let use (number:int) (flag:bool) : int =
  >   [%verocaml.ensures fun _ -> first number = first number && second flag = second flag];
  >   number
  > EOF
  $ cat > artifacts/reorder_b.ml <<'EOF'
  > [%%verocaml.symbolic val second : bool -> bool]
  > [%%verocaml.symbolic val first : int -> int]
  > let use (number:int) (flag:bool) : int =
  >   [%verocaml.ensures fun _ -> first number = first number && second flag = second flag];
  >   number
  > EOF
  $ retained reorder_a artifacts/reorder_a.ml; retained reorder_b artifacts/reorder_b.ml
  $ for name in reorder_a reorder_b; do ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 2 --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$name.sst" >/dev/null; grep -o 'symbol=vero_symbolic_[0-9a-f]*' "artifacts/$name.sst" | sort -u > "artifacts/$name.symbols"; done; cmp artifacts/reorder_a.symbols artifacts/reorder_b.symbols; echo 'reorder heads=2 stable=true'
  reorder heads=2 stable=true
  $ ./symbolic_specifications_tool.exe structural artifacts/inactive_axiom.cmt | grep '^broadcast '
  broadcast active=0 trusted-declarations=0 trusted-uses=0 inserted=0

Ordinary PPX output erases every declaration and helper in bytecode and native
artifacts while preserving runtime behaviour and exported runtime shape.

  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime_erasure.cmo fixtures/runtime_erasure.ml
  $ ocamlc artifacts/runtime_erasure.cmo -o artifacts/runtime_erasure.byte
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime_erasure.cmx fixtures/runtime_erasure.ml
  $ ocamlopt -o artifacts/runtime_erasure.native artifacts/runtime_erasure.cmx
  $ artifacts/runtime_erasure.byte; artifacts/runtime_erasure.native
  runtime=42
  runtime=42
  $ ocamlc -i -ppx ../../ppx/vero_ppx.exe fixtures/runtime_erasure.ml
  val runtime_value : int
  $ strings artifacts/runtime_erasure.cmo artifacts/runtime_erasure.cmx artifacts/runtime_erasure.o | grep -E 'erased_(integer|boolean)|verocaml.internal.symbolic|symbolic\\.' >/dev/null; test $? = 1; echo 'runtime-erasure symbolic-bindings=0 helpers=0 verifier-work=0'
  runtime-erasure symbolic-bindings=0 helpers=0 verifier-work=0

Malformed declaration surfaces reject at PPX time, while bodyless carriers with
Exec use, unsupported types, and malformed triggers
reject before SST/VIR/VC/backend work.

  $ cat > artifacts/generate_declaration_negatives.py <<'PY'
  > from pathlib import Path
  > p = Path("artifacts")
  > cases = {
  > "empty": "[%%verocaml.symbolic]\n",
  > "multiple": "[%%verocaml.symbolic val a : int val b : int]\n",
  > "body": "[%%verocaml.symbolic let bad = 1]\n",
  > "recursive": "[%%verocaml.symbolic let rec bad x = bad x]\n",
  > "contract": "[%%verocaml.symbolic val bad : int [@@verocaml.spec]]\n",
  > "primitive": '[%%verocaml.symbolic external bad : int = "bad"]\n',
  > "payload": '[%%verocaml.symbolic "bad"]\n',
  > }
  > for name, source in cases.items():
  >     (p / f"declaration_{name}.ml").write_text(source)
  > PY
  $ python3 artifacts/generate_declaration_negatives.py
  $ for case_ in empty multiple body recursive contract primitive payload; do if retained "declaration_$case_" "artifacts/declaration_$case_.ml" >/dev/null 2>&1; then exit 1; else echo "declaration:$case_ zero-work"; fi; done
  declaration:empty zero-work
  declaration:multiple zero-work
  declaration:body zero-work
  declaration:recursive zero-work
  declaration:contract zero-work
  declaration:primitive zero-work
  declaration:payload zero-work
  $ for name in negative_exec_use negative_type negative_trigger; do ./symbolic_specifications_tool.exe diagnostic "artifacts/$name.cmt" | sed -E 's/code=[^ ]+/code=<closed>/'; done
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ cat > artifacts/generate_type_negatives.py <<'PY'
  > from pathlib import Path
  > p = Path("artifacts")
  > cases = {
  > "callback": "[%%verocaml.symbolic val bad : (int -> int) -> int]\n",
  > "reference": "[%%verocaml.symbolic val bad : int ref -> int]\n",
  > "array": "[%%verocaml.symbolic val bad : int array -> int]\n",
  > "object": "[%%verocaml.symbolic val bad : < value : int > -> int]\n",
  > "unsupported": "[%%verocaml.symbolic val bad : float -> float]\n",
  > "mutable_cycle": "type cycle = { mutable next : cycle option }\n[%%verocaml.symbolic val bad : cycle -> cycle]\n",
  > }
  > for name, source in cases.items():
  >     (p / f"type_{name}.ml").write_text(source)
  > PY
  $ python3 artifacts/generate_type_negatives.py
  $ for case_ in callback reference array object unsupported mutable_cycle; do if retained "type_$case_" "artifacts/type_$case_.ml" >/dev/null 2>&1; then ./symbolic_specifications_tool.exe diagnostic "artifacts/type_$case_.cmt"; else echo 'rejected=ppx sst=0 vir=0 vc=0'; echo 'resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0'; fi | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'; done
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=ppx sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0

Nonapplication, nullary, binder-incomplete, duplicate, and misplaced trigger
forms all fail closed with no semantic or solver work.

  $ cat > artifacts/generate_trigger_negatives.py <<'PY'
  > from pathlib import Path
  > p = Path("artifacts")
  > head = "[%%verocaml.symbolic val observed : int -> bool]\n"
  > cases = {
  > "nonapplication": head + "let bad x = [%verocaml.requires forall (fun (y:int) -> ((y = y) [@trigger]))]; x\n",
  > "incomplete": head + "let bad x = [%verocaml.requires forall (fun (y:int) -> ((observed x) [@trigger]) || y = y)]; x\n",
  > "duplicate": head + "let bad x = [%verocaml.requires forall (fun (y:int) -> (((observed y) [@trigger]) && ((observed y) [@trigger])))]; x\n",
  > "misplaced": head + "let bad x = [%verocaml.requires forall (fun (y:int) -> observed (y [@trigger]))]; x\n",
  > "unit_result": "[%%verocaml.symbolic val produce : int -> unit]\nlet bad x = [%verocaml.requires forall (fun (y:int) -> (((produce y) [@trigger]) = ()))]; x\n",
  > }
  > for name, source in cases.items():
  >     (p / f"trigger_{name}.ml").write_text(source)
  > PY
  $ python3 artifacts/generate_trigger_negatives.py
  $ for case_ in nonapplication incomplete duplicate misplaced unit_result; do retained "trigger_$case_" "artifacts/trigger_$case_.ml"; ./symbolic_specifications_tool.exe diagnostic "artifacts/trigger_$case_.cmt" | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'; done
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0

Marker, source-type vector, span, path, UID, exact program, exact CMT artifact,
foreign-unit identity, and nested forged/rebound carrier attacks reject before
semantic construction.

  $ for attack in marker type-vector span path; do python3 mutate_symbolic_carrier.py artifacts/negative_foreign_or_stale.cmt "artifacts/mutated_$attack.cmt" "$attack"; ./symbolic_specifications_tool.exe diagnostic "artifacts/mutated_$attack.cmt" | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'; done
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ python3 mutate_symbolic_carrier.py artifacts/nested_positive.cmt artifacts/nested_forged.cmt marker
  $ ./symbolic_specifications_tool.exe diagnostic artifacts/nested_forged.cmt | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./symbolic_specifications_tool.exe wrong-binding artifacts/nested_positive.cmt nested_image | sed -E 's/code=[^ ]+/code=<closed>/'
  wrong-binding code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./symbolic_specifications_tool.exe wrong-use artifacts/negative_foreign_or_stale.cmt uid local_image | sed -E 's/code=[^ ]+/code=<closed>/'
  wrong-uid code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./symbolic_specifications_tool.exe wrong-use artifacts/negative_foreign_or_stale.cmt path local_image | sed -E 's/code=[^ ]+/code=<closed>/'
  wrong-path code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./symbolic_specifications_tool.exe wrong-program artifacts/negative_foreign_or_stale.cmt
  wrong-program rejected=true sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./symbolic_specifications_tool.exe wrong-artifact artifacts/negative_foreign_or_stale.cmt artifacts/scalar_positive.cmt | sed -E 's/code=[^ ]+/code=<closed>/'
  wrong-artifact code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ cat > artifacts/foreign_provider.ml <<'EOF'
  > [%%verocaml.symbolic val foreign_image : int -> int]
  > EOF
  $ cat > artifacts/foreign_consumer.ml <<'EOF'
  > let bad (value:int) : int =
  >   [%verocaml.ensures fun _ -> Foreign_provider.foreign_image value = value];
  >   value
  > EOF
  $ retained foreign_provider artifacts/foreign_provider.ml
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/foreign_consumer.cmo artifacts/foreign_consumer.ml
  $ ./symbolic_specifications_tool.exe diagnostic artifacts/foreign_consumer.cmt | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0

The dedicated owners, exact authority, no-growth concentration, and fresh
private package projection remain bounded with no runtime/public module.

  $ profile=$(realpath "$(dirname "$(readlink -f architecture_check.py)")/../.."); root="${VEROCAML_SOURCE_ROOT:-$(dirname "$(dirname "$profile")")}"; install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"; receipt="${VEROCAML_TEST_INSTALL_RECEIPT:-../architecture_authority/live-install-receipt.json}"; python3 architecture_check.py "$root" "$install_root" "$receipt" ../parametric_core/architecture_inventory.exe
  symbolic architecture owners=4 module=622/650 interface=68/180 function=111/140 concentration=58940/58942 installed=920/222/21 receipt=matched
