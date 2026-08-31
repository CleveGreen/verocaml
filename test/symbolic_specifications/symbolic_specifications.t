Bodyless symbolic declarations retain only authenticated ghost carriers and
verify through stable query-local UF/constant semantics.

  $ mkdir artifacts
  $ retained () { name=$1; source=$2; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ for name in scalar_positive nullary_positive polymorphic_nullary_positive polymorphic_symbolic_unwrap_positive parametric_positive adt_positive congruence_control broadcast_axiom_positive inactive_axiom general_trigger_positive negative_semantics negative_exec_use negative_type negative_trigger negative_foreign_or_stale; do retained "$name" "fixtures/$name.ml"; done

Polymorphic symbolic results can be unwrapped through a trusted generic axiom.
Source and retained-CMT routes remain stable across repetition and worker count.

  $ for input in fixtures/polymorphic_symbolic_unwrap_positive.ml artifacts/polymorphic_symbolic_unwrap_positive.cmt; do for threads in 1 2; do for repeat in 1 2; do out="artifacts/polymorphic-symbolic-unwrap.$(basename "$input").$threads.$repeat.out"; timeout 20 env OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads "$threads" --timeout-ms 10000 --rlimit 100000 >"$out" 2>&1; grep -Eq 'verified-with-trusted-axioms file=.* functions=11 obligations=20 trusted-external-bodies=1 trusted-external-body-uses=3' "$out"; done; cmp "artifacts/polymorphic-symbolic-unwrap.$(basename "$input").$threads.1.out" "artifacts/polymorphic-symbolic-unwrap.$(basename "$input").$threads.2.out"; done; done; echo 'polymorphic-symbolic-unwrap source+cmt threads=1/2 repeat=stable functions=11 obligations=20 trusted-generic-axiom=true'
  polymorphic-symbolic-unwrap source+cmt threads=1/2 repeat=stable functions=11 obligations=20 trusted-generic-axiom=true

Nested declarations retain their full resolved module path in CMT identity,
remain same-unit-only hidden module members, and erase from ordinary code.
The fixture is self-contained and independent of the verification library.

  $ retained nested_positive fixtures/nested_positive.ml
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
  $ ./symbolic_specifications_tool.exe semantic artifacts/inactive_axiom.cmt | tail -1
  resources backend=1 contexts=1 solvers=1 resets=1 cleaned=1 live=0
  $ ./symbolic_specifications_tool.exe semantic artifacts/negative_semantics.cmt | tail -1
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

Inactive broadcasts contribute no private trust or insertion resources.

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

  $ ./generate_declaration_negatives.exe artifacts
  $ for case_ in empty multiple body recursive contract primitive payload; do if retained "declaration_$case_" "artifacts/declaration_$case_.ml" >/dev/null 2>&1; then exit 1; else echo "declaration:$case_ zero-work"; fi; done
  declaration:empty zero-work
  declaration:multiple zero-work
  declaration:body zero-work
  declaration:recursive zero-work
  declaration:contract zero-work
  declaration:primitive zero-work
  declaration:payload zero-work
  $ for name in negative_exec_use negative_type negative_trigger; do ./symbolic_specifications_tool.exe diagnostic "artifacts/$name.cmt" | tail -1; done
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ for name in symbolic_callback_type symbolic_reference_type symbolic_array_type symbolic_float_type symbolic_mutable_cycle; do retained "$name" "fixtures/$name.ml"; ./symbolic_specifications_tool.exe diagnostic "artifacts/$name.cmt" | tail -1; done
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ if retained symbolic_object_type fixtures/symbolic_object_type.ml >/dev/null 2>&1; then false; fi

Nonapplication, nullary, binder-incomplete, duplicate, and misplaced trigger
forms all fail closed with no semantic or solver work.

  $ for name in trigger_nonapplication trigger_incomplete trigger_duplicate trigger_misplaced trigger_unit_result; do retained "$name" "fixtures/$name.ml"; ./symbolic_specifications_tool.exe diagnostic "artifacts/$name.cmt" | tail -1; done
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0

Marker, source-type vector, span, path, UID, exact program, exact CMT artifact,
foreign-unit identity, and nested forged/rebound carrier attacks reject before
semantic construction.

  $ for attack in marker type-vector span path; do ./mutate_symbolic_carrier.exe artifacts/negative_foreign_or_stale.cmt "artifacts/mutated_$attack.cmt" "$attack"; ./symbolic_specifications_tool.exe diagnostic "artifacts/mutated_$attack.cmt" | sed -n '1p;$p' | sed -E 's/code=[^ ]+/code=<closed>/'; done
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  rejected=adapter code=<closed> sst=0 vir=0 vc=0
  resources backend=0 contexts=0 solvers=0 resets=0 cleaned=0 live=0
  $ ./mutate_symbolic_carrier.exe artifacts/nested_positive.cmt artifacts/nested_forged.cmt marker
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
Imported symbolic identities remain bound to the exact consumer issuance even
though their declarations are not members of the consumer's physical program.
Exact path and compiler UID are required, and equal-but-unissued or ambiguous
declarations fail closed.

  $ ./symbolic_specifications_tool.exe imported-issuance
  imported-symbolic path=exact uid=exact issuance=consumer-bound ambiguity=rejected
