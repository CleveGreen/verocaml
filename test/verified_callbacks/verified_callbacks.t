The focused callback gate compiles compiler-retained inputs with fixed
per-obligation resources.  Source and CMT verification, repeated execution,
and one/two-thread scheduling must agree.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for name in top_level_callbacks local_callbacks polymorphic_apply relational_result runtime negative_missing_contract negative_builtin_grammar negative_partial negative_escape negative_capture negative_recursive negative_equality; do retained "$name"; done
  $ ./verified_callbacks_tool.exe verify artifacts/top_level_callbacks.cmt 1
  status=verified functions=8 obligations=17 threads=1
  $ ./verified_callbacks_tool.exe verify artifacts/local_callbacks.cmt 1
  status=verified functions=11 obligations=36 threads=1
  $ ./verified_callbacks_tool.exe verify artifacts/polymorphic_apply.cmt 1
  status=verified functions=6 obligations=10 threads=1
  $ ./verified_callbacks_tool.exe verify artifacts/relational_result.cmt 1
  status=verified functions=3 obligations=7 threads=1
  $ ./verified_callbacks_tool.exe parity artifacts/top_level_callbacks.cmt
  parity=repeat/threads status=verified functions=8 obligations=17 resources=100000 timeout-ms=5000
  $ timeout 30 sh -c './verified_callbacks_tool.exe verify artifacts/top_level_callbacks.cmt 1 >/dev/null && ./verified_callbacks_tool.exe verify artifacts/local_callbacks.cmt 1 >/dev/null && ./verified_callbacks_tool.exe verify artifacts/polymorphic_apply.cmt 1 >/dev/null && ./verified_callbacks_tool.exe verify artifacts/relational_result.cmt 1 >/dev/null' && echo focused-runtime=under-30s
  focused-runtime=under-30s
  $ ./verified_callbacks_tool.exe resource-evidence artifacts/top_level_callbacks.cmt
  resources obligations=17 contexts=17/17 live=0 solvers=17/17 max-live=1 policy=100000/5000

The structural dump contains opaque callback relations and reached-call
preconditions, but no function sort or imported callback body equation.

  $ ./verified_callbacks_tool.exe inspect artifacts/top_level_callbacks.cmt
  callbacks reached=2 preconditions=2 relations=12 helpers=1 function-sort=0 body-equation=0
  $ ./verified_callbacks_tool.exe structural-evidence artifacts/top_level_callbacks.cmt
  structural reached=2 preconditions=2 exact=true fresh-results=true exact-ensures=true max-vcs=3
  $ ./verified_callbacks_tool.exe repeated-call-evidence artifacts/local_callbacks.cmt
  repeated-calls sites=2 path-facts=6 pairs=1 results=2 distinct=true
  $ ./verified_callbacks_tool.exe callback-kinds artifacts/top_level_callbacks.cmt
  apply:f:formal
  apply_labelled:f:formal
  use_identity:identity:top
  use_zero_or_self:zero_or_self:top
  use_labelled:select:top

Authentication and policy failures reject before the verification pipeline or
solver is entered.

  $ for name in negative_missing_contract negative_builtin_grammar negative_partial negative_escape negative_capture negative_recursive negative_equality; do printf '%s: ' "$name"; ./verified_callbacks_tool.exe reject-zero-work "artifacts/$name.cmt"; done
  negative_missing_contract: rejected=pre-solver code=VERO_CALLBACK_CONTRACT driver=1 pipeline=0 solver=0 z3=0/0
  negative_builtin_grammar: rejected=pre-solver code=VERO_INVALID_CALLBACK driver=1 pipeline=0 solver=0 z3=0/0
  negative_partial: rejected=pre-solver code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION driver=1 pipeline=0 solver=0 z3=0/0
  negative_escape: rejected=pre-solver code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION driver=1 pipeline=0 solver=0 z3=0/0
  negative_capture: rejected=pre-solver code=VERO_UNSUPPORTED_MUTATION driver=1 pipeline=0 solver=0 z3=0/0
  negative_recursive: rejected=pre-solver code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION driver=1 pipeline=0 solver=0 z3=0/0
  negative_equality: rejected=pre-solver code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION driver=1 pipeline=0 solver=0 z3=0/0

The source-backed capture matrix rejects reference, mutable record/ADT,
transitively mutable, function, and nested-callback captures before validation
or solver creation.

  $ cat > artifacts/capture_mutable_record.ml <<'EOF'
  > type box = { mutable value : int }
  > let apply f x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let rejected x = let captured = { value = x } in let callback y = [%verocaml.requires true]; [%verocaml.ensures fun _ -> true]; y + captured.value in apply callback x
  > EOF
  $ cat > artifacts/capture_mutable_adt.ml <<'EOF'
  > type cell = Cell of int ref
  > let apply f x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let rejected x = let captured = Cell (ref x) in let callback y = [%verocaml.requires true]; [%verocaml.ensures fun _ -> true]; match captured with Cell r -> y + !r in apply callback x
  > EOF
  $ cat > artifacts/capture_transitive.ml <<'EOF'
  > type inner = { cell : int ref }
  > type outer = { nested : inner }
  > let apply f x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let rejected x = let captured = { nested = { cell = ref x } } in let callback y = [%verocaml.requires true]; [%verocaml.ensures fun _ -> true]; y + !(captured.nested.cell) in apply callback x
  > EOF
  $ cat > artifacts/capture_function.ml <<'EOF'
  > let apply f x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let rejected x = let helper (z : int) = z in let callback y = [%verocaml.requires true]; [%verocaml.ensures fun _ -> true]; helper y in apply callback x
  > EOF
  $ cat > artifacts/capture_nested.ml <<'EOF'
  > let apply f x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let rejected x = let outer (y : int) = [%verocaml.requires true]; [%verocaml.ensures fun result -> result = y]; y in let callback y = [%verocaml.requires true]; [%verocaml.ensures fun _ -> true]; outer y in apply callback x
  > EOF
  $ for name in capture_mutable_record capture_mutable_adt capture_transitive capture_function capture_nested; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "artifacts/$name.ml"; printf '%s: ' "$name"; ./verified_callbacks_tool.exe reject-zero-work "artifacts/$name.cmt"; done
  capture_mutable_record: rejected=pre-solver code=VERO_CALLBACK_POLICY driver=1 pipeline=0 solver=0 z3=0/0
  capture_mutable_adt: rejected=pre-solver code=VERO_UNSUPPORTED_TYPE driver=1 pipeline=0 solver=0 z3=0/0
  capture_transitive: rejected=pre-solver code=VERO_UNSUPPORTED_TYPE driver=1 pipeline=0 solver=0 z3=0/0
  capture_function: rejected=pre-solver code=VERO_CALLBACK_CONTRACT driver=1 pipeline=0 solver=0 z3=0/0
  capture_nested: rejected=pre-solver code=VERO_CALLBACK_POLICY driver=1 pipeline=0 solver=0 z3=0/0

Semantic failures are real retained programs and enter the solver.  Diagnostics
separate an unproved reached-callback precondition, the callback closure's own
postcondition, and a client claim stronger than the callback relation.

  $ for name in semantic_precondition semantic_closure semantic_stronger; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; printf '%s: ' "$name"; ./verified_callbacks_tool.exe semantic-negative "artifacts/$name.cmt"; done
  semantic_precondition: semantic-negative=counterexample function=unchecked kind=callback-precondition diagnostics=1
  semantic_closure: semantic-negative=counterexample function=bad kind=callback-closure-postcondition diagnostics=1
  semantic_stronger: semantic-negative=counterexample function=client kind=stronger-than-callback-postcondition diagnostics=1

Exact CMT bytes and physical sessions participate in callback authority.
Repetition authenticates, while changed CMT bytes, copied caller identity, and
cross-session replay reject without solver work.

  $ ./verified_callbacks_tool.exe lifecycle-unit artifacts/top_level_callbacks.cmt artifacts/local_callbacks.cmt
  lifecycle binding=callback stable=true bound=true authenticated=true copied=true stale=true session=true solver-work=0
  $ ./verified_callbacks_tool.exe authenticate-cmt artifacts/top_level_callbacks.cmt artifacts/local_callbacks.cmt
  cmt-auth callbacks=true authenticated=true(ok) cross-cmt=true cross-session=true raw-source=true solver-work=0
  $ cp artifacts/top_level_callbacks.cmt artifacts/top_level_callbacks.copy.cmt
  $ ./verified_callbacks_tool.exe compare-cmt-identities artifacts/top_level_callbacks.cmt artifacts/top_level_callbacks.copy.cmt
  cmt-identity equivalent=true

Real OxCaml mode programs cover explicit local/once/portable/unique callback
arrows; local/once/unique actuals verify, while the compiler itself rejects a
nonportable contract-bearing callback in the portable slot.  A wrong label is
likewise a native compiler error, while the saturated labelled positive above
is accepted.

  $ cat > artifacts/mode_matrix.ml <<'EOF'
  > let apply_portable (f : (int -> int) @ portable) x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let apply_local (f : (int -> int) @ local) x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let apply_once (f : (int -> int) @ once) x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let apply_unique (f : (int -> int) @ unique) x = [%verocaml.requires call_requires (f x)]; [%verocaml.ensures fun result -> call_ensures (f x) result]; f x
  > let id x = [%verocaml.requires true]; [%verocaml.ensures fun result -> result = x]; x
  > let local_client x = apply_local id x
  > let once_client x = apply_once id x
  > let unique_client x = apply_unique id x
  > EOF
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/mode_matrix.cmo artifacts/mode_matrix.ml
  $ ./verified_callbacks_tool.exe verify artifacts/mode_matrix.cmt 1
  status=verified functions=8 obligations=12 threads=1
  $ cp artifacts/mode_matrix.ml artifacts/mode_portable_negative.ml; echo 'let portable_client x = apply_portable id x' >> artifacts/mode_portable_negative.ml
  $ ocamlc -w -A -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c artifacts/mode_portable_negative.ml > artifacts/mode-portable.err 2>&1; test $? = 2; grep -q nonportable artifacts/mode-portable.err; echo compiler-mode-negative=native
  compiler-mode-negative=native
  $ cat > artifacts/wrong_label.ml <<'EOF'
  > let labelled ~item = item
  > let _ = labelled ~wrong:1
  > EOF
  $ ocamlc -w -A -c artifacts/wrong_label.ml > artifacts/wrong-label.err 2>&1; test $? = 2; grep -q '~wrong' artifacts/wrong-label.err; echo compiler-label-negative=native
  compiler-label-negative=native

Source input and retained CMT produce identical normalized SST/VIR.  Repeated
and two-thread CMT runs preserve relation declarations, callback identities,
VC order, and outcome under the explicit focused resource policy.

  $ mkdir artifacts/temp
  $ TMPDIR="$PWD/artifacts/temp" OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/top_level_callbacks.ml --threads 1 --timeout-ms 5000 --rlimit 100000 --dump-sst artifacts/source.sst --dump-vir artifacts/source.vir >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/top_level_callbacks.cmt --threads 1 --timeout-ms 5000 --rlimit 100000 --dump-sst artifacts/cmt.sst --dump-vir artifacts/cmt.vir >/dev/null
  $ sed '/^instance-modes/,$d' artifacts/source.sst > artifacts/source.norm.sst; sed '/^instance-modes/,$d' artifacts/cmt.sst > artifacts/cmt.norm.sst
  $ cmp artifacts/source.norm.sst artifacts/cmt.norm.sst; cmp artifacts/source.vir artifacts/cmt.vir; echo source-cmt=sst/vir-equal
  source-cmt=sst/vir-equal
  $ for run in repeat threaded; do threads=1; test "$run" = threaded && threads=2; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/top_level_callbacks.cmt --threads "$threads" --timeout-ms 5000 --rlimit 100000 --dump-sst "artifacts/$run.sst" --dump-vir "artifacts/$run.vir" >/dev/null; done
  $ cmp artifacts/cmt.sst artifacts/repeat.sst; cmp artifacts/cmt.sst artifacts/threaded.sst; cmp artifacts/cmt.vir artifacts/repeat.vir; cmp artifacts/cmt.vir artifacts/threaded.vir; echo repeat-threads=sst/vir-equal
  repeat-threads=sst/vir-equal

Ordinary compilation erases the logical carrier.  Bytecode and native code
retain the ordinary callback ABI and behavior, and generated artifacts contain
no callback carrier or descriptor symbol.

  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime_erased.cmo fixtures/runtime.ml
  $ ocamlc -o artifacts/runtime.byte artifacts/runtime_erased.cmo
  $ artifacts/runtime.byte
  17
  $ ocamlopt -ppx ../../ppx/vero_ppx.exe -o artifacts/runtime.native fixtures/runtime.ml
  $ artifacts/runtime.native
  17
  $ ocamlc -w -A -dsource -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime_dsource.cmo fixtures/runtime.ml > artifacts/runtime.dsource.out 2> artifacts/runtime.dsource
  $ if grep -Ei 'Vero_ghost|call_requires|call_ensures|callback_(certificate|shape)' artifacts/runtime.dsource; then false; else echo dsource-carrier=absent; fi
  dsource-carrier=absent
  $ if strings artifacts/runtime_erased.cmi artifacts/runtime_erased.cmo artifacts/runtime_erased.cmt artifacts/runtime.byte artifacts/runtime.native | grep -Ei 'vero_callback|call_requires|call_ensures|callback_certificate|callback_shape'; then false; fi

Installed public SST/VIR transitively expose the abstract callback-bearing
records.  With only the ordinary package include path, a consumer can use
those public records but cannot name either private authority module.

  $ cat > artifacts/package_public.ml <<'EOF'
  > let callback_name (binding : Sst.callback_binding) = binding.callback_name
  > let reached_name (call : Vir.reached_callback_call) = call.application.callback.callback_name
  > EOF
  $ root="${PWD%%/_build/*}"; install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"; ocamlc -w -A -I "$install_root/lib/verocaml/core" -c -o artifacts/package_public.cmo artifacts/package_public.ml
  $ cat > artifacts/package_private.ml <<'EOF'
  > let cannot_name_shape (_ : Callback_shape_private.t) = ()
  > let cannot_name_certificate (_ : Callback_certificate_private.t) = ()
  > EOF
  $ root="${PWD%%/_build/*}"; ocamlc -w -A -I "$root/_build/install/default/lib/verocaml/core" -c artifacts/package_private.ml > artifacts/package-private.err 2>&1; test $? = 2; grep -q 'Unbound module.*Callback_shape_private' artifacts/package-private.err; echo package=public-sst/vir-private-authority-hidden
  package=public-sst/vir-private-authority-hidden
