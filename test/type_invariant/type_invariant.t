Invariant metadata remains authenticated and opaque, but verification does not
turn type membership, entry parameters, or call results into instance authority.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained authenticated_descriptor
  $ ./type_invariant_tool.exe descriptor artifacts/authenticated_descriptor.cmt | sed -E 's/digest=[0-9a-f]+/digest=DIGEST/'
  handle=invariant:Box.t:1:Box.invariant:2 certificate=same-cmt:1:11:13 type=Box.t#1 model=Box.model#1 snapshot=snapshot#0 predicate=Box.invariant#2 digest=DIGEST operations=4
  $ ./type_invariant_tool.exe attacks artifacts/authenticated_descriptor.cmt
  predicate-identity: rejected before registration and VIR
  predicate-role: rejected before registration and VIR
  operation-snapshot: rejected before registration and VIR
  model-identity: rejected before registration and VIR
  copied-token: rejected before registration and VIR
  certificate-type: rejected before registration and VIR

The former proof-entry positive is now a required Ghost-formal rejection. Raw
SST remains dumpable for diagnostics, but no VIR or backend path is available.

  $ retained authenticated_invariant
  $ ./type_invariant_tool.exe dump-sst artifacts/authenticated_invariant.cmt | grep 'use-type-invariant' | sed -E 's/ @ .*//'
        use-type-invariant id=verocaml:use-type-invariant:1:732:766 : unit
        use-type-invariant id=verocaml:use-type-invariant:1:844:878 : unit
  $ ./type_invariant_tool.exe dump-vir artifacts/authenticated_invariant.cmt 2>&1 | grep -F 'use_type_invariant requires an exact Exec or Tracked instance'
  probe_locality: malformed SST: probe_locality: invalid semantic SST: use_type_invariant requires an exact Exec or Tracked instance at authenticated_invariant.ml:36:32-36:35 at authenticated_invariant.ml:36:32-36:35

Runtime, wrong-type, and direct trusted invariant attempts reject before VIR.
The compiler still protects the hidden representation.

  $ for name in runtime_use wrong_type_use; do retained "$name"; ./type_invariant_tool.exe reject "artifacts/$name.cmt"; done
  semantic invariant gate rejected before VIR
  semantic invariant gate rejected before VIR
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/representation_leak.cmo fixtures/representation_leak.ml >/dev/null 2>&1; test $? -ne 0

Call-result and transition receipts remain unresolved. The old transition
fixtures therefore fail on the missing exact predecessor fact rather than
manufacturing call-result or entry authority.

  $ for name in transition_invariant transition_invalid; do retained "$name"; ./type_invariant_tool.exe dump-vir "artifacts/$name.cmt" 2>&1 | grep -F 'invariant transition predecessor has no authenticated closed validity fact' | sed -E 's/^.*malformed SST: /malformed SST: /'; done
  malformed SST: invariant transition predecessor has no authenticated closed validity fact at transition_invariant.ml:34:8-34:25
  malformed SST: invariant transition predecessor has no authenticated closed validity fact at transition_invalid.ml:32:4-32:22

Installed clients can consume opaque accessors but cannot construct or copy a
handle, even when installed private directories are on the include path.

  $ root="${PWD%%/_build/*}"
  $ core="$root/_build/install/default/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$root/_build/install/default/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_handle_forgery.ml -o artifacts/installed_handle_forgery.cmo 2>&1 | grep -F 'Unbound record field'
  Error: Unbound record field "invariant_id"
