The large positive gate exercises executable parametric functions as ordinary
OCaml: immutable collections, nested generic data, structural recursion,
type-changing verified callbacks, labelled callbacks, and retained providers.
Runtime polymorphic structural equality is deliberately not part of this gate.

  $ mkdir artifacts
  $ retained () { name=$1; source=${2:-fixtures/$1.ml}; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "$source"; }
  $ retained persistent_collections
  $ retained callback_workflows
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/workflow_provider.cmi fixtures/workflow_provider.mli
  $ retained workflow_provider
  $ retained workflow_consumer

Each substantial source verifies directly and from its retained CMT. Serial
and parallel VC checking agree.

  $ for name in persistent_collections callback_workflows; do OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$name.ml" --threads 1 --timeout-ms 10000; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 2 --timeout-ms 10000; done
  verocaml: verified file=fixtures/persistent_collections.ml functions=28 obligations=89
  verocaml: verified file=artifacts/persistent_collections.cmt functions=28 obligations=89
  verocaml: verified file=fixtures/callback_workflows.ml functions=17 obligations=100
  verocaml: verified file=artifacts/callback_workflows.cmt functions=17 obligations=100
  $ cp fixtures/workflow_provider.ml artifacts/workflow_provider_source.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/workflow_provider_source.ml --threads 1 --timeout-ms 10000
  verocaml: verified file=artifacts/workflow_provider_source.ml functions=14 obligations=44
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/workflow_provider.cmt --threads 2 --timeout-ms 10000
  verocaml: verified file=artifacts/workflow_provider.cmt functions=14 obligations=44
  $ cp fixtures/workflow_consumer.ml artifacts/workflow_consumer_source.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/workflow_consumer_source.ml --threads 1 --timeout-ms 10000 --dependency artifacts/workflow_provider.cmt
  verocaml: verified dependency unit=Workflow_provider interface-digest=90d4fa5d7decf4a566b1ac50eb1159bb direct=none transitive=none trust=none
  verocaml: verified file=artifacts/workflow_consumer_source.ml functions=14 obligations=6
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/workflow_consumer.cmt --threads 2 --timeout-ms 10000 --dependency artifacts/workflow_provider.cmt
  verocaml: verified dependency unit=Workflow_provider interface-digest=90d4fa5d7decf4a566b1ac50eb1159bb direct=none transitive=none trust=none
  verocaml: verified file=artifacts/workflow_consumer.cmt functions=14 obligations=6

The retained semantic dump keeps generic functions generic rather than
enumerating concrete instantiations.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/persistent_collections.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/collections.sst >/dev/null
  $ grep -E '^function (identity|copy_option|reverse_into|mirror)#.*binders=' artifacts/collections.sst | sed -E 's/ policy=.*//'
  function identity#0 binders=['0@identity#0] mode=exec recursive=false result='0@identity#0
  function copy_option#3 binders=['0@copy_option#3] mode=exec recursive=false result=Stdlib.option<'0@copy_option#3>
  function reverse_into#10 binders=['0@reverse_into#10] mode=exec recursive=true result=chain<'0@reverse_into#10>
  function mirror#23 binders=['0@mirror#23] mode=exec recursive=true result=tree<'0@mirror#23>
  $ test "$(grep -Ec '^function [^ ]+<' artifacts/collections.sst)" = 0

Ordinary bytecode execution preserves the expected OCaml behavior after ghost
annotations are erased.

  $ ordinary () { name=$1; source=${2:-fixtures/$1.ml}; ocamlc -w -A -alert -all -I artifacts -ppx ../../ppx/vero_ppx.exe -c -o "artifacts/$name.cmo" "$source"; }
  $ ordinary persistent_collections
  $ ordinary callback_workflows
  $ ocamlc -w -A -alert -all -I artifacts -ppx ../../ppx/vero_ppx.exe -c -o artifacts/workflow_provider.cmi fixtures/workflow_provider.mli
  $ ordinary workflow_provider
  $ ordinary workflow_consumer
  $ ordinary runtime
  $ ocamlc -o artifacts/runtime.byte artifacts/persistent_collections.cmo artifacts/callback_workflows.cmo artifacts/workflow_provider.cmo artifacts/workflow_consumer.cmo artifacts/runtime.cmo
  $ artifacts/runtime.byte
  parametric-programs-runtime: ok
