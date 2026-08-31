The ordinary stable outcomes live in outcome_cases.ml. This legacy target
retains only the private generic-encoding and ordinary-runtime specialist
contracts listed in test/support/migrations/w05.org.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/persistent_collections.cmo fixtures/persistent_collections.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/persistent_collections.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/collections.sst >/dev/null
  $ grep -E '^function (identity|copy_option|reverse_into|mirror)#.*binders=' artifacts/collections.sst | sed -E 's/ policy=.*//'
  function identity#0 binders=['0@identity#0] mode=exec recursive=false result='0@identity#0
  function copy_option#3 binders=['0@copy_option#3] mode=exec recursive=false result=option<'0@copy_option#3>
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
