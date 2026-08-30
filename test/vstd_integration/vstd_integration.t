The ordinary Dune library dependency exposes Vstd.Seq and implicitly imports
the library's verified external type specifications.  No verifier dependency
flags or wrapper/member naming knowledge appear in the consumer.

  $ root="${PWD%%/_build/*}"; consumer="$root/_build/default/test/vstd_integration/.vstd_consumer.objs/byte/vstd_consumer.cmt"
  $ OCAML_COLOR=never ../../src/verocaml.exe verify "$consumer" --threads 2 --timeout-ms 20000 > output 2>&1 || { cat output; exit 2; }
  $ test "$(grep -c '^verocaml: verified dependency unit=' output)" = 2; echo vstd-hidden-members=discovered-from-imported-wrapper
  vstd-hidden-members=discovered-from-imported-wrapper
  $ test "$(grep -c '^verocaml: trusted external body .*function=.*axiom_length_domain' output)" = 1; echo vstd-sequence-axiom=reported-once
  vstd-sequence-axiom=reported-once
  $ grep '^verocaml: verified-with-trusted-axioms ' output | sed -E 's#file=[^ ]*/test/vstd_integration/#file=#; s/ obligations=[0-9]+.*/ obligations=<n>/'
  verocaml: verified-with-trusted-axioms file=.vstd_consumer.objs/byte/vstd_consumer.cmt functions=5 obligations=<n>

Generic axioms use the same imported type catalog as ordinary proofs. This
includes external Option and Result specifications, recursively nested local
types, and imported abstract parametric types.

  $ root="${PWD%%/_build/*}"; consumer="$root/_build/default/test/vstd_integration/.imported_generic_axiom_consumer.objs/byte/imported_generic_axiom_consumer.cmt"
  $ OCAML_COLOR=never ../../src/verocaml.exe verify "$consumer" --threads 2 --timeout-ms 20000 > generic-axiom.output 2>&1 || { cat generic-axiom.output; exit 2; }
  $ grep '^verocaml: verified-with-trusted-axioms ' generic-axiom.output | sed -E 's#file=[^ ]*/test/vstd_integration/#file=#; s/ obligations=[0-9]+.*/ obligations=<n>/'
  verocaml: verified-with-trusted-axioms file=.imported_generic_axiom_consumer.objs/byte/imported_generic_axiom_consumer.cmt functions=6 obligations=<n>

Wrapped Dune libraries resolve exported broadcast groups through compiler value
identity rather than hidden implementation-module spelling.  The public
=Vstd.Seq= path activates the imported initialization laws for a polymorphic
sequence specification.

  $ broadcast="$root/_build/default/test/vstd_integration/.imported_broadcast_group_consumer.objs/byte/imported_broadcast_group_consumer.cmt"
  $ for threads in 1 2; do OCAML_COLOR=never ../../src/verocaml.exe verify "$broadcast" --threads "$threads" --timeout-ms 20000 --dump-vir "broadcast.$threads.vir" >"broadcast.$threads.out" 2>&1 || { cat "broadcast.$threads.out"; exit 2; }; done
  $ cmp broadcast.1.vir broadcast.2.vir && echo wrapped-vstd-broadcast-group=public-path-stable-verified
  wrapped-vstd-broadcast-group=public-path-stable-verified

The same compiler-identity resolution applies to a directly selected wrapped
declaration, a second library that forwards the wrapped group, and a second
library that selects a wrapped declaration into a new group.

  $ for unit in imported_broadcast_declaration_consumer forwarded_broadcast_group_consumer selected_broadcast_group_consumer; do cmt="$root/_build/default/test/vstd_integration/.$unit.objs/byte/$unit.cmt"; for threads in 1 2; do OCAML_COLOR=never ../../src/verocaml.exe verify "$cmt" --threads "$threads" --timeout-ms 20000 --dump-vir "$unit.$threads.vir" >"$unit.$threads.out" 2>&1 || { cat "$unit.$threads.out"; exit 2; }; done; cmp "$unit.1.vir" "$unit.2.vir" || exit 2; echo "$unit=stable-verified"; done
  imported_broadcast_declaration_consumer=stable-verified
  forwarded_broadcast_group_consumer=stable-verified
  selected_broadcast_group_consumer=stable-verified

An ambient Dune =-I= path is not an import.  Linking the library into the
compile environment without using =Vstd= must not inject its hidden catalogs
or axioms.

  $ ambient="$root/_build/default/test/vstd_integration/.ambient_vstd_consumer.objs/byte/ambient_vstd_consumer.cmt"
  $ OCAML_COLOR=never ../../src/verocaml.exe verify "$ambient" --threads 1 --timeout-ms 20000 > ambient.output 2>&1 || { cat ambient.output; exit 2; }
  $ test "$(grep -c '^verocaml: verified dependency unit=' ambient.output)" = 0; test "$(grep -c '^verocaml: trusted ' ambient.output)" = 0; echo ambient-vstd-path=zero-authority
  ambient-vstd-path=zero-authority
