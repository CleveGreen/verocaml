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
  verocaml: verified-with-trusted-axioms file=.imported_generic_axiom_consumer.objs/byte/imported_generic_axiom_consumer.cmt functions=3 obligations=<n>

An ambient Dune =-I= path is not an import.  Linking the library into the
compile environment without using =Vstd= must not inject its hidden catalogs
or axioms.

  $ ambient="$root/_build/default/test/vstd_integration/.ambient_vstd_consumer.objs/byte/ambient_vstd_consumer.cmt"
  $ OCAML_COLOR=never ../../src/verocaml.exe verify "$ambient" --threads 1 --timeout-ms 20000 > ambient.output 2>&1 || { cat ambient.output; exit 2; }
  $ test "$(grep -c '^verocaml: verified dependency unit=' ambient.output)" = 0; test "$(grep -c '^verocaml: trusted ' ambient.output)" = 0; echo ambient-vstd-path=zero-authority
  ambient-vstd-path=zero-authority
